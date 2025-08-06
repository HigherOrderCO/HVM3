{-./Type.hs-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HVM.Collapse where

import Control.Monad (ap, forM, forM_)
import Control.Monad.IO.Class
import Data.Char (chr, ord)
import qualified Data.Kind as DK
import Unsafe.Coerce (unsafeCoerce)
import Data.IORef
import Data.Bits ((.&.), xor, (.|.), complement, shiftR)
import Data.Word
import Debug.Trace
import GHC.Conc
import HVM.Foreign
import HVM.Type
import System.Exit (exitFailure)
import System.IO.Unsafe (unsafeInterleaveIO)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as MS

data CTerm
  = CVar String
  | CRef String Word16 [CTerm]
  | CEra
  | CLam String CTerm
  | CApp CTerm CTerm
  | CSup Word32 CTerm CTerm
  | CCtr String [CTerm]
  | CU32 Word32
  | CChr Char
  | COp2 Oper CTerm CTerm
  | CLet LetT String CTerm CTerm
  | CMat MatT CTerm [(String, CTerm)] [(String, [String], CTerm)]
  | CInc CTerm
  | CDec CTerm
  | CCol Word32 Int CTerm -- Internal "selection tag"
  deriving (Eq)

data Nat = Z | S Nat

data SNat :: Nat -> DK.Type where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

type family Add (n :: Nat) (m :: Nat) :: Nat where
  Add Z     m = m
  Add (S n) m = S (Add n m)

addSNat :: SNat n -> SNat m -> SNat (Add n m)
addSNat SZ     m = m
addSNat (SS n) m = SS (addSNat n m)

type family CtrT (n :: Nat) :: DK.Type where
  CtrT Z     = CTerm
  CtrT (S p) = CTerm -> CtrT p

data SomeSNat where SomeSNat :: SNat n -> SomeSNat

toSomeSNat :: Int -> SomeSNat
toSomeSNat 0 = SomeSNat SZ
toSomeSNat n = case toSomeSNat (n-1) of SomeSNat s -> SomeSNat (SS s)

withSNat :: Int -> (forall n. SNat n -> r) -> r
withSNat n f = case toSomeSNat n of SomeSNat s -> f s

data SemiTerm where
  SemiTerm :: SNat k -> (CTerm -> CtrT k) -> SemiTerm

-- An initial term with no hole.
start :: SemiTerm
start = SemiTerm SZ id

-- Replaces the leftmost hole in S, by a term T with N holes.
-- When T has no holes (N=0):
-- - If S has a leftmost hole: fill it.
-- - Else: apply S to T, extending the spine.
-- Examples:
-- - extend (_,_) X     ~> (X,_)     -- fill the leftmost hole
-- - extend (A,_) (_,X) ~> (A,(_,X)) -- fill the leftmost hole
-- - extend (A,B) X     ~> ((A,B) X) -- extend the spine
extend :: SemiTerm -> SNat n -> CtrT n -> SemiTerm
extend (SemiTerm k l) n ctr = case k of
  SZ    -> SemiTerm n               (ini l n ctr)
  SS k' -> SemiTerm (addSNat n k')  (ext k' l n ctr)
  where ini :: (CTerm -> CTerm) -> SNat n -> CtrT n -> (CTerm -> CtrT n)
        ext :: SNat k -> (CTerm -> CtrT (S k)) -> SNat n -> CtrT n -> CtrT (S (Add n k))
        ini   l SZ      ctr = \k -> CApp (l k) ctr
        ini   l (SS n') ctr = \x -> ini l n' (ctr x)
        ext _ l SZ      ctr = l ctr
        ext k l (SS n') ctr = \x -> ext k l n' (ctr x)

-- Fills remaining holes with a placeholder variable.
complete :: SemiTerm -> CTerm
complete (SemiTerm SZ     l) = l (CVar "F")
complete (SemiTerm (SS p) l) = complete (SemiTerm p (l (CVar "X")))


wnf :: CTerm -> CTerm
wnf (CApp f x)   = app f x
wnf (CCol l i x) = col l i x
wnf v            = v

app :: CTerm -> CTerm -> CTerm
app (wnf -> CLam k f)   (wnf -> x) = wnf $ substCTerm k x f
app (wnf -> CSup l x y) (wnf -> v) = CSup l (CApp x (CCol l 0 v)) (CApp y (CCol l 1 v))
app f                   x          = CApp f x

substCTerm :: String -> CTerm -> CTerm -> CTerm
substCTerm var val term = case term of
  CVar v | v == var -> val
  CVar v            -> CVar v
  CRef n f args     -> CRef n f (map (substCTerm var val) args)
  CEra              -> CEra
  CLam v b | v /= var -> CLam v (substCTerm var val b)
  CLam v b          -> CLam v b
  CApp f x          -> CApp (substCTerm var val f) (substCTerm var val x)
  CSup l a b        -> CSup l (substCTerm var val a) (substCTerm var val b)
  CCtr n fields     -> CCtr n (map (substCTerm var val) fields)
  CU32 v            -> CU32 v
  CChr c            -> CChr c
  COp2 o a b        -> COp2 o (substCTerm var val a) (substCTerm var val b)
  CLet m v e b | v /= var -> CLet m v (substCTerm var val e) (substCTerm var val b)
  CLet m v e b      -> CLet m v (substCTerm var val e) b
  CMat t v m c      -> CMat t (substCTerm var val v) 
                          (map (\(k,e) -> (k, substCTerm var val e)) m)
                          (map (\(cn,vs,b) -> if var `elem` vs then (cn,vs,b) else (cn,vs,substCTerm var val b)) c)
  CInc x            -> CInc (substCTerm var val x)
  CDec x            -> CDec (substCTerm var val x)
  CCol l i x        -> CCol l i (substCTerm var val x)

col :: Word32 -> Int -> CTerm -> CTerm
col l i (wnf -> CVar k)     = CVar k
col l i (wnf -> CLam k f)   = CLam k (CCol l i f)
col l i (wnf -> CApp f x)   = CApp f x
col l i (wnf -> CCtr n fs)  = CCtr n (map (CCol l i) fs)
col l i (wnf -> CEra)       = CEra
col l i (wnf -> CSup r x y) = if l == r then [x,y] !! i else CSup r (CCol l i x) (CCol l i y)
col l i (wnf -> CCol r j x) = CCol l i (CCol r j x)
col l i (wnf -> CU32 v)     = CU32 v
col l i (wnf -> CChr c)     = CChr c
col l i (wnf -> COp2 o a b) = COp2 o (CCol l i a) (CCol l i b)
col l i (wnf -> CRef n f args) = CRef n f (map (CCol l i) args)
col l i (wnf -> CLet m v e b) = CLet m v (CCol l i e) (CCol l i b)
col l i (wnf -> CMat t v m c) = CMat t (CCol l i v) 
                                   (map (\(k,e) -> (k, CCol l i e)) m)
                                   (map (\(cn,vs,b) -> (cn,vs,CCol l i b)) c)
col l i (wnf -> CInc x)     = CInc (CCol l i x)
col l i (wnf -> CDec x)     = CDec (CCol l i x)

collapse :: [CTerm] -> SemiTerm -> CTerm
collapse [] semi =
  case complete semi of
    CApp _ x  -> x
    otherwise -> error "unreachable"
collapse ((wnf -> tm) : tms) semi =
  case tm of
    CVar k       -> collapse tms $ extend semi SZ (CVar k)
    CLam k f     -> 
      let f' = substCTerm k (CVar k) f
      in collapse (f' : tms) $ extend semi (SS SZ) (\body -> CLam k body)
    CApp f x     -> collapse (f : x : tms) $ extend semi (SS (SS SZ)) CApp
    CCtr n fields -> 
      let arity = length fields
      in withSNat arity $ \snat ->
        collapse (fields ++ tms) $ extend semi snat (unsafeCoerce $ buildCtr n arity)
    CEra         -> CEra
    CSup l a b   -> CSup l (collapse (a : map (CCol l 0) tms) semi) (collapse (b : map (CCol l 1) tms) semi)
    CCol l i x   -> collapse (x : tms) $ extend semi (SS SZ) (CCol l i)
    CU32 v       -> collapse tms $ extend semi SZ (CU32 v)
    CChr c       -> collapse tms $ extend semi SZ (CChr c)
    COp2 o a b   -> collapse (a : b : tms) $ extend semi (SS (SS SZ)) (COp2 o)
    CRef n f args -> 
      let arity = length args
      in withSNat arity $ \snat ->
        collapse (args ++ tms) $ extend semi snat (unsafeCoerce $ buildRef n f arity)
    CLet m v e b -> collapse (e : b : tms) $ extend semi (SS (SS SZ)) (CLet m v)
    CMat t v m c -> 
      let moves = map snd m
          movesArity = length moves
      in withSNat (1 + movesArity) $ \snat ->
        collapse (v : moves ++ tms) $ extend semi snat (unsafeCoerce $ buildMat t (map fst m) c (1 + movesArity))
    CInc x       -> collapse (x : tms) $ extend semi (SS SZ) CInc
    CDec x       -> collapse (x : tms) $ extend semi (SS SZ) CDec

buildCtr :: forall n. String -> Int -> CtrT n
buildCtr name = go where
  go :: Int -> CtrT n
  go 0 = unsafeCoerce (CCtr name [])
  go n = unsafeCoerce $ \x -> unsafeCoerce $ buildCtrAux name (n-1) [x]
  
  buildCtrAux :: String -> Int -> [CTerm] -> CTerm
  buildCtrAux name 0 acc = CCtr name (reverse acc)
  buildCtrAux name n acc = unsafeCoerce $ \x -> buildCtrAux name (n-1) (x:acc)

buildRef :: forall n. String -> Word16 -> Int -> CtrT n
buildRef name fid = go where
  go :: Int -> CtrT n
  go 0 = unsafeCoerce (CRef name fid [])
  go n = unsafeCoerce $ \x -> unsafeCoerce $ buildRefAux name fid (n-1) [x]
  
  buildRefAux :: String -> Word16 -> Int -> [CTerm] -> CTerm  
  buildRefAux name fid 0 acc = CRef name fid (reverse acc)
  buildRefAux name fid n acc = unsafeCoerce $ \x -> buildRefAux name fid (n-1) (x:acc)

buildMat :: forall n. MatT -> [String] -> [(String, [String], CTerm)] -> Int -> CtrT n
buildMat t moveKeys cases = go where
  go :: Int -> CtrT n
  go 1 = unsafeCoerce $ \v -> CMat t v [] cases
  go n = unsafeCoerce $ buildMatAux t moveKeys cases n Nothing []
  
  buildMatAux :: MatT -> [String] -> [(String, [String], CTerm)] -> Int -> Maybe CTerm -> [CTerm] -> CTerm
  buildMatAux t mk cs 1 (Just v) moves = CMat t v (zip mk (reverse moves)) cs
  buildMatAux t mk cs n Nothing moves = unsafeCoerce $ \v -> buildMatAux t mk cs (n-1) (Just v) moves
  buildMatAux t mk cs n (Just v) moves = unsafeCoerce $ \m -> buildMatAux t mk cs (n-1) (Just v) (m:moves)


-- Dup Collapser
-- -------------
collapseDupsAt :: IM.IntMap [Int] -> ReduceAt -> Book -> Loc -> HVM Core
collapseDupsAt state@(paths) reduceAt book host = unsafeInterleaveIO $ do
  term <- reduceAt book host
  case termTag term of
    t | t == _ERA_ -> do
      return Era

    t | t == _LET_ -> do
      let loc = termLoc term
      let mode = modeT (termLab term)
      name <- return $ "$" ++ show (loc + 0)
      val0 <- collapseDupsAt state reduceAt book (loc + 1)
      bod0 <- collapseDupsAt state reduceAt book (loc + 2)
      return $ Let mode name val0 bod0

    t | t == _LAM_ -> do
      let loc = termLoc term
      name <- return $ "$" ++ show (loc + 0)
      bod0 <- collapseDupsAt state reduceAt book (loc + 0)
      return $ Lam name bod0

    t | t == _APP_ -> do
      let loc = termLoc term
      fun0 <- collapseDupsAt state reduceAt book (loc + 0)
      arg0 <- collapseDupsAt state reduceAt book (loc + 1)
      return $ App fun0 arg0

    t | t == _SUP_ -> do
      let loc = termLoc term
      let lab = termLab term
      case IM.lookup (fromIntegral lab) paths of
        Just (p:ps) -> do
          let newPaths = IM.insert (fromIntegral lab) ps paths
          collapseDupsAt (newPaths) reduceAt book (loc + fromIntegral p)
        _ -> do
          tm00 <- collapseDupsAt state reduceAt book (loc + 0)
          tm11 <- collapseDupsAt state reduceAt book (loc + 1)
          return $ Sup lab tm00 tm11

    t | t == _VAR_ -> do
      let loc = termLoc term
      sub <- got loc
      if termGetBit sub /= 0
      then do
        set (loc + 0) (termRemBit sub)
        collapseDupsAt state reduceAt book (loc + 0)
      else do
        name <- return $ "$" ++ show loc
        return $ Var name

    t | t == _DP0_ -> do
      let loc = termLoc term
      let lab = termLab term
      sb0 <- got (loc+0)
      if termGetBit sb0 /= 0
      then do
        set (loc + 0) (termRemBit sb0)
        collapseDupsAt state reduceAt book (loc + 0)
      else do
        let newPaths = IM.alter (Just . maybe [0] (0:)) (fromIntegral lab) paths
        collapseDupsAt (newPaths) reduceAt book (loc + 0)

    t | t == _DP1_ -> do
      let loc = termLoc term
      let lab = termLab term
      sb1 <- got (loc+0)
      if termGetBit sb1 /= 0
      then do
        set (loc + 0) (termRemBit sb1)
        collapseDupsAt state reduceAt book (loc + 0)
      else do
        let newPaths = IM.alter (Just . maybe [1] (1:)) (fromIntegral lab) paths
        collapseDupsAt (newPaths) reduceAt book (loc + 0)

    t | t == _CTR_ -> do
      let loc = termLoc term
      let lab = termLab term
      let cid = fromIntegral lab
      let nam = MS.findWithDefault "?" cid (cidToCtr book)
      let ari = mget (cidToAri book) cid
      let aux = if ari == 0 then [] else [0 .. ari-1]
      fds0 <- forM aux (\i -> collapseDupsAt state reduceAt book (loc + fromIntegral i))
      return $ Ctr nam fds0

    t | t == _MAT_ -> do
      let loc = termLoc term
      let lab = termLab term
      let cid = fromIntegral lab
      let len = fromIntegral $ mget (cidToLen book) cid
      val0 <- collapseDupsAt state reduceAt book (loc + 0)
      css0 <- forM [0..len-1] $ \i -> do
        let ctr = mget (cidToCtr book) (cid + i)
        let ari = fromIntegral $ mget (cidToAri book) (cid + i)
        let fds = if ari == 0 then [] else ["$" ++ show (loc + 1 + j) | j <- [0..ari-1]]
        bod0 <- collapseDupsAt state reduceAt book (loc + 1 + fromIntegral i)
        return (ctr, fds, bod0)
      return $ Mat (MAT cid) val0 [] css0

    t | t == _IFL_ -> do
      let loc = termLoc term
      let lab = termLab term
      let cid = fromIntegral lab
      val0 <- collapseDupsAt state reduceAt book (loc + 0)
      cs00 <- collapseDupsAt state reduceAt book (loc + 1)
      cs10 <- collapseDupsAt state reduceAt book (loc + 2)
      return $ Mat (IFL cid) val0 [] [(mget (cidToCtr book) cid, [], cs00), ("_", [], cs10)]

    t | t == _SWI_ -> do
      let loc = termLoc term
      let lab = termLab term
      let len = fromIntegral lab
      val0 <- collapseDupsAt state reduceAt book (loc + 0)
      css0 <- forM [0..len-1] $ \i -> do
        bod0 <- collapseDupsAt state reduceAt book (loc + 1 + i)
        return (show i, [], bod0)
      return $ Mat SWI val0 [] css0

    t | t == _W32_ -> do
      let val = termLoc term
      return $ U32 (fromIntegral val)

    t | t == _CHR_ -> do
      let val = termLoc term
      return $ Chr (chr (fromIntegral val))

    t | t == _OPX_ -> do
      let loc = termLoc term
      let opr = toEnum (fromIntegral (termLab term))
      nm00 <- collapseDupsAt state reduceAt book (loc + 0)
      nm10 <- collapseDupsAt state reduceAt book (loc + 1)
      return $ Op2 opr nm00 nm10

    t | t == _OPY_ -> do
      let loc = termLoc term
      let opr = toEnum (fromIntegral (termLab term))
      nm00 <- collapseDupsAt state reduceAt book (loc + 0)
      nm10 <- collapseDupsAt state reduceAt book (loc + 1)
      return $ Op2 opr nm00 nm10

    t | t == _REF_ -> do
      let loc = termLoc term
      let lab = termLab term
      let fid = fromIntegral lab
      let ari = fromIntegral (funArity book fid)
      arg0 <- forM [0..ari-1] (\i -> collapseDupsAt state reduceAt book (loc + i))
      let name = MS.findWithDefault "?" fid (fidToNam book)
      return $ Ref name fid arg0

    t | t == _INC_ -> do
      let loc = termLoc term
      val0 <- collapseDupsAt state reduceAt book (loc + 0)
      return $ Inc val0

    t | t == _DEC_ -> do
      let loc = termLoc term
      val0 <- collapseDupsAt state reduceAt book (loc + 0)
      return $ Dec val0

    tag -> do
      return $ Var ("?" ++ show tag)
      -- exitFailure

-- Tree Collapser
-- --------------
--
-- doCollapseAt :: ReduceAt -> Book -> Loc -> HVM Core
-- doCollapseAt reduceAt book host = do
  -- Step 1: Read the low-level representation into a high-level Core term.
--   initialCore <- collapseDupsAt IM.empty reduceAt book host
  -- Step 2: Convert to our internal CTerm AST.
--   let initialCTerm = coreToCTerm initialCore
  -- Step 3: Run the new, powerful collapse algorithm.
--   let collapsedCTerm = collapse [initialCTerm] start
  -- Step 4: Convert the resulting Sup-tree back to the public Core type.
--   let finalCore = ctermToCore collapsedCTerm
--   return finalCore
--

data SQ a = SQ [a] [a]

sqNew :: SQ a
sqNew = SQ [] []

sqPop :: SQ a -> Maybe (a, SQ a)
sqPop (SQ [] [])     = Nothing
sqPop (SQ [] ys)     = sqPop (SQ (reverse ys) [])
sqPop (SQ (x:xs) ys) = Just (x, SQ xs ys)

sqPut :: a -> SQ a -> SQ a
sqPut x (SQ xs ys) = SQ xs (x:ys)

-- Priority Queue
-- --------------
-- A stable min-heap implemented with a radix tree.
data PQ p v
    = Bin !Word64 !p v !Word64 !(PQ p v) !(PQ p v)
    | Tip !Word64 !p v
    | Nil
    deriving Show

pqPush :: Ord p => Word64 -> p -> v -> PQ p v -> PQ p v
pqPush k1 p1 x1 t = case t of
  Nil -> Tip k1 p1 x1
  (Tip k2 p2 x2)
    | (p1, k1) < (p2, k2) -> link k1 p1 x1 k2 (Tip k2 p2 x2) Nil
    | otherwise           -> link k2 p2 x2 k1 (Tip k1 p1 x1) Nil
  (Bin k2 p2 x2 m l r)
    | nomatch k1 k2 m, (p1, k1) < (p2, k2) -> link k1 p1 x1 k2 (Bin k2 p2 x2 m l r) Nil
    | nomatch k1 k2 m                     -> link k2 p2 x2 k1 (Tip k1 p1 x1) (pqMerge m l r)
    | (p1, k1) < (p2, k2), zero k2 m      -> Bin k1 p1 x1 m (pqPush k2 p2 x2 l) r
    | (p1, k1) < (p2, k2)                 -> Bin k1 p1 x1 m l (pqPush k2 p2 x2 r)
    | zero k1 m                           -> Bin k2 p2 x2 m (pqPush k1 p1 x1 l) r
    | otherwise                           -> Bin k2 p2 x2 m l (pqPush k1 p1 x1 r)
  where
    nomatch :: Word64 -> Word64 -> Word64 -> Bool
    nomatch k1 k2 m =
      let maskW = complement (m-1) `xor` m
      in (k1 .&. maskW) /= (k2 .&. maskW)

    zero :: Word64 -> Word64 -> Bool
    zero i m = i .&. m == 0

    link :: Word64 -> p -> v -> Word64 -> (PQ p v) -> (PQ p v) -> (PQ p v)
    link k p x k' fst snd =
      let m = highestBitMask (k `xor` k')
      in if zero m k'
         then Bin k p x m fst snd
         else Bin k p x m snd fst

    highestBitMask :: Word64 -> Word64
    highestBitMask x1 =
      let x2 = x1 .|. x1 `shiftR` 1
          x3 = x2 .|. x2 `shiftR` 2
          x4 = x3 .|. x3 `shiftR` 4
          x5 = x4 .|. x4 `shiftR` 8
          x6 = x5 .|. x5 `shiftR` 16
          x7 = x6 .|. x6 `shiftR` 32
      in x7 `xor` (x7 `shiftR` 1)

pqPop :: Ord p => PQ p v -> Maybe (Word64, p, v, PQ p v)
pqPop t = case t of
  Nil             -> Nothing
  Tip k p x       -> Just (k, p, x, Nil)
  Bin k p x m l r -> Just (k, p, x, pqMerge m l r)

pqMerge :: Ord p => Word64 -> PQ p v -> PQ p v -> PQ p v
pqMerge m l r = case (l, r) of
  (Nil, r) -> r
  (l, Nil) -> l
  (Tip lk lp lx, Tip rk rp rx)
    | (lp, lk) < (rp, rk) -> Bin lk lp lx m Nil r
    | otherwise           -> Bin rk rp rx m l Nil
  (Tip lk lp lx, Bin rk rp rx rm rl rr)
    | (lp, lk) < (rp, rk) -> Bin lk lp lx m Nil r
    | otherwise           -> Bin rk rp rx m l (pqMerge rm rl rr)
  (Bin lk lp lx lm ll lr, Tip rk rp rx)
    | (lp, lk) < (rp, rk) -> Bin lk lp lx m (pqMerge lm ll lr) r
    | otherwise           -> Bin rk rp rx m l Nil
  (Bin lk lp lx lm ll lr, Bin rk rp rx rm rl rr)
    | (lp, lk) < (rp, rk) -> Bin lk lp lx m (pqMerge lm ll lr) r
    | otherwise           -> Bin rk rp rx m l (pqMerge rm rl rr)

-- Flattener 
-- ---------------------------------------
-- Traverses the CTerm tree, returning a list of non-superposed terms.
-- It correctly handles Inc/Dec to adjust priority without including
-- them in the final output.
flattenPRI :: CTerm -> [CTerm]
flattenPRI term = go 1 (Tip 0 0 term) where
  -- The go loop takes a unique key 'i' and the priority queue 'pq'.
  go :: Word64 -> PQ Int CTerm -> [CTerm]
  go i pq = case pqPop pq of
    Nothing -> []
    Just (_, pri, node, pq') -> case node of
      -- Traversal-control nodes: these are consumed to guide the search.
      CEra -> go i pq'
      CInc t -> go (i + 1) (pqPush i (pri - 1) t pq')
      CDec t -> go (i + 1) (pqPush i (pri + 1) t pq')
      CSup _ a b ->
        let pq1 = pqPush (i + 0) (pri + 1) a pq'
            pq2 = pqPush (i + 1) (pri + 1) b pq1
        in go (i + 2) pq2
      
      -- Value nodes: any other constructor is a terminal value for the
      -- flattening process and is added to the result list.
      val -> val : go i pq'

flatten :: CTerm -> [CTerm]
flatten term = go term (sqNew :: SQ CTerm) where
  go (CSup k a b) sq = go CEra (sqPut b $ sqPut a $ sq)
  go CEra         sq = case sqPop sq of { Just (v,sq) -> go v sq ; Nothing -> [] }
  go x            sq = x : go CEra sq

doCollapseFlatAt :: ReduceAt -> Book -> Loc -> HVM [Core]
doCollapseFlatAt reduceAt book host = do
  initialCore <- collapseDupsAt IM.empty reduceAt book host
  let initialCTerm = coreToCTerm initialCore
  let collapsedCTerm = collapse [initialCTerm] start
  let flattenedCTerms = flattenPRI collapsedCTerm
  let finalCores = map ctermToCore flattenedCTerms
  return finalCores

-- Convert Core to CTerm
coreToCTerm :: Core -> CTerm
coreToCTerm core = case core of
  Var name        -> CVar name
  Ref name fid args -> CRef name fid (map coreToCTerm args)
  Era             -> CEra
  Lam name body   -> CLam name (coreToCTerm body)
  App fun arg     -> CApp (coreToCTerm fun) (coreToCTerm arg)
  Sup lab a b     -> CSup lab (coreToCTerm a) (coreToCTerm b)
  Ctr name fields -> CCtr name (map coreToCTerm fields)
  U32 val         -> CU32 val
  Chr val         -> CChr val
  Op2 oper a b    -> COp2 oper (coreToCTerm a) (coreToCTerm b)
  Let m n v f     -> CLet m n (coreToCTerm v) (coreToCTerm f)
  Mat t v m c     -> CMat t (coreToCTerm v) (map (fmap coreToCTerm) m) (map (\(cn,vs,b) -> (cn,vs,coreToCTerm b)) c)
  Inc x           -> CInc (coreToCTerm x)
  Dec x           -> CDec (coreToCTerm x)
  _               -> error "coreToCTerm: unsupported Core construct"

-- Convert CTerm to Core
ctermToCore :: CTerm -> Core
ctermToCore cterm = case cterm of
  CVar name       -> Var name
  CRef name fid args -> Ref name fid (map ctermToCore args)
  CEra            -> Era
  CLam name body  -> Lam name (ctermToCore body)
  CApp fun arg    -> App (ctermToCore fun) (ctermToCore arg)
  CSup lab a b    -> Sup lab (ctermToCore a) (ctermToCore b)
  CCtr name fields-> Ctr name (map ctermToCore fields)
  CU32 val        -> U32 val
  CChr val        -> Chr val
  COp2 oper a b   -> Op2 oper (ctermToCore a) (ctermToCore b)
  CLet m n v f    -> Let m n (ctermToCore v) (ctermToCore f)
  CMat t v m c    -> Mat t (ctermToCore v) (map (fmap ctermToCore) m) (map (\(cn,vs,b) -> (cn,vs,ctermToCore b)) c)
  CInc x          -> Inc (ctermToCore x)
  CDec x          -> Dec (ctermToCore x)
  _               -> error "ctermToCore: unsupported CTerm construct"
