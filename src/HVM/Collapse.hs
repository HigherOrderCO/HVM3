{-./Type.hs-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module HVM.Collapse where

import Control.Monad (ap, forM, forM_)
import Control.Monad.IO.Class
import Data.Char (chr, ord)
import qualified Data.Kind as DK
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

-- Recursive, type-safe builder functions
buildCtr :: SNat n -> String -> CtrT n
buildCtr s k = buildCtr' s [] where
  buildCtr' :: SNat m -> [CTerm] -> CtrT m
  buildCtr' SZ     acc = CCtr k (reverse acc)
  buildCtr' (SS s') acc = \arg -> buildCtr' s' (arg : acc)

buildRef :: SNat n -> String -> Word16 -> CtrT n
buildRef s k i = buildRef' s [] where
  buildRef' :: SNat m -> [CTerm] -> CtrT m
  buildRef' SZ      acc = CRef k i (reverse acc)
  buildRef' (SS s') acc = \arg -> buildRef' s' (arg : acc)

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
wnf (CCol l i x) = col l i x
wnf v            = v

col :: Word32 -> Int -> CTerm -> CTerm
col l i (wnf -> CSup r x y)   = if l == r then [x, y] !! i else CSup r (col l i x) (col l i y)
col l i (wnf -> CLam n f)     = CLam n (col l i f)
col l i (wnf -> CApp f x)     = CApp (col l i f) (col l i x)
col l i (wnf -> CCtr k xs)    = CCtr k (map (col l i) xs)
col l i (wnf -> CRef k r xs)  = CRef k r (map (col l i) xs)
col l i (wnf -> COp2 o a b)   = COp2 o (col l i a) (col l i b)
col l i (wnf -> CLet m n v b) = CLet m n (col l i v) (col l i b)
col l i (wnf -> CMat t s ms cs) = CMat t (col l i s) (map (fmap (col l i)) ms) (map (\(c,vs,b) -> (c,vs,col l i b)) cs)
col l i (wnf -> CInc x)       = CInc (col l i x)
col l i (wnf -> CDec x)       = CDec (col l i x)
col l i (wnf -> x)            = x

collapse :: [CTerm] -> SemiTerm -> CTerm
collapse [] semi = case complete semi of CApp _ x -> x; term -> term
collapse ((wnf -> tm) : tms) semi =
  case tm of
    CSup l a b  -> CSup l (collapse (a : map (CCol l 0) tms) semi)
                        (collapse (b : map (CCol l 1) tms) semi)
    CCol l i x  -> collapse (x : tms) $ extend semi (SS SZ) (CCol l i)
    CVar k      -> collapse tms $ extend semi SZ (CVar k)
    CEra        -> CEra
    CLam x f    -> collapse (f : tms) $ extend semi (SS SZ) (\b -> CLam x b)
    CApp f x    -> collapse (f : x : tms) semi
    COp2 o a b  -> collapse (a : b : tms) $ extend semi (SS (SS SZ)) (COp2 o)
    CCtr k xs   -> withSNat (length xs) $ \sNat -> collapse (xs ++ tms) $ extend semi sNat (buildCtr sNat k)
    CRef k i xs -> withSNat (length xs) $ \sNat -> collapse (xs ++ tms) $ extend semi sNat (buildRef sNat k i)
    CU32 n      -> collapse tms $ extend semi SZ (CU32 n)
    CChr c      -> collapse tms $ extend semi SZ (CChr c)
    CInc x      -> collapse (x : tms) $ extend semi (SS SZ) CInc
    CDec x      -> collapse (x : tms) $ extend semi (SS SZ) CDec
    CLet m k v b-> collapse (v : b : tms) $ extend semi (SS (SS SZ)) (CLet m k)
    CMat t v m c-> collapse tms $ extend semi SZ (CMat t v m c)
    _           -> tm



-- The Collapse Monad 
-- ------------------
-- See: https://gist.github.com/VictorTaelin/60d3bc72fb4edefecd42095e44138b41

-- A bit-string
data Bin
  = O Bin
  | I Bin
  | E
  deriving Show

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

doCollapseAt :: ReduceAt -> Book -> Loc -> HVM Core
doCollapseAt reduceAt book host = do
  -- Step 1: Read the low-level representation into a high-level Core term.
  initialCore <- collapseDupsAt IM.empty reduceAt book host
  -- Step 2: Convert to our internal CTerm AST.
  let initialCTerm = coreToCTerm initialCore
  -- Step 3: Run the new, powerful collapse algorithm.
  let collapsedCTerm = collapse [initialCTerm] start
  -- Step 4: Convert the resulting Sup-tree back to the public Core type.
  let finalCore = ctermToCore collapsedCTerm
  return finalCore


data SQ a = SQ [a] [a]

sqNew :: SQ a
sqNew = SQ [] []

sqPop :: SQ a -> Maybe (a, SQ a)
sqPop (SQ [] [])     = Nothing
sqPop (SQ [] ys)     = sqPop (SQ (reverse ys) [])
sqPop (SQ (x:xs) ys) = Just (x, SQ xs ys)

sqPut :: a -> SQ a -> SQ a
sqPut x (SQ xs ys) = SQ xs (x:ys)

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
  let flattenedCTerms = flatten collapsedCTerm
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
