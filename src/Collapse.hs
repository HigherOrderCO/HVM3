{-./Type.hs-}

module Collapse where

import Control.Monad (ap, forM, forM_)
import Control.Monad.IO.Class
import Data.Char (chr, ord)
import Data.IORef
import Data.Bits ((.&.), xor, (.|.), complement, shiftR)
import Data.Word
import Debug.Trace
import GHC.Conc
import Foreign
import Type
import System.Exit (exitFailure)
import System.IO.Unsafe (unsafeInterleaveIO)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as MS

-- The Collapse Monad 
-- ------------------
-- See: https://gist.github.com/VictorTaelin/60d3bc72fb4edefecd42095e44138b41

-- A bit-string
data Bin
  = O Bin
  | I Bin
  | E
  deriving Show

-- A Collapse is a tree of superposed values
data Collapse a
  = CSup Lab (Collapse a) (Collapse a)
  | CInc (Collapse a)
  | CDec (Collapse a)
  | CVal a
  | CEra
  deriving Show

bind :: Collapse a -> (a -> Collapse b) -> Collapse b
bind k f = fork k IM.empty where
  -- fork :: Collapse a -> IntMap (Bin -> Bin) -> Collapse b
  fork CEra         paths = CEra
  fork (CVal v)     paths = pass (f v) (IM.map (\x -> x E) paths)
  fork (CInc x)     paths = CInc (fork x paths)
  fork (CDec x)     paths = CDec (fork x paths)
  fork (CSup k x y) paths =
    let lft = fork x $ IM.alter (\x -> Just (maybe (putO id) putO x)) (fromIntegral k) paths in
    let rgt = fork y $ IM.alter (\x -> Just (maybe (putI id) putI x)) (fromIntegral k) paths in
    CSup k lft rgt 
  -- pass :: Collapse b -> IntMap Bin -> Collapse b
  pass CEra         paths = CEra
  pass (CVal v)     paths = CVal v
  pass (CInc x)     paths = CInc (pass x paths)
  pass (CDec x)     paths = CDec (pass x paths)
  pass (CSup k x y) paths = case IM.lookup (fromIntegral k) paths of
    Just (O p) -> pass x (IM.insert (fromIntegral k) p paths)
    Just (I p) -> pass y (IM.insert (fromIntegral k) p paths)
    Just E     -> CSup k (pass x paths) (pass y paths)
    Nothing    -> CSup k (pass x paths) (pass y paths)
  -- putO :: (Bin -> Bin) -> (Bin -> Bin)
  putO bs = \x -> bs (O x)
  -- putI :: (Bin -> Bin) -> (Bin -> Bin) 
  putI bs = \x -> bs (I x)

instance Functor Collapse where
  fmap f (CVal v)     = CVal (f v)
  fmap f (CSup k x y) = CSup k (fmap f x) (fmap f y)
  fmap f (CInc x)     = CInc (fmap f x)
  fmap f (CDec x)     = CDec (fmap f x)
  fmap _ CEra         = CEra

instance Applicative Collapse where
  pure  = CVal
  (<*>) = ap

instance Monad Collapse where
  return = pure
  (>>=)  = bind

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
      return $ Var "?"
      -- exitFailure

-- Sup Collapser
-- -------------

collapseSups :: Book -> Core -> Collapse Core

collapseSups book core = case core of

  Var name -> do
    return $ Var name

  Ref name fid args -> do
    args <- mapM (collapseSups book) args
    return $ Ref name fid args

  Lam name body -> do
    body <- collapseSups book body
    return $ Lam name body

  App fun arg -> do
    fun <- collapseSups book fun
    arg <- collapseSups book arg
    return $ App fun arg

  Dup lab x y val body -> do
    val <- collapseSups book val
    body <- collapseSups book body
    return $ Dup lab x y val body

  Ctr nam fields -> do
    fields <- mapM (collapseSups book) fields
    return $ Ctr nam fields

  Mat kin val mov css -> do
    val <- collapseSups book val
    mov <- mapM (\(key, expr) -> do
      expr <- collapseSups book expr
      return (key, expr)) mov
    css <- mapM (\(ctr, fds, bod) -> do
      bod <- collapseSups book bod
      return (ctr, fds, bod)) css
    return $ Mat kin val mov css

  U32 val -> do
    return $ U32 val

  Chr val -> do
    return $ Chr val

  Op2 op x y -> do
    x <- collapseSups book x
    y <- collapseSups book y
    return $ Op2 op x y

  Let mode name val body -> do
    val <- collapseSups book val
    body <- collapseSups book body
    return $ Let mode name val body

  Era -> do
    CEra

  Sup lab tm0 tm1 -> do
    let tm0' = collapseSups book tm0
    let tm1' = collapseSups book tm1
    CSup lab tm0' tm1'

  Inc val -> do
    let val' = collapseSups book val
    CInc val'

  Dec val -> do
    let val' = collapseSups book val
    CDec val'

-- Tree Collapser
-- --------------

doCollapseAt :: ReduceAt -> Book -> Loc -> HVM (Collapse Core)
doCollapseAt reduceAt book host = do
  -- namesRef <- newIORef MS.empty
  let state = (IM.empty)
  core <- collapseDupsAt state reduceAt book host
  return $ collapseSups book core

-- Simple Queue
-- ------------
-- Allows pushing to an end, and popping from another.
-- Simple purely functional implementation.
-- Includes sqPop and sqPut.

data SQ a = SQ [a] [a]

sqPop :: SQ a -> Maybe (a, SQ a)
sqPop (SQ [] [])     = Nothing
sqPop (SQ [] ys)     = sqPop (SQ (reverse ys) [])
sqPop (SQ (x:xs) ys) = Just (x, SQ xs ys)

sqPut :: a -> SQ a -> SQ a
sqPut x (SQ xs ys) = SQ xs (x:ys)

-- Priority Queue
-- --------------
-- A stable min-heap implemented with a radix tree.
-- Orders by an Int priority and a unique Word64 key.
-- Based on IntPSQ from the the psqueues library (https://hackage.haskell.org/package/psqueues-0.2.8.1)

data PQ p v
    = Bin !Word64 !p v !Word64 !(PQ p v) !(PQ p v)
    | Tip !Word64 !p v
    | Nil

pqPush :: Ord p => Word64 -> p -> v -> PQ p v -> PQ p v
pqPush k1 p1 x1 t = case t of
  Nil                                      -> Tip k1 p1 x1
  (Tip k2 p2 x2)
    | (p1, k1) < (p2, k2)                  -> link k1 p1 x1 k2 (Tip k2 p2 x2) Nil
    | otherwise                            -> link k2 p2 x2 k1 (Tip k1 p1 x1) Nil
  (Bin k2 p2 x2 m l r)
    | nomatch k1 k2 m, (p1, k1) < (p2, k2) -> link k1 p1 x1 k2 (Bin k2 p2 x2 m l r) Nil
    | nomatch k1 k2 m                      -> link k2 p2 x2 k1 (Tip k1 p1 x1) (pqMerge m l r)
    | (p1, k1) < (p2, k2), zero k2 m       -> Bin k1 p1 x1 m (pqPush k2 p2 x2 l) r
    | (p1, k1) < (p2, k2)                  -> Bin k1 p1 x1 m l (pqPush k2 p2 x2 r)
    | zero k1 m                            -> Bin k2 p2 x2 m (pqPush k1 p1 x1 l) r
    | otherwise                            -> Bin k2 p2 x2 m l (pqPush k1 p1 x1 r)
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
  (Nil, r)                     -> r
  (l, Nil)                     -> l
  (Tip lk lp lx, Tip rk rp rx)
    | (lp, lk) < (rp, rk)      -> Bin lk lp lx m Nil r
    | otherwise                -> Bin rk rp rx m l Nil
  (Tip lk lp lx, Bin rk rp rx rm rl rr)
    | (lp, lk) < (rp, rk)      -> Bin lk lp lx m Nil r
    | otherwise                -> Bin rk rp rx m l (pqMerge rm rl rr)
  (Bin lk lp lx lm ll lr, Tip rk rp rx)
    | (lp, lk) < (rp, rk)      -> Bin lk lp lx m (pqMerge lm ll lr) r
    | otherwise                -> Bin rk rp rx m l Nil
  (Bin lk lp lx lm ll lr, Bin rk rp rx rm rl rr)
    | (lp, lk) < (rp, rk)      -> Bin lk lp lx m (pqMerge lm ll lr) r
    | otherwise                -> Bin rk rp rx m l (pqMerge rm rl rr)


-- Flattener
-- ---------

flattenDFS :: Collapse a -> [a]
flattenDFS (CSup k a b) = flatten a ++ flatten b
flattenDFS (CVal x)     = [x]
flattenDFS (CInc x)     = flattenDFS x
flattenDFS (CDec x)     = flattenDFS x
flattenDFS CEra         = []

flattenBFS :: Collapse a -> [a]
flattenBFS term = go term (SQ [] [] :: SQ (Collapse a)) where
  go (CSup k a b) sq = go CEra (sqPut b $ sqPut a $ sq)
  go (CVal x)     sq = x : go CEra sq
  go (CInc x)     sq = go x sq
  go (CDec x)     sq = go x sq
  go CEra         sq = case sqPop sq of
    Just (v,sq) -> go v sq
    Nothing     -> []

-- Priority-Queue Flattener
-- ------------------------
-- * priority starts at 0
-- * since PQ is a min-queue, we invert the scoers:
-- * passing through (CInc t) subs 1 ; (CDec t) adds 1
-- * when no Inc/Dec are present every node has priority == depth
--   hence the order matches plain BFS exactly (stable heap).

flattenPRI :: Collapse a -> [a]
flattenPRI term = go 1 (Tip 0 0 term) where
  go i pq = case pqPop pq of
    Nothing -> []
    Just (_, pri, node, pq') -> case node of
      CEra   -> go i pq'
      CVal v -> v : go i pq'
      CInc t -> go (i + 1) (pqPush i (pri - 1) t pq')
      CDec t -> go (i + 1) (pqPush i (pri + 1) t pq')
      CSup _ a b ->
        let pq1 = (pqPush (i + 0) (pri + 1) a pq')
            pq2 = (pqPush (i + 1) (pri + 1) b pq1)
        in go (i + 2) pq2

-- Default Flattener
-- -----------------

flatten :: Collapse a -> [a]
flatten = flattenPRI

-- Flat Collapser
-- --------------

doCollapseFlatAt :: ReduceAt -> Book -> Loc -> HVM [Core]
doCollapseFlatAt reduceAt book host = do
  coll <- doCollapseAt reduceAt book host
  return $ flatten coll
