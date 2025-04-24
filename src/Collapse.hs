{-./Type.hs-}

module Collapse where

import Control.Monad (ap, forM, forM_)
import Control.Monad.IO.Class
import Data.Char (chr, ord)
import Data.IORef
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

-- Priority Queue
-- --------------

-- | A stable min-heap that orders first by priority, then by insertion order.
data PQ a
  = PQLeaf
  | PQNode (Int, Int, a) (PQ a) (PQ a)  -- (priority, seq#, payload)
  deriving Show

pqUnion :: PQ a -> PQ a -> PQ a
pqUnion PQLeaf h = h
pqUnion h PQLeaf = h
pqUnion h1@(PQNode x@(p1,i1,_) l1 r1) h2@(PQNode y@(p2,i2,_) l2 r2)
  | p1 <  p2  = PQNode x (pqUnion r1 h2) l1
  | p1 >  p2  = PQNode y (pqUnion h1 r2) l2
  | i1 <= i2  = PQNode x (pqUnion r1 h2) l1
  | otherwise = PQNode y (pqUnion h1 r2) l2

pqPut :: (Int, Int, a) -> PQ a -> PQ a
pqPut x = pqUnion (PQNode x PQLeaf PQLeaf)

pqPop :: PQ a -> Maybe ((Int, Int, a), PQ a)
pqPop PQLeaf         = Nothing
pqPop (PQNode x l r) = Just (x, pqUnion l r)

-- Priority-Queue Flattener
-- ------------------------
-- * priority starts at 0
-- * since PQ is a min-queue, we invert the scoers:
-- * passing through (CInc t) subs 1 ; (CDec t) adds 1
-- * when no Inc/Dec are present every node has priority 0,
--   hence the order matches plain BFS exactly (stable heap).

flattenPRI :: Collapse a -> [a]
flattenPRI term = go 1 (pqPut (0, 0, term) PQLeaf) where
  go :: Int -> PQ (Collapse a) -> [a]
  go idx pq = case pqPop pq of
    Nothing -> []
    Just ((pr, _seq, node), pq') -> case node of
      CEra   -> go idx pq'
      CVal v -> v : go idx pq'
      CInc t -> go (idx + 1) (pqPut (pr - 1, idx, t) pq')
      CDec t -> go (idx + 1) (pqPut (pr + 1, idx, t) pq')
      CSup _ a b ->
        let pq1 = pqPut (pr, idx    , b) pq'   -- push right first
            pq2 = pqPut (pr, idx + 1, a) pq1   -- then left (matches BFS order)
        in  go (idx + 2) pq2

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
