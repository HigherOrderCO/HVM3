{-./Type.hs-}
{-./Inject.hs-}
{-./Extract.hs-}

-- module Collapse where

-- import Control.Monad (ap, forM, forM_)
-- import Control.Monad.IO.Class
-- import Data.Char (chr, ord)
-- import Data.IORef
-- import Data.Word
-- import Debug.Trace
-- import GHC.Conc
-- import Foreign
-- import Type
-- import System.Exit (exitFailure)
-- import System.IO.Unsafe (unsafeInterleaveIO)
-- import qualified Data.IntMap.Strict as IM
-- import qualified Data.Map.Strict as MS

-- -- The Collapse Monad 
-- -- ------------------
-- -- See: https://gist.github.com/VictorTaelin/60d3bc72fb4edefecd42095e44138b41

-- -- A bit-string
-- data Bin
  -- = O Bin
  -- | I Bin
  -- | E
  -- deriving Show

-- -- A Collapse is a tree of superposed values
-- data Collapse a = CSup Lab (Collapse a) (Collapse a) | CVal a | CEra
  -- deriving Show

-- bind :: Collapse a -> (a -> Collapse b) -> Collapse b
-- bind k f = fork k IM.empty where
  -- -- fork :: Collapse a -> IntMap (Bin -> Bin) -> Collapse b
  -- fork CEra         paths = CEra
  -- fork (CVal v)     paths = pass (f v) (IM.map (\x -> x E) paths)
  -- fork (CSup k x y) paths =
    -- let lft = fork x $ IM.alter (\x -> Just (maybe (putO id) putO x)) (fromIntegral k) paths in
    -- let rgt = fork y $ IM.alter (\x -> Just (maybe (putI id) putI x)) (fromIntegral k) paths in
    -- CSup k lft rgt 
  -- -- pass :: Collapse b -> IntMap Bin -> Collapse b
  -- pass CEra         paths = CEra
  -- pass (CVal v)     paths = CVal v
  -- pass (CSup k x y) paths = case IM.lookup (fromIntegral k) paths of
    -- Just (O p) -> pass x (IM.insert (fromIntegral k) p paths)
    -- Just (I p) -> pass y (IM.insert (fromIntegral k) p paths)
    -- Just E     -> CSup k (pass x paths) (pass y paths)
    -- Nothing    -> CSup k (pass x paths) (pass y paths)
  -- -- putO :: (Bin -> Bin) -> (Bin -> Bin)
  -- putO bs = \x -> bs (O x)
  -- -- putI :: (Bin -> Bin) -> (Bin -> Bin) 
  -- putI bs = \x -> bs (I x)

-- instance Functor Collapse where
  -- fmap f (CVal v)     = CVal (f v)
  -- fmap f (CSup k x y) = CSup k (fmap f x) (fmap f y)
  -- fmap _ CEra         = CEra

-- instance Applicative Collapse where
  -- pure  = CVal
  -- (<*>) = ap

-- instance Monad Collapse where
  -- return = pure
  -- (>>=)  = bind

-- -- Dup Collapser
-- -- -------------

-- collapseDupsAt :: IM.IntMap [Int] -> ReduceAt -> Book -> Loc -> HVM Core

-- collapseDupsAt state@(paths) reduceAt book host = unsafeInterleaveIO $ do
  -- term <- reduceAt book host
  -- case termTag term of
    -- t | t == _ERA_ -> do
      -- return Era

    -- t | t == _LET_ -> do
      -- let loc = termLoc term
      -- let mode = modeT (termLab term)
      -- name <- return $ "$" ++ show (loc + 0)
      -- val0 <- collapseDupsAt state reduceAt book (loc + 1)
      -- bod0 <- collapseDupsAt state reduceAt book (loc + 2)
      -- return $ Let mode name val0 bod0

    -- t | t == _LAM_ -> do
      -- let loc = termLoc term
      -- name <- return $ "$" ++ show (loc + 0)
      -- bod0 <- collapseDupsAt state reduceAt book (loc + 0)
      -- return $ Lam name bod0

    -- t | t == _APP_ -> do
      -- let loc = termLoc term
      -- fun0 <- collapseDupsAt state reduceAt book (loc + 0)
      -- arg0 <- collapseDupsAt state reduceAt book (loc + 1)
      -- return $ App fun0 arg0

    -- t | t == _SUP_ -> do
      -- let loc = termLoc term
      -- let lab = termLab term
      -- case IM.lookup (fromIntegral lab) paths of
        -- Just (p:ps) -> do
          -- let newPaths = IM.insert (fromIntegral lab) ps paths
          -- collapseDupsAt (newPaths) reduceAt book (loc + fromIntegral p)
        -- _ -> do
          -- tm00 <- collapseDupsAt state reduceAt book (loc + 0)
          -- tm11 <- collapseDupsAt state reduceAt book (loc + 1)
          -- return $ Sup lab tm00 tm11

    -- t | t == _VAR_ -> do
      -- let loc = termLoc term
      -- sub <- got loc
      -- if termGetBit sub /= 0
      -- then do
        -- set (loc + 0) (termRemBit sub)
        -- collapseDupsAt state reduceAt book (loc + 0)
      -- else do
        -- name <- return $ "$" ++ show loc
        -- return $ Var name

    -- t | t == _DP0_ -> do
      -- let loc = termLoc term
      -- let lab = termLab term
      -- sb0 <- got (loc+0)
      -- if termGetBit sb0 /= 0
      -- then do
        -- set (loc + 0) (termRemBit sb0)
        -- collapseDupsAt state reduceAt book (loc + 0)
      -- else do
        -- let newPaths = IM.alter (Just . maybe [0] (0:)) (fromIntegral lab) paths
        -- collapseDupsAt (newPaths) reduceAt book (loc + 0)

    -- t | t == _DP1_ -> do
      -- let loc = termLoc term
      -- let lab = termLab term
      -- sb1 <- got (loc+0)
      -- if termGetBit sb1 /= 0
      -- then do
        -- set (loc + 0) (termRemBit sb1)
        -- collapseDupsAt state reduceAt book (loc + 0)
      -- else do
        -- let newPaths = IM.alter (Just . maybe [1] (1:)) (fromIntegral lab) paths
        -- collapseDupsAt (newPaths) reduceAt book (loc + 0)

    -- t | t == _CTR_ -> do
      -- let loc = termLoc term
      -- let lab = termLab term
      -- let cid = fromIntegral lab
      -- let nam = MS.findWithDefault "?" cid (cidToCtr book)
      -- let ari = mget (cidToAri book) cid
      -- let aux = if ari == 0 then [] else [0 .. ari-1]
      -- fds0 <- forM aux (\i -> collapseDupsAt state reduceAt book (loc + fromIntegral i))
      -- return $ Ctr nam fds0

    -- t | t == _MAT_ -> do
      -- let loc = termLoc term
      -- let lab = termLab term
      -- let cid = fromIntegral lab
      -- let len = fromIntegral $ mget (cidToLen book) cid
      -- val0 <- collapseDupsAt state reduceAt book (loc + 0)
      -- css0 <- forM [0..len-1] $ \i -> do
        -- let ctr = mget (cidToCtr book) (cid + i)
        -- let ari = fromIntegral $ mget (cidToAri book) (cid + i)
        -- let fds = if ari == 0 then [] else ["$" ++ show (loc + 1 + j) | j <- [0..ari-1]]
        -- bod0 <- collapseDupsAt state reduceAt book (loc + 1 + fromIntegral i)
        -- return (ctr, fds, bod0)
      -- return $ Mat (MAT cid) val0 [] css0

    -- t | t == _IFL_ -> do
      -- let loc = termLoc term
      -- let lab = termLab term
      -- let cid = fromIntegral lab
      -- val0 <- collapseDupsAt state reduceAt book (loc + 0)
      -- cs00 <- collapseDupsAt state reduceAt book (loc + 1)
      -- cs10 <- collapseDupsAt state reduceAt book (loc + 2)
      -- return $ Mat (IFL cid) val0 [] [(mget (cidToCtr book) cid, [], cs00), ("_", [], cs10)]

    -- t | t == _SWI_ -> do
      -- let loc = termLoc term
      -- let lab = termLab term
      -- let len = fromIntegral lab
      -- val0 <- collapseDupsAt state reduceAt book (loc + 0)
      -- css0 <- forM [0..len-1] $ \i -> do
        -- bod0 <- collapseDupsAt state reduceAt book (loc + 1 + i)
        -- return (show i, [], bod0)
      -- return $ Mat SWI val0 [] css0

    -- t | t == _W32_ -> do
      -- let val = termLoc term
      -- return $ W32 (fromIntegral val)

    -- t | t == _CHR_ -> do
      -- let val = termLoc term
      -- return $ Chr (chr (fromIntegral val))

    -- t | t == _OPX_ -> do
      -- let loc = termLoc term
      -- let opr = toEnum (fromIntegral (termLab term))
      -- nm00 <- collapseDupsAt state reduceAt book (loc + 0)
      -- nm10 <- collapseDupsAt state reduceAt book (loc + 1)
      -- return $ Op2 opr nm00 nm10

    -- t | t == _OPY_ -> do
      -- let loc = termLoc term
      -- let opr = toEnum (fromIntegral (termLab term))
      -- nm00 <- collapseDupsAt state reduceAt book (loc + 0)
      -- nm10 <- collapseDupsAt state reduceAt book (loc + 1)
      -- return $ Op2 opr nm00 nm10

    -- t | t == _REF_ -> do
      -- let loc = termLoc term
      -- let lab = termLab term
      -- let fid = fromIntegral lab
      -- let ari = fromIntegral (funArity book fid)
      -- arg0 <- forM [0..ari-1] (\i -> collapseDupsAt state reduceAt book (loc + i))
      -- let name = MS.findWithDefault "?" fid (fidToNam book)
      -- return $ Ref name fid arg0

    -- tag -> do
      -- return $ Var "?"
      -- -- exitFailure

-- -- Sup Collapser
-- -- -------------

-- collapseSups :: Book -> Core -> Collapse Core

-- collapseSups book core = case core of

  -- Var name -> do
    -- return $ Var name

  -- Ref name fid args -> do
    -- args <- mapM (collapseSups book) args
    -- return $ Ref name fid args

  -- Lam name body -> do
    -- body <- collapseSups book body
    -- return $ Lam name body

  -- App fun arg -> do
    -- fun <- collapseSups book fun
    -- arg <- collapseSups book arg
    -- return $ App fun arg

  -- Dup lab x y val body -> do
    -- val <- collapseSups book val
    -- body <- collapseSups book body
    -- return $ Dup lab x y val body

  -- Ctr nam fields -> do
    -- fields <- mapM (collapseSups book) fields
    -- return $ Ctr nam fields

  -- Mat kin val mov css -> do
    -- val <- collapseSups book val
    -- mov <- mapM (\ (key, expr) -> do
      -- expr <- collapseSups book expr
      -- return (key, expr)) mov
    -- css <- mapM (\ (ctr, fds, bod) -> do
      -- bod <- collapseSups book bod
      -- return (ctr, fds, bod)) css
    -- return $ Mat kin val mov css

  -- W32 val -> do
    -- return $ W32 val

  -- Chr val -> do
    -- return $ Chr val

  -- Op2 op x y -> do
    -- x <- collapseSups book x
    -- y <- collapseSups book y
    -- return $ Op2 op x y

  -- Let mode name val body -> do
    -- val <- collapseSups book val
    -- body <- collapseSups book body
    -- return $ Let mode name val body

  -- Era -> do
    -- CEra

  -- Sup lab tm0 tm1 -> do
    -- let tm0' = collapseSups book tm0
    -- let tm1' = collapseSups book tm1
    -- CSup lab tm0' tm1'

-- -- Tree Collapser
-- -- --------------

-- doCollapseAt :: ReduceAt -> Book -> Loc -> HVM (Collapse Core)
-- doCollapseAt reduceAt book host = do
  -- -- namesRef <- newIORef MS.empty
  -- let state = (IM.empty)
  -- core <- collapseDupsAt state reduceAt book host
  -- return $ collapseSups book core

-- -- Priority Queue
-- -- --------------

-- data PQ a
  -- = PQLeaf
  -- | PQNode (Word64, a) (PQ a) (PQ a)
  -- deriving (Show)

-- pqUnion :: PQ a -> PQ a -> PQ a
-- pqUnion PQLeaf heap = heap
-- pqUnion heap PQLeaf = heap
-- pqUnion heap1@(PQNode (k1,v1) l1 r1) heap2@(PQNode (k2,v2) l2 r2)
  -- | k1 <= k2  = PQNode (k1,v1) (pqUnion heap2 r1) l1
  -- | otherwise = PQNode (k2,v2) (pqUnion heap1 r2) l2

-- pqPop :: PQ a -> Maybe ((Word64, a), PQ a)
-- pqPop PQLeaf         = Nothing
-- pqPop (PQNode x l r) = Just (x, pqUnion l r)

-- pqPut :: (Word64,a) -> PQ a -> PQ a
-- pqPut (k,v) = pqUnion (PQNode (k,v) PQLeaf PQLeaf)

-- -- Simple Queue
-- -- ------------
-- -- Allows pushing to an end, and popping from another.
-- -- Simple purely functional implementation.
-- -- Includes sqPop and sqPut.

-- data SQ a = SQ [a] [a]

-- sqPop :: SQ a -> Maybe (a, SQ a)
-- sqPop (SQ [] [])     = Nothing
-- sqPop (SQ [] ys)     = sqPop (SQ (reverse ys) [])
-- sqPop (SQ (x:xs) ys) = Just (x, SQ xs ys)

-- sqPut :: a -> SQ a -> SQ a
-- sqPut x (SQ xs ys) = SQ xs (x:ys)

-- -- Flattener
-- -- ---------

-- flattenDFS :: Collapse a -> [a]
-- flattenDFS (CSup k a b) = flatten a ++ flatten b
-- flattenDFS (CVal x)     = [x]
-- flattenDFS CEra         = []

-- flattenBFS :: Collapse a -> [a]
-- flattenBFS term = go term (SQ [] [] :: SQ (Collapse a)) where
  -- go (CSup k a b) sq = go CEra (sqPut b $ sqPut a $ sq)
  -- go (CVal x)     sq = x : go CEra sq
  -- go CEra         sq = case sqPop sq of
    -- Just (v,sq) -> go v sq
    -- Nothing     -> []

-- flattenPQ :: Collapse a -> [a]
-- flattenPQ term = go term (PQLeaf :: PQ (Collapse a)) where
  -- go (CSup k a b) pq = go CEra (pqPut (fromIntegral k,a) $ pqPut (fromIntegral k,b) $ pq)
  -- go (CVal x)     pq = x : go CEra pq
  -- go CEra         pq = case pqPop pq of
    -- Just ((k,v),pq) -> go v pq
    -- Nothing         -> []

-- flatten :: Collapse a -> [a]
-- flatten = flattenBFS

-- -- Flat Collapser
-- -- --------------

-- doCollapseFlatAt :: ReduceAt -> Book -> Loc -> HVM [Core]
-- doCollapseFlatAt reduceAt book host = do
  -- coll <- doCollapseAt reduceAt book host
  -- return $ flatten coll

-- UPDATED:

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
data Collapse a = CSup Lab (Collapse a) (Collapse a) | CVal a | CEra
  deriving Show

bind :: Collapse a -> (a -> Collapse b) -> Collapse b
bind k f = fork k IM.empty where
  -- fork :: Collapse a -> IntMap (Bin -> Bin) -> Collapse b
  fork CEra         paths = CEra
  fork (CVal v)     paths = pass (f v) (IM.map (\x -> x E) paths)
  fork (CSup k x y) paths =
    let lft = fork x $ IM.alter (\x -> Just (maybe (putO id) putO x)) (fromIntegral k) paths in
    let rgt = fork y $ IM.alter (\x -> Just (maybe (putI id) putI x)) (fromIntegral k) paths in
    CSup k lft rgt 
  -- pass :: Collapse b -> IntMap Bin -> Collapse b
  pass CEra         paths = CEra
  pass (CVal v)     paths = CVal v
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
      val0 <- collapseDupsAt state reduceAt book (loc + 0)
      bod0 <- collapseDupsAt state reduceAt book (loc + 1)
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

    t | t == _REF_ -> do
      let loc = termLoc term
      let lab = termLab term
      let fid = fromIntegral lab
      let ari = fromIntegral (funArity book fid)
      arg0 <- forM [0..ari-1] (\i -> collapseDupsAt state reduceAt book (loc + i))
      let name = MS.findWithDefault "?" fid (fidToNam book)
      return $ Ref name fid arg0

    t | t == _SET_ -> do
      return Set

    t | t == _EMP_ -> do
      return Emp

    t | t == _EFQ_ -> do
      let loc = termLoc term
      c <- collapseDupsAt state reduceAt book (loc + 0)
      return $ Efq c []

    t | t == _UNI_ -> do
      return Uni

    t | t == _NIL_ -> do
      return Nil

    t | t == _USE_ -> do
      let loc = termLoc term
      c <- collapseDupsAt state reduceAt book (loc + 0)
      b <- collapseDupsAt state reduceAt book (loc + 1)
      return $ Use c [] b

    t | t == _U32_ -> do
      return U32

    t | t == _W32_ -> do
      let val = termLoc term
      return $ W32 (fromIntegral val)

    t | t == _SWI_ -> do
      let loc = termLoc term
      c <- collapseDupsAt state reduceAt book (loc + 0)
      z <- collapseDupsAt state reduceAt book (loc + 1)
      s <- collapseDupsAt state reduceAt book (loc + 2)
      return $ Swi c [] z s

    t | t == _OPX_ -> do
      let loc = termLoc term
      let opr = toEnum (fromIntegral (termLab term))
      a <- collapseDupsAt state reduceAt book (loc + 0)
      b <- collapseDupsAt state reduceAt book (loc + 1)
      return $ Op2 opr a b

    t | t == _OPY_ -> do
      let loc = termLoc term
      let opr = toEnum (fromIntegral (termLab term))
      b <- collapseDupsAt state reduceAt book (loc + 0)
      a <- collapseDupsAt state reduceAt book (loc + 1)
      return $ Op2 opr a b

    t | t == _SIG_ -> do
      let loc = termLoc term
      _A <- collapseDupsAt state reduceAt book (loc + 0)
      _B <- collapseDupsAt state reduceAt book (loc + 1)
      return $ Sig _A _B

    t | t == _TUP_ -> do
      let loc = termLoc term
      a <- collapseDupsAt state reduceAt book (loc + 0)
      b <- collapseDupsAt state reduceAt book (loc + 1)
      return $ Tup a b

    t | t == _GET_ -> do
      let loc = termLoc term
      c <- collapseDupsAt state reduceAt book (loc + 0)
      b <- collapseDupsAt state reduceAt book (loc + 1)
      return $ Get c [] b

    t | t == _ALL_ -> do
      let loc = termLoc term
      _A <- collapseDupsAt state reduceAt book (loc + 0)
      _B <- collapseDupsAt state reduceAt book (loc + 1)
      return $ All _A _B

    tag -> do
      return $ Var "?"

-- Sup Collapser
-- -------------

collapseSups :: Book -> Core -> Collapse Core

collapseSups book core = case core of

  Var name -> do
    return $ Var name

  Ref name fid args -> do
    args <- mapM (collapseSups book) args
    return $ Ref name fid args

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

  Dup lab x y val body -> do
    val <- collapseSups book val
    body <- collapseSups book body
    return $ Dup lab x y val body

  Set -> do
    return Set

  Emp -> do
    return Emp

  Efq c ms -> do
    c <- collapseSups book c
    ms <- mapM (\ (key, expr) -> do
      expr <- collapseSups book expr
      return (key, expr)) ms
    return $ Efq c ms

  Uni -> do
    return Uni

  Nil -> do
    return Nil

  Use c ms b -> do
    c <- collapseSups book c
    ms <- mapM (\ (key, expr) -> do
      expr <- collapseSups book expr
      return (key, expr)) ms
    b <- collapseSups book b
    return $ Use c ms b

  U32 -> do
    return U32

  W32 val -> do
    return $ W32 val

  Swi c ms z s -> do
    c <- collapseSups book c
    ms <- mapM (\ (key, expr) -> do
      expr <- collapseSups book expr
      return (key, expr)) ms
    z <- collapseSups book z
    s <- collapseSups book s
    return $ Swi c ms z s

  Op2 op x y -> do
    x <- collapseSups book x
    y <- collapseSups book y
    return $ Op2 op x y

  Sig _A _B -> do
    _A <- collapseSups book _A
    _B <- collapseSups book _B
    return $ Sig _A _B

  Tup a b -> do
    a <- collapseSups book a
    b <- collapseSups book b
    return $ Tup a b

  Get c ms b -> do
    c <- collapseSups book c
    ms <- mapM (\ (key, expr) -> do
      expr <- collapseSups book expr
      return (key, expr)) ms
    b <- collapseSups book b
    return $ Get c ms b

  All _A _B -> do
    _A <- collapseSups book _A
    _B <- collapseSups book _B
    return $ All _A _B

  Lam name body -> do
    body <- collapseSups book body
    return $ Lam name body

  App fun arg -> do
    fun <- collapseSups book fun
    arg <- collapseSups book arg
    return $ App fun arg

-- Tree Collapser
-- --------------

doCollapseAt :: ReduceAt -> Book -> Loc -> HVM (Collapse Core)
doCollapseAt reduceAt book host = do
  -- namesRef <- newIORef MS.empty
  let state = (IM.empty)
  core <- collapseDupsAt state reduceAt book host
  return $ collapseSups book core

-- Priority Queue
-- --------------

data PQ a
  = PQLeaf
  | PQNode (Word64, a) (PQ a) (PQ a)
  deriving (Show)

pqUnion :: PQ a -> PQ a -> PQ a
pqUnion PQLeaf heap = heap
pqUnion heap PQLeaf = heap
pqUnion heap1@(PQNode (k1,v1) l1 r1) heap2@(PQNode (k2,v2) l2 r2)
  | k1 <= k2  = PQNode (k1,v1) (pqUnion heap2 r1) l1
  | otherwise = PQNode (k2,v2) (pqUnion heap1 r2) l2

pqPop :: PQ a -> Maybe ((Word64, a), PQ a)
pqPop PQLeaf         = Nothing
pqPop (PQNode x l r) = Just (x, pqUnion l r)

pqPut :: (Word64,a) -> PQ a -> PQ a
pqPut (k,v) = pqUnion (PQNode (k,v) PQLeaf PQLeaf)

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
flattenDFS (CSup k a b) = flattenDFS a ++ flattenDFS b
flattenDFS (CVal x)     = [x]
flattenDFS CEra         = []

flattenBFS :: Collapse a -> [a]
flattenBFS term = go term (SQ [] [] :: SQ (Collapse a)) where
  go (CSup k a b) sq = go CEra (sqPut b $ sqPut a $ sq)
  go (CVal x)     sq = x : go CEra sq
  go CEra         sq = case sqPop sq of
    Just (v,sq) -> go v sq
    Nothing     -> []

flattenPQ :: Collapse a -> [a]
flattenPQ term = go term (PQLeaf :: PQ (Collapse a)) where
  go (CSup k a b) pq = go CEra (pqPut (fromIntegral k,a) $ pqPut (fromIntegral k,b) $ pq)
  go (CVal x)     pq = x : go CEra pq
  go CEra         pq = case pqPop pq of
    Just ((k,v),pq) -> go v pq
    Nothing         -> []

flatten :: Collapse a -> [a]
flatten = flattenBFS

-- Flat Collapser
-- --------------

doCollapseFlatAt :: ReduceAt -> Book -> Loc -> HVM [Core]
doCollapseFlatAt reduceAt book host = do
  coll <- doCollapseAt reduceAt book host
  return $ flatten coll
