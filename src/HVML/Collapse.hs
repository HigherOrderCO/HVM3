-- //./Type.hs//

-- FIXME: when SUP labels have large vals, this takes a lot of time.

module HVML.Collapse where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad (ap, forM, forM_)
import Control.Monad.IO.Class
import Data.Char (chr, ord)
import Data.IORef
import Data.Word
import GHC.Conc
import HVML.Show
import HVML.Type
import System.Exit (exitFailure)
import System.IO.Unsafe (unsafeInterleaveIO, unsafePerformIO)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as MS

import Debug.Trace

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
data Collapse a = CSup Word64 (Collapse a) (Collapse a) | CVal a | CEra
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
    Just E     -> CSup k x y
    Nothing    -> CSup k x y

  -- putO :: (Bin -> Bin) -> (Bin -> Bin)
  putO bs = \x -> bs (O x)

  -- putI :: (Bin -> Bin) -> (Bin -> Bin) 
  putI bs = \x -> bs (I x)

-- Mutates an element at given index in a list
mut :: Word64 -> (a -> a) -> [a] -> [a]
mut 0 f (x:xs) = f x : xs
mut n f (x:xs) = x : mut (n-1) f xs
mut _ _ []     = []

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

-- collapseDupsAt :: IM.IntMap [Int] -> ReduceAt -> Book -> Loc -> HVM Core
-- collapseDupsAt state@(paths) reduceAt book host = unsafeInterleaveIO $ do
  -- term <- reduceAt book host
  -- putStrLn $ "normalized: " ++ termToString term
  -- case tagT (termTag term) of
    -- ERA -> do
      -- return Era

    -- LET -> do
      -- let loc = termLoc term
      -- let mode = modeT (termLab term)
      -- name <- return $ "$" ++ show (loc + 0)
      -- val0 <- collapseDupsAt state reduceAt book (loc + 1)
      -- bod0 <- collapseDupsAt state reduceAt book (loc + 2)
      -- return $ Let mode name val0 bod0

    -- LAM -> do
      -- let loc = termLoc term
      -- name <- return $ "$" ++ show (loc + 0)
      -- bod0 <- collapseDupsAt state reduceAt book (loc + 0)
      -- return $ Lam name bod0

    -- APP -> do
      -- let loc = termLoc term
      -- fun0 <- collapseDupsAt state reduceAt book (loc + 0)
      -- arg0 <- collapseDupsAt state reduceAt book (loc + 1)
      -- return $ App fun0 arg0

    -- SUP -> do
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

    -- VAR -> do
      -- let loc = termLoc term
      -- sub <- got loc
      -- if termGetBit sub /= 0
      -- then do
        -- set (loc + 0) (termRemBit sub)
        -- collapseDupsAt state reduceAt book (loc + 0)
      -- else do
        -- name <- return $ "$" ++ show loc
        -- return $ Var name

    -- DP0 -> do
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

    -- DP1 -> do
      -- let loc = termLoc term
      -- let lab = termLab term
      -- sb1 <- got (loc+1)
      -- if termGetBit sb1 /= 0
      -- then do
        -- set (loc + 1) (termRemBit sb1)
        -- collapseDupsAt state reduceAt book (loc + 1)
      -- else do
        -- let newPaths = IM.alter (Just . maybe [1] (1:)) (fromIntegral lab) paths
        -- collapseDupsAt (newPaths) reduceAt book (loc + 0)

    -- CTR -> do
      -- let loc = termLoc term
      -- let lab = termLab term
      -- let cid = u12v2X lab
      -- let ari = u12v2Y lab
      -- let aux = if ari == 0 then [] else [loc + i | i <- [0..ari-1]]
      -- fds0 <- forM aux (collapseDupsAt state reduceAt book)
      -- return $ Ctr cid fds0
    
    -- MAT -> do
      -- let loc = termLoc term
      -- let len = u12v2X $ termLab term
      -- let aux = if len == 0 then [] else [loc + 1 + i | i <- [0..len-1]]
      -- val0 <- collapseDupsAt state reduceAt book (loc + 0)
      -- css0 <- forM aux $ \h -> do
        -- bod <- collapseDupsAt state reduceAt book h
        -- return $ ("#", [], bod) -- TODO: recover constructor and fields
      -- return $ Mat val0 [] css0

    -- W32 -> do
      -- let val = termLoc term
      -- return $ U32 (fromIntegral val)

    -- CHR -> do
      -- let val = termLoc term
      -- return $ Chr (chr (fromIntegral val))

    -- OPX -> do
      -- let loc = termLoc term
      -- let opr = toEnum (fromIntegral (termLab term))
      -- nm00 <- collapseDupsAt state reduceAt book (loc + 0)
      -- nm10 <- collapseDupsAt state reduceAt book (loc + 1)
      -- return $ Op2 opr nm00 nm10

    -- OPY -> do
      -- let loc = termLoc term
      -- let opr = toEnum (fromIntegral (termLab term))
      -- nm00 <- collapseDupsAt state reduceAt book (loc + 0)
      -- nm10 <- collapseDupsAt state reduceAt book (loc + 1)
      -- return $ Op2 opr nm00 nm10

    -- REF -> do
      -- let loc = termLoc term
      -- let lab = termLab term
      -- let fid = u12v2X lab
      -- let ari = u12v2Y lab
      -- arg0 <- mapM (collapseDupsAt state reduceAt book) [loc + i | i <- [0..ari-1]]
      -- let name = MS.findWithDefault "?" fid (idToName book)
      -- return $ Ref name fid arg0

    -- tag -> do
      -- putStrLn ("unexpected-tag:" ++ show tag)
      -- return $ Var "?"
      -- -- exitFailure

-- TASK: make this function parallel by using forkIO on the CTR case only, when the arity is exactly 4.
-- TASK: now, rewrite the whole function to use forkIO.  make ctr properly spark
-- one thread for each field. remember: all constructors must parallelize. EX:
-- Sup must fork 2 threads, let must fork 2 threads, etc. -- in general, every
-- constructor with multiple children must sapwn a thread for each child. to
-- avoid making the code big and repetitive, abstract the forking logic. try to
-- keep it as concise and similar to the original one as you can. do it now.
-- this looks ok, but is still too repetitive. can't we abstract it? like:
-- your code introduces a bug. you can't map through [0..len-1] because len,
-- ari, etc., can be 0. since they're word64, 0 - 1 results in a very large
-- number. there is a reason we have the guards in the original code. also, on
-- the MAT case, you can reduce the val first and then fork only the cases, to
-- make it cleaner, avoiding that ugly parenthesis.
-- -- I see the issue. we need the ! to force the whnf of the recursive call to
-- -- collapseDupsAt, otherwise it will just return a thunk and not parallelize
-- -- the heavy computation.
-- TODO: based on the information above, write the final version of the parallel collapser:
-- (code hidden)
-- PROBLEM: the reduceAt function must be called with a "ID" argument (here, 0),
-- which must always be unique; i.e., it can't be called with the same id at the
-- same time by two different threads. so, for example, if we call it with id 7,
-- then, we can only call another reduce with id 7 again once that one returns.
-- to fix this, we must keep a pool of ids (with 16 ids exactly) that we pop and
-- push before/after callind reduce. when the list is empty, we must retry until
-- it isn't empty anymore, in an efficient way.
-- is it possible to implement such logic in Haskell?

-- Helper function to fork multiple computations and collect results
fork :: [HVM a] -> HVM [a]
fork comps = do
  mvars <- sequence [newEmptyMVar | _ <- comps]
  forM_ (zip comps mvars) $ \(comp, mv) ->
    forkIO $ do
      !val <- comp
      putMVar mv val
  mapM takeMVar mvars

-- Thread ID pool management
tidPool :: MVar [TID]
tidPool = unsafePerformIO $ newMVar [0..15]

-- Get a thread ID from the pool, retrying if empty
getTID :: HVM TID
getTID = do
  mTid <- tryTakeMVar tidPool
  case mTid of
    Nothing -> do
      -- threadDelay 100 -- Small delay before retry
      getTID
    Just (tid:rest) -> do
      putMVar tidPool rest
      return tid
    Just [] -> getTID

-- Return a thread ID to the pool
putTID :: TID -> HVM ()
putTID tid = do
  tids <- takeMVar tidPool
  putMVar tidPool (tid:tids)

-- Wrapper for reduceAt that manages thread IDs
safeReduceAt :: ReduceAt -> Book -> Loc -> HVM Term
safeReduceAt reduceAt book loc = do
  tid <- getTID
  result <- reduceAt book tid loc
  putTID tid
  return result

collapseDupsAt :: IM.IntMap [Int] -> ReduceAt -> Book -> Loc -> HVM Core
collapseDupsAt state@(paths) reduceAt book host = unsafeInterleaveIO $ do
  term <- safeReduceAt reduceAt book host
  case tagT (termTag term) of
    ERA -> do
      return Era

    LET -> do
      let loc = termLoc term
      let mode = modeT (termLab term)
      name <- return $ "$" ++ show (loc + 0)
      [val0, bod0] <- fork [
        collapseDupsAt state reduceAt book (loc + 1),
        collapseDupsAt state reduceAt book (loc + 2)]
      return $ Let mode name val0 bod0

    LAM -> do
      let loc = termLoc term
      name <- return $ "$" ++ show (loc + 0)
      bod0 <- collapseDupsAt state reduceAt book (loc + 0)
      return $ Lam name bod0

    APP -> do
      let loc = termLoc term
      [fun0, arg0] <- fork [
        collapseDupsAt state reduceAt book (loc + 0),
        collapseDupsAt state reduceAt book (loc + 1)]
      return $ App fun0 arg0

    SUP -> do
      let loc = termLoc term
      let lab = termLab term
      case IM.lookup (fromIntegral lab) paths of
        Just (p:ps) -> do
          let newPaths = IM.insert (fromIntegral lab) ps paths
          collapseDupsAt (newPaths) reduceAt book (loc + fromIntegral p)
        _ -> do
          [tm00, tm11] <- fork [
            collapseDupsAt state reduceAt book (loc + 0),
            collapseDupsAt state reduceAt book (loc + 1)]
          return $ Sup lab tm00 tm11

    VAR -> do
      let loc = termLoc term
      sub <- got loc
      if termGetBit sub /= 0
      then do
        set (loc + 0) (termRemBit sub)
        collapseDupsAt state reduceAt book (loc + 0)
      else do
        name <- return $ "$" ++ show loc
        return $ Var name

    DP0 -> do
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

    DP1 -> do
      let loc = termLoc term
      let lab = termLab term
      sb1 <- got (loc+1)
      if termGetBit sb1 /= 0
      then do
        set (loc + 1) (termRemBit sb1)
        collapseDupsAt state reduceAt book (loc + 1)
      else do
        let newPaths = IM.alter (Just . maybe [1] (1:)) (fromIntegral lab) paths
        collapseDupsAt (newPaths) reduceAt book (loc + 0)

    CTR -> do
      let loc = termLoc term
      let lab = termLab term
      let cid = u12v2X lab
      let ari = u12v2Y lab
      if ari == 0 
      then return $ Ctr cid []
      else do
        let aux = [loc + i | i <- [0..ari-1]]
        fields <- fork [collapseDupsAt state reduceAt book i | i <- aux]
        return $ Ctr cid fields

    MAT -> do
      let loc = termLoc term
      let len = u12v2X $ termLab term
      val0 <- collapseDupsAt state reduceAt book (loc + 0)
      if len == 0
      then return $ Mat val0 [] []
      else do
        let aux = [loc + 1 + i | i <- [0..len-1]]
        bods <- fork [collapseDupsAt state reduceAt book i | i <- aux]
        return $ Mat val0 [] (map (\bod -> ("#", [], bod)) bods)

    W32 -> do
      let val = termLoc term
      return $ U32 (fromIntegral val)

    CHR -> do
      let val = termLoc term
      return $ Chr (chr (fromIntegral val))

    OPX -> do
      let loc = termLoc term
      let opr = toEnum (fromIntegral (termLab term))
      [nm00, nm10] <- fork [
        collapseDupsAt state reduceAt book (loc + 0),
        collapseDupsAt state reduceAt book (loc + 1)]
      return $ Op2 opr nm00 nm10

    OPY -> do
      let loc = termLoc term
      let opr = toEnum (fromIntegral (termLab term))
      [nm00, nm10] <- fork [
        collapseDupsAt state reduceAt book (loc + 0),
        collapseDupsAt state reduceAt book (loc + 1)]
      return $ Op2 opr nm00 nm10

    REF -> do
      let loc = termLoc term
      let lab = termLab term
      let fid = u12v2X lab
      let ari = u12v2Y lab
      if ari == 0
      then do
        let name = MS.findWithDefault "?" fid (idToName book)
        return $ Ref name fid []
      else do
        let aux = [loc + i | i <- [0..ari-1]]
        args <- fork [collapseDupsAt state reduceAt book i | i <- aux]
        let name = MS.findWithDefault "?" fid (idToName book)
        return $ Ref name fid args

    tag -> do
      putStrLn ("unexpected-tag:" ++ show tag)
      return $ Var "?"


-- Sup Collapser
-- -------------

collapseSups :: Book -> Core -> Collapse Core
collapseSups book core = case core of
  Var name -> return $ Var name
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
  Ctr cid fields -> do
    fields <- mapM (collapseSups book) fields
    return $ Ctr cid fields
  Mat val mov css -> do
    val <- collapseSups book val
    mov <- mapM (\(key, expr) -> do
      expr <- collapseSups book expr
      return (key, expr)) mov
    css <- mapM (\(ctr, fds, bod) -> do
      bod <- collapseSups book bod
      return (ctr, fds, bod)) css
    return $ Mat val mov css
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
flattenDFS (CSup k a b) = flatten a ++ flatten b
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
  go (CSup k a b) pq = go CEra (pqPut (k,a) $ pqPut (k,b) $ pq)
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
