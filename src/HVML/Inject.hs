-- //./Type.hs//

module HVML.Inject where

import Control.Monad (foldM, when, forM_)
import Control.Monad.State
import Data.Char (ord)
import Data.Word
import HVML.Show
import HVML.Type
import qualified Data.Map.Strict as Map

type InjectM a = StateT InjectState HVM a

data InjectState = InjectState
  { args :: Map.Map String Term -- maps var names to binder locations
  , vars :: [(String, Loc)]     -- list of (var name, usage location) pairs
  }

emptyState :: InjectState
emptyState = InjectState Map.empty []

injectCore :: Book -> Core -> TID -> Loc -> InjectM ()
injectCore _ Era tid loc = do
  lift $ set loc (termNew _ERA_ 0 0)

injectCore _ (Var nam) tid loc = do
  argsMap <- gets args
  case Map.lookup nam argsMap of
    Just term -> do
      lift $ set loc term
      when (head nam /= '&') $ do
        modify $ \s -> s { args = Map.delete nam (args s) }
    Nothing -> do
      modify $ \s -> s { vars = (nam, loc) : vars s }

injectCore book (Let mod nam val bod) tid loc = do
  let_node <- lift $ allocNode tid 2
  modify $ \s -> s { args = Map.insert nam (termNew _VAR_ 0 (let_node + 0)) (args s) }
  injectCore book val tid (let_node + 0)
  injectCore book bod tid (let_node + 1)
  lift $ set loc (termNew _LET_ (fromIntegral $ fromEnum mod) let_node)

injectCore book (Lam vr0 bod) tid loc = do
  lam <- lift $ allocNode tid 1
  -- lift $ set (lam + 0) (termNew _SUB_ 0 0)
  modify $ \s -> s { args = Map.insert vr0 (termNew _VAR_ 0 (lam + 0)) (args s) }
  injectCore book bod tid (lam + 0)
  lift $ set loc (termNew _LAM_ 0 lam)

injectCore book (App fun arg) tid loc = do
  app <- lift $ allocNode tid 2
  injectCore book fun tid (app + 0)
  injectCore book arg tid (app + 1)
  lift $ set loc (termNew _APP_ 0 app)

injectCore book (Sup lab tm0 tm1) tid loc = do
  sup <- lift $ allocNode tid 2
  injectCore book tm0 tid (sup + 0)
  injectCore book tm1 tid (sup + 1)
  lift $ set loc (termNew _SUP_ lab sup)

injectCore book (Dup lab dp0 dp1 val bod) tid loc = do
  dup <- lift $ allocNode tid 2
  -- lift $ set (dup + 0) (termNew _SUB_ 0 0)
  lift $ set (dup + 1) (termNew _SUB_ 0 0)
  modify $ \s -> s 
    { args = Map.insert dp0 (termNew _DP0_ lab dup) 
           $ Map.insert dp1 (termNew _DP1_ lab dup) (args s) 
    }
  injectCore book val tid (dup + 0)
  injectCore book bod tid loc

injectCore book (Ref nam fid arg) tid loc = do
  -- lift $ set loc (termNew _REF_ 0 fid)
  let arity = length arg
  ref <- lift $ allocNode tid (fromIntegral arity)
  sequence_ [injectCore book x tid (ref + i) | (i,x) <- zip [0..] arg]
  lift $ set loc (termNew _REF_ (u12v2New fid (fromIntegral arity)) ref)

injectCore book (Ctr cid fds) tid loc = do
  let arity = length fds
  ctr <- lift $ allocNode tid (fromIntegral arity)
  sequence_ [injectCore book fd tid (ctr + ix) | (ix,fd) <- zip [0..] fds]
  lift $ set loc (termNew _CTR_ (u12v2New cid (fromIntegral arity)) ctr)

injectCore book tm@(Mat val mov css) tid loc = do
  -- Allocate space for the Mat term
  mat <- lift $ allocNode tid (1 + fromIntegral (length css))
  -- Inject the value being matched
  injectCore book val tid (mat + 0)
  -- Inject each case body
  forM_ (zip [0..] css) $ \ (idx, (ctr, fds, bod)) -> do
    injectCore book (foldr Lam (foldr Lam bod (map fst mov)) fds) tid (mat + 1 + idx)
  -- After processing all cases, create the Mat term
  trm <- return $ termNew _MAT_ (u12v2New (fromIntegral (length css)) (ifLetLab book tm)) mat
  ret <- foldM (\mat (_, val) -> do
      app <- lift $ allocNode tid 2
      lift $ set (app + 0) mat
      injectCore book val tid (app + 1)
      return $ termNew _APP_ 0 app)
    trm
    mov
  lift $ set loc ret

injectCore book (U32 val) tid loc = do
  lift $ set loc (termNew _W32_ 0 (fromIntegral val))

injectCore book (Chr val) tid loc = do
  lift $ set loc (termNew _CHR_ 0 (fromIntegral $ ord val))

injectCore book (Op2 opr nm0 nm1) tid loc = do
  opx <- lift $ allocNode tid 2
  injectCore book nm0 tid (opx + 0)
  injectCore book nm1 tid (opx + 1)
  lift $ set loc (termNew _OPX_ (fromIntegral $ fromEnum opr) opx)

doInjectCoreAt :: Book -> Core -> TID -> Loc -> [(String,Term)] -> HVM Term
doInjectCoreAt book core tid host argList = do
  (_, state) <- runStateT (injectCore book core tid host) (emptyState { args = Map.fromList argList })
  foldM (\m (name, loc) -> do
    case Map.lookup name (args state) of
      Just term -> do
        set loc term
        if (head name /= '&') then do
          return $ Map.delete name m
        else do
          return $ m
      Nothing -> do
        error $ "Unbound variable: " ++ name)
    (args state)
    (vars state)
  got host
