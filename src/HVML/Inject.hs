module HVML.Inject where

import Control.Monad (foldM)
import Control.Monad (forM_)
import Control.Monad.State
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

injectCore :: Book -> Core -> Loc -> InjectM ()
injectCore _ Era loc = do
  lift $ set loc (termNew _ERA_ 0 0)

injectCore _ (Var nam) loc = do
  args <- gets args
  case Map.lookup nam args of
    Just term -> lift $ set loc term
    Nothing   -> modify $ \s -> s { vars = (nam, loc) : vars s }

injectCore book (Let mod nam val bod) loc = do
  let_node <- lift $ allocNode 3
  lift $ set (let_node + 0) (termNew _SUB_ 0 0)
  modify $ \s -> s { args = Map.insert nam (termNew _VAR_ 0 (let_node + 0)) (args s) }
  injectCore book val (let_node + 1)
  injectCore book bod (let_node + 2)
  lift $ set loc (termNew _LET_ (fromIntegral $ fromEnum mod) let_node)

injectCore book (Lam vr0 bod) loc = do
  lam <- lift $ allocNode 2
  lift $ set (lam + 0) (termNew _SUB_ 0 0)
  modify $ \s -> s { args = Map.insert vr0 (termNew _VAR_ 0 (lam + 0)) (args s) }
  injectCore book bod (lam + 1)
  lift $ set loc (termNew _LAM_ 0 lam)

injectCore book (App fun arg) loc = do
  app <- lift $ allocNode 2
  injectCore book fun (app + 0)
  injectCore book arg (app + 1)
  lift $ set loc (termNew _APP_ 0 app)

injectCore book (Sup lab tm0 tm1) loc = do
  sup <- lift $ allocNode 2
  injectCore book tm0 (sup + 0)
  injectCore book tm1 (sup + 1)
  lift $ set loc (termNew _SUP_ lab sup)

injectCore book (Dup lab dp0 dp1 val bod) loc = do
  dup <- lift $ allocNode 3
  lift $ set (dup + 0) (termNew _SUB_ 0 0)
  lift $ set (dup + 1) (termNew _SUB_ 0 0)
  modify $ \s -> s 
    { args = Map.insert dp0 (termNew _DP0_ lab dup) 
           $ Map.insert dp1 (termNew _DP1_ lab dup) (args s) 
    }
  injectCore book val (dup + 2)
  injectCore book bod loc

injectCore book (Ref nam fid arg) loc = do
  -- lift $ set loc (termNew _REF_ 0 fid)
  let arity = length arg
  ref <- lift $ allocNode (fromIntegral arity)
  sequence_ [injectCore book x (ref + i) | (i,x) <- zip [0..] arg]
  lift $ set loc (termNew _REF_ (u12v2New fid (fromIntegral arity)) ref)

injectCore book (Ctr cid fds) loc = do
  let arity = length fds
  ctr <- lift $ allocNode (fromIntegral arity)
  sequence_ [injectCore book fd (ctr + ix) | (ix,fd) <- zip [0..] fds]
  lift $ set loc (termNew _CTR_ (u12v2New cid (fromIntegral arity)) ctr)

injectCore book (Mat val css) loc = do
  mat <- lift $ allocNode (1 + fromIntegral (length css))
  injectCore book val (mat + 0)
  sequence_ [injectCore book (snd bod) (mat + 1 + ix) | (ix,bod) <- zip [0..] css]
  lift $ set loc (termNew _MAT_ (fromIntegral (length css)) mat)

injectCore book (U32 val) loc = do
  lift $ set loc (termNew _W32_ 0 (fromIntegral val))

injectCore book (Op2 opr nm0 nm1) loc = do
  opx <- lift $ allocNode 2
  injectCore book nm0 (opx + 0)
  injectCore book nm1 (opx + 1)
  lift $ set loc (termNew _OPX_ (fromIntegral $ fromEnum opr) opx)

injectCore book (USp lab tm0 tm1) loc = do
  usp <- lift $ allocNode 2
  injectCore book tm0 (usp + 0)
  injectCore book tm1 (usp + 1)
  lift $ set loc (termNew _USP_ lab usp)

injectCore book (UDp lab dp0 val bod) loc = do
  udp <- lift $ allocNode 1
  modify $ \s -> s { args = Map.insert dp0 (termNew _UDP_ lab udp) (args s) }
  injectCore book val (udp + 0)
  injectCore book bod loc

doInjectCoreAt :: Book -> Core -> Loc -> [(String, Term)] -> HVM Term
doInjectCoreAt book core host argList = do
  (_, state) <- runStateT (injectCore book core host) (emptyState { args = Map.fromList argList })
  forM_ (vars state) $ \(name, loc) -> 
    case Map.lookup name (args state) of
      Just term -> set loc term
      Nothing   -> error $ "Unbound variable: " ++ name
  got host
