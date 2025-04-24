{-./Type.hs-}

module Inject where

import Control.Monad (foldM, when, forM_)
import Control.Monad.State
import Data.Bits (shiftL, (.|.))
import Data.Char (ord)
import Data.List (foldr, take)
import Data.Word
import Debug.Trace
import Foreign
import Type
import qualified Data.Map.Strict as MS

type InjectM a = StateT InjectState HVM a

data InjectState = InjectState
  { args :: MS.Map String Term -- maps var names to binder locations
  , vars :: [(String, Loc)]    -- list of (var name, usage location) pairs
  }

emptyState :: InjectState
emptyState = InjectState MS.empty []

injectCore :: Book -> Core -> Loc -> InjectM ()

injectCore _ Era loc = do
  lift $ set loc (termNew _ERA_ 0 0)

injectCore _ (Var nam) loc = do
  argsMap <- gets args
  case MS.lookup nam argsMap of
    Just term -> do
      lift $ set loc term
      when (head nam /= '&') $ do
        modify $ \s -> s { args = MS.delete nam (args s) }
    Nothing -> do
      modify $ \s -> s { vars = (nam, loc) : vars s }

injectCore book (Let mod nam val bod) loc = do
  let_node <- lift $ allocNode 2
  modify $ \s -> s { args = MS.insert nam (termNew _VAR_ 0 (let_node + 0)) (args s) }
  injectCore book val (let_node + 0)
  injectCore book bod (let_node + 1)
  lift $ set loc (termNew _LET_ (fromIntegral $ fromEnum mod) let_node)

injectCore book (Lam vr0 bod) loc = do
  lam <- lift $ allocNode 1
  modify $ \s -> s { args = MS.insert vr0 (termNew _VAR_ 0 (lam + 0)) (args s) }
  injectCore book bod (lam + 0)
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
  dup <- lift $ allocNode 1
  modify $ \s -> s 
    { args = MS.insert dp0 (termNew _DP0_ lab dup) 
           $ MS.insert dp1 (termNew _DP1_ lab dup) (args s) 
    }
  injectCore book val (dup + 0)
  injectCore book bod loc

injectCore book (Ref nam fid arg) loc = do
  let ari = funArity book fid
  let lab = fromIntegral fid
  when (ari /= fromIntegral (length arg)) $ do
    error $ "Arity mismatch on term: " ++ show (Ref nam fid arg) ++ ". Expected " ++ show ari ++ ", got " ++ show (length arg) ++ "."
  ref <- lift $ allocNode (fromIntegral ari)
  sequence_ [injectCore book x (ref + i) | (i,x) <- zip [0..] arg]
  lift $ set loc (termNew _REF_ lab ref)

injectCore book (Ctr nam fds) loc = do
  let ari = length fds
  let cid = mget (ctrToCid book) nam
  let lab = fromIntegral cid
  ctr <- lift $ allocNode (fromIntegral ari)
  sequence_ [injectCore book fd (ctr + ix) | (ix,fd) <- zip [0..] fds]
  lift $ set loc (termNew _CTR_ lab ctr)

injectCore book tm@(Mat kin val mov css) loc = do
  mat <- lift $ allocNode (1 + fromIntegral (length css))
  injectCore book val (mat + 0)
  forM_ (zip [0..] css) $ \ (idx, (ctr, fds, bod)) -> do
    injectCore book (foldr (\x b -> Lam x b) (foldr (\x b -> Lam x b) bod (map fst mov)) fds) (mat + 1 + fromIntegral idx)
  let tag = case kin of { SWI -> _SWI_ ; (MAT _) -> _MAT_ ; (IFL _) -> _IFL_ }
  let lab = case kin of { SWI -> fromIntegral $ length css ; (MAT cid) -> fromIntegral cid ; (IFL cid) -> fromIntegral cid }
  trm <- return $ termNew tag lab mat
  ret <- foldM (\mat (_, val) -> do
      app <- lift $ allocNode 2
      lift $ set (app + 0) mat
      injectCore book val (app + 1)
      return $ termNew _APP_ 0 app)
    trm
    mov
  lift $ set loc ret

injectCore book (U32 val) loc = do
  lift $ set loc (termNew _W32_ 0 (fromIntegral val))

injectCore book (Chr val) loc = do
  lift $ set loc (termNew _CHR_ 0 (fromIntegral $ ord val))

injectCore book (Op2 opr nm0 nm1) loc = do
  opx <- lift $ allocNode 2
  injectCore book nm0 (opx + 0)
  injectCore book nm1 (opx + 1)
  lift $ set loc (termNew _OPX_ (fromIntegral $ fromEnum opr) opx)

doInjectCoreAt :: Book -> Core -> Loc -> [(String,Term)] -> HVM Term
doInjectCoreAt book core host argList = do
  (_, state) <- runStateT (injectCore book core host) (emptyState { args = MS.fromList argList })
  foldM (\m (name, loc) -> do
    case MS.lookup name (args state) of
      Just term -> do
        set loc term
        return $ MS.delete name m
      Nothing -> do
        error $ "Unbound variable: \n\x1b[2m" ++ name ++ "\n\x1b[0mIn term:\n\x1b[2m" ++ Data.List.take 1024 (show core) ++ "...\x1b[0m")
    (args state)
    (vars state)
  got host
