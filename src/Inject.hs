{-./Type.hs-}

module Inject where

import Control.Monad (foldM, when, forM_)
import Control.Monad.State
import Data.Bits (shiftL, (.|.))
import Data.Char (ord)
import Data.List (foldr)
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

-- injectCore :: Book -> Core -> Loc -> InjectM ()

-- injectCore _ Era loc = do
  -- lift $ set loc (termNew _ERA_ 0 0)

-- injectCore _ (Var nam) loc = do
  -- argsMap <- gets args
  -- case MS.lookup nam argsMap of
    -- Just term -> do
      -- lift $ set loc term
      -- when (head nam /= '&') $ do
        -- modify $ \s -> s { args = MS.delete nam (args s) }
    -- Nothing -> do
      -- modify $ \s -> s { vars = (nam, loc) : vars s }

-- injectCore book (Let mod nam val bod) loc = do
  -- let_node <- lift $ allocNode 2
  -- modify $ \s -> s { args = MS.insert nam (termNew _VAR_ 0 (let_node + 0)) (args s) }
  -- injectCore book val (let_node + 0)
  -- injectCore book bod (let_node + 1)
  -- lift $ set loc (termNew _LET_ (fromIntegral $ fromEnum mod) let_node)

-- injectCore book (Lam vr0 bod) loc = do
  -- lam <- lift $ allocNode 1
  -- modify $ \s -> s { args = MS.insert vr0 (termNew _VAR_ 0 (lam + 0)) (args s) }
  -- injectCore book bod (lam + 0)
  -- lift $ set loc (termNew _LAM_ 0 lam)

-- injectCore book (App fun arg) loc = do
  -- app <- lift $ allocNode 2
  -- injectCore book fun (app + 0)
  -- injectCore book arg (app + 1)
  -- lift $ set loc (termNew _APP_ 0 app)

-- injectCore book (Sup lab tm0 tm1) loc = do
  -- sup <- lift $ allocNode 2
  -- injectCore book tm0 (sup + 0)
  -- injectCore book tm1 (sup + 1)
  -- lift $ set loc (termNew _SUP_ lab sup)

-- injectCore book (Dup lab dp0 dp1 val bod) loc = do
  -- dup <- lift $ allocNode 1
  -- modify $ \s -> s 
    -- { args = MS.insert dp0 (termNew _DP0_ lab dup) 
           -- $ MS.insert dp1 (termNew _DP1_ lab dup) (args s) 
    -- }
  -- injectCore book val (dup + 0)
  -- injectCore book bod loc

-- injectCore book (Ref nam fid arg) loc = do
  -- let ari = funArity book fid
  -- let lab = fromIntegral fid
  -- when (ari /= fromIntegral (length arg)) $ do
    -- error $ "Arity mismatch on term: " ++ show (Ref nam fid arg) ++ ". Expected " ++ show ari ++ ", got " ++ show (length arg) ++ "."
  -- ref <- lift $ allocNode (fromIntegral ari)
  -- sequence_ [injectCore book x (ref + i) | (i,x) <- zip [0..] arg]
  -- lift $ set loc (termNew _REF_ lab ref)

-- injectCore book (Ctr nam fds) loc = do
  -- let ari = length fds
  -- let cid = mget (ctrToCid book) nam
  -- let lab = fromIntegral cid
  -- ctr <- lift $ allocNode (fromIntegral ari)
  -- sequence_ [injectCore book fd (ctr + ix) | (ix,fd) <- zip [0..] fds]
  -- lift $ set loc (termNew _CTR_ lab ctr)

-- injectCore book tm@(Mat kin val mov css) loc = do
  -- mat <- lift $ allocNode (1 + fromIntegral (length css))
  -- injectCore book val (mat + 0)
  -- forM_ (zip [0..] css) $ \ (idx, (ctr, fds, bod)) -> do
    -- injectCore book (foldr (\x b -> Lam x b) (foldr (\x b -> Lam x b) bod (map fst mov)) fds) (mat + 1 + fromIntegral idx)
  -- let tag = case kin of { SWI -> _SWI_ ; (MAT _) -> _MAT_ ; (IFL _) -> _IFL_ }
  -- let lab = case kin of { SWI -> fromIntegral $ length css ; (MAT cid) -> fromIntegral cid ; (IFL cid) -> fromIntegral cid }
  -- trm <- return $ termNew tag lab mat
  -- ret <- foldM (\mat (_, val) -> do
      -- app <- lift $ allocNode 2
      -- lift $ set (app + 0) mat
      -- injectCore book val (app + 1)
      -- return $ termNew _APP_ 0 app)
    -- trm
    -- mov
  -- lift $ set loc ret

-- injectCore book (W32 val) loc = do
  -- lift $ set loc (termNew _W32_ 0 (fromIntegral val))

-- injectCore book (Chr val) loc = do
  -- lift $ set loc (termNew _CHR_ 0 (fromIntegral $ ord val))

-- injectCore book (Op2 opr nm0 nm1) loc = do
  -- opx <- lift $ allocNode 2
  -- injectCore book nm0 (opx + 0)
  -- injectCore book nm1 (opx + 1)
  -- lift $ set loc (termNew _OPX_ (fromIntegral $ fromEnum opr) opx)

-- injectCore _ Set loc = do
  -- lift $ set loc (termNew _SET_ 0 0)

-- injectCore book (All typ bod) loc = do
  -- all_node <- lift $ allocNode 2
  -- injectCore book typ (all_node + 0)
  -- injectCore book bod (all_node + 1)
  -- lift $ set loc (termNew _ALL_ 0 all_node)

-- injectCore book (Adt nam fds) loc = do
  -- let ari = length fds
  -- let cid = mget (ctrToCid book) nam
  -- let lab = fromIntegral cid
  -- adt <- lift $ allocNode (fromIntegral ari)
  -- sequence_ [injectCore book fd (adt + ix) | (ix,fd) <- zip [0..] fds]
  -- lift $ set loc (termNew _ADT_ lab adt)

-- injectCore _ U32 loc = do
  -- lift $ set loc (termNew _W32_ 0 0)

-- doInjectCoreAt :: Book -> Core -> Loc -> [(String,Term)] -> HVM Term
-- doInjectCoreAt book core host argList = do
  -- (_, state) <- runStateT (injectCore book core host) (emptyState { args = MS.fromList argList })
  -- foldM (\m (name, loc) -> do
    -- case MS.lookup name (args state) of
      -- Just term -> do
        -- set loc term
        -- return $ MS.delete name m
      -- Nothing -> do
        -- error $ "Unbound variable: \n\x1b[2m" ++ name ++ "\n\x1b[0mIn term:\n\x1b[2m" ++ Data.List.take 1024 (show core) ++ "...\x1b[0m")
    -- (args state)
    -- (vars state)
  -- got host

-- UPDATED:

injectCore :: Book -> Core -> Loc -> InjectM ()

injectCore _ Era loc = do
  lift $ set loc (termNew _ERA_ 0 0)

injectCore _ (Var x) loc = do
  argsMap <- gets args
  case MS.lookup x argsMap of
    Just term -> do
      lift $ set loc term
      when (head x /= '&') $ do
        modify $ \s -> s { args = MS.delete x (args s) }
    Nothing -> do
      modify $ \s -> s { vars = (x, loc) : vars s }

injectCore book (Ref x i xs) loc = do
  let ari = funArity book i
  let lab = fromIntegral i
  when (ari /= fromIntegral (length xs)) $ do
    error $ "Arity mismatch on term: " ++ show (Ref x i xs) ++ ". Expected " ++ show ari ++ ", got " ++ show (length xs) ++ "."
  ref <- lift $ allocNode (fromIntegral ari)
  sequence_ [injectCore book x (ref + i) | (i,x) <- zip [0..] xs]
  lift $ set loc (termNew _REF_ lab ref)

injectCore book (Let m x v f) loc = do
  let_node <- lift $ allocNode 2
  modify $ \s -> s { args = MS.insert x (termNew _VAR_ 0 (let_node + 0)) (args s) }
  injectCore book v (let_node + 0)
  injectCore book f (let_node + 1)
  lift $ set loc (termNew _LET_ (fromIntegral $ fromEnum m) let_node)

injectCore _ Era loc = do
  lift $ set loc (termNew _ERA_ 0 0)

injectCore book (Sup l a b) loc = do
  sup <- lift $ allocNode 2
  injectCore book a (sup + 0)
  injectCore book b (sup + 1)
  lift $ set loc (termNew _SUP_ l sup)

injectCore book (Dup l x y v f) loc = do
  dup <- lift $ allocNode 1
  modify $ \s -> s 
    { args = MS.insert x (termNew _DP0_ l dup) 
           $ MS.insert y (termNew _DP1_ l dup) (args s) 
    }
  injectCore book v (dup + 0)
  injectCore book f loc

injectCore _ Set loc = do
  lift $ set loc (termNew _SET_ 0 0)

injectCore _ Emp loc = do
  lift $ set loc (termNew _EMP_ 0 0)

injectCore book (Efq c ms) loc = do
  efq <- lift $ allocNode 1
  injectCore book c (efq + 0)
  applyMoves book (termNew _EFQ_ 0 efq) ms loc

injectCore _ Uni loc = do
  lift $ set loc (termNew _UNI_ 0 0)

injectCore _ Nil loc = do
  lift $ set loc (termNew _NIL_ 0 0)

injectCore book (Use c ms b) loc = do
  use <- lift $ allocNode 2
  injectCore book c (use + 0)
  injectCore book b (use + 1)
  applyMoves book (termNew _USE_ 0 use) ms loc

injectCore _ U32 loc = do
  lift $ set loc (termNew _U32_ 0 0)

injectCore _ (W32 n) loc = do
  lift $ set loc (termNew _W32_ 0 (fromIntegral n))

injectCore book (Swi c ms z s) loc = do
  swi <- lift $ allocNode 3
  injectCore book c (swi + 0)
  injectCore book z (swi + 1)
  injectCore book s (swi + 2)
  applyMoves book (termNew _SWI_ 0 swi) ms loc

injectCore book (Op2 o a b) loc = do
  opx <- lift $ allocNode 2
  injectCore book a (opx + 0)
  injectCore book b (opx + 1)
  lift $ set loc (termNew _OPX_ (fromIntegral $ fromEnum o) opx)

injectCore book (Sig _A _B) loc = do
  sig <- lift $ allocNode 2
  injectCore book _A (sig + 0)
  injectCore book _B (sig + 1)
  lift $ set loc (termNew _SIG_ 0 sig)

injectCore book (Tup a b) loc = do
  tup <- lift $ allocNode 2
  injectCore book a (tup + 0)
  injectCore book b (tup + 1)
  lift $ set loc (termNew _TUP_ 0 tup)

injectCore book (Get c ms b) loc = do
  get <- lift $ allocNode 2
  injectCore book c (get + 0)
  injectCore book b (get + 1)
  applyMoves book (termNew _GET_ 0 get) ms loc

injectCore book (All _A _B) loc = do
  all_node <- lift $ allocNode 2
  injectCore book _A (all_node + 0)
  injectCore book _B (all_node + 1)
  lift $ set loc (termNew _ALL_ 0 all_node)

injectCore book (Lam x f) loc = do
  lam <- lift $ allocNode 1
  modify $ \s -> s { args = MS.insert x (termNew _VAR_ 0 (lam + 0)) (args s) }
  injectCore book f (lam + 0)
  lift $ set loc (termNew _LAM_ 0 lam)

injectCore book (App f x) loc = do
  app <- lift $ allocNode 2
  injectCore book f (app + 0)
  injectCore book x (app + 1)
  lift $ set loc (termNew _APP_ 0 app)

applyMoves :: Book -> Term -> [Move] -> Loc -> InjectM ()
applyMoves book base ms loc = do
  result <- foldM (\term (_, v) -> do
      app <- lift $ allocNode 2
      lift $ set (app + 0) term
      injectCore book v (app + 1)
      return $ termNew _APP_ 0 app)
    base
    ms
  lift $ set loc result

doInjectCoreAt :: Book -> Core -> Loc -> [(String,Term)] -> HVM Term
doInjectCoreAt book core host argList = do
  (_, state) <- runStateT (injectCore book core host) (emptyState { args = MS.fromList argList })
  _ <- foldM (\m (name, loc) -> do
    case MS.lookup name m of
      Just term -> do
        set loc term
        return $ MS.delete name m
      Nothing -> do
        error $ "Unbound variable: \n\x1b[2m" ++ name ++ "\n\x1b[0mIn term:\n\x1b[2m" ++ Prelude.take 1024 (show core) ++ "...\x1b[0m")
    (args state)
    (vars state)
  got host
