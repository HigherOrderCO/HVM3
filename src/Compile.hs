-- //./Type.hs//
-- //./Inject.hs//

module Compile where

import Control.Monad (forM_, forM, foldM, when)
import Control.Monad.State
import Data.Bits (shiftL, (.|.))
import Data.List
import Data.Word
import Debug.Trace
import Foreign hiding (fresh)
import Type
import qualified Data.Map.Strict as MS

-- Compilation
-- -----------

data CompileState = CompileState
  { next :: Word64
  , tabs :: Int
  , bins :: MS.Map String String  -- var_name => binder_host
  , vars :: [(String, String)]    -- [(var_name, var_host)]
  , code :: [String]
  , reus :: MS.Map Int [String]   -- arity => [reuse_loc]
  }

type Compile = State CompileState

compileHeaders :: Book -> String
compileHeaders book =
  let funcs   = MS.toList (fidToNam book)
      decls_f = map (\(_, name) -> "Term " ++ name ++ "_f(Term);") funcs
      decls_t = map (\(_, name) -> "Term " ++ name ++ "_t(Term);") funcs
  in unlines $ decls_f ++ decls_t

compile :: Book -> Word16 -> String
compile book fid =
  let full = compileWith compileFull book fid in
  let fast = compileWith compileFast book fid in
  let slow = compileWith compileSlow book fid in
  if "<ERR>" `isInfixOf` fast
    then unlines [ full , slow ]
    else unlines [ full , fast ]

-- Compiles a function using either Fast-Mode or Full-Mode
compileWith :: (Book -> Word16 -> Core -> Bool -> [(Bool,String)] -> Compile ()) -> Book -> Word16 -> String
compileWith cmp book fid = 
  let copy   = fst (fst (mget (fidToFun book) fid)) in
  let args   = snd (fst (mget (fidToFun book) fid)) in
  let core   = snd (mget (fidToFun book) fid) in
  let state  = CompileState 0 0 MS.empty [] [] MS.empty in
  let result = runState (cmp book fid core copy args) state in
  unlines $ reverse $ code (snd result)

emit :: String -> Compile ()
emit line = modify $ \st -> st { code = (replicate (tabs st * 2) ' ' ++ line) : code st }

tabInc :: Compile ()
tabInc = modify $ \st -> st { tabs = tabs st + 1 }

tabDec :: Compile ()
tabDec = modify $ \st -> st { tabs = tabs st - 1 }

bind :: String -> String -> Compile ()
bind var host = modify $ \st -> st { bins = MS.insert var host (bins st) }

fresh :: String -> Compile String
fresh name = do
  uid <- gets next
  modify $ \s -> s { next = uid + 1 }
  return $ name ++ show uid

reuse :: Int -> String -> Compile ()
reuse arity loc = modify $ \st -> st { reus = MS.insertWith (++) arity [loc] (reus st) }

-- Full Compiler
-- -------------

compileFull :: Book -> Word16 -> Core -> Bool -> [(Bool,String)] -> Compile ()
compileFull book fid core copy args = do
  emit $ "Term " ++ mget (fidToNam book) fid ++ "_t(Term ref) {"
  tabInc
  forM_ (zip [0..] args) $ \(i, arg) -> do
    argVar <- fresh "arg"
    if fst arg
      then emit $ "Term " ++ argVar ++ " = reduce_at(term_loc(ref) + " ++ show i ++ ");"
      else emit $ "Term " ++ argVar ++ " = got(term_loc(ref) + " ++ show i ++ ");"
    let argName = snd arg
    bind argName argVar
  result <- compileFullCore book fid core "root"
  st <- get
  forM_ (vars st) $ \ (var,host) -> do
    let varTerm = MS.findWithDefault "" var (bins st)
    emit $ "set(" ++ host ++ ", " ++ varTerm ++ ");"
  emit $ "return " ++ result ++ ";"
  tabDec
  emit "}"

compileFullVar :: String -> String -> Compile String
compileFullVar var host = do
  bins <- gets bins
  case MS.lookup var bins of
    Just entry -> do
      return entry
    Nothing -> do
      modify $ \s -> s { vars = (var, host) : vars s }
      return "0"

compileFullCore :: Book -> Word16 -> Core -> String -> Compile String

compileFullCore book fid Era _ = do
  return $ "term_new(ERA, 0, 0)"

compileFullCore book fid (Var name) host = do
  compileFullVar name host

compileFullCore book fid (Let mode var val bod) host = do
  letNam <- fresh "let"
  emit $ "Loc " ++ letNam ++ " = alloc_node(2);"
  valT <- compileFullCore book fid val (letNam ++ " + 0")
  emit $ "set(" ++ letNam ++ " + 0, " ++ valT ++ ");"
  bind var $ "term_new(VAR, 0, " ++ letNam ++ " + 0)"
  bodT <- compileFullCore book fid bod (letNam ++ " + 1")
  emit $ "set(" ++ letNam ++ " + 1, " ++ bodT ++ ");"
  return $ "term_new(LET, " ++ show (fromEnum mode) ++ ", " ++ letNam ++ ")"

compileFullCore book fid (Lam var bod) host = do
  lamNam <- fresh "lam"
  emit $ "Loc " ++ lamNam ++ " = alloc_node(1);"
  bind var $ "term_new(VAR, 0, " ++ lamNam ++ " + 0)"
  bodT <- compileFullCore book fid bod (lamNam ++ " + 0")
  emit $ "set(" ++ lamNam ++ " + 0, " ++ bodT ++ ");"
  return $ "term_new(LAM, 0, " ++ lamNam ++ ")"

compileFullCore book fid (App fun arg) host = do
  appNam <- fresh "app"
  emit $ "Loc " ++ appNam ++ " = alloc_node(2);"
  funT <- compileFullCore book fid fun (appNam ++ " + 0")
  argT <- compileFullCore book fid arg (appNam ++ " + 1")
  emit $ "set(" ++ appNam ++ " + 0, " ++ funT ++ ");"
  emit $ "set(" ++ appNam ++ " + 1, " ++ argT ++ ");"
  return $ "term_new(APP, 0, " ++ appNam ++ ")"

compileFullCore book fid (Sup lab tm0 tm1) host = do
  supNam <- fresh "sup"
  emit $ "Loc " ++ supNam ++ " = alloc_node(2);"
  tm0T <- compileFullCore book fid tm0 (supNam ++ " + 0")
  tm1T <- compileFullCore book fid tm1 (supNam ++ " + 1")
  emit $ "set(" ++ supNam ++ " + 0, " ++ tm0T ++ ");"
  emit $ "set(" ++ supNam ++ " + 1, " ++ tm1T ++ ");"
  return $ "term_new(SUP, " ++ show lab ++ ", " ++ supNam ++ ")"

compileFullCore book fid (Dup lab dp0 dp1 val bod) host = do
  dupNam <- fresh "dup"
  emit $ "Loc " ++ dupNam ++ " = alloc_node(1);"
  bind dp0 $ "term_new(DP0, " ++ show lab ++ ", " ++ dupNam ++ " + 0)"
  bind dp1 $ "term_new(DP1, " ++ show lab ++ ", " ++ dupNam ++ " + 0)"
  valT <- compileFullCore book fid val (dupNam ++ " + 0")
  emit $ "set(" ++ dupNam ++ " + 0, " ++ valT ++ ");"
  bodT <- compileFullCore book fid bod host
  return bodT

compileFullCore book fid (Ctr nam fds) host = do
  ctrNam <- fresh "ctr"
  let arity = length fds
  let cid = mget (ctrToCid book) nam
  emit $ "Loc " ++ ctrNam ++ " = alloc_node(" ++ show arity ++ ");"
  fdsT <- mapM (\ (i,fd) -> compileFullCore book fid fd (ctrNam ++ " + " ++ show i)) (zip [0..] fds)
  sequence_ [emit $ "set(" ++ ctrNam ++ " + " ++ show i ++ ", " ++ fdT ++ ");" | (i,fdT) <- zip [0..] fdsT]
  return $ "term_new(CTR, " ++ show cid ++ ", " ++ ctrNam ++ ")"

compileFullCore book fid tm@(Mat kin val mov css) host = do
  matNam <- fresh "mat"
  emit $ "Loc " ++ matNam ++ " = alloc_node(" ++ show (1 + length css) ++ ");"
  valT <- compileFullCore book fid val (matNam ++ " + 0")
  emit $ "set(" ++ matNam ++ " + 0, " ++ valT ++ ");"
  forM_ (zip [0..] css) $ \ (i,(ctr,fds,bod)) -> do
    let bod' = foldr (\x b -> Lam x b) (foldr (\x b -> Lam x b) bod (map fst mov)) fds
    bodT <- compileFullCore book fid bod' (matNam ++ " + " ++ show (i+1))
    emit $ "set(" ++ matNam ++ " + " ++ show (i+1) ++ ", " ++ bodT ++ ");"
  let tag = case kin of { SWI -> "SWI" ; (IFL _) -> "IFL" ; (MAT _) -> "MAT" }
  let lab = case kin of { SWI -> fromIntegral (length css) ; (IFL cid) -> cid ; (MAT cid) -> cid }
  let mat = "term_new(" ++ tag ++ ", " ++ show lab ++ ", " ++ matNam ++ ")"
  foldM (\term (key, val) -> do
    appNam <- fresh "app"
    emit $ "Loc " ++ appNam ++ " = alloc_node(2);"
    valT <- compileFullCore book fid val (appNam ++ " + 1")
    emit $ "set(" ++ appNam ++ " + 0, " ++ term ++ ");"
    emit $ "set(" ++ appNam ++ " + 1, " ++ valT ++ ");"
    return $ "term_new(APP, 0, " ++ appNam ++ ")") mat mov

compileFullCore book fid (U32 val) _ =
  return $ "term_new(W32, 0, " ++ show (fromIntegral val) ++ ")"

compileFullCore book fid (Chr val) _ =
  return $ "term_new(CHR, 0, " ++ show (fromEnum val) ++ ")"

compileFullCore book fid (Op2 opr nu0 nu1) host = do
  opxNam <- fresh "opx"
  emit $ "Loc " ++ opxNam ++ " = alloc_node(2);"
  nu0T <- compileFullCore book fid nu0 (opxNam ++ " + 0")
  nu1T <- compileFullCore book fid nu1 (opxNam ++ " + 1")
  emit $ "set(" ++ opxNam ++ " + 0, " ++ nu0T ++ ");"
  emit $ "set(" ++ opxNam ++ " + 1, " ++ nu1T ++ ");"
  return $ "term_new(OPX, " ++ show (fromEnum opr) ++ ", " ++ opxNam ++ ")"

compileFullCore book fid t@(Ref rNam rFid rArg) host = do
  checkRefAri book t
  refNam <- fresh "ref"
  let arity = length rArg
  emit $ "Loc " ++ refNam ++ " = alloc_node(" ++ show arity ++ ");"
  argsT <- mapM (\ (i,arg) -> compileFullCore book fid arg (refNam ++ " + " ++ show i)) (zip [0..] rArg)
  sequence_ [emit $ "set(" ++ refNam ++ " + " ++ show i ++ ", " ++ argT ++ ");" | (i,argT) <- zip [0..] argsT]
  return $ "term_new(REF, " ++ show rFid ++ ", " ++ refNam ++ ")"

-- Fast Compiler
-- -------------

-- Compiles a function using Fast-Mode
compileFast :: Book -> Word16 -> Core -> Bool -> [(Bool,String)] -> Compile ()
compileFast book fid core copy args = do
  emit $ "Term " ++ mget (fidToNam book) fid ++ "_f(Term ref) {"
  tabInc
  emit "u64 itrs = 0;"
  args <- forM (zip [0..] args) $ \ (i, (strict, arg)) -> do
    argNam <- fresh "arg"
    if strict then do
      emit $ "Term " ++ argNam ++ " = reduce_at(term_loc(ref) + " ++ show i ++ ");"
    else do
      emit $ "Term " ++ argNam ++ " = got(term_loc(ref) + " ++ show i ++ ");"
    if copy && strict then do
      case MS.lookup fid (fidToLab book) of
        Just labs -> do
          emit $ "if (term_tag(" ++ argNam ++ ") == ERA) {"
          emit $ "  itrs += 1;"
          emit $ "  *HVM.itrs += itrs;"
          emit $ "  return term_new(ERA, 0, 0);"
          emit $ "}"
          emit $ "if (term_tag(" ++ argNam ++ ") == SUP) {"
          tabInc
          emit $ "u64 lab = term_lab(" ++ argNam ++ ");"
          emit $ "if (1"
          forM_ (MS.keys labs) $ \lab -> do
            emit $ "    && lab != " ++ show lab
          emit $ ") {"
          tabInc
          emit $ "Term term = reduce_ref_sup(ref, " ++ show i ++ ");"
          emit $ "return term;"
          tabDec
          emit $ "}"
          tabDec
          emit $ "}"
        Nothing -> return ()
    else
      return ()
    bind arg argNam
    return argNam
  reuse (length (snd (fst (mget (fidToFun book) fid)))) "term_loc(ref)"
  compileFastArgs book fid core args
  tabDec
  emit "}"

-- Compiles a fast function's argument list
compileFastArgs :: Book -> Word16 -> Core -> [String] -> Compile ()
compileFastArgs book fid body ctx = do
  emit $ "_Bool fst_iter = 1;"
  emit $ "while (1) {"
  tabInc
  compileFastBody book fid body ctx False 0
  tabDec
  emit $ "}"

-- Compiles a fast function body (pattern-matching)
compileFastBody :: Book -> Word16 -> Core -> [String] -> Bool -> Int -> Compile ()
compileFastBody book fid term@(Mat kin val mov css) ctx stop@False itr = do
  valT   <- compileFastCore book fid val
  valNam <- fresh "val"
  emit $ "Term " ++ valNam ++ " = (" ++ valT ++ ");"
  let isNumeric = length css > 0 && (let (ctr,fds,bod) = css !! 0 in ctr == "0")

  -- Numeric Pattern-Matching
  if isNumeric then do
    numNam <- fresh "num"
    emit $ "if (term_tag("++valNam++") == W32) {"
    tabInc
    emit $ "u32 " ++ numNam ++ " = term_loc(" ++ valNam ++ ");"
    emit $ "switch (" ++ numNam ++ ") {"
    tabInc
    forM_ (zip [0..] css) $ \ (i, (ctr,fds,bod)) -> do
      if i < length css - 1 then do
        emit $ "case " ++ show i ++ ": {"
        tabInc
        forM_ mov $ \ (key,val) -> do
          valT <- compileFastCore book fid val
          bind key valT
        compileFastBody book fid bod ctx stop (itr + 1 + length mov)
        tabDec
        emit $ "}"
      else do
        emit $ "default: {"
        tabInc
        preNam <- fresh "pre"
        emit $ "Term " ++ preNam ++ " = " ++ "term_new(W32, 0, "++numNam++" - "++show (length css - 1)++");"
        forM_ fds $ \ fd -> do
          bind fd preNam
        forM_ mov $ \ (key,val) -> do
          valT <- compileFastCore book fid val
          bind key valT
        compileFastBody book fid bod ctx stop (itr + 1 + length fds + length mov)
        tabDec
        emit $ "}"
    tabDec
    emit $ "}"
    tabDec
    emit $ "} else if (term_tag(" ++ valNam ++ ") == ERA) {"
    tabInc
    compileFastBody book fid Era ctx stop (itr + 1)
    tabDec
    emit $ "} else {"
    tabInc
    val <- compileFastCore book fid term
    emit $ "itrs += " ++ show itr ++ ";"
    compileFastSave book fid term ctx itr
    emit $ "return " ++ val ++ ";"
    tabDec
    emit $ "}"

  -- Constructor Pattern-Matching (with IfLet)
  else if (case kin of { (IFL _) -> True ; _ -> False }) then do
    let (Var defNam) = val
    let css          = undoIfLetChain defNam term
    let (_, dflt)    = last css
    let othCss       = init css
    emit $ "if (term_tag(" ++ valNam ++ ") == CTR) {"
    tabInc
    emit $ "switch (term_lab(" ++ valNam ++ ")) {"
    tabInc
    reuse' <- gets reus
    itrA <- foldM (\itr (mov, (ctr, fds, bod)) -> do
      emit $ "case " ++ show (mget (ctrToCid book) ctr) ++ ": {"
      tabInc
      reuse (length fds) ("term_loc(" ++ valNam ++ ")")
      forM_ (zip [0..] fds) $ \(k, fd) -> do
        fdNam <- fresh "fd"
        emit $ "Term " ++ fdNam ++ " = got(term_loc(" ++ valNam ++ ") + " ++ show k ++ ");"
        bind fd fdNam
      forM_ mov $ \(key, val) -> do
        valT <- compileFastCore book fid val
        bind key valT
      compileFastBody book fid bod ctx stop (itr + 1 + length fds + length mov)
      tabDec
      emit $ "}"
      modify $ \st -> st { reus = reuse' }
      return (itr + 1 + 1 + length mov)) itr othCss
    emit $ "default: {"
    tabInc
    let (_, [dflNam], dflBod) = dflt
    fdNam <- fresh "fd"
    emit $ "Term " ++ fdNam ++ " = " ++ valNam ++ ";"
    bind dflNam fdNam
    forM_ mov $ \(key, val) -> do
      valT <- compileFastCore book fid val
      bind key valT
    compileFastBody book fid dflBod ctx stop itrA
    tabDec
    emit $ "}"
    tabDec
    emit $ "}"
    tabDec
    emit $ "} else if (term_tag(" ++ valNam ++ ") == ERA) {"
    tabInc
    compileFastBody book fid Era ctx stop (itr + 1)
    tabDec
    emit $ "} else {"
    tabInc
    val <- compileFastCore book fid term
    emit $ "itrs += " ++ show itr ++ ";"
    compileFastSave book fid term ctx itr
    emit $ "return " ++ val ++ ";"
    tabDec
    emit $ "}"

  -- Constructor Pattern-Matching (without IfLet)
  else do
    emit $ "if (term_tag(" ++ valNam ++ ") == CTR) {"
    tabInc
    emit $ "switch (term_lab(" ++ valNam ++ ") - " ++ show (case kin of { (IFL c) -> c ; (MAT c) -> c ; _ -> 0 }) ++ ") {"
    tabInc
    reuse' <- gets reus
    forM_ (zip [0..] css) $ \ (i, (ctr,fds,bod)) -> do
      emit $ "case " ++ show i ++ ": {"
      tabInc
      reuse (length fds) ("term_loc(" ++ valNam ++ ")")
      forM_ (zip [0..] fds) $ \ (k,fd) -> do
        fdNam <- fresh "fd"
        emit $ "Term " ++ fdNam ++ " = got(term_loc(" ++ valNam ++ ") + " ++ show k ++ ");"
        bind fd fdNam
      forM_ mov $ \ (key,val) -> do
        valT <- compileFastCore book fid val
        bind key valT
      compileFastBody book fid bod ctx stop (itr + 1 + length fds + length mov)
      tabDec
      emit $ "}"
      modify $ \st -> st { reus = reuse' }
    tabDec
    emit $ "}"
    tabDec
    emit $ "} else if (term_tag(" ++ valNam ++ ") == ERA) {"
    tabInc
    compileFastBody book fid Era ctx stop (itr + 1)
    tabDec
    emit $ "} else {"
    tabInc
    val <- compileFastCore book fid term
    emit $ "itrs += " ++ show itr ++ ";"
    compileFastSave book fid term ctx itr
    emit $ "return " ++ val ++ ";"
    tabDec
    emit $ "}"
  where
    undoIfLetChain :: String -> Core -> [([(String,Core)], (String, [String], Core))]
    undoIfLetChain expNam term@(Mat _ (Var gotNam) mov [(ctr, fds, bod), ("_", [nxtNam], rest)]) =
      if gotNam == expNam
        then (mov, (ctr, fds, bod)) : undoIfLetChain nxtNam rest
        else [([], ("_", [expNam], term))]
    undoIfLetChain expNam term = [([], ("_", [expNam], term))]

compileFastBody book fid term@(Dup lab dp0 dp1 val bod) ctx stop itr = do
  valT <- compileFastCore book fid val
  valNam <- fresh "val"
  dp0Nam <- fresh "dpA"
  dp1Nam <- fresh "dpB"
  emit $ "Term " ++ valNam ++ " = (" ++ valT ++ ");"
  emit $ "Term " ++ dp0Nam ++ ";"
  emit $ "Term " ++ dp1Nam ++ ";"
  emit $ "if (term_is_atom(" ++ valNam ++ ")) {"
  tabInc
  emit $ "itrs += 1;"
  emit $ dp0Nam ++ " = " ++ valNam ++ ";"
  emit $ dp1Nam ++ " = " ++ valNam ++ ";"
  tabDec
  emit $ "} else {"
  tabInc
  dupNam <- fresh "dup"
  compileFastAlloc dupNam 1
  emit $ "set(" ++ dupNam ++ " + 0, " ++ valNam ++ ");"
  emit $ dp0Nam ++ " = term_new(DP0, " ++ show lab ++ ", " ++ dupNam ++ " + 0);"
  emit $ dp1Nam ++ " = term_new(DP1, " ++ show lab ++ ", " ++ dupNam ++ " + 0);"
  tabDec
  emit $ "}"
  bind dp0 dp0Nam
  bind dp1 dp1Nam
  compileFastBody book fid bod ctx stop itr

compileFastBody book fid term@(Let mode var val bod) ctx stop itr = do
  valT <- compileFastCore book fid val
  case mode of
    LAZY -> do
      bind var valT
    STRI -> do
      case val of
        t@(Ref _ rFid _) -> do
          checkRefAri book t
          valNam <- fresh "val"
          emit $ "Term " ++ valNam ++ " = reduce(" ++ mget (fidToNam book) rFid ++ "_f(" ++ valT ++ "));"
          bind var valNam
        _ -> do
          valNam <- fresh "val" 
          emit $ "Term " ++ valNam ++ " = reduce(" ++ valT ++ ");"
          bind var valNam
  compileFastBody book fid bod ctx stop itr

compileFastBody book fid term@(Ref fNam fFid fArg) ctx stop itr | fFid == fid = do
  checkRefAri book term
  forM_ (zip fArg ctx) $ \ (arg, ctxVar) -> do
    argT <- compileFastCore book fid arg
    emit $ "" ++ ctxVar ++ " = " ++ argT ++ ";"
  emit $ "itrs += " ++ show (itr + 1) ++ ";"
  emit $ "fst_iter = false;"
  emit $ "continue;"

compileFastBody book fid term ctx stop itr = do
  body <- compileFastCore book fid term
  emit $ "itrs += " ++ show itr ++ ";"
  compileFastSave book fid term ctx itr
  emit $ "return " ++ body ++ ";"

-- Completes a fast mode call
compileFastSave :: Book -> Word16 -> Core -> [String] -> Int -> Compile ()
compileFastSave book fid term ctx itr = do
  emit $ "*HVM.itrs += itrs;"

-- Helper function to allocate nodes with reuse
compileFastAlloc :: String -> Int -> Compile ()
compileFastAlloc name 0 = do
  emit $ "Loc " ++ name ++ " = 0;"
compileFastAlloc name arity = do
  reuse <- gets reus
  -- Find the smallest reuse location that's big enough
  -- Very large reuses are usually functions with a lot of state that doesn't need to be moved
  -- Don't fragment those to avoid moving those values (a separate optimization)
  let bigEnough = [(k,locs) | (k,locs) <- MS.toList reuse, k >= arity, k <= arity + 5, not (null locs)]
  case bigEnough of
    [] -> do
      emit $ "Loc " ++ name ++ " = alloc_node(" ++ show arity ++ ");"
    ((k,loc:locs):_) -> do
      emit $ "Loc " ++ name ++ ";"
      -- Too hard to determine statically if reusing is ok in tail-call-optimization
      emit $ "if (fst_iter) {"
      emit $ "  " ++ name ++ " = " ++ loc ++ ";"
      emit $ "} else {"
      emit $ "  " ++ name ++ " = alloc_node(" ++ show arity ++ ");"
      emit $ "}"
      -- Remove the used location
      let reuse' = MS.insert k locs reuse
      -- If we used a location bigger than needed, add the remainder back
      let reuse'' = if k > arity 
                    then MS.insertWith (++) (k - arity) [loc ++ " + " ++ show arity] reuse'
                    else reuse'
      modify $ \st -> st { reus = reuse'' }

-- Compiles a core term in fast mode
compileFastCore :: Book -> Word16 -> Core -> Compile String

compileFastCore book fid Era = 
  return $ "term_new(ERA, 0, 0)"

compileFastCore book fid (Let mode var val bod) = do
  valT <- compileFastCore book fid val
  case mode of
    LAZY -> do
      emit $ "itrs += 1;"
      bind var valT
    STRI -> do
      valNam <- fresh "val"
      emit $ "itrs += 1;"
      emit $ "Term " ++ valNam ++ " = reduce(" ++ valT ++ ");"
      bind var valNam
  compileFastCore book fid bod

compileFastCore book fid (Var name) = do
  compileFastVar name

compileFastCore book fid (Lam var bod) = do
  lamNam <- fresh "lam"
  compileFastAlloc lamNam 1
  bind var $ "term_new(VAR, 0, " ++ lamNam ++ " + 0)"
  bodT <- compileFastCore book fid bod
  emit $ "set(" ++ lamNam ++ " + 0, " ++ bodT ++ ");"
  return $ "term_new(LAM, 0, " ++ lamNam ++ ")"

compileFastCore book fid (App fun arg) = do
  appNam <- fresh "app"
  compileFastAlloc appNam 2
  funT <- compileFastCore book fid fun
  argT <- compileFastCore book fid arg
  emit $ "set(" ++ appNam ++ " + 0, " ++ funT ++ ");"
  emit $ "set(" ++ appNam ++ " + 1, " ++ argT ++ ");"
  return $ "term_new(APP, 0, " ++ appNam ++ ")"

compileFastCore book fid (Sup lab tm0 tm1) = do
  supNam <- fresh "sup"
  compileFastAlloc supNam 2
  tm0T <- compileFastCore book fid tm0
  tm1T <- compileFastCore book fid tm1
  emit $ "set(" ++ supNam ++ " + 0, " ++ tm0T ++ ");"
  emit $ "set(" ++ supNam ++ " + 1, " ++ tm1T ++ ");"
  return $ "term_new(SUP, " ++ show lab ++ ", " ++ supNam ++ ")"

compileFastCore book fid (Dup lab dp0 dp1 val bod) = do
  dupNam <- fresh "dup"
  dp0Nam <- fresh "dpA"
  dp1Nam <- fresh "dpB"
  valNam <- fresh "val"
  valT   <- compileFastCore book fid val
  emit $ "Term " ++ valNam ++ " = (" ++ valT ++ ");"
  emit $ "Term " ++ dp0Nam ++ ";"
  emit $ "Term " ++ dp1Nam ++ ";"
  emit $ "if (term_is_atom(" ++ valNam ++ ")) {"
  tabInc
  emit $ "itrs += 1;"
  emit $ dp0Nam ++ " = " ++ valNam ++ ";"
  emit $ dp1Nam ++ " = " ++ valNam ++ ";"
  tabDec
  emit $ "} else {"
  tabInc
  compileFastAlloc dupNam 1
  emit $ "set(" ++ dupNam ++ " + 0, " ++ valNam ++ ");"
  emit $ dp0Nam ++ " = term_new(DP0, " ++ show lab ++ ", " ++ dupNam ++ " + 0);"
  emit $ dp1Nam ++ " = term_new(DP1, " ++ show lab ++ ", " ++ dupNam ++ " + 0);"
  tabDec
  emit $ "}"
  bind dp0 dp0Nam
  bind dp1 dp1Nam
  compileFastCore book fid bod

compileFastCore book fid (Ctr nam fds) = do
  ctrNam <- fresh "ctr"
  let arity = length fds
  let cid = mget (ctrToCid book) nam
  compileFastAlloc ctrNam arity
  fdsT <- mapM (\ (i,fd) -> compileFastCore book fid fd) (zip [0..] fds)
  sequence_ [emit $ "set(" ++ ctrNam ++ " + " ++ show i ++ ", " ++ fdT ++ ");" | (i,fdT) <- zip [0..] fdsT]
  return $ "term_new(CTR, " ++ show cid ++ ", " ++ ctrNam ++ ")"

compileFastCore book fid tm@(Mat kin val mov css) = do
  matNam <- fresh "mat"
  compileFastAlloc matNam (1 + length css)
  valT <- compileFastCore book fid val
  emit $ "set(" ++ matNam ++ " + 0, " ++ valT ++ ");"
  forM_ (zip [0..] css) $ \(i,(ctr,fds,bod)) -> do
    let bod' = foldr (\x b -> Lam x b) (foldr (\x b -> Lam x b) bod (map fst mov)) fds
    bodT <- compileFastCore book fid bod'
    emit $ "set(" ++ matNam ++ " + " ++ show (i+1) ++ ", " ++ bodT ++ ");"
  let tag = case kin of { SWI -> "SWI" ; (IFL _) -> "IFL" ; (MAT _) -> "MAT" }
  let lab = case kin of { SWI -> fromIntegral (length css) ; (IFL cid) -> cid ; (MAT cid) -> cid }
  retNam <- fresh "ret"
  emit $ "Term " ++ retNam ++ " = term_new(" ++ tag ++ ", " ++ show lab ++ ", " ++ matNam ++ ");"
  foldM (\acc (_, val) -> do
    appNam <- fresh "app"
    compileFastAlloc appNam 2
    emit $ "set(" ++ appNam ++ " + 0, " ++ acc ++ ");"
    valT <- compileFastCore book fid val
    emit $ "set(" ++ appNam ++ " + 1, " ++ valT ++ ");"
    return $ "term_new(APP, 0, " ++ appNam ++ ")") retNam mov

compileFastCore book fid (U32 val) =
  return $ "term_new(W32, 0, " ++ show (fromIntegral val) ++ ")"

compileFastCore book fid (Chr val) =
  return $ "term_new(CHR, 0, " ++ show (fromEnum val) ++ ")"

compileFastCore book fid (Op2 opr nu0 nu1) = do
  opxNam <- fresh "opx"
  retNam <- fresh "ret"
  nu0Nam <- fresh "nu0"
  nu1Nam <- fresh "nu1"
  nu0T <- compileFastCore book fid nu0
  nu1T <- compileFastCore book fid nu1
  emit $ "Term " ++ nu0Nam ++ " = (" ++ nu0T ++ ");"
  emit $ "Term " ++ nu1Nam ++ " = (" ++ nu1T ++ ");"
  emit $ "Term " ++ retNam ++ ";"
  emit $ "if (term_tag(" ++ nu0Nam ++ ") == W32 && term_tag(" ++ nu1Nam ++ ") == W32) {"
  emit $ "  itrs += 2;"
  let oprStr = case opr of
        OP_ADD -> "+"
        OP_SUB -> "-"
        OP_MUL -> "*"
        OP_DIV -> "/"
        OP_MOD -> "%"
        OP_EQ  -> "=="
        OP_NE  -> "!="
        OP_LT  -> "<"
        OP_GT  -> ">"
        OP_LTE -> "<="
        OP_GTE -> ">="
        OP_AND -> "&"
        OP_OR  -> "|"
        OP_XOR -> "^"
        OP_LSH -> "<<"
        OP_RSH -> ">>"
  emit $ "  " ++ retNam ++ " = term_new(W32, 0, term_loc(" ++ nu0Nam ++ ") " ++ oprStr ++ " term_loc(" ++ nu1Nam ++ "));"
  emit $ "} else {"
  tabInc
  compileFastAlloc opxNam 2
  emit $ "set(" ++ opxNam ++ " + 0, " ++ nu0Nam ++ ");"
  emit $ "set(" ++ opxNam ++ " + 1, " ++ nu1Nam ++ ");"
  emit $ retNam ++ " = term_new(OPX, " ++ show (fromEnum opr) ++ ", " ++ opxNam ++ ");"
  tabDec
  emit $ "}"
  return $ retNam

compileFastCore book fid t@(Ref rNam rFid rArg) = do
  checkRefAri book t

  -- Inline Dynamic SUP
  if rNam == "SUP" then do
    let [lab, tm0, tm1] = rArg
    supNam <- fresh "sup"
    labNam <- fresh "lab"
    labT <- compileFastCore book fid lab
    emit $ "Term " ++ labNam ++ " = reduce(" ++ labT ++ ");"
    emit $ "if (term_tag(" ++ labNam ++ ") != W32) {"
    emit $ "  printf(\"ERROR:non-numeric-sup-label\\n\");"
    emit $ "}"
    emit $ "itrs += 1;"
    compileFastAlloc supNam 2
    tm0T <- compileFastCore book fid tm0
    tm1T <- compileFastCore book fid tm1
    emit $ "set(" ++ supNam ++ " + 0, " ++ tm0T ++ ");"
    emit $ "set(" ++ supNam ++ " + 1, " ++ tm1T ++ ");"
    return $ "term_new(SUP, term_loc(" ++ labNam ++ "), " ++ supNam ++ ")"

  -- Inline Dynamic DUP
  else if rNam == "DUP" && (case rArg of [_, _, Lam _ (Lam _ _)] -> True ; _ -> False) then do
    let [lab, val, Lam x (Lam y body)] = rArg
    dupNam <- fresh "dup"
    labNam <- fresh "lab"
    labT <- compileFastCore book fid lab
    emit $ "Term " ++ labNam ++ " = reduce(" ++ labT ++ ");"
    emit $ "if (term_tag(" ++ labNam ++ ") != W32) {"
    emit $ "  printf(\"ERROR:non-numeric-sup-label\\n\");"
    emit $ "}"
    emit $ "itrs += 3;"
    compileFastAlloc dupNam 1
    valT <- compileFastCore book fid val
    emit $ "set(" ++ dupNam ++ " + 0, " ++ valT ++ ");"
    bind x $ "term_new(DP0, term_loc(" ++ labNam ++ "), " ++ dupNam ++ " + 0)"
    bind y $ "term_new(DP1, term_loc(" ++ labNam ++ "), " ++ dupNam ++ " + 0)"
    compileFastCore book fid body

  -- Create REF node
  else do
    refNam <- fresh "ref"
    let arity = length rArg
    compileFastAlloc refNam arity
    argsT <- mapM (\ (i,arg) -> compileFastCore book fid arg) (zip [0..] rArg)
    sequence_ [emit $ "set(" ++ refNam ++ " + " ++ show i ++ ", " ++ argT ++ ");" | (i,argT) <- zip [0..] argsT]
    return $ "term_new(REF, " ++ show rFid ++ ", " ++ refNam ++ ")"

-- Compiles a variable in fast mode
compileFastVar :: String -> Compile String
compileFastVar var = do
  bins <- gets bins
  case MS.lookup var bins of
    Just entry -> do
      return entry
    Nothing -> do
      return $ "<ERR>"

-- Compiles a function using Fast-Mode
compileSlow :: Book -> Word16 -> Core -> Bool -> [(Bool,String)] -> Compile ()
compileSlow book fid core copy args = do
  book <- return book
  emit $ "Term " ++ mget (fidToNam book) fid ++ "_f(Term ref) {"
  emit $ "  return " ++ mget (fidToNam book) fid ++ "_t(ref);"
  emit $ "}"

checkRefAri :: Book -> Core -> Compile ()
checkRefAri book core = do
  case core of
    Ref nam lab arg -> do
      let fid = fromIntegral lab
      let ari = funArity book fid
      let len = length arg
      when (ari /= fromIntegral len) $ do
        error $ "Arity mismatch on term: " ++ show core ++ ". Expected " ++ show ari ++ ", got " ++ show len ++ "."
    _ -> return ()

-- isTailRecursive :: Book -> Word16 -> Core -> Bool
-- isTailRecursive book fid core = go core where
  -- go (Mat _ _ _ css)              = any (\(_, _, bod) -> go bod) css
  -- go (Dup _ _ _ _ bod)            = go bod
  -- go (Let _ _ _ bod)              = go bod
  -- go (Ref _ rFid _) | rFid == fid = True
  -- go _                            = False
