{-./Type.hs-}

-- module Compile where

-- import Control.Monad (forM_, forM, foldM, when)
-- import Control.Monad.State
-- import Data.Bits (shiftL, (.|.))
-- import Data.List
-- import Data.Word
-- import Debug.Trace
-- import Foreign hiding (fresh)
-- import Type
-- import qualified Data.Map.Strict as MS

-- -- Compilation
-- -- -----------

-- data CompileState = CompileState
  -- { next :: Word64
  -- , tabs :: Int
  -- , bins :: MS.Map String String  -- var_name => binder_host
  -- , vars :: [(String, String)]    -- [(var_name, var_host)]
  -- , code :: [String]
  -- , reus :: MS.Map Int [String]   -- arity => [reuse_loc]
  -- }

-- type Compile = State CompileState

-- compileHeaders :: Book -> String
-- compileHeaders book =
  -- let funcs = MS.toList (fidToNam book)
      -- decls = map (\(_, name) -> "Term " ++ name ++ "_f(Term);") funcs
  -- in unlines decls

-- compile :: Book -> Word16 -> String
-- compile book fid =
  -- let full = compileWith compileFull book fid in
  -- let fast = compileWith compileFast book fid in
  -- if "<ERR>" `isInfixOf` fast then full else fast

-- -- Compiles a function using either Fast-Mode or Full-Mode
-- compileWith :: (Book -> Word16 -> Core -> Bool -> [(Bool,String)] -> Compile ()) -> Book -> Word16 -> String
-- compileWith cmp book fid = 
  -- let copy   = fst (fst (mget (fidToFun book) fid)) in
  -- let args   = snd (fst (mget (fidToFun book) fid)) in
  -- let core   = snd (mget (fidToFun book) fid) in
  -- let state  = CompileState 0 0 MS.empty [] [] MS.empty in
  -- let result = runState (cmp book fid core copy args) state in
  -- unlines $ reverse $ code (snd result)

-- emit :: String -> Compile ()
-- emit line = modify $ \st -> st { code = (replicate (tabs st * 2) ' ' ++ line) : code st }

-- tabInc :: Compile ()
-- tabInc = modify $ \st -> st { tabs = tabs st + 1 }

-- tabDec :: Compile ()
-- tabDec = modify $ \st -> st { tabs = tabs st - 1 }

-- bind :: String -> String -> Compile ()
-- bind var host = modify $ \st -> st { bins = MS.insert var host (bins st) }

-- fresh :: String -> Compile String
-- fresh name = do
  -- uid <- gets next
  -- modify $ \s -> s { next = uid + 1 }
  -- return $ name ++ show uid

-- reuse :: Int -> String -> Compile ()
-- reuse arity loc = modify $ \st -> st { reus = MS.insertWith (++) arity [loc] (reus st) }

-- -- Full Compiler
-- -- -------------

-- compileFull :: Book -> Word16 -> Core -> Bool -> [(Bool,String)] -> Compile ()
-- compileFull book fid core copy args = do
  -- emit $ "Term " ++ mget (fidToNam book) fid ++ "_f(Term ref) {"
  -- tabInc
  -- forM_ (zip [0..] args) $ \(i, arg) -> do
    -- argVar <- fresh "arg"
    -- if fst arg
      -- then emit $ "Term " ++ argVar ++ " = reduce_at(term_loc(ref) + " ++ show i ++ ");"
      -- else emit $ "Term " ++ argVar ++ " = got(term_loc(ref) + " ++ show i ++ ");"
    -- let argName = snd arg
    -- bind argName argVar
  -- result <- compileFullCore book fid core "root"
  -- st <- get
  -- forM_ (vars st) $ \ (var,host) -> do
    -- let varTerm = MS.findWithDefault "" var (bins st)
    -- emit $ "set(" ++ host ++ ", " ++ varTerm ++ ");"
  -- emit $ "return " ++ result ++ ";"
  -- tabDec
  -- emit "}"

-- compileFullVar :: String -> String -> Compile String
-- compileFullVar var host = do
  -- bins <- gets bins
  -- case MS.lookup var bins of
    -- Just entry -> do
      -- return entry
    -- Nothing -> do
      -- modify $ \s -> s { vars = (var, host) : vars s }
      -- return "0"

-- compileFullCore :: Book -> Word16 -> Core -> String -> Compile String

-- compileFullCore book fid Era _ = do
  -- return $ "term_new(ERA, 0, 0)"

-- compileFullCore book fid (Var name) host = do
  -- compileFullVar name host

-- compileFullCore book fid (Let mode var val bod) host = do
  -- letNam <- fresh "let"
  -- emit $ "Loc " ++ letNam ++ " = alloc_node(2);"
  -- valT <- compileFullCore book fid val (letNam ++ " + 0")
  -- emit $ "set(" ++ letNam ++ " + 0, " ++ valT ++ ");"
  -- bind var $ "term_new(VAR, 0, " ++ letNam ++ " + 0)"
  -- bodT <- compileFullCore book fid bod (letNam ++ " + 1")
  -- emit $ "set(" ++ letNam ++ " + 1, " ++ bodT ++ ");"
  -- return $ "term_new(LET, " ++ show (fromEnum mode) ++ ", " ++ letNam ++ ")"

-- compileFullCore book fid (Lam var bod) host = do
  -- lamNam <- fresh "lam"
  -- emit $ "Loc " ++ lamNam ++ " = alloc_node(1);"
  -- bind var $ "term_new(VAR, 0, " ++ lamNam ++ " + 0)"
  -- bodT <- compileFullCore book fid bod (lamNam ++ " + 0")
  -- emit $ "set(" ++ lamNam ++ " + 0, " ++ bodT ++ ");"
  -- return $ "term_new(LAM, 0, " ++ lamNam ++ ")"

-- compileFullCore book fid (App fun arg) host = do
  -- appNam <- fresh "app"
  -- emit $ "Loc " ++ appNam ++ " = alloc_node(2);"
  -- funT <- compileFullCore book fid fun (appNam ++ " + 0")
  -- argT <- compileFullCore book fid arg (appNam ++ " + 1")
  -- emit $ "set(" ++ appNam ++ " + 0, " ++ funT ++ ");"
  -- emit $ "set(" ++ appNam ++ " + 1, " ++ argT ++ ");"
  -- return $ "term_new(APP, 0, " ++ appNam ++ ")"

-- compileFullCore book fid (Sup lab tm0 tm1) host = do
  -- supNam <- fresh "sup"
  -- emit $ "Loc " ++ supNam ++ " = alloc_node(2);"
  -- tm0T <- compileFullCore book fid tm0 (supNam ++ " + 0")
  -- tm1T <- compileFullCore book fid tm1 (supNam ++ " + 1")
  -- emit $ "set(" ++ supNam ++ " + 0, " ++ tm0T ++ ");"
  -- emit $ "set(" ++ supNam ++ " + 1, " ++ tm1T ++ ");"
  -- return $ "term_new(SUP, " ++ show lab ++ ", " ++ supNam ++ ")"

-- compileFullCore book fid (Dup lab dp0 dp1 val bod) host = do
  -- dupNam <- fresh "dup"
  -- emit $ "Loc " ++ dupNam ++ " = alloc_node(1);"
  -- bind dp0 $ "term_new(DP0, " ++ show lab ++ ", " ++ dupNam ++ " + 0)"
  -- bind dp1 $ "term_new(DP1, " ++ show lab ++ ", " ++ dupNam ++ " + 0)"
  -- valT <- compileFullCore book fid val (dupNam ++ " + 0")
  -- emit $ "set(" ++ dupNam ++ " + 0, " ++ valT ++ ");"
  -- bodT <- compileFullCore book fid bod host
  -- return bodT

-- compileFullCore book fid (Ctr nam fds) host = do
  -- ctrNam <- fresh "ctr"
  -- let arity = length fds
  -- let cid = mget (ctrToCid book) nam
  -- emit $ "Loc " ++ ctrNam ++ " = alloc_node(" ++ show arity ++ ");"
  -- fdsT <- mapM (\ (i,fd) -> compileFullCore book fid fd (ctrNam ++ " + " ++ show i)) (zip [0..] fds)
  -- sequence_ [emit $ "set(" ++ ctrNam ++ " + " ++ show i ++ ", " ++ fdT ++ ");" | (i,fdT) <- zip [0..] fdsT]
  -- return $ "term_new(CTR, " ++ show cid ++ ", " ++ ctrNam ++ ")"

-- compileFullCore book fid tm@(Mat kin val mov css) host = do
  -- matNam <- fresh "mat"
  -- emit $ "Loc " ++ matNam ++ " = alloc_node(" ++ show (1 + length css) ++ ");"
  -- valT <- compileFullCore book fid val (matNam ++ " + 0")
  -- emit $ "set(" ++ matNam ++ " + 0, " ++ valT ++ ");"
  -- forM_ (zip [0..] css) $ \ (i,(ctr,fds,bod)) -> do
    -- let bod' = foldr (\x b -> Lam x b) (foldr (\x b -> Lam x b) bod (map fst mov)) fds
    -- bodT <- compileFullCore book fid bod' (matNam ++ " + " ++ show (i+1))
    -- emit $ "set(" ++ matNam ++ " + " ++ show (i+1) ++ ", " ++ bodT ++ ");"
  -- let tag = case kin of { SWI -> "SWI" ; (IFL _) -> "IFL" ; (MAT _) -> "MAT" }
  -- let lab = case kin of { SWI -> fromIntegral (length css) ; (IFL cid) -> cid ; (MAT cid) -> cid }
  -- let mat = "term_new(" ++ tag ++ ", " ++ show lab ++ ", " ++ matNam ++ ")"
  -- foldM (\term (key, val) -> do
    -- appNam <- fresh "app"
    -- emit $ "Loc " ++ appNam ++ " = alloc_node(2);"
    -- valT <- compileFullCore book fid val (appNam ++ " + 1")
    -- emit $ "set(" ++ appNam ++ " + 0, " ++ term ++ ");"
    -- emit $ "set(" ++ appNam ++ " + 1, " ++ valT ++ ");"
    -- return $ "term_new(APP, 0, " ++ appNam ++ ")") mat mov

-- compileFullCore book fid (W32 val) _ =
  -- return $ "term_new(W32, 0, " ++ show (fromIntegral val) ++ ")"

-- compileFullCore book fid (Chr val) _ =
  -- return $ "term_new(CHR, 0, " ++ show (fromEnum val) ++ ")"

-- compileFullCore book fid (Op2 opr nu0 nu1) host = do
  -- opxNam <- fresh "opx"
  -- emit $ "Loc " ++ opxNam ++ " = alloc_node(2);"
  -- nu0T <- compileFullCore book fid nu0 (opxNam ++ " + 0")
  -- nu1T <- compileFullCore book fid nu1 (opxNam ++ " + 1")
  -- emit $ "set(" ++ opxNam ++ " + 0, " ++ nu0T ++ ");"
  -- emit $ "set(" ++ opxNam ++ " + 1, " ++ nu1T ++ ");"
  -- return $ "term_new(OPX, " ++ show (fromEnum opr) ++ ", " ++ opxNam ++ ")"

-- compileFullCore book fid t@(Ref rNam rFid rArg) host = do
  -- checkRefAri book t
  -- refNam <- fresh "ref"
  -- let arity = length rArg
  -- emit $ "Loc " ++ refNam ++ " = alloc_node(" ++ show arity ++ ");"
  -- argsT <- mapM (\ (i,arg) -> compileFullCore book fid arg (refNam ++ " + " ++ show i)) (zip [0..] rArg)
  -- sequence_ [emit $ "set(" ++ refNam ++ " + " ++ show i ++ ", " ++ argT ++ ");" | (i,argT) <- zip [0..] argsT]
  -- return $ "term_new(REF, " ++ show rFid ++ ", " ++ refNam ++ ")"

-- -- Fast Compiler
-- -- -------------

-- -- Compiles a function using Fast-Mode
-- compileFast :: Book -> Word16 -> Core -> Bool -> [(Bool,String)] -> Compile ()
-- compileFast book fid core copy args = do
  -- emit $ "Term " ++ mget (fidToNam book) fid ++ "_f(Term ref) {"
  -- tabInc
  -- emit "u64 itrs = 0;"
  -- args <- forM (zip [0..] args) $ \ (i, (strict, arg)) -> do
    -- argNam <- fresh "arg"
    -- if strict then do
      -- emit $ "Term " ++ argNam ++ " = reduce_at(term_loc(ref) + " ++ show i ++ ");"
    -- else do
      -- emit $ "Term " ++ argNam ++ " = got(term_loc(ref) + " ++ show i ++ ");"
    -- if copy && strict then do
      -- case MS.lookup fid (fidToLab book) of
        -- Just labs -> do
          -- emit $ "if (term_tag(" ++ argNam ++ ") == ERA) {"
          -- emit $ "  itrs += 1;"
          -- emit $ "  *HVM.itrs += itrs;"
          -- emit $ "  return term_new(ERA, 0, 0);"
          -- emit $ "}"
          -- emit $ "if (term_tag(" ++ argNam ++ ") == SUP) {"
          -- tabInc
          -- emit $ "u64 lab = term_lab(" ++ argNam ++ ");"
          -- emit $ "if (1"
          -- forM_ (MS.keys labs) $ \lab -> do
            -- emit $ "    && lab != " ++ show lab
          -- emit $ ") {"
          -- tabInc
          -- emit $ "Term term = reduce_ref_sup(ref, " ++ show i ++ ");"
          -- emit $ "return term;"
          -- tabDec
          -- emit $ "}"
          -- tabDec
          -- emit $ "}"
        -- Nothing -> return ()
    -- else
      -- return ()
    -- bind arg argNam
    -- return argNam
  -- reuse (length (snd (fst (mget (fidToFun book) fid)))) "term_loc(ref)"
  -- compileFastArgs book fid core args
  -- tabDec
  -- emit "}"

-- -- Compiles a fast function's argument list
-- compileFastArgs :: Book -> Word16 -> Core -> [String] -> Compile ()
-- compileFastArgs book fid body ctx = do
  -- emit $ "_Bool fst_iter = 1;"
  -- emit $ "while (1) {"
  -- tabInc
  -- compileFastBody book fid body ctx False 0
  -- tabDec
  -- emit $ "}"

-- -- Compiles a fast function body (pattern-matching)
-- compileFastBody :: Book -> Word16 -> Core -> [String] -> Bool -> Int -> Compile ()
-- compileFastBody book fid term@(Mat kin val mov css) ctx stop@False itr = do
  -- valT   <- compileFastCore book fid val
  -- valNam <- fresh "val"
  -- emit $ "Term " ++ valNam ++ " = (" ++ valT ++ ");"
  -- let isNumeric = length css > 0 && (let (ctr,fds,bod) = css !! 0 in ctr == "0")

  -- -- Numeric Pattern-Matching
  -- if isNumeric then do
    -- numNam <- fresh "num"
    -- emit $ "if (term_tag("++valNam++") == W32) {"
    -- tabInc
    -- emit $ "u32 " ++ numNam ++ " = term_loc(" ++ valNam ++ ");"
    -- emit $ "switch (" ++ numNam ++ ") {"
    -- tabInc
    -- forM_ (zip [0..] css) $ \ (i, (ctr,fds,bod)) -> do
      -- if i < length css - 1 then do
        -- emit $ "case " ++ show i ++ ": {"
        -- tabInc
        -- forM_ mov $ \ (key,val) -> do
          -- valT <- compileFastCore book fid val
          -- bind key valT
        -- compileFastBody book fid bod ctx stop (itr + 1 + length mov)
        -- tabDec
        -- emit $ "}"
      -- else do
        -- emit $ "default: {"
        -- tabInc
        -- preNam <- fresh "pre"
        -- emit $ "Term " ++ preNam ++ " = " ++ "term_new(W32, 0, "++numNam++" - "++show (length css - 1)++");"
        -- forM_ fds $ \ fd -> do
          -- bind fd preNam
        -- forM_ mov $ \ (key,val) -> do
          -- valT <- compileFastCore book fid val
          -- bind key valT
        -- compileFastBody book fid bod ctx stop (itr + 1 + length fds + length mov)
        -- tabDec
        -- emit $ "}"
    -- tabDec
    -- emit $ "}"
    -- tabDec
    -- emit $ "} else if (term_tag(" ++ valNam ++ ") == ERA) {"
    -- tabInc
    -- compileFastBody book fid Era ctx stop (itr + 1)
    -- tabDec
    -- emit $ "} else {"
    -- tabInc
    -- val <- compileFastCore book fid term
    -- emit $ "itrs += " ++ show itr ++ ";"
    -- compileFastSave book fid term ctx itr
    -- emit $ "return " ++ val ++ ";"
    -- tabDec
    -- emit $ "}"

  -- -- Constructor Pattern-Matching (with IfLet)
  -- else if (case kin of { (IFL _) -> True ; _ -> False }) then do
    -- let (Var defNam) = val
    -- let css          = undoIfLetChain defNam term
    -- let (_, dflt)    = last css
    -- let othCss       = init css
    -- emit $ "if (term_tag(" ++ valNam ++ ") == CTR) {"
    -- tabInc
    -- emit $ "switch (term_lab(" ++ valNam ++ ")) {"
    -- tabInc
    -- reuse' <- gets reus
    -- itrA <- foldM (\itr (mov, (ctr, fds, bod)) -> do
      -- emit $ "case " ++ show (mget (ctrToCid book) ctr) ++ ": {"
      -- tabInc
      -- reuse (length fds) ("term_loc(" ++ valNam ++ ")")
      -- forM_ (zip [0..] fds) $ \(k, fd) -> do
        -- fdNam <- fresh "fd"
        -- emit $ "Term " ++ fdNam ++ " = got(term_loc(" ++ valNam ++ ") + " ++ show k ++ ");"
        -- bind fd fdNam
      -- forM_ mov $ \(key, val) -> do
        -- valT <- compileFastCore book fid val
        -- bind key valT
      -- compileFastBody book fid bod ctx stop (itr + 1 + length fds + length mov)
      -- tabDec
      -- emit $ "}"
      -- modify $ \st -> st { reus = reuse' }
      -- return (itr + 1 + 1 + length mov)) itr othCss
    -- emit $ "default: {"
    -- tabInc
    -- let (_, [dflNam], dflBod) = dflt
    -- fdNam <- fresh "fd"
    -- emit $ "Term " ++ fdNam ++ " = " ++ valNam ++ ";"
    -- bind dflNam fdNam
    -- forM_ mov $ \(key, val) -> do
      -- valT <- compileFastCore book fid val
      -- bind key valT
    -- compileFastBody book fid dflBod ctx stop itrA
    -- tabDec
    -- emit $ "}"
    -- tabDec
    -- emit $ "}"
    -- tabDec
    -- emit $ "} else if (term_tag(" ++ valNam ++ ") == ERA) {"
    -- tabInc
    -- compileFastBody book fid Era ctx stop (itr + 1)
    -- tabDec
    -- emit $ "} else {"
    -- tabInc
    -- val <- compileFastCore book fid term
    -- emit $ "itrs += " ++ show itr ++ ";"
    -- compileFastSave book fid term ctx itr
    -- emit $ "return " ++ val ++ ";"
    -- tabDec
    -- emit $ "}"

  -- -- Constructor Pattern-Matching (without IfLet)
  -- else do
    -- emit $ "if (term_tag(" ++ valNam ++ ") == CTR) {"
    -- tabInc
    -- emit $ "switch (term_lab(" ++ valNam ++ ") - " ++ show (case kin of { (IFL c) -> c ; (MAT c) -> c ; _ -> 0 }) ++ ") {"
    -- tabInc
    -- reuse' <- gets reus
    -- forM_ (zip [0..] css) $ \ (i, (ctr,fds,bod)) -> do
      -- emit $ "case " ++ show i ++ ": {"
      -- tabInc
      -- reuse (length fds) ("term_loc(" ++ valNam ++ ")")
      -- forM_ (zip [0..] fds) $ \ (k,fd) -> do
        -- fdNam <- fresh "fd"
        -- emit $ "Term " ++ fdNam ++ " = got(term_loc(" ++ valNam ++ ") + " ++ show k ++ ");"
        -- bind fd fdNam
      -- forM_ mov $ \ (key,val) -> do
        -- valT <- compileFastCore book fid val
        -- bind key valT
      -- compileFastBody book fid bod ctx stop (itr + 1 + length fds + length mov)
      -- tabDec
      -- emit $ "}"
      -- modify $ \st -> st { reus = reuse' }
    -- tabDec
    -- emit $ "}"
    -- tabDec
    -- emit $ "} else if (term_tag(" ++ valNam ++ ") == ERA) {"
    -- tabInc
    -- compileFastBody book fid Era ctx stop (itr + 1)
    -- tabDec
    -- emit $ "} else {"
    -- tabInc
    -- val <- compileFastCore book fid term
    -- emit $ "itrs += " ++ show itr ++ ";"
    -- compileFastSave book fid term ctx itr
    -- emit $ "return " ++ val ++ ";"
    -- tabDec
    -- emit $ "}"
  -- where
    -- undoIfLetChain :: String -> Core -> [([(String,Core)], (String, [String], Core))]
    -- undoIfLetChain expNam term@(Mat _ (Var gotNam) mov [(ctr, fds, bod), ("_", [nxtNam], rest)]) =
      -- if gotNam == expNam
        -- then (mov, (ctr, fds, bod)) : undoIfLetChain nxtNam rest
        -- else [([], ("_", [expNam], term))]
    -- undoIfLetChain expNam term = [([], ("_", [expNam], term))]

-- compileFastBody book fid term@(Dup lab dp0 dp1 val bod) ctx stop itr = do
  -- valT <- compileFastCore book fid val
  -- valNam <- fresh "val"
  -- dp0Nam <- fresh "dpA"
  -- dp1Nam <- fresh "dpB"
  -- emit $ "Term " ++ valNam ++ " = (" ++ valT ++ ");"
  -- emit $ "Term " ++ dp0Nam ++ ";"
  -- emit $ "Term " ++ dp1Nam ++ ";"
  -- emit $ "if (term_is_atom(" ++ valNam ++ ")) {"
  -- tabInc
  -- emit $ "itrs += 1;"
  -- emit $ dp0Nam ++ " = " ++ valNam ++ ";"
  -- emit $ dp1Nam ++ " = " ++ valNam ++ ";"
  -- tabDec
  -- emit $ "} else {"
  -- tabInc
  -- dupNam <- fresh "dup"
  -- compileFastAlloc dupNam 1
  -- emit $ "set(" ++ dupNam ++ " + 0, " ++ valNam ++ ");"
  -- emit $ dp0Nam ++ " = term_new(DP0, " ++ show lab ++ ", " ++ dupNam ++ " + 0);"
  -- emit $ dp1Nam ++ " = term_new(DP1, " ++ show lab ++ ", " ++ dupNam ++ " + 0);"
  -- tabDec
  -- emit $ "}"
  -- bind dp0 dp0Nam
  -- bind dp1 dp1Nam
  -- compileFastBody book fid bod ctx stop itr

-- compileFastBody book fid term@(Let mode var val bod) ctx stop itr = do
  -- valT <- compileFastCore book fid val
  -- case mode of
    -- LAZY -> do
      -- bind var valT
    -- STRI -> do
      -- case val of
        -- t@(Ref _ rFid _) -> do
          -- checkRefAri book t
          -- valNam <- fresh "val"
          -- emit $ "Term " ++ valNam ++ " = reduce(" ++ mget (fidToNam book) rFid ++ "_f(" ++ valT ++ "));"
          -- bind var valNam
        -- _ -> do
          -- valNam <- fresh "val" 
          -- emit $ "Term " ++ valNam ++ " = reduce(" ++ valT ++ ");"
          -- bind var valNam
  -- compileFastBody book fid bod ctx stop itr

-- compileFastBody book fid term@(Ref fNam fFid fArg) ctx stop itr | fFid == fid = do
  -- checkRefAri book term
  -- forM_ (zip fArg ctx) $ \ (arg, ctxVar) -> do
    -- argT <- compileFastCore book fid arg
    -- emit $ "" ++ ctxVar ++ " = " ++ argT ++ ";"
  -- emit $ "itrs += " ++ show (itr + 1) ++ ";"
  -- emit $ "fst_iter = false;"
  -- emit $ "continue;"

-- compileFastBody book fid term ctx stop itr = do
  -- body <- compileFastCore book fid term
  -- emit $ "itrs += " ++ show itr ++ ";"
  -- compileFastSave book fid term ctx itr
  -- emit $ "return " ++ body ++ ";"

-- -- Completes a fast mode call
-- compileFastSave :: Book -> Word16 -> Core -> [String] -> Int -> Compile ()
-- compileFastSave book fid term ctx itr = do
  -- emit $ "*HVM.itrs += itrs;"

-- -- Helper function to allocate nodes with reuse
-- compileFastAlloc :: String -> Int -> Compile ()
-- compileFastAlloc name 0 = do
  -- emit $ "Loc " ++ name ++ " = 0;"
-- compileFastAlloc name arity = do
  -- reuse <- gets reus
  -- -- Find the smallest reuse location that's big enough
  -- -- Very large reuses are usually functions with a lot of state that doesn't need to be moved
  -- -- Don't fragment those to avoid moving those values (a separate optimization)
  -- let bigEnough = [(k,locs) | (k,locs) <- MS.toList reuse, k >= arity, k <= arity + 5, not (null locs)]
  -- case bigEnough of
    -- [] -> do
      -- emit $ "Loc " ++ name ++ " = alloc_node(" ++ show arity ++ ");"
    -- ((k,loc:locs):_) -> do
      -- emit $ "Loc " ++ name ++ ";"
      -- -- Too hard to determine statically if reusing is ok in tail-call-optimization
      -- emit $ "if (fst_iter) {"
      -- emit $ "  " ++ name ++ " = " ++ loc ++ ";"
      -- emit $ "} else {"
      -- emit $ "  " ++ name ++ " = alloc_node(" ++ show arity ++ ");"
      -- emit $ "}"
      -- -- Remove the used location
      -- let reuse' = MS.insert k locs reuse
      -- -- If we used a location bigger than needed, add the remainder back
      -- let reuse'' = if k > arity 
                    -- then MS.insertWith (++) (k - arity) [loc ++ " + " ++ show arity] reuse'
                    -- else reuse'
      -- modify $ \st -> st { reus = reuse'' }

-- -- Compiles a core term in fast mode
-- compileFastCore :: Book -> Word16 -> Core -> Compile String

-- compileFastCore book fid Era = 
  -- return $ "term_new(ERA, 0, 0)"

-- compileFastCore book fid (Let mode var val bod) = do
  -- valT <- compileFastCore book fid val
  -- case mode of
    -- LAZY -> do
      -- emit $ "itrs += 1;"
      -- bind var valT
    -- STRI -> do
      -- valNam <- fresh "val"
      -- emit $ "itrs += 1;"
      -- emit $ "Term " ++ valNam ++ " = reduce(" ++ valT ++ ");"
      -- bind var valNam
  -- compileFastCore book fid bod

-- compileFastCore book fid (Var name) = do
  -- compileFastVar name

-- compileFastCore book fid (Lam var bod) = do
  -- lamNam <- fresh "lam"
  -- compileFastAlloc lamNam 1
  -- bind var $ "term_new(VAR, 0, " ++ lamNam ++ " + 0)"
  -- bodT <- compileFastCore book fid bod
  -- emit $ "set(" ++ lamNam ++ " + 0, " ++ bodT ++ ");"
  -- return $ "term_new(LAM, 0, " ++ lamNam ++ ")"

-- compileFastCore book fid (App fun arg) = do
  -- appNam <- fresh "app"
  -- compileFastAlloc appNam 2
  -- funT <- compileFastCore book fid fun
  -- argT <- compileFastCore book fid arg
  -- emit $ "set(" ++ appNam ++ " + 0, " ++ funT ++ ");"
  -- emit $ "set(" ++ appNam ++ " + 1, " ++ argT ++ ");"
  -- return $ "term_new(APP, 0, " ++ appNam ++ ")"

-- compileFastCore book fid (Sup lab tm0 tm1) = do
  -- supNam <- fresh "sup"
  -- compileFastAlloc supNam 2
  -- tm0T <- compileFastCore book fid tm0
  -- tm1T <- compileFastCore book fid tm1
  -- emit $ "set(" ++ supNam ++ " + 0, " ++ tm0T ++ ");"
  -- emit $ "set(" ++ supNam ++ " + 1, " ++ tm1T ++ ");"
  -- return $ "term_new(SUP, " ++ show lab ++ ", " ++ supNam ++ ")"

-- compileFastCore book fid (Dup lab dp0 dp1 val bod) = do
  -- dupNam <- fresh "dup"
  -- dp0Nam <- fresh "dpA"
  -- dp1Nam <- fresh "dpB"
  -- valNam <- fresh "val"
  -- valT   <- compileFastCore book fid val
  -- emit $ "Term " ++ valNam ++ " = (" ++ valT ++ ");"
  -- emit $ "Term " ++ dp0Nam ++ ";"
  -- emit $ "Term " ++ dp1Nam ++ ";"
  -- emit $ "if (term_is_atom(" ++ valNam ++ ")) {"
  -- tabInc
  -- emit $ "itrs += 1;"
  -- emit $ dp0Nam ++ " = " ++ valNam ++ ";"
  -- emit $ dp1Nam ++ " = " ++ valNam ++ ";"
  -- tabDec
  -- emit $ "} else {"
  -- tabInc
  -- compileFastAlloc dupNam 1
  -- emit $ "set(" ++ dupNam ++ " + 0, " ++ valNam ++ ");"
  -- emit $ dp0Nam ++ " = term_new(DP0, " ++ show lab ++ ", " ++ dupNam ++ " + 0);"
  -- emit $ dp1Nam ++ " = term_new(DP1, " ++ show lab ++ ", " ++ dupNam ++ " + 0);"
  -- tabDec
  -- emit $ "}"
  -- bind dp0 dp0Nam
  -- bind dp1 dp1Nam
  -- compileFastCore book fid bod

-- compileFastCore book fid (Ctr nam fds) = do
  -- ctrNam <- fresh "ctr"
  -- let arity = length fds
  -- let cid = mget (ctrToCid book) nam
  -- compileFastAlloc ctrNam arity
  -- fdsT <- mapM (\ (i,fd) -> compileFastCore book fid fd) (zip [0..] fds)
  -- sequence_ [emit $ "set(" ++ ctrNam ++ " + " ++ show i ++ ", " ++ fdT ++ ");" | (i,fdT) <- zip [0..] fdsT]
  -- return $ "term_new(CTR, " ++ show cid ++ ", " ++ ctrNam ++ ")"

-- compileFastCore book fid tm@(Mat kin val mov css) = do
  -- matNam <- fresh "mat"
  -- compileFastAlloc matNam (1 + length css)
  -- valT <- compileFastCore book fid val
  -- emit $ "set(" ++ matNam ++ " + 0, " ++ valT ++ ");"
  -- forM_ (zip [0..] css) $ \(i,(ctr,fds,bod)) -> do
    -- let bod' = foldr (\x b -> Lam x b) (foldr (\x b -> Lam x b) bod (map fst mov)) fds
    -- bodT <- compileFastCore book fid bod'
    -- emit $ "set(" ++ matNam ++ " + " ++ show (i+1) ++ ", " ++ bodT ++ ");"
  -- let tag = case kin of { SWI -> "SWI" ; (IFL _) -> "IFL" ; (MAT _) -> "MAT" }
  -- let lab = case kin of { SWI -> fromIntegral (length css) ; (IFL cid) -> cid ; (MAT cid) -> cid }
  -- retNam <- fresh "ret"
  -- emit $ "Term " ++ retNam ++ " = term_new(" ++ tag ++ ", " ++ show lab ++ ", " ++ matNam ++ ");"
  -- foldM (\acc (_, val) -> do
    -- appNam <- fresh "app"
    -- compileFastAlloc appNam 2
    -- emit $ "set(" ++ appNam ++ " + 0, " ++ acc ++ ");"
    -- valT <- compileFastCore book fid val
    -- emit $ "set(" ++ appNam ++ " + 1, " ++ valT ++ ");"
    -- return $ "term_new(APP, 0, " ++ appNam ++ ")") retNam mov

-- compileFastCore book fid (W32 val) =
  -- return $ "term_new(W32, 0, " ++ show (fromIntegral val) ++ ")"

-- compileFastCore book fid (Chr val) =
  -- return $ "term_new(CHR, 0, " ++ show (fromEnum val) ++ ")"

-- compileFastCore book fid (Op2 opr nu0 nu1) = do
  -- opxNam <- fresh "opx"
  -- retNam <- fresh "ret"
  -- nu0Nam <- fresh "nu0"
  -- nu1Nam <- fresh "nu1"
  -- nu0T <- compileFastCore book fid nu0
  -- nu1T <- compileFastCore book fid nu1
  -- emit $ "Term " ++ nu0Nam ++ " = (" ++ nu0T ++ ");"
  -- emit $ "Term " ++ nu1Nam ++ " = (" ++ nu1T ++ ");"
  -- emit $ "Term " ++ retNam ++ ";"
  -- emit $ "if (term_tag(" ++ nu0Nam ++ ") == W32 && term_tag(" ++ nu1Nam ++ ") == W32) {"
  -- emit $ "  itrs += 2;"
  -- let oprStr = case opr of
        -- OP_ADD -> "+"
        -- OP_SUB -> "-"
        -- OP_MUL -> "*"
        -- OP_DIV -> "/"
        -- OP_MOD -> "%"
        -- OP_EQ  -> "=="
        -- OP_NE  -> "!="
        -- OP_LT  -> "<"
        -- OP_GT  -> ">"
        -- OP_LTE -> "<="
        -- OP_GTE -> ">="
        -- OP_AND -> "&"
        -- OP_OR  -> "|"
        -- OP_XOR -> "^"
        -- OP_LSH -> "<<"
        -- OP_RSH -> ">>"
  -- emit $ "  " ++ retNam ++ " = term_new(W32, 0, term_loc(" ++ nu0Nam ++ ") " ++ oprStr ++ " term_loc(" ++ nu1Nam ++ "));"
  -- emit $ "} else {"
  -- tabInc
  -- compileFastAlloc opxNam 2
  -- emit $ "set(" ++ opxNam ++ " + 0, " ++ nu0Nam ++ ");"
  -- emit $ "set(" ++ opxNam ++ " + 1, " ++ nu1Nam ++ ");"
  -- emit $ retNam ++ " = term_new(OPX, " ++ show (fromEnum opr) ++ ", " ++ opxNam ++ ");"
  -- tabDec
  -- emit $ "}"
  -- return $ retNam

-- compileFastCore book fid t@(Ref rNam rFid rArg) = do
  -- checkRefAri book t

  -- -- Inline Dynamic SUP
  -- if rNam == "SUP" then do
    -- let [lab, tm0, tm1] = rArg
    -- supNam <- fresh "sup"
    -- labNam <- fresh "lab"
    -- labT <- compileFastCore book fid lab
    -- emit $ "Term " ++ labNam ++ " = reduce(" ++ labT ++ ");"
    -- emit $ "if (term_tag(" ++ labNam ++ ") != W32) {"
    -- emit $ "  printf(\"ERROR:non-numeric-sup-label\\n\");"
    -- emit $ "}"
    -- emit $ "itrs += 1;"
    -- compileFastAlloc supNam 2
    -- tm0T <- compileFastCore book fid tm0
    -- tm1T <- compileFastCore book fid tm1
    -- emit $ "set(" ++ supNam ++ " + 0, " ++ tm0T ++ ");"
    -- emit $ "set(" ++ supNam ++ " + 1, " ++ tm1T ++ ");"
    -- return $ "term_new(SUP, term_loc(" ++ labNam ++ "), " ++ supNam ++ ")"

  -- -- Inline Dynamic DUP
  -- else if rNam == "DUP" && (case rArg of [_, _, Lam _ (Lam _ _)] -> True ; _ -> False) then do
    -- let [lab, val, Lam x (Lam y body)] = rArg
    -- dupNam <- fresh "dup"
    -- labNam <- fresh "lab"
    -- labT <- compileFastCore book fid lab
    -- emit $ "Term " ++ labNam ++ " = reduce(" ++ labT ++ ");"
    -- emit $ "if (term_tag(" ++ labNam ++ ") != W32) {"
    -- emit $ "  printf(\"ERROR:non-numeric-sup-label\\n\");"
    -- emit $ "}"
    -- emit $ "itrs += 3;"
    -- compileFastAlloc dupNam 1
    -- valT <- compileFastCore book fid val
    -- emit $ "set(" ++ dupNam ++ " + 0, " ++ valT ++ ");"
    -- bind x $ "term_new(DP0, term_loc(" ++ labNam ++ "), " ++ dupNam ++ " + 0)"
    -- bind y $ "term_new(DP1, term_loc(" ++ labNam ++ "), " ++ dupNam ++ " + 0)"
    -- compileFastCore book fid body

  -- -- Create REF node
  -- else do
    -- refNam <- fresh "ref"
    -- let arity = length rArg
    -- compileFastAlloc refNam arity
    -- argsT <- mapM (\ (i,arg) -> compileFastCore book fid arg) (zip [0..] rArg)
    -- sequence_ [emit $ "set(" ++ refNam ++ " + " ++ show i ++ ", " ++ argT ++ ");" | (i,argT) <- zip [0..] argsT]
    -- return $ "term_new(REF, " ++ show rFid ++ ", " ++ refNam ++ ")"

-- -- Compiles a variable in fast mode
-- compileFastVar :: String -> Compile String
-- compileFastVar var = do
  -- bins <- gets bins
  -- case MS.lookup var bins of
    -- Just entry -> do
      -- return entry
    -- Nothing -> do
      -- return $ "<ERR>"

-- checkRefAri :: Book -> Core -> Compile ()
-- checkRefAri book core = do
  -- case core of
    -- Ref nam lab arg -> do
      -- let fid = fromIntegral lab
      -- let ari = funArity book fid
      -- let len = length arg
      -- when (ari /= fromIntegral len) $ do
        -- error $ "Arity mismatch on term: " ++ show core ++ ". Expected " ++ show ari ++ ", got " ++ show len ++ "."
    -- _ -> return ()

-- UPDATED COMPILE.HS:

-- module Compile where

-- import Control.Monad (forM_, forM, foldM, when)
-- import Control.Monad.State
-- import Data.Bits (shiftL, (.|.))
-- import Data.List
-- import Data.Word
-- import Debug.Trace
-- import Foreign hiding (fresh)
-- import Type
-- import qualified Data.Map.Strict as MS

-- -- Compilation
-- -- -----------

-- data CompileState = CompileState
  -- { next :: Word64
  -- , tabs :: Int
  -- , bins :: MS.Map String String  -- var_name => binder_host
  -- , vars :: [(String, String)]    -- [(var_name, var_host)]
  -- , code :: [String]
  -- , reus :: MS.Map Int [String]   -- arity => [reuse_loc]
  -- }

-- type Compile = State CompileState

-- compileHeaders :: Book -> String
-- compileHeaders book =
  -- let funcs = MS.toList (fidToNam book)
      -- decls = map (\(_, name) -> "Term " ++ name ++ "_f(Term);") funcs
  -- in unlines decls

-- compile :: Book -> Word16 -> String
-- compile book fid =
  -- let full = compileWith compileFull book fid in
  -- let fast = compileWith compileFast book fid in
  -- if "<ERR>" `isInfixOf` fast then full else fast

-- -- Compiles a function using either Fast-Mode or Full-Mode
-- compileWith :: (Book -> Word16 -> Core -> Bool -> [(Bool,String)] -> Compile ()) -> Book -> Word16 -> String
-- compileWith cmp book fid = 
  -- let copy   = fst (fst (mget (fidToFun book) fid)) in
  -- let args   = snd (fst (mget (fidToFun book) fid)) in
  -- let core   = snd (mget (fidToFun book) fid) in
  -- let state  = CompileState 0 0 MS.empty [] [] MS.empty in
  -- let result = runState (cmp book fid core copy args) state in
  -- unlines $ reverse $ code (snd result)

-- emit :: String -> Compile ()
-- emit line = modify $ \st -> st { code = (replicate (tabs st * 2) ' ' ++ line) : code st }

-- tabInc :: Compile ()
-- tabInc = modify $ \st -> st { tabs = tabs st + 1 }

-- tabDec :: Compile ()
-- tabDec = modify $ \st -> st { tabs = tabs st - 1 }

-- bind :: String -> String -> Compile ()
-- bind var host = modify $ \st -> st { bins = MS.insert var host (bins st) }

-- fresh :: String -> Compile String
-- fresh name = do
  -- uid <- gets next
  -- modify $ \s -> s { next = uid + 1 }
  -- return $ name ++ show uid

-- reuse :: Int -> String -> Compile ()
-- reuse arity loc = modify $ \st -> st { reus = MS.insertWith (++) arity [loc] (reus st) }

-- -- Full Compiler
-- -- -------------

-- compileFull :: Book -> Word16 -> Core -> Bool -> [(Bool,String)] -> Compile ()
-- compileFull book fid core copy args = do
  -- emit $ "Term " ++ mget (fidToNam book) fid ++ "_f(Term ref) {"
  -- tabInc
  -- forM_ (zip [0..] args) $ \(i, arg) -> do
    -- argVar <- fresh "arg"
    -- if fst arg
      -- then emit $ "Term " ++ argVar ++ " = reduce_at(term_loc(ref) + " ++ show i ++ ");"
      -- else emit $ "Term " ++ argVar ++ " = got(term_loc(ref) + " ++ show i ++ ");"
    -- let argName = snd arg
    -- bind argName argVar
  -- result <- compileFullCore book fid core "root"
  -- st <- get
  -- forM_ (vars st) $ \ (var,host) -> do
    -- let varTerm = MS.findWithDefault "" var (bins st)
    -- emit $ "set(" ++ host ++ ", " ++ varTerm ++ ");"
  -- emit $ "return " ++ result ++ ";"
  -- tabDec
  -- emit "}"

-- compileFullVar :: String -> String -> Compile String
-- compileFullVar var host = do
  -- bins <- gets bins
  -- case MS.lookup var bins of
    -- Just entry -> do
      -- return entry
    -- Nothing -> do
      -- modify $ \s -> s { vars = (var, host) : vars s }
      -- return "0"

-- compileFullCore :: Book -> Word16 -> Core -> String -> Compile String

-- compileFullCore _ _ (Var x) host = do
  -- compileFullVar x host

-- compileFullCore book fid t@(Ref x i xs) host = do
  -- checkRefAri book t
  -- refNam <- fresh "ref"
  -- let arity = length xs
  -- emit $ "Loc " ++ refNam ++ " = alloc_node(" ++ show arity ++ ");"
  -- argsT <- mapM (\ (i,arg) -> compileFullCore book fid arg (refNam ++ " + " ++ show i)) (zip [0..] xs)
  -- sequence_ [emit $ "set(" ++ refNam ++ " + " ++ show i ++ ", " ++ argT ++ ");" | (i,argT) <- zip [0..] argsT]
  -- return $ "term_new(REF, " ++ show i ++ ", " ++ refNam ++ ")"

-- compileFullCore book fid (Let mode var val bod) host = do
  -- letNam <- fresh "let"
  -- emit $ "Loc " ++ letNam ++ " = alloc_node(2);"
  -- valT <- compileFullCore book fid val (letNam ++ " + 0")
  -- emit $ "set(" ++ letNam ++ " + 0, " ++ valT ++ ");"
  -- bind var $ "term_new(VAR, 0, " ++ letNam ++ " + 0)"
  -- bodT <- compileFullCore book fid bod (letNam ++ " + 1")
  -- emit $ "set(" ++ letNam ++ " + 1, " ++ bodT ++ ");"
  -- return $ "term_new(LET, " ++ show (fromEnum mode) ++ ", " ++ letNam ++ ")"

-- compileFullCore _ _ Era _ = do
  -- return $ "term_new(ERA, 0, 0)"

-- compileFullCore book fid (Sup lab a b) host = do
  -- supNam <- fresh "sup"
  -- emit $ "Loc " ++ supNam ++ " = alloc_node(2);"
  -- aT <- compileFullCore book fid a (supNam ++ " + 0")
  -- bT <- compileFullCore book fid b (supNam ++ " + 1")
  -- emit $ "set(" ++ supNam ++ " + 0, " ++ aT ++ ");"
  -- emit $ "set(" ++ supNam ++ " + 1, " ++ bT ++ ");"
  -- return $ "term_new(SUP, " ++ show lab ++ ", " ++ supNam ++ ")"

-- compileFullCore book fid (Dup lab x y v f) host = do
  -- dupNam <- fresh "dup"
  -- emit $ "Loc " ++ dupNam ++ " = alloc_node(1);"
  -- bind x $ "term_new(DP0, " ++ show lab ++ ", " ++ dupNam ++ " + 0)"
  -- bind y $ "term_new(DP1, " ++ show lab ++ ", " ++ dupNam ++ " + 0)"
  -- valT <- compileFullCore book fid v (dupNam ++ " + 0")
  -- emit $ "set(" ++ dupNam ++ " + 0, " ++ valT ++ ");"
  -- bodT <- compileFullCore book fid f host
  -- return bodT

-- compileFullCore _ _ Set _ = do
  -- return $ "term_new(SET, 0, 0)"

-- compileFullCore _ _ Emp _ = do
  -- return $ "term_new(EMP, 0, 0)"

-- compileFullCore book fid (Efq c ms) host = do
  -- efqNam <- fresh "efq"
  -- emit $ "Loc " ++ efqNam ++ " = alloc_node(1);"
  -- cT <- compileFullCore book fid c (efqNam ++ " + 0")
  -- emit $ "set(" ++ efqNam ++ " + 0, " ++ cT ++ ");"
  -- let efq = "term_new(EFQ, 0, " ++ efqNam ++ ")"
  -- foldM (\term (key, val) -> do
    -- appNam <- fresh "app"
    -- emit $ "Loc " ++ appNam ++ " = alloc_node(2);"
    -- valT <- compileFullCore book fid val (appNam ++ " + 1")
    -- emit $ "set(" ++ appNam ++ " + 0, " ++ term ++ ");"
    -- emit $ "set(" ++ appNam ++ " + 1, " ++ valT ++ ");"
    -- return $ "term_new(APP, 0, " ++ appNam ++ ")") efq ms

-- compileFullCore _ _ Uni _ = do
  -- return $ "term_new(UNI, 0, 0)"

-- compileFullCore _ _ Nil _ = do
  -- return $ "term_new(NIL, 0, 0)"

-- compileFullCore book fid (Use c ms b) host = do
  -- useNam <- fresh "use"
  -- emit $ "Loc " ++ useNam ++ " = alloc_node(2);"
  -- cT <- compileFullCore book fid c (useNam ++ " + 0")
  -- bT <- compileFullCore book fid b (useNam ++ " + 1")
  -- emit $ "set(" ++ useNam ++ " + 0, " ++ cT ++ ");"
  -- emit $ "set(" ++ useNam ++ " + 1, " ++ bT ++ ");"
  -- let use = "term_new(USE, 0, " ++ useNam ++ ")"
  -- foldM (\term (key, val) -> do
    -- appNam <- fresh "app"
    -- emit $ "Loc " ++ appNam ++ " = alloc_node(2);"
    -- valT <- compileFullCore book fid val (appNam ++ " + 1")
    -- emit $ "set(" ++ appNam ++ " + 0, " ++ term ++ ");"
    -- emit $ "set(" ++ appNam ++ " + 1, " ++ valT ++ ");"
    -- return $ "term_new(APP, 0, " ++ appNam ++ ")") use ms

-- compileFullCore _ _ U32 _ = do
  -- return $ "term_new(U32, 0, 0)"

-- compileFullCore _ _ (W32 val) _ = do
  -- return $ "term_new(W32, 0, " ++ show (fromIntegral val) ++ ")"

-- compileFullCore book fid (Swi c ms z s) host = do
  -- swiNam <- fresh "swi"
  -- emit $ "Loc " ++ swiNam ++ " = alloc_node(3);"
  -- cT <- compileFullCore book fid c (swiNam ++ " + 0")
  -- zT <- compileFullCore book fid z (swiNam ++ " + 1")
  -- sT <- compileFullCore book fid s (swiNam ++ " + 2")
  -- emit $ "set(" ++ swiNam ++ " + 0, " ++ cT ++ ");"
  -- emit $ "set(" ++ swiNam ++ " + 1, " ++ zT ++ ");"
  -- emit $ "set(" ++ swiNam ++ " + 2, " ++ sT ++ ");"
  -- let swi = "term_new(SWI, 0, " ++ swiNam ++ ")"
  -- foldM (\term (key, val) -> do
    -- appNam <- fresh "app"
    -- emit $ "Loc " ++ appNam ++ " = alloc_node(2);"
    -- valT <- compileFullCore book fid val (appNam ++ " + 1")
    -- emit $ "set(" ++ appNam ++ " + 0, " ++ term ++ ");"
    -- emit $ "set(" ++ appNam ++ " + 1, " ++ valT ++ ");"
    -- return $ "term_new(APP, 0, " ++ appNam ++ ")") swi ms

-- compileFullCore book fid (Op2 opr a b) host = do
  -- opxNam <- fresh "opx"
  -- emit $ "Loc " ++ opxNam ++ " = alloc_node(2);"
  -- aT <- compileFullCore book fid a (opxNam ++ " + 0")
  -- bT <- compileFullCore book fid b (opxNam ++ " + 1")
  -- emit $ "set(" ++ opxNam ++ " + 0, " ++ aT ++ ");"
  -- emit $ "set(" ++ opxNam ++ " + 1, " ++ bT ++ ");"
  -- return $ "term_new(OPX, " ++ show (fromEnum opr) ++ ", " ++ opxNam ++ ")"

-- compileFullCore book fid (Sig _A _B) host = do
  -- sigNam <- fresh "sig"
  -- emit $ "Loc " ++ sigNam ++ " = alloc_node(2);"
  -- aT <- compileFullCore book fid _A (sigNam ++ " + 0")
  -- bT <- compileFullCore book fid _B (sigNam ++ " + 1")
  -- emit $ "set(" ++ sigNam ++ " + 0, " ++ aT ++ ");"
  -- emit $ "set(" ++ sigNam ++ " + 1, " ++ bT ++ ");"
  -- return $ "term_new(SIG, 0, " ++ sigNam ++ ")"

-- compileFullCore book fid (Tup a b) host = do
  -- tupNam <- fresh "tup"
  -- emit $ "Loc " ++ tupNam ++ " = alloc_node(2);"
  -- aT <- compileFullCore book fid a (tupNam ++ " + 0")
  -- bT <- compileFullCore book fid b (tupNam ++ " + 1")
  -- emit $ "set(" ++ tupNam ++ " + 0, " ++ aT ++ ");"
  -- emit $ "set(" ++ tupNam ++ " + 1, " ++ bT ++ ");"
  -- return $ "term_new(TUP, 0, " ++ tupNam ++ ")"

-- compileFullCore book fid (Get c ms b) host = do
  -- getNam <- fresh "get"
  -- emit $ "Loc " ++ getNam ++ " = alloc_node(2);"
  -- cT <- compileFullCore book fid c (getNam ++ " + 0")
  -- bT <- compileFullCore book fid b (getNam ++ " + 1")
  -- emit $ "set(" ++ getNam ++ " + 0, " ++ cT ++ ");"
  -- emit $ "set(" ++ getNam ++ " + 1, " ++ bT ++ ");"
  -- let get = "term_new(GET, 0, " ++ getNam ++ ")"
  -- foldM (\term (key, val) -> do
    -- appNam <- fresh "app"
    -- emit $ "Loc " ++ appNam ++ " = alloc_node(2);"
    -- valT <- compileFullCore book fid val (appNam ++ " + 1")
    -- emit $ "set(" ++ appNam ++ " + 0, " ++ term ++ ");"
    -- emit $ "set(" ++ appNam ++ " + 1, " ++ valT ++ ");"
    -- return $ "term_new(APP, 0, " ++ appNam ++ ")") get ms

-- compileFullCore book fid (All _A _B) host = do
  -- allNam <- fresh "all"
  -- emit $ "Loc " ++ allNam ++ " = alloc_node(2);"
  -- aT <- compileFullCore book fid _A (allNam ++ " + 0")
  -- bT <- compileFullCore book fid _B (allNam ++ " + 1")
  -- emit $ "set(" ++ allNam ++ " + 0, " ++ aT ++ ");"
  -- emit $ "set(" ++ allNam ++ " + 1, " ++ bT ++ ");"
  -- return $ "term_new(ALL, 0, " ++ allNam ++ ")"

-- compileFullCore book fid (Lam var bod) host = do
  -- lamNam <- fresh "lam"
  -- emit $ "Loc " ++ lamNam ++ " = alloc_node(1);"
  -- bind var $ "term_new(VAR, 0, " ++ lamNam ++ " + 0)"
  -- bodT <- compileFullCore book fid bod (lamNam ++ " + 0")
  -- emit $ "set(" ++ lamNam ++ " + 0, " ++ bodT ++ ");"
  -- return $ "term_new(LAM, 0, " ++ lamNam ++ ")"

-- compileFullCore book fid (App fun arg) host = do
  -- appNam <- fresh "app"
  -- emit $ "Loc " ++ appNam ++ " = alloc_node(2);"
  -- funT <- compileFullCore book fid fun (appNam ++ " + 0")
  -- argT <- compileFullCore book fid arg (appNam ++ " + 1")
  -- emit $ "set(" ++ appNam ++ " + 0, " ++ funT ++ ");"
  -- emit $ "set(" ++ appNam ++ " + 1, " ++ argT ++ ");"
  -- return $ "term_new(APP, 0, " ++ appNam ++ ")"

-- -- Fast Compiler
-- -- -------------

-- -- Compiles a function using Fast-Mode
-- compileFast :: Book -> Word16 -> Core -> Bool -> [(Bool,String)] -> Compile ()
-- compileFast book fid core copy args = do
  -- emit $ "Term " ++ mget (fidToNam book) fid ++ "_f(Term ref) {"
  -- tabInc
  -- emit "u64 itrs = 0;"
  -- args <- forM (zip [0..] args) $ \ (i, (strict, arg)) -> do
    -- argNam <- fresh "arg"
    -- if strict then do
      -- emit $ "Term " ++ argNam ++ " = reduce_at(term_loc(ref) + " ++ show i ++ ");"
    -- else do
      -- emit $ "Term " ++ argNam ++ " = got(term_loc(ref) + " ++ show i ++ ");"
    -- if copy && strict then do
      -- case MS.lookup fid (fidToLab book) of
        -- Just labs -> do
          -- emit $ "if (term_tag(" ++ argNam ++ ") == ERA) {"
          -- emit $ "  itrs += 1;"
          -- emit $ "  *HVM.itrs += itrs;"
          -- emit $ "  return term_new(ERA, 0, 0);"
          -- emit $ "}"
          -- emit $ "if (term_tag(" ++ argNam ++ ") == SUP) {"
          -- tabInc
          -- emit $ "u64 lab = term_lab(" ++ argNam ++ ");"
          -- emit $ "if (1"
          -- forM_ (MS.keys labs) $ \lab -> do
            -- emit $ "    && lab != " ++ show lab
          -- emit $ ") {"
          -- tabInc
          -- emit $ "Term term = reduce_ref_sup(ref, " ++ show i ++ ");"
          -- emit $ "return term;"
          -- tabDec
          -- emit $ "}"
          -- tabDec
          -- emit $ "}"
        -- Nothing -> return ()
    -- else
      -- return ()
    -- bind arg argNam
    -- return argNam
  -- reuse (length (snd (fst (mget (fidToFun book) fid)))) "term_loc(ref)"
  -- compileFastArgs book fid core args
  -- tabDec
  -- emit "}"

-- -- Compiles a fast function's argument list
-- compileFastArgs :: Book -> Word16 -> Core -> [String] -> Compile ()
-- compileFastArgs book fid body ctx = do
  -- emit $ "_Bool fst_iter = 1;"
  -- emit $ "while (1) {"
  -- tabInc
  -- compileFastBody book fid body ctx False 0
  -- tabDec
  -- emit $ "}"

-- -- Compiles a fast function body (pattern-matching)
-- compileFastBody :: Book -> Word16 -> Core -> [String] -> Bool -> Int -> Compile ()
-- compileFastBody book fid term@(Swi c ms z s) ctx stop itr = do
  -- cT     <- compileFastCore book fid c
  -- cNam   <- fresh "cond"
  -- emit $ "Term " ++ cNam ++ " = (" ++ cT ++ ");"
  
  -- emit $ "if (term_tag(" ++ cNam ++ ") == W32) {"
  -- tabInc
  -- emit $ "u32 num = term_loc(" ++ cNam ++ ");"
  -- emit $ "itrs += 1;"
  -- emit $ "if (num == 0) {"
  -- tabInc
  -- forM_ ms $ \ (key,val) -> do
    -- valT <- compileFastCore book fid val
    -- bind key valT
  -- compileFastBody book fid z ctx stop (itr + 1 + length ms)
  -- tabDec
  -- emit $ "} else {"
  -- tabInc
  -- preNam <- fresh "pre"
  -- predVar <- fresh "pred"
  -- emit $ "Term " ++ preNam ++ " = term_new(W32, 0, num - 1);"
  -- emit $ "Term " ++ predVar ++ " = " ++ preNam ++ ";"
  -- forM_ ms $ \ (key,val) -> do
    -- valT <- compileFastCore book fid val
    -- bind key valT
  -- compileFastBody book fid (App s (Var predVar)) ctx stop (itr + 1 + length ms)
  -- tabDec
  -- emit $ "}"
  -- tabDec
  -- emit $ "} else if (term_tag(" ++ cNam ++ ") == ERA) {"
  -- tabInc
  -- compileFastBody book fid Era ctx stop (itr + 1)
  -- tabDec
  -- emit $ "} else {"
  -- tabInc
  -- val <- compileFastCore book fid term
  -- emit $ "itrs += " ++ show itr ++ ";"
  -- compileFastSave book fid term ctx itr
  -- emit $ "return " ++ val ++ ";"
  -- tabDec
  -- emit $ "}"

-- compileFastBody book fid term@(Use c ms b) ctx stop itr = do
  -- cT   <- compileFastCore book fid c
  -- cNam <- fresh "cond"
  -- emit $ "Term " ++ cNam ++ " = (" ++ cT ++ ");"
  
  -- emit $ "if (term_tag(" ++ cNam ++ ") == NIL) {"
  -- tabInc
  -- emit $ "itrs += 1;"
  -- forM_ ms $ \ (key,val) -> do
    -- valT <- compileFastCore book fid val
    -- bind key valT
  -- compileFastBody book fid b ctx stop (itr + 1 + length ms)
  -- tabDec
  -- emit $ "} else if (term_tag(" ++ cNam ++ ") == ERA) {"
  -- tabInc
  -- compileFastBody book fid Era ctx stop (itr + 1)
  -- tabDec
  -- emit $ "} else {"
  -- tabInc
  -- val <- compileFastCore book fid term
  -- emit $ "itrs += " ++ show itr ++ ";"
  -- compileFastSave book fid term ctx itr
  -- emit $ "return " ++ val ++ ";"
  -- tabDec
  -- emit $ "}"

-- compileFastBody book fid term@(Get c ms b) ctx stop itr = do
  -- cT   <- compileFastCore book fid c
  -- cNam <- fresh "cond"
  -- emit $ "Term " ++ cNam ++ " = (" ++ cT ++ ");"
  
  -- emit $ "if (term_tag(" ++ cNam ++ ") == TUP) {"
  -- tabInc
  -- emit $ "itrs += 1;"
  -- f0Var <- fresh "fst"
  -- f1Var <- fresh "snd"
  -- emit $ "Term " ++ f0Var ++ " = got(term_loc(" ++ cNam ++ ") + 0);"
  -- emit $ "Term " ++ f1Var ++ " = got(term_loc(" ++ cNam ++ ") + 1);"
  -- reuse 2 ("term_loc(" ++ cNam ++ ")")
  -- bT <- compileFastCore book fid b
  -- forM_ ms $ \ (key,val) -> do
    -- valT <- compileFastCore book fid val
    -- bind key valT
  -- appANam <- fresh "appa"
  -- appALoc <- fresh "appaloc"
  -- compileFastAlloc appALoc 2
  -- emit $ "set(" ++ appALoc ++ " + 0, " ++ bT ++ ");"
  -- emit $ "set(" ++ appALoc ++ " + 1, " ++ f0Var ++ ");"
  -- emit $ "Term " ++ appANam ++ " = term_new(APP, 0, " ++ appALoc ++ ");"
  -- appBNam <- fresh "appb"
  -- appBLoc <- fresh "appbloc"
  -- compileFastAlloc appBLoc 2
  -- emit $ "set(" ++ appBLoc ++ " + 0, " ++ appANam ++ ");"
  -- emit $ "set(" ++ appBLoc ++ " + 1, " ++ f1Var ++ ");"
  -- emit $ "Term " ++ appBNam ++ " = term_new(APP, 0, " ++ appBLoc ++ ");"
  -- compileFastBody book fid (Var appBNam) ctx stop (itr + 3 + length ms)
  -- tabDec
  -- emit $ "} else if (term_tag(" ++ cNam ++ ") == ERA) {"
  -- tabInc
  -- compileFastBody book fid Era ctx stop (itr + 1)
  -- tabDec
  -- emit $ "} else {"
  -- tabInc
  -- val <- compileFastCore book fid term
  -- emit $ "itrs += " ++ show itr ++ ";"
  -- compileFastSave book fid term ctx itr
  -- emit $ "return " ++ val ++ ";"
  -- tabDec
  -- emit $ "}"

-- compileFastBody book fid term@(Efq c ms) ctx stop itr = do
  -- val <- compileFastCore book fid term
  -- emit $ "itrs += " ++ show itr ++ ";"
  -- compileFastSave book fid term ctx itr
  -- emit $ "return " ++ val ++ ";"

-- compileFastBody book fid term@(Dup lab dp0 dp1 val bod) ctx stop itr = do
  -- valT <- compileFastCore book fid val
  -- valNam <- fresh "val"
  -- dp0Nam <- fresh "dpA"
  -- dp1Nam <- fresh "dpB"
  -- emit $ "Term " ++ valNam ++ " = (" ++ valT ++ ");"
  -- emit $ "Term " ++ dp0Nam ++ ";"
  -- emit $ "Term " ++ dp1Nam ++ ";"
  -- emit $ "if (term_is_atom(" ++ valNam ++ ")) {"
  -- tabInc
  -- emit $ "itrs += 1;"
  -- emit $ dp0Nam ++ " = " ++ valNam ++ ";"
  -- emit $ dp1Nam ++ " = " ++ valNam ++ ";"
  -- tabDec
  -- emit $ "} else {"
  -- tabInc
  -- dupNam <- fresh "dup"
  -- compileFastAlloc dupNam 1
  -- emit $ "set(" ++ dupNam ++ " + 0, " ++ valNam ++ ");"
  -- emit $ dp0Nam ++ " = term_new(DP0, " ++ show lab ++ ", " ++ dupNam ++ " + 0);"
  -- emit $ dp1Nam ++ " = term_new(DP1, " ++ show lab ++ ", " ++ dupNam ++ " + 0);"
  -- tabDec
  -- emit $ "}"
  -- bind dp0 dp0Nam
  -- bind dp1 dp1Nam
  -- compileFastBody book fid bod ctx stop itr

-- compileFastBody book fid term@(Let mode var val bod) ctx stop itr = do
  -- valT <- compileFastCore book fid val
  -- case mode of
    -- LAZY -> do
      -- bind var valT
    -- STRI -> do
      -- case val of
        -- t@(Ref _ rFid _) -> do
          -- checkRefAri book t
          -- valNam <- fresh "val"
          -- emit $ "Term " ++ valNam ++ " = reduce(" ++ mget (fidToNam book) rFid ++ "_f(" ++ valT ++ "));"
          -- bind var valNam
        -- _ -> do
          -- valNam <- fresh "val" 
          -- emit $ "Term " ++ valNam ++ " = reduce(" ++ valT ++ ");"
          -- bind var valNam
  -- compileFastBody book fid bod ctx stop itr

-- compileFastBody book fid term@(Ref fNam fFid fArg) ctx stop itr | fFid == fid = do
  -- checkRefAri book term
  -- forM_ (zip fArg ctx) $ \ (arg, ctxVar) -> do
    -- argT <- compileFastCore book fid arg
    -- emit $ "" ++ ctxVar ++ " = " ++ argT ++ ";"
  -- emit $ "itrs += " ++ show (itr + 1) ++ ";"
  -- emit $ "fst_iter = false;"
  -- emit $ "continue;"

-- compileFastBody book fid term ctx stop itr = do
  -- body <- compileFastCore book fid term
  -- emit $ "itrs += " ++ show itr ++ ";"
  -- compileFastSave book fid term ctx itr
  -- emit $ "return " ++ body ++ ";"

-- -- Completes a fast mode call
-- compileFastSave :: Book -> Word16 -> Core -> [String] -> Int -> Compile ()
-- compileFastSave book fid term ctx itr = do
  -- emit $ "*HVM.itrs += itrs;"

-- -- Helper function to allocate nodes with reuse
-- compileFastAlloc :: String -> Int -> Compile ()
-- compileFastAlloc name 0 = do
  -- emit $ "Loc " ++ name ++ " = 0;"
-- compileFastAlloc name arity = do
  -- reuse <- gets reus
  -- -- Find the smallest reuse location that's big enough
  -- -- Very large reuses are usually functions with a lot of state that doesn't need to be moved
  -- -- Don't fragment those to avoid moving those values (a separate optimization)
  -- let bigEnough = [(k,locs) | (k,locs) <- MS.toList reuse, k >= arity, k <= arity + 5, not (null locs)]
  -- case bigEnough of
    -- [] -> do
      -- emit $ "Loc " ++ name ++ " = alloc_node(" ++ show arity ++ ");"
    -- ((k,loc:locs):_) -> do
      -- emit $ "Loc " ++ name ++ ";"
      -- -- Too hard to determine statically if reusing is ok in tail-call-optimization
      -- emit $ "if (fst_iter) {"
      -- emit $ "  " ++ name ++ " = " ++ loc ++ ";"
      -- emit $ "} else {"
      -- emit $ "  " ++ name ++ " = alloc_node(" ++ show arity ++ ");"
      -- emit $ "}"
      -- -- Remove the used location
      -- let reuse' = MS.insert k locs reuse
      -- -- If we used a location bigger than needed, add the remainder back
      -- let reuse'' = if k > arity 
                    -- then MS.insertWith (++) (k - arity) [loc ++ " + " ++ show arity] reuse'
                    -- else reuse'
      -- modify $ \st -> st { reus = reuse'' }

-- -- Compiles a core term in fast mode
-- compileFastCore :: Book -> Word16 -> Core -> Compile String

-- compileFastCore _ _ (Var k) = do
  -- compileFastVar k

-- compileFastCore book fid t@(Ref rNam rFid rArg) = do
  -- checkRefAri book t

  -- -- Inline Dynamic SUP
  -- if rNam == "SUP" then do
    -- let [lab, tm0, tm1] = rArg
    -- supNam <- fresh "sup"
    -- labNam <- fresh "lab"
    -- labT <- compileFastCore book fid lab
    -- emit $ "Term " ++ labNam ++ " = reduce(" ++ labT ++ ");"
    -- emit $ "if (term_tag(" ++ labNam ++ ") != W32) {"
    -- emit $ "  printf(\"ERROR:non-numeric-sup-label\\n\");"
    -- emit $ "}"
    -- emit $ "itrs += 1;"
    -- compileFastAlloc supNam 2
    -- tm0T <- compileFastCore book fid tm0
    -- tm1T <- compileFastCore book fid tm1
    -- emit $ "set(" ++ supNam ++ " + 0, " ++ tm0T ++ ");"
    -- emit $ "set(" ++ supNam ++ " + 1, " ++ tm1T ++ ");"
    -- return $ "term_new(SUP, term_loc(" ++ labNam ++ "), " ++ supNam ++ ")"

  -- -- Inline Dynamic DUP
  -- else if rNam == "DUP" && (case rArg of [_, _, Lam _ (Lam _ _)] -> True ; _ -> False) then do
    -- let [lab, val, Lam x (Lam y body)] = rArg
    -- dupNam <- fresh "dup"
    -- labNam <- fresh "lab"
    -- labT <- compileFastCore book fid lab
    -- emit $ "Term " ++ labNam ++ " = reduce(" ++ labT ++ ");"
    -- emit $ "if (term_tag(" ++ labNam ++ ") != W32) {"
    -- emit $ "  printf(\"ERROR:non-numeric-sup-label\\n\");"
    -- emit $ "}"
    -- emit $ "itrs += 3;"
    -- compileFastAlloc dupNam 1
    -- valT <- compileFastCore book fid val
    -- emit $ "set(" ++ dupNam ++ " + 0, " ++ valT ++ ");"
    -- bind x $ "term_new(DP0, term_loc(" ++ labNam ++ "), " ++ dupNam ++ " + 0)"
    -- bind y $ "term_new(DP1, term_loc(" ++ labNam ++ "), " ++ dupNam ++ " + 0)"
    -- compileFastCore book fid body

  -- -- Create REF node
  -- else do
    -- refNam <- fresh "ref"
    -- let arity = length rArg
    -- compileFastAlloc refNam arity
    -- argsT <- mapM (\ (i,arg) -> compileFastCore book fid arg) (zip [0..] rArg)
    -- sequence_ [emit $ "set(" ++ refNam ++ " + " ++ show i ++ ", " ++ argT ++ ");" | (i,argT) <- zip [0..] argsT]
    -- return $ "term_new(REF, " ++ show rFid ++ ", " ++ refNam ++ ")"

-- compileFastCore book fid (Let mode k v f) = do
  -- valT <- compileFastCore book fid v
  -- case mode of
    -- LAZY -> do
      -- emit $ "itrs += 1;"
      -- bind k valT
    -- STRI -> do
      -- valNam <- fresh "val"
      -- emit $ "itrs += 1;"
      -- emit $ "Term " ++ valNam ++ " = reduce(" ++ valT ++ ");"
      -- bind k valNam
  -- compileFastCore book fid f

-- compileFastCore _ _ Era = 
  -- return $ "term_new(ERA, 0, 0)"

-- compileFastCore book fid (Sup l a b) = do
  -- supNam <- fresh "sup"
  -- compileFastAlloc supNam 2
  -- aT <- compileFastCore book fid a
  -- bT <- compileFastCore book fid b
  -- emit $ "set(" ++ supNam ++ " + 0, " ++ aT ++ ");"
  -- emit $ "set(" ++ supNam ++ " + 1, " ++ bT ++ ");"
  -- return $ "term_new(SUP, " ++ show l ++ ", " ++ supNam ++ ")"

-- compileFastCore book fid (Dup l x y v f) = do
  -- dupNam <- fresh "dup"
  -- dp0Nam <- fresh "dpA"
  -- dp1Nam <- fresh "dpB"
  -- valNam <- fresh "val"
  -- valT   <- compileFastCore book fid v
  -- emit $ "Term " ++ valNam ++ " = (" ++ valT ++ ");"
  -- emit $ "Term " ++ dp0Nam ++ ";"
  -- emit $ "Term " ++ dp1Nam ++ ";"
  -- emit $ "if (term_is_atom(" ++ valNam ++ ")) {"
  -- tabInc
  -- emit $ "itrs += 1;"
  -- emit $ dp0Nam ++ " = " ++ valNam ++ ";"
  -- emit $ dp1Nam ++ " = " ++ valNam ++ ";"
  -- tabDec
  -- emit $ "} else {"
  -- tabInc
  -- compileFastAlloc dupNam 1
  -- emit $ "set(" ++ dupNam ++ " + 0, " ++ valNam ++ ");"
  -- emit $ dp0Nam ++ " = term_new(DP0, " ++ show l ++ ", " ++ dupNam ++ " + 0);"
  -- emit $ dp1Nam ++ " = term_new(DP1, " ++ show l ++ ", " ++ dupNam ++ " + 0);"
  -- tabDec
  -- emit $ "}"
  -- bind x dp0Nam
  -- bind y dp1Nam
  -- compileFastCore book fid f

-- compileFastCore _ _ Set = 
  -- return $ "term_new(SET, 0, 0)"

-- compileFastCore _ _ Emp = 
  -- return $ "term_new(EMP, 0, 0)"

-- compileFastCore book fid (Efq c ms) = do
  -- efqNam <- fresh "efq"
  -- compileFastAlloc efqNam 1
  -- cT <- compileFastCore book fid c
  -- emit $ "set(" ++ efqNam ++ " + 0, " ++ cT ++ ");"
  -- let efq = "term_new(EFQ, 0, " ++ efqNam ++ ")"
  -- foldM (\acc (_, val) -> do
    -- appNam <- fresh "app"
    -- compileFastAlloc appNam 2
    -- emit $ "set(" ++ appNam ++ " + 0, " ++ acc ++ ");"
    -- valT <- compileFastCore book fid val
    -- emit $ "set(" ++ appNam ++ " + 1, " ++ valT ++ ");"
    -- return $ "term_new(APP, 0, " ++ appNam ++ ")") efq ms

-- compileFastCore _ _ Uni = 
  -- return $ "term_new(UNI, 0, 0)"

-- compileFastCore _ _ Nil = 
  -- return $ "term_new(NIL, 0, 0)"

-- compileFastCore book fid (Use c ms b) = do
  -- useNam <- fresh "use"
  -- compileFastAlloc useNam 2
  -- cT <- compileFastCore book fid c
  -- bT <- compileFastCore book fid b
  -- emit $ "set(" ++ useNam ++ " + 0, " ++ cT ++ ");"
  -- emit $ "set(" ++ useNam ++ " + 1, " ++ bT ++ ");"
  -- let use = "term_new(USE, 0, " ++ useNam ++ ")"
  -- foldM (\acc (_, val) -> do
    -- appNam <- fresh "app"
    -- compileFastAlloc appNam 2
    -- emit $ "set(" ++ appNam ++ " + 0, " ++ acc ++ ");"
    -- valT <- compileFastCore book fid val
    -- emit $ "set(" ++ appNam ++ " + 1, " ++ valT ++ ");"
    -- return $ "term_new(APP, 0, " ++ appNam ++ ")") use ms

-- compileFastCore _ _ U32 = 
  -- return $ "term_new(U32, 0, 0)"

-- compileFastCore _ _ (W32 v) =
  -- return $ "term_new(W32, 0, " ++ show (fromIntegral v) ++ ")"

-- compileFastCore book fid (Swi c ms z s) = do
  -- swiNam <- fresh "swi"
  -- compileFastAlloc swiNam 3
  -- cT <- compileFastCore book fid c
  -- zT <- compileFastCore book fid z
  -- sT <- compileFastCore book fid s
  -- emit $ "set(" ++ swiNam ++ " + 0, " ++ cT ++ ");"
  -- emit $ "set(" ++ swiNam ++ " + 1, " ++ zT ++ ");"
  -- emit $ "set(" ++ swiNam ++ " + 2, " ++ sT ++ ");"
  -- let swi = "term_new(SWI, 0, " ++ swiNam ++ ")"
  -- foldM (\acc (_, val) -> do
    -- appNam <- fresh "app"
    -- compileFastAlloc appNam 2
    -- emit $ "set(" ++ appNam ++ " + 0, " ++ acc ++ ");"
    -- valT <- compileFastCore book fid val
    -- emit $ "set(" ++ appNam ++ " + 1, " ++ valT ++ ");"
    -- return $ "term_new(APP, 0, " ++ appNam ++ ")") swi ms

-- compileFastCore book fid (Op2 o a b) = do
  -- opxNam <- fresh "opx"
  -- retNam <- fresh "ret"
  -- nu0Nam <- fresh "nu0"
  -- nu1Nam <- fresh "nu1"
  -- nu0T <- compileFastCore book fid a
  -- nu1T <- compileFastCore book fid b
  -- emit $ "Term " ++ nu0Nam ++ " = (" ++ nu0T ++ ");"
  -- emit $ "Term " ++ nu1Nam ++ " = (" ++ nu1T ++ ");"
  -- emit $ "Term " ++ retNam ++ ";"
  -- emit $ "if (term_tag(" ++ nu0Nam ++ ") == W32 && term_tag(" ++ nu1Nam ++ ") == W32) {"
  -- emit $ "  itrs += 2;"
  -- let oprStr = case o of
        -- OP_ADD -> "+"
        -- OP_SUB -> "-"
        -- OP_MUL -> "*"
        -- OP_DIV -> "/"
        -- OP_MOD -> "%"
        -- OP_EQ  -> "=="
        -- OP_NE  -> "!="
        -- OP_LT  -> "<"
        -- OP_GT  -> ">"
        -- OP_LTE -> "<="
        -- OP_GTE -> ">="
        -- OP_AND -> "&"
        -- OP_OR  -> "|"
        -- OP_XOR -> "^"
        -- OP_LSH -> "<<"
        -- OP_RSH -> ">>"
  -- emit $ "  " ++ retNam ++ " = term_new(W32, 0, term_loc(" ++ nu0Nam ++ ") " ++ oprStr ++ " term_loc(" ++ nu1Nam ++ "));"
  -- emit $ "} else {"
  -- tabInc
  -- compileFastAlloc opxNam 2
  -- emit $ "set(" ++ opxNam ++ " + 0, " ++ nu0Nam ++ ");"
  -- emit $ "set(" ++ opxNam ++ " + 1, " ++ nu1Nam ++ ");"
  -- emit $ retNam ++ " = term_new(OPX, " ++ show (fromEnum o) ++ ", " ++ opxNam ++ ");"
  -- tabDec
  -- emit $ "}"
  -- return $ retNam

-- compileFastCore book fid (Sig _A _B) = do
  -- sigNam <- fresh "sig"
  -- compileFastAlloc sigNam 2
  -- aT <- compileFastCore book fid _A
  -- bT <- compileFastCore book fid _B
  -- emit $ "set(" ++ sigNam ++ " + 0, " ++ aT ++ ");"
  -- emit $ "set(" ++ sigNam ++ " + 1, " ++ bT ++ ");"
  -- return $ "term_new(SIG, 0, " ++ sigNam ++ ")"

-- compileFastCore book fid (Tup a b) = do
  -- tupNam <- fresh "tup"
  -- compileFastAlloc tupNam 2
  -- aT <- compileFastCore book fid a
  -- bT <- compileFastCore book fid b
  -- emit $ "set(" ++ tupNam ++ " + 0, " ++ aT ++ ");"
  -- emit $ "set(" ++ tupNam ++ " + 1, " ++ bT ++ ");"
  -- return $ "term_new(TUP, 0, " ++ tupNam ++ ")"

-- compileFastCore book fid (Get c ms b) = do
  -- getNam <- fresh "get"
  -- compileFastAlloc getNam 2
  -- cT <- compileFastCore book fid c
  -- bT <- compileFastCore book fid b
  -- emit $ "set(" ++ getNam ++ " + 0, " ++ cT ++ ");"
  -- emit $ "set(" ++ getNam ++ " + 1, " ++ bT ++ ");"
  -- let get = "term_new(GET, 0, " ++ getNam ++ ")"
  -- foldM (\acc (_, val) -> do
    -- appNam <- fresh "app"
    -- compileFastAlloc appNam 2
    -- emit $ "set(" ++ appNam ++ " + 0, " ++ acc ++ ");"
    -- valT <- compileFastCore book fid val
    -- emit $ "set(" ++ appNam ++ " + 1, " ++ valT ++ ");"
    -- return $ "term_new(APP, 0, " ++ appNam ++ ")") get ms

-- compileFastCore book fid (All _A _B) = do
  -- allNam <- fresh "all"
  -- compileFastAlloc allNam 2
  -- aT <- compileFastCore book fid _A
  -- bT <- compileFastCore book fid _B
  -- emit $ "set(" ++ allNam ++ " + 0, " ++ aT ++ ");"
  -- emit $ "set(" ++ allNam ++ " + 1, " ++ bT ++ ");"
  -- return $ "term_new(ALL, 0, " ++ allNam ++ ")"

-- compileFastCore book fid (Lam x f) = do
  -- lamNam <- fresh "lam"
  -- compileFastAlloc lamNam 1
  -- bind x $ "term_new(VAR, 0, " ++ lamNam ++ " + 0)"
  -- bodT <- compileFastCore book fid f
  -- emit $ "set(" ++ lamNam ++ " + 0, " ++ bodT ++ ");"
  -- return $ "term_new(LAM, 0, " ++ lamNam ++ ")"

-- compileFastCore book fid (App f x) = do
  -- appNam <- fresh "app"
  -- compileFastAlloc appNam 2
  -- fT <- compileFastCore book fid f
  -- xT <- compileFastCore book fid x
  -- emit $ "set(" ++ appNam ++ " + 0, " ++ fT ++ ");"
  -- emit $ "set(" ++ appNam ++ " + 1, " ++ xT ++ ");"
  -- return $ "term_new(APP, 0, " ++ appNam ++ ")"

-- -- Compiles a variable in fast mode
-- compileFastVar :: String -> Compile String
-- compileFastVar var = do
  -- bins <- gets bins
  -- case MS.lookup var bins of
    -- Just entry -> do
      -- return entry
    -- Nothing -> do
      -- return $ "<ERR>"

-- checkRefAri :: Book -> Core -> Compile ()
-- checkRefAri book core = do
  -- case core of
    -- Ref _ lab arg -> do
      -- let fid = fromIntegral lab
      -- let ari = funArity book fid
      -- let len = length arg
      -- when (ari /= fromIntegral len) $ do
        -- error $ "Arity mismatch on term: " ++ show core ++ ". Expected " ++ show ari ++ ", got " ++ show len ++ "."
    -- _ -> return ()

-- NOTE: the Compile.hs file has been updated to use the new Core Term type.
-- the goal of this update was to assume the original Compile.hs file was correct,
-- understand its semantics (full compiler, fast compiler, optimizations, etc.),
-- and rewrite it, from scratch, "as if" if has been written with the new Core
-- type in mind. that said, in this process, errors, bugs, typos and other issues
-- might have been introduced. it is extremely important that the new Compile.hs
-- behaves correctly, just like the old Compile.hs did, and that they match in
-- behavior (of course, other than the fact they use a different Core term).
-- your goal is to make sure that the new Runtime.c is equivalent to the old one.
-- review the file above fully and thoroughly.
-- then, report below any error or deviation you find.
-- write a complete report below, in English:

-- Refined Report on Compile.hs Issues (Focusing on Preserving Original Semantics)
-- Based on the analysis and the strict requirement to maintain the original compiler's behavior, here are the identified discrepancies that likely cause deviations or require correction to match the original semantics:
-- I. Critical Issues Requiring Correction for Basic Functionality/Equivalence:
-- compileFastBody - Swi Successor Case (else branch): Missing bind for predVar
-- Where: Inside the Swi case, within the else block handling non-zero numbers.
-- Problem: A C variable predVar is created to hold the predecessor term (num - 1). The code then recursively calls compileFastBody with (App s (Var predVar)). However, the Haskell name "predVar" is never associated with the C variable predVar in the compiler's bins map using bind.
-- Consequence: When compileFastBody tries to compile Var predVar (likely via compileFastCore -> compileFastVar), the lookup for "predVar" fails, returning "<ERR>". This breaks the fast-path compilation for the successor case.
-- Why it's critical for preserving semantics: This compilation pattern (generating an intermediate term, giving it a name, and recursing using that name) requires the name to be bound. The original compiler's equivalent logic (likely in Mat for numbers) must have performed a similar binding for this pattern to function correctly. Failure to bind breaks the established compilation pattern.
-- Required Change: Immediately after emit $ "Term " ++ predVar ++ " = " ++ preNam ++ ";", add bind predVar predVar.
-- compileFastBody - Get Tup Case: Missing bind for appBNam
-- Where: Inside the Get case, within the if (term_tag(...) == TUP) block.
-- Problem: The code generates C variables appANam and appBNam representing the nested application result ((b f0Var) f1Var). It then recursively calls compileFastBody with (Var appBNam). Like point 1, the Haskell name "appBNam" is not bound to the C variable appBNam in the bins map.
-- Consequence: The lookup for "appBNam" fails during the recursive step, returning "<ERR>", breaking the fast-path compilation for Get.
-- Why it's critical for preserving semantics: Get replaces functionality previously handled by Mat. The compilation strategy chosen (create nested Apps, recurse on the result Var) mirrors patterns likely used in the old Mat compiler. This pattern requires the result variable to be bound. Omitting the bind breaks this strategy.
-- Required Change: Immediately after emit $ "Term " ++ appBNam ++ " = term_new(APP, 0, " ++ appBLoc ++ ");", add bind appBNam appBNam.
-- Variable Encoding (VAR tag): Potential Mismatch with Runtime Expectation
-- Where: compileFullCore (for Lam) and compileFastCore (for Lam).
-- Problem: The compiler generates VAR terms using term_new(VAR, 0, loc). The previous report suggested the old compiler used the lab field (the 0 here) to store binder information (e.g., depth).
-- Consequence: If the C runtime's substitution logic relies on information previously stored in term_lab(VAR) (like depth for de Bruijn indices), then providing 0 will cause the runtime to misinterpret variable bindings, leading to incorrect reduction behavior.
-- Why it's critical for preserving semantics: The compiler must produce terms that the C runtime interprets correctly according to the established (old) conventions. If the convention was to use lab for binder info, the new compiler must replicate that encoding. Failing to do so breaks the compiler-runtime contract and changes program behavior.
-- Required Change: Verify how the original compiler/runtime encoded binder information for VAR nodes. Update the term_new(VAR, lab, loc) calls in compileFullCore and compileFastCore for Lam to use the correct value for lab based on the original system's convention (e.g., calculate and insert depth if that was the convention).
-- II. Issues Affecting Performance/Behavioral Consistency (Likely Deviations from Original):
-- Missing Fast-Paths for Eliminator-Superposition (SUP) Interactions
-- Where: compileFastBody cases for Use, Get, Swi.
-- Problem: The new compiler lacks specific checks and handling for when the scrutinee (c) of these eliminators is a SUP node. For example, there's no else if (term_tag(cNam) == SUP) block.
-- Consequence: When a Use/Get/Swi encounters a SUP node in the fast path, it falls back to the generic else block, recompiling the original term (Use (Sup ...) etc.) and pushing the interaction entirely to the C runtime. The original compiler likely had fast-path handling for APP-SUP and MAT-SUP (as suggested by reduce_ref_sup usage and interaction rules) which would distribute the operation during compilation.
-- Why it affects consistency: Omitting these fast paths means the new compiler performs fewer compile-time reductions than the old one likely did in analogous situations (APP-SUP, MAT-SUP). This represents a performance regression and a deviation from the original compiler's optimization level for these common patterns.
-- Required Change: To restore the original behavior, add else if (term_tag(cNam) == SUP) clauses to the compileFastBody handling for Use, Get, and Swi. The logic inside should mimic the original compiler's handling of APP-SUP/MAT-SUP – typically involving distributing the eliminator over the SUP branches and duplicating continuations as needed using DP0/DP1.
-- Missing OPY Node Generation
-- Where: compileFastCore case for Op2.
-- Problem: The code only generates OPX nodes, even when the left operand (nu0Nam) is known to be W32 and the right (nu1Nam) is not.
-- Consequence: The C runtime interaction OPY-W32 (which likely performed x <op> y directly) can never be triggered directly from compiled code. All binary operations become OPX, potentially forcing the runtime interaction OPX-W32 (which might rearrange to (>op y x)) even when OPY-W32 would have been applicable.
-- Why it affects consistency: The interaction rules imply OPX and OPY served distinct roles. The original compiler likely chose between them based on operand availability to enable more direct runtime reductions. By only generating OPX, the new compiler prevents the runtime from potentially taking the OPY-W32 path, altering the reduction sequence/performance compared to the original system.
-- Required Change: Modify the else block in compileFastCore for Op2. Before generating the OPX node, check if (term_tag(nu0Nam) == W32). If true, generate term_new(OPY, op, opxNam) instead of OPX. (Ensure the opxNam node correctly stores nu0Nam at +0 and nu1Nam at +1 regardless of emitting OPX or OPY).
-- Summary:
-- To ensure the new Compile.hs faithfully replicates the behavior of the original:
-- Must Fix: Items 1, 2, 3 (Missing binds, VAR encoding).
-- Should Fix for Equivalence: Items 4, 5 (SUP fast-paths, OPY generation) to match the likely optimization level and runtime interaction patterns of the original compiler.


-- TASK: rewrite Compile.hs again, to address the issues reported above.
-- COMPLETE UPDATED COMPILE.HS:

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
  let funcs = MS.toList (fidToNam book)
      decls = map (\(_, name) -> "Term " ++ name ++ "_f(Term);") funcs
  in unlines decls

compile :: Book -> Word16 -> String
compile book fid =
  let full = compileWith compileFull book fid in
  let fast = compileWith compileFast book fid in
  if "<ERR>" `isInfixOf` fast then full else fast

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
  emit $ "Term " ++ mget (fidToNam book) fid ++ "_f(Term ref) {"
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

compileFullCore _ _ (Var x) host = do
  compileFullVar x host

compileFullCore book fid t@(Ref x i xs) host = do
  checkRefAri book t
  refNam <- fresh "ref"
  let arity = length xs
  emit $ "Loc " ++ refNam ++ " = alloc_node(" ++ show arity ++ ");"
  argsT <- mapM (\ (i,arg) -> compileFullCore book fid arg (refNam ++ " + " ++ show i)) (zip [0..] xs)
  sequence_ [emit $ "set(" ++ refNam ++ " + " ++ show i ++ ", " ++ argT ++ ");" | (i,argT) <- zip [0..] argsT]
  return $ "term_new(REF, " ++ show i ++ ", " ++ refNam ++ ")"

compileFullCore book fid (Let mode var val bod) host = do
  letNam <- fresh "let"
  emit $ "Loc " ++ letNam ++ " = alloc_node(2);"
  valT <- compileFullCore book fid val (letNam ++ " + 0")
  emit $ "set(" ++ letNam ++ " + 0, " ++ valT ++ ");"
  bind var $ "term_new(VAR, 0, " ++ letNam ++ " + 0)"
  bodT <- compileFullCore book fid bod (letNam ++ " + 1")
  emit $ "set(" ++ letNam ++ " + 1, " ++ bodT ++ ");"
  return $ "term_new(LET, " ++ show (fromEnum mode) ++ ", " ++ letNam ++ ")"

compileFullCore _ _ Era _ = do
  return $ "term_new(ERA, 0, 0)"

compileFullCore book fid (Sup lab a b) host = do
  supNam <- fresh "sup"
  emit $ "Loc " ++ supNam ++ " = alloc_node(2);"
  aT <- compileFullCore book fid a (supNam ++ " + 0")
  bT <- compileFullCore book fid b (supNam ++ " + 1")
  emit $ "set(" ++ supNam ++ " + 0, " ++ aT ++ ");"
  emit $ "set(" ++ supNam ++ " + 1, " ++ bT ++ ");"
  return $ "term_new(SUP, " ++ show lab ++ ", " ++ supNam ++ ")"

compileFullCore book fid (Dup lab x y v f) host = do
  dupNam <- fresh "dup"
  emit $ "Loc " ++ dupNam ++ " = alloc_node(1);"
  bind x $ "term_new(DP0, " ++ show lab ++ ", " ++ dupNam ++ " + 0)"
  bind y $ "term_new(DP1, " ++ show lab ++ ", " ++ dupNam ++ " + 0)"
  valT <- compileFullCore book fid v (dupNam ++ " + 0")
  emit $ "set(" ++ dupNam ++ " + 0, " ++ valT ++ ");"
  bodT <- compileFullCore book fid f host
  return bodT

compileFullCore _ _ Set _ = do
  return $ "term_new(SET, 0, 0)"

compileFullCore _ _ Emp _ = do
  return $ "term_new(EMP, 0, 0)"

compileFullCore book fid (Efq c ms) host = do
  efqNam <- fresh "efq"
  emit $ "Loc " ++ efqNam ++ " = alloc_node(1);"
  cT <- compileFullCore book fid c (efqNam ++ " + 0")
  emit $ "set(" ++ efqNam ++ " + 0, " ++ cT ++ ");"
  let efq = "term_new(EFQ, 0, " ++ efqNam ++ ")"
  foldM (\term (key, val) -> do
    appNam <- fresh "app"
    emit $ "Loc " ++ appNam ++ " = alloc_node(2);"
    valT <- compileFullCore book fid val (appNam ++ " + 1")
    emit $ "set(" ++ appNam ++ " + 0, " ++ term ++ ");"
    emit $ "set(" ++ appNam ++ " + 1, " ++ valT ++ ");"
    return $ "term_new(APP, 0, " ++ appNam ++ ")") efq ms

compileFullCore _ _ Uni _ = do
  return $ "term_new(UNI, 0, 0)"

compileFullCore _ _ Nil _ = do
  return $ "term_new(NIL, 0, 0)"

compileFullCore book fid (Use c ms b) host = do
  useNam <- fresh "use"
  emit $ "Loc " ++ useNam ++ " = alloc_node(2);"
  cT <- compileFullCore book fid c (useNam ++ " + 0")
  bT <- compileFullCore book fid b (useNam ++ " + 1")
  emit $ "set(" ++ useNam ++ " + 0, " ++ cT ++ ");"
  emit $ "set(" ++ useNam ++ " + 1, " ++ bT ++ ");"
  let use = "term_new(USE, 0, " ++ useNam ++ ")"
  foldM (\term (key, val) -> do
    appNam <- fresh "app"
    emit $ "Loc " ++ appNam ++ " = alloc_node(2);"
    valT <- compileFullCore book fid val (appNam ++ " + 1")
    emit $ "set(" ++ appNam ++ " + 0, " ++ term ++ ");"
    emit $ "set(" ++ appNam ++ " + 1, " ++ valT ++ ");"
    return $ "term_new(APP, 0, " ++ appNam ++ ")") use ms

compileFullCore _ _ U32 _ = do
  return $ "term_new(U32, 0, 0)"

compileFullCore _ _ (W32 val) _ = do
  return $ "term_new(W32, 0, " ++ show (fromIntegral val) ++ ")"

compileFullCore book fid (Swi c ms z s) host = do
  swiNam <- fresh "swi"
  emit $ "Loc " ++ swiNam ++ " = alloc_node(3);"
  cT <- compileFullCore book fid c (swiNam ++ " + 0")
  zT <- compileFullCore book fid z (swiNam ++ " + 1")
  sT <- compileFullCore book fid s (swiNam ++ " + 2")
  emit $ "set(" ++ swiNam ++ " + 0, " ++ cT ++ ");"
  emit $ "set(" ++ swiNam ++ " + 1, " ++ zT ++ ");"
  emit $ "set(" ++ swiNam ++ " + 2, " ++ sT ++ ");"
  let swi = "term_new(SWI, 0, " ++ swiNam ++ ")"
  foldM (\term (key, val) -> do
    appNam <- fresh "app"
    emit $ "Loc " ++ appNam ++ " = alloc_node(2);"
    valT <- compileFullCore book fid val (appNam ++ " + 1")
    emit $ "set(" ++ appNam ++ " + 0, " ++ term ++ ");"
    emit $ "set(" ++ appNam ++ " + 1, " ++ valT ++ ");"
    return $ "term_new(APP, 0, " ++ appNam ++ ")") swi ms

compileFullCore book fid (Op2 opr a b) host = do
  opxNam <- fresh "opx"
  emit $ "Loc " ++ opxNam ++ " = alloc_node(2);"
  aT <- compileFullCore book fid a (opxNam ++ " + 0")
  bT <- compileFullCore book fid b (opxNam ++ " + 1")
  emit $ "set(" ++ opxNam ++ " + 0, " ++ aT ++ ");"
  emit $ "set(" ++ opxNam ++ " + 1, " ++ bT ++ ");"
  return $ "term_new(OPX, " ++ show (fromEnum opr) ++ ", " ++ opxNam ++ ")"

compileFullCore book fid (Sig _A _B) host = do
  sigNam <- fresh "sig"
  emit $ "Loc " ++ sigNam ++ " = alloc_node(2);"
  aT <- compileFullCore book fid _A (sigNam ++ " + 0")
  bT <- compileFullCore book fid _B (sigNam ++ " + 1")
  emit $ "set(" ++ sigNam ++ " + 0, " ++ aT ++ ");"
  emit $ "set(" ++ sigNam ++ " + 1, " ++ bT ++ ");"
  return $ "term_new(SIG, 0, " ++ sigNam ++ ")"

compileFullCore book fid (Tup a b) host = do
  tupNam <- fresh "tup"
  emit $ "Loc " ++ tupNam ++ " = alloc_node(2);"
  aT <- compileFullCore book fid a (tupNam ++ " + 0")
  bT <- compileFullCore book fid b (tupNam ++ " + 1")
  emit $ "set(" ++ tupNam ++ " + 0, " ++ aT ++ ");"
  emit $ "set(" ++ tupNam ++ " + 1, " ++ bT ++ ");"
  return $ "term_new(TUP, 0, " ++ tupNam ++ ")"

compileFullCore book fid (Get c ms b) host = do
  getNam <- fresh "get"
  emit $ "Loc " ++ getNam ++ " = alloc_node(2);"
  cT <- compileFullCore book fid c (getNam ++ " + 0")
  bT <- compileFullCore book fid b (getNam ++ " + 1")
  emit $ "set(" ++ getNam ++ " + 0, " ++ cT ++ ");"
  emit $ "set(" ++ getNam ++ " + 1, " ++ bT ++ ");"
  let get = "term_new(GET, 0, " ++ getNam ++ ")"
  foldM (\term (key, val) -> do
    appNam <- fresh "app"
    emit $ "Loc " ++ appNam ++ " = alloc_node(2);"
    valT <- compileFullCore book fid val (appNam ++ " + 1")
    emit $ "set(" ++ appNam ++ " + 0, " ++ term ++ ");"
    emit $ "set(" ++ appNam ++ " + 1, " ++ valT ++ ");"
    return $ "term_new(APP, 0, " ++ appNam ++ ")") get ms

compileFullCore book fid (All _A _B) host = do
  allNam <- fresh "all"
  emit $ "Loc " ++ allNam ++ " = alloc_node(2);"
  aT <- compileFullCore book fid _A (allNam ++ " + 0")
  bT <- compileFullCore book fid _B (allNam ++ " + 1")
  emit $ "set(" ++ allNam ++ " + 0, " ++ aT ++ ");"
  emit $ "set(" ++ allNam ++ " + 1, " ++ bT ++ ");"
  return $ "term_new(ALL, 0, " ++ allNam ++ ")"

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
compileFastBody book fid term@(Swi c ms z s) ctx stop itr = do
  cT     <- compileFastCore book fid c
  cNam   <- fresh "cond"
  emit $ "Term " ++ cNam ++ " = (" ++ cT ++ ");"
  
  emit $ "if (term_tag(" ++ cNam ++ ") == W32) {"
  tabInc
  emit $ "u32 num = term_loc(" ++ cNam ++ ");"
  emit $ "itrs += 1;"
  emit $ "if (num == 0) {"
  tabInc
  forM_ ms $ \ (key,val) -> do
    valT <- compileFastCore book fid val
    bind key valT
  compileFastBody book fid z ctx stop (itr + 1 + length ms)
  tabDec
  emit $ "} else {"
  tabInc
  preNam <- fresh "pre"
  predVar <- fresh "pred"
  emit $ "Term " ++ preNam ++ " = term_new(W32, 0, num - 1);"
  emit $ "Term " ++ predVar ++ " = " ++ preNam ++ ";"
  bind predVar predVar  -- Fix: bind the predVar
  forM_ ms $ \ (key,val) -> do
    valT <- compileFastCore book fid val
    bind key valT
  compileFastBody book fid (App s (Var predVar)) ctx stop (itr + 1 + length ms)
  tabDec
  emit $ "}"
  tabDec
  emit $ "} else if (term_tag(" ++ cNam ++ ") == SUP) {"
  tabInc
  emit $ "u64 lab = term_lab(" ++ cNam ++ ");"
  emit $ "Term a = got(term_loc(" ++ cNam ++ ") + 0);"
  emit $ "Term b = got(term_loc(" ++ cNam ++ ") + 1);"
  reuse 2 ("term_loc(" ++ cNam ++ ")")
  
  -- Create duplicated zero and successor cases
  z0Nam <- fresh "z0"
  z1Nam <- fresh "z1"
  s0Nam <- fresh "s0"
  s1Nam <- fresh "s1"
  
  -- Duplicate the zero case
  zT <- compileFastCore book fid z
  emit $ "Term " ++ z0Nam ++ " = " ++ zT ++ ";"
  emit $ "Term " ++ z1Nam ++ " = " ++ zT ++ ";"
  
  -- Duplicate the successor case
  sT <- compileFastCore book fid s
  emit $ "Term " ++ s0Nam ++ " = " ++ sT ++ ";"
  emit $ "Term " ++ s1Nam ++ " = " ++ sT ++ ";"
  
  -- Create the distributed Swi terms
  swi0Nam <- fresh "swi0"
  swi1Nam <- fresh "swi1"
  compileFastAlloc swi0Nam 3
  compileFastAlloc swi1Nam 3
  
  emit $ "set(" ++ swi0Nam ++ " + 0, a);"
  emit $ "set(" ++ swi0Nam ++ " + 1, " ++ z0Nam ++ ");"
  emit $ "set(" ++ swi0Nam ++ " + 2, " ++ s0Nam ++ ");"
  
  emit $ "set(" ++ swi1Nam ++ " + 0, b);"
  emit $ "set(" ++ swi1Nam ++ " + 1, " ++ z1Nam ++ ");"
  emit $ "set(" ++ swi1Nam ++ " + 2, " ++ s1Nam ++ ");"
  
  -- Create the result superposition
  supNam <- fresh "sup"
  compileFastAlloc supNam 2
  
  emit $ "Term swi0 = term_new(SWI, 0, " ++ swi0Nam ++ ");"
  emit $ "Term swi1 = term_new(SWI, 0, " ++ swi1Nam ++ ");"
  
  -- Apply moves to both sides
  forM_ ms $ \ (key,val) -> do
    valT <- compileFastCore book fid val
    bind key valT
  
  emit $ "set(" ++ supNam ++ " + 0, swi0);"
  emit $ "set(" ++ supNam ++ " + 1, swi1);"
  emit $ "Term result = term_new(SUP, lab, " ++ supNam ++ ");"
  
  compileFastBody book fid (Var "result") ctx stop (itr + 5 + length ms)
  tabDec
  emit $ "} else if (term_tag(" ++ cNam ++ ") == ERA) {"
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

compileFastBody book fid term@(Use c ms b) ctx stop itr = do
  cT   <- compileFastCore book fid c
  cNam <- fresh "cond"
  emit $ "Term " ++ cNam ++ " = (" ++ cT ++ ");"
  
  emit $ "if (term_tag(" ++ cNam ++ ") == NIL) {"
  tabInc
  emit $ "itrs += 1;"
  forM_ ms $ \ (key,val) -> do
    valT <- compileFastCore book fid val
    bind key valT
  compileFastBody book fid b ctx stop (itr + 1 + length ms)
  tabDec
  emit $ "} else if (term_tag(" ++ cNam ++ ") == SUP) {"
  tabInc
  emit $ "u64 lab = term_lab(" ++ cNam ++ ");"
  emit $ "Term a = got(term_loc(" ++ cNam ++ ") + 0);"
  emit $ "Term b = got(term_loc(" ++ cNam ++ ") + 1);"
  reuse 2 ("term_loc(" ++ cNam ++ ")")
  
  -- Create duplicated body
  b0Nam <- fresh "b0"
  b1Nam <- fresh "b1"
  bT <- compileFastCore book fid b
  emit $ "Term " ++ b0Nam ++ " = " ++ bT ++ ";"
  emit $ "Term " ++ b1Nam ++ " = " ++ bT ++ ";"
  
  -- Create the distributed Use terms
  use0Nam <- fresh "use0"
  use1Nam <- fresh "use1"
  compileFastAlloc use0Nam 2
  compileFastAlloc use1Nam 2
  
  emit $ "set(" ++ use0Nam ++ " + 0, a);"
  emit $ "set(" ++ use0Nam ++ " + 1, " ++ b0Nam ++ ");"
  
  emit $ "set(" ++ use1Nam ++ " + 0, b);"
  emit $ "set(" ++ use1Nam ++ " + 1, " ++ b1Nam ++ ");"
  
  -- Create the result superposition
  supNam <- fresh "sup"
  compileFastAlloc supNam 2
  
  emit $ "Term use0 = term_new(USE, 0, " ++ use0Nam ++ ");"
  emit $ "Term use1 = term_new(USE, 0, " ++ use1Nam ++ ");"
  
  -- Apply moves to both sides
  forM_ ms $ \ (key,val) -> do
    valT <- compileFastCore book fid val
    bind key valT
  
  emit $ "set(" ++ supNam ++ " + 0, use0);"
  emit $ "set(" ++ supNam ++ " + 1, use1);"
  emit $ "Term result = term_new(SUP, lab, " ++ supNam ++ ");"
  
  compileFastBody book fid (Var "result") ctx stop (itr + 5 + length ms)
  tabDec
  emit $ "} else if (term_tag(" ++ cNam ++ ") == ERA) {"
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

compileFastBody book fid term@(Get c ms b) ctx stop itr = do
  cT   <- compileFastCore book fid c
  cNam <- fresh "cond"
  emit $ "Term " ++ cNam ++ " = (" ++ cT ++ ");"
  
  emit $ "if (term_tag(" ++ cNam ++ ") == TUP) {"
  tabInc
  emit $ "itrs += 1;"
  f0Var <- fresh "fst"
  f1Var <- fresh "snd"
  emit $ "Term " ++ f0Var ++ " = got(term_loc(" ++ cNam ++ ") + 0);"
  emit $ "Term " ++ f1Var ++ " = got(term_loc(" ++ cNam ++ ") + 1);"
  reuse 2 ("term_loc(" ++ cNam ++ ")")
  bT <- compileFastCore book fid b
  forM_ ms $ \ (key,val) -> do
    valT <- compileFastCore book fid val
    bind key valT
  appANam <- fresh "appa"
  appALoc <- fresh "appaloc"
  compileFastAlloc appALoc 2
  emit $ "set(" ++ appALoc ++ " + 0, " ++ bT ++ ");"
  emit $ "set(" ++ appALoc ++ " + 1, " ++ f0Var ++ ");"
  emit $ "Term " ++ appANam ++ " = term_new(APP, 0, " ++ appALoc ++ ");"
  appBNam <- fresh "appb"
  appBLoc <- fresh "appbloc"
  compileFastAlloc appBLoc 2
  emit $ "set(" ++ appBLoc ++ " + 0, " ++ appANam ++ ");"
  emit $ "set(" ++ appBLoc ++ " + 1, " ++ f1Var ++ ");"
  emit $ "Term " ++ appBNam ++ " = term_new(APP, 0, " ++ appBLoc ++ ");"
  bind appBNam appBNam  -- Fix: bind the appBNam
  compileFastBody book fid (Var appBNam) ctx stop (itr + 3 + length ms)
  tabDec
  emit $ "} else if (term_tag(" ++ cNam ++ ") == SUP) {"
  tabInc
  emit $ "u64 lab = term_lab(" ++ cNam ++ ");"
  emit $ "Term a = got(term_loc(" ++ cNam ++ ") + 0);"
  emit $ "Term b = got(term_loc(" ++ cNam ++ ") + 1);"
  reuse 2 ("term_loc(" ++ cNam ++ ")")
  
  -- Create duplicated body
  b0Nam <- fresh "b0"
  b1Nam <- fresh "b1"
  bT <- compileFastCore book fid b
  emit $ "Term " ++ b0Nam ++ " = " ++ bT ++ ";"
  emit $ "Term " ++ b1Nam ++ " = " ++ bT ++ ";"
  
  -- Create the distributed Get terms
  get0Nam <- fresh "get0"
  get1Nam <- fresh "get1"
  compileFastAlloc get0Nam 2
  compileFastAlloc get1Nam 2
  
  emit $ "set(" ++ get0Nam ++ " + 0, a);"
  emit $ "set(" ++ get0Nam ++ " + 1, " ++ b0Nam ++ ");"
  
  emit $ "set(" ++ get1Nam ++ " + 0, b);"
  emit $ "set(" ++ get1Nam ++ " + 1, " ++ b1Nam ++ ");"
  
  -- Create the result superposition
  supNam <- fresh "sup"
  compileFastAlloc supNam 2
  
  emit $ "Term get0 = term_new(GET, 0, " ++ get0Nam ++ ");"
  emit $ "Term get1 = term_new(GET, 0, " ++ get1Nam ++ ");"
  
  -- Apply moves to both sides
  forM_ ms $ \ (key,val) -> do
    valT <- compileFastCore book fid val
    bind key valT
  
  emit $ "set(" ++ supNam ++ " + 0, get0);"
  emit $ "set(" ++ supNam ++ " + 1, get1);"
  emit $ "Term result = term_new(SUP, lab, " ++ supNam ++ ");"
  
  compileFastBody book fid (Var "result") ctx stop (itr + 5 + length ms)
  tabDec
  emit $ "} else if (term_tag(" ++ cNam ++ ") == ERA) {"
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

compileFastBody book fid term@(Efq c ms) ctx stop itr = do
  val <- compileFastCore book fid term
  emit $ "itrs += " ++ show itr ++ ";"
  compileFastSave book fid term ctx itr
  emit $ "return " ++ val ++ ";"

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

compileFastCore _ _ (Var k) = do
  compileFastVar k

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

compileFastCore book fid (Let mode k v f) = do
  valT <- compileFastCore book fid v
  case mode of
    LAZY -> do
      emit $ "itrs += 1;"
      bind k valT
    STRI -> do
      valNam <- fresh "val"
      emit $ "itrs += 1;"
      emit $ "Term " ++ valNam ++ " = reduce(" ++ valT ++ ");"
      bind k valNam
  compileFastCore book fid f

compileFastCore _ _ Era = 
  return $ "term_new(ERA, 0, 0)"

compileFastCore book fid (Sup l a b) = do
  supNam <- fresh "sup"
  compileFastAlloc supNam 2
  aT <- compileFastCore book fid a
  bT <- compileFastCore book fid b
  emit $ "set(" ++ supNam ++ " + 0, " ++ aT ++ ");"
  emit $ "set(" ++ supNam ++ " + 1, " ++ bT ++ ");"
  return $ "term_new(SUP, " ++ show l ++ ", " ++ supNam ++ ")"

compileFastCore book fid (Dup l x y v f) = do
  dupNam <- fresh "dup"
  dp0Nam <- fresh "dpA"
  dp1Nam <- fresh "dpB"
  valNam <- fresh "val"
  valT   <- compileFastCore book fid v
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
  emit $ dp0Nam ++ " = term_new(DP0, " ++ show l ++ ", " ++ dupNam ++ " + 0);"
  emit $ dp1Nam ++ " = term_new(DP1, " ++ show l ++ ", " ++ dupNam ++ " + 0);"
  tabDec
  emit $ "}"
  bind x dp0Nam
  bind y dp1Nam
  compileFastCore book fid f

compileFastCore _ _ Set = 
  return $ "term_new(SET, 0, 0)"

compileFastCore _ _ Emp = 
  return $ "term_new(EMP, 0, 0)"

compileFastCore book fid (Efq c ms) = do
  efqNam <- fresh "efq"
  compileFastAlloc efqNam 1
  cT <- compileFastCore book fid c
  emit $ "set(" ++ efqNam ++ " + 0, " ++ cT ++ ");"
  let efq = "term_new(EFQ, 0, " ++ efqNam ++ ")"
  foldM (\acc (_, val) -> do
    appNam <- fresh "app"
    compileFastAlloc appNam 2
    emit $ "set(" ++ appNam ++ " + 0, " ++ acc ++ ");"
    valT <- compileFastCore book fid val
    emit $ "set(" ++ appNam ++ " + 1, " ++ valT ++ ");"
    return $ "term_new(APP, 0, " ++ appNam ++ ")") efq ms

compileFastCore _ _ Uni = 
  return $ "term_new(UNI, 0, 0)"

compileFastCore _ _ Nil = 
  return $ "term_new(NIL, 0, 0)"

compileFastCore book fid (Use c ms b) = do
  useNam <- fresh "use"
  compileFastAlloc useNam 2
  cT <- compileFastCore book fid c
  bT <- compileFastCore book fid b
  emit $ "set(" ++ useNam ++ " + 0, " ++ cT ++ ");"
  emit $ "set(" ++ useNam ++ " + 1, " ++ bT ++ ");"
  let use = "term_new(USE, 0, " ++ useNam ++ ")"
  foldM (\acc (_, val) -> do
    appNam <- fresh "app"
    compileFastAlloc appNam 2
    emit $ "set(" ++ appNam ++ " + 0, " ++ acc ++ ");"
    valT <- compileFastCore book fid val
    emit $ "set(" ++ appNam ++ " + 1, " ++ valT ++ ");"
    return $ "term_new(APP, 0, " ++ appNam ++ ")") use ms

compileFastCore _ _ U32 = 
  return $ "term_new(U32, 0, 0)"

compileFastCore _ _ (W32 v) =
  return $ "term_new(W32, 0, " ++ show (fromIntegral v) ++ ")"

compileFastCore book fid (Swi c ms z s) = do
  swiNam <- fresh "swi"
  compileFastAlloc swiNam 3
  cT <- compileFastCore book fid c
  zT <- compileFastCore book fid z
  sT <- compileFastCore book fid s
  emit $ "set(" ++ swiNam ++ " + 0, " ++ cT ++ ");"
  emit $ "set(" ++ swiNam ++ " + 1, " ++ zT ++ ");"
  emit $ "set(" ++ swiNam ++ " + 2, " ++ sT ++ ");"
  let swi = "term_new(SWI, 0, " ++ swiNam ++ ")"
  foldM (\acc (_, val) -> do
    appNam <- fresh "app"
    compileFastAlloc appNam 2
    emit $ "set(" ++ appNam ++ " + 0, " ++ acc ++ ");"
    valT <- compileFastCore book fid val
    emit $ "set(" ++ appNam ++ " + 1, " ++ valT ++ ");"
    return $ "term_new(APP, 0, " ++ appNam ++ ")") swi ms

compileFastCore book fid (Op2 o a b) = do
  opxNam <- fresh "opx"
  retNam <- fresh "ret"
  nu0Nam <- fresh "nu0"
  nu1Nam <- fresh "nu1"
  nu0T <- compileFastCore book fid a
  nu1T <- compileFastCore book fid b
  emit $ "Term " ++ nu0Nam ++ " = (" ++ nu0T ++ ");"
  emit $ "Term " ++ nu1Nam ++ " = (" ++ nu1T ++ ");"
  emit $ "Term " ++ retNam ++ ";"
  emit $ "if (term_tag(" ++ nu0Nam ++ ") == W32 && term_tag(" ++ nu1Nam ++ ") == W32) {"
  emit $ "  itrs += 2;"
  let oprStr = case o of
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
  emit $ "} else if (term_tag(" ++ nu0Nam ++ ") == W32) {"
  tabInc
  compileFastAlloc opxNam 2
  emit $ "set(" ++ opxNam ++ " + 0, " ++ nu0Nam ++ ");"
  emit $ "set(" ++ opxNam ++ " + 1, " ++ nu1Nam ++ ");"
  emit $ retNam ++ " = term_new(OPY, " ++ show (fromEnum o) ++ ", " ++ opxNam ++ ");"
  tabDec
  emit $ "} else {"
  tabInc
  compileFastAlloc opxNam 2
  emit $ "set(" ++ opxNam ++ " + 0, " ++ nu0Nam ++ ");"
  emit $ "set(" ++ opxNam ++ " + 1, " ++ nu1Nam ++ ");"
  emit $ retNam ++ " = term_new(OPX, " ++ show (fromEnum o) ++ ", " ++ opxNam ++ ");"
  tabDec
  emit $ "}"
  return $ retNam

compileFastCore book fid (Sig _A _B) = do
  sigNam <- fresh "sig"
  compileFastAlloc sigNam 2
  aT <- compileFastCore book fid _A
  bT <- compileFastCore book fid _B
  emit $ "set(" ++ sigNam ++ " + 0, " ++ aT ++ ");"
  emit $ "set(" ++ sigNam ++ " + 1, " ++ bT ++ ");"
  return $ "term_new(SIG, 0, " ++ sigNam ++ ")"

compileFastCore book fid (Tup a b) = do
  tupNam <- fresh "tup"
  compileFastAlloc tupNam 2
  aT <- compileFastCore book fid a
  bT <- compileFastCore book fid b
  emit $ "set(" ++ tupNam ++ " + 0, " ++ aT ++ ");"
  emit $ "set(" ++ tupNam ++ " + 1, " ++ bT ++ ");"
  return $ "term_new(TUP, 0, " ++ tupNam ++ ")"

compileFastCore book fid (Get c ms b) = do
  getNam <- fresh "get"
  compileFastAlloc getNam 2
  cT <- compileFastCore book fid c
  bT <- compileFastCore book fid b
  emit $ "set(" ++ getNam ++ " + 0, " ++ cT ++ ");"
  emit $ "set(" ++ getNam ++ " + 1, " ++ bT ++ ");"
  let get = "term_new(GET, 0, " ++ getNam ++ ")"
  foldM (\acc (_, val) -> do
    appNam <- fresh "app"
    compileFastAlloc appNam 2
    emit $ "set(" ++ appNam ++ " + 0, " ++ acc ++ ");"
    valT <- compileFastCore book fid val
    emit $ "set(" ++ appNam ++ " + 1, " ++ valT ++ ");"
    return $ "term_new(APP, 0, " ++ appNam ++ ")") get ms

compileFastCore book fid (All _A _B) = do
  allNam <- fresh "all"
  compileFastAlloc allNam 2
  aT <- compileFastCore book fid _A
  bT <- compileFastCore book fid _B
  emit $ "set(" ++ allNam ++ " + 0, " ++ aT ++ ");"
  emit $ "set(" ++ allNam ++ " + 1, " ++ bT ++ ");"
  return $ "term_new(ALL, 0, " ++ allNam ++ ")"

compileFastCore book fid (Lam x f) = do
  lamNam <- fresh "lam"
  compileFastAlloc lamNam 1
  bind x $ "term_new(VAR, 0, " ++ lamNam ++ " + 0)"
  bodT <- compileFastCore book fid f
  emit $ "set(" ++ lamNam ++ " + 0, " ++ bodT ++ ");"
  return $ "term_new(LAM, 0, " ++ lamNam ++ ")"

compileFastCore book fid (App f x) = do
  appNam <- fresh "app"
  compileFastAlloc appNam 2
  fT <- compileFastCore book fid f
  xT <- compileFastCore book fid x
  emit $ "set(" ++ appNam ++ " + 0, " ++ fT ++ ");"
  emit $ "set(" ++ appNam ++ " + 1, " ++ xT ++ ");"
  return $ "term_new(APP, 0, " ++ appNam ++ ")"

-- Compiles a variable in fast mode
compileFastVar :: String -> Compile String
compileFastVar var = do
  bins <- gets bins
  case MS.lookup var bins of
    Just entry -> do
      return entry
    Nothing -> do
      return $ "<ERR>"

checkRefAri :: Book -> Core -> Compile ()
checkRefAri book core = do
  case core of
    Ref _ lab arg -> do
      let fid = fromIntegral lab
      let ari = funArity book fid
      let len = length arg
      when (ari /= fromIntegral len) $ do
        error $ "Arity mismatch on term: " ++ show core ++ ". Expected " ++ show ari ++ ", got " ++ show len ++ "."
    _ -> return ()
