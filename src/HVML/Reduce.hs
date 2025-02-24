-- //./Type.hs//

module HVML.Reduce where

import Control.Monad (when, forM, forM_)
import Data.Word
import HVML.Collapse
import HVML.Extract
import HVML.Foreign
import HVML.Inject
import HVML.Show
import HVML.Type
import System.Exit
import qualified Data.Map.Strict as MS

reduceAt :: Bool -> ReduceAt

reduceAt debug book host gc = do 
  term <- got host
  let tag = termTag term
  let lab = termLab term
  let loc = termLoc term

  when debug $ do
    root <- doExtractCoreAt gotT book 0
    core <- doExtractCoreAt gotT book host
    putStrLn $ "reduce: " ++ termToString term
    -- putStrLn $ "---------------- CORE: "
    -- putStrLn $ coreToString core
    putStrLn $ "---------------- ROOT: "
    putStrLn $ coreToString (doLiftDups root)

  case tagT tag of

    LET -> do
      case modeT lab of
        LAZY -> do
          val <- got (loc + 0)
          cont host (reduceLet term val)
        STRI -> do
          val <- reduceAt debug book (loc + 0) gc
          cont host (reduceLet term val)
        PARA -> do
          error "TODO"

    APP -> do
      fun <- reduceAt debug book (loc + 0) gc
      case tagT (termTag fun) of
        ERA -> cont host (reduceAppEra term fun)
        LAM -> cont host (reduceAppLam term fun)
        SUP -> cont host (reduceAppSup term fun)
        CTR -> cont host (reduceAppCtr term fun)
        W32 -> cont host (reduceAppW32 term fun)
        CHR -> cont host (reduceAppW32 term fun)
        _   -> setOld (loc + 0) fun >> return term

    MAT -> do
      val <- reduceAt debug book (loc + 0) gc
      case tagT (termTag val) of
        ERA -> cont host (reduceMatEra term val)
        LAM -> cont host (reduceMatLam term val)
        SUP -> cont host (reduceMatSup term val)
        CTR -> cont host (reduceMatCtr term val)
        W32 -> cont host (reduceMatW32 term val)
        CHR -> cont host (reduceMatW32 term val)
        _   -> setOld (loc + 0) val >> return term

    IFL -> do
      val <- reduceAt debug book (loc + 0) gc
      case tagT (termTag val) of
        ERA -> cont host (reduceMatEra term val)
        LAM -> cont host (reduceMatLam term val)
        SUP -> cont host (reduceMatSup term val)
        CTR -> cont host (reduceMatCtr term val)
        W32 -> cont host (reduceMatW32 term val)
        CHR -> cont host (reduceMatW32 term val)
        _   -> setOld (loc + 0) val >> return term

    SWI -> do
      val <- reduceAt debug book (loc + 0) gc
      case tagT (termTag val) of
        ERA -> cont host (reduceMatEra term val)
        LAM -> cont host (reduceMatLam term val)
        SUP -> cont host (reduceMatSup term val)
        CTR -> cont host (reduceMatCtr term val)
        W32 -> cont host (reduceMatW32 term val)
        CHR -> cont host (reduceMatW32 term val)
        _   -> setOld (loc + 0) val >> return term

    OPX -> do
      val <- reduceAt debug book (loc + 0) gc
      case tagT (termTag val) of
        ERA -> cont host (reduceOpxEra term val)
        LAM -> cont host (reduceOpxLam term val)
        SUP -> cont host (reduceOpxSup term val)
        CTR -> cont host (reduceOpxCtr term val)
        W32 -> cont host (reduceOpxW32 term val)
        CHR -> cont host (reduceOpxW32 term val)
        _   -> setOld (loc + 0) val >> return term

    OPY -> do
      val <- reduceAt debug book (loc + 1) gc
      case tagT (termTag val) of
        ERA -> cont host (reduceOpyEra term val)
        LAM -> cont host (reduceOpyLam term val)
        SUP -> cont host (reduceOpySup term val)
        CTR -> cont host (reduceOpyCtr term val)
        W32 -> cont host (reduceOpyW32 term val)
        CHR -> cont host (reduceOpyW32 term val)
        _   -> setOld (loc + 1) val >> return term

    DP0 -> do
      sb0 <- got (loc + 0)
      if termGetBit sb0 == 0
        then do
          val <- reduceAt debug book (loc + 0) gc
          case tagT (termTag val) of
            ERA -> cont host (reduceDupEra term val)
            LAM -> cont host (reduceDupLam term val)
            SUP -> cont host (reduceDupSup term val)
            CTR -> cont host (reduceDupCtr term val)
            W32 -> cont host (reduceDupW32 term val)
            CHR -> cont host (reduceDupW32 term val)
            _   -> setOld (loc + 0) val >> return term
        else do
          setOld host (termRemBit sb0)
          reduceAt debug book host gc

    DP1 -> do
      sb1 <- got (loc + 0)
      if termGetBit sb1 == 0
        then do
          val <- reduceAt debug book (loc + 0) gc
          case tagT (termTag val) of
            ERA -> cont host (reduceDupEra term val)
            LAM -> cont host (reduceDupLam term val)
            SUP -> cont host (reduceDupSup term val)
            CTR -> cont host (reduceDupCtr term val)
            W32 -> cont host (reduceDupW32 term val)
            CHR -> cont host (reduceDupW32 term val)
            _   -> setOld (loc + 0) val >> return term
        else do
          setOld host (termRemBit sb1)
          reduceAt debug book host gc

    VAR -> do
      sub <- got (loc + 0)
      if termGetBit sub == 0
        then return term
        else do
          setOld host (termRemBit sub)
          reduceAt debug book host gc

    REF -> do
      reduceRefAt book host
      reduceAt debug book host gc

    otherwise -> do
      return term

  where
    cont host action = do
      ret <- action
      setOld host ret
      reduceAt debug book host gc

gotT :: Book -> Loc -> Bool -> HVM Term
gotT book host gc = got host

reduceRefAt :: Book -> Loc -> HVM Term
reduceRefAt book host = do
  term <- got host
  let lab = termLab term
  let loc = termLoc term
  let fid = lab
  let ari = funArity book fid
  case fid of
    x | x == _DUP_F_ -> reduceRefAt_DupF book host loc ari
    x | x == _SUP_F_ -> reduceRefAt_SupF book host loc ari
    x | x == _LOG_F_ -> reduceRefAt_LogF book host loc ari
    oterwise -> case MS.lookup fid (fidToFun book) of
      Just ((copy, args), core) -> do
        incItr
        when (length args /= fromIntegral ari) $ do
          putStrLn $ "RUNTIME_ERROR: arity mismatch on call to '@" ++ mget (fidToNam book) fid ++ "'."
          exitFailure
        argTerms <- if ari == 0
          then return []
          else forM (zip [0..] args) $ \(i, (strict, _)) -> do
            term <- got (loc + i)
            if strict
              then reduceAt False book (loc + i) False
              else return term
        doInjectCoreAt book core host $ zip (map snd args) argTerms

-- Primitive: Dynamic Dup `@DUP(lab val λdp0λdp1(bod))`
reduceRefAt_DupF :: Book -> Loc -> Loc -> Word64 -> HVM Term  
reduceRefAt_DupF book host loc ari = do
  incItr
  when (ari /= 3) $ do
    putStrLn $ "RUNTIME_ERROR: arity mismatch on call to '@DUP'."
    exitFailure
  lab <- reduceAt False book (loc + 0) False
  val <- got (loc + 1)
  bod <- got (loc + 2)
  dup <- allocNode 1
  case tagT (termTag lab) of
    W32 -> do
      when (termLoc lab > 0xFFFF) $ do
        error "RUNTIME_ERROR: dynamic DUP label too large"
      -- Create the DUP node with value
      setNew (dup + 0) val
      -- Create first APP node for (APP bod DP0)
      app1 <- allocNode 2
      setNew (app1 + 0) bod
      setNew (app1 + 1) (termNew _DP0_ (termLoc lab) dup)
      -- Create second APP node for (APP (APP bod DP0) DP1)
      app2 <- allocNode 2
      setNew (app2 + 0) (termNew _APP_ 0 app1)
      setNew (app2 + 1) (termNew _DP1_ (termLoc lab) dup)
      let ret = termNew _APP_ 0 app2
      setOld host ret
      return ret
    _ -> do
      core <- doExtractCoreAt gotT book (loc + 0)
      putStrLn $ "RUNTIME_ERROR: dynamic DUP without numeric label: " ++ termToString lab
      putStrLn $ coreToString (doLiftDups core)
      exitFailure

-- Primitive: Dynamic Sup `@SUP(lab tm0 tm1)`
reduceRefAt_SupF :: Book -> Loc -> Loc -> Word64 -> HVM Term
reduceRefAt_SupF book host loc ari = do
  incItr
  when (ari /= 3) $ do
    putStrLn $ "RUNTIME_ERROR: arity mismatch on call to '@SUP'."
    exitFailure
  lab <- reduceAt False book (loc + 0) False
  tm0 <- got (loc + 1)
  tm1 <- got (loc + 2)
  sup <- allocNode 2
  case tagT (termTag lab) of
    W32 -> do
      when (termLoc lab > 0xFFFF) $ do
        error "RUNTIME_ERROR: dynamic SUP label too large"
      let ret = termNew _SUP_ (termLoc lab) sup
      setNew (sup + 0) tm0
      setNew (sup + 1) tm1
      setOld host ret
      return ret
    _ -> error "RUNTIME_ERROR: dynamic SUP without numeric label."

-- Primitive: Logger `@LOG(msg)`
-- Will extract the term and log it. 
-- Returns 0.
reduceRefAt_LogF :: Book -> Loc -> Loc -> Word64 -> HVM Term
reduceRefAt_LogF book host loc ari = do
  incItr
  when (ari /= 1) $ do
    putStrLn $ "RUNTIME_ERROR: arity mismatch on call to '@LOG'."
    exitFailure
  msg <- doExtractCoreAt gotT book (loc + 0)
  putStrLn $ coreToString (doLiftDups msg)
  -- msgs <- doCollapseFlatAt gotT book (loc + 0)
  -- forM_ msgs $ \msg -> do
    -- putStrLn $ coreToString msg
  let ret = termNew _W32_ 0 0
  setOld host ret
  return ret

-- Primitive: Fresh `@FRESH`
-- Returns a fresh dup label.
reduceRefAt_FreshF :: Book -> Loc -> Loc -> Word64 -> HVM Term
reduceRefAt_FreshF book host loc ari = do
  incItr
  when (ari /= 0) $ do
    putStrLn $ "RUNTIME_ERROR: arity mismatch on call to '@Fresh'."
    exitFailure
  num <- fresh
  let ret = termNew _W32_ 0 num
  setOld host ret
  return ret

reduceCAt :: Bool -> ReduceAt
reduceCAt = \ _ _ host gc -> do
  term <- got host
  spush (termNew _SUB_ 0 host)
  whnf <- reduceC term (if gc then 1 else 0)
  tHst <- spop
  setOld (termLoc tHst) whnf
  return $ whnf

-- normalAtWith :: (Book -> Term -> HVM Term) -> Book -> Loc -> HVM Term
-- normalAtWith reduceAt book host = do
  -- term <- got host
  -- if termBit term == 1 then do
    -- return term
  -- else do
    -- whnf <- reduceAt book host
    -- set host $ termSetBit whnf
    -- let tag = termTag whnf
    -- let lab = termLab whnf
    -- let loc = termLoc whnf
    -- case tagT tag of
      -- APP -> do
        -- normalAtWith reduceAt book (loc + 0)
        -- normalAtWith reduceAt book (loc + 1)
        -- return whnf
      -- LAM -> do
        -- normalAtWith reduceAt book (loc + 1)
        -- return whnf
      -- SUP -> do
        -- normalAtWith reduceAt book (loc + 0)
        -- normalAtWith reduceAt book (loc + 1)
        -- return whnf
      -- DP0 -> do
        -- normalAtWith reduceAt book (loc + 0)
        -- return whnf
      -- DP1 -> do
        -- normalAtWith reduceAt book (loc + 0)
        -- return whnf
      -- CTR -> do
        -- let ari = u12v2Y lab
        -- let ars = (if ari == 0 then [] else [0 .. ari - 1]) :: [Word64]
        -- mapM_ (\i -> normalAtWith reduceAt book (loc + i)) ars
        -- return whnf
      -- MAT -> do
        -- let ari = lab
        -- let ars = [0 .. ari] :: [Word64]
        -- mapM_ (\i -> normalAtWith reduceAt book (loc + i)) ars
        -- return whnf
      -- _ -> do
        -- return whnf

-- normalAt :: Book -> Loc -> HVM Term
-- normalAt = normalAtWith (reduceAt False)

-- normalCAt :: Book -> Loc -> HVM Term
-- normalCAt = normalAtWith (reduceCAt False)

-- TODO: I disabled Fast Copy Optimization on interpreted mode because I
-- don't think it is relevant here. We use it for speed, to trigger the
-- hot paths on compiled functions, which don't happen when interpreted.
-- I think leaving it out is good because it ensures interpreted mode is
-- always optimal (minimizing interactions). This also allows the dev to
-- see how Fast Copy Mode affects the interaction count.
-- let inject = doInjectCoreAt book core host $ zip (map snd args) argTerms
-- Fast Copy Optimization
-- if copy then do
  -- let supGet = \x (idx,sup) -> if tagT (termTag sup) == SUP then Just (idx,sup) else x
  -- let supGot = foldl' supGet Nothing $ zip [0..] argTerms
  -- case supGot of
    -- Just (idx,sup) -> do
      -- let isCopySafe = case MS.lookup fid (idToLabs book) of
            -- Nothing   -> False
            -- Just labs -> not $ MS.member (termLab sup) labs
      -- if isCopySafe then do
        -- term <- reduceRefSup term idx
        -- set host term
        -- return term
      -- else inject
    -- otherwise -> inject
-- else inject
