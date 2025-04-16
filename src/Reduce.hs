{-./Type.hs-}

module Reduce where

import Control.Monad (when, forM, forM_)
import Data.Word
import Collapse
import Extract
import Foreign
import Inject
import Type
import System.Exit
import qualified Data.Map.Strict as MS

reduceAt :: Bool -> ReduceAt

reduceAt debug book host = do 
  term <- got host
  let tag = termTag term
  let lab = termLab term
  let loc = termLoc term

  when debug $ do
    root <- doExtractCoreAt gotT book 0
    core <- doExtractCoreAt gotT book host
    putStrLn $ "reduce: " ++ showTerm term
    -- putStrLn $ "---------------- CORE: "
    -- putStrLn $ showCore core
    putStrLn $ "---------------- ROOT: "
    putStrLn $ showCore (doLiftDups root)

  case tag of
    t | t == _LET_ -> do
      case modeT lab of
        LAZY -> do
          val <- got (loc + 0)
          cont host (reduceLet term val)
        STRI -> do
          val <- reduceAt debug book (loc + 0)
          cont host (reduceLet term val)

    t | t == _APP_ -> do
      fun <- reduceAt debug book (loc + 0)
      case termTag fun of
        t | t == _ERA_ -> cont host (reduceAppEra term fun)
        t | t == _LAM_ -> cont host (reduceAppLam term fun)
        t | t == _SUP_ -> cont host (reduceAppSup term fun)
        t | t == _CTR_ -> cont host (reduceAppCtr term fun)
        t | t == _W32_ -> cont host (reduceAppW32 term fun)
        t | t == _CHR_ -> cont host (reduceAppW32 term fun)
        _   -> set (loc + 0) fun >> return term

    t | t == _MAT_ -> do
      val <- reduceAt debug book (loc + 0)
      case termTag val of
        t | t == _ERA_ -> cont host (reduceMatEra term val)
        t | t == _LAM_ -> cont host (reduceMatLam term val)
        t | t == _SUP_ -> cont host (reduceMatSup term val)
        t | t == _CTR_ -> cont host (reduceMatCtr term val)
        t | t == _W32_ -> cont host (reduceMatW32 term val)
        t | t == _CHR_ -> cont host (reduceMatW32 term val)
        _   -> set (loc + 0) val >> return term

    t | t == _IFL_ -> do
      val <- reduceAt debug book (loc + 0)
      case termTag val of
        t | t == _ERA_ -> cont host (reduceMatEra term val)
        t | t == _LAM_ -> cont host (reduceMatLam term val)
        t | t == _SUP_ -> cont host (reduceMatSup term val)
        t | t == _CTR_ -> cont host (reduceMatCtr term val)
        t | t == _W32_ -> cont host (reduceMatW32 term val)
        t | t == _CHR_ -> cont host (reduceMatW32 term val)
        _   -> set (loc + 0) val >> return term

    t | t == _SWI_ -> do
      val <- reduceAt debug book (loc + 0)
      case termTag val of
        t | t == _ERA_ -> cont host (reduceMatEra term val)
        t | t == _LAM_ -> cont host (reduceMatLam term val)
        t | t == _SUP_ -> cont host (reduceMatSup term val)
        t | t == _CTR_ -> cont host (reduceMatCtr term val)
        t | t == _W32_ -> cont host (reduceMatW32 term val)
        t | t == _CHR_ -> cont host (reduceMatW32 term val)
        _   -> set (loc + 0) val >> return term

    t | t == _OPX_ -> do
      val <- reduceAt debug book (loc + 0)
      case termTag val of
        t | t == _ERA_ -> cont host (reduceOpxEra term val)
        t | t == _LAM_ -> cont host (reduceOpxLam term val)
        t | t == _SUP_ -> cont host (reduceOpxSup term val)
        t | t == _CTR_ -> cont host (reduceOpxCtr term val)
        t | t == _W32_ -> cont host (reduceOpxW32 term val)
        t | t == _CHR_ -> cont host (reduceOpxW32 term val)
        _   -> set (loc + 0) val >> return term

    t | t == _OPY_ -> do
      val <- reduceAt debug book (loc + 0)
      case termTag val of
        t | t == _ERA_ -> cont host (reduceOpyEra term val)
        t | t == _LAM_ -> cont host (reduceOpyLam term val)
        t | t == _SUP_ -> cont host (reduceOpySup term val)
        t | t == _CTR_ -> cont host (reduceOpyCtr term val)
        t | t == _W32_ -> cont host (reduceOpyW32 term val)
        t | t == _CHR_ -> cont host (reduceOpyW32 term val)
        _   -> set (loc + 0) val >> return term

    t | t == _DP0_ -> do
      sb0 <- got (loc + 0)
      if termGetBit sb0 == 0
        then do
          val <- reduceAt debug book (loc + 0)
          case termTag val of
            t | t == _ERA_ -> cont host (reduceDupEra term val)
            t | t == _LAM_ -> cont host (reduceDupLam term val)
            t | t == _SUP_ -> cont host (reduceDupSup term val)
            t | t == _CTR_ -> cont host (reduceDupCtr term val)
            t | t == _W32_ -> cont host (reduceDupW32 term val)
            t | t == _CHR_ -> cont host (reduceDupW32 term val)
            _   -> set (loc + 0) val >> return term
        else do
          set host (termRemBit sb0)
          reduceAt debug book host

    t | t == _DP1_ -> do
      sb1 <- got (loc + 0)
      if termGetBit sb1 == 0
        then do
          val <- reduceAt debug book (loc + 0)
          case termTag val of
            t | t == _ERA_ -> cont host (reduceDupEra term val)
            t | t == _LAM_ -> cont host (reduceDupLam term val)
            t | t == _SUP_ -> cont host (reduceDupSup term val)
            t | t == _CTR_ -> cont host (reduceDupCtr term val)
            t | t == _W32_ -> cont host (reduceDupW32 term val)
            t | t == _CHR_ -> cont host (reduceDupW32 term val)
            _   -> set (loc + 0) val >> return term
        else do
          set host (termRemBit sb1)
          reduceAt debug book host

    t | t == _VAR_ -> do
      sub <- got (loc + 0)
      if termGetBit sub == 0
        then return term
        else do
          set host (termRemBit sub)
          reduceAt debug book host

    t | t == _REF_ -> do
      reduceRefAt book host
      reduceAt debug book host

    _ -> do
      return term

  where
    cont host action = do
      ret <- action
      set host ret
      reduceAt debug book host

gotT :: Book -> Loc -> HVM Term
gotT book host = got host

reduceRefAt :: Book -> Loc -> HVM Term
reduceRefAt book host = do
  term <- got host
  let lab = termLab term
  let loc = termLoc term
  let fid = fromIntegral lab
  let ari = funArity book fid
  case lab of
    x | x == _DUP_F_ -> reduceRefAt_DupF book host loc ari
    x | x == _SUP_F_ -> reduceRefAt_SupF book host loc ari
    x | x == _LOG_F_ -> reduceRefAt_LogF book host loc ari
    otherwise -> case MS.lookup fid (fidToFun book) of
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
              then reduceAt False book (loc + i)
              else return term
        doInjectCoreAt book core host $ zip (map snd args) argTerms
      Nothing -> do
        putStrLn $ "RUNTIME_ERROR: Function ID " ++ show fid ++ " not found in fidToFun book."
        exitFailure

-- Primitive: Dynamic Dup `@DUP(lab val λdp0λdp1(bod))`
reduceRefAt_DupF :: Book -> Loc -> Loc -> Word16 -> HVM Term  
reduceRefAt_DupF book host loc ari = do
  incItr
  when (ari /= 3) $ do
    putStrLn $ "RUNTIME_ERROR: arity mismatch on call to '@DUP'."
    exitFailure
  lab <- reduceAt False book (loc + 0)
  val <- got (loc + 1)
  bod <- got (loc + 2)
  dup <- allocNode 1
  case termTag lab of
    t | t == _W32_ -> do
      when (termLoc lab > 0xFFFFFF) $ do
        error "RUNTIME_ERROR: dynamic DUP label too large"
      -- Create the DUP node with value
      set (dup + 0) val
      -- Create first APP node for (APP bod DP0)
      app1 <- allocNode 2
      set (app1 + 0) bod
      set (app1 + 1) (termNew _DP0_ (termLoc lab) dup)
      -- Create second APP node for (APP (APP bod DP0) DP1)
      app2 <- allocNode 2
      set (app2 + 0) (termNew _APP_ 0 app1)
      set (app2 + 1) (termNew _DP1_ (termLoc lab) dup)
      let ret = termNew _APP_ 0 app2
      set host ret
      return ret
    _ -> do
      core <- doExtractCoreAt gotT book (loc + 0)
      putStrLn $ "RUNTIME_ERROR: dynamic DUP without numeric label: " ++ showTerm lab
      putStrLn $ showCore (doLiftDups core)
      exitFailure

-- Primitive: Dynamic Sup `@SUP(lab tm0 tm1)`
reduceRefAt_SupF :: Book -> Loc -> Loc -> Word16 -> HVM Term
reduceRefAt_SupF book host loc ari = do
  incItr
  when (ari /= 3) $ do
    putStrLn $ "RUNTIME_ERROR: arity mismatch on call to '@SUP'."
    exitFailure
  lab <- reduceAt False book (loc + 0)
  tm0 <- got (loc + 1)
  tm1 <- got (loc + 2)
  sup <- allocNode 2
  case termTag lab of
    t | t == _W32_ -> do
      when (termLoc lab > 0xFFFFFF) $ do
        error "RUNTIME_ERROR: dynamic SUP label too large"
      let ret = termNew _SUP_ (termLoc lab) sup
      set (sup + 0) tm0
      set (sup + 1) tm1
      set host ret
      return ret
    _ -> error "RUNTIME_ERROR: dynamic SUP without numeric label."

-- Primitive: Logger `@LOG(msg)`
-- Will extract the term and log it. 
-- Returns 0.
reduceRefAt_LogF :: Book -> Loc -> Loc -> Word16 -> HVM Term
reduceRefAt_LogF book host loc ari = do
  incItr
  when (ari /= 1) $ do
    putStrLn $ "RUNTIME_ERROR: arity mismatch on call to '@LOG'."
    exitFailure
  msg <- doExtractCoreAt gotT book (loc + 0)
  putStrLn $ showCore (doLiftDups msg)
  -- msgs <- doCollapseFlatAt gotT book (loc + 0)
  -- forM_ msgs $ \msg -> do
    -- putStrLn $ showCore msg
  let ret = termNew _W32_ 0 0
  set host ret
  return ret

-- Primitive: Fresh `@FRESH`
-- Returns a fresh dup label.
reduceRefAt_FreshF :: Book -> Loc -> Loc -> Word16 -> HVM Term
reduceRefAt_FreshF book host loc ari = do
  incItr
  when (ari /= 0) $ do
    putStrLn $ "RUNTIME_ERROR: arity mismatch on call to '@Fresh'."
    exitFailure
  num <- fromIntegral <$> fresh
  let ret = termNew _W32_ 0 num
  set host ret
  return ret

reduceCAt :: Bool -> ReduceAt
reduceCAt = \ _ _ host -> reduceAtC host
