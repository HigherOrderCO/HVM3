{-./Type.hs-}
{-./Inject.hs-}

module Extract where

import Control.Monad (foldM, forM_, forM)
import Control.Monad.State
import Data.Bits (shiftR)
import Data.Char (chr, ord)
import Data.IORef
import Data.Word
import Debug.Trace
import Foreign
import System.IO.Unsafe (unsafeInterleaveIO)
import Type
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as MS

extractCoreAt :: IORef IS.IntSet -> ReduceAt -> Book -> Loc -> HVM Core

extractCoreAt dupsRef reduceAt book host = unsafeInterleaveIO $ do
  term <- reduceAt book host
  -- trace ("extract " ++ show host ++ " " ++ termToString term) $
  let tag = termTag term
  case tag of
    t | t == _VAR_ -> do
      let loc = termLoc term
      sub <- got (loc + 0)
      if termGetBit sub == 0
        then do
          name <- return $ "$" ++ show (loc + 0)
          return $ Var name
        else do
          set (loc + 0) (termRemBit sub)
          extractCoreAt dupsRef reduceAt book (loc + 0)

    t | t == _REF_ -> do
      let loc = termLoc term
      let lab = termLab term
      let fid = fromIntegral lab
      let ari = fromIntegral $ funArity book fid
      let aux = if ari == 0 then [] else [0..ari-1]
      xs <- mapM (\i -> extractCoreAt dupsRef reduceAt book (loc + i)) aux
      let name = MS.findWithDefault "?" fid (fidToNam book)
      return $ Ref name fid xs

    t | t == _LET_ -> do
      let loc  = termLoc term
      let mode = modeT (termLab term)
      name <- return $ "$" ++ show (loc + 0)
      v  <- extractCoreAt dupsRef reduceAt book (loc + 0)
      f  <- extractCoreAt dupsRef reduceAt book (loc + 1)
      return $ Let mode name v f

    t | t == _ERA_ -> do
      return Era

    t | t == _SUP_ -> do
      let loc = termLoc term
      let lab = termLab term
      a <- extractCoreAt dupsRef reduceAt book (loc + 0)
      b <- extractCoreAt dupsRef reduceAt book (loc + 1)
      return $ Sup lab a b

    t | t == _DP0_ -> do
      let loc = termLoc term
      let lab = termLab term
      dups <- readIORef dupsRef
      if IS.member (fromIntegral loc) dups
      then do
        name <- return $ "$" ++ show (loc + 0) ++ "_0"
        return $ Var name
      else do
        x <- return $ "$" ++ show (loc + 0) ++ "_0"
        y <- return $ "$" ++ show (loc + 0) ++ "_1"
        v <- extractCoreAt dupsRef reduceAt book (loc + 0)
        modifyIORef' dupsRef (IS.insert (fromIntegral loc))
        return $ Dup lab x y v (Var x)

    t | t == _DP1_ -> do
      let loc = termLoc term
      let lab = termLab term
      dups <- readIORef dupsRef
      if IS.member (fromIntegral loc) dups
      then do
        name <- return $ "$" ++ show (loc + 0) ++ "_1"
        return $ Var name
      else do
        x <- return $ "$" ++ show (loc + 0) ++ "_0"
        y <- return $ "$" ++ show (loc + 0) ++ "_1"
        v <- extractCoreAt dupsRef reduceAt book (loc + 0)
        modifyIORef' dupsRef (IS.insert (fromIntegral loc))
        return $ Dup lab x y v (Var y)

    t | t == _SET_ -> do
      return Set

    t | t == _EMP_ -> do
      return Emp

    t | t == _EFQ_ -> do
      let loc = termLoc term
      c <- extractCoreAt dupsRef reduceAt book (loc + 0)
      return $ Efq c []

    t | t == _UNI_ -> do
      return Uni

    t | t == _NIL_ -> do
      return Nil

    t | t == _USE_ -> do
      let loc = termLoc term
      c <- extractCoreAt dupsRef reduceAt book (loc + 0)
      b <- extractCoreAt dupsRef reduceAt book (loc + 1)
      return $ Use c [] b

    t | t == _U32_ -> do
      return U32

    t | t == _W32_ -> do
      let val = termLoc term
      return $ W32 (fromIntegral val)

    t | t == _SWI_ -> do
      let loc = termLoc term
      c <- extractCoreAt dupsRef reduceAt book (loc + 0)
      z <- extractCoreAt dupsRef reduceAt book (loc + 1)
      s <- extractCoreAt dupsRef reduceAt book (loc + 2)
      return $ Swi c [] z s

    t | t == _OPX_ -> do
      let loc = termLoc term
      let opr = toEnum (fromIntegral (termLab term))
      a <- extractCoreAt dupsRef reduceAt book (loc + 0)
      b <- extractCoreAt dupsRef reduceAt book (loc + 1)
      return $ Op2 opr a b

    t | t == _OPY_ -> do
      let loc = termLoc term
      let opr = toEnum (fromIntegral (termLab term))
      b <- extractCoreAt dupsRef reduceAt book (loc + 0)
      a <- extractCoreAt dupsRef reduceAt book (loc + 1)
      return $ Op2 opr a b

    t | t == _SIG_ -> do
      let loc = termLoc term
      _A <- extractCoreAt dupsRef reduceAt book (loc + 0)
      _B <- extractCoreAt dupsRef reduceAt book (loc + 1)
      return $ Sig _A _B

    t | t == _TUP_ -> do
      let loc = termLoc term
      a <- extractCoreAt dupsRef reduceAt book (loc + 0)
      b <- extractCoreAt dupsRef reduceAt book (loc + 1)
      return $ Tup a b

    t | t == _GET_ -> do
      let loc = termLoc term
      c <- extractCoreAt dupsRef reduceAt book (loc + 0)
      b <- extractCoreAt dupsRef reduceAt book (loc + 1)
      return $ Get c [] b

    t | t == _ALL_ -> do
      let loc = termLoc term
      _A <- extractCoreAt dupsRef reduceAt book (loc + 0)
      _B <- extractCoreAt dupsRef reduceAt book (loc + 1)
      return $ All _A _B

    t | t == _LAM_ -> do
      let loc = termLoc term
      x <- return $ "$" ++ show (loc + 0)
      f <- extractCoreAt dupsRef reduceAt book (loc + 0)
      return $ Lam x f

    t | t == _APP_ -> do
      let loc = termLoc term
      f <- extractCoreAt dupsRef reduceAt book (loc + 0)
      x <- extractCoreAt dupsRef reduceAt book (loc + 1)
      return $ App f x

    _ -> do
      return Era

doExtractCoreAt :: ReduceAt -> Book -> Loc -> HVM Core
doExtractCoreAt reduceAt book loc = do
  dupsRef <- newIORef IS.empty
  core    <- extractCoreAt dupsRef reduceAt book loc
  return core
  -- return $ doLiftDups core

-- Lifting Dups
-- ------------

liftDups :: Core -> (Core, Core -> Core)

liftDups (Var x) =
  (Var x, id)

liftDups (Ref x i xs) =
  let (xsT, xsD) = liftDupsList xs
  in (Ref x i xsT, xsD)

liftDups (Let m x v f) =
  let (vT, vD) = liftDups v
      (fT, fD) = liftDups f
  in (Let m x vT fT, vD . fD)

liftDups Era =
  (Era, id)

liftDups (Sup l a b) =
  let (aT, aD) = liftDups a
      (bT, bD) = liftDups b
  in (Sup l aT bT, aD . bD)

liftDups (Dup l x y v f) =
  let (vT, vD) = liftDups v
      (fT, fD) = liftDups f
  in (fT, \term -> vD (fD (Dup l x y vT term)))

liftDups Set =
  (Set, id)

liftDups Emp =
  (Emp, id)

liftDups (Efq c ms) =
  let (cT, cD) = liftDups c
      (msT, msD) = liftDupsMov ms
  in (Efq cT msT, cD . msD)

liftDups Uni =
  (Uni, id)

liftDups Nil =
  (Nil, id)

liftDups (Use c ms b) =
  let (cT, cD) = liftDups c
      (msT, msD) = liftDupsMov ms
      (bT, bD) = liftDups b
  in (Use cT msT bT, cD . msD . bD)

liftDups U32 =
  (U32, id)

liftDups (W32 n) =
  (W32 n, id)

liftDups (Swi c ms z s) =
  let (cT, cD) = liftDups c
      (msT, msD) = liftDupsMov ms
      (zT, zD) = liftDups z
      (sT, sD) = liftDups s
  in (Swi cT msT zT sT, cD . msD . zD . sD)

liftDups (Op2 o a b) =
  let (aT, aD) = liftDups a
      (bT, bD) = liftDups b
  in (Op2 o aT bT, aD . bD)

liftDups (Sig _A _B) =
  let (aT, aD) = liftDups _A
      (bT, bD) = liftDups _B
  in (Sig aT bT, aD . bD)

liftDups (Tup a b) =
  let (aT, aD) = liftDups a
      (bT, bD) = liftDups b
  in (Tup aT bT, aD . bD)

liftDups (Get c ms b) =
  let (cT, cD) = liftDups c
      (msT, msD) = liftDupsMov ms
      (bT, bD) = liftDups b
  in (Get cT msT bT, cD . msD . bD)

liftDups (All _A _B) =
  let (aT, aD) = liftDups _A
      (bT, bD) = liftDups _B
  in (All aT bT, aD . bD)

liftDups (Lam x f) =
  let (fT, fD) = liftDups f
  in (Lam x fT, fD)

liftDups (App f x) =
  let (fT, fD) = liftDups f
      (xT, xD) = liftDups x
  in (App fT xT, fD . xD)

liftDupsList :: [Core] -> ([Core], Core -> Core)

liftDupsList [] = 
  ([], id)

liftDupsList (x:xs) =
  let (xT, xD)   = liftDups x
      (xsT, xsD) = liftDupsList xs
  in (xT:xsT, xD . xsD)

liftDupsMov :: [(String, Core)] -> ([(String, Core)], Core -> Core)

liftDupsMov [] = 
  ([], id)

liftDupsMov ((k,v):xs) =
  let (vT, vD)   = liftDups v
      (xsT, xsD) = liftDupsMov xs
  in ((k,vT):xsT, vD . xsD)

doLiftDups :: Core -> Core
doLiftDups term =
  let (termExpr, termDups) = liftDups term in
  let termBody = termDups (Var "") in
  -- hack to print expr before dups
  Let LAZY "" termExpr termBody
