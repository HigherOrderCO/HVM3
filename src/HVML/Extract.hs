-- //./Type.hs//
-- //./Inject.hs//

module HVML.Extract where

import Control.Monad (foldM, forM_, forM)
import Control.Monad.State
import Data.Bits (shiftR)
import Data.Char (chr, ord)
import Data.IORef
import Data.Word
import Debug.Trace
import HVML.Show
import HVML.Type
import System.IO.Unsafe (unsafeInterleaveIO)
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as MS

extractCoreAt :: IORef IS.IntSet -> ReduceAt -> Book -> Loc -> HVM Core

extractCoreAt dupsRef reduceAt book host = do
  term <- reduceAt book host True
  -- trace ("extract " ++ show host ++ " " ++ termToString term) $
  case tagT (termTag term) of

    ERA -> do
      return Era

    LET -> do
      let loc  = termLoc term
      let mode = modeT (termLab term)
      name <- return $ "$" ++ show (loc + 0)
      ridx <- rpush term 2
      val  <- extractCoreAtLazy dupsRef reduceAt book ridx 0
      bod  <- extractCoreAtLazy dupsRef reduceAt book ridx 1
      return $ Let mode name val bod

    LAM -> do
      let loc = termLoc term
      name <- return $ "$" ++ show (loc + 0)
      ridx <- rpush term 1
      bod  <- extractCoreAtLazy dupsRef reduceAt book ridx 0
      return $ Lam name bod

    APP -> do
      let loc = termLoc term
      ridx <- rpush term 2
      fun <- extractCoreAtLazy dupsRef reduceAt book ridx 0
      arg <- extractCoreAtLazy dupsRef reduceAt book ridx 1
      return $ App fun arg

    SUP -> do
      let loc = termLoc term
      let lab = termLab term
      ridx <- rpush term 2
      tm0 <- extractCoreAtLazy dupsRef reduceAt book ridx 0
      tm1 <- extractCoreAtLazy dupsRef reduceAt book ridx 1
      return $ Sup lab tm0 tm1

    VAR -> do
      let loc = termLoc term
      sub <- got (loc + 0)
      if termGetBit sub == 0
        then do
          name <- return $ "$" ++ show (loc + 0)
          return $ Var name
        else do
          setOld (loc + 0) (termRemBit sub)
          extractCoreAt dupsRef reduceAt book (loc + 0)

    DP0 -> do
      let loc = termLoc term
      let lab = termLab term
      dups <- readIORef dupsRef
      if IS.member (fromIntegral loc) dups
      then do
        name <- return $ "$" ++ show (loc + 0) ++ "_0"
        return $ Var name
      else do
        dp0 <- return $ "$" ++ show (loc + 0) ++ "_0"
        dp1 <- return $ "$" ++ show (loc + 0) ++ "_1"
        ridx <- rpush term 1
        val <- extractCoreAtLazy dupsRef reduceAt book ridx 0
        modifyIORef' dupsRef (IS.insert (fromIntegral loc))
        return $ Dup lab dp0 dp1 val (Var dp0)

    DP1 -> do
      let loc = termLoc term
      let lab = termLab term
      dups <- readIORef dupsRef
      if IS.member (fromIntegral loc) dups
      then do
        name <- return $ "$" ++ show (loc + 0) ++ "_1"
        return $ Var name
      else do
        dp0 <- return $ "$" ++ show (loc + 0) ++ "_0"
        dp1 <- return $ "$" ++ show (loc + 0) ++ "_1"
        ridx <- rpush term 1
        val <- extractCoreAtLazy dupsRef reduceAt book ridx 0
        modifyIORef' dupsRef (IS.insert (fromIntegral loc))
        return $ Dup lab dp0 dp1 val (Var dp1)

    CTR -> do
      let loc = termLoc term
      let lab = termLab term
      let cid = lab
      let nam = mget (cidToCtr book) cid
      let ari = mget (cidToAri book) cid
      let ars = if ari == 0 then [] else [0..ari-1]
      ridx <- rpush term (fromIntegral ari)
      fds <- mapM (\i -> extractCoreAtLazy dupsRef reduceAt book ridx i) ars
      return $ Ctr nam fds

    MAT -> do
      let loc = termLoc term
      let lab = termLab term
      let cid = lab
      let len = fromIntegral $ mget (cidToLen book) cid
      ridx <- rpush term (1 + len)
      val <- extractCoreAtLazy dupsRef reduceAt book ridx 0
      css <- foldM (\css i -> do
        let ctr = mget (cidToCtr book) (cid + i)
        let ari = fromIntegral $ mget (cidToAri book) (cid + i)
        let fds = if ari == 0 then [] else ["$" ++ show (loc + 1 + j) | j <- [0..ari-1]]
        bod <- extractCoreAtLazy dupsRef reduceAt book ridx (1 + i)
        return $ (ctr,fds,bod):css) [] [0..len-1]
      return $ Mat val [] (reverse css)

    IFL -> do
      let loc = termLoc term
      let lab = termLab term
      ridx <- rpush term 3
      val <- extractCoreAtLazy dupsRef reduceAt book ridx 0
      cs0 <- extractCoreAtLazy dupsRef reduceAt book ridx 1
      cs1 <- extractCoreAtLazy dupsRef reduceAt book ridx 2
      return $ Mat val [] [(mget (cidToCtr book) lab, [], cs0), ("_", [], cs1)]

    SWI -> do
      let loc = termLoc term
      let lab = termLab term
      let len = fromIntegral $ mget (cidToLen book) lab
      ridx <- rpush term (1 + len)
      val <- extractCoreAtLazy dupsRef reduceAt book ridx 0
      css <- foldM (\css i -> do
        bod <- extractCoreAtLazy dupsRef reduceAt book ridx (1 + i)
        return $ (show i, [], bod):css) [] [0..len-1]
      return $ Mat val [] (reverse css)

    W32 -> do
      let val = termLoc term
      return $ U32 (fromIntegral val)

    CHR -> do
      let val = termLoc term
      return $ Chr (chr (fromIntegral val))

    OPX -> do
      let loc = termLoc term
      let opr = toEnum (fromIntegral (termLab term))
      ridx <- rpush term 2
      nm0 <- extractCoreAtLazy dupsRef reduceAt book ridx 0
      nm1 <- extractCoreAtLazy dupsRef reduceAt book ridx 1
      return $ Op2 opr nm0 nm1

    OPY -> do
      let loc = termLoc term
      let opr = toEnum (fromIntegral (termLab term))
      ridx <- rpush term 2
      nm0 <- extractCoreAtLazy dupsRef reduceAt book ridx 0
      nm1 <- extractCoreAtLazy dupsRef reduceAt book ridx 1
      return $ Op2 opr nm0 nm1

    REF -> do
      let loc = termLoc term
      let lab = termLab term
      let fid = lab
      let ari = funArity book fid
      let aux = if ari == 0 then [] else [0..ari-1]
      ridx <- rpush term (fromIntegral ari)
      arg <- mapM (\i -> extractCoreAtLazy dupsRef reduceAt book ridx i) aux
      let name = MS.findWithDefault "?" fid (fidToNam book)
      return $ Ref name fid arg

    _ -> do
      return Era

  where
    extractCoreAtLazy dupsRef reduceAt book ridx off = unsafeInterleaveIO $ do
      base <- rtake ridx
      extractCoreAt dupsRef reduceAt book ((termLoc base) + off)

doExtractCoreAt :: ReduceAt -> Book -> Loc -> HVM Core
doExtractCoreAt reduceAt book loc = do
  dupsRef <- newIORef IS.empty
  core    <- extractCoreAt dupsRef reduceAt book loc
  return core
  -- return $ doLiftDups core

-- Lifting Dups
-- ------------

liftDups :: Core -> (Core, Core -> Core)

liftDups (Var nam) =
  (Var nam, id)

liftDups (Ref nam fid arg) =
  let (argT, argD) = liftDupsList arg
  in (Ref nam fid argT, argD)

liftDups Era =
  (Era, id)

liftDups (Lam str bod) =
  let (bodT, bodD) = liftDups bod
  in (Lam str bodT, bodD)

liftDups (App fun arg) =
  let (funT, funD) = liftDups fun
      (argT, argD) = liftDups arg
  in (App funT argT, funD . argD)

liftDups (Sup lab tm0 tm1) =
  let (tm0T, tm0D) = liftDups tm0
      (tm1T, tm1D) = liftDups tm1
  in (Sup lab tm0T tm1T, tm0D . tm1D)

liftDups (Dup lab dp0 dp1 val bod) =
  let (valT, valD) = liftDups val
      (bodT, bodD) = liftDups bod
  in (bodT, \x -> valD (bodD (Dup lab dp0 dp1 valT x)))

liftDups (Ctr nam fds) =
  let (fdsT, fdsD) = liftDupsList fds
  in (Ctr nam fdsT, fdsD)

liftDups (Mat val mov css) =
  let (valT, valD) = liftDups val
      (movT, movD) = liftDupsMov mov
      (cssT, cssD) = liftDupsCss css
  in (Mat valT movT cssT, valD . movD . cssD)

liftDups (U32 val) =
  (U32 val, id)

liftDups (Chr val) =
  (Chr val, id)

liftDups (Op2 opr nm0 nm1) =
  let (nm0T, nm0D) = liftDups nm0
      (nm1T, nm1D) = liftDups nm1
  in (Op2 opr nm0T nm1T, nm0D . nm1D)

liftDups (Let mod nam val bod) =
  let (valT, valD) = liftDups val
      (bodT, bodD) = liftDups bod
  in (Let mod nam valT bodT, valD . bodD)

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

liftDupsCss :: [(String, [String], Core)] -> ([(String, [String], Core)], Core -> Core)

liftDupsCss [] = 
  ([], id)

liftDupsCss ((c,fs,b):xs) =
  let (bT, bD)   = liftDups b
      (xsT, xsD) = liftDupsCss xs
  in ((c,fs,bT):xsT, bD . xsD)

doLiftDups :: Core -> Core
doLiftDups term =
  let (termExpr, termDups) = liftDups term in
  let termBody = termDups (Var "") in
  -- hack to print expr before dups
  Let LAZY "" termExpr termBody
