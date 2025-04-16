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
    t | t == _ERA_ -> do
      return Era

    t | t == _LET_ -> do
      let loc  = termLoc term
      let mode = modeT (termLab term)
      name <- return $ "$" ++ show (loc + 0)
      val  <- extractCoreAt dupsRef reduceAt book (loc + 0)
      bod  <- extractCoreAt dupsRef reduceAt book (loc + 1)
      return $ Let mode name val bod

    t | t == _LAM_ -> do
      let loc = termLoc term
      name <- return $ "$" ++ show (loc + 0)
      bod  <- extractCoreAt dupsRef reduceAt book (loc + 0)
      return $ Lam name bod

    t | t == _APP_ -> do
      let loc = termLoc term
      fun <- extractCoreAt dupsRef reduceAt book (loc + 0)
      arg <- extractCoreAt dupsRef reduceAt book (loc + 1)
      return $ App fun arg

    t | t == _SUP_ -> do
      let loc = termLoc term
      let lab = termLab term
      tm0 <- extractCoreAt dupsRef reduceAt book (loc + 0)
      tm1 <- extractCoreAt dupsRef reduceAt book (loc + 1)
      return $ Sup lab tm0 tm1

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

    t | t == _DP0_ -> do
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
        val <- extractCoreAt dupsRef reduceAt book (loc + 0)
        modifyIORef' dupsRef (IS.insert (fromIntegral loc))
        return $ Dup lab dp0 dp1 val (Var dp0)

    t | t == _DP1_ -> do
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
        val <- extractCoreAt dupsRef reduceAt book (loc + 0)
        modifyIORef' dupsRef (IS.insert (fromIntegral loc))
        return $ Dup lab dp0 dp1 val (Var dp1)

    t | t == _CTR_ -> do
      let loc = termLoc term
      let lab = termLab term
      let cid = fromIntegral lab
      let nam = mget (cidToCtr book) cid
      let ari = mget (cidToAri book) cid
      let ars = if ari == 0 then [] else [0..fromIntegral ari-1]
      fds <- mapM (\i -> extractCoreAt dupsRef reduceAt book (loc + i)) ars
      return $ Ctr nam fds

    t | t == _MAT_ -> do
      let loc = termLoc term
      let lab = termLab term
      let cid = fromIntegral lab
      let len = mget (cidToLen book) cid
      val <- extractCoreAt dupsRef reduceAt book (loc + 0)
      css <- foldM (\css i -> do
        let ctr = mget (cidToCtr book) (cid + i)
        let ari = mget (cidToAri book) (cid + i)
        let fds = if ari == 0 then [] else ["$" ++ show (loc + 1 + j) | j <- [0..fromIntegral ari-1]]
        bod <- extractCoreAt dupsRef reduceAt book (loc + 1 + fromIntegral i)
        return $ (ctr,fds,bod):css) [] [0..len-1]
      return $ Mat val [] (reverse css)

    t | t == _IFL_ -> do
      let loc = termLoc term
      let lab = termLab term
      let cid = fromIntegral lab
      val <- extractCoreAt dupsRef reduceAt book (loc + 0)
      cs0 <- extractCoreAt dupsRef reduceAt book (loc + 1)
      cs1 <- extractCoreAt dupsRef reduceAt book (loc + 2)
      return $ Mat val [] [(mget (cidToCtr book) cid, [], cs0), ("_", [], cs1)]

    t | t == _SWI_ -> do
      let loc = termLoc term
      let lab = termLab term
      let len = fromIntegral lab
      val <- extractCoreAt dupsRef reduceAt book (loc + 0)
      css <- foldM (\css i -> do
        bod <- extractCoreAt dupsRef reduceAt book (loc + 1 + i)
        return $ (show i, [], bod):css) [] [0..len-1]
      return $ Mat val [] (reverse css)

    t | t == _W32_ -> do
      let val = termLoc term
      return $ U32 (fromIntegral val)

    t | t == _CHR_ -> do
      let val = termLoc term
      return $ Chr (chr (fromIntegral val))

    t | t == _OPX_ -> do
      let loc = termLoc term
      let opr = toEnum (fromIntegral (termLab term))
      nmx <- extractCoreAt dupsRef reduceAt book (loc + 0)
      nmy <- extractCoreAt dupsRef reduceAt book (loc + 1)
      return $ Op2 opr nmx nmy

    t | t == _OPY_ -> do
      let loc = termLoc term
      let opr = toEnum (fromIntegral (termLab term))
      nmy <- extractCoreAt dupsRef reduceAt book (loc + 0)
      nmx <- extractCoreAt dupsRef reduceAt book (loc + 1)
      return $ Op2 opr nmx nmy

    t | t == _REF_ -> do
      let loc = termLoc term
      let lab = termLab term
      let fid = fromIntegral lab
      let ari = fromIntegral $ funArity book fid
      let aux = if ari == 0 then [] else [0..ari-1]
      arg <- mapM (\i -> extractCoreAt dupsRef reduceAt book (loc + i)) aux
      let name = MS.findWithDefault "?" fid (fidToNam book)
      return $ Ref name fid arg

    t | t == _FWD_ -> do
      return Era

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


















