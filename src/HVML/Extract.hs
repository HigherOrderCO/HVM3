-- //./Type.hs//

module HVML.Extract where

import Control.Monad (foldM)
import Control.Monad.State
import Data.Char (chr, ord)
import Data.Word
import HVML.Show
import HVML.Type
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as MS

import Debug.Trace

type ExtractM a = StateT (IS.IntSet, MS.Map Loc String) HVM a

extractCore :: Book -> Term -> ExtractM Core
extractCore book term = case tagT (termTag term) of
  ERA -> do
    return Era

  LET -> do
    let loc = termLoc term
    let mode = modeT (termLab term)
    val <- lift $ got (loc + 1)
    bod <- lift $ got (loc + 2)
    name <- genName (loc + 0)
    val0 <- extractCore book val
    bod0 <- extractCore book bod
    return $ Let mode name val0 bod0
  
  LAM -> do
    let loc = termLoc term
    bod <- lift $ got (loc + 1)
    name <- genName (loc + 0)
    bod0 <- extractCore book bod
    return $ Lam name bod0
    
  APP -> do
    let loc = termLoc term
    fun <- lift $ got (loc + 0)
    arg <- lift $ got (loc + 1)
    fun0 <- extractCore book fun
    arg0 <- extractCore book arg
    return $ App fun0 arg0
    
  SUP -> do
    let loc = termLoc term
    let lab = termLab term
    tm0 <- lift $ got (loc + 0)
    tm1 <- lift $ got (loc + 1)
    tm00 <- extractCore book tm0
    tm10 <- extractCore book tm1
    return $ Sup lab tm00 tm10
    
  VAR -> do
    let key = termKey term
    sub <- lift $ got key
    if termTag sub == _SUB_
    then do
      name <- genName key
      return $ Var name
    else extractCore book sub
      
  DP0 -> do
    let loc = termLoc term
    let lab = termLab term
    let key = termKey term
    sub <- lift $ got key
    if termTag sub == _SUB_
    then do
      (dups, _) <- get
      if IS.member (fromIntegral loc) dups
      then do
        name <- genName key
        return $ Var name
      else do
        dp0 <- genName (loc + 0)
        dp1 <- genName (loc + 1)
        val <- lift $ got (loc + 2)
        modify $ \x -> (IS.insert (fromIntegral loc) dups, snd x)
        val0 <- extractCore book val
        return $ Dup lab dp0 dp1 val0 (Var dp0)
    else extractCore book sub
      
  DP1 -> do
    let loc = termLoc term
    let lab = termLab term
    let key = termKey term
    sub <- lift $ got key
    if termTag sub == _SUB_
    then do
      (dups, _) <- get
      if IS.member (fromIntegral loc) dups
      then do
        name <- genName key
        return $ Var name
      else do
        dp0 <- genName (loc + 0)
        dp1 <- genName (loc + 1)
        val <- lift $ got (loc + 2)
        modify $ \x -> (IS.insert (fromIntegral loc) dups, snd x)
        val0 <- extractCore book val
        return $ Dup lab dp0 dp1 val0 (Var dp1)
    else extractCore book sub

  UDP -> do
    let loc = termLoc term
    val <- lift $ got loc
    extractCore book $ val

  USP -> do
    let loc = termLoc term
    let lab = termLab term
    tm0 <- lift $ got (loc + 0)
    tm1 <- lift $ got (loc + 1)
    tm00 <- extractCore book tm0
    tm10 <- extractCore book tm1
    return $ USp lab tm00 tm10
      
  CTR -> do
    let loc = termLoc term
    let lab = termLab term
    let cid = u12v2X lab
    let ari = u12v2Y lab
    fds <- if ari == 0
      then return []
      else lift $ mapM (\i -> got (loc + i)) [0..ari-1]
    fds0 <- mapM (extractCore book) fds
    return $ Ctr cid fds0
    
  MAT -> do
    let loc = termLoc term
    let len = termLab term
    val <- lift $ got (loc + 0)
    css <- if len == 0
      then return []
      else lift $ mapM (\i -> got (loc + 1 + i)) [0..len-1]
    val0 <- extractCore book val
    css0 <- mapM (extractCore book) css
    css1 <- mapM (\ cs -> return (0, cs)) css0 -- NOTE: case arity lost on extraction
    return $ Mat val0 css1
    
  W32 -> do
    let val = termLoc term
    return $ U32 (fromIntegral val)
    
  OPX -> do
    let loc = termLoc term
    let opr = toEnum (fromIntegral (termLab term))
    nm0 <- lift $ got (loc + 0)
    nm1 <- lift $ got (loc + 1)
    nm00 <- extractCore book nm0
    nm10 <- extractCore book nm1
    return $ Op2 opr nm00 nm10
    
  OPY -> do
    let loc = termLoc term
    let opr = toEnum (fromIntegral (termLab term))
    nm0 <- lift $ got (loc + 0)
    nm1 <- lift $ got (loc + 1)
    nm00 <- extractCore book nm0
    nm10 <- extractCore book nm1
    return $ Op2 opr nm00 nm10
    
  REF -> do
    let loc = termLoc term
    let lab = termLab term
    let fid = u12v2X lab
    let ari = u12v2Y lab
    arg <- if ari == 0
      then return []
      else lift $ mapM (\i -> got (loc + i)) [0..ari-1]
    arg0 <- mapM (extractCore book) arg
    let name = MS.findWithDefault "?" fid (idToName book)
    return $ Ref name fid arg0
    
  _ -> return Era

doExtractCore :: Book -> Term -> HVM Core
doExtractCore book term = do
  core <- evalStateT (extractCore book term) (IS.empty, MS.empty)
  return $ doLiftDups core

genName :: Loc -> ExtractM String
genName loc = do
  (dups, nameMap) <- get
  case MS.lookup loc nameMap of
    Just name -> do
      return name
    Nothing -> do
      let newName = genNameFromIndex (MS.size nameMap)
      put (dups, MS.insert loc newName nameMap)
      return newName

genNameFromIndex :: Int -> String
genNameFromIndex n = go (n + 1) "" where
  go n ac | n == 0    = ac
          | otherwise = go q (chr (ord 'a' + r) : ac)
          where (q,r) = quotRem (n - 1) 26

-- Lifting Dups
-- ------------

liftDups :: Core -> State (Core -> Core) Core
liftDups (Var nam) = return $ Var nam
liftDups (Ref nam fid arg) = do
  arg <- mapM liftDups arg
  return $ Ref nam fid arg
liftDups Era = return Era
liftDups (Lam str bod) = do
  bod <- liftDups bod
  return $ Lam str bod
liftDups (App fun arg) = do
  fun <- liftDups fun
  arg <- liftDups arg
  return $ App fun arg
liftDups (Sup lab tm0 tm1) = do
  tm0 <- liftDups tm0
  tm1 <- liftDups tm1
  return $ Sup lab tm0 tm1
liftDups (Dup lab dp0 dp1 val bod) = do
  val <- liftDups val
  bod <- liftDups bod
  modify (\oldState k -> oldState (Dup lab dp0 dp1 val k))
  return bod
liftDups (Ctr cid fds) = do
  fds <- mapM liftDups fds
  return $ Ctr cid fds
liftDups (Mat val css) = do
  val <- liftDups val
  css <- mapM (\(ar, cs) -> do
    cs <- liftDups cs
    return (ar, cs)) css
  return $ Mat val css
liftDups (U32 val) = return $ U32 val
liftDups (Op2 opr nm0 nm1) = do
  nm0 <- liftDups nm0
  nm1 <- liftDups nm1
  return $ Op2 opr nm0 nm1
liftDups (Let mod nam val bod) = do
  val <- liftDups val
  bod <- liftDups bod
  return $ Let mod nam val bod
liftDups (USp lab tm0 tm1) = do
  tm0 <- liftDups tm0
  tm1 <- liftDups tm1
  return $ USp lab tm0 tm1
liftDups (UDp lab dp0 val bod) = do
  val <- liftDups val
  bod <- liftDups bod
  modify (\oldState k -> oldState (UDp lab dp0 val k))
  return bod

doLiftDups :: Core -> Core
doLiftDups term =
  let (liftedTerm, finalState) = runState (liftDups term) id
  in finalState liftedTerm
