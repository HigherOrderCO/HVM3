module HVM.Adjust where

import Control.Monad
import Control.Monad.State
import Data.List (sortOn)
import Data.Word
import HVM.Type
import qualified Data.Map as MS

adjustBook :: Book -> Book
adjustBook book =
  let fns = map (\(fid, ((cp, ars), cr)) -> (fid, ((cp, ars), adjust book cr))) (MS.toList (fidToFun book))
  in book { fidToFun = MS.fromList fns }

adjust :: Book -> Core -> Core
adjust book term =
  let termA = setRefIds (namToFid book) term  in
  let termB = setCtrIds (ctrToCid book) (cidToADT book) termA in
  let termC = sortCases (ctrToCid book) termB in
  let termD = lexify termC in
  termD


-- Adjusters
----------------------

-- Adds the function id to Ref constructors
setRefIds :: MS.Map String Word16 -> Core -> Core
setRefIds fids term = go term
  where
    go :: Core -> Core
    go term = case term of
      Var nam         -> Var nam
      Let m x v b     -> Let m x (go v) (go b)
      Lam x bod       -> Lam x (go bod)
      App f x         -> App (go f) (go x)
      Sup l x y       -> Sup l (go x) (go y)
      Dup l x y v b   -> Dup l x y (go v) (go b)
      Ctr nam fds     -> Ctr nam (map go fds)
      Mat k x mov css -> Mat k (go x) (map (\ (k,v) -> (k, go v)) mov) (map (\ (ctr,fds,cs) -> (ctr, fds, go cs)) css)
      Op2 op x y      -> Op2 op (go x) (go y)
      U32 n           -> U32 n
      Chr c           -> Chr c
      Era             -> Era
      Inc x           -> Inc (go x)
      Dec x           -> Dec (go x)
      Ref nam _ arg   -> Ref nam (mget fids nam) (map go arg)

-- Adds the constructor id to Mat and IFL terms
setCtrIds :: MS.Map String Word16 -> MS.Map Word16 Word16 -> Core -> Core
setCtrIds cids adts term = go term
  where
    go :: Core -> Core
    go term = case term of
      Var nam         -> Var nam
      Let m x v b     -> Let m x (go v) (go b)
      Lam x bod       -> Lam x (go bod)
      App f x         -> App (go f) (go x)
      Sup l x y       -> Sup l (go x) (go y)
      Dup l x y v b   -> Dup l x y (go v) (go b)
      Ctr nam fds     -> Ctr nam (map go fds)
      Mat k x mov css -> 
        let k' = case k of
                  SWI   ->
                    SWI
                  MAT _ ->
                    let (ctr, _, _) = head css in
                    let cid = mget cids ctr in
                    let adt = mget adts cid in
                    MAT adt
                  IFL _ ->
                    let (ctr, _, _) = head css in
                    let cid = mget cids ctr in
                    IFL cid
                  _ -> k in
        let mov' = map (\(k,v) -> (k, go v)) mov in
        let css' = map (\(ctr,fds,cs) -> (ctr, fds, go cs)) css in
        Mat k' (go x) mov' css'
      Op2 op x y      -> Op2 op (go x) (go y)
      U32 n           -> U32 n
      Chr c           -> Chr c
      Era             -> Era
      Inc x           -> Inc (go x)
      Dec x           -> Dec (go x)
      Ref nam fid arg -> Ref nam fid (map go arg)

-- Sorts match cases by constructor ID or numeric value
sortCases :: MS.Map String Word16 -> Core -> Core
sortCases cids term = go term
  where
    go :: Core -> Core
    go term = case term of
      Var nam         -> Var nam
      Let m x v b     -> Let m x (go v) (go b)
      Lam x bod       -> Lam x (go bod)
      App f x         -> App (go f) (go x)
      Sup l x y       -> Sup l (go x) (go y)
      Dup l x y v b   -> Dup l x y (go v) (go b)
      Ctr nam fds     -> Ctr nam (map go fds)
      Mat k x mov css ->
        let sort = sortOn sortKey css in
        let css' = map (\(ctr, fds, bod) -> (ctr, fds, go bod)) sort in
        let mov' = map (\(k,v) -> (k, go v)) mov in
        Mat k (go x) mov' css'
      Op2 op x y      -> Op2 op (go x) (go y)
      U32 n           -> U32 n
      Chr c           -> Chr c
      Era             -> Era
      Inc x           -> Inc (go x)
      Dec x           -> Dec (go x)
      Ref nam fid arg -> Ref nam fid (map go arg)

    sortKey :: (String, [String], Core) -> Word16
    sortKey (name, _, _) =
      case name of
        ('#':_) -> case MS.lookup name cids of
          Nothing -> maxBound
          Just id -> id
        _ -> case reads name of
          [(num :: Word16, "")] -> num
          _                     -> maxBound

-- Gives unique names to lexically scoped vars, unless they start with '$'.
-- Example: `λx λt (t λx(x) x)` will read as `λx0 λt1 (t1 λx2(x2) x0)`.
lexify :: Core -> Core
lexify term = evalState (go term MS.empty) 0 where
  fresh :: String -> State Int String
  fresh nam@('$':_) = return $ nam
  fresh nam         = do i <- get; put (i+1); return $ nam++"$"++show i

  extend :: String -> String -> MS.Map String String -> State Int (MS.Map String String)
  extend old@('$':_) new ctx = return $ ctx
  extend old         new ctx = return $ MS.insert old new ctx

  go :: Core -> MS.Map String String -> State Int Core
  go term ctx = case term of
    Var nam -> 
      return $ Var (MS.findWithDefault nam nam ctx)
    Ref nam fid arg -> do
      arg <- mapM (\x -> go x ctx) arg
      return $ Ref nam fid arg
    Let mod nam val bod -> do
      val  <- go val ctx
      nam' <- fresh nam
      ctx  <- extend nam nam' ctx
      bod  <- go bod ctx
      return $ Let mod nam' val bod
    Lam nam bod -> do
      nam' <- fresh nam
      ctx  <- extend nam nam' ctx
      bod  <- go bod ctx
      return $ Lam nam' bod
    App fun arg -> do
      fun <- go fun ctx
      arg <- go arg ctx
      return $ App fun arg
    Sup lab tm0 tm1 -> do
      tm0 <- go tm0 ctx
      tm1 <- go tm1 ctx
      return $ Sup lab tm0 tm1
    Dup lab dp0 dp1 val bod -> do
      val  <- go val ctx
      dp0' <- fresh dp0
      dp1' <- fresh dp1
      ctx  <- extend dp0 dp0' ctx
      ctx  <- extend dp1 dp1' ctx
      bod  <- go bod ctx
      return $ Dup lab dp0' dp1' val bod
    Ctr nam fds -> do
      fds <- mapM (\x -> go x ctx) fds
      return $ Ctr nam fds
    Mat kin val mov css -> do
      val' <- go val ctx
      mov' <- forM mov $ \ (k,v) -> do
        k' <- fresh k
        v  <- go v ctx
        return $ (k', v)
      css' <- forM css $ \ (ctr,fds,bod) -> do
        fds' <- mapM fresh fds
        ctx  <- foldM (\ ctx (fd,fd') -> extend fd fd' ctx) ctx (zip fds fds')
        ctx  <- foldM (\ ctx ((k,_),(k',_)) -> extend k k' ctx) ctx (zip mov mov')
        bod <- go bod ctx
        return (ctr, fds', bod)
      return $ Mat kin val' mov' css'
    Op2 op nm0 nm1 -> do
      nm0 <- go nm0 ctx
      nm1 <- go nm1 ctx
      return $ Op2 op nm0 nm1
    U32 n -> 
      return $ U32 n
    Chr c ->
      return $ Chr c
    Era -> 
      return Era
    Inc x -> do
      x <- go x ctx
      return $ Inc x
    Dec x -> do
      x <- go x ctx
      return $ Dec x
