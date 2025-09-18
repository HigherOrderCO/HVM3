module HVM.Adjust where

import Control.Monad
import Control.Monad.State
import Data.List (sortOn)
import Data.Word
import HVM.Type
import qualified Data.Map as MS
import Debug.Trace (trace)

-- External API
----------------

adjustBook :: Book -> Book
adjustBook book = foldr adjustFunc book (MS.toList (fidToFun book))

adjustFunc :: (Word16, Func) -> Book -> Book
adjustFunc (fid, ((cp, ars), cr)) book =
  let nam       = mget (fidToNam book) fid in
  let (b', cr') = adjust nam book cr (map snd ars) in
  let ars'      = map (\(s, n) -> (s, stripName n)) ars in
  b' { fidToFun = MS.insert fid ((cp, ars'), cr') (fidToFun b') }

adjust :: Name -> Book -> Core -> [String] -> (Book, Core)
adjust orig book term binds =
  let termA       = setRefIds (namToFid book) term
      termB       = setCtrIds (ctrToCid book) (cidToADT book) termA
      termC       = sortCases (ctrToCid book) termB
      (fr, termD) = insertDups (freshLab book) binds termC
      termE       = lexify termD
      termF       = validate orig book termE
  in (book { freshLab = fr }, termF)


-- Adjusters
-------------

-- Adds the function id to Ref constructors
setRefIds :: MS.Map String Word16 -> Core -> Core
setRefIds fids term = go term
  where
    go :: Core -> Core
    go (Var nam)         = Var nam
    go (Let m x v b)     = Let m x (go v) (go b)
    go (Lam x bod)       = Lam x (go bod)
    go (App f x)         = App (go f) (go x)
    go (Sup l x y)       = Sup l (go x) (go y)
    go (Dup l x y v b)   = Dup l x y (go v) (go b)
    go (Ctr nam fds)     = Ctr nam (map go fds)
    go (Mat k x mov css) = Mat k (go x) (map (\ (k,v) -> (k, go v)) mov) (map (\ (ctr,fds,cs) -> (ctr, fds, go cs)) css)
    go (Op2 op x y)      = Op2 op (go x) (go y)
    go (U32 n)           = U32 n
    go (Chr c)           = Chr c
    go Era               = Era
    go (Inc x)           = Inc (go x)
    go (Dec x)           = Dec (go x)
    go (Ref nam fid arg) =
      case MS.lookup nam fids of
        Just fid -> Ref nam fid (map go arg)
        Nothing  -> error $ "Unknown function: " ++ show nam


-- Adds the constructor id to Mat and IFL terms
setCtrIds :: MS.Map String Word16 -> MS.Map Word16 Word16 -> Core -> Core
setCtrIds cids adts term = go term
  where
    go :: Core -> Core
    go (Var nam)         = Var nam
    go (Let m x v b)     = Let m x (go v) (go b)
    go (Lam x bod)       = Lam x (go bod)
    go (App f x)         = App (go f) (go x)
    go (Sup l x y)       = Sup l (go x) (go y)
    go (Dup l x y v b)   = Dup l x y (go v) (go b)
    go (Ctr nam fds)     = Ctr nam (map go fds)
    go (Mat k x mov css) = Mat k' (go x) mov' css' where
      getCtr (ctr, _, _) = ctr
      mov' = map (\(k,v) -> (k, go v)) mov
      css' = map (\(ctr,fds,cs) -> (ctr, fds, go cs)) css
      k'   = case k of
        SWI   -> SWI
        MAT _ -> MAT (mget adts (mget cids (getCtr (head css))))
        IFL _ -> IFL (mget cids (getCtr (head css)))
        _     -> k
    go (Op2 op x y)      = Op2 op (go x) (go y)
    go (U32 n)           = U32 n
    go (Chr c)           = Chr c
    go Era               = Era
    go (Inc x)           = Inc (go x)
    go (Dec x)           = Dec (go x)
    go (Ref nam fid arg) = Ref nam fid (map go arg)


-- Sorts match cases by constructor ID or numeric value
sortCases :: MS.Map String Word16 -> Core -> Core
sortCases cids term = go term
  where
    go :: Core -> Core
    go (Var nam)         = Var nam
    go (Let m x v b)     = Let m x (go v) (go b)
    go (Lam x bod)       = Lam x (go bod)
    go (App f x)         = App (go f) (go x)
    go (Sup l x y)       = Sup l (go x) (go y)
    go (Dup l x y v b)   = Dup l x y (go v) (go b)
    go (Ctr nam fds)     = Ctr nam (map go fds)
    go (Mat k x mov css) = Mat k (go x) mov' css' where
      mov' = map (\(k,v) -> (k, go v)) mov
      css' = map (\(ctr,fds,bod) -> (ctr, fds, go bod)) sort
      sort = sortOn sortKey css
      sortKey (name, _, _) =
        case name of
          ('#':_) -> case MS.lookup name cids of
            Nothing -> maxBound
            Just id -> id
          _ -> case reads name of
            [(num :: Word16, "")] -> num
            _                     -> maxBound
    go (Op2 op x y)      = Op2 op (go x) (go y)
    go (U32 n)           = U32 n
    go (Chr c)           = Chr c
    go Era               = Era
    go (Inc x)           = Inc (go x)
    go (Dec x)           = Dec (go x)
    go (Ref nam fid arg) = Ref nam fid (map go arg)


-- Inserts Dup nodes for vars that have been used more than once.
-- Renames vars according to the new Dup bindings.
-- Gives fresh labels to the new Dup nodes.
insertDups :: Lab -> [String] -> Core -> (Lab, Core)
insertDups fresh binds term =
  let (term', (fresh', _)) = runState (withBinds binds term) (fresh, MS.empty)
  in (fresh', term')
  where
    go :: Core -> State (Lab, MS.Map String [String]) Core
    go (Var nam)         = do
      nam <- useVar nam
      return $ (Var nam)
    go (Let m x v b)     = do
      v <- go v
      b <- withBinds [x] b
      return $ Let m (stripName x) v b
    go (Lam x bod)       = do
      bod <- withBinds [x] bod
      return $ Lam (stripName x) bod
    go (App fun arg)     = do
      fun <- go fun
      arg <- go arg
      return $ App fun arg
    go (Sup lab tm0 tm1) = do
      tm0 <- go tm0
      tm1 <- go tm1
      return $ Sup lab tm0 tm1
    go (Dup lab x y v b) = do
      v <- go v
      b <- withBinds [x, y] b
      return $ Dup lab (stripName x) (stripName y) v b
    go (Ctr nam fds)     = do
      fds <- mapM go fds
      return $ Ctr nam fds
    go (Mat k x mov css) = do
      x   <- go x
      mov <- forM mov (\(k,v) -> do
        v <- go v
        return (k, v))
      css <- forM css (\(ctr,fds,bod) -> do
        bod <- withBinds ((map fst mov) ++ fds) bod
        return (ctr, map stripName fds, bod))
      let mov' = map (\(k,v) -> (stripName k, v)) mov
      return $ Mat k x mov' css
    go (Op2 op x y)      = do
      x <- go x
      y <- go y
      return $ Op2 op x y
    go (U32 n)           = do
      return $ U32 n
    go (Chr c)           = do
      return $ Chr c
    go Era               = do
      return Era
    go (Inc x)           = do
      x <- go x
      return $ Inc x
    go (Dec x)           = do
      x <- go x
      return $ Dec x
    go (Ref nam fid arg) = do
      arg <- mapM go arg
      return $ Ref nam fid arg

    -- Recurses on the body of a term that binds variables.
    -- Adds Dups if the new vars are used more than once.
    withBinds :: [String] -> Core -> State (Lab, MS.Map String [String]) Core
    withBinds vars term = do
      (lab, prev) <- get
      -- Add the new binds
      let bfor = foldr addVar prev vars
      put (lab, bfor)
      term <- go term
      term <- foldM applyDups term vars
      -- Remove the new binds
      (lab, aftr) <- get
      let next = foldr (restoreVar prev) (foldr remVar aftr vars) vars
      put (lab, next)
      return term
      where
        addVar var uses = MS.insert (stripName var) [] uses
        remVar var uses = MS.delete (stripName var) uses
        restoreVar old var new = 
          case MS.lookup (stripName var) old of
            Just val -> MS.insert (stripName var) val new
            Nothing  -> new

    applyDups :: Core -> String -> State (Lab, MS.Map String [String]) Core
    applyDups body var = do
      (_, uses) <- get
      let vUse = mget uses (stripName var)
      when ((head var /= '&') && (length vUse > 1)) $
        error $ "Linear variable " ++ show var ++ " used " ++ show (length vUse) ++ " times"
      case (reverse vUse) of
        [] -> do
          return body
        [_] -> do
          return body
        (name:dups) -> do
          foldM (\acc currName -> do
            label <- genFresh
            return $ Dup label name currName (Var name) acc) body dups

    genFresh :: State (Lab, MS.Map String [String]) Lab
    genFresh = do
      (lab, _) <- get
      when (lab > 0x7FFF) $ do
        error "Label overflow: generated label would be too large"
      modify (\(lab, uses) -> (lab + 1, uses))
      return $ 0x8000 + lab

    useVar :: String -> State (Lab, MS.Map String [String]) String
    useVar nam@('$':_) = do
      return nam
    useVar nam = do
      (_, uses) <- get
      case mget uses nam of
        [] -> do
          modify (\(lab, uses) -> (lab, MS.insert nam [nam] uses))
          return nam
        vUse -> do
          let dupNam = nam ++ "$dup" ++ show (length vUse)
          modify (\(lab, uses) -> (lab, MS.insert nam (dupNam : vUse) uses))
          return dupNam

-- Strip the & prefix from a non-linear variable name
-- e.g., "&x" -> "x", "x" -> "x"
stripName :: String -> String
stripName ('&':nam) = nam
stripName nam       = nam


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

validate :: Name -> Book -> Core -> Core
validate orig book term = go term where
  go :: Core -> Core
  go (Var nam)         = Var nam
  go (Let m x v b)     = Let m x (go v) (go b)
  go (Lam x bod)       = Lam x (go bod)
  go (App f x)         = App (go f) (go x)
  go (Sup l x y)       = Sup l (go x) (go y)
  go (Dup l x y v b)   = Dup l x y (go v) (go b)
  go (Ctr nam fds)     =
    case MS.lookup nam (ctrToCid book) of
      Nothing ->
        error $ header ++ "Unknown constructor: " ++ show nam
      Just cid ->
        if length fds /= fromIntegral (mget (cidToAri book) cid) then
          error $ header ++ "Arity mismatch on Ctr: " ++ show (Ctr nam fds) ++ ". " ++ "Expected " ++ show (mget (cidToAri book) cid) ++ " arguments, got " ++ show (length fds)
        else
          Ctr nam (map go fds)
  go (Mat k x mov css) =
    if not uniqueCss then error $ header ++ "Duplicate match case: " ++ show (Mat k x mov css) ++ "."
    else Mat k (go x) mov' css'
    where
      mov' = map (\(k,v) -> (k, go v)) mov
      css' = map (\(ctr,fds,bod) -> (ctr, fds, go bod)) css
      ctrs = map (\(ctr, _, _) -> ctr) css
      uniqueCss = null (filter (\ctr -> length (filter (== ctr) ctrs) > 1) ctrs)
  go (Op2 op x y)      = Op2 op (go x) (go y)
  go (U32 n)           = U32 n
  go (Chr c)           = Chr c
  go Era               = Era
  go (Inc x)           = Inc (go x)
  go (Dec x)           = Dec (go x)
  go (Ref nam fid arg) =
    if not ariOk then
      error $ header ++ "Arity mismatch on Ref: " ++ show (Ref nam fid arg) ++ ". " ++ "Expected " ++ show (funArity book fid) ++ " arguments, got " ++ show (length arg)
    else
      Ref nam fid (map go arg)
    where
      ariOk = length arg == fromIntegral (funArity book fid)

  header = if null orig then "" else "In function @" ++ orig ++ ": "
