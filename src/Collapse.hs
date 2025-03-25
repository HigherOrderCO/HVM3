module Collapse where

import Control.Monad (forM, forM_)
import Control.Monad.IO.Class
import Data.Char (chr, ord)
import Data.Word
import Extract
import Reduce
import Foreign
import Show
import Type
import System.IO.Unsafe (unsafeInterleaveIO)
import qualified Data.Map.Strict as MS

-- ~ * { K0 K1 K2 ... }
-- --------------------- MAT-ERA
-- *
icEraMat :: Term -> Term -> HVM Term
icEraMat mat _ = do
  let eraTerm = termNew _ERA_ 0 0
  return eraTerm

-- #C{* K0 K1}
-- -------------- CTR-ERA (if already in normal form)
-- *
icEraCtr :: Term -> Term -> HVM Term
icEraCtr ctr _ = do
  let eraTerm = termNew _ERA_ 0 0
  return eraTerm

-- λx.*
-- ------ ERA-LAM
-- x <- *
-- *
icEraLam :: Term -> Term -> HVM Term
icEraLam lam _ = do
  let lamLoc = termLoc lam
  let eraTerm = termNew _ERA_ 0 0
  set lamLoc (termSetBit eraTerm)
  return eraTerm

-- (f *)
-- ------ ERA-APP
-- *
icEraApp :: Term -> Term -> HVM Term
icEraApp app era = do
  return (termNew _ERA_ 0 0)

-- !&L{r,s} = *;
-- K
-- -------------- DUP-ERA
-- r <- *
-- s <- *
-- K
icDupEra :: Term -> Term -> HVM Term
icDupEra dup era = do
  let dupLoc = termLoc dup
  let dupTag = termTag dup
  let isCo0 = (dupTag == _DP0_)

  let eraTerm = termNew _ERA_ 0 0

  set dupLoc (termSetBit eraTerm)
  return eraTerm

-- λx.&L{f0,f1}
-- ----------------- SUP-LAM
-- x <- &L{x0,x1}
-- &L{λx0.f0,λx1.f1}
icSupLam :: Book -> Term -> Term -> HVM Term
icSupLam book lam sup = do
  let lamLoc = termLoc lam
  let supLoc = termLoc sup

  let supLab = termLab sup

  f0 <- got (supLoc + 0)
  f1 <- got (supLoc + 1)

  lam0Loc <- allocNode 1
  lam1Loc <- allocNode 1

  set lam0Loc f0
  set lam1Loc f1

  let x0 = termNew _VAR_ 0 lam0Loc
  let x1 = termNew _VAR_ 0 lam1Loc

  newSupLoc <- allocNode 2
  set (newSupLoc + 0) x0
  set (newSupLoc + 1) f1

  let newSup = termNew _SUP_ supLab newSupLoc

  set lamLoc (termSetBit newSup)

  let lam0Term = termNew _LAM_ 0 lam0Loc
  let lam1Term = termNew _LAM_ 0 lam1Loc

  resultSupLoc <- allocNode 2
  set (resultSupLoc + 0) lam0Term
  set (resultSupLoc + 1) lam1Term

  return (termNew _SUP_ supLab resultSupLoc)

-- (f &L{x0,x1})
-- ------------------- SUP-APP
-- !&L{f0,f1} = f
-- &L{(f0 x0),(f1 x1)}
icSupApp :: Term -> Term -> HVM Term
icSupApp app sup = do
  let appLoc = termLoc app
  let supLoc = termLoc sup
  let supLab = termLab sup

  fun <- got (appLoc + 0)
  lft <- got (supLoc + 0)
  rgt <- got (supLoc + 1)

  dupLoc <- allocNode 1
  set dupLoc fun

  let f0 = termNew _DP0_ supLab dupLoc
  let f1 = termNew _DP1_ supLab dupLoc

  app0Loc <- allocNode 2
  set (app0Loc + 0) f0
  set (app0Loc + 1) lft
  let app0 = termNew _APP_ 0 app0Loc

  app1Loc <- allocNode 2
  set (app1Loc + 0) f1
  set (app1Loc + 1) rgt
  let app1 = termNew _APP_ 0 app1Loc

  resultSupLoc <- allocNode 2 
  set (resultSupLoc + 0) app0
  set (resultSupLoc + 1) app1
  return (termNew _SUP_ supLab resultSupLoc)

-- &R{&L{x0,x1},y}
-- ----------------------- SUP-SUP-X (if R>L)
-- !&R{y0,y1} = y;
-- &L{&R{x0,y0},&R{x1,y1}}
icSupSupX :: Term -> Term -> HVM Term
icSupSupX outerSup innerSup = do
  let outerLoc = termLoc outerSup 
  let outerLab = termLab outerSup
  let innerLoc = termLoc innerSup
  let innerLab = termLab innerSup

  x0 <- got (innerLoc + 0)
  x1 <- got (innerLoc + 1)
  y  <- got (outerLoc + 1)

  dupLoc <- allocNode 1
  set dupLoc y

  let y0 = termNew _DP0_ outerLab dupLoc
  let y1 = termNew _DP1_ outerLab dupLoc

  sup0Loc <- allocNode 2
  set (sup0Loc + 0) x0
  set (sup0Loc + 1) y0
  let sup0 = termNew _SUP_ outerLab sup0Loc

  sup1Loc <- allocNode 2
  set (sup1Loc + 0) x1
  set (sup1Loc + 1) y1
  let sup1 = termNew _SUP_ outerLab sup1Loc

  resultSupLoc <- allocNode 2
  set (resultSupLoc + 0) sup0
  set (resultSupLoc + 1) sup1
  return (termNew _SUP_ innerLab resultSupLoc)

-- &R{x,&L{y0,y1}}
-- ----------------------- SUP-SUP-Y (if R>L)
-- !&R{x0,x1} = x;
-- &L{&R{x0,y0},&R{x1,y1}}
icSupSupY :: Term -> Term -> HVM Term
icSupSupY outerSup innerSup = do
  let outerLoc = termLoc outerSup
  let outerLab = termLab outerSup
  let innerLoc = termLoc innerSup
  let innerLab = termLab innerSup
  
  x  <- got (outerLoc + 0)
  y0 <- got (innerLoc + 0)
  y1 <- got (innerLoc + 1)

  dupLoc <- allocNode 1
  set dupLoc x

  let x0 = termNew _DP0_ outerLab dupLoc
  let x1 = termNew _DP1_ outerLab dupLoc

  sup0Loc <- allocNode 2
  set (sup0Loc + 0) x0
  set (sup0Loc + 1) y0
  let sup0 = termNew _SUP_ outerLab sup0Loc

  sup1Loc <- allocNode 2
  set (sup1Loc + 0) x1
  set (sup1Loc + 1) y1
  let sup1 = termNew _SUP_ outerLab sup1Loc

  resultSupLoc <- allocNode 2
  set (resultSupLoc + 0) sup0
  set (resultSupLoc + 1) sup1
  return (termNew _SUP_ innerLab resultSupLoc)
  

-- !&L{x0,x1} = x; K
-- ----------------- DUP-VAR
-- x0 <- x
-- x1 <- x
-- K
icDupVar :: Term -> Term -> HVM Term
icDupVar dup var = do
  let dupLoc = termLoc dup
  set dupLoc (termSetBit var)
  return var

-- !&L{a0,a1} = (f x); K
-- --------------------- DUP-APP
-- a0 <- (f0 x0)
-- a1 <- (f1 x1)
-- !&L{f0,f1} = f;
-- !&L{x0,x1} = x;
-- K
icDupApp :: Term -> Term -> HVM Term
icDupApp dup app = do
  let dupLoc = termLoc dup 
  let lab = termLab dup
  let tag = termTag dup
  let isCo0 = (tag == _DP0_)

  let appLoc = termLoc app
  fun <- got (appLoc + 0)
  arg <- got (appLoc + 1)

  dupFunLoc <- allocNode 1
  set dupFunLoc fun
  dupArgLoc <- allocNode 1
  set dupArgLoc arg

  let f0 = termNew _DP0_ lab dupFunLoc
  let f1 = termNew _DP1_ lab dupFunLoc

  let x0 = termNew _DP0_ lab dupArgLoc
  let x1 = termNew _DP1_ lab dupArgLoc

  app0Loc <- allocNode 2
  set (app0Loc + 0) f0
  set (app0Loc + 1) x0
  let app0 = termNew _APP_ 0 app0Loc

  app1Loc <- allocNode 2
  set (app1Loc + 0) f1
  set (app1Loc + 1) x1
  let app1 = termNew _APP_ 0 app1Loc

  if isCo0 
  then do
    set dupLoc (termSetBit app1)
    return app0
  else do
    set dupLoc (termSetBit app0)
    return app1

-- #CTR {&L{x0 x1} k0 k1 ... kn}
-- --------------------------- SUP-CTR
-- !&L{CTR0 CTR1} = CTR
-- !&L{k00, k01}=k0
-- !&L{k10, k11}=k1
-- !&L{kn0, kn1}=kn
-- &L{#CTR0{x0 k00 k10 ... kn0} #CTR1{x1 k01 k11 kn1}}
icSupCtr :: Term -> Term -> Lab -> Loc -> Loc -> HVM Term
icSupCtr ctr sup cid len idx = do
  -- Extract locations and label
  let ctrLoc = termLoc ctr
  let supLoc = termLoc sup
  let supLab = termLab sup

  -- Get superposition components
  x0 <- got (supLoc + 0)
  x1 <- got (supLoc + 1)

  -- Duplicate all arguments except the one at idx
  aList <- forM [0 .. len - 1] $ \j -> do
    if j == idx then
      return Nothing  -- Don’t duplicate the superposition
    else do
      aj <- got (ctrLoc + j)              -- Get argument aj
      dupAjLoc <- allocNode 1             -- Allocate space for duplication
      set dupAjLoc aj                  -- Store aj
      let aj0 = termNew _DP0_ supLab dupAjLoc  -- First duplicate
      let aj1 = termNew _DP1_ supLab dupAjLoc  -- Second duplicate
      return (Just (aj0, aj1))            -- Return pair (aj0, aj1)

  -- Create CTR0: #CTR{a0' a1' ... x0 ... a_{len-1}'}
  ctr0Loc <- allocNode len
  forM_ [0 .. len - 1] $ \j -> do
    if j == idx then
      set (ctr0Loc + j) x0             -- Use x0 at idx
    else do
      let Just (aj0, _) = aList !! fromIntegral j  -- Get aj0
      set (ctr0Loc + j) aj0            -- Use duplicate aj0
  let ctr0 = termNew _CTR_ cid ctr0Loc    -- Construct CTR0

  -- Create CTR1: #CTR{a0'' a1'' ... x1 ... a_{len-1}''}
  ctr1Loc <- allocNode len
  forM_ [0 .. len - 1] $ \j -> do
    if j == idx then
      set (ctr1Loc + j) x1             -- Use x1 at idx
    else do
      let Just (_, aj1) = aList !! fromIntegral j  -- Get aj1
      set (ctr1Loc + j) aj1            -- Use duplicate aj1
  let ctr1 = termNew _CTR_ cid ctr1Loc    -- Construct CTR1

  -- Create resulting superposition: &L{CTR0 CTR1}
  supResultLoc <- allocNode 2
  set (supResultLoc + 0) ctr0
  set (supResultLoc + 1) ctr1
  let supResult = termNew _SUP_ supLab supResultLoc

  -- Return the new superposition
  return supResult

-- ~N{#A: &L{z0 z1} #B: x #C: y ...}
-- --------------------------------- SUP-MAT-CTR
-- !&L{N0,N1} = N;
-- !&L{x0,x1} = x;
-- !&L{y0,y1} = y;
-- &L{~N0{#A:z0 #B: x0 #C: y0} ~N1{#A:z1 #B: x1 #C: y1}}
icSupMatCtr :: Tag -> Term -> Term -> Loc -> Loc -> HVM Term
icSupMatCtr matTag mat sup matLen supIdx = do
  let matLoc = termLoc mat
  let matLab = termLab mat
  let supLab = termLab sup
  let supLoc = termLoc sup
  n <- got (matLoc + 0)
  dupNLoc <- allocNode 1
  set dupNLoc n
  let n0 = termNew _DP0_ supLab dupNLoc
  let n1 = termNew _DP1_ supLab dupNLoc
  z0 <- got (supLoc + 0)
  z1 <- got (supLoc + 1)
  uList <- forM [1 .. matLen] $ \j -> do
    let caseOffset = matLoc + j
    if j == supIdx then return (z0, z1)
    else do
      tj <- got caseOffset
      dupTjLoc <- allocNode 1
      set dupTjLoc tj
      let t0j = termNew _DP0_ supLab dupTjLoc
      let t1j = termNew _DP1_ supLab dupTjLoc
      return (t0j, t1j)
  mat0Loc <- allocNode (matLen + 1)
  set (mat0Loc + 0) n0
  forM_ [1 .. matLen] $ \j -> do
    let (u_j, _) = uList !! fromIntegral (j - 1)
    set (mat0Loc + 1 + (j - 1)) u_j
  let mat0 = termNew matTag matLab mat0Loc
  mat1Loc <- allocNode (matLen + 1)
  set (mat1Loc + 0) n1
  forM_ [1 .. matLen] $ \j -> do
    let (_, v_j) = uList !! fromIntegral (j - 1)
    set (mat1Loc + 1 + (j - 1)) v_j
  let mat1 = termNew matTag matLab mat1Loc
  supLoc <- allocNode 2
  set (supLoc + 0) mat0
  set (supLoc + 1) mat1
  return (termNew _SUP_ supLab supLoc)

isDup :: TAG -> Bool
isDup tag =
  case tag of
    DP0 -> True
    DP1 -> True
    _   -> False

isEra :: Term -> Bool
isEra term = termTag term == _ERA_

isEraC :: Core -> Bool
isEraC Era = True
isEraC _   = False

isSup :: Term -> Bool
isSup term = termTag term == _SUP_

isSupC :: Core -> Bool
isSupC (Sup _ _ _) = True
isSupC _           = False



-- Dup Collapser
-- -------------

collapseDupsAt :: Book -> Loc -> HVM Core
collapseDupsAt book host = unsafeInterleaveIO $ do
  t <- got host
  term <- reduceC t

  let tag = termTag term
  let loc = termLoc term
  let lab = termLab term
  case tagT tag of
    ERA -> return Era

    LET -> do
      let mode = modeT lab
      name <- return $ "$" ++ show (loc + 0)
      val0 <- collapseDupsAt book (loc + 1)
      bod0 <- collapseDupsAt book (loc + 2)
      return $ Let mode name val0 bod0

    LAM -> do 
      bod <- got (loc + 0)
      case (tagT (termTag bod)) of
        SUP -> do
          -- trm <- icSupLam book term bod
          -- set host trm
          -- collapseDupsAt book host

          name <- return $ "$" ++ show (loc + 0)
          bod0 <- collapseDupsAt book (loc + 0)
          return $ Lam lab name bod0
        _   ->  do 
          name <- return $ "$" ++ show (loc + 0)
          bod0 <- collapseDupsAt book (loc + 0)
          return $ Lam lab name bod0

    APP -> do
      fun <- got (loc + 0)
      arg <- got (loc + 1)
      if (tagT (termTag arg)) == SUP then do
        trm <- icSupApp term arg
        set host trm
        collapseDupsAt book host
      else do
        fun0 <- collapseDupsAt book (loc + 0)
        arg0 <- collapseDupsAt book (loc + 1)
        return $ App lab fun0 arg0
     
    SUP -> do
      -- core <- doExtractCoreAt book host
      tm00 <- collapseDupsAt book (loc + 0)
      tm11 <- collapseDupsAt book (loc + 1)
      return $ Sup lab tm00 tm11

    VAR -> do
      sub <- got loc
      if termGetBit sub /= 0 then do
        set (loc + 0) (termRemBit sub)
        collapseDupsAt book (loc + 0)
      else do
        name <- return $ "$" ++ show loc
        return $ Var name

    DP0 -> do
      sb0 <- got (loc + 0)
      if termGetBit sb0 /= 0 then do
        set (loc + 0) (termRemBit sb0)
        collapseDupsAt book (loc + 0)
      else
        collapseDupsAt book (loc + 0)

    DP1 -> do
      sb1 <- got (loc + 0)
      if termGetBit sb1 /= 0 then do
        set (loc + 0) (termRemBit sb1)
        collapseDupsAt book (loc + 0)
      else
        collapseDupsAt book (loc + 0)


    CTR -> do
      let cid = fromIntegral lab
      let nam = MS.findWithDefault "?" cid (cidToCtr book)
      let ari = mget (cidToAri book) cid
      let aux = if ari == 0 then [] else [0 .. ari-1]
      fds0 <- forM aux $ \i -> do
        field <- got (loc + fromIntegral i) 
        case (tagT (termTag field)) of
          ERA -> do
            trm <- icEraCtr term field
            set host trm
            collapseDupsAt book host
          SUP -> do
            trm <- icSupCtr term field (fromIntegral cid) (fromIntegral ari) (fromIntegral i)
            set host trm
            collapseDupsAt book host
          _ -> do 
            collapseDupsAt book (loc + fromIntegral i)

      let result = case filter isEraC fds0 of
                    (era:_) -> Era  -- If ERA exists, return Era
                    [] -> case filter isSupC fds0 of
                           (sup:_) -> sup  -- If no ERA but SUP exists, return SUP
                           [] -> Ctr nam fds0  -- Otherwise, return Ctr
      return $ result


    MAT -> do
      let cid = fromIntegral lab
      let len = fromIntegral $ mget (cidToLen book) cid
      val0 <- collapseDupsAt book (loc + 0)
      css0 <- forM [0..len-1] $ \i -> do
        let ctr = mget (cidToCtr book) (cid + i)
        let ari = fromIntegral $ mget (cidToAri book) (cid + i)
        let fds = if ari == 0 then [] else ["$" ++ show (loc + 1 + j) | j <- [0..ari-1]]
        bod0 <- collapseDupsAt book (loc + 1 + fromIntegral i)
        return (ctr, fds, bod0)
      return $ Mat val0 [] css0

    IFL -> do
      let cid = fromIntegral lab
      val0 <- collapseDupsAt book (loc + 0)
      cs00 <- collapseDupsAt book (loc + 1)
      cs10 <- collapseDupsAt book (loc + 2)
      return $ Mat val0 [] [(mget (cidToCtr book) cid, [], cs00), ("_", [], cs10)]

    SWI -> do
      let len = fromIntegral lab
      val0 <- collapseDupsAt book (loc + 0)
      css0 <- forM [0..len-1] $ \i -> do
        bod0 <- collapseDupsAt book (loc + 1 + i)
        return (show i, [], bod0)
      return $ Mat val0 [] css0

    W32 -> do
      return $ U32 (fromIntegral loc)

    CHR -> do
      return $ Chr (chr (fromIntegral loc))

    OPX -> do
      let opr = toEnum (fromIntegral lab)
      nm00 <- collapseDupsAt book (loc + 0)
      nm10 <- collapseDupsAt book (loc + 1)
      return $ Op2 opr nm00 nm10

    OPY -> do
      let opr = toEnum (fromIntegral lab)
      nm00 <- collapseDupsAt book (loc + 0)
      nm10 <- collapseDupsAt book (loc + 1)
      return $ Op2 opr nm00 nm10

    REF -> do
      let fid = fromIntegral lab
      let ari = fromIntegral (funArity book fid)
      arg0 <- forM [0..ari-1] (\i -> collapseDupsAt book (loc + i))
      let name = MS.findWithDefault "?" fid (fidToNam book)
      return $ Ref name fid arg0

    _ -> return $ Var "?"

-- Sup Collapser
-- -------------

collapseSups :: Book -> Core -> HVM Core
collapseSups book core = case core of
  Var name -> return $ Var name
  Ref name fid args -> do
    args <- mapM (collapseSups book) args
    return $ Ref name fid args
  Lam lab name body -> do
    body <- collapseSups book body
    return $ Lam lab name body
  App lab fun arg -> do
    fun <- collapseSups book fun
    arg <- collapseSups book arg
    return $ App lab fun arg
  Dup lab x y val body -> do
    val <- collapseSups book val
    body <- collapseSups book body
    return $ Dup lab x y val body
  Ctr nam fields -> do
    fields <- mapM (collapseSups book) fields
    return $ Ctr nam fields
  Mat val mov css -> do
    val <- collapseSups book val
    mov <- mapM (\(key, expr) -> do
      expr <- collapseSups book expr
      return (key, expr)) mov
    css <- mapM (\(ctr, fds, bod) -> do
      bod <- collapseSups book bod
      return (ctr, fds, bod)) css
    return $ Mat val mov css
  U32 val -> return $ U32 val
  Chr val -> return $ Chr val
  Op2 op x y -> do
    x <- collapseSups book x
    y <- collapseSups book y
    return $ Op2 op x y
  Let mode name val body -> do
    val <- collapseSups book val
    body <- collapseSups book body
    return $ Let mode name val body
  Era -> return Era
  Sup lab tm0 tm1 -> do
    tm0' <- collapseSups book tm0
    tm1' <- collapseSups book tm1
    return $ Sup lab tm0' tm1'

-- Tree Collapser
-- --------------

doCollapseAt :: Book -> Loc -> HVM Core
doCollapseAt book host = do
  core <- collapseDupsAt book host
  collapseSups book core

-- Flattener
-- ---------

flattenDFS :: Core -> [Core]
flattenDFS (Sup _ a b) = flattenDFS a ++ flattenDFS b
flattenDFS Era = []
flattenDFS x = [x]

flattenBFS :: Core -> [Core]
flattenBFS root = go root (SQ [] []) where
  go (Sup _ a b) sq = go Era (sqPut b $ sqPut a sq)
  go Era sq = case sqPop sq of
    Just (v, sq') -> go v sq'
    Nothing -> []
  go x sq = x : go Era sq

-- Simple Queue
-- ------------

data SQ a = SQ [a] [a]

sqPop :: SQ a -> Maybe (a, SQ a)
sqPop (SQ [] [])     = Nothing
sqPop (SQ [] ys)     = sqPop (SQ (reverse ys) [])
sqPop (SQ (x:xs) ys) = Just (x, SQ xs ys)

sqPut :: a -> SQ a -> SQ a
sqPut x (SQ xs ys) = SQ xs (x:ys)

flatten :: Core -> [Core]
flatten = flattenBFS

-- Flat Collapser
-- --------------

doCollapseFlatAt :: Book -> Loc -> HVM [Core]
doCollapseFlatAt book host = do
  core <- doCollapseAt book host
  return $ flatten core
