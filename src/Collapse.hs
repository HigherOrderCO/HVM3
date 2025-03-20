-- //./Type.hs//

module Collapse where

import Control.Monad (ap, forM, forM_, foldM)
import Control.Monad.IO.Class
import Data.Char (chr, ord)
import Data.IORef
import Data.Word
import Debug.Trace
import GHC.Conc
import Foreign
import Show
import Type
import System.Exit (exitFailure)
import System.IO.Unsafe (unsafeInterleaveIO)
import qualified Data.IntMap.Strict as IM
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
  setOld lamLoc (termSetBit eraTerm)
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

  setOld dupLoc (termSetBit eraTerm)
  return eraTerm

-- λx.&L{f0,f1}
-- ----------------- SUP-LAM
-- x <- &L{x0,x1}
-- &L{λx0.f0,λx1.f1}
icSupLam :: Term -> Term -> HVM Term
icSupLam lam sup = do
  let lamLoc = termLoc lam
  let supLoc = termLoc sup
  let supLab = termLab sup

  f0 <- got (supLoc + 0)
  f1 <- got (supLoc + 1)

  lam0Loc <- allocNode 1
  lam1Loc <- allocNode 1

  setNew lam0Loc f0
  setNew lam1Loc f1

  let x0 = termNew _VAR_ 0 lam0Loc
  let x1 = termNew _VAR_ 0 lam1Loc

  newSupLoc <- allocNode 2
  setNew (newSupLoc + 0) x0
  setNew (newSupLoc + 1) x1

  let newSup = termNew _SUP_ supLab newSupLoc

  setOld lamLoc (termSetBit newSup)

  let lam0Term = termNew _LAM_ 0 lam0Loc
  let lam1Term = termNew _LAM_ 0 lam1Loc

  resultSupLoc <- allocNode 2
  setNew (resultSupLoc + 0) lam0Term
  setNew (resultSupLoc + 1) lam1Term

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
  setNew dupLoc fun

  let f0 = termNew _DP0_ supLab dupLoc
  let f1 = termNew _DP1_ supLab dupLoc

  app0Loc <- allocNode 2
  setNew (app0Loc + 0) f0
  setNew (app0Loc + 1) lft
  let app0 = termNew _APP_ 0 app0Loc

  app1Loc <- allocNode 2
  setNew (app1Loc + 0) f1
  setNew (app1Loc + 1) rgt
  let app1 = termNew _APP_ 0 app1Loc

  resultSupLoc <- allocNode 2 
  setNew (resultSupLoc + 0) app0
  setNew (resultSupLoc + 1) app1
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
  setNew dupLoc y

  let y0 = termNew _DP0_ outerLab dupLoc
  let y1 = termNew _DP1_ outerLab dupLoc

  sup0Loc <- allocNode 2
  setNew (sup0Loc + 0) x0
  setNew (sup0Loc + 1) y0
  let sup0 = termNew _SUP_ outerLab sup0Loc

  sup1Loc <- allocNode 2
  setNew (sup1Loc + 0) x1
  setNew (sup1Loc + 1) y1
  let sup1 = termNew _SUP_ outerLab sup1Loc

  resultSupLoc <- allocNode 2
  setNew (resultSupLoc + 0) sup0
  setNew (resultSupLoc + 1) sup1
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
  setNew dupLoc x

  let x0 = termNew _DP0_ outerLab dupLoc
  let x1 = termNew _DP1_ outerLab dupLoc

  sup0Loc <- allocNode 2
  setNew (sup0Loc + 0) x0
  setNew (sup0Loc + 1) y0
  let sup0 = termNew _SUP_ outerLab sup0Loc

  sup1Loc <- allocNode 2
  setNew (sup1Loc + 0) x1
  setNew (sup1Loc + 1) y1
  let sup1 = termNew _SUP_ outerLab sup1Loc

  resultSupLoc <- allocNode 2
  setNew (resultSupLoc + 0) sup0
  setNew (resultSupLoc + 1) sup1
  return (termNew _SUP_ innerLab resultSupLoc)
  

-- !&L{x0,x1} = x; K
-- ----------------- DUP-VAR
-- x0 <- x
-- x1 <- x
-- K
icDupVar :: Term -> Term -> HVM Term
icDupVar dup var = do
  let dupLoc = termLoc dup
  setOld dupLoc (termSetBit var)
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
  setNew dupFunLoc fun
  dupArgLoc <- allocNode 1
  setNew dupArgLoc arg

  let f0 = termNew _DP0_ lab dupFunLoc
  let f1 = termNew _DP1_ lab dupFunLoc

  let x0 = termNew _DP0_ lab dupArgLoc
  let x1 = termNew _DP1_ lab dupArgLoc

  app0Loc <- allocNode 2
  setNew (app0Loc + 0) f0
  setNew (app0Loc + 1) x0
  let app0 = termNew _APP_ 0 app0Loc

  app1Loc <- allocNode 2
  setNew (app1Loc + 0) f1
  setNew (app1Loc + 1) x1
  let app1 = termNew _APP_ 0 app1Loc

  if isCo0 
  then do
    setOld dupLoc (termSetBit app1)
    return app0
  else do
    setOld dupLoc (termSetBit app0)
    return app1

-- #CTR {&L{x0 x1} k0 k1 ... kn}
-- --------------------------- SUP-CTR
-- !&L{CTR0 CTR1} = CTR
-- !&L{k00, k01}=k0
-- !&L{k10, k11}=k1
-- !&L{kn0, kn1}=kn
-- &L{#CTR0{x0 k00 k10 ... kn0} #CTR1{x1 k01 k11 kn1}}
icSupCtr :: Term -> Term -> Word64 -> Word64 -> Word64 -> HVM Term
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
      setNew dupAjLoc aj                  -- Store aj
      let aj0 = termNew _DP0_ supLab dupAjLoc  -- First duplicate
      let aj1 = termNew _DP1_ supLab dupAjLoc  -- Second duplicate
      return (Just (aj0, aj1))            -- Return pair (aj0, aj1)

  -- Create CTR0: #CTR{a0' a1' ... x0 ... a_{len-1}'}
  ctr0Loc <- allocNode len
  forM_ [0 .. len - 1] $ \j -> do
    if j == idx then
      setNew (ctr0Loc + j) x0             -- Use x0 at idx
    else do
      let Just (aj0, _) = aList !! fromIntegral j  -- Get aj0
      setNew (ctr0Loc + j) aj0            -- Use duplicate aj0
  let ctr0 = termNew _CTR_ cid ctr0Loc    -- Construct CTR0

  -- Create CTR1: #CTR{a0'' a1'' ... x1 ... a_{len-1}''}
  ctr1Loc <- allocNode len
  forM_ [0 .. len - 1] $ \j -> do
    if j == idx then
      setNew (ctr1Loc + j) x1             -- Use x1 at idx
    else do
      let Just (_, aj1) = aList !! fromIntegral j  -- Get aj1
      setNew (ctr1Loc + j) aj1            -- Use duplicate aj1
  let ctr1 = termNew _CTR_ cid ctr1Loc    -- Construct CTR1

  -- Create resulting superposition: &L{CTR0 CTR1}
  supResultLoc <- allocNode 2
  setNew (supResultLoc + 0) ctr0
  setNew (supResultLoc + 1) ctr1
  let supResult = termNew _SUP_ supLab supResultLoc

  -- Return the new superposition
  return supResult

-- ~N{#A: &L{z0 z1} #B: x #C: y ...}
-- --------------------------------- SUP-MAT-CTR
-- !&L{N0,N1} = N;
-- !&L{x0,x1} = x;
-- !&L{y0,y1} = y;
-- &L{~N0{#A:z0 #B: x0 #C: y0} ~N1{#A:z1 #B: x1 #C: y1}}
icSupMatCtr :: Tag -> Term -> Term -> Word64 -> Word64 -> HVM Term
icSupMatCtr matTag mat sup matLen supIdx = do
  let matLoc = termLoc mat
  let matLab = termLab mat
  let supLab = termLab sup
  let supLoc = termLoc sup
  n <- got (matLoc + 0)
  dupNLoc <- allocNode 1
  setNew dupNLoc n
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
      setNew dupTjLoc tj
      let t0j = termNew _DP0_ supLab dupTjLoc
      let t1j = termNew _DP1_ supLab dupTjLoc
      return (t0j, t1j)
  mat0Loc <- allocNode (matLen + 1)
  setNew (mat0Loc + 0) n0
  forM_ [1 .. matLen] $ \j -> do
    let (u_j, _) = uList !! fromIntegral (j - 1)
    setNew (mat0Loc + 1 + (j - 1)) u_j
  let mat0 = termNew matTag matLab mat0Loc
  mat1Loc <- allocNode (matLen + 1)
  setNew (mat1Loc + 0) n1
  forM_ [1 .. matLen] $ \j -> do
    let (_, v_j) = uList !! fromIntegral (j - 1)
    setNew (mat1Loc + 1 + (j - 1)) v_j
  let mat1 = termNew matTag matLab mat1Loc
  supLoc <- allocNode 2
  setNew (supLoc + 0) mat0
  setNew (supLoc + 1) mat1
  return (termNew _SUP_ supLab supLoc)

isDup :: TAG -> Bool
isDup tag =
  case tag of
    DP0 -> True
    DP1 -> True
    _   -> False

isEra :: Term -> Bool
isEra term = termTag term == _ERA_

isSup :: Term -> Bool
isSup term = termTag term == _SUP_

collapseSupsTerm :: Book -> Term -> HVM Term
collapseSupsTerm book root = do
  term <- reduceC root 0
  let tag = termTag term
  let lab = termLab term
  let loc = termLoc term
  case (tagT tag) of
    LAM -> do
      bod <- got (loc + 0)
      bodCol <- collapseSupsTerm book bod
      setOld (loc + 0) bodCol
    APP -> do
      fun <- got (loc + 0)
      arg <- got (loc + 1)
      funCol <- collapseSupsTerm book fun
      argCol <- collapseSupsTerm book arg
      setOld (loc + 0) funCol
      setOld (loc + 1) argCol
    SUP -> do
      lft <- got (loc + 0)
      rgt <- got (loc + 1)
      lftCol <- collapseSupsTerm book lft
      rgtCol <- collapseSupsTerm book rgt
      setOld (loc + 0) lftCol
      setOld (loc + 1) rgtCol

    MAT -> do
        let cid = termLab term
        let matLen = mget (cidToLen book) cid
        forM_ [1 .. matLen] $ \i -> do
          ctr <- got (loc + i)
          ctrCol <- collapseSupsTerm book ctr
          setOld (loc + i) ctrCol
        return ()

    CTR -> do
      let cid = termLab term
      let ctrAri = mget (cidToAri book) cid
      forM_ [1 .. ctrAri] $ \i -> do
        field <- got (loc + i - 1)
        fieldCol <- collapseSupsTerm book field
        setOld (loc + i - 1) fieldCol
      return ()

    _ -> return ()

  term <- reduceC term 0
  let tag = termTag term
  let lab = termLab term
  let loc = termLoc term
  case (tagT tag) of
    LAM -> do
      bodCol <- got (loc + 0)
      if isSup bodCol then do
        result <- icSupLam term bodCol
        collapseSupsTerm book result
      else if isEra bodCol then do
        result <- icEraLam term bodCol
        collapseSupsTerm book result
      else return term
    APP -> do
      funCol <- got (loc + 0)
      argCol <- got (loc + 1)
      if isSup argCol then do
        result <- icSupApp term argCol
        collapseSupsTerm book result
      else if isEra argCol then do
        result <- icEraApp term argCol
        collapseSupsTerm book result
      else return term
    SUP -> do
      lftCol <- got (loc + 0)
      rgtCol <- got (loc + 1)
      if isSup lftCol && lab > termLab lftCol then do
        result <- icSupSupX term lftCol
        collapseSupsTerm book result
      else if isSup rgtCol && lab > termLab rgtCol then do
        result <- icSupSupY term rgtCol
        collapseSupsTerm book result
      else return term

    CTR -> do
      let cid = termLab term
      let ctrAri = mget (cidToAri book) cid
      let checkSupOrEra i = if i > ctrAri then return term else do
            trm <- got (loc + i - 1)
            if isSup trm then do
              res <- icSupCtr term trm cid ctrAri (i - 1)
              collapseSupsTerm book res
            else if isEra trm then do
              res <- icEraCtr term trm 
              collapseSupsTerm book res
            else checkSupOrEra (i + 1)
      checkSupOrEra 1

    tag | tag == MAT || tag == SWI -> do
      let matLen = if tag == SWI then 2 else mget (cidToLen book) lab
      let matTag = if tag == SWI then _SWI_ else _MAT_
      let checkEraOrSup i = if i > matLen then return term else do
            trm <- got (loc + i)
            if isEra trm then do
              res <- icEraMat term trm
              collapseSupsTerm book res
            else if isSup trm then do
              res <- icSupMatCtr matTag term trm matLen i
              collapseSupsTerm book res
            else checkEraOrSup (i + 1)
      checkEraOrSup 1

    _ -> return term


collapseDupsTerm :: Book -> Term -> HVM Term
collapseDupsTerm book root = do
  term <- reduceC root 0
  let tag = termTag term
  let loc = termLoc term
  case (tagT tag) of
    tag | isDup tag -> do
      val <- got loc
      valCol <- collapseDupsTerm book val
      let valTag = termTag valCol
      case (tagT valTag) of
        VAR -> do
          result <- icDupVar term valCol
          collapseDupsTerm book result
        APP -> do
          result <- icDupApp term valCol
          collapseDupsTerm book result
        ERA -> do
          result <- icDupEra term valCol
          collapseDupsTerm book result
        _ -> return term

    LAM -> do
      bod <- got (loc + 0)
      bodCol <- collapseDupsTerm book bod
      setOld (loc + 0) bodCol
      return term

    APP -> do
      fun <- got (loc + 0)
      arg <- got (loc + 1)
      funCol <- collapseDupsTerm book fun
      argCol <- collapseDupsTerm book arg
      setOld (loc + 0) funCol
      setOld (loc + 1) argCol
      return term

    SUP -> do
      lft <- got (loc + 0)
      rgt <- got (loc + 1)
      lftCol <- collapseDupsTerm book lft
      rgtCol <- collapseDupsTerm book rgt
      setOld (loc + 0) lftCol
      setOld (loc + 1) rgtCol
      return term

    OPX -> do
      child <- got (loc + 0)
      childCol <- collapseDupsTerm book child
      setOld (loc + 0) childCol
      return term

    CTR -> do
      let cid = termLab term
      let ctrAri = mget (cidToAri book) cid
      forM_ [1 .. ctrAri] $ \i -> do
        field <- got (loc + i - 1)
        fieldCol <- collapseDupsTerm book field
        setOld (loc + i - 1) fieldCol
      return term

    tag | tag == MAT || tag == SWI -> do
      let matLen = if tag == SWI then 2 else mget (cidToLen book) (termLab term)
      scrutinee <- got (loc + 0)
      scrutineeCol <- collapseDupsTerm book scrutinee
      setOld (loc + 0) scrutineeCol
      forM_ [1 .. matLen] $ \i -> do
        caseTerm <- got (loc + i)
        caseCol <- collapseDupsTerm book caseTerm
        setOld (loc + i) caseCol
      return term
    ERA -> return term
    W32 -> return term
    _   -> return term

flattenDFS :: Core -> [Core]
flattenDFS (Sup lab a b) = flattenDFS a ++ flattenDFS b
flattenDFS Era = []
flattenDFS x = [x]

data SQ a = SQ [a] [a]

sqPop :: SQ a -> Maybe (a, SQ a)
sqPop (SQ [] [])     = Nothing
sqPop (SQ [] ys)     = sqPop (SQ (reverse ys) [])
sqPop (SQ (x:xs) ys) = Just (x, SQ xs ys)

sqPut :: a -> SQ a -> SQ a
sqPut x (SQ xs ys) = SQ xs (x:ys)

flattenBFS :: Core -> [Core]
flattenBFS root = bfs (sqPut root (SQ [] [])) []
  where
    bfs :: SQ Core -> [Core] -> [Core]
    bfs sq acc =
      case sqPop sq of
        Nothing -> reverse acc
        Just (node, sq') ->
          case node of
            Sup _ a b -> bfs (sqPut b (sqPut a sq')) acc
            Era -> bfs sq' acc
            x -> bfs sq' (x : acc)

