-- //./Type.hs//
-- //./Extract.hs//

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
icSupCtr ::  Term -> Term -> Word64 -> Word64 -> HVM Term
icSupCtr ctr sup ctrAri supIdx = do
  -- Extract locations and labels
  let ctrLoc = termLoc ctr
  let supLoc = termLoc sup
  let supLab = termLab sup  -- Label L of the superposition
  let cid = termLab ctr     -- Constructor ID

  -- Get the superposition components x0 and x1
  x0 <- got (supLoc + 0)
  x1 <- got (supLoc + 1)

  -- Build field pairs for both constructors
  fieldPairs <- forM [0 .. ctrAri - 1] $ \j -> do
    if j == supIdx then
      -- Superposed field: use x0 and x1 directly
      return (x0, x1)
    else do
      -- Non-superposed field: duplicate it
      fj <- got (ctrLoc + j)
      dupFjLoc <- allocNode 1
      setNew dupFjLoc fj
      let fj0 = termNew _DP0_ supLab dupFjLoc
      let fj1 = termNew _DP1_ supLab dupFjLoc
      return (fj0, fj1)

  -- Extract fields for each constructor
  let fields0 = map fst fieldPairs  -- Fields for CTR0: x0 at supIdx, fj0 elsewhere
  let fields1 = map snd fieldPairs  -- Fields for CTR1: x1 at supIdx, fj1 elsewhere

  -- Allocate and populate first constructor (CTR0)
  ctr0Loc <- allocNode ctrAri
  forM_ [0 .. ctrAri - 1] $ \j -> do
    setNew (ctr0Loc + j) (fields0 !! fromIntegral j)
  let ctr0 = termNew _CTR_ cid ctr0Loc

  -- Allocate and populate second constructor (CTR1)
  ctr1Loc <- allocNode ctrAri
  forM_ [0 .. ctrAri - 1] $ \j -> do
    setNew (ctr1Loc + j) (fields1 !! fromIntegral j)
  let ctr1 = termNew _CTR_ cid ctr1Loc

  -- Create the resulting superposition
  newSupLoc <- allocNode 2
  setNew (newSupLoc + 0) ctr0
  setNew (newSupLoc + 1) ctr1
  return (termNew _SUP_ supLab newSupLoc)



-- ~N{#A: &L{z0 z1} #B: x #C: y ...}
-- --------------------------------- SUP-MAT-CTR
-- !&L{N0,N1} = N;
-- !&L{x0,x1} = x;
-- !&L{y0,y1} = y;
-- &L{~N0{#A:z0 #B: x0 #C: y0} ~N1{#A:z1 #B: x1 #C: y1}}
icSupMatCtr :: Term -> Term -> Word64 -> Word64 -> HVM Term
icSupMatCtr mat sup matLen supIdx = do
  let matLoc = termLoc mat
  let matLab = termLab mat
  let supLab = termLab sup 
  let supLoc = termLoc sup

  -- Step 1: Get N from the MAT term and duplicate it
  n <- got (matLoc + 0)
  dupNLoc <- allocNode 1
  setNew dupNLoc n
  let n0 = termNew _DP0_ supLab dupNLoc
  let n1 = termNew _DP1_ supLab dupNLoc

  -- Step 2: Get the superposition components from sup
  z0 <- got (supLoc + 0)
  z1 <- got (supLoc + 1)

  -- Step 3: Process all case terms, duplicating non-superposed ones
  uList <- forM [1 .. matLen] $ \j -> do
    let caseOffset = matLoc + j
    if j == supIdx then do
      -- This is the superposed case; use z0 and z1 directly
      return (z0, z1)
    else do
      -- Duplicate non-superposed case
      tj <- got caseOffset

      dupTjLoc <- allocNode 1
      setNew dupTjLoc tj
      let t0j = termNew _DP0_ supLab dupTjLoc
      let t1j = termNew _DP1_ supLab dupTjLoc
      return (t0j, t1j)

  -- Step 4: Create the first MAT term
  mat0Loc <- allocNode (matLen + 1)
  setNew (mat0Loc + 0) n0
  forM_ [0 .. matLen - 1] $ \j -> do
    let (u_j, _) = uList !! fromIntegral j
    setNew (mat0Loc + 1 + j) u_j
  let mat0 = termNew _MAT_ matLab mat0Loc

  -- Step 5: Create the second MAT term
  mat1Loc <- allocNode (matLen + 1)
  setNew (mat1Loc + 0) n1
  forM_ [0 .. matLen - 1] $ \j -> do
    let (_, v_j) = uList !! fromIntegral j
    setNew (mat1Loc + 1 + j) v_j
  let mat1 = termNew _MAT_ matLab mat1Loc

  -- Step 6: Create the resulting superposition
  supLoc <- allocNode 2
  setNew (supLoc + 0) mat0
  setNew (supLoc + 1) mat1
  return (termNew _SUP_ supLab supLoc)

      
-- ~N{0:&L{z0,z1};+:s;}
-- --------------------------------- SUP-SWI-Z
-- !&L{N0,N1} = N;
-- !&L{S0,S1} = S;
-- &L{~N0{0:z0;+:S0},~N1{0:z1;+:S1}}
icSupSwiZ :: Term -> Term -> HVM Term
icSupSwiZ swi sup = do
  let swiLoc = termLoc swi
  let supLoc = termLoc sup
  let supLab = termLab sup

  num <- got (swiLoc + 0)
  z0  <- got (supLoc + 0)
  z1  <- got (supLoc + 1)
  s   <- got (swiLoc + 2)

  dupNLoc <- allocNode 1
  dupSLoc <- allocNode 1

  setNew dupNLoc num
  setNew dupSLoc s

  let n0 = termNew _DP0_ supLab dupNLoc
  let n1 = termNew _DP1_ supLab dupNLoc
  let s0 = termNew _DP0_ supLab dupSLoc
  let s1 = termNew _DP1_ supLab dupSLoc

  swi0Loc <- allocNode 3
  setNew (swi0Loc + 0) n0
  setNew (swi0Loc + 1) z0
  setNew (swi0Loc + 2) s0

  swi1Loc <- allocNode 3
  setNew (swi1Loc + 0) n1
  setNew (swi1Loc + 1) z1
  setNew (swi1Loc + 2) s1

  let swi0 = termNew _SWI_ 0 swi0Loc
  let swi1 = termNew _SWI_ 0 swi1Loc

  resLoc <- allocNode 2
  setNew (resLoc + 0) swi0
  setNew (resLoc + 1) swi1

  return (termNew _SUP_ supLab resLoc)

-- ~N{0:z;+:&0{s0,s1};}
-- --------------------------------- SUP-SWI-S
-- !&L{N0,N1} = N;
-- !&L{Z0,Z1} = Z;
-- &L{~N0{0:z0;+:S0},~N1{0:z1;+:S1}}
icSupSwiS :: Term -> Term -> HVM Term
icSupSwiS swi sup = do
  let swiLoc = termLoc swi
  let supLoc = termLoc sup
  let supLab = termLab sup

  num <- got (swiLoc + 0)
  z   <- got (swiLoc + 1)
  s0  <- got (supLoc + 0)
  s1  <- got (supLoc + 1)

  dupNLoc <- allocNode 1
  dupZLoc <- allocNode 1

  setNew dupNLoc num
  setNew dupZLoc z

  let n0 = termNew _DP0_ supLab dupNLoc
  let n1 = termNew _DP1_ supLab dupNLoc
  let z0 = termNew _DP0_ supLab dupZLoc
  let z1 = termNew _DP1_ supLab dupZLoc

  swi0Loc <- allocNode 3
  setNew (swi0Loc + 0) n0
  setNew (swi0Loc + 1) z0
  setNew (swi0Loc + 2) s0

  swi1Loc <- allocNode 3
  setNew (swi1Loc + 0) n1
  setNew (swi1Loc + 1) z1
  setNew (swi1Loc + 2) s1

  let swi0 = termNew _SWI_ 0 swi0Loc
  let swi1 = termNew _SWI_ 0 swi1Loc

  resLoc <- allocNode 2
  setNew (resLoc + 0) swi0
  setNew (resLoc + 1) swi1

  return (termNew _SUP_ supLab resLoc)

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
  -- Step 1: Reduce to WHNF
  term <- reduceC root 0

  let tag = termTag term
  let lab = termLab term
  let loc = termLoc term

  -- Step 2: Recursively collapse children
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
    CTR -> do
      let cid = termLab term
      let ctrAri = mget (cidToAri book) cid
      forM_ [1 .. ctrAri] $ \i -> do
        ctr <- got (loc + (i - 1))
        ctrCol <- collapseSupsTerm book ctr
        setOld (loc + (i - 1)) ctrCol
      return ()

    _ -> do return ()

  -- Step 3: Reduce to WHNF again
  term <- reduceC term 0
  let tag = termTag term
  let lab = termLab term
  let loc = termLoc term

  -- Step 4: Apply interaction rules
  case (tagT tag) of
    LAM -> do
      bodCol <- got (loc + 0)
      if isSup bodCol then do
        result <- icSupLam term bodCol
        collapseSupsTerm book result

      else if isEra bodCol then do
        result <- icEraLam term bodCol
        collapseSupsTerm book result
      else
        return term
    APP -> do
      funCol <- got (loc + 0)
      argCol <- got (loc + 1)
      if isSup argCol then do
        result <- icSupApp term argCol
        collapseSupsTerm book result
      else if isEra argCol then do
        result <- icEraApp term argCol
        collapseSupsTerm book result
      else
        return term
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

    SWI -> do
      _ <- got (loc + 0)  -- num (unused in interactions)
      ifz <- got (loc + 1)
      ifs <- got (loc + 2)
      if isSup ifz then do
        result <- icSupSwiZ term ifz
        collapseSupsTerm book result
      else if isSup ifs then do
        result <- icSupSwiS term ifs
        collapseSupsTerm book result
      else
        return term


    MAT -> do
        let matLen = mget (cidToLen book) lab

        result <- let findFirstSup i = do
                        if i > matLen 
                          then return term 
                          else do
                            trm <- got (loc + (i - 1))
                            if isSup trm 
                              then do
                                res <- icSupMatCtr term trm matLen (i - 1)
                                collapseSupsTerm book res 
                              else findFirstSup (i + 1)
                  in findFirstSup 1

        return result

    CTR -> do
          let cid = termLab term
          let ctrAri = mget (cidToAri book) cid
          let findFirstSup i = if i >= ctrAri then return term else do
                field <- got (loc + i)
                if isSup field then do
                  res <- icSupCtr term field ctrAri i
                  collapseSupsTerm book res
                else findFirstSup (i + 1)
          findFirstSup 0

    _ -> do
      return term

collapseDupsTerm :: Book -> Term -> HVM Term
collapseDupsTerm book root = do
  -- Reduce the term to weak head normal form (WHNF)
  term <- reduceC root 0

  let tag = termTag term
  let loc = termLoc term

  case (tagT tag) of
    tag | isDup tag -> do
      -- Get the value this duplication points to and collapse it
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
      -- Collapse the body of the lambda
      bod <- got (loc + 0)
      bodCol <- collapseDupsTerm book bod
      setOld (loc + 0) bodCol
      return term

    APP -> do
      -- Collapse the function and argument of the application
      fun <- got (loc + 0)
      arg <- got (loc + 1)
      funCol <- collapseDupsTerm book fun
      argCol <- collapseDupsTerm book arg
      setOld (loc + 0) funCol
      setOld (loc + 1) argCol
      return term

    SUP -> do
      -- Collapse the left and right subterms of the superposition
      lft <- got (loc + 0)
      rgt <- got (loc + 1)
      lftCol <- collapseDupsTerm book lft
      rgtCol <- collapseDupsTerm book rgt
      setOld (loc + 0) lftCol
      setOld (loc + 1) rgtCol
      return term

    OPX -> do 
      -- Collapse the child of the successor
      child <- got (loc + 0)
      childCol <- collapseDupsTerm book child
      setOld (loc + 0) childCol
      return term

    SWI -> do
      -- Collapse the number, if-zero, and if-successor branches of the switch
      num <- got (loc + 0)
      ifz <- got (loc + 1)
      ifs <- got (loc + 2)
      numCol <- collapseDupsTerm book num
      ifzCol <- collapseDupsTerm book ifz
      ifsCol <- collapseDupsTerm book ifs
      setOld (loc + 0) numCol
      setOld (loc + 1) ifzCol
      setOld (loc + 2) ifsCol
      return term

    CTR -> do
      -- Collapse all the fields of the constructor
      let cid = termLab term 
      let ctrAri = mget (cidToAri book) cid
      forM_ [1 .. ctrAri] $ \i -> do
        ctr <- got (loc + (i - 1))
        ctrCol <- collapseDupsTerm book ctr
        setOld (loc + (i - 1)) ctrCol

      return term

    MAT -> do
      -- Get the constructor ID and number of cases
      let cid = termLab term
      let matLen = mget (cidToLen book) cid
      -- Collapse the scrutinee
      scrutinee <- got (loc + 0)
      scrutineeCol <- collapseDupsTerm book scrutinee
      setOld (loc + 0) scrutineeCol
      -- Collapse each case
      forM_ [1 .. matLen] $ \i -> do
        caseTerm <- got (loc + i)
        caseCol <- collapseDupsTerm book caseTerm
        setOld (loc + i) caseCol
      return term
  
    ERA -> return term
    W32 -> return term
    _   -> return term

