module Prover.Search where

import HVM.Type
import HVM.Foreign (getItr, reduceAtC, allocNode, termLoc, got, hvmInit, hvmFree, termTag)
import HVM.Inject (doInjectCoreAt)
import HVM.Extract (doExtractCoreAt)
import HVM.Reduce (reduceAt)
import Prover.CoreSyntaxBridge (HsFormula(..), injectHsFormula, extractHsFormula, hsToCoreFormula)
import Data.Word (Word64, Word32)
import Control.Monad (foldM)
import Data.Maybe (catMaybes)

import qualified Data.Map.Strict as MS
import qualified Data.Heap as Heap
import qualified Data.Set as Set

-- Type alias for reduction function type
type ReduceFn = Book -> Loc -> HVM Term

data SearchNode = SearchNode
  { snFormulaLoc :: Loc    -- HVM Loc of the derived formula
  , snHsFormula  :: HsFormula -- Haskell representation
  , snGCost      :: Word64 -- Cost from start to this node
  , snHCost      :: Word64 -- Heuristic cost to goal
  , snPath       :: [String] -- Path description
  } deriving (Show, Eq)

instance Ord SearchNode where
  compare n1 n2 = compare (snGCost n1 + snHCost n1) (snGCost n2 + snHCost n2)

-- Core size estimation for memory allocation
coreSizeHint :: Core -> Word32
coreSizeHint (Var _) = 1
coreSizeHint (Ref _ _ args) = 1 + sum (map coreSizeHint args)
coreSizeHint Era = 1
coreSizeHint (Lam _ b) = 1 + coreSizeHint b
coreSizeHint (App f a) = 1 + coreSizeHint f + coreSizeHint a
coreSizeHint (Sup _ a b) = 1 + coreSizeHint a + coreSizeHint b
coreSizeHint (Dup _ _ _ v b) = 1 + coreSizeHint v + coreSizeHint b
coreSizeHint (Ctr _ fds) = 1 + sum (map coreSizeHint fds)
coreSizeHint (U32 _) = 1
coreSizeHint (Chr _) = 1
coreSizeHint (Op2 _ a b) = 1 + coreSizeHint a + coreSizeHint b
coreSizeHint (Let _ _ v b) = 1 + coreSizeHint v + coreSizeHint b
coreSizeHint (Mat _ v ms cs) = 1 + coreSizeHint v + sum (map (coreSizeHint . snd) ms) + sum (map (\(_,_,b) -> coreSizeHint b) cs)
coreSizeHint (Inc b) = 1 + coreSizeHint b
coreSizeHint (Dec b) = 1 + coreSizeHint b

-- Extract Core term from a location
extractCoreFromLoc :: Book -> Loc -> IO (Maybe Core)
extractCoreFromLoc book locToExtract = do
    let extractionReducer :: ReduceAt = \b l -> got l
    core <- doExtractCoreAt extractionReducer book locToExtract
    if isErrorCore core then return Nothing else return (Just core)
  where
    isErrorCore (Var "?") = True
    isErrorCore Era = True
    isErrorCore _ = False

-- Find matching formulas for Modus Ponens
findMatchingFormulaLocs :: HsFormula -> [HsFormula] -> [Loc] -> MS.Map HsFormula Loc -> IO [Loc]
findMatchingFormulaLocs targetHsFormula initialAxioms_hs initialAxiom_locs closedSet_map = do
    let axiomMatches = [loc | (hs, loc) <- zip initialAxioms_hs initialAxiom_locs, hs == targetHsFormula]
    let closedMatches = case MS.lookup targetHsFormula closedSet_map of
                            Just loc -> [loc]
                            Nothing -> []
    return $ axiomMatches ++ closedMatches

findMatchingImplicationLocs :: HsFormula -> [HsFormula] -> [Loc] -> MS.Map HsFormula Loc -> IO [Loc]
findMatchingImplicationLocs targetConsequentHsFormula initialAxioms_hs initialAxiom_locs closedSet_map = do
    let checkAndGetLoc (hsImp, loc) = case hsImp of
                                        HsImp _ cons | cons == targetConsequentHsFormula -> Just loc
                                        _ -> Nothing
    
    let axiomImplicationMatches = catMaybes $ map checkAndGetLoc (zip initialAxioms_hs initialAxiom_locs)
    let closedImplicationMatches = catMaybes $ map (\(hsImp, loc) -> checkAndGetLoc (hsImp,loc)) (MS.toList closedSet_map)
    return $ axiomImplicationMatches ++ closedImplicationMatches

-- Apply an HVM rule and get result & cost
applyHvmRule :: Book 
             -> ReduceFn 
             -> String
             -> [Loc]
             -> IO (Maybe (Loc, Word64))
applyHvmRule book reduceFn ruleHvmName premiseLocs = do
    extractedPremiseCores_m <- mapM (extractCoreFromLoc book) premiseLocs
    let premiseCores_m = sequence extractedPremiseCores_m
    
    case premiseCores_m of
        Nothing -> return Nothing
        Just actualPremiseCores -> do
            let ruleFid = mget (namToFid book) ruleHvmName
            let callCore = Ref ruleHvmName ruleFid actualPremiseCores
            let estimatedCallNodes = coreSizeHint callCore
            
            workerLoc <- allocNode estimatedCallNodes
            _ <- doInjectCoreAt book callCore workerLoc []
            
            oldItrs <- getItr
            resultTermPtr <- reduceFn book workerLoc
            newItrs <- getItr
            let costOfRuleApplication = newItrs - oldItrs
            
            let resultValueLoc = termLoc resultTermPtr
            let resultTag = termTag resultTermPtr
            
            if resultTag == _ERA_ 
                then return Nothing
                else do
                    maybeResultCore <- extractCoreFromLoc book resultValueLoc
                    case maybeResultCore of
                        Just Era -> return Nothing
                        Just _ -> return $ Just (resultValueLoc, costOfRuleApplication)
                        Nothing -> return Nothing

-- A* Search implementation
aStarSearch :: Book -> ReduceFn -> [HsFormula] -> HsFormula -> [String] -> Word64 -> IO (Maybe SearchNode)
aStarSearch book reduceFn initialAxioms targetFormula ruleNames budgetCt = do
  putStrLn "Initializing HVM for A* search..."
  hvmInit

  axiomLocs <- mapM (injectHsFormula book) initialAxioms
  targetLoc <- injectHsFormula book targetFormula
  
  Just hsTarget <- extractHsFormula reduceFn book targetLoc

  let startNodes = map (\(loc, axiom) -> 
                        SearchNode loc axiom 0 (heuristic hsTarget axiom) ["axiom"]) 
                      (zip axiomLocs initialAxioms)
  
  let openSet = Heap.fromList startNodes
  let closedSet = MS.empty :: MS.Map HsFormula Loc

  searchLoop openSet closedSet 0 axiomLocs initialAxioms hsTarget
  where
    heuristic :: HsFormula -> HsFormula -> Word64
    heuristic target current = fromIntegral $ syntacticDifference target current

    syntacticDifference :: HsFormula -> HsFormula -> Int
    syntacticDifference t@(HsAtom n1) c@(HsAtom n2) = if n1 == n2 then 0 else 1
    syntacticDifference (HsImp tA tC) (HsImp cA cC) = 
        syntacticDifference tA cA + syntacticDifference tC cC
    syntacticDifference _ _ = 100

    searchLoop :: Heap.MinHeap SearchNode 
               -> MS.Map HsFormula Loc 
               -> Word64 
               -> [Loc]
               -> [HsFormula]
               -> HsFormula
               -> IO (Maybe SearchNode)
    searchLoop open closed totalInteractionsConsumed axiomLocs axiomHs target = do
      case Heap.view open of
        Nothing -> do
          putStrLn "A* Search: Open set empty. Goal not found."
          hvmFree
          return Nothing
        Just (currentNode, restOpen) -> do
          let currentHsFormula = snHsFormula currentNode
          let currentLoc = snFormulaLoc currentNode
          let currentGCost = snGCost currentNode

          putStrLn $ "Expanding: " ++ show currentHsFormula 
                     ++ " (g=" ++ show currentGCost ++ ")"

          if totalInteractionsConsumed >= budgetCt then do
            putStrLn $ "A* Search: Budget " ++ show budgetCt ++ " exhausted."
            hvmFree
            return Nothing
          else if currentHsFormula == target then do
            putStrLn "A* Search: Goal found!"
            hvmFree
            return $ Just currentNode
          else if MS.member currentHsFormula closed then
            searchLoop restOpen closed totalInteractionsConsumed axiomLocs axiomHs target
          else do
            entryItrs <- getItr
            successorNodes <- generateSuccessors currentNode axiomLocs axiomHs closed target
            exitItrs <- getItr
            let interactionsThisExpansion = exitItrs - entryItrs
            
            let newOpen = foldl (flip Heap.insert) restOpen successorNodes
            let newClosed = MS.insert currentHsFormula currentLoc closed
            
            searchLoop newOpen newClosed 
                      (totalInteractionsConsumed + interactionsThisExpansion)
                      axiomLocs axiomHs target

    generateSuccessors :: SearchNode 
                      -> [Loc] 
                      -> [HsFormula]
                      -> MS.Map HsFormula Loc
                      -> HsFormula
                      -> IO [SearchNode]
    generateSuccessors currentNode axiomLocs axiomHs closed target = do
      -- Try current as first premise (A->B), find matching A
      modusPonensResults1 <- case snHsFormula currentNode of
        HsImp hs_A_ante hs_B_cons -> do
          secondPremiseLocs <- findMatchingFormulaLocs hs_A_ante axiomHs axiomLocs closed
          catMaybes <$> mapM (tryModusPonens currentNode True closed target) 
                            (zip secondPremiseLocs (repeat hs_A_ante))
        _ -> return []

      -- Try current as second premise (A), find matching X->A
      modusPonensResults2 <- case snHsFormula currentNode of
        atom@(HsAtom _) -> do
          firstPremiseLocs <- findMatchingImplicationLocs atom axiomHs axiomLocs closed
          catMaybes <$> mapM (tryModusPonens currentNode False closed target)
                            (zip firstPremiseLocs (repeat atom))
        _ -> return []

      return $ modusPonensResults1 ++ modusPonensResults2

    tryModusPonens :: SearchNode 
                   -> Bool 
                   -> MS.Map HsFormula Loc
                   -> HsFormula
                   -> (Loc, HsFormula) 
                   -> IO (Maybe SearchNode)
    tryModusPonens currentNode isFirstArg closed target (otherLoc, otherFormula) = do
      let (firstLoc, secondLoc) = if isFirstArg
                                 then (snFormulaLoc currentNode, otherLoc)
                                 else (otherLoc, snFormulaLoc currentNode)
      
      ruleResult <- applyHvmRule book reduceFn "@apply_ModusPonens" [firstLoc, secondLoc]
      
      case ruleResult of
        Nothing -> return Nothing
        Just (resultLoc, ruleCost) -> do
          maybeFormula <- extractHsFormula reduceFn book resultLoc
          case maybeFormula of
            Nothing -> return Nothing
            Just newFormula -> do
              if MS.member newFormula closed
                then return Nothing
                else do
                  let ruleDesc = "ModusPonens " ++ show (snHsFormula currentNode) 
                               ++ " + " ++ show otherFormula
                  let newNode = SearchNode {
                    snFormulaLoc = resultLoc,
                    snHsFormula = newFormula,
                    snGCost = snGCost currentNode + ruleCost,
                    snHCost = heuristic target newFormula,
                    snPath = ruleDesc : snPath currentNode
                  }
                  putStrLn $ "    Generated: " ++ show newFormula
                  return $ Just newNode