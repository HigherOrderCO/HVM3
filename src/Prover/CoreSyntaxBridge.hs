module Prover.CoreSyntaxBridge where

import HVM.Type -- Assuming this exports Core, Term, Loc, etc.
import HVM.Inject (doInjectCoreAt) -- For injecting terms
import HVM.Extract (doExtractCoreAt) -- For reading terms back
import HVM.Reduce (reduceAt) -- Using reduceAt directly instead of ReduceAt type
import HVM.Foreign (allocNode) -- For allocating HVM memory
import qualified Data.Map.Strict as MS

-- Type alias for reduction function type
type ReduceFn = Book -> Loc -> HVM Term

-- Abstract representation of our formulas in Haskell (optional, but can be clearer)
data HsFormula
  = HsAtom String
  | HsImp HsFormula HsFormula
  -- Add other connectives as needed
  deriving (Eq, Ord, Show) -- Ord for use in Maps/Sets

-- Convert abstract HsFormula to HVM.Type.Core for injection
hsToCoreFormula :: HsFormula -> Core
hsToCoreFormula (HsAtom name) = Ctr "#FAtom" [Ctr ("#" ++ name) []] -- e.g. HsAtom "P" -> Ctr "#FAtom" [Ctr "#P" []]
hsToCoreFormula (HsImp ante cons) = Ctr "#FImp" [hsToCoreFormula ante, hsToCoreFormula cons]

-- Convert HVM.Type.Core (extracted from HVM) back to HsFormula
coreToHsFormula :: Core -> Maybe HsFormula
coreToHsFormula (Ctr "#FAtom" [Ctr ('#':name) []]) = Just $ HsAtom name
coreToHsFormula (Ctr "#FImp" [anteCore, consCore]) = do
  hsAnte <- coreToHsFormula anteCore
  hsCons <- coreToHsFormula consCore
  Just $ HsImp hsAnte hsCons
coreToHsFormula _ = Nothing

-- Function to inject an HsFormula into HVM and get its Loc
injectHsFormula :: Book -> HsFormula -> HVM Loc
injectHsFormula book hsForm = do
  let coreForm = hsToCoreFormula hsForm
  -- We need a starting Loc to inject into; 0 is often the root,
  -- but for multiple injections, we need fresh HVM heap space.
  -- For now, let's assume we get a fresh root loc.
  -- This part needs careful thought about HVM heap management for the prover.
  -- For Sprint 0, we might just inject everything starting at high Locs.
  -- A proper memory scheme for the prover's working set is for later.
  -- Let's placeholder with a call to allocNode for a single root,
  -- though what we really need is a dedicated area or a way to get a fresh pointer.
  -- This is a simplification for Sprint 0.
  newLoc <- allocNode 1 -- Placeholder for getting a location to write
  _ <- doInjectCoreAt book coreForm newLoc [] -- Args empty for simple term
  return newLoc

-- Function to extract an HsFormula from HVM given a Loc
extractHsFormula :: ReduceFn -> Book -> Loc -> HVM (Maybe HsFormula)
extractHsFormula reduceFn book loc = do
  coreTerm <- doExtractCoreAt reduceFn book loc -- Assuming reduceFn normalizes enough
  return $ coreToHsFormula coreTerm 