# IC-Prove Setup Summary

## Completed Steps:

1. ✅ Created a new branch `feature/ic-prove`
2. ✅ Created the `src/Prover` directory for Haskell modules
3. ✅ Created the `examples` directory for HVM files
4. ✅ Created `examples/logic_syntax.hvm` with propositional logic syntax definitions
5. ✅ Created `examples/prover_rules.hvm` with a simple Modus Ponens implementation
6. ✅ Created Haskell module files:
   - ✅ `src/Prover/CoreSyntaxBridge.hs` - Implements the conversion between Haskell and HVM3 representations
   - ✅ `src/Prover/Search.hs` - Implements the A* skeleton for proof search
   - ✅ `src/Prover/Proof.hs` - Empty module for future implementation
7. ✅ Updated `HVM.cabal` to include the new Prover modules and dependencies (`heap` package)
8. ✅ Added a `prove` command to `app/Main.hs`

## Next Steps:

1. ⏳ Install Cabal (and GHC) if not already installed:
   - Option 1: Install GHCup from https://www.haskell.org/ghcup/
   - Option 2: `brew install ghc cabal-install` (on macOS with Homebrew)

2. ⏳ Install the required Haskell dependencies:
   ```
   cabal update
   cabal install heap --lib
   ```

3. ⏳ Build the project:
   ```
   cabal build
   ```

4. ⏳ Complete the successor generation part of the A* search in `src/Prover/Search.hs`

5. ⏳ Create a test file with a target formula:
   ```
   // examples/target_pimpq.hvm
   @target = @PropQ // We want to prove Q
   ```

6. ⏳ Test the prover:
   ```
   hvm3 prove examples/target_pimpq.hvm
   ```

## Additional Notes:

- The current implementation of equality checking in `prover_rules.hvm` is very simplistic and only handles atomic propositions. A more robust equality check will be needed for complex formulas.
- The A* search implementation will require further work to complete the successor generation part.
- The code uses several functions (`allocNode`, `hvmInit`, `hvmFree`, etc.) that may need to be properly imported or implemented.
- The `heap` package is required for the priority queue implementation in the A* search. 