# IC-Prove: An Optimal Theorem Prover

The IC-Prove extension adds automated theorem proving capabilities to HVM3, implemented as an optimal A* search over the space of HVM3 interactions.

## Structure

- `src/Prover/CoreSyntaxBridge.hs`: Translates between Haskell formulas and HVM3 terms
- `src/Prover/Search.hs`: Implements A* search for proofs
- `src/Prover/Proof.hs`: Will contain proof representation and output utilities

## HVM Files

- `examples/logic_syntax.hvm`: Defines the syntax for propositional logic
- `examples/prover_rules.hvm`: Defines inference rules like Modus Ponens
- `examples/axioms.hvm`: Example axioms as HVM terms
- `examples/target_pimpq.hvm`: Example theorem to prove (Q from P and P→Q)
- `examples/test_prover.hvm`: Test file for running directly with HVM3 run command

## Usage

To run the prover:

```bash
hvm3 prove examples/target_pimpq.hvm -b1000000
```

Or use the provided test script:

```bash
./test_prove.sh
```

## Sprint 0 Status

The current implementation has the following components:

- ✅ Basic data structures defined
- ✅ A* search algorithm structure in place
- ✅ HVM3 rule application system implemented
- ✅ Successor generation for Modus Ponens
- ✅ HVM3 integration framework in place

Remaining to be done:

- More robust formula equality checking
- Support for more inference rules
- Improved HVM memory management
- Better heuristics for the A* search
- Real integration with axiom and target files

## Implementation Details

### Successor Generation

The successor generation in the A* search works as follows:

1. For each node in the search frontier, we try to apply the Modus Ponens rule:
   - If the current formula is of the form A→B, we look for A in the known formulas
   - If the current formula is A, we look for formulas of the form A→B
   
2. Each rule application is done by:
   - Constructing an HVM term that calls the `@apply_ModusPonens` function
   - Injecting this term into the HVM heap
   - Reducing the term using the HVM reducer
   - Measuring the number of interactions used (cost)
   - Extracting the result if successful

3. The cost of a proof step is measured in HVM interactions, providing a direct
   metric for the "Δ-cost" from Informational Constructivism.

### HVM Interaction

The prover uses several HVM3 mechanisms:
- `allocNode` for creating space on the heap
- `doInjectCoreAt` for putting terms on the heap
- `doExtractCoreAt` for retrieving terms
- `reduceAt` or `reduceAtC` for evaluating terms
- `getItr` for measuring computational cost

## Planned Improvements

In future sprints, we will:
1. Implement a full suite of logical rules
2. Improve memory management to avoid re-injecting terms
3. Develop better heuristics for the A* search
4. Add visualization of proofs
5. Support more complex logical systems 