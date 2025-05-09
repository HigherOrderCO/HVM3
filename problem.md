# IC-Prove Implementation Issues

## Problem Description

The IC-Prove theorem prover extension for HVM3 is encountering several syntax and reference errors when attempting to build and run the prover.

### Error 1: HVM Syntax Error in Logic Definition

When running the prover, we get a syntax error in the `logic_syntax.hvm` file:

```
PARSE_ERROR (examples/logic_syntax.hvm)
- expected: "#" | "}"
- detected:
14 |   #FAtom(Prop)      // Atomic formula, e.g., FAtom(#P)
```

The issue appears to be with the constructor syntax. HVM3 has specific requirements for data type definitions, and the current syntax with parentheses for constructor arguments is causing problems.

### Error 2: Main Function Not Found

After the syntax error, we see:
```
Error: 'main' not found.
```

This suggests that either the parser fails to process the entire file due to the syntax error, or the `main` function is not correctly defined/exported.

### Error 3: Unbound Reference

When trying to run `test_prover.hvm` directly:
```
error:unbound-ref @P_implies_Q
```

This indicates that the reference to `@P_implies_Q` cannot be resolved, either because it's not defined or not correctly imported.

## Relevant Code

### examples/logic_syntax.hvm
```hvm
// examples/logic_syntax.hvm

// Propositional Variables (Atoms)
// We'll use simple constructors for now.
// #P, #Q, #R will represent atomic propositions P, Q, R.
data Prop { 
  #P // Represents proposition P
  #Q // Represents proposition Q
  #R // Represents proposition R 
}

// Logical Formulas
data Formula {
  #FAtom(Prop)      // Atomic formula, e.g., FAtom(#P)
  #FNot(Formula)   // Negation: ¬A
  #FAnd(Formula, Formula) // Conjunction: A ∧ B
  #FOr(Formula, Formula) // Disjunction: A ∨ B
  #FImp(Formula, Formula) // Implication: A → B
}

// Helper to create an implication easily
@Implies(a, b) = #FImp(a, b)

// Example formulas (can be used for testing later)
@PropP = #FAtom(#P)
@PropQ = #FAtom(#Q)
@P_implies_Q = @Implies(@PropP, @PropQ)

// Define main function for HVM3 to run
@main = @P_implies_Q 
```

### examples/prover_rules.hvm
```hvm
// examples/prover_rules.hvm
// Import the syntax
// (HVM3 doesn't have explicit imports in .hvm files for data, 
// the `Book` will combine them. Assume `Formula` and `Prop` are known.)

// @apply_ModusPonens(term_A_implies_B, term_A)
// If term_A_implies_B is of the form (A' -> B')
// And term_A is of the form A''
// And A' is structurally equal to A''
// Then return B', otherwise return ERA
@apply_ModusPonens(a_implies_b, a) = 
  ~ a_implies_b {
    #FImp(premise_ante, premise_cons): // a_implies_b is (premise_ante -> premise_cons)
      // Now we need to check if 'premise_ante' is equal to 'a'
      // For Sprint 0, we'll assume a very simple direct equality check.
      // A full @eq function for Formulas is complex and for a later sprint.
      // This simplified version only works if `premise_ante` and `a` are *identical pointer-wise*
      // or structurally identical simple atoms.
      @if(@eq_simple(premise_ante, a), premise_cons, *) // * is ERA
    _: * // a_implies_b was not an #FImp, so rule fails
  }

// Extremely simplified equality for atoms for now.
// This will need to be replaced with a recursive structural equality function.
@eq_simple(term1, term2) = 
  ~ term1 !term2 {
    #FAtom(p1): ~ term2 { 
      #FAtom(p2): @eq_prop(p1, p2)
      _: 0 // Not equal if not both FAtoms
    }
    // Add more cases for #FImp, #FAnd etc. as needed, returning 0 if types mismatch
    _: 0 // term1 is not an FAtom
  }

@eq_prop(prop1, prop2) =
  ~ prop1 !prop2 {
    #P: ~ prop2 { #P:1 _:0 }
    #Q: ~ prop2 { #Q:1 _:0 }
    #R: ~ prop2 { #R:1 _:0 }
    _:0
  }

// Rule for using an axiom/proven fact directly
// @use_Axiom(axiom_formula) = axiom_formula
// (This might not be needed as an HVM3 function; A* can just use existing facts)

// Main function for HVM3 to run
@main = @apply_ModusPonens(@P_implies_Q, @PropP) 
```

### examples/target_pimpq.hvm
```hvm
// examples/target_pimpq.hvm
// Target theorem to prove: Q
// We'll set up axioms P and P→Q elsewhere, and the prover should find
// that Q follows by Modus Ponens

// Define the main function to return our target formula
@main = @PropQ // From examples/logic_syntax.hvm 
```

## Root Cause Analysis

The primary issues appear to be:

1. **Incorrect HVM3 Syntax**: The data type constructors in `logic_syntax.hvm` are using parentheses for arguments, but HVM3 seems to expect a different syntax.

2. **Reference Resolution**: The prover tries to reference `@P_implies_Q` and other symbols that may not be correctly defined or imported.

3. **Book Merging Logic**: The `cliProve` function in `Main.hs` attempts to merge two HVM books (syntax and rules), but this might not be correctly handling all the necessary fields.

## Solution Path

1. Fix the data type constructor syntax in `logic_syntax.hvm` to match HVM3's requirements.
2. Ensure that required definitions like `@P_implies_Q` and `@PropQ` are properly defined and exported.
3. Check the book merging logic to ensure all necessary fields are correctly combined.
4. Verify that the theorem prover's A* search correctly interacts with the HVM3 runtime. 