# HVM3 - Work In Progress

The **HVM** is an efficient implementation of the [Interaction Calculus](https://github.com/VictorTaelin/Interaction-Calculus) (IC).

The Interaction Calculus is a new foundation for computing, similar to the
Lambda Calculus, but theoretically optimal. The HVM is an efficient
implementation of this new paradigm, and can be seen as a fast engine for
symbolic computations.

In some ways, it is very similar to Haskell, but it has some key differences:
- Lambdas must be linear oraffine, making it resource-aware
- Lambdas have no scope boundaries, enabling global substitutions
- First-class duplications allow a term to be copied into to two locations
- First-class superpositions allow 2 terms to be stored in 1 location

These primitives allow HVM to natively represent concepts that are not present
in the traditional λ-Calculus, including continuations, linear HOAS interpreters
and mutable references. Moreover, superpositions and duplications allow it to
perform optimal beta-reduction, allowing some expressions to be evaluated with
an exponential speedup. Finally, being fully affine makes its garbage collector
very efficient, and greatly simplifies parallelism.

The HVM3 is the successor to HVM1 and HVM2, combining their strengths. It aims
to be the main compile target of [Bend](https://github.com/HigherOrderCO/Bend).
It is a WIP under active development.

## Specifications

- [IC.md](./IC.md): Interaction Calculus, the theoretical foundation behind HVM

- [HVM.md](./HVM.md): the HVM language, which extends IC with pragmatic primitives

## Install

1. Install Cabal.

3. Clone this repository.

3. Run `cabal install`.

## Usage

```bash
hvm run file.hvml    # runs interpreted (slow)
hvm run file.hvml -c # runs compiled (fast)
```

Note: the `-c` flag will also generate a standalone `.main.c` file, which if you
want, you can compile and run it independently. See examples on the [book/](book/) directory.


BROADER GOALS- we are forking this github to do the following:
Preamble: The Vision
This document outlines the foundational principles, architectural design, and implementation strategy for Project IC-Prove: an automated theorem prover built from the first principles of Informational Constructivism (IC). Our goal is to demonstrate that mathematical proof search can be framed as a resource-constrained, shortest-path problem within an operational framework defined by IC, using the HVM3 Interaction Calculus engine (as detailed in the accompanying HVM3 repository) as its computational substrate.
The core thesis is that "simplicity" in proof (reviving Hilbert's 24th problem) can be quantified as the minimal computational cost (Δ-cost, measured in HVM3 interactions) to construct a target theorem-record from foundational distinctions. This approach aims to not only create a novel prover but also to provide an empirical and constructive lens on the nature of mathematical truth as observer-relative and cost-dependent.
Target Audience: This document is intended for the development team tasked with building IC-Prove, comprising software engineers and mathematically-inclined individuals, as well as for stakeholders interested in the theoretical underpinnings and practical realization of this project.
Part I: Conceptual Foundations of Informational Constructivism (IC) & the Prover
(This section is a highly compressed summary of the "Informational Constructivism — Core Axiom Set + Implications" manuscript, focusing on aspects directly relevant to the prover.)
1. Core Principle: Distinction and Construction
1.1. Primacy of Distinction: The universe of discourse begins with the ability to make distinctions. IC postulates an infinite set of irreducible "Types of Distinction" (
S
0
=
{
U
i
}
S 
0
​
 ={U 
i
​
 }
). These are the atomic, non-decomposable qualitative building blocks.
Prover Implication: 
S
0
S 
0
​
 
 provides the fundamental "alphabet" for all symbols (propositional variables, constants, etc.) in our logical language.
1.2. Emergence of Quantity ((\Delta_{gen})): Repetition of a single Type 
U
i
U 
i
​
 
, 
n
n
 times, generates the concept of quantity ((\hat{n})). The set of these quantities forms the natural numbers ((\mathbb{N}_0^{\text{IC}})).
Prover Implication: Numerals and basic arithmetic structures are constructed, not assumed. Their representation in the prover will reflect this constructive origin.
1.3. Emergence of Multiplicative Structure ((\Delta_{compare}, \Delta_{factor})): Comparing and factoring quantities ((\hat{n})) reveals primes as multiplicatively irreducible counts.
Prover Implication: Arithmetic reasoning within the prover (e.g., for Presburger arithmetic or number theory fragments) will be built from these constructive operations, each with an associated cost.
1.4. The Integer Structure Grid (ISG): A coordinate system 
(
x
,
y
)
(x,y)
 based on prime factorizations (
n
=
∏
p
i
e
i
⇒
x
=
max
⁡
e
i
,
y
=
∑
e
i
n=∏p 
i
e 
i
​
 
​
 ⇒x=maxe 
i
​
 ,y=∑e 
i
​
 
), mapping all composed structures.
Prover Implication: While not directly part of the prover's core search logic for general theorems, the ISG represents the broader space of potential structured numerical objects the prover might reason about. Its principles of structural complexity (depth 
x
x
 vs. volume 
y
y
) might inform heuristics for number-theoretic proofs.
2. The IC Observer and Constraints
2.1. Observer 
O
O
: A finite system that applies (\Delta)-operations (distinction-making operations).
2.2. Constraint Vector 
C
=
(
c
s
,
c
d
,
c
t
)
C=(c 
s
​
 ,c 
d
​
 ,c 
t
​
 )
:
c
s
c 
s
​
 
: Storage capacity (bits).
c
d
c 
d
​
 
: Max derivation/recursion depth for self-referential operations (like (\Delta_{\text{self}})).
c
t
c 
t
​
 
: Compute/time budget per (\Delta)-step or for a global task.
Prover Implication: 
c
s
c 
s
​
 
 bounds the number of formulas, lemmas, and proof states held in memory. 
c
d
c 
d
​
 
 limits proof depth or complexity of recursive rule applications. 
c
t
c 
t
​
 
 limits total search effort or per-step complexity. Provability becomes relative to 
C
C
.
2.3. Memory Graph 
G
O
G 
O
​
 
: A directed acyclic graph (DAG) where nodes are "records" (results of (\Delta)-operations) and edges are derivation links. 
G
O
G 
O
​
 
 evolves under constraints.
Prover Implication: 
G
O
G 
O
​
 
 is the state space for proof search. Records will represent logical formulas, known lemmas, and applied rules. Edges represent inference steps.
2.4. (\Delta)-Operations, Cost, and (\Delta_{self}): The observer uses (\Delta)-operators (like (\Delta_{gen}, \Delta_{compare}, \Delta_{factor}), and higher-level ones like (\Delta_{apply}, \Delta_{abstract}) for the prover) to transform and create records. All operations have a cost.
IC Cost Convention (Summary): Primitive IC operations (e.g., READ_PTR, WRITE_REC, COMPARE_BITS(k), HASH(k), ARITHMETIC_OP, FACTORIZE(n)) have defined baseline costs. The cost of a (\Delta)-operator is the sum of these primitive costs. (Refer to IC Manuscript, Axiom 1, Cost Convention Box for details). This IC-level cost will be mapped to HVM3 interaction counts.
(\Delta_{self}) allows the observer to reflect on its own records to create new ones (e.g., abstracting a proof path into a lemma).
Prover Implication: The prover applying an inference rule to derive a new formula is a specific, structured (\Delta)-operation ((\Delta_{apply})). Compressing a found proof into a reusable lemma is (\Delta_{abstract}).
3. The Prover's Goal in IC Terms
Target: A specific "theorem-record" (an HVM3 term representing the formula to be proven).
Proof: A finite, costed path in 
G
O
G 
O
​
 
 (represented by HVM3 state transitions) from "axiom-records" (foundational, low-cost derivable truths like 
A
→
A
A→A
, or primitive distinctions 
U
i
≠
U
j
U 
i
​
 

=U 
j
​
 
) to the theorem-record.
Optimality: The "simplest proof" is the path with the minimal total (\Delta)-cost (measured in HVM3 interactions).
Graded Mathematics: What is provable (i.e., a reachable record within budget 
C
C
) depends on 
C
C
. A weaker observer (smaller 
C
C
) proves a subset of what a stronger observer can.
Part II: The HVM3 Interaction Calculus Engine
(A compressed summary of HVM3, as specified in the project's IC.md, HVM.md, and key aspects of src/HVM/Type.hs and src/HVM/Runtime.c.)
1. HVM3 as the Computational Substrate for IC (\Delta)-Operations
1.1. Interaction Calculus (ICalc): A minimal, affine term rewriting system (TRS) with global variables, lambdas (LAM), applications (APP), superpositions (SUP), and duplications (DUP). (Key aspects from IC.md). Affine means variables are used at most once unless explicitly duplicated.
1.2. HVM3 Runtime: An efficient, extended implementation of ICalc. Adds constructors (CTR), pattern matching (MAT, SWI, IFL), unboxed numbers (W32, CHR), operations (OPX, OPY), references to global functions (REF), etc. (Key aspects from HVM.md).
Prover Implication: ICalc + HVM3 extensions provide the "assembly language" for our prover's (\Delta)-operators. The affine nature means if a proof rule uses a premise multiple times, the HVM3 function implementing it must explicitly take multiple copies or use HVM3's DUP operation, all contributing to cost.
1.3. Term Representation (src/HVM/Type.hs, HVM.md Memory Layout):
Haskell Core type: AST for HVM terms (Var, Ref, Era, Lam, App, Sup, Dup, Ctr, U32, etc.).
Runtime Term: A Word64 encoding tag|lab|val. Nodes are blocks in memory managed by Runtime.c.
Prover Implication: IC "records" (formulas, rules) will be represented as HVM3 Core terms, which are then injected into the HVM3 runtime and manipulated as Terms by Runtime.c.
2. Key HVM3 Mechanisms for the Prover
2.1. Reduction (src/HVM/Reduce.hs, Runtime.c): The process of applying ICalc interaction rules to normalize terms. reduceAt (Haskell interpreter version) and reduceAtC (C runtime version via FFI) are key entry points.
Prover Implication: A sequence of HVM3 reductions represents the execution of a (\Delta)-operation.
2.2. Interaction Counting (getItr in src/HVM/Foreign.hs): The C runtime meticulously counts every primitive interaction rule application.
Prover Implication: This itrs count is the direct, operational measure of Δ-cost for the prover.
2.3. Superposition and Collapse (Sup term, src/HVM/Collapse.hs):
Sup Lab Term0 Term1: Represents a choice or superposition of two terms. HVM3 can evaluate terms containing Sup nodes, effectively exploring multiple computational paths simultaneously.
Collapse.hs: Provides Haskell functions (collapseDupsAt, collapseSups, flatten) to extract distinct, normalized results from a computation that involved superpositions.
Prover Implication: This is potentially a highly powerful mechanism for efficient search. Instead of the A* controller in Haskell making one choice (e.g., which rule to apply, which premise to use) and then backtracking, HVM3 could explore many such choices "simultaneously" using Sup. Invalid proof branches would naturally terminate in ERA or fail unification checks within HVM3, being pruned automatically.
2.4. Defining Functions (@name(args) = body syntax): HVM3 allows defining reusable functions, which can be parsed (Parse.hs) and then called (REF nodes).
Prover Implication: Inference rules ((\Delta_{apply})) and derived lemmas ((\Delta_{abstract})) will be implemented as HVM3 functions.
2.5. Memory Management (allocNode, getLen in Foreign.hs): The C runtime handles heap allocation for HVM3 terms.
Prover Implication: This corresponds to the construction of the IC observer's memory graph 
G
O
G 
O
​
 
. getLen can be used to monitor 
c
s
c 
s
​
 
 (storage constraint).
2.6. Haskell-C Interface (src/HVM/Foreign.hs, Inject.hs, Extract.hs): Allows a Haskell program to control the C runtime, inject HVM3 terms into its heap, trigger reductions, and extract resulting terms.
Prover Implication: The A* search controller (to be written in Haskell) will use this FFI extensively.
Part III: IC-Prove Architecture and Implementation Plan
1. Core Objective:
Implement a theorem prover where:
Proof search is finding the shortest (minimal HVM3 interaction cost) path in an HVM3-managed state graph.
States are derived HVM3 Formula terms (or sets of derived terms).
Transitions are applications of HVM3 functions representing inference rules (our (\Delta_{apply})).
2. Key Software Components:
A. HVM3 Logic Library (.hvm files):
logic_syntax.hvm: Defines HVM3 data for Formula { #Atom ... #And ... }, Prop { #P ... }, etc., to represent logical syntax.
prover_rules.hvm: Defines HVM3 functions like @apply_ModusPonens(formula_A_implies_B, formula_A) -> formula_B_or_Era. These functions will perform unification (using HVM3 MAT, SWI, or custom HVM3 code) and construct the consequent term. Their HVM3 itrs cost upon execution is their edge weight in the proof search.
standard_axioms.hvm: A collection of foundational HVM3 Formula terms (e.g., for propositional logic, Peano arithmetic identities) that are considered "given" (i.e., they are starting points for the search, perhaps with a very low or zero initial derivation cost).
B. Haskell Prover Engine (New modules, e.g., in src/Prover/):
Prover.CoreSyntaxBridge.hs:
Functions to translate between an abstract logical syntax (e.g., a simple Haskell ADT for formulas/rules if needed for clarity in the Haskell controller) and their HVM3 Core term representations.
Utilities for injecting these Core terms into the HVM3 runtime via HVM.Inject and extracting them for analysis/display via HVM.Extract.
Prover.Search.hs:
Implements the A* (or IDA*) search algorithm.
Search Node: Could be (formula_loc :: Loc, g_cost :: Word64, h_cost :: Word64, path_to_node :: ProofPath). formula_loc points to an HVM3 term on the HVM3 heap representing a derived formula. ProofPath would store the sequence of rules and parent Locs.
Successor Generation:
For a current formula F (represented by formula_s_loc on the HVM3 heap), iterate through available inference rules (HVM3 functions like @apply_ModusPonens defined in prover_rules.hvm).
For each rule, identify how F could be one of its premises. Find other needed premises from the set of already proven formulas (these are also HVM3 terms on the heap, whose Locs are managed by A*'s closed set or a similar structure). The Haskell A* controller will need to maintain a mapping from canonical formula representations (e.g., extracted Core terms, possibly alpha-normalized) to their HVM3 Locs to enable reuse.
Construct an HVM3 App (Ref "@rule_name" fid_for_rule) [premise_1_loc, premise_2_loc, ...] term representing the rule application.
Inject this application term into a new HVM3 heap location (worker_loc) using HVM.Inject.
Record old_itrs <- getItr.
Execute the rule application: result_term_at_worker_loc <- reduceAtC book worker_loc. (Use reduceAtC for speed; reduceAt from HVM.Reduce might be useful for debugging Haskell-side interaction logic if any is developed).
Record new_itrs <- getItr.
edge_cost = new_itrs - old_itrs. This is the actual computational cost of that inference step.
If the result_term_at_worker_loc is not an ERA term (HVM3's erasure/failure marker), then a new formula has been derived. Extract it (if needed for the heuristic or closed set), and add this new state (the Loc of the result, new_g_cost = g_cost(S) + edge_cost, etc.) to A*'s open set.
Heuristic Function h(formula_loc): Takes an HVM3 Loc, uses HVM.Extract.doExtractCoreAt to get the Core term, analyzes its syntactic difference from the target theorem (e.g., number of mismatched subformulas, difference in logical operators, complexity difference), and returns an estimated cost-to-go. Must be admissible for A* optimality.
Constraint Management:
c
t
c 
t
​
 
 (Compute Budget): Integrated into A* as a global limit on total g_cost or per-expansion edge_cost.
c
s
c 
s
​
 
 (Storage Budget): Monitor HVM3 heap size via HVM.Foreign.getLen. If exceeded, A* might need strategies like pruning less promising branches from its open set and instructing HVM3 to garbage collect (if/when available) or by overwriting "forgotten" term Locs with ERA.
c
d
c 
d
​
 
 (Depth Budget): Managed by A* path depth, can terminate branches exceeding a certain length.
Prover.Proof.hs:
Data structures for representing a completed proof path (a sequence of rule applications, HVM3 Locs of intermediate formulas, and associated costs).
Functions to pretty-print proofs or translate them into external formats (e.g., Lean/Coq scripts, natural deduction text).
C. Main Application (Modifications to app/Main.hs):
Add a prove command: hvm3 prove --target <target_formula.hvm> --axioms <axioms.hvm> --rules <rules.hvm> --budget_ct <c_t_limit> [--budget_cs <c_s_limit>] [--budget_cd <c_d_limit>].
This command would:
Initialize the HVM3 runtime (hvmInit).
Parse the specified HVM files to populate the Book (containing formula definitions, rule functions, axiom terms).
Set up the initial search state for A* (inject axioms into HVM3 heap, get their Locs; inject target theorem).
Run the A* search loop from Prover.Search.hs.
Output the result (proof found or budget exhausted).
Clean up (hvmFree).
3. Workflow for a Proof Attempt:
User specifies axioms, rules, and a target theorem (all as .hvm files containing HVM3 term definitions or functions).
app/Main.hs (via the new prove command) parses these files, creating an HVM3 Book.
Prover.Search.hs initializes A*:
Axiom formulas are injected into the HVM3 heap. Their Locs, along with initial costs (e.g., 0), form the initial A* search states.
The target theorem is also injected, and its Loc (or its extracted Core structure) is used for the goal test.
The A* search loop (in Haskell) begins:
It pops the most promising state (lowest f = g+h cost) from its open set. This state includes an HVM3 Loc pointing to a derived formula.
It generates successor states by attempting to apply all known inference rules (which are HVM3 functions like @apply_RuleX).
For each rule application:
The Haskell controller constructs the HVM3 App term for the rule call (e.g., App (Ref "@RuleX" ...) [premise1_loc, premise2_loc]).
This App term is injected into the HVM3 heap at a new worker_loc.
The HVM3 runtime is called (e.g., reduceAtC book worker_loc) to evaluate this rule application.
The number of HVM3 interactions (delta_itrs) for this reduction is the cost of this proof step (edge cost).
If the HVM3 reduction yields a new, non-ERA formula, this formula (now at worker_loc or a location it reduced to) becomes part of a new A* search state.
The loop continues until the target formula is derived or the resource budget 
C
C
 is exhausted.
If the goal formula's Loc is reached, Prover.Proof.hs reconstructs the path from the A* search (sequence of HVM3 rule function calls and intermediate formula Locs) and outputs it.
4. Role of HVM3 Superposition (Advanced/Optional Initial Phase):
The enum_*.hvm examples demonstrate HVM3's capability to search/evaluate superposed terms. This is a powerful concept from Interaction Calculi.
Instead of the Haskell A* controller making one choice at a time (e.g., which rule to apply next), one could construct a single, large, superposed HVM3 term. This term would represent, for example, "try applying Rule1 OR Rule2 OR Rule3 to the current formula(s)".
HVM3 would then reduce this superposed term. Branches of the superposition that lead to contradictions or rule application failures would naturally reduce to ERA (HVM3's erasure constructor), effectively being pruned by the HVM3 runtime itself.
HVM.Collapse.flatten could then be used to extract all successful derivation paths from the final superposed result.
Benefit: Potentially massive parallelism in exploring proof branches, handled efficiently by HVM3's core mechanics.
Challenge: Designing these "uber-terms" and the logic to control them (e.g., ensuring affine variable use across superposed branches, implementing equality checks like @eq over complex superposed formulas) would be advanced HVM3 programming. This is likely a "Phase 2" optimization after a simpler Haskell-controlled A* is functional.
5. Lemma Generation ((\Delta_{abstract})) (Advanced):
If A* finds a proof for a (sub)goal L, this proof path (which is a sequence of HVM3 operations and intermediate terms) can be encapsulated.
IC Concept: This is (\Delta_{abstract}), creating a new, compressed record of the derivation.
HVM3 Implementation:
A new HVM3 function, say @lemma_L(args) = ...body_of_the_proof_path_as_HVM3_code..., can be dynamically constructed (as a Core term in Haskell, then injected).
This @lemma_L can be added to the HVM3 Book.
HVM.Compile.compileBook could then (in theory, if HVM3 supports dynamic recompilation or loading) compile this new lemma into the C runtime, making its future applications very fast (cost of a REF call + argument handling).
This process would effectively "learn" lemmas, making subsequent proofs that use them cheaper.
Part IV: Key Challenges and Design Decisions
Defining "Formula Equality" in HVM3: For A*'s closed set and goal test, determining if two HVM3 terms (potentially at different Locs) represent the same logical formula is critical. This may require:
Extracting terms to Core via HVM.Extract and performing AST comparison in Haskell (potentially slow for many checks).
Implementing a robust @eq(formula1, formula2) -> Bool_Term HVM3 function. This function would need to handle issues like (\alpha)-equivalence for bound variables if the formula representation uses HVM3 Lam for binders.
Unification in HVM3: Rule application (@apply_RuleX) will require unification to match rule patterns against existing formulas. This will likely involve HVM3's pattern matching constructs (MAT, SWI) for terms representing formulas. Complex unification logic might be computationally expensive if implemented purely in HVM3 and could be a candidate for C-side helper functions via FFI if it becomes a bottleneck.
Heuristic Design for A* (h function): A good, admissible heuristic is crucial for A* performance. Developing this for logical formulas will require experimentation.
Managing HVM3 Heap (
c
s
c 
s
​
 
): A long proof search can generate many intermediate HVM3 formula terms, consuming heap space.
HVM3's affine nature means terms that are no longer referenced are implicitly garbage. However, the A* search algorithm (in Haskell) might hold onto Locs in its open/closed sets, keeping those terms alive.
Strategies for explicit "forgetting" (pruning from A* sets and potentially marking HVM3 terms for collection if a precise GC were available, or overwriting with ERA to free nodes) will be vital for managing the 
c
s
c 
s
​
 
 constraint.
Bridging IC Costs and HVM3 Costs: Ensure the abstract IC "Cost Convention" aligns with HVM3 itrs. For example, is comparing two N-bit numbers in HVM3 via an OPX node truly proportional to 
N
N
, as IC might assume in its cost model? This needs careful thought and possibly calibration (e.g., weighting different HVM3 interactions if necessary, though using raw itrs is the simplest start).
Scope and Variable Binding: HVM3's Interaction Calculus has global affine variables. Standard logical systems have scoped variables and non-affine use. Representing logical quantifiers (∀, ∃) and their bound variables using HVM3 Lam (which introduces a new variable scope for its body) is a natural approach but requires careful handling during unification and substitution in rule applications to maintain correctness (e.g., avoiding capture, ensuring freshness). The enum_lam_smart.hvm and enum_coc_smart.hvm examples, which manage contexts for binders in superposed computations, offer valuable insights.
Debugging and Introspection: Debugging a system that spans Haskell control logic and HVM3 C runtime execution, especially with superpositions, can be challenging. Developing good logging, visualization (of HVM3 heap snippets or proof search states), and step-through capabilities (even if slow, via reduceAt in Haskell) will be important.
Part V: Next Steps & Suggested "Sprint 0"
Sprint 0 Goal: Prove a very simple propositional logic theorem (e.g., from axioms 
P
P
 and 
P
→
Q
P→Q
, derive 
Q
Q
; or prove 
P
→
P
P→P
) using the basic A* architecture controlled from Haskell.
Tasks for Sprint 0:
Define HVM3 data Formula: For atomic propositions (e.g., #P, #Q) and implication (#Imp {ante, cons}). Store in logic_syntax.hvm.
Write HVM3 functions for basic rules:
@apply_Assumption(formula_A): Simply returns formula_A (if it's in the axiom set).
@apply_ModusPonens_simple(formula_A_implies_B, formula_A): A simplified version that assumes exact structural matches for A and A \to B and returns B.
Store these in prover_rules.hvm.
Implement a basic A* in Haskell (Prover.Search.hs):
Can initialize HVM3, load the .hvm files into a Book.
Inject a small set of axiom formulas (e.g., #P, #Imp{#P, #Q}) and a target formula (e.g., #Q) into HVM3, getting their Locs.
The search loop should call the HVM3 rule functions via FFI as described in Part III.2.B.
Use HVM3 itrs as edge costs.
For the goal test and closed set, it can initially rely on HVM.Extract.doExtractCoreAt to get Core terms and compare their ASTs (simplistic, will need refinement for alpha-equivalence later).
If successful, output the trace of HVM3 rule functions called and intermediate formula Locs.
This refined document should provide a robust starting point.
Glossary (Selected Terms)
IC (Informational Constructivism): The theoretical framework.
S
0
S 
0
​
 
: IC's foundational infinite set of irreducible "Types of Distinction."
(\Delta)-operation: A fundamental operation in IC that creates or transforms distinctions/records (e.g., (\Delta_{gen}), (\Delta_{compare}), (\Delta_{apply})).
Record (IC): A piece of information or structure stored by an IC observer. Mapped to HVM3 terms.
G
O
G 
O
​
 
: The Memory Graph of an IC observer, a DAG of records and derivation links. Mapped to the HVM3 heap state.
C
=
(
c
s
,
c
d
,
c
t
)
C=(c 
s
​
 ,c 
d
​
 ,c 
t
​
 )
: The IC constraint vector (storage, depth, time/compute).
Δ-cost: The computational cost of a (\Delta)-operation, measured in primitive IC operations or, for IC-Prove, in HVM3 interactions.
HVM3: The Higher-Order Virtual Machine, an efficient runtime for the Interaction Calculus.
Interaction Calculus (ICalc): The minimal term rewriting system underlying HVM3.
Term (HVM3): The fundamental data unit in HVM3, represented as a Word64 pointer/value or as a Core AST in Haskell.
Interaction (HVM3): A single application of an HVM3 reduction rule (e.g., APP-LAM, DUP-SUP).
itrs (HVM3): The count of HVM3 interactions, used as the measure for Δ-cost.
Loc (HVM3): A memory location (address) on the HVM3 heap.
CTR (HVM3): Constructor node, used for algebraic data types (e.g., Formula, Prop).
LAM (HVM3): Lambda abstraction. Can be used to represent logical binders.
APP (HVM3): Application.
SUP (HVM3): Superposition node, allows multiple terms to exist at one "location" conceptually.
DUP (HVM3): Duplication node, for explicit copying (due to affine nature).
ERA (HVM3): Erasure/Nil term. Used to signify failure or pruned branches.
REF (HVM3): Reference to a globally defined HVM3 function (e.g., @apply_ModusPonens).
Book (HVM3): A collection of HVM3 function definitions and data type declarations.
