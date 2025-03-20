# HVM

The HVM is a extension, and efficient runtime, for the Interaction Calculus.

## Project Organization

- `README.md`: introduction and general information

- `IC.md`: full spec of the Interaction Calculus (read it!)

- `HVM.md`: full spec of the HVM runtime (read it!)

- `Inters.md`: the complete interaction table

- `examples/`: many example `.hvm` files

- `src/`: Haskell and C implementation
  - `Type.hs`: defines the Term and Book types, used in all files (read it!)
  - `Show.hs`: converts a Term to a String
  - `Parse.hs`: converts a String to a Term
  - `Reduce.hs`: evaluates a Term to weak head normal form (WHNF)
  - `Inject.hs`: converts a Haskell-side Term to a C-side Term
  - `Extract.hs`: normalize and extracts a C-side Term into a Haskell-side IC Term
  - `Collapse.hs`: normalizes and collapses a C-side Term into a list of Haskell-side λC (i.e., dup/sup-free) Terms
  - `Foreign.hs`: imports C-side functions on Haskell-side
  - `Runtime.c`: the complete C runtime, including memory types, interactions, and a faster WHNF evaluator
  - `Compile.hs`: converts a top-level Book into a list of optimized, native C-code

- `dist/`, `dist-newstyle`: Haskell artifacts

## Memory Layout

On HVM, each Term is represented as a word, with the following fields:

- `sub`: true if this is a substitution
- `tag`: the tag identifying the term type
- `lab`: a label, used to trigger commutations
- `val`: the value (a node address, or an unboxed number)

The length of each field depends on the version:

- **32-bit**: 1-bit sub | 5-bit tag | 2-bit lab | 24-bit val
- **64-bit**: 1-bit sub | 5-bit tag | 18-bit lab | 40-bit val

The meaning of the val field depends on the term's tag, as follows:

Tag | ID   | Value points to / stores ...
--- | ---- | --------------------------------------
DP0 | 0x00 | Dup Node ({val: Term}) or substitution
DP1 | 0x01 | Dup Node ({val: Term}) or substitution
VAR | 0x02 | Lam Node ({bod: Term}) or substitution
FWD | 0x03 | TODO: document
REF | 0x04 | Ref Node ({arg0: Term, ... argN: Term})
LET | 0x05 | Let Node
APP | 0x06 | App Node ({fun: Term, arg: Term})
MAT | 0x08 | Mat Node
IFL | 0x09 | IfL Node
SWI | 0x0A | Swi Node
OPX | 0x0B | OpX Node
OPY | 0x0C | OpY Node
ERA | 0x0D | Unused
LAM | 0x0E | Lam Node ({bod: Term})
SUP | 0x0F | Sup Node
CTR | 0x10 | Ctr Node ({x0: Term, ... xN: Term})
W32 | 0x11 | Unboxed U32 Number
CHR | 0x12 | Unboxed U32 Number

A Node is a consecutive block of its child terms. For example, the SUP term
points to the memory location where its two child terms are stored.

Variable terms (`VAR`, `DP0`, and `DP1`) point to an entry on the subst map. As
an optimization, HVM doesn't have a separate subst map. Instead, variables point
to the location of the corresponding binder node (like a Lam or Dup). When an
interaction occurs, that location is reused as a subst map entry, and we set the
'sub' bit of the stored term to '1'. When a variable points to a term with the
bit flag set, we it is a substitution, so we retrieve it and clear the flag.

Note that there is no explicit DUP term. That's because Dup nodes are special:
they aren't part of the AST, and they don't store a body; they "float" on the
heap.  In other words, `λx. !&0{x0,x1}=x; &0{x0,x1}` and `!&0{x0,x1}=x; λx.
&0{x0,x1}` are both valid, and stored identically in memory. As such, the only
way to access a Dup node is via its bound variables, `DP0` and `DP1`.

Note that, when a Dup Node interacts, it usually generates two substitutions.
So, how can we store them in its location, given that a Dup Node has only one
word? The answer is: we don't. Dup Nodes only interact when we access them
via either a `DP0` or a `DP1`. As such, we immediatelly return one of the
substitutions to the variable that triggered the interaction, and store the
other substitution on the Dup Node's location.

For example, the DUP-SUP interaction could be implemented as:

```
def dup_sup(dup, sup):
  dup_lab = dup.tag & 0x3
  sup_lab = sup.tag & 0x3
  if dup_lab == sup_lab:
    tm0 = heap[sup.loc + 0]
    tm1 = heap[sup.loc + 1]
    heap[dup.loc] = as_sub(tm1 if (dup.tag & 0x4) == 0 else tm0)
    return (tm0 if (dup.tag & 0x4) == 0 else tm1)
  else:
    co0_loc = alloc(1)
    co1_loc = alloc(1)
    su0_loc = alloc(2)
    su1_loc = alloc(2)
    su0_val = Term(SP0 + sup_lab, su0_loc)
    su1_val = Term(SP0 + sup_lab, su1_loc)
    heap[co0_loc] = heap[sup.loc + 0]
    heap[co1_loc] = heap[sup.loc + 1]
    heap[su0_loc + 0] = Term(CX0 + dup_lab, co0_loc)
    heap[su0_loc + 1] = Term(CX0 + dup_lab, co1_loc)
    heap[su1_loc + 0] = Term(CY0 + dup_lab, co0_loc)
    heap[su1_loc + 1] = Term(CY0 + dup_lab, co1_loc)
    heap[dup.loc] = as_sub(su1_val if (dup.tag & 0x4) == 0 else su0_val)
    return (su0_val if (dup.tag & 0x4) == 0 else su1_val)
```

Note that HVM extends the Interaction Calculus with many new types and
interactions. The complete Interaction Table is at INTERS.md.

## C FFI and Compiler

In this project, Terms have two representations:

- A Haskell-side representation (the Term type on `Type.hs`)

- A C-side representation (the pointer format specified on `HVM.md`)

Functions are implemented on Haskell, C, or both:

- Interactions are exclusively implemented on C, except for CALL.

- The WHNF function is implemented on both Haskell and C.

- All other functions are implemented on Haskell only.

There are two evaluation modes:

- Interpreted Mode:
  - uses Haskell-side parser, WHNF, collapser, and stringifier
  - uses Haskell-side CALL interaction
  - uses C-side for other interactions

- Compiled Mode:
  - uses Haskell-side parser, collapser and strinigifier
  - uses C-side CALL interaction
  - uses C-side WHNF and interactions

To run HVM3 on Compiled Mode, we generate a new copy of `Runtime.c` with the
compiled functions inlined, and then compile with GCC and reload as a dylib.
This allows the C-side WHNF to dispatch CALL interactions to native C
procedures, which run much faster than `inject`.

## The CALL interaction

The CALL interaction performs a global function call. For example, given:

```
@mul2(x) = ~ x { 0:0 p:(+ 2 @mul2(p)) }
```

When we evaluate the expression:

```
@foo(4)
```

It will expand to:

```
~ 4 { 0:0 p:(+ 2 @mul2(p)) }
```

On Interpreted Mode, this is done by `inject`, which allocates the body of the
function, substituting variables by the respective arguments.

On Compiled Mode, this is done by calling a native C function, via two paths:

`Slow Path`: a C function that just allocates the body, like `inject`. Example:

```c
Term mul2_t(Term ref) {
  Term arg0 = got(term_loc(ref) + 0);
  Loc mat1 = alloc_node(3);
  set_new(mat1 + 0, arg0);
  set_new(mat1 + 1, term_new(W32, 0, 0));
  Loc lam2 = alloc_node(1);
  Loc opx3 = alloc_node(2);
  Loc ref4 = alloc_node(1);
  set_new(ref4 + 0, term_new(VAR, 0, lam2 + 0));
  set_new(opx3 + 0, term_new(W32, 0, 2));
  set_new(opx3 + 1, term_new(REF, 0, ref4));
  set_new(lam2 + 0, term_new(OPX, 0, opx3));
  set_new(mat1 + 2, term_new(LAM, 0, lam2));
  return term_new(SWI, 2, mat1);
}
```

`Fast Path`: a C function that attempts to perform *inline interactions*,
avoiding allocating extra memory. For example, in the `mul2` case, it will check
if the argument is a number. If so, it will perform a native C switch, instead
of allocating a `SWI` node. It also performs inline arithmetic and even loops,
when the function is tail-call recursive. Example:

```c
Term mul2_f(Term ref) {
  u64 itrs = 0;
  Term arg0 = got(term_loc(ref) + 0);
  while (1) {
    Term val1 = (arg0);
    if (term_tag(val1) == W32) {
      u32 num2 = term_loc(val1);
      switch (num2) {
        case 0: {
          itrs += 1;
          *HVM.itrs += itrs;
          return term_new(W32, 0, 0);
          break;
        }
        default: {
          Term pre3 = term_new(W32, 0, num2 - 1);
          itrs += 2;
          Loc ref8 = alloc_node(1);
          set_new(ref8 + 0, pre3);
          Term nu06 = (term_new(W32, 0, 2));
          Term nu17 = (term_new(REF, 0, ref8));
          Term ret5;
          if (term_tag(nu06) == W32 && term_tag(nu17) == W32) {
            itrs += 2;
            ret5 = term_new(W32, 0, term_loc(nu06) + term_loc(nu17));
          } else {
            Loc opx4 = alloc_node(2);
            set_new(opx4 + 0, nu06);
            set_new(opx4 + 1, nu17);
            ret5 = term_new(OPX, 0, opx4);
          }
          *HVM.itrs += itrs;
          return ret5;
          break;
        }
      }
    }
    set_old(term_loc(ref) + 0, arg0);
    return mul2_t(ref);
  }
}
```

# Parser

On HVM, all bound variables have global range. For example, consider the term:

```
λt.((t x) λx.λy.y)
```

Here, the `x` variable appears before its binder, `λx`. Since runtime variables
must point to their bound λ's, linking them correctly requires caution. A way to
do it is to store two structures at parse-time: a list from names to locations,
and a map from names to variable terms.

Whenever we parse a name, we add the current location to the 'uses' array, and
whenever we parse a binder (lams, lets, etc.), we add a variable term pointing
to it to the 'vars' map. Then, once the parsing is done, we run iterate through
the 'uses' array, and write, to each location, the corresponding term. Below
are some example parsers using this strategy:

```python
def parse_var(loc):
  nam = parse_name()
  uses.push((nam,loc))

def parse_lam(loc):
  lam = alloc(1)
  consume("λ")
  nam = parse_name()
  consume(".")
  vars[nam] = Term(VAR, 0, lam)
  parse_term(lam)
  heap[loc] = Term(LAM, 0, lam)

def parse_app(loc):
  app = alloc(2)
  consume("(")
  parse_term(app + 0)
  consume(" ")
  parse_term(app + 1)
  consume(")")
  heap[loc] = Term(APP, 0, app)

...
```

# Stringifier

Converting HVM terms to strings faces two challenges:

First, HVM terms and nodes don't store variable names. As such, we must
generate fresh, unique variable names during stringification, and maintain a
mapping from each binder's memory location to its assigned name.

Second, on HVM, Dup nodes aren't part of the main program's AST. Instead, they
"float" on the heap, and are only reachable via DP0 and DP1 variables. Because
of that, by stringifying a term naively, Col nodes will be missing.

To solve these, we proceed as follows:

1. Before stringifying, we pass through the full term, and assign a id to each
variable binder we find (on lam, let, dup, etc.)

2. We also register every Dup node we found, avoiding duplicates (remember the
same dup node is pointed to by up to 2 variables, DP0 and DP1)

Then, to stringify the term, we first stringify each DUP node, and then we
stringify the actual term. As such, the result will always be in the form:

! &{x0 x1} = t0
! &{x2 x3} = t1
! &{x4 x5} = t2
...
term

With no Dup nodes inside the ASTs of t0, t1, t2 ... and term.
