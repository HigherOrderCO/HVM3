# The Interaction Calculus

The Interaction Calculus is a minimal term rewriting system inspired by the
Lambda Calculus (λC), but with some key differences:
1. Vars are affine: they can only occur up to one time.
2. Vars are global: they can occur anywhere in the program.
3. There is a new core primitive: the superposition.

An HVM core term is defined by the following grammar:

```
Term ::=
  | VAR: Name
  | ERA: "*"
  | LAM: "λ" Name "." Term
  | APP: "(" Term " " Term ")"
  | SUP: "&" Label "{" Term "," Term "}"
  | DUP: "!" "&" Label "{" Name "," Name "}" "=" Term ";" Term
```

Where:
- VAR represents a variable.
- ERA represents an erasure.
- LAM represents a lambda.
- APP represents a application.
- SUP represents a superposition.
- DUP represents a duplication.

Lambdas are curried, and work like their λC counterpart, except with a relaxed
scope, and with affine usage. Applications eliminate lambdas, like in λC,
through the beta-reduce (APP-LAM) interaction.

Superpositions work like pairs. Duplications eliminate superpositions through
the DUP-SUP interaction, which works exactly like a pair projection.

What makes SUPs and DUPs unique is how they interact with LAMs and APPs. When a
SUP is applied to an argument, it reduces through the APP-SUP interaction, and
when a LAM is projected, it reduces through the DUP-LAM interaction. This gives
a computational behavior for every possible interaction: there are no runtime
errors on the core Interaction Calculus.

The 'Label' is a numeric value that affects some interactions, like DUP-SUP,
causing terms to commute, instead of annihilate. Read Lafont's Interaction
Combinators paper to learn more.

The core interaction rules are listed below:

```
(* a)
----- APP-ERA
*

(λx.f a)
-------- APP-LAM
x <- a
f

(&L{a,b} c)
----------------- APP-SUP
! &L{c0,c1} = c;
&L{(a c0),(b c1)}

! &L{r,s} = *;
K
-------------- DUP-ERA
r <- *
s <- *
K

! &L{r,s} = λx.f;
K
----------------- DUP-LAM
r <- λx0.f0
s <- λx1.f1
x <- &L{x0,x1}
! &L{f0,f1} = f;
K

! &L{x,y} = &L{a,b};
K
-------------------- DUP-SUP (if equal labels)
x <- a
y <- b
K

! &L{x,y} = &R{a,b};
K
-------------------- DUP-SUP (if different labels)
x <- &R{a0,b0} 
y <- &R{a1,b1}
! &L{a0,a1} = a;
! &L{b0,b1} = b;
K
```

Where `x <- t` stands for a global substitution of `x` by `t`.

Since variables are affine, substitutions can be implemented efficiently by just
inserting an entry in a global substitution map (`sub[var] = value`). There is
no need to traverse the target term, or to handle name capture, as long as fresh
variable names are globally unique. Thread-safe substitutions can be performed
with a single atomic-swap.

Below is a pseudocode implementation of these interaction rules:

```
def app_lam(app, lam):
  sub[lam.nam] = app.arg
  return lam.bod

def app_sup(app, sup):
  x0 = fresh()
  x1 = fresh()
  a0 = App(sup.lft, Var(x0))
  a1 = App(sup.rgt, Var(x1))
  return Dup(sup.lab, x0, x1, app.arg, Sup(a0, a1))

def dup_lam(dup, lam):
  x0 = fresh()
  x1 = fresh()
  f0 = fresh()
  f1 = fresh()
  sub[dup.lft] = Lam(x0, Var(f0))
  sub[dup.rgt] = Lam(x1, Var(f1))
  sub[lam.nam] = Sup(dup.lab, Var(x0), Var(x1))
  return Dup(dup.lab, f0, f1, lam.bod, dup.bod)

def dup_sup(dup, sup):
  if dup.lab == sup.lab:
    sub[dup.lft] = sup.lft
    sub[dup.rgt] = sup.rgt
    return dup.bod
```

Terms can be reduced to weak head normal form, which means reducing until the
outermost constructor is a value (LAM, SUP, etc.), or until no more reductions
are possible. Example:

```
def whnf(term):
  while True:
    match term:
      case Var(nam):
        if nam in sub:
          term = sub[nam]
        else:
          return term
      case App(fun, arg):
        fun = whnf(fun)
        match fun.tag:
          case LAM: term = app_lam(term, fun)
          case SUP: term = app_sup(term, fun)
          case _  : return App(fun, arg)
      case Dup(lft, rgt, val, bod):
        val = whnf(val)
        match val.tag:
          case LAM: term = dup_lam(term, val)
          case SUP: term = dup_sup(term, val)
          case _  : return Dup(lft, rgt, val, bod)
      case _:
        return term
```

Terms can be reduced to full normal form by recursively taking the whnf:

```
def normal(term):
  term = whnf(term)
  match term:
    case Lam(nam, bod):
      bod_nf = normal(bod)
      return Lam(nam, bod_nf)
    case App(fun, arg):
      fun_nf = normal(fun)
      arg_nf = normal(arg)
      return App(fun_nf, arg_nf)
    ...
    case _:
      return term
```

Below are some normalization examples.

Example 0: (simple λ-term)

```
(λx.λt.(t x) λy.y)
------------------ APP-LAM
λt.(t λy.y)
```

Example 1: (larger λ-term)

```
(λb.λt.λf.((b f) t) λT.λF.T)
---------------------------- APP-LAM
λt.λf.((λT.λF.T f) t)
----------------------- APP-LAM
λt.λf.(λF.t f)
-------------- APP-LAM
λt.λf.t
```

Example 2: (global scopes)

```
{x,(λx.λy.y λk.k)}
------------------ APP-LAM
{λk.k,λy.y}
```

Example 3: (superposition)

```
!{a,b} = {λx.x,λy.y}; (a b)
--------------------------- DUP-SUP
(λx.x λy.y)
----------- APP-LAM
λy.y
```

Example 4: (overlap)

```
({λx.x,λy.y} λz.z)
------------------ APP-SUP  
! {x0,x1} = λz.z; {(λx.x x0),(λy.y x1)}  
--------------------------------------- DUP-LAM  
! {f0,f1} = {r,s}; {(λx.x λr.f0),(λy.y λs.f1)}  
---------------------------------------------- DUP-SUP  
{(λx.x λr.r),(λy.y λs.s)}  
------------------------- APP-LAM  
{λr.r,(λy.y λs.s)}  
------------------ APP-LAM  
{λr.r,λs.s}  
```

Example 5: (default test term)

The following term can be used to test all interactions:

```
((λf.λx.!{f0,f1}=f;(f0 (f1 x)) λB.λT.λF.((B F) T)) λa.λb.a)
----------------------------------------------------------- 16 interactions
λa.λb.a
```

# Collapsing

An Interaction Calculus term can be collapsed to a superposed tree of pure
Lambda Calculus terms without SUPs and DUPs, by extending the evaluator with the
following collapse interactions:

```
λx.*
------ ERA-LAM
x <- *
*

(f *)
----- ERA-APP
*

λx.&L{f0,f1}
----------------- SUP-LAM
x <- &L{x0,x1}
&L{λx0.f0,λx1.f1}

(f &L{x0,x1})
------------------- SUP-APP
!&L{f0,f1} = f;
&L{(f0 x0),(f1 x1)}

&R{&L{x0,x1},y}
----------------------- SUP-SUP-X (if R>L)
!&R{y0,y1} = y;
&L{&R{x0,x1},&R{y0,y1}}

&R{x,&L{y0,y1}}
----------------------- SUP-SUP-Y (if R>L)
!&R{x0,x1} = x;
&L{&R{x0,x1},&R{y0,y1}}

!&L{x0,x1} = x; K
----------------- DUP-VAR
x0 <- x
x1 <- x
K

!&L{a0,a1} = (f x); K
--------------------- DUP-APP
a0 <- (f0 x0)
a1 <- (f1 x1)
!&L{f0,f1} = f;
!&L{x0,x1} = x;
K
```
