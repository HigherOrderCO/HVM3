# HVM - Interaction Table

TODO: this document is a WIP. It is not complete yet.

## Core Interactions

Lambdas and Superpositions:

```haskell
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

Numbers:

```haskell
+N
--- SUC-NUM
N+1

+*
-- SUC-ERA
*

+&L{x,y}
--------- SUC-SUP
&L{+x,+y}

?N{0:z;+:s;}
------------ SWI-NUM (if N==0)
z

?N{0:z;+:s;}
------------ SWI-NUM (if N>0)
(s N-1)

?*{0:z;+:s;}
------------ SWI-ERA
*

?&L{x,y}{0:z;+:s;}
--------------------------------- SWI-SUP
!&L{z0,z1} = z;
!&L{s0,s1} = s;
&L{?x{0:z0;+:s0;},?y{0:z1;+:s1;}}

! &L{x,y} = N;
K
-------------- DUP-NUM
x <- N
y <- N
K
```

## Collapsing Interactions

These interactions are NOT part of the WHNF. They're called by the collapser.

```haskell
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

~N{0:&L{z0,z1};+:s;}
--------------------------------- SUP-SWI-Z
!&L{N0,N1} = N;
!&L{S0,S1} = S;
&L{~N0{0:z0;+:S0},~N1{0:z1;+:S1}}

~N{0:z;+:&0{s0,s1};}
--------------------------------- SUP-SWI-S
!&L{N0,N1} = N;
!&L{Z0,Z1} = Z;
&L{~N0{0:z0;+:S0},~N1{0:z1;+:S1}}

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
