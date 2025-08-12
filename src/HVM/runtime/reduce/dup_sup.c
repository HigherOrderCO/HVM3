#include "Runtime.h"

// ! &L{x y} = &R{a b}
// ------------------- DUP-SUP
// if L == R:
//   x <- a
//   y <- b
// else:
//   x <- &R{a0 b0} 
//   y <- &R{a1 b1}
//   ! &L{a0 a1} = a
//   ! &L{b0 b1} = b
Term reduce_dup_sup(Term dup, Term sup) {
  inc_itr();
  Loc dup_loc = term_loc(dup);
  Lab dup_lab = term_lab(dup);
  Lab sup_lab = term_lab(sup);
  Loc sup_loc = term_loc(sup);
  if (dup_lab == sup_lab) {
    Term tm0 = got(sup_loc + 0);
    Term tm1 = got(sup_loc + 1);
    if (term_tag(dup) == DP0) {
      sub(dup_loc + 0, tm1);
      return tm0;
    } else {
      sub(dup_loc + 0, tm0);
      return tm1;
    }
  } else {
    Loc loc = alloc_node(4);
    Loc du0 = sup_loc + 0;
    Loc du1 = sup_loc + 1;
    Loc su0 = loc + 0;
    Loc su1 = loc + 2;
    set(su0 + 0, term_new(DP0, dup_lab, du0));
    set(su0 + 1, term_new(DP0, dup_lab, du1));
    set(su1 + 0, term_new(DP1, dup_lab, du0));
    set(su1 + 1, term_new(DP1, dup_lab, du1));
    if (term_tag(dup) == DP0) {
      sub(dup_loc + 0, term_new(SUP, sup_lab, su1));
      return term_new(SUP, sup_lab, su0);
    } else {
      sub(dup_loc + 0, term_new(SUP, sup_lab, su0));
      return term_new(SUP, sup_lab, su1);
    }
  }
}
