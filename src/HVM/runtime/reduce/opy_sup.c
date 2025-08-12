#include "Runtime.h"

// >op(a &L{x y})
// --------------------- OPY-SUP
// &L{>op(a x) >op(a y)}
Term reduce_opy_sup(Term opy, Term sup) {
  inc_itr();
  Loc opy_loc = term_loc(opy);
  Loc sup_loc = term_loc(sup);
  Lab sup_lab = term_lab(sup);
  Term nmx    = got(opy_loc + 1);
  Term tm0    = got(sup_loc + 0);
  Term tm1    = got(sup_loc + 1);
  Loc op0     = sup_loc;
  Loc op1     = opy_loc;
  Loc su0     = alloc_node(2);
  set(op0 + 1, nmx);
  set(op1 + 0, tm1);
  set(su0 + 0, term_new(OPY, term_lab(opy), op0));
  set(su0 + 1, term_new(OPY, term_lab(opy), op1));
  return term_new(SUP, sup_lab, su0);
}
