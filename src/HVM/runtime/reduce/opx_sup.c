#include "Runtime.h"

// <op(&L{x0 x1} y)
// ------------------------- OPX-SUP
// ! &L{y0 y1} = y
// &L{<op(x0 y0) <op(x1 y1)}
Term reduce_opx_sup(Term opx, Term sup) {
  inc_itr();
  HVM.interactions->opx_sup++;
  Loc opx_loc = term_loc(opx);
  Loc sup_loc = term_loc(sup);
  Lab sup_lab = term_lab(sup);
  Term nmy    = got(opx_loc + 1);
  Term tm0    = got(sup_loc + 0);
  Term tm1    = got(sup_loc + 1);
  Loc loc     = alloc_node(3);
  Loc op0     = opx_loc;
  Loc op1     = sup_loc;
  Loc su0     = loc + 0;
  Loc du0     = loc + 2;
  set(op0 + 0, tm0);
  set(op0 + 1, term_new(DP0, sup_lab, du0));
  set(op1 + 0, tm1);
  set(op1 + 1, term_new(DP1, sup_lab, du0));
  set(su0 + 0, term_new(OPX, term_lab(opx), op0));
  set(su0 + 1, term_new(OPX, term_lab(opx), op1));
  set(du0 + 0, nmy);
  return term_new(SUP, sup_lab, su0);
}
