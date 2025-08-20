#include "Runtime.h"

// <op(x0 x1)
// ---------- OPX-W32
// >op(x0 x1)
Term reduce_opx_w32(Term opx, Term nmx) {
  inc_itr();
  HVM.interactions->opx_w32++;
  Lab opx_lab = term_lab(opx);
  Loc opx_loc = term_loc(opx);
  Term nmy = got(opx_loc + 1);
  set(opx_loc + 0, nmy);
  set(opx_loc + 1, nmx);
  return term_new(OPY, opx_lab, opx_loc);
}
