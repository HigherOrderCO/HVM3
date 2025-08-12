#include "Runtime.h"

// <op(↑x y) / <op(↓x y)  →  ↑<op(x y) / ↓<op(x y)
Term reduce_opx_una(Term opx, Term una, Tag tag) {
  inc_itr();
  Loc opx_loc = term_loc(opx);
  Loc una_loc = term_loc(una);
  Term lhs    = got(una_loc + 0);
  Term rhs    = got(opx_loc + 1);         // already stored

  set(opx_loc + 0, lhs);
  set(opx_loc + 1, rhs);
  set(una_loc + 0, term_new(OPX, term_lab(opx), opx_loc));
  return una;
}

Term reduce_opx_inc(Term opx, Term inc) {
  return reduce_opx_una(opx, inc, INC);
}

Term reduce_opx_dec(Term opx, Term dec) {
  return reduce_opx_una(opx, dec, DEC);
}
