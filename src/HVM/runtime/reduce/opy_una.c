#include "Runtime.h"

// >op(a ↑y) / >op(a ↓y)  →  ↑>op(a y) / ↓>op(a y)
Term reduce_opy_una(Term opy, Term una, Tag tag) {
  inc_itr();
  Loc opy_loc = term_loc(opy);
  Loc una_loc = term_loc(una);
  Term rhs    = got(una_loc + 0);
  Term lhs    = got(opy_loc + 1);         // first operand stored at +1

  set(opy_loc + 0, rhs);
  set(opy_loc + 1, lhs);
  set(una_loc + 0, term_new(OPY, term_lab(opy), opy_loc));
  return una;
}

Term reduce_opy_inc(Term opy, Term inc) {
  return reduce_opy_una(opy, inc, INC);
}

Term reduce_opy_dec(Term opy, Term dec) {
  return reduce_opy_una(opy, dec, DEC);
}
