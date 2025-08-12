#include "Runtime.h"

// ~(↑x) {…} / ~(↓x) {…}
//  →  ↑(~x {…}) / ↓(~x {…})
Term reduce_mat_una(Term mat, Term una, Tag tag) {
  inc_itr();
  Loc mat_loc = term_loc(mat);
  Loc una_loc = term_loc(una);
  Term inner  = got(una_loc + 0);

  set(mat_loc + 0, inner);       // plug x inside the matcher
  set(una_loc + 0, mat);         // re-attach wrapped matcher
  return una;
}

Term reduce_mat_inc(Term mat, Term inc) {
  return reduce_mat_una(mat, inc, INC);
}

Term reduce_mat_dec(Term mat, Term dec) {
  return reduce_mat_una(mat, dec, DEC);
}
