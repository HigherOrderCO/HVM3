#include "Runtime.h"

// ! x = val
// bod
// --------- LET
// x <- val
// bod
Term reduce_let(Term let, Term val) {
  inc_itr();
  Loc let_loc = term_loc(let);
  Term bod    = got(let_loc + 1);
  sub(let_loc + 0, val);
  return bod;
}
