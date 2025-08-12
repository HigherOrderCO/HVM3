#include "Runtime.h"

// ! &L{x y} = *
// ------------- DUP-ERA
// x <- *
// y <- *
Term reduce_dup_era(Term dup, Term era) {
  inc_itr();
  Loc dup_loc = term_loc(dup);
  sub(dup_loc + 0, era);
  return era;
}
