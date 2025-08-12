#include "Runtime.h"

// ! &L{x y} = 123
// --------------- DUP-W32
// x <- 123
// y <- 123
Term reduce_dup_w32(Term dup, Term w32) {
  inc_itr();
  Loc dup_loc = term_loc(dup);
  sub(dup_loc + 0, w32);
  return w32;
}
