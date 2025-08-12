#include "Runtime.h"

// ! &L{a b} = ↑x / ↓x
// ------------- DUP-INC/DEC
// ! &L{A B} = x
// a <- ↑A / ↓A
// b <- ↑B / ↓B
Term reduce_dup_una(Term dup, Term una, Tag tag) {
  inc_itr();
  Loc dup_loc = term_loc(dup);
  Lab lab     = term_lab(dup);
  Loc una_loc = term_loc(una);
  Term inner  = got(una_loc + 0);

  // duplicate inner value
  Loc du_loc = una_loc;
  Loc w0_loc = alloc_node(1);
  Loc w1_loc = alloc_node(1);

  // wrap duplicates in INC / DEC
  set(w0_loc + 0, term_new(DP0, lab, du_loc));
  set(w1_loc + 0, term_new(DP1, lab, du_loc));

  if (term_tag(dup) == DP0) {
    sub(dup_loc + 0, term_new(tag, 0, w1_loc));
    return term_new(tag, 0, w0_loc);
  } else {
    sub(dup_loc + 0, term_new(tag, 0, w0_loc));
    return term_new(tag, 0, w1_loc);
  }
}

Term reduce_dup_inc(Term dup, Term inc) {
  return reduce_dup_una(dup, inc, INC);
}

Term reduce_dup_dec(Term dup, Term dec) {
  return reduce_dup_una(dup, dec, DEC);
}
