#include "Runtime.h"

// ! &L{x y} = @foo(a b c ...)
// --------------------------- DUP-REF-COPY (when &L not in @foo)
// ! &L{a0 a1} = a
// ! &L{b0 b1} = b
// ! &L{c0 c1} = c
// ...
// x <- @foo(a0 b0 c0 ...)
// y <- @foo(a1 b1 c1 ...)
Term reduce_dup_ref(Term dup, Term ref) {
  inc_itr();
  Loc dup_loc = term_loc(dup);
  Lab dup_lab = term_lab(dup);
  Loc ref_loc = term_loc(ref);
  Lab ref_lab = term_lab(ref);
  u64 ref_ari = HVM.fari[ref_lab];

  Loc loc     = alloc_node(ref_ari * 2);
  Loc ref0    = ref_loc;
  Loc ref1    = loc + 0;
  for (u64 i = 0; i < ref_ari; i++) {
    Loc du0 = loc + ref_ari + i;
    set(du0 + 0, got(ref_loc + i));
    set(ref0 + i, term_new(DP0, dup_lab, du0));
    set(ref1 + i, term_new(DP1, dup_lab, du0));
  }
  if (term_tag(dup) == DP0) {
    sub(dup_loc + 0, term_new(REF, ref_lab, ref1));
    return term_new(REF, ref_lab, ref0);
  } else {
    sub(dup_loc + 0, term_new(REF, ref_lab, ref0));
    return term_new(REF, ref_lab, ref1);
  }
}
