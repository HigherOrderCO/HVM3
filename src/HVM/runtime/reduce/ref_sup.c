#include "Runtime.h"

// @foo(&L{ax ay} b c ...)
// ----------------------- REF-SUP-COPY (when @L not in @foo)
// ! &L{bx by} = b
// ! &L{cx cy} = b
// ...
// &L{@foo(ax bx cx ...) @foo(ay by cy ...)}
Term reduce_ref_sup(Term ref, u16 idx) {
  inc_itr();
  Loc ref_loc = term_loc(ref);
  Lab ref_lab = term_lab(ref);
  u16 fun_id = ref_lab;
  u16 arity  = HVM.fari[fun_id];
  HVM.interactions->ref_sup[fun_id]++;
  if (idx >= arity) {
    printf("ERROR: Invalid index in reduce_ref_sup\n");
    exit(1);
  }
  Term sup = got(ref_loc + idx);
  if (term_tag(sup) != SUP) {
    printf("ERROR: Expected SUP at index %u\n", idx);
    exit(1);
  }
  Lab sup_lab = term_lab(sup);
  Loc sup_loc = term_loc(sup);
  Term sup0 = got(sup_loc + 0);
  Term sup1 = got(sup_loc + 1);
  // Allocate space for new REF node arguments for the second branch
  Loc ref1_loc = alloc_node(arity);
  for (u64 i = 0; i < arity; ++i) {
    if (i != idx) {
      // Duplicate argument
      Term arg = got(ref_loc + i);
      Loc dup_loc = alloc_node(1);
      set(dup_loc + 0, arg);
      set(ref_loc + i, term_new(DP0, sup_lab, dup_loc));
      set(ref1_loc + i, term_new(DP1, sup_lab, dup_loc));
    } else {
      // Set the SUP components directly
      set(ref_loc + i, sup0);
      set(ref1_loc + i, sup1);
    }
  }
  // Create new REF nodes
  Term ref0 = term_new(REF, ref_lab, ref_loc);
  Term ref1 = term_new(REF, ref_lab, ref1_loc);
  // Reuse sup_loc to create the new SUP node
  set(sup_loc + 0, ref0);
  set(sup_loc + 1, ref1);
  return term_new(SUP, sup_lab, sup_loc);
}
