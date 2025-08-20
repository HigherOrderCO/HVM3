#include "Runtime.h"

// Primitive: Dynamic Sup `@SUP(lab tm0 tm1)`
// Allocates a new SUP node with given label.
Term SUP_f(Term ref) {
  Loc ref_loc = term_loc(ref);
  Term lab = reduce(got(ref_loc + 0));
  Term lab_val = term_loc(lab);
  if (term_tag(lab) != W32) {
    printf("ERROR:non-numeric-sup-label\n");
  }
  if (lab_val > 0xFFFF) {
    printf("ERROR:sup-label-too-large\n");
  }
  Term tm0 = got(ref_loc + 1);
  Term tm1 = got(ref_loc + 2);
  Loc  sup = alloc_node(2);
  Term ret = term_new(SUP, lab_val, sup);
  set(sup + 0, tm0);
  set(sup + 1, tm1);
  *HVM.itrs += 1;
  HVM.interactions->ref_fast[SUP_F]++;
  return ret;
}

