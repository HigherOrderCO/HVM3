#include "Runtime.h"

// Primitive: Dynamic Dup `@DUP(lab val λdp0λdp1(bod))`
// Creates a DUP node with given label.
Term DUP_f(Term ref) {
  HVM.interactions->ref_fast[DUP_F]++;
  Loc ref_loc = term_loc(ref);
  Term lab = reduce(got(ref_loc + 0));
  Term lab_val = term_loc(lab);
  if (term_tag(lab) != W32) {
    printf("ERROR:non-numeric-dup-label\n");
  }
  if (lab_val > 0xFFFF) {
    printf("ERROR:dup-label-too-large\n");
  }
  Term val = got(ref_loc + 1);
  Term bod = got(ref_loc + 2);
  Loc  dup = alloc_node(1);
  set(dup + 0, val);
  if (term_tag(bod) == LAM) {
    Loc  lam0 = term_loc(bod);
    Term bod0 = got(lam0 + 0);
    if (term_tag(bod0) == LAM) {
      Loc  lam1 = term_loc(bod0);
      Term bod1 = got(lam1 + 0);
      sub(lam0 + 0, term_new(DP0, lab_val, dup));
      sub(lam1 + 0, term_new(DP1, lab_val, dup));
      *HVM.itrs += 3;
      return bod1;
    }
  }
  Loc app0 = alloc_node(2);
  set(app0 + 0, bod);
  set(app0 + 1, term_new(DP0, lab_val, dup));
  Loc app1 = alloc_node(2);
  set(app1 + 0, term_new(APP, 0, app0));
  set(app1 + 1, term_new(DP1, lab_val, dup));
  *HVM.itrs += 1;
  HVM.interactions->ref_fall[DUP_F]++;
  return term_new(APP, 0, app1);
}

