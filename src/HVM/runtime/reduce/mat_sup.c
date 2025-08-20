#include "Runtime.h"

// ~ &L{x y} {K0 K1 K2 ...}
// ------------------------ MAT-SUP
// ! &L{k0a k0b} = K0
// ! &L{k1a k1b} = K1
// ! &L{k2a k2b} = K2
// ...
// &L{ ~ x {K0a K1a K2a ...}
//     ~ y {K0b K1b K2b ...} }
Term reduce_mat_sup(Term mat, Term sup) {
  inc_itr();
  HVM.interactions->mat_sup++;
  Tag mat_tag = term_tag(mat);
  Lab mat_lab = term_lab(mat);
  Loc mat_loc = term_loc(mat);
  Loc sup_loc = term_loc(sup);
  Lab sup_lab = term_lab(sup);

  Term tm0    = got(sup_loc + 0);
  Term tm1    = got(sup_loc + 1);
  u64 mat_len = mat_tag == SWI ? mat_lab : mat_tag == IFL ? 2 : HVM.clen[mat_lab];

  Loc loc     = alloc_node(1 + mat_len + mat_len);
  Loc mat0    = mat_loc;
  Loc mat1    = loc + 0;
  Loc sup0    = sup_loc;

  set(mat0 + 0, tm0);
  set(mat1 + 0, tm1);
  for (u64 i = 0; i < mat_len; i++) {
    Loc du0 = loc + 1 + mat_len + i;
    set(du0 + 0, got(mat_loc + 1 + i));
    set(mat0 + 1 + i, term_new(DP0, sup_lab, du0));
    set(mat1 + 1 + i, term_new(DP1, sup_lab, du0));
  }
  set(sup0 + 0, term_new(mat_tag, mat_lab, mat0));
  set(sup0 + 1, term_new(mat_tag, mat_lab, mat1));
  return term_new(SUP, sup_lab, sup0);
}
