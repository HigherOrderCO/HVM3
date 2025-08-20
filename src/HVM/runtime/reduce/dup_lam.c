#include "Runtime.h"

// ! &L{r s} = λx(f)
// ------------------- DUP-LAM
// ! &L{f0 f1} = f
// r <- λx0(f0)
// s <- λx1(f1)
// x <- &L{x0 x1}
Term reduce_dup_lam(Term dup, Term lam) {
  inc_itr();
  HVM.interactions->dup_lam++;
  Loc dup_loc = term_loc(dup);
  Loc lam_loc = term_loc(lam);
  Lab dup_lab = term_lab(dup);

  Term bod    = got(lam_loc + 0);
  
  Loc loc     = alloc_node(5);
  Loc lm0     = loc + 0;
  Loc lm1     = loc + 1;
  Loc su0     = loc + 2;
  Loc du0     = loc + 4;

  sub(lam_loc + 0, term_new(SUP, dup_lab, su0));

  set(lm0 + 0, term_new(DP0, dup_lab, du0));
  set(lm1 + 0, term_new(DP1, dup_lab, du0));
  set(su0 + 0, term_new(VAR, 0, lm0));
  set(su0 + 1, term_new(VAR, 0, lm1));
  set(du0 + 0, bod);

  if (term_tag(dup) == DP0) {
    sub(dup_loc + 0, term_new(LAM, 0, lm1));
    return term_new(LAM, 0, lm0);
  } else {
    sub(dup_loc + 0, term_new(LAM, 0, lm0));
    return term_new(LAM, 0, lm1);
  }
}
