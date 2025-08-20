#include "Runtime.h"

// (&L{a b} c)
// --------------------- APP-SUP
// ! &L{x0 x1} = c
// &L{(a x0) (b x1)}
Term reduce_app_sup(Term app, Term sup) {
  inc_itr();
  HVM.interactions->app_sup++;
  Loc app_loc = term_loc(app);
  Loc sup_loc = term_loc(sup);
  Lab sup_lab = term_lab(sup);

  Term arg    = got(app_loc + 1);
  Term tm1    = got(sup_loc + 1);

  Loc loc = alloc_node(3);
  Loc ap0 = sup_loc;
  Loc ap1 = loc + 0;
  Loc su0 = app_loc;
  Loc dup = loc + 2;

  set(ap0 + 1, term_new(DP0, sup_lab, dup));

  set(ap1 + 0, tm1);
  set(ap1 + 1, term_new(DP1, sup_lab, dup));

  // Reuse app_loc for the result superposition
  set(su0 + 0, term_new(APP, 0, ap0));
  set(su0 + 1, term_new(APP, 0, ap1));

  set(dup + 0, arg);

  return term_new(SUP, sup_lab, su0);
}
