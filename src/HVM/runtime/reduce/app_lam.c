#include "Runtime.h"

// (λx(body) arg)
// ---------------- APP-LAM
// x <- arg
// body
Term reduce_app_lam(Term app, Term lam) {
  inc_itr();
  HVM.interactions->app_lam++;
  Loc app_loc = term_loc(app);
  Loc lam_loc = term_loc(lam);
  Term bod    = got(lam_loc + 0);
  Term arg    = got(app_loc + 1);
  sub(lam_loc + 0, arg);
  return bod;
}
