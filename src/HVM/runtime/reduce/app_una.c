#include "Runtime.h"

// (↑f x) / (↓f x)
// ---------------- APP-INC/DEC
// ↑(f x) / ↓(f x)
Term reduce_app_una(Term app, Term una, Tag tag) {
  inc_itr();
  Loc app_loc = term_loc(app);
  Loc una_loc = term_loc(una);
  Term fun    = got(una_loc + 0);
  Term arg    = got(app_loc + 1);

  // build the inner application in-place, re-using app_loc
  set(app_loc + 0, fun);
  set(app_loc + 1, arg);

  // point INC/DEC to the freshly built APP
  set(una_loc + 0, term_new(APP, 0, app_loc));
  return una;
}

Term reduce_app_inc(Term app, Term inc) {
  return reduce_app_una(app, inc, INC);
}

Term reduce_app_dec(Term app, Term dec) {
  return reduce_app_una(app, dec, DEC);
}
