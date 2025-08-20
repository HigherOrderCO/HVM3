#include "Runtime.h"

// (* a)
// ------- APP-ERA
// *
Term reduce_app_era(Term app, Term era) {
  inc_itr();
  HVM.interactions->app_era++;
  return era;
}
