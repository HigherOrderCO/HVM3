#include "Runtime.h"

// >op(a *)
// -------- OPY-ERA
// *
Term reduce_opy_era(Term opy, Term era) {
  inc_itr();
  HVM.interactions->opy_era++;
  return era;
}
