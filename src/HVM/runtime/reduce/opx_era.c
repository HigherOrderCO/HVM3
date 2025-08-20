#include "Runtime.h"

// <op(* b)
// -------- OPX-ERA
// *
Term reduce_opx_era(Term opx, Term era) {
  inc_itr();
  HVM.interactions->opx_era++;
  return era;
}
