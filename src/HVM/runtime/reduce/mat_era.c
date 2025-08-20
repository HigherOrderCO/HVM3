#include "Runtime.h"

// ~ * {K0 K1 K2 ...} 
// ------------------ MAT-ERA
// *
Term reduce_mat_era(Term mat, Term era) {
  inc_itr();
  HVM.interactions->mat_era++;
  return era;
}
