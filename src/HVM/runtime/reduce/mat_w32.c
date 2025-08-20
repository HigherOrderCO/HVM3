#include "Runtime.h"

// ~ num {K0 K1 K2 ... KN}
// ----------------------- MAT-W32
// if n < N: Kn
// else    : KN(num-N)
Term reduce_mat_w32(Term mat, Term w32) {
  inc_itr();
  HVM.interactions->mat_w32++;
  Loc mat_loc = term_loc(mat);
  Lab mat_lab = term_lab(mat);
  u64 mat_len = mat_lab;
  u64 w32_val = term_loc(w32);
  if (w32_val < mat_len - 1) {
    return got(mat_loc + 1 + w32_val);
  } else {
    Term fn = got(mat_loc + mat_len);
    Loc app = mat_loc;
    set(app + 0, fn);
    set(app + 1, term_new(W32, 0, w32_val - (mat_len - 1)));
    return term_new(APP, 0, app);
  }
}
