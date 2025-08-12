#include "Runtime.h"

// &L(123 a)
// --------- APP-W32
// ⊥
Term reduce_app_w32(Term app, Term w32) {
  printf("invalid:app-w32(%llu)", (unsigned long long)term_loc(w32));
  exit(0);
}
