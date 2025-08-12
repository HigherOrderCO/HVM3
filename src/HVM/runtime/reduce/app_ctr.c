#include "Runtime.h"

// &L(#{x y z ...} a)
// ------------------ APP-CTR
// ⊥
Term reduce_app_ctr(Term app, Term ctr) {
  printf("invalid:app-ctr(%lu)", (unsigned long)term_lab(ctr));
  exit(0);
}
