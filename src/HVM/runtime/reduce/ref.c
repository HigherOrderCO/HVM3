#include "Runtime.h"

// @foo(a b c ...)
// -------------------- REF
// book[foo](a b c ...)
Term reduce_ref(Term ref) {
  inc_itr();
  return HVM.book[term_lab(ref)](ref);
}
