#include "Runtime.h"

// Compact, consistent term helpers
Term term_new(Tag tag, Lab lab, Loc loc) {
  return ((Term)(tag)) | ((Term)(lab) << 8) | ((Term)(loc) << 32);
}

Tag term_tag(Term x) { return (Tag)(x & 0x7FULL); }
Lab term_lab(Term x) { return (Lab)((x >> 8) & 0xFFFFFFULL); }
Loc term_loc(Term x) { return (Loc)((x >> 32) & 0xFFFFFFFFULL); }
u64 term_get_bit(Term x) { return (x >> 7) & 1ULL; }
Term term_set_bit(Term x) { return x | (1ULL << 7); }
Term term_rem_bit(Term x) { return x & ~(1ULL << 7); }
Term term_set_loc(Term x, Loc loc) { return (x & 0x00000000FFFFFFFFULL) | ((Term)(loc) << 32); }

_Bool term_is_atom(Term t) {
  Tag tg = term_tag(t);
  return (tg == ERA) || (tg == W32) || (tg == CHR);
}
