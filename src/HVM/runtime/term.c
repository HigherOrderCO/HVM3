// Term encoding and helpers
// -------------------------
// Layout (least-significant bit on the right):
//   [ 63 ............... 24 ][ 23 .... 8 ][ 7 ][ 6 .... 0 ]
//        location (40b)          lab(16b)  S        tag(7b)
//   - tag: 7-bit node tag (see Runtime.h constants)
//   - S  : substitution/"sub" bit (1 when value is substituted)
//   - lab: constructor/operator label (16 bits)
//   - loc: heap location / payload (40 bits)

#include "Runtime.h"

// Bit masks and shifts for clarity (no behavior change)
#define TAG_MASK   (0x7FULL)
#define SUB_MASK   (1ULL << 7)
#define LAB_MASK   (0xFFFFULL)
#define LOC_MASK   (0xFFFFFFFFFFULL)
#define LAB_SHIFT  (8)
#define LOC_SHIFT  (24)

Term term_new(Tag tag, Lab lab, Loc loc) {
  return ((Term)tag)
       | ((Term)lab << LAB_SHIFT)
       | ((Term)loc << LOC_SHIFT);
}

Tag term_tag(Term x) {
  return (Tag)(x & TAG_MASK);
}

Lab term_lab(Term x) {
  return (Lab)((x >> LAB_SHIFT) & LAB_MASK);
}

Loc term_loc(Term x) {
  return (Loc)((x >> LOC_SHIFT) & LOC_MASK);
}

u64 term_get_bit(Term x) {
  return (x >> 7) & 1ULL;
}

Term term_set_bit(Term x) {
  return x | SUB_MASK;
}

Term term_rem_bit(Term x) {
  return x & ~SUB_MASK;
}

Term term_set_loc(Term x, Loc loc) {
  return (x & 0x0000000000FFFFFFULL) | ((Term)loc << LOC_SHIFT);
}

_Bool term_is_atom(Term t) {
  Tag tg = term_tag(t);
  return (tg == ERA) || (tg == W32) || (tg == CHR);
}
