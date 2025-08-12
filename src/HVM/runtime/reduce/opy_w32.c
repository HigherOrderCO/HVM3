#include "Runtime.h"

// >op(x y)
// --------- OPY-W32
// x op y
Term reduce_opy_w32(Term opy, Term w32) {
  inc_itr();
  Loc opy_loc = term_loc(opy);
  Tag t = term_tag(w32);
  u32 x = term_loc(got(opy_loc + 1));
  u32 y = term_loc(w32);
  u32 result;
  switch (term_lab(opy)) {
    case OP_ADD: result = x + y; break;
    case OP_SUB: result = x - y; break;
    case OP_MUL: result = x * y; break;
    case OP_DIV: result = x / y; break;
    case OP_MOD: result = x % y; break;
    case OP_EQ:  result = x == y; break;
    case OP_NE:  result = x != y; break;
    case OP_LT:  result = x < y; break;
    case OP_GT:  result = x > y; break;
    case OP_LTE: result = x <= y; break;
    case OP_GTE: result = x >= y; break;
    case OP_AND: result = x & y; break;
    case OP_OR:  result = x | y; break;
    case OP_XOR: result = x ^ y; break;
    case OP_LSH: result = x << y; break;
    case OP_RSH: result = x >> y; break;
    default: {
      printf("invalid:opy-w32");
      exit(0);
    }
  }
  return term_new(t, 0, result);
}
