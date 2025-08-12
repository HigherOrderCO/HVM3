#include "Runtime.h"

void print_tag(Tag tag) {
  switch (tag) {
    case VAR: printf("VAR"); break;
    case DP0: printf("DP0"); break;
    case DP1: printf("DP1"); break;
    case APP: printf("APP"); break;
    case LAM: printf("LAM"); break;
    case ERA: printf("ERA"); break;
    case SUP: printf("SUP"); break;
    case REF: printf("REF"); break;
    case LET: printf("LET"); break;
    case CTR: printf("CTR"); break;
    case MAT: printf("MAT"); break;
    case IFL: printf("IFL"); break;
    case SWI: printf("SWI"); break;
    case W32: printf("W32"); break;
    case CHR: printf("CHR"); break;
    case OPX: printf("OPX"); break;
    case OPY: printf("OPY"); break;
    case INC: printf("INC"); break;
    case DEC: printf("DEC"); break;
    default : printf("???"); break;
  }
}

void print_term(Term term) {
  printf("term_new(");
  print_tag(term_tag(term));
  printf(",0x%06llx,0x%08llx)", (u64)term_lab(term), (u64)term_loc(term));
}

void print_heap() {
  for (Loc i = 0; i < *HVM.size; i++) {
    Term term = got(i);
    if (term != 0) {
      printf("set(0x%08llx, ", (u64)i);
      print_term(term);
      printf(");\n");
    }
  }
}

