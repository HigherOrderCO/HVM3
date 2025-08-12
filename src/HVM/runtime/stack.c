#include "Runtime.h"

void spush(Term term, Term* sbuf, u64* spos) {
  if (*spos >= MAX_STACK_SIZE) {
    printf("Stack memory limit exceeded\n");
    exit(1);
  }
  sbuf[(*spos)++] = term;
}

Term spop(Term* sbuf, u64* spos) {
  return sbuf[--(*spos)];
}

