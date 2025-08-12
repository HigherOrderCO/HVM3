#include "Runtime.h"

// Heap counters
void set_len(u64 size) { *HVM.size = size; }
void set_itr(u64 itrs) { *HVM.itrs = itrs; }
u64  get_len() { return *HVM.size; }
u64  get_itr() { return *HVM.itrs; }
u64  fresh()   { return (*HVM.frsh)++; }

// Atomics
Term swap(Loc loc, Term term) {
  Term val = HVM.heap[loc];
  HVM.heap[loc] = term;
  if (val == 0) {
    printf("SWAP 0 at %08llx\n", (u64)loc);
    exit(0);
  }
  return val;
}

Term got(Loc loc) {
  Term val = HVM.heap[loc];
  if (val == 0) {
    printf("GOT 0 at %08llx\n", (u64)loc);
    exit(0);
  }
  return val;
}

void set(Loc loc, Term term) { HVM.heap[loc] = term; }
void sub(Loc loc, Term term) { set(loc, term_set_bit(term)); }
Term take(Loc loc) { return swap(loc, VOID); }

// Allocation and accounting
Loc alloc_node(Loc arity) {
  if (*HVM.size + arity > MAX_HEAP_SIZE) {
    printf("Heap memory limit exceeded\n");
    exit(1);
  }
  u64 old = *HVM.size;
  *HVM.size += arity;
  return old;
}

void inc_itr() { (*HVM.itrs)++; }

