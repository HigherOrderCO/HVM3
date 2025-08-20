#include "Runtime.h"

State HVM = {
  .sbuf = NULL,
  .spos = NULL,
  .heap = NULL,
  .size = NULL,
  .itrs = NULL,
  .frsh = NULL,
  .book = {NULL},
  .cari = {0},
  .clen = {0},
  .cadt = {0},
  .fari = {0},
};

State* hvm_get_state() {
  return &HVM;
}

void hvm_set_state(State* hvm) {
  HVM.sbuf = hvm->sbuf;
  HVM.spos = hvm->spos;
  HVM.heap = hvm->heap;
  HVM.size = hvm->size;
  HVM.itrs = hvm->itrs;
  HVM.frsh = hvm->frsh;
  for (int i = 0; i < 65536; i++) {
    HVM.book[i] = hvm->book[i];
    HVM.fari[i] = hvm->fari[i];
    HVM.cari[i] = hvm->cari[i];
    HVM.clen[i] = hvm->clen[i];
    HVM.cadt[i] = hvm->cadt[i];
  }
  // Copy heatmap instrumentation state so compiled runtime writes into host buffers
  HVM.heat_enabled = hvm->heat_enabled;
  HVM.heat_w       = hvm->heat_w;
  HVM.heat_h       = hvm->heat_h;
  HVM.heat_itrs_max= hvm->heat_itrs_max;
  HVM.heat_mem_max = hvm->heat_mem_max;
  HVM.heat_reads   = hvm->heat_reads;
  HVM.heat_writes  = hvm->heat_writes;
}

void hvm_define(u16 fid, Term (*func)()) {
  HVM.book[fid] = func;
}

void hvm_set_cari(u16 cid, u16 arity) {
  HVM.cari[cid] = arity;
}

void hvm_set_fari(u16 fid, u16 arity) {
  HVM.fari[fid] = arity;
}

void hvm_set_clen(u16 cid, u16 cases) {
  HVM.clen[cid] = cases;
}

void hvm_set_cadt(u16 cid, u16 adt) {
  HVM.cadt[cid] = adt;
}
