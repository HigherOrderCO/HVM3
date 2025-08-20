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
  .interactions = NULL
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
  HVM.interactions = hvm->interactions;
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

u64 hvm_get_let_lazy() { return HVM.interactions->let_lazy; }
u64 hvm_get_let_stri() { return HVM.interactions->let_stri; }
u64 hvm_get_app_era() { return HVM.interactions->app_era; }
u64 hvm_get_app_lam() { return HVM.interactions->app_lam; }
u64 hvm_get_app_sup() { return HVM.interactions->app_sup; }
u64 hvm_get_dup_era() { return HVM.interactions->dup_era; }
u64 hvm_get_dup_lam() { return HVM.interactions->dup_lam; }
u64 hvm_get_dup_sup_anni() { return HVM.interactions->dup_sup_anni; }
u64 hvm_get_dup_sup_comm() { return HVM.interactions->dup_sup_comm; }
u64 hvm_get_dup_ctr(u16 cid) { return HVM.interactions->dup_ctr[cid]; }
u64 hvm_get_dup_w32() { return HVM.interactions->dup_w32; }
u64 hvm_get_mat_era() { return HVM.interactions->mat_era; }
u64 hvm_get_mat_sup() { return HVM.interactions->mat_sup; }
u64 hvm_get_mat_ctr(u16 cid) { return HVM.interactions->mat_ctr[cid]; }
u64 hvm_get_mat_w32() { return HVM.interactions->mat_w32; }
u64 hvm_get_opx_era() { return HVM.interactions->opx_era; }
u64 hvm_get_opx_sup() { return HVM.interactions->opx_sup; }
u64 hvm_get_opx_w32() { return HVM.interactions->opx_w32; }
u64 hvm_get_opy_era() { return HVM.interactions->opy_era; }
u64 hvm_get_opy_sup() { return HVM.interactions->opy_sup; }
u64 hvm_get_opy_w32() { return HVM.interactions->opy_w32; }
u64 hvm_get_ref_dup(u16 fid) { return HVM.interactions->ref_dup[fid]; }
u64 hvm_get_ref_sup(u16 fid) { return HVM.interactions->ref_sup[fid]; }
u64 hvm_get_ref_era(u16 fid) { return HVM.interactions->ref_era[fid]; }
u64 hvm_get_ref_fast(u16 fid) { return HVM.interactions->ref_fast[fid]; }
u64 hvm_get_ref_fall(u16 fid) { return HVM.interactions->ref_fall[fid]; }
u64 hvm_get_ref_itrs(u16 fid) { return HVM.interactions->ref_itrs[fid]; }
u64 hvm_get_ref_slow(u16 fid) { return HVM.interactions->ref_slow[fid]; }