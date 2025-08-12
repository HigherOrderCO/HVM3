#include "Runtime.h"

// Core reducer and helpers (dispatcher, WHNF, normal form)

Term reduce(Term term) {
  if (term_tag(term) >= ERA) return term;
  Term  next = term;
  u64   stop = *HVM.spos;
  u64   spos = stop;
  Term* sbuf = HVM.sbuf;

  while (1) {
    Tag tag = term_tag(next);
    Lab lab = term_lab(next);
    Loc loc = term_loc(next);

    // On variables: substitute
    // On eliminators: move to field
    switch (tag) {
      case LET: {
        switch (lab) {
          case LAZY: next = reduce_let(next, got(loc + 0)); continue;
          case STRI: spush(next, sbuf, &spos); next = got(loc + 0); continue;
          default:  printf("invalid:let"); exit(0);
        }
      }
      case APP:
      case MAT:
      case IFL:
      case SWI:
      case OPX:
      case OPY: { spush(next, sbuf, &spos); next = got(loc + 0); continue; }
      case DP0:
      case DP1: {
        Term sub = got(loc + 0);
        if (term_get_bit(sub) == 0) { spush(next, sbuf, &spos); next = sub; continue; }
        next = term_rem_bit(sub); continue;
      }
      case VAR: {
        Term sub = got(loc);
        if (term_get_bit(sub) == 0) break;
        next = term_rem_bit(sub); continue;
      }
      case REF: { *HVM.spos = spos; next = reduce_ref(next); spos = *HVM.spos; continue; }
      default: break;
    }

    // Empty stack: term is in WHNF
    if (spos == stop) { *HVM.spos = spos; return next; }

    // Interaction Dispatcher
    Term prev = spop(sbuf, &spos);
    switch (term_tag(prev)) {
      case LET: next = reduce_let(prev, next); continue;
      case APP: switch (tag) {
        case ERA: next = reduce_app_era(prev, next); continue;
        case LAM: next = reduce_app_lam(prev, next); continue;
        case SUP: next = reduce_app_sup(prev, next); continue;
        case CTR: next = reduce_app_ctr(prev, next); continue;
        case W32: case CHR: next = reduce_app_w32(prev, next); continue;
        case INC: next = reduce_app_inc(prev, next); continue;
        case DEC: next = reduce_app_dec(prev, next); continue;
        default: break;
      }
      case DP0:
      case DP1: switch (tag) {
        case ERA: next = reduce_dup_era(prev, next); continue;
        case LAM: next = reduce_dup_lam(prev, next); continue;
        case SUP: next = reduce_dup_sup(prev, next); continue;
        case CTR: next = reduce_dup_ctr(prev, next); continue;
        case W32: case CHR: next = reduce_dup_w32(prev, next); continue;
        case INC: next = reduce_dup_inc(prev, next); continue;
        case DEC: next = reduce_dup_dec(prev, next); continue;
        default: break;
      }
      case MAT:
      case IFL:
      case SWI: switch (tag) {
        case ERA: next = reduce_mat_era(prev, next); continue;
        case LAM: next = reduce_mat_lam(prev, next); continue;
        case SUP: next = reduce_mat_sup(prev, next); continue;
        case CTR: next = reduce_mat_ctr(prev, next); continue;
        case W32: case CHR: next = reduce_mat_w32(prev, next); continue;
        case INC: next = reduce_mat_inc(prev, next); continue;
        case DEC: next = reduce_mat_dec(prev, next); continue;
        default: break;
      }
      case OPX: switch (tag) {
        case ERA: next = reduce_opx_era(prev, next); continue;
        case LAM: next = reduce_opx_lam(prev, next); continue;
        case SUP: next = reduce_opx_sup(prev, next); continue;
        case CTR: next = reduce_opx_ctr(prev, next); continue;
        case W32: case CHR: next = reduce_opx_w32(prev, next); continue;
        case INC: next = reduce_opx_inc(prev, next); continue;
        case DEC: next = reduce_opx_dec(prev, next); continue;
        default: break;
      }
      case OPY: switch (tag) {
        case ERA: next = reduce_opy_era(prev, next); continue;
        case LAM: next = reduce_opy_lam(prev, next); continue;
        case SUP: next = reduce_opy_sup(prev, next); continue;
        case CTR: next = reduce_opy_ctr(prev, next); continue;
        case W32: case CHR: next = reduce_opy_w32(prev, next); continue;
        case INC: next = reduce_opy_inc(prev, next); continue;
        case DEC: next = reduce_opy_dec(prev, next); continue;
        default: break;
      }
      default: break;
    }

    // No interaction: push term back to stack, update parent chain
    spush(prev, sbuf, &spos);
    while (spos > stop) {
      Term host = spop(sbuf, &spos);
      set(term_loc(host) + 0, next);
      next = host;
    }
    *HVM.spos = spos;
    return next;
  }
}

Term reduce_at(Loc host) {
  Term tm0 = got(host);
  if (term_tag(tm0) >= ERA) return tm0;
  Term tm1 = reduce(tm0);
  set(host, tm1);
  return tm1;
}

Term normal(Term term) {
  Term wnf = reduce(term);
  Tag tag = term_tag(wnf);
  Lab lab = term_lab(wnf);
  Loc loc = term_loc(wnf);
  switch (tag) {
    case LAM: { Term bod = got(loc + 0); bod = normal(bod); set(term_loc(wnf) + 1, bod); return wnf; }
    case APP: { Term fun = got(loc + 0); Term arg = got(loc + 1); fun = normal(fun); arg = normal(arg); set(term_loc(wnf) + 0, fun); set(term_loc(wnf) + 1, arg); return wnf; }
    case SUP: { Term tm0 = got(loc + 0); Term tm1 = got(loc + 1); tm0 = normal(tm0); tm1 = normal(tm1); set(term_loc(wnf) + 0, tm0); set(term_loc(wnf) + 1, tm1); return wnf; }
    case DP0:
    case DP1: { Term val = got(loc + 0); val = normal(val); set(term_loc(wnf) + 0, val); return wnf; }
    case CTR: { u64 cid = lab; u64 ari = HVM.cari[cid]; for (u64 i = 0; i < ari; i++) { Term arg = got(loc + i); arg = normal(arg); set(term_loc(wnf) + i, arg); } return wnf; }
    case MAT:
    case IFL:
    case SWI: { u64 len = tag == SWI ? lab : tag == IFL ? 2 : HVM.clen[lab]; for (u64 i = 0; i <= len; i++) { Term arg = got(loc + i); arg = normal(arg); set(term_loc(wnf) + i, arg); } return wnf; }
    default: return wnf;
  }
}
