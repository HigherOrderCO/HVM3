//./Type.hs//

#include <stdatomic.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <time.h>

typedef uint8_t  Tag;
typedef uint32_t Lab;
typedef uint32_t Loc;
typedef uint64_t Term;
typedef uint8_t  u8;
typedef uint32_t u32;
typedef uint64_t u64;

typedef _Atomic(u64) a64;
typedef _Atomic(Term) ATerm;

// Runtime Types
// -------------

// Global State Type
typedef struct {
  Term*  sbuf; // reduction stack buffer
  u64*   spos; // reduction stack position
  ATerm* heap; // global node buffer
  u64*   next; // next alloc index
  u64*   itrs; // interaction count
  //a64*   ntid; // next thread id
  Term (*book[4096])(u8,Term); // functions
} State;

// Global State Value
static State HVM = {
  .sbuf = NULL,
  .spos = NULL,
  .heap = NULL,
  .next = NULL,
  .itrs = NULL,
  .book = {NULL}
};

// Constants
// ---------

#define DP0 0x00
#define DP1 0x01
#define VAR 0x02
#define SUB 0x03
#define REF 0x04
#define LET 0x05
#define APP 0x06
#define MAT 0x07
#define OPX 0x08
#define OPY 0x09
#define ERA 0x0A
#define LAM 0x0B
#define SUP 0x0C
#define CTR 0x0D
#define W32 0x0E
#define CHR 0x0F

#define OP_ADD 0x00
#define OP_SUB 0x01
#define OP_MUL 0x02
#define OP_DIV 0x03
#define OP_MOD 0x04
#define OP_EQ  0x05
#define OP_NE  0x06
#define OP_LT  0x07
#define OP_GT  0x08
#define OP_LTE 0x09
#define OP_GTE 0x0A
#define OP_AND 0x0B
#define OP_OR  0x0C
#define OP_XOR 0x0D
#define OP_LSH 0x0E
#define OP_RSH 0x0F

#define DUP_F 0xFFF
#define SUP_F 0xFFE
#define LOG_F 0xFFD
#define FRESH_F 0xFFC

#define LAZY 0x0
#define STRI 0x1
#define PARA 0x2

#define VOID 0x00000000000000

// Heap
// ----

u64 get_len() {
  u64 len = 0;
  for (u32 i = 0; i < 16; ++i) {
    len += HVM.next[i];
  }
  return len;
}

u64 get_itr() {
  u64 itr = 0;
  for (u32 i = 0; i < 16; ++i) {
    itr += HVM.itrs[i];
  }
  return itr;
}

// Terms
// ------

Term term_new(Tag tag, Lab lab, Loc loc) {
  Term tag_enc = tag;
  Term lab_enc = ((Term)lab) << 8;
  Term loc_enc = ((Term)loc) << 32;
  return tag_enc | lab_enc | loc_enc;
}

Tag term_tag(Term x) {
  return x & 0x7F;
}

Lab term_lab(Term x) {
  return (x >> 8) & 0xFFFFFF;
}

Loc term_loc(Term x) {
  return (x >> 32) & 0xFFFFFFFF;
}

Tag term_get_bit(Term x) {
  return (x >> 7) & 1;
}

Term term_set_bit(Term term) {
  return term | (1ULL << 7);
}

Term term_rem_bit(Term term) {
  return term & ~(1ULL << 7);

}

// u12v2
// -----

u64 u12v2_new(u64 x, u64 y) {
  return (y << 12) | x;
}

u64 u12v2_x(u64 u12v2) {
  return u12v2 & 0xFFF;
}

u64 u12v2_y(u64 u12v2) {
  return u12v2 >> 12;
}

// Atomics
// -------

Term swap(Loc loc, Term term) {
  Term val = atomic_exchange_explicit(&HVM.heap[loc], term, memory_order_relaxed);
  if (val == 0) {
    printf("SWAP 0 at %x\n", loc);
    exit(0);
  }
  return val;
}

Term got(Loc loc) {
  Term val = atomic_load_explicit(&HVM.heap[loc], memory_order_relaxed);
  if (val == 0) {
    printf("GOT 0 at %x\n", loc);
    exit(0);
  }
  return val;
}

u64 race(Loc loc) {
  Term got = swap(loc, 0xFFFFFFFF);
  if (got == 0xFFFFFFFF) {
    printf("RACE\n");
    return 1;
  }
  return 0;
}

void set(Loc loc, Term term) {
  atomic_store_explicit(&HVM.heap[loc], term, memory_order_relaxed);
}

void sub(Loc loc, Term term) {
  set(loc, term_set_bit(term));
}

Term take(Loc loc) {
  return swap(loc, VOID);
}

// Allocation
// ----------

Loc alloc_node(u8 tid, Loc arity) {
  u64 old = HVM.next[tid];
  HVM.next[tid] += arity;
  //return old;
  if (old < ((1ULL << 32) / 16) - arity) {
    return old + tid * ((1ULL << 32) / 16);
  } else {
    printf("OOM\n");
    exit(0);
  }
}

Loc inc_itr(u8 tid, u64 amount) {
  u64 old = HVM.itrs[tid];
  HVM.itrs[tid] += amount;
  return old;
}

// Stringification
// ---------------

void print_tag(Tag tag) {
  switch (tag) {
    case SUB: printf("SUB"); break;
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
    case W32: printf("W32"); break;
    case CHR: printf("CHR"); break;
    case OPX: printf("OPX"); break;
    case OPY: printf("OPY"); break;
    default : printf("???"); break;
  }
}

void print_term(Term term) {
  printf("term_new(");
  print_tag(term_tag(term));
  printf(",0x%06x,0x%09x)", term_lab(term), term_loc(term));
}

void print_term_ln(Term term) {
  print_term(term);
  printf("\n");
}

void print_heap(u8 tid) {
  Loc len = get_len();
  for (Loc i = 0; i < len; i++) {
    Term term = got(i);
    if (term != 0) {
      printf("set(0x%09x, ", i);
      print_term(term);
      printf(");\n");
    }
  }
}

// Evaluation
// ----------

// @foo(&L{ax ay} b c ...)
// ----------------------- REF-SUP-COPY (when @L not in @foo)
// ! &L{bx by} = b
// ! &L{cx cy} = b
// ...
// &L{@foo(ax bx cx ...) @foo(ay by cy ...)}
Term reduce_ref_sup(u8 tid, Term ref, u32 idx) {
  inc_itr(tid, 1);
  Loc ref_loc = term_loc(ref);
  Lab ref_lab = term_lab(ref);
  u64 fun_id = u12v2_x(ref_lab);
  u64 arity  = u12v2_y(ref_lab);
  if (idx >= arity) {
    printf("ERROR: Invalid index in reduce_ref_sup\n");
    exit(1);
  }
  Term sup = got(ref_loc + idx);
  if (race(term_loc(ref) + idx)) return 0;
  if (term_tag(sup) != SUP) {
    printf("ERROR: Expected SUP at index %u\n", idx);
    exit(1);
  }
  Lab sup_lab = term_lab(sup);
  Loc sup_loc = term_loc(sup);
  Term sup0 = got(sup_loc + 0);
  Term sup1 = got(sup_loc + 1);
  // Allocate space for new REF node arguments for the second branch
  Loc ref1_loc = alloc_node(tid, arity);
  for (u64 i = 0; i < arity; ++i) {
    if (i != idx) {
      // Duplicate argument
      Term arg = got(ref_loc + i);
      Loc dup_loc = alloc_node(tid, 2);
      set(dup_loc + 0, arg);
      set(dup_loc + 1, term_new(SUB, 0, 0));
      set(ref_loc + i, term_new(DP0, sup_lab, dup_loc));
      set(ref1_loc + i, term_new(DP1, sup_lab, dup_loc));
    } else {
      // Set the SUP components directly
      set(ref_loc + i, sup0);
      set(ref1_loc + i, sup1);
    }
  }
  // Create new REF nodes
  Term ref0 = term_new(REF, ref_lab, ref_loc);
  Term ref1 = term_new(REF, ref_lab, ref1_loc);
  // Reuse sup_loc to create the new SUP node
  Loc su0 = alloc_node(tid, 2);
  set(su0 + 0, ref0);
  set(su0 + 1, ref1);
  return term_new(SUP, sup_lab, su0);
}

// @foo(a b c ...)
// -------------------- REF
// book[foo](a b c ...)
Term reduce_ref(u8 tid, Term ref) {
  //printf("[%u] reduce_ref ", tid); print_term(ref); printf("\n");
  //printf("call %d %p\n", term_loc(ref), HVM.book[term_loc(ref)]);
  inc_itr(tid, 1);
  return HVM.book[u12v2_x(term_lab(ref))](tid, ref);
}

// ! x = val
// bod
// --------- LET
// x <- val
// bod
Term reduce_let(u8 tid, Term let, Term val) {
  if (race(term_loc(let) + 0)) return 0;
  //printf("[%u] reduce_let ", tid); print_term(let); printf("\n");
  inc_itr(tid, 1);
  Loc let_loc = term_loc(let);
  Term bod    = got(let_loc + 1);
  sub(let_loc + 0, val);
  return bod;
}

// (* a)
// ----- APP-ERA
// *
Term reduce_app_era(u8 tid, Term app, Term era) {
  if (race(term_loc(app) + 0)) return 0;
  //printf("[%u] reduce_app_era ", tid); print_term(app); printf("\n");
  inc_itr(tid, 1);
  return era;
}

// (λx(body) a)
// ------------ APP-LAM
// x <- a
// body
Term reduce_app_lam(u8 tid, Term app, Term lam) {
  if (race(term_loc(app) + 0)) return 0;
  //printf("[%u] reduce_app_lam ", tid); print_term(app); printf("\n");
  inc_itr(tid, 1);
  Loc app_loc = term_loc(app);
  Loc lam_loc = term_loc(lam);
  Term arg    = got(app_loc + 1);
  Term bod    = got(lam_loc + 0);
  sub(lam_loc + 0, arg);
  return bod;
}

// (&L{a b} c)
// ----------------- APP-SUP
// ! &L{x0 x1} = c
// &L{(a x0) (b x1)}
Term reduce_app_sup(u8 tid, Term app, Term sup) {
  if (race(term_loc(app) + 0)) return 0;
  //printf("[%u] reduce_app_sup ", tid); print_term(app); printf("\n");
  inc_itr(tid, 1);
  Loc app_loc = term_loc(app);
  Loc sup_loc = term_loc(sup);
  Lab sup_lab = term_lab(sup);
  Term arg    = got(app_loc + 1);
  Term tm0    = got(sup_loc + 0);
  Term tm1    = got(sup_loc + 1);
  Loc du0     = alloc_node(tid, 2);
  Loc ap0     = alloc_node(tid, 2);
  //Loc ap0     = app_loc; // REUSE
  //Loc su0     = alloc_node(tid, 2);
  Loc su0     = sup_loc;
  Loc ap1     = alloc_node(tid, 2);
  set(du0 + 0, arg);
  set(du0 + 1, term_new(SUB, 0, 0));
  set(ap0 + 0, tm0);
  set(ap0 + 1, term_new(DP0, sup_lab, du0));
  set(ap1 + 0, tm1);
  set(ap1 + 1, term_new(DP1, sup_lab, du0));
  set(su0 + 0, term_new(APP, 0, ap0));
  set(su0 + 1, term_new(APP, 0, ap1));
  return term_new(SUP, sup_lab, su0);
}

// (#{x y z ...} a)
// ---------------- APP-CTR
// ⊥
Term reduce_app_ctr(u8 tid, Term app, Term ctr) {
  if (race(term_loc(app) + 0)) return 0;
  //printf("[%u] reduce_app_ctr ", tid); print_term(app); printf("\n");
  printf("invalid:app-ctr");
  exit(0);
}

// (123 a)
// ------- APP-W32
// ⊥
Term reduce_app_w32(u8 tid, Term app, Term w32) {
  if (race(term_loc(app) + 0)) return 0;
  //printf("[%u] reduce_app_w32 ", tid); print_term(app); printf("\n");
  printf("invalid:app-w32");
  exit(0);
}

// ! &L{x y} = *
// ------------- DUP-ERA
// x <- *
// y <- *
Term reduce_dup_era(u8 tid, Term dup, Term era) {
  if (race(term_loc(dup) + 0)) return 0;
  //printf("[%u] reduce_dup_era ", tid); print_term(dup); printf("\n");
  inc_itr(tid, 1);
  Loc dup_loc = term_loc(dup);
  Tag dup_num = term_tag(dup) == DP0 ? 0 : 1;
  sub(dup_loc + 0, era);
  sub(dup_loc + 1, era);
  return term_rem_bit(got(dup_loc + dup_num));
}

// ! &L{r s} = λx(f)
// ----------------- DUP-LAM
// ! &L{f0 f1} = f
// r <- λx0(f0)
// s <- λx1(f1)
// x <- &L{x0 x1}
Term reduce_dup_lam(u8 tid, Term dup, Term lam) {
  if (race(term_loc(dup) + 0)) return 0;
  //printf("[%u] reduce_dup_lam ", tid); print_term(dup); printf("\n");
  inc_itr(tid, 1);
  Loc dup_loc = term_loc(dup);
  Lab dup_lab = term_lab(dup);
  Tag dup_num = term_tag(dup) == DP0 ? 0 : 1;
  Loc lam_loc = term_loc(lam);
  Term bod    = got(lam_loc + 0);
  Loc du0     = alloc_node(tid, 2);
  Loc lm0     = alloc_node(tid, 1);
  Loc lm1     = alloc_node(tid, 1);
  Loc su0     = alloc_node(tid, 2);
  set(du0 + 0, bod);
  set(du0 + 1, term_new(SUB, 0, 0));
  //set(lm0 + 0, term_new(SUB, 0, 0));
  set(lm0 + 0, term_new(DP0, dup_lab, du0));
  //set(lm1 + 0, term_new(SUB, 0, 0));
  set(lm1 + 0, term_new(DP1, dup_lab, du0));
  set(su0 + 0, term_new(VAR, 0, lm0));
  set(su0 + 1, term_new(VAR, 0, lm1));
  sub(dup_loc + 0, term_new(LAM, 0, lm0));
  sub(dup_loc + 1, term_new(LAM, 0, lm1));
  sub(lam_loc + 0, term_new(SUP, dup_lab, su0));
  return term_rem_bit(got(dup_loc + dup_num));
}

// ! &L{x y} = &R{a b}
// ------------------- DUP-SUP
// if L == R:
//   x <- a
//   y <- b
// else:
//   x <- &R{a0 b0} 
//   y <- &R{a1 b1}
//   ! &L{a0 a1} = a
//   ! &L{b0 b1} = b
Term reduce_dup_sup(u8 tid, Term dup, Term sup) {
  if (race(term_loc(dup) + 0)) return 0;
  //printf("[%u] reduce_dup_sup %u %u | %llu ", tid, term_lab(dup), term_lab(sup), *HVM.spos); print_term(dup); printf(" "); print_term(sup); printf("\n");
  inc_itr(tid, 1);
  Loc dup_loc = term_loc(dup);
  Lab dup_lab = term_lab(dup);
  Tag dup_num = term_tag(dup) == DP0 ? 0 : 1;
  Lab sup_lab = term_lab(sup);
  Loc sup_loc = term_loc(sup);
  if (dup_lab == sup_lab) {
    Term tm0 = got(sup_loc + 0);
    Term tm1 = got(sup_loc + 1);
    sub(dup_loc + 0, tm0);
    sub(dup_loc + 1, tm1);
    return term_rem_bit(got(dup_loc + dup_num));
  } else {
    Loc du0 = alloc_node(tid, 2);
    Loc du1 = alloc_node(tid, 2);
    //Loc su0 = alloc_node(tid, 2);
    Loc su0 = sup_loc;
    Loc su1 = alloc_node(tid, 2);
    Term tm0 = take(sup_loc + 0);
    Term tm1 = take(sup_loc + 1);
    set(du0 + 0, tm0);
    set(du0 + 1, term_new(SUB, 0, 0));
    set(du1 + 0, tm1);
    set(du1 + 1, term_new(SUB, 0, 0));
    set(su0 + 0, term_new(DP0, dup_lab, du0));
    set(su0 + 1, term_new(DP0, dup_lab, du1));
    set(su1 + 0, term_new(DP1, dup_lab, du0));
    set(su1 + 1, term_new(DP1, dup_lab, du1));
    sub(dup_loc + 0, term_new(SUP, sup_lab, su0));
    sub(dup_loc + 1, term_new(SUP, sup_lab, su1));
    return term_rem_bit(got(dup_loc + dup_num));
  }
}

// ! &L{x y} = #{a b c ...}
// ------------------------ DUP-CTR
// ! &L{a0 a1} = a
// ! &L{b0 b1} = b
// ! &L{c0 c1} = c
// ...
// x <- #{a0 b0 c0 ...} 
// y <- #{a1 b1 c1 ...}
Term reduce_dup_ctr(u8 tid, Term dup, Term ctr) {
  if (race(term_loc(dup) + 0)) return 0;
  //printf("[%u] reduce_dup_ctr ", tid); print_term(dup); printf("\n");
  inc_itr(tid, 1);
  Loc dup_loc = term_loc(dup);
  Lab dup_lab = term_lab(dup);
  Tag dup_num = term_tag(dup) == DP0 ? 0 : 1;
  Loc ctr_loc = term_loc(ctr);
  Lab ctr_lab = term_lab(ctr);
  u64 ctr_ari = u12v2_y(ctr_lab);
  //Loc ctr0    = alloc_node(tid, ctr_ari);
  Loc ctr0    = ctr_loc;
  Loc ctr1    = alloc_node(tid, ctr_ari);
  for (u64 i = 0; i < ctr_ari; i++) {
    Loc du0 = alloc_node(tid, 2);
    set(du0 + 0, got(ctr_loc + i));
    set(du0 + 1, term_new(SUB, 0, 0));
    set(ctr0 + i, term_new(DP0, dup_lab, du0));
    set(ctr1 + i, term_new(DP1, dup_lab, du0));
  }
  sub(dup_loc + 0, term_new(CTR, ctr_lab, ctr0));
  sub(dup_loc + 1, term_new(CTR, ctr_lab, ctr1));
  return term_rem_bit(got(dup_loc + dup_num));
}

// ! &L{x y} = 123
// --------------- DUP-W32
// x <- 123
// y <- 123
Term reduce_dup_w32(u8 tid, Term dup, Term w32) {
  if (race(term_loc(dup) + 0)) return 0;
  //printf("[%u] reduce_dup_w32 ", tid); print_term(dup); printf("\n");
  inc_itr(tid, 1);
  Loc dup_loc = term_loc(dup);
  Tag dup_num = term_tag(dup) == DP0 ? 0 : 1;
  sub(dup_loc + 0, w32);
  sub(dup_loc + 1, w32);
  return term_rem_bit(got(dup_loc + dup_num));
}

// ! &L{x y} = @foo(a b c ...)
// --------------------------- DUP-REF-COPY (when &L not in @foo)
// ! &L{a0 a1} = a
// ! &L{b0 b1} = b
// ! &L{c0 c1} = c
// ...
// x <- @foo(a0 b0 c0 ...)
// y <- @foo(a1 b1 c1 ...)
Term reduce_dup_ref(u8 tid, Term dup, Term ref) {
  if (race(term_loc(dup) + 0)) return 0;
  //printf("[%u] reduce_dup_ref ", tid); print_term(dup); printf("\n");
  inc_itr(tid, 1);
  Loc dup_loc = term_loc(dup);
  Lab dup_lab = term_lab(dup);
  Tag dup_num = term_tag(dup) == DP0 ? 0 : 1;
  Loc ref_loc = term_loc(ref);
  Lab ref_lab = term_lab(ref);
  u64 ref_ari = u12v2_y(ref_lab);
  Loc ref0    = ref_loc;
  Loc ref1    = alloc_node(tid, 1 + ref_ari);
  for (u64 i = 0; i < ref_ari; i++) {
    Loc du0 = alloc_node(tid, 2);
    set(du0 + 0, got(ref_loc + i));
    set(du0 + 1, term_new(SUB, 0, 0));
    set(ref0 + i, term_new(DP0, dup_lab, du0));
    set(ref1 + i, term_new(DP1, dup_lab, du0));
  }
  sub(dup_loc + 0, term_new(REF, ref_lab, ref0));
  sub(dup_loc + 1, term_new(REF, ref_lab, ref1));
  return term_rem_bit(got(dup_loc + dup_num));
}

// ~ * {K0 K1 K2 ...} 
// ------------------ MAT-ERA
// *
Term reduce_mat_era(u8 tid, Term mat, Term era) {
  if (race(term_loc(mat) + 0)) return 0;
  //printf("[%u] reduce_mat_era ", tid); print_term(mat); printf("\n");
  inc_itr(tid, 1);
  return era;
}

// ~ λx(x) {K0 K1 K2 ...}
// ---------------------- MAT-LAM
// ⊥
Term reduce_mat_lam(u8 tid, Term mat, Term lam) {
  if (race(term_loc(mat) + 0)) return 0;
  //printf("[%u] reduce_mat_lam ", tid); print_term(mat); printf("\n");
  printf("invalid:mat-lam");
  exit(0);
}

// ~ &L{x y} {K0 K1 K2 ...}
// ------------------------ MAT-SUP
// ! &L{k0a k0b} = K0
// ! &L{k1a k1b} = K1
// ! &L{k2a k2b} = K2
// ...
// &L{ ~ x {K0a K1a K2a ...}
//     ~ y {K0b K1b K2b ...} }
Term reduce_mat_sup(u8 tid, Term mat, Term sup) {
  if (race(term_loc(mat) + 0)) return 0;
  //printf("[%u] reduce_mat_sup ", tid); print_term(mat); printf("\n");
  inc_itr(tid, 1);
  Loc mat_loc = term_loc(mat);
  Loc sup_loc = term_loc(sup);
  Lab sup_lab = term_lab(sup);
  Term tm0    = got(sup_loc + 0);
  Term tm1    = got(sup_loc + 1);
  Lab mat_lab = term_lab(mat);
  u64 mat_len = u12v2_x(mat_lab);
  Loc mat1    = alloc_node(tid, 1 + mat_len);
  Loc mat0    = alloc_node(tid, 1 + mat_len);
  //Loc mat0    = mat_loc; // REUSE
  //Loc sup0    = alloc_node(tid, 2);
  Loc sup0    = sup_loc;
  set(mat0 + 0, tm0);
  set(mat1 + 0, tm1);
  for (u64 i = 0; i < mat_len; i++) {
    Loc du0 = alloc_node(tid, 2);
    set(du0 + 0, got(mat_loc + 1 + i));
    set(du0 + 1, term_new(SUB, 0, 0));
    set(mat0 + 1 + i, term_new(DP0, sup_lab, du0));
    set(mat1 + 1 + i, term_new(DP1, sup_lab, du0));
  }
  set(sup0 + 0, term_new(MAT, mat_lab, mat0));
  set(sup0 + 1, term_new(MAT, mat_lab, mat1));
  return term_new(SUP, sup_lab, sup0);
}

// ~ #N{x y z ...} {K0 K1 K2 ...} 
// ------------------------------ MAT-CTR
// (((KN x) y) z ...)
Term reduce_mat_ctr(u8 tid, Term mat, Term ctr) {
  if (race(term_loc(mat) + 0)) return 0;
  //printf("[%u] reduce_mat_ctr ", tid); print_term(mat); printf("\n");
  inc_itr(tid, 1);
  Loc mat_loc = term_loc(mat);
  Lab mat_lab = term_lab(mat);
  // If-Let
  if (u12v2_y(mat_lab) > 0) {
    Loc ctr_loc = term_loc(ctr);
    Lab ctr_lab = term_lab(ctr);
    u64 mat_ctr = u12v2_y(mat_lab) - 1;
    u64 ctr_num = u12v2_x(ctr_lab);
    u64 ctr_ari = u12v2_y(ctr_lab);
    if (mat_ctr == ctr_num) {
      Term app = got(mat_loc + 1);
      for (u64 i = 0; i < ctr_ari; i++) {
        Loc new_app = alloc_node(tid, 2);
        set(new_app + 0, app);
        set(new_app + 1, got(ctr_loc + i));
        app = term_new(APP, 0, new_app);
      }
      return app;
    } else {
      Term app = got(mat_loc + 2);
      Loc new_app = alloc_node(tid, 2);
      set(new_app + 0, app);
      set(new_app + 1, ctr);
      app = term_new(APP, 0, new_app);
      return app;
    }
  // Match
  } else {
    Loc ctr_loc = term_loc(ctr);
    Lab ctr_lab = term_lab(ctr);
    u64 ctr_num = u12v2_x(ctr_lab);
    u64 ctr_ari = u12v2_y(ctr_lab);
    Term app = got(mat_loc + 1 + ctr_num);
    for (u64 i = 0; i < ctr_ari; i++) {
      Loc new_app = alloc_node(tid, 2);
      set(new_app + 0, app);
      set(new_app + 1, got(ctr_loc + i));
      app = term_new(APP, 0, new_app);
    }
    return app;
  }
}

// ~ num {K0 K1 K2 ... KN}
// ----------------------- MAT-W32
// if n < N: Kn
// else    : KN(num-N)
Term reduce_mat_w32(u8 tid, Term mat, Term w32) {
  if (race(term_loc(mat) + 0)) return 0;
  //printf("[%u] reduce_mat_w32 ", tid); print_term(mat); printf("\n");
  inc_itr(tid, 1);
  Lab mat_tag = term_tag(mat);
  Loc mat_loc = term_loc(mat);
  Lab mat_lab = term_lab(mat);
  u64 mat_len = u12v2_x(mat_lab);
  u64 w32_val = term_loc(w32);
  if (w32_val < mat_len - 1) {
    return got(mat_loc + 1 + w32_val);
  } else {
    Loc app = alloc_node(tid, 2);
    set(app + 0, got(mat_loc + mat_len));
    set(app + 1, term_new(W32, 0, w32_val - (mat_len - 1)));
    return term_new(APP, 0, app);
  }
}

// <op(* b)
// -------- OPX-ERA
// *
Term reduce_opx_era(u8 tid, Term opx, Term era) {
  if (race(term_loc(opx) + 0)) return 0;
  //printf("[%u] reduce_opx_era ", tid); print_term(opx); printf("\n");
  inc_itr(tid, 1);
  return era;
}

// <op(λx(B) y)
// ------------ OPX-LAM
// ⊥
Term reduce_opx_lam(u8 tid, Term opx, Term lam) {
  if (race(term_loc(opx) + 0)) return 0;
  //printf("[%u] reduce_opx_lam ", tid); print_term(opx); printf("\n");
  printf("invalid:opx-lam");
  exit(0);
}

// <op(&L{x0 x1} y)
// ------------------------- OPX-SUP
// ! &L{y0 y1} = y
// &L{<op(x0 y0) <op(x1 y1)}
Term reduce_opx_sup(u8 tid, Term opx, Term sup) {
  if (race(term_loc(opx) + 0)) return 0;
  //printf("[%u] reduce_opx_sup ", tid); print_term(opx); printf("\n");
  inc_itr(tid, 1);
  Loc opx_loc = term_loc(opx);
  Loc sup_loc = term_loc(sup);
  Lab sup_lab = term_lab(sup);
  Term nmy    = got(opx_loc + 1);
  Term tm0    = got(sup_loc + 0);
  Term tm1    = got(sup_loc + 1);
  Loc du0     = alloc_node(tid, 2);
  Loc op0     = alloc_node(tid, 2);
  Loc op1     = alloc_node(tid, 2);
  Loc su0     = sup_loc; // REUSE
  set(du0 + 0, nmy);
  set(du0 + 1, term_new(SUB, 0, 0));
  set(op0 + 0, tm0);
  set(op0 + 1, term_new(DP0, sup_lab, du0));
  set(op1 + 0, tm1);
  set(op1 + 1, term_new(DP1, sup_lab, du0));
  set(su0 + 0, term_new(OPX, term_lab(opx), op0));
  set(su0 + 1, term_new(OPX, term_lab(opx), op1));
  return term_new(SUP, sup_lab, su0);
}

// <op(#{x0 x1 x2...} y)
// --------------------- OPX-CTR
// ⊥
Term reduce_opx_ctr(u8 tid, Term opx, Term ctr) {
  if (race(term_loc(opx) + 0)) return 0;
  //printf("[%u] reduce_opx_ctr ", tid); print_term(opx); printf("\n");
  printf("invalid:opx-ctr");
  exit(0);
}

// <op(x0 x1)
// ---------- OPX-W32
// >op(x0 x1)
Term reduce_opx_w32(u8 tid, Term opx, Term w32) {
  if (race(term_loc(opx) + 0)) return 0;
  //printf("[%u] reduce_opx_w32 ", tid); print_term(opx); printf("\n");
  inc_itr(tid, 1);
  Loc op0 = alloc_node(tid, 2);
  Lab opx_lab = term_lab(opx);
  Lab opx_loc = term_loc(opx);
  Term nm1 = got(opx_loc + 1);
  set(op0 + 0, w32);
  set(op0 + 1, nm1);
  return term_new(OPY, opx_lab, op0);
}

// >op(a *)
// -------- OPY-ERA
// *
Term reduce_opy_era(u8 tid, Term opy, Term era) {
  if (race(term_loc(opy) + 1)) return 0;
  //printf("[%u] reduce_opy_era ", tid); print_term(opy); printf("\n");
  inc_itr(tid, 1);
  return era;
}

// >op(a λx(B))
// ------------ OPY-LAM
// *
Term reduce_opy_lam(u8 tid, Term opy, Term era) {
  if (race(term_loc(opy) + 1)) return 0;
  //printf("[%u] reduce_opy_lam ", tid); print_term(opy); printf("\n");
  printf("invalid:opy-lam");
  exit(0);
}

// >op(a &L{x y})
// --------------------- OPY-SUP
// &L{>op(a x) >op(a y)}
Term reduce_opy_sup(u8 tid, Term opy, Term sup) {
  if (race(term_loc(opy) + 1)) return 0;
  //printf("[%u] reduce_opy_sup ", tid); print_term(opy); printf("\n");
  inc_itr(tid, 1);
  Loc opy_loc = term_loc(opy);
  Loc sup_loc = term_loc(sup);
  Lab sup_lab = term_lab(sup);
  Term nmx    = got(opy_loc + 0);
  Term tm0    = got(sup_loc + 0);
  Term tm1    = got(sup_loc + 1);
  Loc op0     = alloc_node(tid, 2);
  Loc op1     = alloc_node(tid, 2);
  Loc su0     = sup_loc; // REUSE
  set(op0 + 0, nmx);
  set(op0 + 1, tm0);
  set(op1 + 0, nmx);
  set(op1 + 1, tm1);
  set(su0 + 0, term_new(OPY, term_lab(opy), op0));
  set(su0 + 1, term_new(OPY, term_lab(opy), op1));
  return term_new(SUP, sup_lab, su0);
}

// >op(#{x y z ...} b)
// ---------------------- OPY-CTR
// ⊥
Term reduce_opy_ctr(u8 tid, Term opy, Term ctr) {
  if (race(term_loc(opy) + 1)) return 0;
  //printf("[%u] reduce_opy_ctr ", tid); print_term(opy); printf("\n");
  printf("invalid:opy-ctr");
  exit(0);
}

// >op(x y)
// --------- OPY-W32
// x <op> y
Term reduce_opy_w32(u8 tid, Term opy, Term w32) {
  if (race(term_loc(opy) + 1)) return 0;
  //printf("[%u] reduce_opy_w32 ", tid); print_term(opy); printf("\n");
  inc_itr(tid, 1);
  Loc opy_loc = term_loc(opy);
  u32 t = term_tag(w32);
  u32 x = term_loc(got(opy_loc + 0));
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
    default: result = 0;
  }
  return term_new(t, 0, result);
}

Term reduce(u8 tid, Term term) {
  //if (term_lab(term) == 0x002000) {
    //volatile u64 i = 0;
    //volatile u64 L = 1 << 30;
    //volatile u64 k = 0;
    //printf("OXI"); print_term_ln(term); printf("\n");
    //while (i < L) {
        //k ^= i;
        //i++;
    //}
    //printf("OK\n");
    //return term_new(W32, 0, 7);
  //}
  //printf("[%u]", tid); print_term_ln(term); printf("\n");
  if (term_tag(term) >= ERA) return term;
  Term next = term;
  u64  stop = HVM.spos[tid];
  u64* spos = &HVM.spos[tid];
  u64* sbuf = HVM.sbuf + ((1ULL << 32) / 16 * tid);
  //printf("REDUCE %d %llu\n", tid, ((1ULL << 32) / 256 * tid));
  while (1) {
    //printf("NEXT "); print_term(term); printf("\n");
    //printf("PATH ");
    //for (u64 i = 0; i < *spos; ++i) {
      //print_tag(term_tag(HVM.sbuf[i]));
      //printf(" ");
    //}
    //printf(" ~ %p", HVM.sbuf);
    //printf("\n");
    if (next == 0) {
      printf("VISH\n");
    }
    Tag tag = term_tag(next);
    Lab lab = term_lab(next);
    Loc loc = term_loc(next);
    if (next != 0) {
      switch (tag) {
        case LET: {
          switch (lab) {
            case LAZY: {
              next = reduce_let(tid, next, got(loc + 0));
              continue;
            }
            case STRI: {
              sbuf[(*spos)++] = next;
              next = got(loc + 1);
              continue;
            }
            case PARA: {
              printf("TODO\n");
              continue;
            }
          }
        }
        case APP: {
          sbuf[(*spos)++] = next;
          next = got(loc + 0);
          continue;
        }
        case MAT: {
          sbuf[(*spos)++] = next;
          next = got(loc + 0);
          continue;
        }
        case OPX: {
          sbuf[(*spos)++] = next;
          next = got(loc + 0);
          continue;
        }
        case OPY: {
          sbuf[(*spos)++] = next;
          next = got(loc + 1);
          continue;
        }
        case DP0: {
          Term sb0 = got(loc + 0);
          if (term_get_bit(sb0) == 0) {
            sbuf[(*spos)++] = next;
            next = got(loc + 0);
            continue;
          } else {
            next = term_rem_bit(sb0);
            continue;
          }
        }
        case DP1: {
          Term sb1 = got(loc + 1);
          if (term_get_bit(sb1) == 0) {
            sbuf[(*spos)++] = next;
            next = got(loc + 0);
            continue;
          } else {
            next = term_rem_bit(sb1);
            continue;
          }
        }
        case VAR: {
          Term sub = got(loc);
          if (term_get_bit(sub) == 0) {
            break;
          } else {
            next = term_rem_bit(sub);
            continue;
          }
        }
        case REF: {
          next = reduce_ref(tid, next); // TODO
          continue;
        }
        default: {
          if ((*spos) == stop) {
            break;
          } else {
            Term prev = sbuf[--(*spos)];
            Tag  ptag = term_tag(prev);
            Lab  plab = term_lab(prev);
            Loc  ploc = term_loc(prev);

            //Loc lock_loc;
            //switch (term_tag(prev)) {
              //case APP: lock_loc = term_loc(prev + 0); break;
              //case DP0: lock_loc = term_loc(prev + 0); break;
              //case DP1: lock_loc = term_loc(prev + 0); break;
              //case MAT: lock_loc = term_loc(prev + 0); break;
              //case OPX: lock_loc = term_loc(prev + 0); break;
              //case OPY: lock_loc = term_loc(prev + 1); break;
            //}
            //Term locked = swap(lock_loc, 0xFFFFFFFF);
            //if (locked == 0xFFFFFFFF) {
              //printf("RACE\n");
            //}

            switch (ptag) {
              case LET: {
                next = reduce_let(tid, prev, next);
                continue;
              }
              case APP: {
                switch (tag) {
                  case ERA: next = reduce_app_era(tid, prev, next); continue;
                  case LAM: next = reduce_app_lam(tid, prev, next); continue;
                  case SUP: next = reduce_app_sup(tid, prev, next); continue;
                  case CTR: next = reduce_app_ctr(tid, prev, next); continue;
                  case W32: next = reduce_app_w32(tid, prev, next); continue;
                  case CHR: next = reduce_app_w32(tid, prev, next); continue;
                  default: break;
                }
                break;
              }
              case DP0:
              case DP1: {
                switch (tag) {
                  case ERA: next = reduce_dup_era(tid, prev, next); continue;
                  case LAM: next = reduce_dup_lam(tid, prev, next); continue;
                  case SUP: next = reduce_dup_sup(tid, prev, next); continue;
                  case CTR: next = reduce_dup_ctr(tid, prev, next); continue;
                  case W32: next = reduce_dup_w32(tid, prev, next); continue;
                  case CHR: next = reduce_dup_w32(tid, prev, next); continue;
                  default: break;
                }
                break;
              }
              case MAT: {
                switch (tag) {
                  case ERA: next = reduce_mat_era(tid, prev, next); continue;
                  case LAM: next = reduce_mat_lam(tid, prev, next); continue;
                  case SUP: next = reduce_mat_sup(tid, prev, next); continue;
                  case CTR: next = reduce_mat_ctr(tid, prev, next); continue;
                  case W32: next = reduce_mat_w32(tid, prev, next); continue;
                  case CHR: next = reduce_mat_w32(tid, prev, next); continue;
                  default: break;
                }
              }
              case OPX: {
                switch (tag) {
                  case ERA: next = reduce_opx_era(tid, prev, next); continue;
                  case LAM: next = reduce_opx_lam(tid, prev, next); continue;
                  case SUP: next = reduce_opx_sup(tid, prev, next); continue;
                  case CTR: next = reduce_opx_ctr(tid, prev, next); continue;
                  case W32: next = reduce_opx_w32(tid, prev, next); continue;
                  case CHR: next = reduce_opx_w32(tid, prev, next); continue;
                  default: break;
                }
              }
              case OPY: {
                switch (tag) {
                  case ERA: next = reduce_opy_era(tid, prev, next); continue;
                  case LAM: next = reduce_opy_lam(tid, prev, next); continue;
                  case SUP: next = reduce_opy_sup(tid, prev, next); continue;
                  case CTR: next = reduce_opy_ctr(tid, prev, next); continue;
                  case W32: next = reduce_opy_w32(tid, prev, next); continue;
                  case CHR: next = reduce_opy_w32(tid, prev, next); continue;
                  default: break;
                }
              } 
              default: break;
            }
            break;
          }
        }
      }
    }
    if (next == 0) {
      *spos = stop;
      return reduce(tid, term);
    } else if ((*spos) == stop) {
      return next;
    } else {
      if (next != 0) {
        Term host = sbuf[--(*spos)];
        Tag  htag = term_tag(host);
        Lab  hlab = term_lab(host);
        Loc  hloc = term_loc(host);
        switch (htag) {
          case APP: set(hloc + 0, next); break;
          case DP0: set(hloc + 0, next); break;
          case DP1: set(hloc + 0, next); break;
          case MAT: set(hloc + 0, next); break;
          case OPX: set(hloc + 0, next); break;
          case OPY: set(hloc + 1, next); break;
        }
      }
      *spos = stop;
      return sbuf[stop];
    }
  }
  printf("retr: ERR\n");
  return 0;
}

Term reduce_at(u8 tid, Loc host) {
  Term term = reduce(tid, got(host));
  set(host, term);
  return term;
}

//Term reduce_par(Term term) {
  //u8 tid = atomic_fetch_add_explicit(HVM.ntid, 1, memory_order_relaxed);
  //printf("[%u] REDUCING\n", tid);
  //Term result = reduce(tid, term);
  //printf("[%u] DONE\n", tid);
  //atomic_fetch_sub_explicit(HVM.ntid, 1, memory_order_relaxed);
  //return result;
//}

Term normal(Term term) {
  Term wnf = reduce(0, term);
  Tag tag = term_tag(wnf);
  Lab lab = term_lab(wnf);
  Loc loc = term_loc(wnf);
  switch (tag) {
    case LAM: {
      Term bod = got(loc + 0);
      bod = normal(bod);
      set(loc + 1, bod);
      return wnf;
    }
    case APP: {
      Term fun = got(loc + 0);
      Term arg = got(loc + 1);
      fun = normal(fun);
      arg = normal(arg);
      set(loc + 0, fun);
      set(loc + 1, arg);
      return wnf;
    }
    case SUP: {
      Term tm0 = got(loc + 0);
      Term tm1 = got(loc + 1);
      tm0 = normal(tm0);
      tm1 = normal(tm1);
      set(loc + 0, tm0);
      set(loc + 1, tm1);
      return wnf;
    }
    case DP0:
    case DP1: {
      Term val = got(loc + 0);
      val = normal(val);
      set(loc + 0, val);
      return wnf;
    }
    case CTR: {
      u64 cid = u12v2_x(lab);
      u64 ari = u12v2_y(lab);
      for (u64 i = 0; i < ari; i++) {
        Term arg = got(loc + i);
        arg = normal(arg);
        set(loc + i, arg);
      }
      return wnf;
    }
    case MAT: {
      u64 mat_len = u12v2_x(lab);
      for (u64 i = 0; i <= mat_len; i++) {
        Term arg = got(loc + i);
        arg = normal(arg);
        set(loc + i, arg);
      }
      return wnf;
    }
    default:
      return wnf;
  }
}

// Primitives
// ----------

// Primitive: Dynamic Sup `@SUP(lab tm0 tm1)`
// Allocates a new SUP node with given label.
Term SUP_f(u8 tid, Term ref) {
  Loc ref_loc = term_loc(ref);
  Term lab = reduce(tid, got(ref_loc + 0));
  if (term_tag(lab) != W32) {
    printf("ERROR:non-numeric-sup-label\n");
  }
  Term tm0 = got(ref_loc + 1);
  Term tm1 = got(ref_loc + 2);
  Loc  sup = alloc_node(tid, 2);
  Term ret = term_new(SUP, term_loc(lab), sup);
  set(sup + 0, tm0);
  set(sup + 1, tm1);
  return ret;
}

// Primitive: Dynamic Dup `@DUP(lab val λdp0λdp1(bod))`
// Creates a DUP node with given label.
Term DUP_f(u8 tid, Term ref) {
  Loc ref_loc = term_loc(ref);
  Term lab = reduce(tid, got(ref_loc + 0));
  if (term_tag(lab) != W32) {
    printf("ERROR:non-numeric-dup-label\n");
  }
  Term val = got(ref_loc + 1);
  Term bod = got(ref_loc + 2);
  Loc dup = alloc_node(tid, 2);
  set(dup + 0, val);
  set(dup + 1, term_new(SUB, 0, 0));
  Term bod_term = got(ref_loc + 2);
  if (term_tag(bod_term) == LAM) {
    Loc lam1_loc = term_loc(bod_term);
    Term lam1_bod = got(lam1_loc + 0);
    if (term_tag(lam1_bod) == LAM) {
      Loc lam2_loc = term_loc(lam1_bod);
      Term lam2_bod = got(lam2_loc + 0);
      sub(lam1_loc + 0, term_new(DP0, term_loc(lab), dup));
      sub(lam2_loc + 0, term_new(DP1, term_loc(lab), dup));
      inc_itr(tid, 2);
      return lam2_bod;
    }
  }
  Loc app1 = alloc_node(tid, 2);
  set(app1 + 0, bod);
  set(app1 + 1, term_new(DP0, term_loc(lab), dup));
  Loc app2 = alloc_node(tid, 2);
  set(app2 + 0, term_new(APP, 0, app1));
  set(app2 + 1, term_new(DP1, term_loc(lab), dup));
  return term_new(APP, 0, app2);

}

Term LOG_f(u8 tid, Term ref) {
  printf("TODO: LOG_f");
  exit(0);
}

Term FRESH_f(u8 tid, Term ref) {
  printf("TODO: FRESH_f");
  exit(0);
}

// Runtime Memory
// --------------

void hvm_init() {
  // FIXME: use mmap instead
  HVM.sbuf  = malloc((1ULL << 32) * sizeof(Term));
  HVM.spos  = malloc(16 * sizeof(u64));
  HVM.heap  = malloc((1ULL << 32) * sizeof(ATerm));
  HVM.next  = malloc(16 * sizeof(u64));
  HVM.itrs  = malloc(16 * sizeof(u64));
  //HVM.ntid  = malloc(sizeof(a64));
  for (u32 i = 0; i < 16; ++i) {
    HVM.spos[i] = 0;
    HVM.next[i] = 0;
    HVM.itrs[i] = 0;
  }
  HVM.next[0] = 1;
  //HVM.ntid[0] = 0;
  HVM.book[SUP_F] = SUP_f;
  HVM.book[DUP_F] = DUP_f;
  HVM.book[LOG_F] = LOG_f;
  HVM.book[FRESH_F] = FRESH_f;
}

void hvm_free() {
  free(HVM.sbuf);
  free(HVM.spos);
  free(HVM.heap);
  free(HVM.next);
  free(HVM.itrs);
  //free(HVM.ntid);
}

State* hvm_get_state() {
  return &HVM;
}

void hvm_set_state(State* hvm) {
  HVM.sbuf = hvm->sbuf;
  HVM.spos = hvm->spos;
  HVM.heap = hvm->heap;
  HVM.next = hvm->next;
  HVM.itrs = hvm->itrs;
  //HVM.ntid = hvm->ntid;
  for (int i = 0; i < 4096; i++) {
    HVM.book[i] = hvm->book[i];
  }
}

void hvm_define(u64 fid, Term (*func)(u8, Term)) {
  //printf("defined %llu %p\n", fid, func);
  HVM.book[fid] = func;
}
