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
typedef uint32_t u32;
typedef uint64_t u64;

typedef _Atomic(Term) ATerm;

// Runtime Types
// -------------


// Global State Type
typedef struct {
  Term*  sbuf; // reduction stack buffer
  u64*   spos; // reduction stack position
  ATerm* heap; // global node buffer
  u64*   size; // global node length
  u64*   itrs; // interaction count
  Term (*book[1024])(Term); // functions
  u64*   adts; // ADT Info
} State;

// Global State Value
static State HVM = {
  .sbuf = NULL,
  .spos = NULL,
  .heap = NULL,
  .size = NULL,
  .itrs = NULL,
  .book = {NULL}
};

// Constants
// ---------

#define DP0 0x00
#define DP1 0x01
#define UDP 0x02
#define VAR 0x03
#define SUB 0x04
#define REF 0x05
#define LET 0x06
#define APP 0x07
#define MAT 0x08
#define OPX 0x09
#define OPY 0x0A
#define ERA 0x0B
#define LAM 0x0C
#define SUP 0x0D
#define USP 0x0E
#define CTR 0x0F
#define W32 0x10

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

#define LAZY 0x0
#define STRI 0x1
#define PARA 0x2

#define VOID 0x00000000000000

// Heap
// ----

Loc get_len() {
  return *HVM.size;
}

u64 get_itr() {
  return *HVM.itrs;
}

void set_len(Loc value) {
  *HVM.size = value;
}

void set_itr(Loc value) {
  *HVM.itrs = value;
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
  return x & 0xFF;
}

Lab term_lab(Term x) {
  return (x >> 8) & 0xFFFFFF;
}

Loc term_loc(Term x) {
  return (x >> 32) & 0xFFFFFFFF;
}

Loc term_key(Term term) {
  switch (term_tag(term)) {
    case DP0: return term_loc(term) + 0;
    case DP1: return term_loc(term) + 1;
    case VAR: return term_loc(term) + 0;
    default:  return 0;
  }
}

// u12v2
// -----

u64 u12v2_new(u64 x, u64 y) {
  return (y << 12) | x;
}

u64 u12v2_x(u64 u12v2) {
  return u12v2 & 0x3FF;
}

u64 u12v2_y(u64 u12v2) {
  return u12v2 >> 12;
}

// Atomics
// -------

Term swap(Loc loc, Term term) {
  return atomic_exchange_explicit(&HVM.heap[loc], term, memory_order_relaxed);
}

Term got(Loc loc) {
  return atomic_load_explicit(&HVM.heap[loc], memory_order_relaxed);
}

void set(Loc loc, Term term) {
  atomic_store_explicit(&HVM.heap[loc], term, memory_order_relaxed);
}

Term take(Loc loc) {
  return swap(loc, VOID);
}

// Allocation
// ----------

Loc alloc_node(Loc arity) {
  u64 old = *HVM.size;
  *HVM.size += arity;
  return old;
}

Loc inc_itr() {
  u64 old = *HVM.itrs;
  *HVM.itrs += 1;
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
    case ERA: printf("ERA"); break;
    case LAM: printf("LAM"); break;
    case SUP: printf("SUP"); break;
    case REF: printf("REF"); break;
    case LET: printf("LET"); break;
    case CTR: printf("CTR"); break;
    case MAT: printf("MAT"); break;
    case W32: printf("W32"); break;
    case OPX: printf("OPX"); break;
    case OPY: printf("OPY"); break;
    case UDP: printf("UDP"); break;
    case USP: printf("USP"); break;
    default : printf("???"); break;
  }
}

void print_term(Term term) {
  printf("term_new(");
  print_tag(term_tag(term));
  printf(",0x%06x,0x%09x)", term_lab(term), term_loc(term));
}

void print_heap() {
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

// @foo
// --------- REF
// book[foo]
Term reduce_ref(Term ref) {
  //printf("call %d %p\n", term_loc(ref), HVM.book[term_loc(ref)]);
  inc_itr();
  return HVM.book[u12v2_x(term_lab(ref))](ref);
}

// ! x = val
// bod
// --------- LET
// x <- val
// bod
Term reduce_let(Term let, Term val) {
  inc_itr();
  Loc let_loc = term_loc(let);
  Term bod    = got(let_loc + 2);
  set(let_loc + 0, val);
  return bod;
}

// (* a)
// ----- APP-ERA
// *
Term reduce_app_era(Term app, Term era) {
  inc_itr();
  return era;
}

// (λx(body) a)
// ------------ APP-LAM
// x <- a
// body
Term reduce_app_lam(Term app, Term lam) {
  inc_itr();
  Loc app_loc = term_loc(app);
  Loc lam_loc = term_loc(lam);
  Term arg    = got(app_loc + 1);
  Term bod    = got(lam_loc + 1);
  set(lam_loc + 0, arg);
  return bod;
}

// (&L{a b} c)
// ----------------- APP-SUP
// ! &L{x0 x1} = c
// &L{(a x0) (b x1)}
Term reduce_app_sup(Term app, Term sup) {
  inc_itr();
  Loc app_loc = term_loc(app);
  Loc sup_loc = term_loc(sup);
  Lab sup_lab = term_lab(sup);
  Term arg    = got(app_loc + 1);
  Term tm0    = got(sup_loc + 0);
  Term tm1    = got(sup_loc + 1);
  Loc du0     = alloc_node(3);
  Loc su0     = alloc_node(2);
  Loc ap0     = alloc_node(2);
  Loc ap1     = alloc_node(2);
  set(du0 + 0, term_new(SUB, 0, 0));
  set(du0 + 1, term_new(SUB, 0, 0));
  set(du0 + 2, arg);
  set(ap0 + 0, tm0);
  set(ap0 + 1, term_new(DP0, sup_lab, du0));
  set(ap1 + 0, tm1);
  set(ap1 + 1, term_new(DP1, sup_lab, du0));
  set(su0 + 0, term_new(APP, 0, ap0));
  set(su0 + 1, term_new(APP, 0, ap1));
  return term_new(SUP, sup_lab, su0);
}

// (%L{a b} c)
// --------------- APP-USP
// ! %L{x} = c
// %L{(a x) (b x)}
Term reduce_app_usp(Term app, Term usp) {
  inc_itr();
  Loc app_loc = term_loc(app);
  Loc usp_loc = term_loc(usp);
  Lab usp_lab = term_lab(usp);
  Term arg    = got(app_loc + 1);
  Term tm0    = got(usp_loc + 0);
  Term tm1    = got(usp_loc + 1);
  Loc su0     = alloc_node(2);
  Loc ap0     = alloc_node(2);
  Loc ap1     = alloc_node(2);
  Term cop;
  if (term_tag(arg) != UDP) {
    Loc du0 = alloc_node(1);
    set(du0 + 0, arg);
    cop = term_new(UDP, usp_lab, du0);
  } else {
    cop = arg;
  }
  set(ap0 + 0, tm0);
  set(ap0 + 1, cop);
  set(ap1 + 0, tm1);
  set(ap1 + 1, cop);
  set(su0 + 0, term_new(APP, 0, ap0));
  set(su0 + 1, term_new(APP, 0, ap1));
  return term_new(USP, usp_lab, su0);
}

// (#{x y z ...} a)
// ---------------- APP-CTR
// ⊥
Term reduce_app_ctr(Term app, Term ctr) {
  printf("invalid:app-ctr");
  exit(0);
}

// (123 a)
// ------- APP-W32
// ⊥
Term reduce_app_w32(Term app, Term w32) {
  printf("invalid:app-w32");
  exit(0);
}

// ! &L{x y} = *
// ------------- DUP-ERA
// x <- *
// y <- *
Term reduce_dup_era(Term dup, Term era) {
  inc_itr();
  Loc dup_loc = term_loc(dup);
  Tag dup_num = term_tag(dup) == DP0 ? 0 : 1;
  set(dup_loc + 0, era);
  set(dup_loc + 1, era);
  return got(dup_loc + dup_num);
}

// ! &L{r s} = λx(f)
// ----------------- DUP-LAM
// ! &L{f0 f1} = f
// r <- λx0(f0)
// s <- λx1(f1)
// x <- &L{x0 x1}
Term reduce_dup_lam(Term dup, Term lam) {
  inc_itr();
  Loc dup_loc = term_loc(dup);
  Lab dup_lab = term_lab(dup);
  Tag dup_num = term_tag(dup) == DP0 ? 0 : 1;
  Loc lam_loc = term_loc(lam);
  Term bod    = got(lam_loc + 1);
  Loc du0     = alloc_node(3);
  Loc lm0     = alloc_node(2);
  Loc lm1     = alloc_node(2);
  Loc su0     = alloc_node(2);
  set(du0 + 0, term_new(SUB, 0, 0));
  set(du0 + 1, term_new(SUB, 0, 0));
  set(du0 + 2, bod);
  set(lm0 + 0, term_new(SUB, 0, 0));
  set(lm0 + 1, term_new(DP0, dup_lab, du0));
  set(lm1 + 0, term_new(SUB, 0, 0));
  set(lm1 + 1, term_new(DP1, dup_lab, du0));
  set(su0 + 0, term_new(VAR, 0, lm0));
  set(su0 + 1, term_new(VAR, 0, lm1));
  set(dup_loc + 0, term_new(LAM, 0, lm0));
  set(dup_loc + 1, term_new(LAM, 0, lm1));
  set(lam_loc + 0, term_new(SUP, dup_lab, su0));
  return got(dup_loc + dup_num);
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
Term reduce_dup_sup(Term dup, Term sup) {
  inc_itr();
  Loc dup_loc = term_loc(dup);
  Lab dup_lab = term_lab(dup);
  Tag dup_num = term_tag(dup) == DP0 ? 0 : 1;
  Lab sup_lab = term_lab(sup);
  Loc sup_loc = term_loc(sup);
  if (dup_lab == sup_lab) {
    Term tm0 = got(sup_loc + 0);
    Term tm1 = got(sup_loc + 1);
    set(dup_loc + 0, tm0);
    set(dup_loc + 1, tm1);
    return got(dup_loc + dup_num);
  } else {
    Loc du0 = alloc_node(3);
    Loc du1 = alloc_node(3);
    Loc su0 = alloc_node(2);
    Loc su1 = alloc_node(2);
    Term tm0 = got(sup_loc + 0);
    Term tm1 = got(sup_loc + 1);
    set(du0 + 0, term_new(SUB, 0, 0));
    set(du0 + 1, term_new(SUB, 0, 0));
    set(du0 + 2, tm0);
    set(du1 + 0, term_new(SUB, 0, 0));
    set(du1 + 1, term_new(SUB, 0, 0));
    set(du1 + 2, tm1);
    set(su0 + 0, term_new(DP0, dup_lab, du0));
    set(su0 + 1, term_new(DP0, dup_lab, du1));
    set(su1 + 0, term_new(DP1, dup_lab, du0));
    set(su1 + 1, term_new(DP1, dup_lab, du1));
    set(dup_loc + 0, term_new(SUP, sup_lab, su0));
    set(dup_loc + 1, term_new(SUP, sup_lab, su1));
    return got(dup_loc + dup_num);
  }
}

// ! &L{x y} = %R{a b}
// ------------------- DUP-USP
// x <- %R{a0 b0} 
// y <- %R{a1 b1}
// ! &L{a0 a1} = a
// ! &L{b0 b1} = b
Term reduce_dup_usp(Term dup, Term usp) {
  inc_itr();
  Loc dup_loc = term_loc(dup);
  Lab dup_lab = term_lab(dup);
  Tag dup_num = term_tag(dup) == DP0 ? 0 : 1;
  Lab usp_lab = term_lab(usp);
  Loc usp_loc = term_loc(usp);
  Loc du0 = alloc_node(3);
  Loc du1 = alloc_node(3);
  Loc us0 = alloc_node(2);
  Loc us1 = alloc_node(2);
  Term tm0 = got(usp_loc + 0);
  Term tm1 = got(usp_loc + 1);
  set(du0 + 0, term_new(SUB, 0, 0));
  set(du0 + 1, term_new(SUB, 0, 0));
  set(du0 + 2, tm0);
  set(du1 + 0, term_new(SUB, 0, 0));
  set(du1 + 1, term_new(SUB, 0, 0));
  set(du1 + 2, tm1);
  set(us0 + 0, term_new(DP0, dup_lab, du0));
  set(us0 + 1, term_new(DP0, dup_lab, du1));
  set(us1 + 0, term_new(DP1, dup_lab, du0));
  set(us1 + 1, term_new(DP1, dup_lab, du1));
  set(dup_loc + 0, term_new(USP, usp_lab, us0));
  set(dup_loc + 1, term_new(USP, usp_lab, us1));
  return got(dup_loc + dup_num);
}

// ! &{x y} = #{a b c ...}
// ----------------------- DUP-CTR
// ! &{a0 a1} = a
// ! &{b0 b1} = b
// ! &{c0 c1} = c
// ...
// {#{a0 b0 c0 ...} #{a1 b1 c1 ...}}
Term reduce_dup_ctr(Term dup, Term ctr) {
  inc_itr();
  Loc dup_loc = term_loc(dup);
  Tag dup_num = term_tag(dup) == DP0 ? 0 : 1;
  Loc ctr_loc = term_loc(ctr);
  Lab ctr_lab = term_lab(ctr);
  u64 ctr_ari = u12v2_y(ctr_lab);
  Loc ctr0    = alloc_node(ctr_ari);
  Loc ctr1    = alloc_node(ctr_ari);
  for (u64 i = 0; i < ctr_ari; i++) {
    Loc du0 = alloc_node(3);
    set(du0 + 0, term_new(SUB, 0, 0));
    set(du0 + 1, term_new(SUB, 0, 0));
    set(du0 + 2, got(ctr_loc + i));
    set(ctr0 + i, term_new(DP0, 0, du0));
    set(ctr1 + i, term_new(DP1, 0, du0));
  }
  Loc sup = alloc_node(2);
  set(sup + 0, term_new(CTR, ctr_lab, ctr0));
  set(sup + 1, term_new(CTR, ctr_lab, ctr1));
  set(dup_loc + 0, term_new(CTR, ctr_lab, ctr0));
  set(dup_loc + 1, term_new(CTR, ctr_lab, ctr1));
  return got(dup_loc + dup_num);
}

// ! &L{x y} = 123
// --------------- DUP-W32
// x <- 123
// y <- 123
Term reduce_dup_w32(Term dup, Term w32) {
  inc_itr();
  Loc dup_loc = term_loc(dup);
  Tag dup_num = term_tag(dup) == DP0 ? 0 : 1;
  set(dup_loc + 0, w32);
  set(dup_loc + 1, w32);
  return got(dup_loc + dup_num);
}

// ~ * {K0 K1 K2 ...} 
// ------------------ MAT-ERA
// *
Term reduce_mat_era(Term mat, Term era) {
  inc_itr();
  return era;
}

// ~ λx(x) {K0 K1 K2 ...}
// ---------------------- MAT-LAM
// ⊥
Term reduce_mat_lam(Term mat, Term lam) {
  printf("invalid:mat-lam");
  exit(0);
}

// TODO: treat SUP label
// ~ &{x y} {K0 K1 K2 ...}
// ----------------------- MAT-SUP
// ! &{k0a k0b} = K0
// ! &{k1a k1b} = K1
// ! &{k2a k2b} = K2
// ...
// &{ ~ x {K0a K1a K2a ...}
//    ~ y {K0b K1b K2b ...} }
Term reduce_mat_sup(Term mat, Term sup) {
  inc_itr();
  Loc mat_loc = term_loc(mat);
  Loc sup_loc = term_loc(sup);
  Term tm0    = got(sup_loc + 0);
  Term tm1    = got(sup_loc + 1);
  Lab mat_len = term_lab(mat);
  Loc sup0    = alloc_node(2);
  Loc mat0    = alloc_node(1 + mat_len);
  Loc mat1    = alloc_node(1 + mat_len);
  set(mat0 + 0, tm0);
  set(mat1 + 0, tm1);
  for (u64 i = 0; i < mat_len; i++) {
    Loc du0 = alloc_node(3);
    set(du0 + 0, term_new(SUB, 0, 0));
    set(du0 + 1, term_new(SUB, 0, 0));
    set(du0 + 2, got(mat_loc + 1 + i));
    set(mat0 + 1 + i, term_new(DP0, 0, du0));
    set(mat1 + 1 + i, term_new(DP1, 0, du0));
  }
  set(sup0 + 0, term_new(MAT, mat_len, mat0));
  set(sup0 + 1, term_new(MAT, mat_len, mat1));
  return term_new(SUP, 0, sup0);
}

// MAT_USP: TODO
// ~ %L{x y} {K0 K1 K2 ...}
// ------------------------ MAT-USP
// ! %L{k0} = K0
// ! %L{k1} = K1
// ! %L{k2} = K2
// ...
// %L{ ~ x {K0 K1 K2 ...}
//     ~ y {K0 K1 K2 ...} }
Term reduce_mat_usp(Term mat, Term usp) {
  inc_itr();
  Loc mat_loc = term_loc(mat);
  Loc usp_loc = term_loc(usp);
  Lab usp_lab = term_lab(usp);
  Term tm0    = got(usp_loc + 0);
  Term tm1    = got(usp_loc + 1);
  Lab mat_len = term_lab(mat);
  Loc usp0    = alloc_node(2);
  Loc mat0    = alloc_node(1 + mat_len);
  Loc mat1    = alloc_node(1 + mat_len);
  set(mat0 + 0, tm0);
  set(mat1 + 0, tm1);
  for (u64 i = 0; i < mat_len; i++) {
    Term cse = got(mat_loc + 1 + i);
    Term cop;
    if (term_tag(cse) != UDP) {
      Loc du0 = alloc_node(1);
      set(du0 + 0, got(mat_loc + 1 + i));
      cop = term_new(UDP, usp_lab, du0);
    } else {
      cop = cse;
    }
    set(mat0 + 1 + i, cop);
    set(mat1 + 1 + i, cop);
  }
  set(usp0 + 0, term_new(MAT, mat_len, mat0));
  set(usp0 + 1, term_new(MAT, mat_len, mat1));
  return term_new(USP, usp_lab, usp0);
}

// ~ #N{x y z ...} {K0 K1 K2 ...} 
// ------------------------------ MAT-CTR
// (((KN x) y) z ...)
Term reduce_mat_ctr(Term mat, Term ctr) {
  inc_itr();
  Loc mat_loc = term_loc(mat);
  Loc ctr_loc = term_loc(ctr);
  Lab ctr_lab = term_lab(ctr);
  u64 ctr_num = u12v2_x(ctr_lab);
  u64 ctr_ari = u12v2_y(ctr_lab);
  Term app = got(mat_loc + 1 + ctr_num);
  for (u64 i = 0; i < ctr_ari; i++) {
    Loc new_app = alloc_node(2);
    set(new_app + 0, app);
    set(new_app + 1, got(ctr_loc + i));
    app = term_new(APP, 0, new_app);
  }
  return app;
}

// ~ num {K0 K1 K2 ... KN}
// ----------------------- MAT-W32
// if n < N: Kn
// else    : KN(num-N)
Term reduce_mat_w32(Term mat, Term w32) {
  inc_itr();
  Loc mat_loc = term_loc(mat);
  Lab mat_len = term_lab(mat);
  u64 w32_val = term_loc(w32);
  if (w32_val < mat_len - 1) {
    return got(mat_loc + 1 + w32_val);
  } else {
    Loc app = alloc_node(2);
    set(app + 0, got(mat_loc + mat_len));
    set(app + 1, term_new(W32, 0, w32_val - (mat_len - 1)));
    return term_new(APP, 0, app);
  }
}

// <op(* b)
// --------- OPX-ERA
// *
Term reduce_opx_era(Term opx, Term era) {
  inc_itr();
  return era;
}

// <op(λx(B) y)
// ------------- OPX-LAM
// ⊥
Term reduce_opx_lam(Term opx, Term lam) {
  printf("invalid:opx-lam");
  exit(0);
}

// <op(&L{x0 x1} y)
// ------------------------- OPX-SUP
// ! &L{y0 y1} = y
// &L{<op(x0 y0) <op(x1 y1)}
Term reduce_opx_sup(Term opx, Term sup) {
  inc_itr();
  Loc opx_loc = term_loc(opx);
  Loc sup_loc = term_loc(sup);
  Lab sup_lab = term_lab(sup);
  Term nmy    = got(opx_loc + 1);
  Term tm0    = got(sup_loc + 0);
  Term tm1    = got(sup_loc + 1);
  Loc du0     = alloc_node(3);
  Loc op0     = alloc_node(2);
  Loc op1     = alloc_node(2);
  Loc su0     = alloc_node(2);
  set(du0 + 0, term_new(SUB, 0, 0));
  set(du0 + 1, term_new(SUB, 0, 0));
  set(du0 + 2, nmy);
  set(op0 + 0, tm0);
  set(op0 + 1, term_new(DP0, 0, du0));
  set(op1 + 0, tm1);
  set(op1 + 1, term_new(DP1, 0, du0));
  set(su0 + 0, term_new(OPX, term_lab(opx), op0));
  set(su0 + 1, term_new(OPX, term_lab(opx), op1));
  return term_new(SUP, sup_lab, su0);
}

// <op(%L{x0 x1} y)
// ------------------------- OPX-USP
// ! %L{y0} = y
// %L{<op(x0 y0) <op(x1 y0)}
Term reduce_opx_usp(Term opx, Term usp) {
  //printf("opx-usp\n");
  inc_itr();
  Loc opx_loc = term_loc(opx);
  Loc usp_loc = term_loc(usp);
  Lab usp_lab = term_lab(usp);
  Term nmy    = got(opx_loc + 1);
  Term tm0    = got(usp_loc + 0);
  Term tm1    = got(usp_loc + 1);
  Loc op0     = alloc_node(2);
  Loc op1     = alloc_node(2);
  Loc us0     = alloc_node(2);
  Term cop;
  if (term_tag(nmy) != UDP) {
    Loc du0 = alloc_node(1);
    set(du0 + 0, nmy);
    cop = term_new(UDP, usp_lab, du0);
  } else {
    cop = nmy;
  }
  set(op0 + 0, tm0);
  set(op0 + 1, cop);
  set(op1 + 0, tm1);
  set(op1 + 1, cop);
  set(us0 + 0, term_new(OPX, term_lab(opx), op0));
  set(us0 + 1, term_new(OPX, term_lab(opx), op1));
  return term_new(USP, usp_lab, us0);
}

// <op(#{x0 x1 x2...} y)
// ---------------------- OPX-CTR
// ⊥
Term reduce_opx_ctr(Term opx, Term ctr) {
  printf("invalid:opx-ctr");
  exit(0);
}

// <op(x0 x1)
// ----------- OPX-W32
// <op(x0 x1)
Term reduce_opx_w32(Term opx, Term w32) {
  inc_itr();
  Lab opx_lab = term_lab(opx);
  Lab opx_loc = term_loc(opx);
  set(opx_loc + 0, w32);
  return term_new(OPY, opx_lab, opx_loc);
}

// >op(a *)
// --------- OPY-ERA
// *
Term reduce_opy_era(Term opy, Term era) {
  inc_itr();
  return era;
}

// >op(a λx(B))
// ------------- OPY-LAM
// *
Term reduce_opy_lam(Term opy, Term era) {
  printf("invalid:opy-lam");
  exit(0);
}

// >op(a &L{x y})
// ------------------------- OPY-SUP
// &L{>op(a x) >op(a y)}
Term reduce_opy_sup(Term opy, Term sup) {
  inc_itr();
  Loc opy_loc = term_loc(opy);
  Loc sup_loc = term_loc(sup);
  Lab sup_lab = term_lab(sup);
  Term nmx    = got(opy_loc + 0);
  Term tm0    = got(sup_loc + 0);
  Term tm1    = got(sup_loc + 1);
  Loc op0     = alloc_node(2);
  Loc op1     = alloc_node(2);
  Loc su0     = alloc_node(2);
  set(op0 + 0, nmx);
  set(op0 + 1, tm0);
  set(op1 + 0, nmx);
  set(op1 + 1, tm1);
  set(su0 + 0, term_new(OPY, term_lab(opy), op0));
  set(su0 + 1, term_new(OPY, term_lab(opy), op1));
  return term_new(SUP, sup_lab, su0);
}

// >op(a %L{x y})
// --------------------- OPY-USP
// %L{>op(a x) >op(a y)}
Term reduce_opy_usp(Term opy, Term usp) {
  inc_itr();
  Loc opy_loc = term_loc(opy);
  Loc usp_loc = term_loc(usp);
  Lab usp_lab = term_lab(usp);
  Term nmx    = got(opy_loc + 0);
  Term tm0    = got(usp_loc + 0);
  Term tm1    = got(usp_loc + 1);
  Loc op0     = alloc_node(2);
  Loc op1     = alloc_node(2);
  Loc us0     = alloc_node(2);
  set(op0 + 0, nmx);
  set(op0 + 1, tm0);
  set(op1 + 0, nmx);
  set(op1 + 1, tm1);
  set(us0 + 0, term_new(OPY, term_lab(opy), op0));
  set(us0 + 1, term_new(OPY, term_lab(opy), op1));
  return term_new(USP, usp_lab, us0);
}

// >op(#{x y z ...} b)
// ---------------------- OPY-CTR
// ⊥
Term reduce_opy_ctr(Term opy, Term ctr) {
  printf("invalid:opy-ctr");
  exit(0);
}

// >op(x y)
// --------- OPY-W32
// x op y
Term reduce_opy_w32(Term opy, Term w32) {
  inc_itr();
  Loc opy_loc = term_loc(opy);
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
  return term_new(W32, 0, result);
}

// ! %L{x} = *
// ----------- UDP-ERA
// x <- *
Term reduce_udp_era(Term udp, Term era) {
  inc_itr();
  return era;
}

// ! &L{F} = λx(f)
// ---------------- UDP-LAM
// ! &L{F} = λx0(G)
// ! &L{G} = f
// x <- &L{x0 x1}
// λx1(G)

// ! &L{F} = λx(f)
// ---------------- UDP-LAM
// ! &L{F} = *
// ! &L{G} = f
// x <- &L{x0 x1}
// λx1(G)

Term reduce_udp_lam(Term udp, Term lam) {
  inc_itr();
  Loc udp_loc = term_loc(udp);
  Lab udp_lab = term_lab(udp);
  Loc lam_loc = term_loc(lam);
  Term bod    = got(lam_loc + 1);
  Loc lm0     = alloc_node(2);
  Loc lm1     = alloc_node(2);
  Loc su0     = alloc_node(2);
  Term cop;
  if (term_tag(bod) != UDP) {
    Loc du0 = alloc_node(1);
    set(du0 + 0, bod);
    cop = term_new(UDP, udp_lab, du0);
  } else {
    cop = bod;
  }
  set(lm0 + 0, term_new(SUB, 0, 0));
  set(lm0 + 1, cop);
  set(lm1 + 0, term_new(SUB, 0, 0));
  set(lm1 + 1, cop);
  set(su0 + 0, term_new(VAR, 0, lm0));
  set(su0 + 1, term_new(VAR, 0, lm1));
  set(udp_loc + 0, term_new(LAM, 0, lm1));
  set(lam_loc + 0, term_new(USP, udp_lab, su0));
  return term_new(LAM, 0, lm0);

  //inc_itr();
  //Loc udp_loc = term_loc(udp);
  //Lab udp_lab = term_lab(udp);
  //Loc lam_loc = term_loc(lam);
  //Term bod    = got(lam_loc + 1);
  //Loc lm0     = alloc_node(2);
  //Loc lm1     = alloc_node(2);
  //Loc su0     = alloc_node(2);
  //Term cop;
  //if (term_tag(bod) != UDP) {
    //Loc du0 = alloc_node(1);
    //set(du0 + 0, bod);
    //cop = term_new(UDP, udp_lab, du0);
  //} else {
    //cop = bod;
  //}
  //set(lm0 + 0, term_new(SUB, 0, 0));
  //set(lm0 + 1, cop);
  //set(lm1 + 0, term_new(SUB, 0, 0));
  //set(lm1 + 1, cop);
  //set(su0 + 0, term_new(VAR, 0, lm0));
  //set(su0 + 1, term_new(VAR, 0, lm1));
  //set(udp_loc + 0, term_new(LAM, 0, lm0));
  //set(lam_loc + 0, term_new(USP, udp_lab, su0));
  //return term_new(LAM, 0, lm1);
}

// TODO: UDP-SUP
Term reduce_udp_sup(Term udp, Term sup) {
  printf("TODO:udp-sup\n");
  exit(0);
}

// ! %L{x} = %R{a b}
// ----------------- UDP-USP
// if L == R:
//   ! %L{x} = b
//   a
// else:
//   TODO
Term reduce_udp_usp(Term udp, Term usp) {
  inc_itr();
  Loc udp_loc = term_loc(udp);
  Lab udp_lab = term_lab(udp);
  Lab usp_lab = term_lab(usp);
  Loc usp_loc = term_loc(usp);
  if (udp_lab == usp_lab) {
    Term tm0 = got(usp_loc + 0);
    Term tm1 = got(usp_loc + 1);
    set(udp_loc + 0, tm1);
    return tm0;
  } else {
    printf("TODO:udp-usp\n");
    exit(0);
  }
}

// ! %L{x} = #{a b c ...}
// ---------------------- UDP-CTR
// x <- #{ax bx cx ...}
// ! %L{ax} = a
// ! %L{bx} = b
// ! %L{cx} = c
// ...
// #{ax bx cx ...}
// NOTE: this must create two separate CTRs
Term reduce_udp_ctr(Term udp, Term ctr) {
  inc_itr();
  Loc udp_loc = term_loc(udp);
  Lab udp_lab = term_lab(udp);
  Loc ctr_loc = term_loc(ctr);
  Lab ctr_lab = term_lab(ctr);
  u64 ctr_ari = u12v2_y(ctr_lab);
  Loc ct0     = alloc_node(ctr_ari);
  Loc ct1     = alloc_node(ctr_ari);
  for (u64 i = 0; i < ctr_ari; i++) {
    Term fld = got(ctr_loc + i);
    Term cop;
    if (term_tag(fld) != UDP) {
      Loc du0 = alloc_node(1);
      set(du0 + 0, got(ctr_loc + i));
      cop = term_new(UDP, udp_lab, du0);
    } else {
      cop = fld;
    }
    set(ct0 + i, cop);
    set(ct1 + i, cop);
  }
  set(udp_loc + 0, term_new(CTR, ctr_lab, ct1));
  return term_new(CTR, ctr_lab, ct0);
}

// ! %L{x} = 123
// ------------- UDP-W32
// 123
Term reduce_udp_w32(Term udp, Term w32) {
  inc_itr();
  return w32;
}

Term reduce(Term term) {
  //printf("reduce\n");
  if (term_tag(term) >= ERA) return term;
  Term next = term;
  u64  stop = *HVM.spos;
  u64* spos = HVM.spos;
  while (1) {
    //printf("NEXT! "); print_term(next); printf("\n");
    Tag tag = term_tag(next);
    Lab lab = term_lab(next);
    Loc loc = term_loc(next);
    switch (tag) {
      case LET: {
        switch (lab) {
          case LAZY: {
            next = reduce_let(next, got(loc + 1));
            continue;
          }
          case STRI: {
            HVM.sbuf[(*spos)++] = next;
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
        HVM.sbuf[(*spos)++] = next;
        next = got(loc + 0);
        continue;
      }
      case DP0:
      case DP1: {
        Loc key = term_key(next);
        Term sub = got(key);
        if (term_tag(sub) == SUB) {
          HVM.sbuf[(*spos)++] = next;
          next = got(loc + 2);
          continue;
        } else {
          next = sub;
          continue;
        }
      }
      case UDP: {
        Loc key = term_key(next);
        Term sub = got(key);
        if (term_tag(sub) == SUB) {
          HVM.sbuf[(*spos)++] = next;
          next = got(loc + 1);
          continue;
        } else {
          next = sub;
          continue;
        }
      }
      case MAT: {
        HVM.sbuf[(*spos)++] = next;
        next = got(loc + 0);
        continue;
      }
      case OPX: {
        HVM.sbuf[(*spos)++] = next;
        next = got(loc + 0);
        continue;
      }
      case OPY: {
        HVM.sbuf[(*spos)++] = next;
        next = got(loc + 1);
        continue;
      }
      case VAR: {
        Loc key = term_key(next);
        Term sub = got(key);
        if (term_tag(sub) == SUB) {
          break;
        } else {
          next = sub;
          continue;
        }
      }
      case REF: {
        next = reduce_ref(next); // TODO
        continue;
      }
      default: {
        if ((*spos) == stop) {
          break;
        } else {
          Term prev = HVM.sbuf[--(*spos)];
          Tag  ptag = term_tag(prev);
          Lab  plab = term_lab(prev);
          Loc  ploc = term_loc(prev);
          switch (ptag) {
            case LET: {
              next = reduce_let(prev, next);
              continue;
            }
            case APP: {
              switch (tag) {
                case ERA: next = reduce_app_era(prev, next); continue;
                case LAM: next = reduce_app_lam(prev, next); continue;
                case SUP: next = reduce_app_sup(prev, next); continue;
                case CTR: next = reduce_app_ctr(prev, next); continue;
                case W32: next = reduce_app_w32(prev, next); continue;
                case USP: next = reduce_app_usp(prev, next); continue;
                default: break;
              }
              break;
            }
            case DP0:
            case DP1: {
              switch (tag) {
                case ERA: next = reduce_dup_era(prev, next); continue;
                case LAM: next = reduce_dup_lam(prev, next); continue;
                case SUP: next = reduce_dup_sup(prev, next); continue;
                case CTR: next = reduce_dup_ctr(prev, next); continue;
                case W32: next = reduce_dup_w32(prev, next); continue;
                case USP: next = reduce_dup_usp(prev, next); continue;
                default: break;
              }
              break;
            }
            case MAT: {
              switch (tag) {
                case ERA: next = reduce_mat_era(prev, next); continue;
                case LAM: next = reduce_mat_lam(prev, next); continue;
                case SUP: next = reduce_mat_sup(prev, next); continue;
                case CTR: next = reduce_mat_ctr(prev, next); continue;
                case W32: next = reduce_mat_w32(prev, next); continue;
                case USP: next = reduce_mat_usp(prev, next); continue;
                default: break;
              }
            }
            case OPX: {
              switch (tag) {
                case ERA: next = reduce_opx_era(prev, next); continue;
                case LAM: next = reduce_opx_lam(prev, next); continue;
                case SUP: next = reduce_opx_sup(prev, next); continue;
                case CTR: next = reduce_opx_ctr(prev, next); continue;
                case W32: next = reduce_opx_w32(prev, next); continue;
                case USP: next = reduce_opx_usp(prev, next); continue;
                default: break;
              }
            }
            case OPY: {
              switch (tag) {
                case ERA: next = reduce_opy_era(prev, next); continue;
                case LAM: next = reduce_opy_lam(prev, next); continue;
                case SUP: next = reduce_opy_sup(prev, next); continue;
                case CTR: next = reduce_opy_ctr(prev, next); continue;
                case W32: next = reduce_opy_w32(prev, next); continue;
                case USP: next = reduce_opy_usp(prev, next); continue;
                default: break;
              }
            } 
            case UDP: {
              switch (tag) {
                case ERA: next = reduce_udp_era(prev, next); continue;
                case LAM: next = reduce_udp_lam(prev, next); continue;
                case SUP: next = reduce_udp_sup(prev, next); continue;
                case CTR: next = reduce_udp_ctr(prev, next); continue;
                case W32: next = reduce_udp_w32(prev, next); continue;
                case USP: next = reduce_udp_usp(prev, next); continue;
                default: break;
              }
            }
            default: break;
          }
          break;
        }
      }
    }
    if ((*HVM.spos) == stop) {
      //printf("retr: "); print_term(next); printf("\n");
      return next;
    } else {
      Term host = HVM.sbuf[--(*HVM.spos)];
      Tag  htag = term_tag(host);
      Lab  hlab = term_lab(host);
      Loc  hloc = term_loc(host);
      switch (htag) {
        case APP: set(hloc + 0, next); break;
        case DP0: set(hloc + 2, next); break;
        case DP1: set(hloc + 2, next); break;
        case MAT: set(hloc + 0, next); break;
      }
      return HVM.sbuf[0];
    }
  }
  printf("retr: ERR\n");
  return 0;
}

Term normal(Term term) {
  Term wnf = reduce(term);
  Tag tag = term_tag(wnf);
  Lab lab = term_lab(wnf);
  Loc loc = term_loc(wnf);
  switch (tag) {
    case APP: {
      Term fun = got(loc + 0);
      Term arg = got(loc + 1);
      fun = normal(fun);
      arg = normal(arg);
      set(loc + 0, fun);
      set(loc + 1, arg);
      return wnf;
    }
    case LAM: {
      Term bod = got(loc + 1);
      bod = normal(bod);
      set(loc + 1, bod);
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
      Term val = got(loc + 2);
      val = normal(val);
      set(loc + 2, val);
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
      u64 ari = lab;
      for (u64 i = 0; i <= ari; i++) {
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

// Runtime Memory
// --------------

void hvm_init() {
  // FIXME: use mmap instead
  HVM.sbuf  = malloc((1ULL << 32) * sizeof(Term));
  HVM.spos  = malloc(sizeof(u64));
  *HVM.spos = 0;
  HVM.heap  = malloc((1ULL << 32) * sizeof(ATerm));
  HVM.size  = malloc(sizeof(u64));
  HVM.itrs  = malloc(sizeof(u64));
  *HVM.size = 1;
  *HVM.itrs = 0;
}

void hvm_free() {
  free(HVM.sbuf);
  free(HVM.spos);
  free(HVM.heap);
  free(HVM.size);
  free(HVM.itrs);
}

State* hvm_get_state() {
  return &HVM;
}

void hvm_set_state(State* hvm) {
  HVM.sbuf = hvm->sbuf;
  HVM.spos = hvm->spos;
  HVM.heap = hvm->heap;
  HVM.size = hvm->size;
  HVM.itrs = hvm->itrs;
  for (int i = 0; i < 1024; i++) {
    HVM.book[i] = hvm->book[i];
  }
}

void hvm_define(u64 fid, Term (*func)()) {
  //printf("defined %llu %p\n", fid, func);
  HVM.book[fid] = func;
}
