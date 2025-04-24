//./Type.hs//
//./../IC.md//

#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <unistd.h>
#include <time.h>
#include <stdbool.h>

#define MAX_HEAP_SIZE (1ULL << 32)
#define MAX_STACK_SIZE (1ULL << 28)

typedef uint8_t  Tag;
typedef uint32_t Lab;
typedef uint32_t Loc;
typedef uint64_t Term;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

// Runtime Types
// -------------

typedef struct {
  Term*  sbuf; // reduction stack buffer
  u64*   spos; // reduction stack position
  Term*  heap; // global node buffer
  u64*   size; // global node buffer position
  u64*   itrs; // interaction count
  u64*   frsh; // fresh dup label count
  Term (*book[65536])(Term); // functions
  u16    cari[65536]; // arity of each constructor
  u16    clen[65536]; // case length of each constructor
  u16    cadt[65536]; // ADT id of each constructor
  u16    fari[65536]; // arity of each function
} State;

static State HVM = {
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

// Constants
// ---------

#define DP0 0x00
#define DP1 0x01
#define VAR 0x02
#define SUB 0x03
#define REF 0x04
#define LET 0x05
#define APP 0x06
#define MAT 0x08
#define IFL 0x09
#define SWI 0x0A
#define OPX 0x0B
#define OPY 0x0C
#define ERA 0x0D
#define LAM 0x0E
#define SUP 0x0F
#define CTR 0x10
#define W32 0x11
#define CHR 0x12
#define INC 0x13
#define DEC 0x14

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

#define DUP_F 0xFFFF
#define SUP_F 0xFFFE
#define LOG_F 0xFFFD

#define LAZY 0x0
#define STRI 0x1

#define VOID 0x00000000000000

// Heap
// ----

void set_len(u64 size) {
  *HVM.size = size;
}

void set_itr(u64 itrs) {
  *HVM.itrs = itrs;
}

u64 get_len() {
  return *HVM.size;
}

u64 get_itr() {
  return *HVM.itrs;
}

u64 fresh() {
  return (*HVM.frsh)++;
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

u64 term_get_bit(Term x) {
  return (x >> 7) & 1;
}

Term term_set_bit(Term term) {
  return term | (1ULL << 7);
}

Term term_rem_bit(Term term) {
  return term & ~(1ULL << 7);
}

Term term_set_loc(Term x, Loc loc) {
  return (x & 0x00000000FFFFFFFF) | (((Term)loc) << 32);
}

_Bool term_is_atom(Term term) {
  switch (term_tag(term)) {
    case ERA:
    case W32:
    case CHR: return 1;
    default: return 0;
  }
}

// Atomics
// -------

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

void set(Loc loc, Term term) {
  HVM.heap[loc] = term;
}

void sub(Loc loc, Term term) {
  set(loc, term_set_bit(term));
}

Term take(Loc loc) {
  return swap(loc, VOID);
}

// Allocation
// ----------

Loc alloc_node(Loc arity) {
  if (*HVM.size + arity > MAX_HEAP_SIZE) {
    printf("Heap memory limit exceeded\n");
    exit(1);
  }
  u64 old = *HVM.size;
  *HVM.size += arity;
  return old;
}

void inc_itr() {
  (*HVM.itrs)++;
}

// Stack
// ----

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

// Stringification
// ---------------

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

// Evaluation
// ----------

// @foo(&L{ax ay} b c ...)
// ----------------------- REF-SUP-COPY (when @L not in @foo)
// ! &L{bx by} = b
// ! &L{cx cy} = b
// ...
// &L{@foo(ax bx cx ...) @foo(ay by cy ...)}
Term reduce_ref_sup(Term ref, u16 idx) {
  inc_itr();
  Loc ref_loc = term_loc(ref);
  Lab ref_lab = term_lab(ref);
  u16 fun_id = ref_lab;
  u16 arity  = HVM.fari[fun_id];
  if (idx >= arity) {
    printf("ERROR: Invalid index in reduce_ref_sup\n");
    exit(1);
  }
  Term sup = got(ref_loc + idx);
  if (term_tag(sup) != SUP) {
    printf("ERROR: Expected SUP at index %u\n", idx);
    exit(1);
  }
  Lab sup_lab = term_lab(sup);
  Loc sup_loc = term_loc(sup);
  Term sup0 = got(sup_loc + 0);
  Term sup1 = got(sup_loc + 1);
  // Allocate space for new REF node arguments for the second branch
  Loc ref1_loc = alloc_node(arity);
  for (u64 i = 0; i < arity; ++i) {
    if (i != idx) {
      // Duplicate argument
      Term arg = got(ref_loc + i);
      Loc dup_loc = alloc_node(1);
      set(dup_loc + 0, arg);
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
  set(sup_loc + 0, ref0);
  set(sup_loc + 1, ref1);
  return term_new(SUP, sup_lab, sup_loc);
}

// @foo(a b c ...)
// -------------------- REF
// book[foo](a b c ...)
Term reduce_ref(Term ref) {
  //printf("reduce_ref "); print_term(ref); printf("\n");
  //printf("call %d %p\n", term_loc(ref), HVM.book[term_loc(ref)]);
  inc_itr();
  return HVM.book[term_lab(ref)](ref);
}

// ! x = val
// bod
// --------- LET
// x <- val
// bod
Term reduce_let(Term let, Term val) {
  //printf("reduce_let "); print_term(let); printf("\n");
  inc_itr();
  Loc let_loc = term_loc(let);
  Term bod    = got(let_loc + 1);
  sub(let_loc + 0, val);
  return bod;
}

// (* a)
// ------- APP-ERA
// *
Term reduce_app_era(Term app, Term era) {
  //printf("reduce_app_era "); print_term(app); printf("\n");
  inc_itr();
  return era;
}

// (λx(body) arg)
// ---------------- APP-LAM
// x <- arg
// body
Term reduce_app_lam(Term app, Term lam) {
  inc_itr();
  Loc app_loc = term_loc(app);
  Loc lam_loc = term_loc(lam);
  Term bod    = got(lam_loc + 0);
  Term arg    = got(app_loc + 1);
  sub(lam_loc + 0, arg);
  return bod;
}

// (&L{a b} c)
// --------------------- APP-SUP
// ! &L{x0 x1} = c
// &L{(a x0) (b x1)}
Term reduce_app_sup(Term app, Term sup) {
  //printf("reduce_app_sup "); print_term(app); printf("\n");
  inc_itr();
  Loc app_loc = term_loc(app);
  Loc sup_loc = term_loc(sup);
  Lab sup_lab = term_lab(sup);

  Term arg    = got(app_loc + 1);
  //Term tm0    = got(sup_loc + 0);
  Term tm1    = got(sup_loc + 1);

  Loc loc = alloc_node(3);
  Loc ap0 = sup_loc;
  Loc ap1 = loc + 0;
  Loc su0 = app_loc;
  Loc dup = loc + 2;

  //set(ap0 + 0, tm0)
  set(ap0 + 1, term_new(DP0, sup_lab, dup));

  set(ap1 + 0, tm1);
  set(ap1 + 1, term_new(DP1, sup_lab, dup));

  // Reuse app_loc for the result superposition
  set(su0 + 0, term_new(APP, 0, ap0));
  set(su0 + 1, term_new(APP, 0, ap1));

  set(dup + 0, arg);

  return term_new(SUP, sup_lab, su0);
  // return term_set_loc(sup, su0);
}

// &L(#{x y z ...} a)
// ------------------ APP-CTR
// ⊥
Term reduce_app_ctr(Term app, Term ctr) {
  //printf("reduce_app_ctr "); print_term(app); printf("\n");
  printf("invalid:app-ctr");
  exit(0);
}

// &L(123 a)
// --------- APP-W32
// ⊥
Term reduce_app_w32(Term app, Term w32) {
  //printf("reduce_app_w32 "); print_term(app); printf("\n");
  printf("invalid:app-w32");
  exit(0);
}

// (↑f x)
// ------- APP-INC
// ↑(f x)
Term reduce_app_una(Term app, Term una, Tag tag) {
  inc_itr();
  Loc app_loc = term_loc(app);
  Loc una_loc = term_loc(una);
  Term fun    = got(una_loc + 0);
  Term arg    = got(app_loc + 1);

  /* build the inner application in-place, re-using app_loc */
  set(app_loc + 0, fun);
  set(app_loc + 1, arg);

  /* point INC/DEC to the freshly built APP */
  set(una_loc + 0, term_new(APP, 0, app_loc));
  return una;
}

Term reduce_app_inc(Term app, Term inc) {
  //printf("reduce_app_inc "); print_term(app); printf("\n");
  return reduce_app_una(app, inc, INC);
}

Term reduce_app_dec(Term app, Term dec) {
  //printf("reduce_app_dec "); print_term(app); printf("\n");
  return reduce_app_una(app, dec, DEC);
}

// ! &L{x y} = *
// ------------- DUP-ERA
// x <- *
// y <- *
Term reduce_dup_era(Term dup, Term era) {
  //printf("reduce_dup_era "); print_term(dup); printf("\n");
  inc_itr();
  Loc dup_loc = term_loc(dup);
  sub(dup_loc + 0, era);
  return era;
}

// ! &L{r s} = λx(f)
// ------------------- DUP-LAM
// ! &L{f0 f1} = f
// r <- λx0(f0)
// s <- λx1(f1)
// x <- &L{x0 x1}
Term reduce_dup_lam(Term dup, Term lam) {
  //printf("reduce_dup_lam "); print_term(dup); printf("\n");
  inc_itr();
  Loc dup_loc = term_loc(dup);
  Loc lam_loc = term_loc(lam);
  Lab dup_lab = term_lab(dup);

  Term bod    = got(lam_loc + 0);
  
  Loc loc     = alloc_node(5);
  Loc lm0     = loc + 0;
  Loc lm1     = loc + 1;
  Loc su0     = loc + 2;
  Loc du0     = loc + 4;

  sub(lam_loc + 0, term_new(SUP, dup_lab, su0));

  set(lm0 + 0, term_new(DP0, dup_lab, du0));
  set(lm1 + 0, term_new(DP1, dup_lab, du0));
  set(su0 + 0, term_new(VAR, 0, lm0));
  set(su0 + 1, term_new(VAR, 0, lm1));
  set(du0 + 0, bod);

  if (term_tag(dup) == DP0) {
    sub(dup_loc + 0, term_new(LAM, 0, lm1));
    return term_new(LAM, 0, lm0);
  } else {
    sub(dup_loc + 0, term_new(LAM, 0, lm0));
    return term_new(LAM, 0, lm1);
  }
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
  //printf("reduce_dup_sup %u %u | %llu ", term_lab(dup), term_lab(sup), *HVM.spos); print_term(dup); printf(" "); print_term(sup); printf("\n");
  inc_itr();
  Loc dup_loc = term_loc(dup);
  Lab dup_lab = term_lab(dup);
  Lab sup_lab = term_lab(sup);
  Loc sup_loc = term_loc(sup);
  if (dup_lab == sup_lab) {
    Term tm0 = got(sup_loc + 0);
    Term tm1 = got(sup_loc + 1);
    if (term_tag(dup) == DP0) {
      sub(dup_loc + 0, tm1);
      return tm0;
    } else {
      sub(dup_loc + 0, tm0);
      return tm1;
    }
  } else {
    Loc loc = alloc_node(4);
    Loc du0 = sup_loc + 0;
    Loc du1 = sup_loc + 1;
    Loc su0 = loc + 0;
    Loc su1 = loc + 2;
    //Term tm0 = got(sup_loc + 0);
    //Term tm1 = got(sup_loc + 1);
    //set(du0 + 0, tm0);
    //set(du1 + 0, tm1);
    set(su0 + 0, term_new(DP0, dup_lab, du0));
    set(su0 + 1, term_new(DP0, dup_lab, du1));
    set(su1 + 0, term_new(DP1, dup_lab, du0));
    set(su1 + 1, term_new(DP1, dup_lab, du1));
    if (term_tag(dup) == DP0) {
      sub(dup_loc + 0, term_new(SUP, sup_lab, su1));
      return term_new(SUP, sup_lab, su0);
    } else {
      sub(dup_loc + 0, term_new(SUP, sup_lab, su0));
      return term_new(SUP, sup_lab, su1);
    }
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
Term reduce_dup_ctr(Term dup, Term ctr) {
  //printf("reduce_dup_ctr "); print_term(dup); printf("\n");
  inc_itr();
  Loc dup_loc = term_loc(dup);
  Lab dup_lab = term_lab(dup);
  Loc ctr_loc = term_loc(ctr);
  Lab ctr_lab = term_lab(ctr);
  u64 ctr_ari = HVM.cari[ctr_lab];

  Loc loc     = alloc_node(ctr_ari * 2);
  //Loc ctr0    = alloc_node(ctr_ari);
  Loc ctr0    = ctr_loc;
  Loc ctr1    = loc + 0;
  for (u64 i = 0; i < ctr_ari; i++) {
    Loc du0 = loc + ctr_ari + i;
    set(du0 + 0, got(ctr_loc + i));
    set(ctr0 + i, term_new(DP0, dup_lab, du0));
    set(ctr1 + i, term_new(DP1, dup_lab, du0));
  }
  if (term_tag(dup) == DP0) {
    sub(dup_loc + 0, term_new(CTR, ctr_lab, ctr1));
    return term_new(CTR, ctr_lab, ctr0);
  } else {
    sub(dup_loc + 0, term_new(CTR, ctr_lab, ctr0));
    return term_new(CTR, ctr_lab, ctr1);
  }
}

// ! &L{x y} = 123
// --------------- DUP-W32
// x <- 123
// y <- 123
Term reduce_dup_w32(Term dup, Term w32) {
  //printf("reduce_dup_w32 "); print_term(dup); printf("\n");
  inc_itr();
  Loc dup_loc = term_loc(dup);
  sub(dup_loc + 0, w32);
  return w32;
}

// ! &L{x y} = @foo(a b c ...)
// --------------------------- DUP-REF-COPY (when &L not in @foo)
// ! &L{a0 a1} = a
// ! &L{b0 b1} = b
// ! &L{c0 c1} = c
// ...
// x <- @foo(a0 b0 c0 ...)
// y <- @foo(a1 b1 c1 ...)
Term reduce_dup_ref(Term dup, Term ref) {
  //printf("reduce_dup_ref "); print_term(dup); printf("\n");
  inc_itr();
  Loc dup_loc = term_loc(dup);
  Lab dup_lab = term_lab(dup);
  Loc ref_loc = term_loc(ref);
  Lab ref_lab = term_lab(ref);
  u64 ref_ari = HVM.fari[ref_lab];

  Loc loc     = alloc_node(ref_ari * 2);
  Loc ref0    = ref_loc;
  Loc ref1    = loc + 0;
  for (u64 i = 0; i < ref_ari; i++) {
    Loc du0 = loc + ref_ari + i;
    set(du0 + 0, got(ref_loc + i));
    set(ref0 + i, term_new(DP0, dup_lab, du0));
    set(ref1 + i, term_new(DP1, dup_lab, du0));
  }
  if (term_tag(dup) == DP0) {
    sub(dup_loc + 0, term_new(REF, ref_lab, ref1));
    return term_new(REF, ref_lab, ref0);
  } else {
    sub(dup_loc + 0, term_new(REF, ref_lab, ref0));
    return term_new(REF, ref_lab, ref1);
  }
}

// ! &L{a b} = ↑x
// ------------- DUP-INC/DEC
// ! &L{A B} = x
// a <- ↑A
// b <- ↑B
Term reduce_dup_una(Term dup, Term una, Tag tag) {
  inc_itr();
  Loc dup_loc = term_loc(dup);
  Lab lab     = term_lab(dup);
  Loc una_loc = term_loc(una);
  Term inner  = got(una_loc + 0);

  /* duplicate inner value */
  Loc du_loc  = alloc_node(1);
  set(du_loc + 0, inner);

  Term dp0 = term_new(DP0, lab, du_loc);
  Term dp1 = term_new(DP1, lab, du_loc);

  /* wrap duplicates in INC / DEC */
  Loc w0_loc = alloc_node(1); set(w0_loc + 0, dp0);
  Loc w1_loc = alloc_node(1); set(w1_loc + 0, dp1);
  Term w0 = term_new(tag, 0, w0_loc);
  Term w1 = term_new(tag, 0, w1_loc);

  if (term_tag(dup) == DP0) {
    sub(dup_loc + 0, w1);
    return w0;
  } else {
    sub(dup_loc + 0, w0);
    return w1;
  }
}

Term reduce_dup_inc(Term dup, Term inc) {
  //printf("reduce_dup_inc "); print_term(dup); printf("\n");
  return reduce_dup_una(dup, inc, INC);
}

Term reduce_dup_dec(Term dup, Term dec) {
  //printf("reduce_dup_dec "); print_term(dup); printf("\n");
  return reduce_dup_una(dup, dec, DEC);
}

// ~ * {K0 K1 K2 ...} 
// ------------------ MAT-ERA
// *
Term reduce_mat_era(Term mat, Term era) {
  //printf("reduce_mat_era "); print_term(mat); printf("\n");
  inc_itr();
  return era;
}

// ~ λx(x) {K0 K1 K2 ...}
// ------------------------ MAT-LAM
// ⊥
Term reduce_mat_lam(Term mat, Term lam) {
  //printf("reduce_mat_lam "); print_term(mat); printf("\n");
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
Term reduce_mat_sup(Term mat, Term sup) {
  //printf("reduce_mat_sup "); print_term(mat); printf("\n");
  inc_itr();
  Tag mat_tag = term_tag(mat);
  Lab mat_lab = term_lab(mat);
  Loc mat_loc = term_loc(mat);
  Loc sup_loc = term_loc(sup);
  Lab sup_lab = term_lab(sup);

  Term tm0    = got(sup_loc + 0);
  Term tm1    = got(sup_loc + 1);
  u64 mat_len = mat_tag == SWI ? mat_lab : mat_tag == IFL ? 2 : HVM.clen[mat_lab];

  Loc loc     = alloc_node(1 + mat_len + mat_len);
  Loc mat0    = mat_loc;
  Loc mat1    = loc + 0;
  Loc sup0    = sup_loc;

  set(mat0 + 0, tm0);
  set(mat1 + 0, tm1);
  for (u64 i = 0; i < mat_len; i++) {
    Loc du0 = loc + 1 + mat_len + i;
    set(du0 + 0, got(mat_loc + 1 + i));
    set(mat0 + 1 + i, term_new(DP0, sup_lab, du0));
    set(mat1 + 1 + i, term_new(DP1, sup_lab, du0));
  }
  set(sup0 + 0, term_new(mat_tag, mat_lab, mat0));
  set(sup0 + 1, term_new(mat_tag, mat_lab, mat1));
  return term_new(SUP, sup_lab, sup0);
}

Term reduce_mat_ctr(Term mat, Term ctr) {
  inc_itr();
  Tag mat_tag = term_tag(mat);
  Loc mat_loc = term_loc(mat);
  Lab mat_lab = term_lab(mat);
  // If-Let
  if (mat_tag == IFL) {
    Loc ctr_loc = term_loc(ctr);
    Lab ctr_lab = term_lab(ctr);
    u64 mat_ctr = mat_lab;
    u64 ctr_num = ctr_lab;
    u64 ctr_ari = HVM.cari[ctr_num];
    if (mat_ctr == ctr_num) {
      Term app = got(mat_loc + 1);
      Loc loc = alloc_node(ctr_ari * 2);
      for (u64 i = 0; i < ctr_ari; i++) {
        Loc new_app = loc + i * 2;
        set(new_app + 0, app);
        set(new_app + 1, got(ctr_loc + i));
        app = term_new(APP, 0, new_app);
      }
      return app;
    } else {
      Term app = got(mat_loc + 2);
      Loc new_app = mat_loc;
      set(new_app + 0, app);
      set(new_app + 1, ctr);
      app = term_new(APP, 0, new_app);
      return app;
    }
  // Match
  } else {
    Loc ctr_loc = term_loc(ctr);
    Lab ctr_lab = term_lab(ctr);
    u64 ctr_num = ctr_lab;
    u64 ctr_ari = HVM.cari[ctr_num];
    u64 mat_ctr = mat_lab;
    u64 cse_idx = ctr_num - mat_ctr;
    Term app = got(mat_loc + 1 + cse_idx);
    Loc loc = alloc_node(ctr_ari * 2);
    for (u64 i = 0; i < ctr_ari; i++) {
      Loc new_app = loc + i * 2;
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
Term reduce_mat_w32(Term mat, Term w32) {
  //printf("reduce_mat_w32 "); print_term(mat); printf("\n");
  inc_itr();
  Loc mat_loc = term_loc(mat);
  Lab mat_lab = term_lab(mat);
  u64 mat_len = mat_lab;
  u64 w32_val = term_loc(w32);
  if (w32_val < mat_len - 1) {
    return got(mat_loc + 1 + w32_val);
  } else {
    Term fn = got(mat_loc + mat_len);
    Loc app = mat_loc;
    set(app + 0, fn);
    set(app + 1, term_new(W32, 0, w32_val - (mat_len - 1)));
    return term_new(APP, 0, app);
  }
}

// ~(↑x) {…}  →  ↑(~x {…})
Term reduce_mat_una(Term mat, Term una, Tag tag) {
  inc_itr();
  Loc mat_loc = term_loc(mat);
  Loc una_loc = term_loc(una);
  Term inner  = got(una_loc + 0);

  set(mat_loc + 0, inner);       /* plug x inside the matcher */
  set(una_loc + 0, mat);         /* re-attach wrapped matcher */
  return una;
}

Term reduce_mat_inc(Term mat, Term inc) {
  //printf("reduce_mat_inc "); print_term(mat); printf("\n");
  return reduce_mat_una(mat, inc, INC);
}

Term reduce_mat_dec(Term mat, Term dec) {
  //printf("reduce_mat_dec "); print_term(mat); printf("\n");
  return reduce_mat_una(mat, dec, DEC);
}

// <op(* b)
// -------- OPX-ERA
// *
Term reduce_opx_era(Term opx, Term era) {
  //printf("reduce_opx_era "); print_term(opx); printf("\n");
  inc_itr();
  return era;
}

// <op(λx(B) y)
// --------------- OPX-LAM
// ⊥
Term reduce_opx_lam(Term opx, Term lam) {
  //printf("reduce_opx_lam "); print_term(opx); printf("\n");
  printf("invalid:opx-lam");
  exit(0);
}

// <op(&L{x0 x1} y)
// ------------------------- OPX-SUP
// ! &L{y0 y1} = y
// &L{<op(x0 y0) <op(x1 y1)}
Term reduce_opx_sup(Term opx, Term sup) {
  //printf("reduce_opx_sup "); print_term(opx); printf("\n");
  inc_itr();
  Loc opx_loc = term_loc(opx);
  Loc sup_loc = term_loc(sup);
  Lab sup_lab = term_lab(sup);
  Term nmy    = got(opx_loc + 1);
  Term tm0    = got(sup_loc + 0);
  Term tm1    = got(sup_loc + 1);
  Loc loc     = alloc_node(3);
  Loc op0     = opx_loc;
  Loc op1     = sup_loc;
  Loc su0     = loc + 0;
  Loc du0     = loc + 2;
  set(op0 + 0, tm0);
  set(op0 + 1, term_new(DP0, sup_lab, du0));
  set(op1 + 0, tm1);
  set(op1 + 1, term_new(DP1, sup_lab, du0));
  set(su0 + 0, term_new(OPX, term_lab(opx), op0));
  set(su0 + 1, term_new(OPX, term_lab(opx), op1));
  set(du0 + 0, nmy);
  return term_new(SUP, sup_lab, su0);
}

// <op(#{x0 x1 x2...} y)
// --------------------- OPX-CTR
// ⊥
Term reduce_opx_ctr(Term opx, Term ctr) {
  //printf("reduce_opx_ctr "); print_term(opx); printf("\n");
  printf("invalid:opx-ctr");
  exit(0);
}

// <op(x0 x1)
// ---------- OPX-W32
// >op(x0 x1)
Term reduce_opx_w32(Term opx, Term nmx) {
  //printf("reduce_opx_w32 "); print_term(opx); printf("\n");
  inc_itr();
  Lab opx_lab = term_lab(opx);
  Loc opx_loc = term_loc(opx);
  Term nmy = got(opx_loc + 1);
  set(opx_loc + 0, nmy);
  set(opx_loc + 1, nmx);
  return term_new(OPY, opx_lab, opx_loc);
}

// <op(↑x y)  →  ↑<op(x y)
Term reduce_opx_una(Term opx, Term una, Tag tag) {
  inc_itr();
  Loc opx_loc = term_loc(opx);
  Loc una_loc = term_loc(una);
  Term lhs    = got(una_loc + 0);
  Term rhs    = got(opx_loc + 1);         /* already stored */

  set(opx_loc + 0, lhs);
  set(opx_loc + 1, rhs);
  set(una_loc + 0, term_new(OPX, term_lab(opx), opx_loc));
  return una;
}

Term reduce_opx_inc(Term opx, Term inc) {
  //printf("reduce_opx_inc "); print_term(opx); printf("\n");
  return reduce_opx_una(opx, inc, INC);
}

Term reduce_opx_dec(Term opx, Term dec) {
  //printf("reduce_opx_dec "); print_term(opx); printf("\n");
  return reduce_opx_una(opx, dec, DEC);
}

// >op(a *)
// -------- OPY-ERA
// *
Term reduce_opy_era(Term opy, Term era) {
  //printf("reduce_opy_era "); print_term(opy); printf("\n");
  inc_itr();
  return era;
}

// >op(a λx(B))
// ------------ OPY-LAM
// *
Term reduce_opy_lam(Term opy, Term era) {
  //printf("reduce_opy_lam "); print_term(opy); printf("\n");
  printf("invalid:opy-lam");
  exit(0);
}

// >op(a &L{x y})
// --------------------- OPY-SUP
// &L{>op(a x) >op(a y)}
Term reduce_opy_sup(Term opy, Term sup) {
  //printf("reduce_opy_sup "); print_term(opy); printf("\n");
  inc_itr();
  Loc opy_loc = term_loc(opy);
  Loc sup_loc = term_loc(sup);
  Lab sup_lab = term_lab(sup);
  Term nmx    = got(opy_loc + 1);
  Term tm0    = got(sup_loc + 0);
  Term tm1    = got(sup_loc + 1);
  Loc op0     = sup_loc;
  Loc op1     = opy_loc;
  Loc su0     = alloc_node(2);
  //set(op0 + 0, tm0);
  set(op0 + 1, nmx);
  set(op1 + 0, tm1);
  //set(op1 + 1, nmx);
  set(su0 + 0, term_new(OPY, term_lab(opy), op0));
  set(su0 + 1, term_new(OPY, term_lab(opy), op1));
  return term_new(SUP, sup_lab, su0);
}

// >op(#{x y z ...} b)
// ---------------------- OPY-CTR
// ⊥
Term reduce_opy_ctr(Term opy, Term ctr) {
  //printf("reduce_opy_ctr "); print_term(opy); printf("\n");
  printf("invalid:opy-ctr");
  exit(0);
}

// >op(x y)
// --------- OPY-W32
// x op y
Term reduce_opy_w32(Term opy, Term w32) {
  //printf("reduce_opy_w32 "); print_term(opy); printf("\n");
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

// >op(a ↑y)  →  ↑>op(a y)
Term reduce_opy_una(Term opy, Term una, Tag tag) {
  inc_itr();
  Loc opy_loc = term_loc(opy);
  Loc una_loc = term_loc(una);
  Term rhs    = got(una_loc + 0);
  Term lhs    = got(opy_loc + 1);         /* first operand stored at +1 */

  set(opy_loc + 0, rhs);
  set(opy_loc + 1, lhs);
  set(una_loc + 0, term_new(OPY, term_lab(opy), opy_loc));
  return una;
}

Term reduce_opy_inc(Term opy, Term inc) {
  //printf("reduce_opy_inc "); print_term(opy); printf("\n");
  return reduce_opy_una(opy, inc, INC);
}

Term reduce_opy_dec(Term opy, Term dec) {
  //printf("reduce_opy_dec "); print_term(opy); printf("\n");
  return reduce_opy_una(opy, dec, DEC);
}

Term reduce(Term term) {
  if (term_tag(term) >= ERA) return term;
  Term  next = term;
  u64   stop = *HVM.spos;
  u64   spos = stop;
  Term* sbuf = HVM.sbuf;

  while (1) {
    //printf("NEXT "); print_term(term); printf("\n");
    //printf("PATH ");
    //for (u64 i = 0; i < *spos; ++i) {
      //print_tag(term_tag(HVM.sbuf[i]));
      //printf(" ");
    //}
    //printf(" ~ %p", HVM.sbuf);
    //printf("\n");
    Tag tag = term_tag(next);
    Lab lab = term_lab(next);
    Loc loc = term_loc(next);


    // On variables: substitute
    // On eliminators: move to field
    switch (tag) {

      case LET: {
        switch (lab) {
          case LAZY: {
            next = reduce_let(next, got(loc + 0));
            continue;
          }
          case STRI: {
            spush(next, sbuf, &spos);
            next = got(loc + 0);
            continue;
          }
          default: {
            printf("invalid:let");
            exit(0);
          }
        }
      }

      case APP:
      case MAT:
      case IFL:
      case SWI:
      case OPX:
      case OPY: {
        spush(next, sbuf, &spos);
        next = got(loc + 0);
        continue;
      }

      case DP0:
      case DP1: {
        Term sub = got(loc + 0);
        if (term_get_bit(sub) == 0) {
          spush(next, sbuf, &spos);
          next = sub;
          continue;
        } else {
          next = term_rem_bit(sub);
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
        *HVM.spos = spos;
        next = reduce_ref(next);
        spos = *HVM.spos;
        continue;
      }

      default: break;
    }

    // Empty stack: term is in WHNF
    if (spos == stop) {
      *HVM.spos = spos;
      return next;
    }

    // Interaction Dispatcher
    Term prev = spop(sbuf, &spos);
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
          case CHR: next = reduce_app_w32(prev, next); continue;
          case INC: next = reduce_app_inc(prev, next); continue;
          case DEC: next = reduce_app_dec(prev, next); continue;
          default: break;
        }
      }

      case DP0:
      case DP1: {
        switch (tag) {
          case ERA: next = reduce_dup_era(prev, next); continue;
          case LAM: next = reduce_dup_lam(prev, next); continue;
          case SUP: next = reduce_dup_sup(prev, next); continue;
          case CTR: next = reduce_dup_ctr(prev, next); continue;
          case W32: next = reduce_dup_w32(prev, next); continue;
          case CHR: next = reduce_dup_w32(prev, next); continue;
          case INC: next = reduce_dup_inc(prev, next); continue;
          case DEC: next = reduce_dup_dec(prev, next); continue;
          default: break;
        }
      }

      case MAT:
      case IFL:
      case SWI: {
        switch (tag) {
          case ERA: next = reduce_mat_era(prev, next); continue;
          case LAM: next = reduce_mat_lam(prev, next); continue;
          case SUP: next = reduce_mat_sup(prev, next); continue;
          case CTR: next = reduce_mat_ctr(prev, next); continue;
          case W32: next = reduce_mat_w32(prev, next); continue;
          case CHR: next = reduce_mat_w32(prev, next); continue;
          case INC: next = reduce_mat_inc(prev, next); continue;
          case DEC: next = reduce_mat_dec(prev, next); continue;
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
          case CHR: next = reduce_opx_w32(prev, next); continue;
          case INC: next = reduce_opx_inc(prev, next); continue;
          case DEC: next = reduce_opx_dec(prev, next); continue;
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
          case CHR: next = reduce_opy_w32(prev, next); continue;
          case INC: next = reduce_opy_inc(prev, next); continue;
          case DEC: next = reduce_opy_dec(prev, next); continue;
          default: break;
        }
      }

      default: break;
    }

    // No interaction: push term back to stack
    spush(prev, sbuf, &spos);

    // Update parent chain
    while (spos > stop) {
      Term host = spop(sbuf, &spos);
      Tag  htag = term_tag(host);
      Lab  hlab = term_lab(host);
      Loc  hloc = term_loc(host);
      set(hloc + 0, next);
      next = host;
    }
    *HVM.spos = spos;
    return next;
  }
}

Term reduce_at(Loc host) {
  Term tm0 = got(host);
  if (term_tag(tm0) >= ERA) {
    return tm0;
  }
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

    case LAM: {
      Term bod = got(loc + 0);
      bod = normal(bod);
      set(term_loc(wnf) + 1, bod);
      return wnf;
    }

    case APP: {
      Term fun = got(loc + 0);
      Term arg = got(loc + 1);
      fun = normal(fun);
      arg = normal(arg);
      set(term_loc(wnf) + 0, fun);
      set(term_loc(wnf) + 1, arg);
      return wnf;
    }

    case SUP: {
      Term tm0 = got(loc + 0);
      Term tm1 = got(loc + 1);
      tm0 = normal(tm0);
      tm1 = normal(tm1);
      set(term_loc(wnf) + 0, tm0);
      set(term_loc(wnf) + 1, tm1);
      return wnf;
    }

    case DP0:
    case DP1: {
      Term val = got(loc + 0);
      val = normal(val);
      set(term_loc(wnf) + 0, val);
      return wnf;
    }

    case CTR: {
      u64 cid = lab;
      u64 ari = HVM.cari[cid];
      for (u64 i = 0; i < ari; i++) {
        Term arg = got(loc + i);
        arg = normal(arg);
        set(term_loc(wnf) + i, arg);
      }
      return wnf;
    }

    case MAT:
    case IFL:
    case SWI: {
      u64 len = tag == SWI ? lab : tag == IFL ? 2 : HVM.clen[lab];
      for (u64 i = 0; i <= len; i++) {
        Term arg = got(loc + i);
        arg = normal(arg);
        set(term_loc(wnf) + i, arg);
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
Term SUP_f(Term ref) {
  Loc ref_loc = term_loc(ref);
  Term lab = reduce(got(ref_loc + 0));
  Term lab_val = term_loc(lab);
  if (term_tag(lab) != W32) {
    printf("ERROR:non-numeric-sup-label\n");
  }
  if (lab_val > 0xFFFF) {
    printf("ERROR:sup-label-too-large\n");
  }
  Term tm0 = got(ref_loc + 1);
  Term tm1 = got(ref_loc + 2);
  Loc  sup = alloc_node(2);
  Term ret = term_new(SUP, lab_val, sup);
  set(sup + 0, tm0);
  set(sup + 1, tm1);
  *HVM.itrs += 1;
  return ret;
}

// Primitive: Dynamic Dup `@DUP(lab val λdp0λdp1(bod))`
// Creates a DUP node with given label.
Term DUP_f(Term ref) {
  Loc ref_loc = term_loc(ref);
  Term lab = reduce(got(ref_loc + 0));
  Term lab_val = term_loc(lab);
  if (term_tag(lab) != W32) {
    printf("ERROR:non-numeric-dup-label\n");
  }
  if (lab_val > 0xFFFF) {
    printf("ERROR:dup-label-too-large\n");
  }
  Term val = got(ref_loc + 1);
  Term bod = got(ref_loc + 2);
  Loc  dup = alloc_node(1);
  set(dup + 0, val);
  if (term_tag(bod) == LAM) {
    Loc  lam0 = term_loc(bod);
    Term bod0 = got(lam0 + 0);
    if (term_tag(bod0) == LAM) {
      Loc  lam1 = term_loc(bod0);
      Term bod1 = got(lam1 + 0);
      sub(lam0 + 0, term_new(DP0, lab_val, dup));
      sub(lam1 + 0, term_new(DP1, lab_val, dup));
      *HVM.itrs += 3;
      return bod1;
    }
  }
  Loc app0 = alloc_node(2);
  set(app0 + 0, bod);
  set(app0 + 1, term_new(DP0, lab_val, dup));
  Loc app1 = alloc_node(2);
  set(app1 + 0, term_new(APP, 0, app0));
  set(app1 + 1, term_new(DP1, lab_val, dup));
  *HVM.itrs += 1;
  return term_new(APP, 0, app1);
}

Term LOG_f(Term ref) {
  printf("TODO: LOG_f");
  exit(0);
}

void *alloc_huge(size_t size) {
    void *ptr = mmap(NULL, size, PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE,
                     -1, 0);
    if (ptr == MAP_FAILED) {
        perror("mmap failed");
        return NULL;
    }
    return ptr;
}

// Runtime Memory
// --------------

void hvm_init() {
  HVM.sbuf = alloc_huge(MAX_STACK_SIZE * sizeof(Term)); 
  HVM.heap = alloc_huge(MAX_HEAP_SIZE  * sizeof(Term));
  HVM.spos = alloc_huge(sizeof(u64));
  HVM.size = alloc_huge(sizeof(u64));
  HVM.itrs = alloc_huge(sizeof(u64));
  HVM.frsh = alloc_huge(sizeof(u64));

  #define CHECK_ALLOC(ptr, name) if (!(ptr)) { printf(name " alloc failed\n"); allocs_failed++; }
  int allocs_failed = 0; // Track if any allocation failed

  CHECK_ALLOC(HVM.sbuf, "sbuf");
  CHECK_ALLOC(HVM.heap, "heap");
  CHECK_ALLOC(HVM.spos, "spos");
  CHECK_ALLOC(HVM.size, "size");
  CHECK_ALLOC(HVM.itrs, "itrs");
  CHECK_ALLOC(HVM.frsh, "frsh");

  if (allocs_failed > 0) {
    printf("hvm_init alloc's failed: %d allocations failed\n", allocs_failed);
    exit(1);
  }
  #undef CHECK_ALLOC

  *HVM.spos = 0;
  *HVM.size = 1;
  *HVM.itrs = 0;
  *HVM.frsh = 0x20;
  HVM.book[SUP_F] = SUP_f;
  HVM.book[DUP_F] = DUP_f;
  HVM.book[LOG_F] = LOG_f;
  for (int i = 0; i < 65536; i++) {
    HVM.cari[i] = 0;
    HVM.clen[i] = 0;
    HVM.cadt[i] = 0;
    HVM.fari[i] = 0;
  }
}

void hvm_munmap(void *ptr, size_t size, const char *name) {
    if (ptr != MAP_FAILED) {
        if (munmap(ptr, size) == -1) {
            perror("munmap failed");
        } else {
            // printf("Successfully unmapped %s\n", name);
        }
    } else {
        printf("%s is already null or invalid.\n", name);
    }
}

void hvm_free() {
    hvm_munmap(HVM.sbuf, MAX_STACK_SIZE * sizeof(Term), "sbuf");
    hvm_munmap(HVM.heap, MAX_HEAP_SIZE  * sizeof(Term), "heap");
    hvm_munmap(HVM.spos, sizeof(u64), "spos");
    hvm_munmap(HVM.size, sizeof(u64), "size");
    hvm_munmap(HVM.itrs, sizeof(u64), "itrs");
    hvm_munmap(HVM.frsh, sizeof(u64), "frsh");
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
  HVM.frsh = hvm->frsh;
  for (int i = 0; i < 65536; i++) {
    HVM.book[i] = hvm->book[i];
    HVM.fari[i] = hvm->fari[i];
    HVM.cari[i] = hvm->cari[i];
    HVM.clen[i] = hvm->clen[i];
    HVM.cadt[i] = hvm->cadt[i];
  }
}

void hvm_define(u16 fid, Term (*func)()) {
  //printf("defined %llu %p\n", fid, func);
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

