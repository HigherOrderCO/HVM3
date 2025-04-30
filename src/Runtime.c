//./../IC.md//
//./Type.hs//

//#include <inttypes.h>
//#include <stdint.h>
//#include <stdio.h>
//#include <stdlib.h>
//#include <sys/mman.h>
//#include <unistd.h>
//#include <time.h>
//#include <stdbool.h>

//#define MAX_HEAP_SIZE (1ULL << 32)
//#define MAX_STACK_SIZE (1ULL << 28)

//typedef uint8_t  Tag;
//typedef uint32_t Lab;
//typedef uint32_t Loc;
//typedef uint64_t Term;
//typedef uint16_t u16;
//typedef uint32_t u32;
//typedef uint64_t u64;

//// Runtime Types
//// -------------

//typedef struct {
  //Term*  sbuf; // reduction stack buffer
  //u64*   spos; // reduction stack position
  //Term*  heap; // global node buffer
  //u64*   size; // global node buffer position
  //u64*   itrs; // interaction count
  //u64*   frsh; // fresh dup label count
  //Term (*book[65536])(Term); // functions
  //u16    cari[65536]; // arity of each constructor
  //u16    clen[65536]; // case length of each constructor
  //u16    cadt[65536]; // ADT id of each constructor
  //u16    fari[65536]; // arity of each function
//} State;

//static State HVM = {
  //.sbuf = NULL,
  //.spos = NULL,
  //.heap = NULL,
  //.size = NULL,
  //.itrs = NULL,
  //.frsh = NULL,
  //.book = {NULL},
  //.cari = {0},
  //.clen = {0},
  //.cadt = {0},
  //.fari = {0},
//};

//// Constants
//// ---------

//#define DP0 0x00
//#define DP1 0x01
//#define VAR 0x02
//#define SUB 0x03
//#define REF 0x04
//#define LET 0x05
//#define APP 0x06
//#define MAT 0x08
//#define IFL 0x09
//#define SWI 0x0A
//#define OPX 0x0B
//#define OPY 0x0C
//#define ERA 0x0D
//#define LAM 0x0E
//#define SUP 0x0F
//#define CTR 0x10
//#define W32 0x11
//#define CHR 0x12

//#define OP_ADD 0x00
//#define OP_SUB 0x01
//#define OP_MUL 0x02
//#define OP_DIV 0x03
//#define OP_MOD 0x04
//#define OP_EQ  0x05
//#define OP_NE  0x06
//#define OP_LT  0x07
//#define OP_GT  0x08
//#define OP_LTE 0x09
//#define OP_GTE 0x0A
//#define OP_AND 0x0B
//#define OP_OR  0x0C
//#define OP_XOR 0x0D
//#define OP_LSH 0x0E
//#define OP_RSH 0x0F

//#define DUP_F 0xFFFF
//#define SUP_F 0xFFFE
//#define LOG_F 0xFFFD

//#define LAZY 0x0
//#define STRI 0x1

//#define VOID 0x00000000000000

//// Heap
//// ----

//void set_len(u64 size) {
  //*HVM.size = size;
//}

//void set_itr(u64 itrs) {
  //*HVM.itrs = itrs;
//}

//u64 get_len() {
  //return *HVM.size;
//}

//u64 get_itr() {
  //return *HVM.itrs;
//}

//u64 fresh() {
  //return (*HVM.frsh)++;
//}

//// Terms
//// ------

//Term term_new(Tag tag, Lab lab, Loc loc) {
  //Term tag_enc = tag;
  //Term lab_enc = ((Term)lab) << 8;
  //Term loc_enc = ((Term)loc) << 32;
  //return tag_enc | lab_enc | loc_enc;
//}

//Tag term_tag(Term x) {
  //return x & 0x7F;
//}

//Lab term_lab(Term x) {
  //return (x >> 8) & 0xFFFFFF;
//}

//Loc term_loc(Term x) {
  //return (x >> 32) & 0xFFFFFFFF;
//}

//u64 term_get_bit(Term x) {
  //return (x >> 7) & 1;
//}

//Term term_set_bit(Term term) {
  //return term | (1ULL << 7);
//}

//Term term_rem_bit(Term term) {
  //return term & ~(1ULL << 7);
//}

//Term term_set_loc(Term x, Loc loc) {
  //return (x & 0x00000000FFFFFFFF) | (((Term)loc) << 32);
//}

//_Bool term_is_atom(Term term) {
  //switch (term_tag(term)) {
    //case ERA:
    //case W32:
    //case CHR: return 1;
    //default: return 0;
  //}
//}

//// Atomics
//// -------

//Term swap(Loc loc, Term term) {
  //Term val = HVM.heap[loc];
  //HVM.heap[loc] = term;
  //if (val == 0) {
    //printf("SWAP 0 at %08llx\n", (u64)loc);
    //exit(0);
  //}
  //return val;
//}

//Term got(Loc loc) {
  //Term val = HVM.heap[loc];
  //if (val == 0) {
    //printf("GOT 0 at %08llx\n", (u64)loc);
    //exit(0);
  //}
  //return val;
//}

//void set(Loc loc, Term term) {
  //HVM.heap[loc] = term;
//}

//void sub(Loc loc, Term term) {
  //set(loc, term_set_bit(term));
//}

//Term take(Loc loc) {
  //return swap(loc, VOID);
//}

//// Allocation
//// ----------

//Loc alloc_node(Loc arity) {
  //if (*HVM.size + arity > MAX_HEAP_SIZE) {
    //printf("Heap memory limit exceeded\n");
    //exit(1);
  //}
  //u64 old = *HVM.size;
  //*HVM.size += arity;
  //return old;
//}

//void inc_itr() {
  //(*HVM.itrs)++;
//}

//// Stack
//// ----

//void spush(Term term, Term* sbuf, u64* spos) {
  //if (*spos >= MAX_STACK_SIZE) {
    //printf("Stack memory limit exceeded\n");
    //exit(1);
  //}
  //sbuf[(*spos)++] = term;
//}

//Term spop(Term* sbuf, u64* spos) {
  //return sbuf[--(*spos)];
//}

//// Stringification
//// ---------------

//void print_tag(Tag tag) {
  //switch (tag) {
    //case VAR: printf("VAR"); break;
    //case DP0: printf("DP0"); break;
    //case DP1: printf("DP1"); break;
    //case APP: printf("APP"); break;
    //case LAM: printf("LAM"); break;
    //case ERA: printf("ERA"); break;
    //case SUP: printf("SUP"); break;
    //case REF: printf("REF"); break;
    //case LET: printf("LET"); break;
    //case CTR: printf("CTR"); break;
    //case MAT: printf("MAT"); break;
    //case IFL: printf("IFL"); break;
    //case SWI: printf("SWI"); break;
    //case W32: printf("W32"); break;
    //case CHR: printf("CHR"); break;
    //case OPX: printf("OPX"); break;
    //case OPY: printf("OPY"); break;
    //default : printf("???"); break;
  //}
//}

//void print_term(Term term) {
  //printf("term_new(");
  //print_tag(term_tag(term));
  //printf(",0x%06llx,0x%08llx)", (u64)term_lab(term), (u64)term_loc(term));
//}

//void print_heap() {
  //for (Loc i = 0; i < *HVM.size; i++) {
    //Term term = got(i);
    //if (term != 0) {
      //printf("set(0x%08llx, ", (u64)i);
      //print_term(term);
      //printf(");\n");
    //}
  //}
//}

//// Evaluation
//// ----------

//// @foo(&L{ax ay} b c ...)
//// ----------------------- REF-SUP-COPY (when @L not in @foo)
//// ! &L{bx by} = b
//// ! &L{cx cy} = b
//// ...
//// &L{@foo(ax bx cx ...) @foo(ay by cy ...)}
//Term reduce_ref_sup(Term ref, u16 idx) {
  //inc_itr();
  //Loc ref_loc = term_loc(ref);
  //Lab ref_lab = term_lab(ref);
  //u16 fun_id = ref_lab;
  //u16 arity  = HVM.fari[fun_id];
  //if (idx >= arity) {
    //printf("ERROR: Invalid index in reduce_ref_sup\n");
    //exit(1);
  //}
  //Term sup = got(ref_loc + idx);
  //if (term_tag(sup) != SUP) {
    //printf("ERROR: Expected SUP at index %u\n", idx);
    //exit(1);
  //}
  //Lab sup_lab = term_lab(sup);
  //Loc sup_loc = term_loc(sup);
  //Term sup0 = got(sup_loc + 0);
  //Term sup1 = got(sup_loc + 1);
  //// Allocate space for new REF node arguments for the second branch
  //Loc ref1_loc = alloc_node(arity);
  //for (u64 i = 0; i < arity; ++i) {
    //if (i != idx) {
      //// Duplicate argument
      //Term arg = got(ref_loc + i);
      //Loc dup_loc = alloc_node(1);
      //set(dup_loc + 0, arg);
      //set(ref_loc + i, term_new(DP0, sup_lab, dup_loc));
      //set(ref1_loc + i, term_new(DP1, sup_lab, dup_loc));
    //} else {
      //// Set the SUP components directly
      //set(ref_loc + i, sup0);
      //set(ref1_loc + i, sup1);
    //}
  //}
  //// Create new REF nodes
  //Term ref0 = term_new(REF, ref_lab, ref_loc);
  //Term ref1 = term_new(REF, ref_lab, ref1_loc);
  //// Reuse sup_loc to create the new SUP node
  //set(sup_loc + 0, ref0);
  //set(sup_loc + 1, ref1);
  //return term_new(SUP, sup_lab, sup_loc);
//}

//// @foo(a b c ...)
//// -------------------- REF
//// book[foo](a b c ...)
//Term reduce_ref(Term ref) {
  ////printf("reduce_ref "); print_term(ref); printf("\n");
  ////printf("call %d %p\n", term_loc(ref), HVM.book[term_loc(ref)]);
  //inc_itr();
  //return HVM.book[term_lab(ref)](ref);
//}

//// ! x = val
//// bod
//// --------- LET
//// x <- val
//// bod
//Term reduce_let(Term let, Term val) {
  ////printf("reduce_let "); print_term(let); printf("\n");
  //inc_itr();
  //Loc let_loc = term_loc(let);
  //Term bod    = got(let_loc + 1);
  //sub(let_loc + 0, val);
  //return bod;
//}

//// (* a)
//// ------- APP-ERA
//// *
//Term reduce_app_era(Term app, Term era) {
  ////printf("reduce_app_era "); print_term(app); printf("\n");
  //inc_itr();
  //return era;
//}

//// (λx(body) arg)
//// ---------------- APP-LAM
//// x <- arg
//// body
//Term reduce_app_lam(Term app, Term lam) {
  //inc_itr();
  //Loc app_loc = term_loc(app);
  //Loc lam_loc = term_loc(lam);
  //Term bod    = got(lam_loc + 0);
  //Term arg    = got(app_loc + 1);
  //sub(lam_loc + 0, arg);
  //return bod;
//}

//// (&L{a b} c)
//// --------------------- APP-SUP
//// ! &L{x0 x1} = c
//// &L{(a x0) (b x1)}
//Term reduce_app_sup(Term app, Term sup) {
  ////printf("reduce_app_sup "); print_term(app); printf("\n");
  //inc_itr();
  //Loc app_loc = term_loc(app);
  //Loc sup_loc = term_loc(sup);
  //Lab sup_lab = term_lab(sup);

  //Term arg    = got(app_loc + 1);
  ////Term tm0    = got(sup_loc + 0);
  //Term tm1    = got(sup_loc + 1);

  //Loc loc = alloc_node(3);
  //Loc ap0 = sup_loc;
  //Loc ap1 = loc + 0;
  //Loc su0 = app_loc;
  //Loc dup = loc + 2;

  ////set(ap0 + 0, tm0)
  //set(ap0 + 1, term_new(DP0, sup_lab, dup));

  //set(ap1 + 0, tm1);
  //set(ap1 + 1, term_new(DP1, sup_lab, dup));

  //// Reuse app_loc for the result superposition
  //set(su0 + 0, term_new(APP, 0, ap0));
  //set(su0 + 1, term_new(APP, 0, ap1));

  //set(dup + 0, arg);

  //return term_new(SUP, sup_lab, su0);
  //// return term_set_loc(sup, su0);
//}

//// &L(#{x y z ...} a)
//// ------------------ APP-CTR
//// ⊥
//Term reduce_app_ctr(Term app, Term ctr) {
  ////printf("reduce_app_ctr "); print_term(app); printf("\n");
  //printf("invalid:app-ctr");
  //exit(0);
//}

//// &L(123 a)
//// --------- APP-W32
//// ⊥
//Term reduce_app_u32(Term app, Term u32) {
  ////printf("reduce_app_u32 "); print_term(app); printf("\n");
  //printf("invalid:app-u32");
  //exit(0);
//}

//// ! &L{x y} = *
//// ------------- DUP-ERA
//// x <- *
//// y <- *
//Term reduce_dup_era(Term dup, Term era) {
  ////printf("reduce_dup_era "); print_term(dup); printf("\n");
  //inc_itr();
  //Loc dup_loc = term_loc(dup);
  //sub(dup_loc + 0, era);
  //return era;
//}

//// ! &L{r s} = λx(f)
//// ------------------- DUP-LAM
//// ! &L{f0 f1} = f
//// r <- λx0(f0)
//// s <- λx1(f1)
//// x <- &L{x0 x1}
//Term reduce_dup_lam(Term dup, Term lam) {
  ////printf("reduce_dup_lam "); print_term(dup); printf("\n");
  //inc_itr();
  //Loc dup_loc = term_loc(dup);
  //Loc lam_loc = term_loc(lam);
  //Lab dup_lab = term_lab(dup);

  //Term bod    = got(lam_loc + 0);
  
  //Loc loc     = alloc_node(5);
  //Loc lm0     = loc + 0;
  //Loc lm1     = loc + 1;
  //Loc su0     = loc + 2;
  //Loc du0     = loc + 4;

  //sub(lam_loc + 0, term_new(SUP, dup_lab, su0));

  //set(lm0 + 0, term_new(DP0, dup_lab, du0));
  //set(lm1 + 0, term_new(DP1, dup_lab, du0));
  //set(su0 + 0, term_new(VAR, 0, lm0));
  //set(su0 + 1, term_new(VAR, 0, lm1));
  //set(du0 + 0, bod);

  //if (term_tag(dup) == DP0) {
    //sub(dup_loc + 0, term_new(LAM, 0, lm1));
    //return term_new(LAM, 0, lm0);
  //} else {
    //sub(dup_loc + 0, term_new(LAM, 0, lm0));
    //return term_new(LAM, 0, lm1);
  //}
//}

//// ! &L{x y} = &R{a b}
//// ------------------- DUP-SUP
//// if L == R:
////   x <- a
////   y <- b
//// else:
////   x <- &R{a0 b0} 
////   y <- &R{a1 b1}
////   ! &L{a0 a1} = a
////   ! &L{b0 b1} = b
//Term reduce_dup_sup(Term dup, Term sup) {
  ////printf("reduce_dup_sup %u %u | %llu ", term_lab(dup), term_lab(sup), *HVM.spos); print_term(dup); printf(" "); print_term(sup); printf("\n");
  //inc_itr();
  //Loc dup_loc = term_loc(dup);
  //Lab dup_lab = term_lab(dup);
  //Lab sup_lab = term_lab(sup);
  //Loc sup_loc = term_loc(sup);
  //if (dup_lab == sup_lab) {
    //Term tm0 = got(sup_loc + 0);
    //Term tm1 = got(sup_loc + 1);
    //if (term_tag(dup) == DP0) {
      //sub(dup_loc + 0, tm1);
      //return tm0;
    //} else {
      //sub(dup_loc + 0, tm0);
      //return tm1;
    //}
  //} else {
    //Loc loc = alloc_node(4);
    //Loc du0 = sup_loc + 0;
    //Loc du1 = sup_loc + 1;
    //Loc su0 = loc + 0;
    //Loc su1 = loc + 2;
    ////Term tm0 = got(sup_loc + 0);
    ////Term tm1 = got(sup_loc + 1);
    ////set(du0 + 0, tm0);
    ////set(du1 + 0, tm1);
    //set(su0 + 0, term_new(DP0, dup_lab, du0));
    //set(su0 + 1, term_new(DP0, dup_lab, du1));
    //set(su1 + 0, term_new(DP1, dup_lab, du0));
    //set(su1 + 1, term_new(DP1, dup_lab, du1));
    //if (term_tag(dup) == DP0) {
      //sub(dup_loc + 0, term_new(SUP, sup_lab, su1));
      //return term_new(SUP, sup_lab, su0);
    //} else {
      //sub(dup_loc + 0, term_new(SUP, sup_lab, su0));
      //return term_new(SUP, sup_lab, su1);
    //}
  //}
//}

//// ! &L{x y} = #{a b c ...}
//// ------------------------ DUP-CTR
//// ! &L{a0 a1} = a
//// ! &L{b0 b1} = b
//// ! &L{c0 c1} = c
//// ...
//// x <- #{a0 b0 c0 ...} 
//// y <- #{a1 b1 c1 ...}
//Term reduce_dup_ctr(Term dup, Term ctr) {
  ////printf("reduce_dup_ctr "); print_term(dup); printf("\n");
  //inc_itr();
  //Loc dup_loc = term_loc(dup);
  //Lab dup_lab = term_lab(dup);
  //Loc ctr_loc = term_loc(ctr);
  //Lab ctr_lab = term_lab(ctr);
  //u64 ctr_ari = HVM.cari[ctr_lab];

  //Loc loc     = alloc_node(ctr_ari * 2);
  ////Loc ctr0    = alloc_node(ctr_ari);
  //Loc ctr0    = ctr_loc;
  //Loc ctr1    = loc + 0;
  //for (u64 i = 0; i < ctr_ari; i++) {
    //Loc du0 = loc + ctr_ari + i;
    //set(du0 + 0, got(ctr_loc + i));
    //set(ctr0 + i, term_new(DP0, dup_lab, du0));
    //set(ctr1 + i, term_new(DP1, dup_lab, du0));
  //}
  //if (term_tag(dup) == DP0) {
    //sub(dup_loc + 0, term_new(CTR, ctr_lab, ctr1));
    //return term_new(CTR, ctr_lab, ctr0);
  //} else {
    //sub(dup_loc + 0, term_new(CTR, ctr_lab, ctr0));
    //return term_new(CTR, ctr_lab, ctr1);
  //}
//}

//// ! &L{x y} = 123
//// --------------- DUP-W32
//// x <- 123
//// y <- 123
//Term reduce_dup_u32(Term dup, Term u32) {
  ////printf("reduce_dup_u32 "); print_term(dup); printf("\n");
  //inc_itr();
  //Loc dup_loc = term_loc(dup);
  //sub(dup_loc + 0, u32);
  //return u32;
//}

//// ! &L{x y} = @foo(a b c ...)
//// --------------------------- DUP-REF-COPY (when &L not in @foo)
//// ! &L{a0 a1} = a
//// ! &L{b0 b1} = b
//// ! &L{c0 c1} = c
//// ...
//// x <- @foo(a0 b0 c0 ...)
//// y <- @foo(a1 b1 c1 ...)
//Term reduce_dup_ref(Term dup, Term ref) {
  ////printf("reduce_dup_ref "); print_term(dup); printf("\n");
  //inc_itr();
  //Loc dup_loc = term_loc(dup);
  //Lab dup_lab = term_lab(dup);
  //Loc ref_loc = term_loc(ref);
  //Lab ref_lab = term_lab(ref);
  //u64 ref_ari = HVM.fari[ref_lab];

  //Loc loc     = alloc_node(ref_ari * 2);
  //Loc ref0    = ref_loc;
  //Loc ref1    = loc + 0;
  //for (u64 i = 0; i < ref_ari; i++) {
    //Loc du0 = loc + ref_ari + i;
    //set(du0 + 0, got(ref_loc + i));
    //set(ref0 + i, term_new(DP0, dup_lab, du0));
    //set(ref1 + i, term_new(DP1, dup_lab, du0));
  //}
  //if (term_tag(dup) == DP0) {
    //sub(dup_loc + 0, term_new(REF, ref_lab, ref1));
    //return term_new(REF, ref_lab, ref0);
  //} else {
    //sub(dup_loc + 0, term_new(REF, ref_lab, ref0));
    //return term_new(REF, ref_lab, ref1);
  //}
//}

//// ~ * {K0 K1 K2 ...} 
//// ------------------ MAT-ERA
//// *
//Term reduce_mat_era(Term mat, Term era) {
  ////printf("reduce_mat_era "); print_term(mat); printf("\n");
  //inc_itr();
  //return era;
//}

//// ~ λx(x) {K0 K1 K2 ...}
//// ------------------------ MAT-LAM
//// ⊥
//Term reduce_mat_lam(Term mat, Term lam) {
  ////printf("reduce_mat_lam "); print_term(mat); printf("\n");
  //printf("invalid:mat-lam");
  //exit(0);
//}

//// ~ &L{x y} {K0 K1 K2 ...}
//// ------------------------ MAT-SUP
//// ! &L{k0a k0b} = K0
//// ! &L{k1a k1b} = K1
//// ! &L{k2a k2b} = K2
//// ...
//// &L{ ~ x {K0a K1a K2a ...}
////     ~ y {K0b K1b K2b ...} }
//Term reduce_mat_sup(Term mat, Term sup) {
  ////printf("reduce_mat_sup "); print_term(mat); printf("\n");
  //inc_itr();
  //Tag mat_tag = term_tag(mat);
  //Lab mat_lab = term_lab(mat);
  //Loc mat_loc = term_loc(mat);
  //Loc sup_loc = term_loc(sup);
  //Lab sup_lab = term_lab(sup);

  //Term tm0    = got(sup_loc + 0);
  //Term tm1    = got(sup_loc + 1);
  //u64 mat_len = mat_tag == SWI ? mat_lab : mat_tag == IFL ? 2 : HVM.clen[mat_lab];

  //Loc loc     = alloc_node(1 + mat_len + mat_len);
  //Loc mat0    = mat_loc;
  //Loc mat1    = loc + 0;
  //Loc sup0    = sup_loc;

  //set(mat0 + 0, tm0);
  //set(mat1 + 0, tm1);
  //for (u64 i = 0; i < mat_len; i++) {
    //Loc du0 = loc + 1 + mat_len + i;
    //set(du0 + 0, got(mat_loc + 1 + i));
    //set(mat0 + 1 + i, term_new(DP0, sup_lab, du0));
    //set(mat1 + 1 + i, term_new(DP1, sup_lab, du0));
  //}
  //set(sup0 + 0, term_new(mat_tag, mat_lab, mat0));
  //set(sup0 + 1, term_new(mat_tag, mat_lab, mat1));
  //return term_new(SUP, sup_lab, sup0);
//}

//Term reduce_mat_ctr(Term mat, Term ctr) {
  //inc_itr();
  //Tag mat_tag = term_tag(mat);
  //Loc mat_loc = term_loc(mat);
  //Lab mat_lab = term_lab(mat);
  //// If-Let
  //if (mat_tag == IFL) {
    //Loc ctr_loc = term_loc(ctr);
    //Lab ctr_lab = term_lab(ctr);
    //u64 mat_ctr = mat_lab;
    //u64 ctr_num = ctr_lab;
    //u64 ctr_ari = HVM.cari[ctr_num];
    //if (mat_ctr == ctr_num) {
      //Term app = got(mat_loc + 1);
      //Loc loc = alloc_node(ctr_ari * 2);
      //for (u64 i = 0; i < ctr_ari; i++) {
        //Loc new_app = loc + i * 2;
        //set(new_app + 0, app);
        //set(new_app + 1, got(ctr_loc + i));
        //app = term_new(APP, 0, new_app);
      //}
      //return app;
    //} else {
      //Term app = got(mat_loc + 2);
      //Loc new_app = mat_loc;
      //set(new_app + 0, app);
      //set(new_app + 1, ctr);
      //app = term_new(APP, 0, new_app);
      //return app;
    //}
  //// Match
  //} else {
    //Loc ctr_loc = term_loc(ctr);
    //Lab ctr_lab = term_lab(ctr);
    //u64 ctr_num = ctr_lab;
    //u64 ctr_ari = HVM.cari[ctr_num];
    //u64 mat_ctr = mat_lab;
    //u64 cse_idx = ctr_num - mat_ctr;
    //Term app = got(mat_loc + 1 + cse_idx);
    //Loc loc = alloc_node(ctr_ari * 2);
    //for (u64 i = 0; i < ctr_ari; i++) {
      //Loc new_app = loc + i * 2;
      //set(new_app + 0, app);
      //set(new_app + 1, got(ctr_loc + i));
      //app = term_new(APP, 0, new_app);
    //}
    //return app;
  //}
//}

//// ~ num {K0 K1 K2 ... KN}
//// ----------------------- MAT-W32
//// if n < N: Kn
//// else    : KN(num-N)
//Term reduce_mat_u32(Term mat, Term u32) {
  ////printf("reduce_mat_u32 "); print_term(mat); printf("\n");
  //inc_itr();
  //Loc mat_loc = term_loc(mat);
  //Lab mat_lab = term_lab(mat);
  //u64 mat_len = mat_lab;
  //u64 u32_val = term_loc(u32);
  //if (u32_val < mat_len - 1) {
    //return got(mat_loc + 1 + u32_val);
  //} else {
    //Term fn = got(mat_loc + mat_len);
    //Loc app = mat_loc;
    //set(app + 0, fn);
    //set(app + 1, term_new(u32, 0, u32_val - (mat_len - 1)));
    //return term_new(APP, 0, app);
  //}
//}

//// <op(* b)
//// -------- OPX-ERA
//// *
//Term reduce_opx_era(Term opx, Term era) {
  ////printf("reduce_opx_era "); print_term(opx); printf("\n");
  //inc_itr();
  //return era;
//}

//// <op(λx(B) y)
//// --------------- OPX-LAM
//// ⊥
//Term reduce_opx_lam(Term opx, Term lam) {
  ////printf("reduce_opx_lam "); print_term(opx); printf("\n");
  //printf("invalid:opx-lam");
  //exit(0);
//}

//// <op(&L{x0 x1} y)
//// ------------------------- OPX-SUP
//// ! &L{y0 y1} = y
//// &L{<op(x0 y0) <op(x1 y1)}
//Term reduce_opx_sup(Term opx, Term sup) {
  ////printf("reduce_opx_sup "); print_term(opx); printf("\n");
  //inc_itr();
  //Loc opx_loc = term_loc(opx);
  //Loc sup_loc = term_loc(sup);
  //Lab sup_lab = term_lab(sup);
  //Term nmy    = got(opx_loc + 1);
  //Term tm0    = got(sup_loc + 0);
  //Term tm1    = got(sup_loc + 1);
  //Loc loc     = alloc_node(3);
  //Loc op0     = opx_loc;
  //Loc op1     = sup_loc;
  //Loc su0     = loc + 0;
  //Loc du0     = loc + 2;
  //set(op0 + 0, tm0);
  //set(op0 + 1, term_new(DP0, sup_lab, du0));
  //set(op1 + 0, tm1);
  //set(op1 + 1, term_new(DP1, sup_lab, du0));
  //set(su0 + 0, term_new(OPX, term_lab(opx), op0));
  //set(su0 + 1, term_new(OPX, term_lab(opx), op1));
  //set(du0 + 0, nmy);
  //return term_new(SUP, sup_lab, su0);
//}

//// <op(#{x0 x1 x2...} y)
//// --------------------- OPX-CTR
//// ⊥
//Term reduce_opx_ctr(Term opx, Term ctr) {
  ////printf("reduce_opx_ctr "); print_term(opx); printf("\n");
  //printf("invalid:opx-ctr");
  //exit(0);
//}

//// <op(x0 x1)
//// ---------- OPX-W32
//// >op(x0 x1)
//Term reduce_opx_u32(Term opx, Term nmx) {
  ////printf("reduce_opx_u32 "); print_term(opx); printf("\n");
  //inc_itr();
  //Lab opx_lab = term_lab(opx);
  //Loc opx_loc = term_loc(opx);
  //Term nmy = got(opx_loc + 1);
  //set(opx_loc + 0, nmy);
  //set(opx_loc + 1, nmx);
  //return term_new(OPY, opx_lab, opx_loc);
//}

//// >op(a *)
//// -------- OPY-ERA
//// *
//Term reduce_opy_era(Term opy, Term era) {
  ////printf("reduce_opy_era "); print_term(opy); printf("\n");
  //inc_itr();
  //return era;
//}

//// >op(a λx(B))
//// ------------ OPY-LAM
//// *
//Term reduce_opy_lam(Term opy, Term era) {
  ////printf("reduce_opy_lam "); print_term(opy); printf("\n");
  //printf("invalid:opy-lam");
  //exit(0);
//}

//// >op(a &L{x y})
//// --------------------- OPY-SUP
//// &L{>op(a x) >op(a y)}
//Term reduce_opy_sup(Term opy, Term sup) {
  ////printf("reduce_opy_sup "); print_term(opy); printf("\n");
  //inc_itr();
  //Loc opy_loc = term_loc(opy);
  //Loc sup_loc = term_loc(sup);
  //Lab sup_lab = term_lab(sup);
  //Term nmx    = got(opy_loc + 1);
  //Term tm0    = got(sup_loc + 0);
  //Term tm1    = got(sup_loc + 1);
  //Loc op0     = sup_loc;
  //Loc op1     = opy_loc;
  //Loc su0     = alloc_node(2);
  ////set(op0 + 0, tm0);
  //set(op0 + 1, nmx);
  //set(op1 + 0, tm1);
  ////set(op1 + 1, nmx);
  //set(su0 + 0, term_new(OPY, term_lab(opy), op0));
  //set(su0 + 1, term_new(OPY, term_lab(opy), op1));
  //return term_new(SUP, sup_lab, su0);
//}

//// >op(#{x y z ...} b)
//// ---------------------- OPY-CTR
//// ⊥
//Term reduce_opy_ctr(Term opy, Term ctr) {
  ////printf("reduce_opy_ctr "); print_term(opy); printf("\n");
  //printf("invalid:opy-ctr");
  //exit(0);
//}

//// >op(x y)
//// --------- OPY-W32
//// x op y
//Term reduce_opy_u32(Term opy, Term val) {
  ////printf("reduce_opy_u32 "); print_term(opy); printf("\n");
  //inc_itr();
  //Loc opy_loc = term_loc(opy);
  //Tag t = term_tag(val);
  //u32 x = term_loc(got(opy_loc + 1));
  //u32 y = term_loc(val);
  //u32 result;
  //switch (term_lab(opy)) {
    //case OP_ADD: result = x + y; break;
    //case OP_SUB: result = x - y; break;
    //case OP_MUL: result = x * y; break;
    //case OP_DIV: result = x / y; break;
    //case OP_MOD: result = x % y; break;
    //case OP_EQ:  result = x == y; break;
    //case OP_NE:  result = x != y; break;
    //case OP_LT:  result = x < y; break;
    //case OP_GT:  result = x > y; break;
    //case OP_LTE: result = x <= y; break;
    //case OP_GTE: result = x >= y; break;
    //case OP_AND: result = x & y; break;
    //case OP_OR:  result = x | y; break;
    //case OP_XOR: result = x ^ y; break;
    //case OP_LSH: result = x << y; break;
    //case OP_RSH: result = x >> y; break;
    //default: {
      //printf("invalid:opy-u32");
      //exit(0);
    //}
  //}
  //return term_new(t, 0, result);
//}

//Term reduce(Term term) {
  //if (term_tag(term) >= ERA) return term;
  //Term  next = term;
  //u64   stop = *HVM.spos;
  //u64   spos = stop;
  //Term* sbuf = HVM.sbuf;

  //while (1) {
    ////printf("NEXT "); print_term(term); printf("\n");
    ////printf("PATH ");
    ////for (u64 i = 0; i < *spos; ++i) {
      ////print_tag(term_tag(HVM.sbuf[i]));
      ////printf(" ");
    ////}
    ////printf(" ~ %p", HVM.sbuf);
    ////printf("\n");
    //Tag tag = term_tag(next);
    //Lab lab = term_lab(next);
    //Loc loc = term_loc(next);


    //// On variables: substitute
    //// On eliminators: move to field
    //switch (tag) {

      //case LET: {
        //switch (lab) {
          //case LAZY: {
            //next = reduce_let(next, got(loc + 0));
            //continue;
          //}
          //case STRI: {
            //spush(next, sbuf, &spos);
            //next = got(loc + 0);
            //continue;
          //}
          //default: {
            //printf("invalid:let");
            //exit(0);
          //}
        //}
      //}

      //case APP:
      //case MAT:
      //case IFL:
      //case SWI:
      //case OPX:
      //case OPY: {
        //spush(next, sbuf, &spos);
        //next = got(loc + 0);
        //continue;
      //}

      //case DP0:
      //case DP1: {
        //Term sub = got(loc + 0);
        //if (term_get_bit(sub) == 0) {
          //spush(next, sbuf, &spos);
          //next = sub;
          //continue;
        //} else {
          //next = term_rem_bit(sub);
          //continue;
        //}
      //}

      //case VAR: {
        //Term sub = got(loc);
        //if (term_get_bit(sub) == 0) {
          //break;
        //} else {
          //next = term_rem_bit(sub);
          //continue;
        //}
      //}

      //case REF: {
        //*HVM.spos = spos;
        //next = reduce_ref(next);
        //spos = *HVM.spos;
        //continue;
      //}

      //default: break;
    //}

    //// Empty stack: term is in WHNF
    //if (spos == stop) {
      //*HVM.spos = spos;
      //return next;
    //}

    //// Interaction Dispatcher
    //Term prev = spop(sbuf, &spos);
    //Tag  ptag = term_tag(prev);
    //Lab  plab = term_lab(prev);
    //Loc  ploc = term_loc(prev);
    //switch (ptag) {

      //case LET: {
        //next = reduce_let(prev, next);
        //continue;
      //}

      //case APP: {
        //switch (tag) {
          //case ERA: next = reduce_app_era(prev, next); continue;
          //case LAM: next = reduce_app_lam(prev, next); continue;
          //case SUP: next = reduce_app_sup(prev, next); continue;
          //case CTR: next = reduce_app_ctr(prev, next); continue;
          //case W32: next = reduce_app_u32(prev, next); continue;
          //case CHR: next = reduce_app_u32(prev, next); continue;
          //default: break;
        //}
      //}

      //case DP0:
      //case DP1: {
        //switch (tag) {
          //case ERA: next = reduce_dup_era(prev, next); continue;
          //case LAM: next = reduce_dup_lam(prev, next); continue;
          //case SUP: next = reduce_dup_sup(prev, next); continue;
          //case CTR: next = reduce_dup_ctr(prev, next); continue;
          //case W32: next = reduce_dup_u32(prev, next); continue;
          //case CHR: next = reduce_dup_u32(prev, next); continue;
          //default: break;
        //}
      //}

      //case MAT:
      //case IFL:
      //case SWI: {
        //switch (tag) {
          //case ERA: next = reduce_mat_era(prev, next); continue;
          //case LAM: next = reduce_mat_lam(prev, next); continue;
          //case SUP: next = reduce_mat_sup(prev, next); continue;
          //case CTR: next = reduce_mat_ctr(prev, next); continue;
          //case W32: next = reduce_mat_u32(prev, next); continue;
          //case CHR: next = reduce_mat_u32(prev, next); continue;
          //default: break;
        //}
      //}

      //case OPX: {
        //switch (tag) {
          //case ERA: next = reduce_opx_era(prev, next); continue;
          //case LAM: next = reduce_opx_lam(prev, next); continue;
          //case SUP: next = reduce_opx_sup(prev, next); continue;
          //case CTR: next = reduce_opx_ctr(prev, next); continue;
          //case W32: next = reduce_opx_u32(prev, next); continue;
          //case CHR: next = reduce_opx_u32(prev, next); continue;
          //default: break;
        //}
      //}

      //case OPY: {
        //switch (tag) {
          //case ERA: next = reduce_opy_era(prev, next); continue;
          //case LAM: next = reduce_opy_lam(prev, next); continue;
          //case SUP: next = reduce_opy_sup(prev, next); continue;
          //case CTR: next = reduce_opy_ctr(prev, next); continue;
          //case W32: next = reduce_opy_u32(prev, next); continue;
          //case CHR: next = reduce_opy_u32(prev, next); continue;
          //default: break;
        //}
      //}

      //default: break;
    //}

    //// No interaction: push term back to stack
    //spush(prev, sbuf, &spos);

    //// Update parent chain
    //while (spos > stop) {
      //Term host = spop(sbuf, &spos);
      //Tag  htag = term_tag(host);
      //Lab  hlab = term_lab(host);
      //Loc  hloc = term_loc(host);
      //set(hloc + 0, next);
      //next = host;
    //}
    //*HVM.spos = spos;
    //return next;
  //}
//}

//Term reduce_at(Loc host) {
  //Term tm0 = got(host);
  //if (term_tag(tm0) >= ERA) {
    //return tm0;
  //}
  //Term tm1 = reduce(tm0);
  //set(host, tm1);
  //return tm1;
//}

//Term normal(Term term) {
  //Term wnf = reduce(term);
  //Tag tag = term_tag(wnf);
  //Lab lab = term_lab(wnf);
  //Loc loc = term_loc(wnf);
  //switch (tag) {

    //case LAM: {
      //Term bod = got(loc + 0);
      //bod = normal(bod);
      //set(term_loc(wnf) + 1, bod);
      //return wnf;
    //}

    //case APP: {
      //Term fun = got(loc + 0);
      //Term arg = got(loc + 1);
      //fun = normal(fun);
      //arg = normal(arg);
      //set(term_loc(wnf) + 0, fun);
      //set(term_loc(wnf) + 1, arg);
      //return wnf;
    //}

    //case SUP: {
      //Term tm0 = got(loc + 0);
      //Term tm1 = got(loc + 1);
      //tm0 = normal(tm0);
      //tm1 = normal(tm1);
      //set(term_loc(wnf) + 0, tm0);
      //set(term_loc(wnf) + 1, tm1);
      //return wnf;
    //}

    //case DP0:
    //case DP1: {
      //Term val = got(loc + 0);
      //val = normal(val);
      //set(term_loc(wnf) + 0, val);
      //return wnf;
    //}

    //case CTR: {
      //u64 cid = lab;
      //u64 ari = HVM.cari[cid];
      //for (u64 i = 0; i < ari; i++) {
        //Term arg = got(loc + i);
        //arg = normal(arg);
        //set(term_loc(wnf) + i, arg);
      //}
      //return wnf;
    //}

    //case MAT:
    //case IFL:
    //case SWI: {
      //u64 len = tag == SWI ? lab : tag == IFL ? 2 : HVM.clen[lab];
      //for (u64 i = 0; i <= len; i++) {
        //Term arg = got(loc + i);
        //arg = normal(arg);
        //set(term_loc(wnf) + i, arg);
      //}
      //return wnf;
    //}

    //default:
      //return wnf;

  //}
//}

//// Primitives
//// ----------

//// Primitive: Dynamic Sup `@SUP(lab tm0 tm1)`
//// Allocates a new SUP node with given label.
//Term SUP_f(Term ref) {
  //Loc ref_loc = term_loc(ref);
  //Term lab = reduce(got(ref_loc + 0));
  //Term lab_val = term_loc(lab);
  //if (term_tag(lab) != W32) {
    //printf("ERROR:non-numeric-sup-label\n");
  //}
  //if (lab_val > 0xFFFF) {
    //printf("ERROR:sup-label-too-large\n");
  //}
  //Term tm0 = got(ref_loc + 1);
  //Term tm1 = got(ref_loc + 2);
  //Loc  sup = alloc_node(2);
  //Term ret = term_new(SUP, lab_val, sup);
  //set(sup + 0, tm0);
  //set(sup + 1, tm1);
  //*HVM.itrs += 1;
  //return ret;
//}

//// Primitive: Dynamic Dup `@DUP(lab val λdp0λdp1(bod))`
//// Creates a DUP node with given label.
//Term DUP_f(Term ref) {
  //Loc ref_loc = term_loc(ref);
  //Term lab = reduce(got(ref_loc + 0));
  //Term lab_val = term_loc(lab);
  //if (term_tag(lab) != W32) {
    //printf("ERROR:non-numeric-dup-label\n");
  //}
  //if (lab_val > 0xFFFF) {
    //printf("ERROR:dup-label-too-large\n");
  //}
  //Term val = got(ref_loc + 1);
  //Term bod = got(ref_loc + 2);
  //Loc  dup = alloc_node(1);
  //set(dup + 0, val);
  //if (term_tag(bod) == LAM) {
    //Loc  lam0 = term_loc(bod);
    //Term bod0 = got(lam0 + 0);
    //if (term_tag(bod0) == LAM) {
      //Loc  lam1 = term_loc(bod0);
      //Term bod1 = got(lam1 + 0);
      //sub(lam0 + 0, term_new(DP0, lab_val, dup));
      //sub(lam1 + 0, term_new(DP1, lab_val, dup));
      //*HVM.itrs += 3;
      //return bod1;
    //}
  //}
  //Loc app0 = alloc_node(2);
  //set(app0 + 0, bod);
  //set(app0 + 1, term_new(DP0, lab_val, dup));
  //Loc app1 = alloc_node(2);
  //set(app1 + 0, term_new(APP, 0, app0));
  //set(app1 + 1, term_new(DP1, lab_val, dup));
  //*HVM.itrs += 1;
  //return term_new(APP, 0, app1);
//}

//Term LOG_f(Term ref) {
  //printf("TODO: LOG_f");
  //exit(0);
//}

//void *alloc_huge(size_t size) {
    //void *ptr = mmap(NULL, size, PROT_READ | PROT_WRITE,
                     //MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE,
                     //-1, 0);
    //if (ptr == MAP_FAILED) {
        //perror("mmap failed");
        //return NULL;
    //}
    //return ptr;
//}

//// Runtime Memory
//// --------------

//void hvm_init() {
  //HVM.sbuf = alloc_huge(MAX_STACK_SIZE * sizeof(Term)); 
  //HVM.heap = alloc_huge(MAX_HEAP_SIZE  * sizeof(Term));
  //HVM.spos = alloc_huge(sizeof(u64));
  //HVM.size = alloc_huge(sizeof(u64));
  //HVM.itrs = alloc_huge(sizeof(u64));
  //HVM.frsh = alloc_huge(sizeof(u64));

  //#define CHECK_ALLOC(ptr, name) if (!(ptr)) { printf(name " alloc failed\n"); allocs_failed++; }
  //int allocs_failed = 0; // Track if any allocation failed

  //CHECK_ALLOC(HVM.sbuf, "sbuf");
  //CHECK_ALLOC(HVM.heap, "heap");
  //CHECK_ALLOC(HVM.spos, "spos");
  //CHECK_ALLOC(HVM.size, "size");
  //CHECK_ALLOC(HVM.itrs, "itrs");
  //CHECK_ALLOC(HVM.frsh, "frsh");

  //if (allocs_failed > 0) {
    //printf("hvm_init alloc's failed: %d allocations failed\n", allocs_failed);
    //exit(1);
  //}
  //#undef CHECK_ALLOC

  //*HVM.spos = 0;
  //*HVM.size = 1;
  //*HVM.itrs = 0;
  //*HVM.frsh = 0x20;
  //HVM.book[SUP_F] = SUP_f;
  //HVM.book[DUP_F] = DUP_f;
  //HVM.book[LOG_F] = LOG_f;
  //for (int i = 0; i < 65536; i++) {
    //HVM.cari[i] = 0;
    //HVM.clen[i] = 0;
    //HVM.cadt[i] = 0;
    //HVM.fari[i] = 0;
  //}
//}

//void hvm_munmap(void *ptr, size_t size, const char *name) {
    //if (ptr != MAP_FAILED) {
        //if (munmap(ptr, size) == -1) {
            //perror("munmap failed");
        //} else {
            //// printf("Successfully unmapped %s\n", name);
        //}
    //} else {
        //printf("%s is already null or invalid.\n", name);
    //}
//}

//void hvm_free() {
    //hvm_munmap(HVM.sbuf, MAX_STACK_SIZE * sizeof(Term), "sbuf");
    //hvm_munmap(HVM.heap, MAX_HEAP_SIZE  * sizeof(Term), "heap");
    //hvm_munmap(HVM.spos, sizeof(u64), "spos");
    //hvm_munmap(HVM.size, sizeof(u64), "size");
    //hvm_munmap(HVM.itrs, sizeof(u64), "itrs");
    //hvm_munmap(HVM.frsh, sizeof(u64), "frsh");
//}


//State* hvm_get_state() {
  //return &HVM;
//}

//void hvm_set_state(State* hvm) {
  //HVM.sbuf = hvm->sbuf;
  //HVM.spos = hvm->spos;
  //HVM.heap = hvm->heap;
  //HVM.size = hvm->size;
  //HVM.itrs = hvm->itrs;
  //HVM.frsh = hvm->frsh;
  //for (int i = 0; i < 65536; i++) {
    //HVM.book[i] = hvm->book[i];
    //HVM.fari[i] = hvm->fari[i];
    //HVM.cari[i] = hvm->cari[i];
    //HVM.clen[i] = hvm->clen[i];
    //HVM.cadt[i] = hvm->cadt[i];
  //}
//}

//void hvm_define(u16 fid, Term (*func)()) {
  ////printf("defined %llu %p\n", fid, func);
  //HVM.book[fid] = func;
//}

//void hvm_set_cari(u16 cid, u16 arity) {
  //HVM.cari[cid] = arity;
//}

//void hvm_set_fari(u16 fid, u16 arity) {
  //HVM.fari[fid] = arity;
//}

//void hvm_set_clen(u16 cid, u16 cases) {
  //HVM.clen[cid] = cases;
//}
//void hvm_set_cadt(u16 cid, u16 adt) {
  //HVM.cadt[cid] = adt;
//}

// GOAL: our goal now is to fully rewrite Runtime.c taking in account the changes made to the Core type
// do it below.

// COMPLETE, UPDATED RUNTIME.C FILE:

//#include <inttypes.h>
//#include <stdint.h>
//#include <stdio.h>
//#include <stdlib.h>
//#include <sys/mman.h>
//#include <unistd.h>
//#include <time.h>
//#include <stdbool.h>

//#define MAX_HEAP_SIZE (1ULL << 32)
//#define MAX_STACK_SIZE (1ULL << 28)

//typedef uint8_t  Tag;
//typedef uint32_t Lab;
//typedef uint32_t Loc;
//typedef uint64_t Term;
//typedef uint16_t u16;
//typedef uint32_t u32;
//typedef uint64_t u64;

//// Runtime Types
//// -------------

//typedef struct {
  //Term*  sbuf; // reduction stack buffer
  //u64*   spos; // reduction stack position
  //Term*  heap; // global node buffer
  //u64*   size; // global node buffer position
  //u64*   itrs; // interaction count
  //u64*   frsh; // fresh dup label count
  //Term (*book[65536])(Term); // functions
  //u16    fari[65536]; // arity of each function
//} State;

//static State HVM = {
  //.sbuf = NULL,
  //.spos = NULL,
  //.heap = NULL,
  //.size = NULL,
  //.itrs = NULL,
  //.frsh = NULL,
  //.book = {NULL},
  //.fari = {0},
//};

//// Constants
//// ---------

//#define DP0 0x00
//#define DP1 0x01
//#define VAR 0x02
//#define REF 0x03
//#define LET 0x04
//#define ERA 0x05
//#define SUP 0x06
//#define DUP 0x07
//#define SET 0x08
//#define EMP 0x09
//#define EFQ 0x0A
//#define UNI 0x0B
//#define NIL 0x0C
//#define USE 0x0D
//#define U32 0x0E
//#define W32 0x0F
//#define SWI 0x10
//#define OPX 0x11
//#define OPY 0x12
//#define SIG 0x13
//#define TUP 0x14
//#define GET 0x15
//#define ALL 0x16
//#define LAM 0x17
//#define APP 0x18

//#define OP_ADD 0x00
//#define OP_SUB 0x01
//#define OP_MUL 0x02
//#define OP_DIV 0x03
//#define OP_MOD 0x04
//#define OP_EQ  0x05
//#define OP_NE  0x06
//#define OP_LT  0x07
//#define OP_GT  0x08
//#define OP_LTE 0x09
//#define OP_GTE 0x0A
//#define OP_AND 0x0B
//#define OP_OR  0x0C
//#define OP_XOR 0x0D
//#define OP_LSH 0x0E
//#define OP_RSH 0x0F

//#define DUP_F 0xFFFF
//#define SUP_F 0xFFFE
//#define LOG_F 0xFFFD

//#define LAZY 0x0
//#define STRI 0x1

//#define VOID 0x00000000000000

//// Heap
//// ----

//void set_len(u64 size) {
  //*HVM.size = size;
//}

//void set_itr(u64 itrs) {
  //*HVM.itrs = itrs;
//}

//u64 get_len() {
  //return *HVM.size;
//}

//u64 get_itr() {
  //return *HVM.itrs;
//}

//u64 fresh() {
  //return (*HVM.frsh)++;
//}

//// Terms
//// ------

//Term term_new(Tag tag, Lab lab, Loc loc) {
  //Term tag_enc = tag;
  //Term lab_enc = ((Term)lab) << 8;
  //Term loc_enc = ((Term)loc) << 32;
  //return tag_enc | lab_enc | loc_enc;
//}

//Tag term_tag(Term x) {
  //return x & 0x7F;
//}

//Lab term_lab(Term x) {
  //return (x >> 8) & 0xFFFFFF;
//}

//Loc term_loc(Term x) {
  //return (x >> 32) & 0xFFFFFFFF;
//}

//u64 term_get_bit(Term x) {
  //return (x >> 7) & 1;
//}

//Term term_set_bit(Term term) {
  //return term | (1ULL << 7);
//}

//Term term_rem_bit(Term term) {
  //return term & ~(1ULL << 7);
//}

//Term term_set_loc(Term x, Loc loc) {
  //return (x & 0x00000000FFFFFFFF) | (((Term)loc) << 32);
//}

//_Bool term_is_atom(Term term) {
  //switch (term_tag(term)) {
    //case ERA:
    //case W32:
    //case NIL:
    //case SET:
    //case EMP:
    //case UNI:
    //case U32: return 1;
    //default: return 0;
  //}
//}

//// Atomics
//// -------

//Term swap(Loc loc, Term term) {
  //Term val = HVM.heap[loc];
  //HVM.heap[loc] = term;
  //if (val == 0) {
    //printf("SWAP 0 at %08llx\n", (u64)loc);
    //exit(0);
  //}
  //return val;
//}

//Term got(Loc loc) {
  //Term val = HVM.heap[loc];
  //if (val == 0) {
    //printf("GOT 0 at %08llx\n", (u64)loc);
    //exit(0);
  //}
  //return val;
//}

//void set(Loc loc, Term term) {
  //HVM.heap[loc] = term;
//}

//void sub(Loc loc, Term term) {
  //set(loc, term_set_bit(term));
//}

//Term take(Loc loc) {
  //return swap(loc, VOID);
//}

//// Allocation
//// ----------

//Loc alloc_node(Loc arity) {
  //if (*HVM.size + arity > MAX_HEAP_SIZE) {
    //printf("Heap memory limit exceeded\n");
    //exit(1);
  //}
  //u64 old = *HVM.size;
  //*HVM.size += arity;
  //return old;
//}

//void inc_itr() {
  //(*HVM.itrs)++;
//}

//// Stack
//// ----

//void spush(Term term, Term* sbuf, u64* spos) {
  //if (*spos >= MAX_STACK_SIZE) {
    //printf("Stack memory limit exceeded\n");
    //exit(1);
  //}
  //sbuf[(*spos)++] = term;
//}

//Term spop(Term* sbuf, u64* spos) {
  //return sbuf[--(*spos)];
//}

//// Stringification
//// ---------------

//void print_tag(Tag tag) {
  //switch (tag) {
    //case VAR: printf("VAR"); break;
    //case DP0: printf("DP0"); break;
    //case DP1: printf("DP1"); break;
    //case REF: printf("REF"); break;
    //case LET: printf("LET"); break;
    //case ERA: printf("ERA"); break;
    //case SUP: printf("SUP"); break;
    //case DUP: printf("DUP"); break;
    //case SET: printf("SET"); break;
    //case EMP: printf("EMP"); break;
    //case EFQ: printf("EFQ"); break;
    //case UNI: printf("UNI"); break;
    //case NIL: printf("NIL"); break;
    //case USE: printf("USE"); break;
    //case U32: printf("U32"); break;
    //case W32: printf("W32"); break;
    //case SWI: printf("SWI"); break;
    //case OPX: printf("OPX"); break;
    //case OPY: printf("OPY"); break;
    //case SIG: printf("SIG"); break;
    //case TUP: printf("TUP"); break;
    //case GET: printf("GET"); break;
    //case ALL: printf("ALL"); break;
    //case LAM: printf("LAM"); break;
    //case APP: printf("APP"); break;
    //default : printf("???"); break;
  //}
//}

//void print_term(Term term) {
  //printf("term_new(");
  //print_tag(term_tag(term));
  //printf(",0x%06llx,0x%08llx)", (u64)term_lab(term), (u64)term_loc(term));
//}

//void print_heap() {
  //for (Loc i = 0; i < *HVM.size; i++) {
    //Term term = got(i);
    //if (term != 0) {
      //printf("set(0x%08llx, ", (u64)i);
      //print_term(term);
      //printf(");\n");
    //}
  //}
//}

//// Evaluation
//// ----------

//// @foo(&L{ax ay} b c ...)
//// ----------------------- REF-SUP-COPY (when @L not in @foo)
//// ! &L{bx by} = b
//// ! &L{cx cy} = b
//// ...
//// &L{@foo(ax bx cx ...) @foo(ay by cy ...)}

//// @foo(a b c ...)
//// -------------------- REF
//// book[foo](a b c ...)
//Term reduce_ref(Term ref) {
  //inc_itr();
  //return HVM.book[term_lab(ref)](ref);
//}

//// ! x = val
//// bod
//// --------- LET
//// x <- val
//// bod
//Term reduce_let(Term let, Term val) {
  ////printf("reduce_let "); print_term(let); printf("\n");
  //inc_itr();
  //Loc let_loc = term_loc(let);
  //Term bod    = got(let_loc + 1);
  //sub(let_loc + 0, val);
  //return bod;
//}

//// (* a)
//// ------- APP-ERA
//// *
//Term reduce_app_era(Term app, Term era) {
  ////printf("reduce_app_era "); print_term(app); printf("\n");
  //inc_itr();
  //return era;
//}

//// (λx.f a)
//// -------- APP-LAM
//// x <- a
//// f
//Term reduce_app_lam(Term app, Term lam) {
  //inc_itr();
  //Loc app_loc = term_loc(app);
  //Loc lam_loc = term_loc(lam);
  //Term bod    = got(lam_loc + 0);
  //Term arg    = got(app_loc + 1);
  //sub(lam_loc + 0, arg);
  //return bod;
//}

//// (&L{a,b} c)
//// ----------------- APP-SUP
//// ! &L{c0,c1} = c;
//// &L{(a c0),(b c1)}
//Term reduce_app_sup(Term app, Term sup) {
  ////printf("reduce_app_sup "); print_term(app); printf("\n");
  //inc_itr();
  //Loc app_loc = term_loc(app);
  //Loc sup_loc = term_loc(sup);
  //Lab sup_lab = term_lab(sup);
  //Term arg = got(app_loc + 1);
  //Term tm1 = got(sup_loc + 1);
  //Loc loc = alloc_node(3);
  //Loc ap0 = sup_loc;
  //Loc ap1 = loc + 0;
  //Loc su0 = app_loc;
  //Loc dup = loc + 2;
  //set(ap0 + 1, term_new(DP0, sup_lab, dup));
  //set(ap1 + 0, tm1);
  //set(ap1 + 1, term_new(DP1, sup_lab, dup));
  //set(su0 + 0, term_new(APP, 0, ap0));
  //set(su0 + 1, term_new(APP, 0, ap1));
  //set(dup + 0, arg);
  //return term_new(SUP, sup_lab, su0);
//}

//// (() a)
//// ------ APP-NIL
//// error
//Term reduce_app_nil(Term app, Term nil) {
  //printf("invalid:app-nil");
  //exit(0);
//}

//// ((a,b) c)
//// --------- APP-TUP
//// error
//Term reduce_app_tup(Term app, Term tup) {
  //printf("invalid:app-tup");
  //exit(0);
//}

//// (123 a)
//// ------- APP-W32
//// error
//Term reduce_app_w32(Term app, Term w32) {
  //printf("invalid:app-w32");
  //exit(0);
//}

//// (T a)
//// ----- APP-TYP
//// error
//Term reduce_app_typ(Term app, Term typ) {
  //printf("invalid:app-typ");
  //exit(0);
//}

//// ~*{():f}
//// -------- USE-ERA
//// *
//Term reduce_use_era(Term use, Term era) {
  //inc_itr();
  //return era;
//}

//// ~&L{a,b}{():f}
//// ----------------------- USE-SUP
//// ! &L{f0,f1} = f;
//// &L{~a{():f0},~b{():f1}}
//Term reduce_use_sup(Term use, Term sup) {
  //inc_itr();
  //Loc use_loc = term_loc(use);
  //Loc sup_loc = term_loc(sup);
  //Lab sup_lab = term_lab(sup);
  //Term f = got(use_loc + 1);
  //Term a = got(sup_loc + 0);
  //Term b = got(sup_loc + 1);
  //Loc loc = alloc_node(3);
  //Loc us0 = sup_loc;
  //Loc us1 = loc + 0;
  //Loc su0 = use_loc;
  //Loc dup = loc + 2;
  //set(us0 + 0, a);
  //set(us0 + 1, term_new(DP0, sup_lab, dup));
  //set(us1 + 0, b);
  //set(us1 + 1, term_new(DP1, sup_lab, dup));
  //set(su0 + 0, term_new(USE, 0, us0));
  //set(su0 + 1, term_new(USE, 0, us1));
  //set(dup + 0, f);
  //return term_new(SUP, sup_lab, su0);
//}

//// ~λx.g{():f}
//// ----------- USE-LAM
//// error
//Term reduce_use_lam(Term use, Term lam) {
  //printf("invalid:use-lam");
  //exit(0);
//}

//// ~(){():f}
//// --------- USE-NIL
//// f
//Term reduce_use_nil(Term use, Term nil) {
  //inc_itr();
  //Loc use_loc = term_loc(use);
  //Term f = got(use_loc + 1);
  //return f;
//}

//// ~(p,q){():f}
//// ------------ USE-TUP
//// error
//Term reduce_use_tup(Term use, Term tup) {
  //printf("invalid:use-tup");
  //exit(0);
//}

//// ~123{():f}
//// ---------- USE-W32
//// error
//Term reduce_use_w32(Term use, Term w32) {
  //printf("invalid:use-w32");
  //exit(0);
//}

//// ~T{():f}
//// -------- USE-TYP
//// error
//Term reduce_use_typ(Term use, Term typ) {
  //printf("invalid:use-typ");
  //exit(0);
//}

//// ~*{(,):f}
//// --------- GET-ERA
//// *
//Term reduce_get_era(Term get, Term era) {
  //inc_itr();
  //return era;
//}

//// ~&L{a,b}{(,): f}
//// ------------------------- GET-SUP
//// ! &L{f0,f1} = f;
//// &L{~a{(,):f0},~b{(,):f1}}
//Term reduce_get_sup(Term get, Term sup) {
  //inc_itr();
  //Loc get_loc = term_loc(get);
  //Loc sup_loc = term_loc(sup);
  //Lab sup_lab = term_lab(sup);
  //Term f = got(get_loc + 1);
  //Term a = got(sup_loc + 0);
  //Term b = got(sup_loc + 1);
  //Loc loc = alloc_node(3);
  //Loc gt0 = sup_loc;
  //Loc gt1 = loc + 0;
  //Loc su0 = get_loc;
  //Loc dup = loc + 2;
  //set(gt0 + 0, a);
  //set(gt0 + 1, term_new(DP0, sup_lab, dup));
  //set(gt1 + 0, b);
  //set(gt1 + 1, term_new(DP1, sup_lab, dup));
  //set(su0 + 0, term_new(GET, 0, gt0));
  //set(su0 + 1, term_new(GET, 0, gt1));
  //set(dup + 0, f);
  //return term_new(SUP, sup_lab, su0);
//}

//// ~(){(,):f}
//// ---------- GET-NIL
//// error
//Term reduce_get_nil(Term get, Term nil) {
  //printf("invalid:get-nil");
  //exit(0);
//}

//// ~(a,b){(,):f}
//// ------------- GET-TUP
//// (f a b)
//Term reduce_get_tup(Term get, Term tup) {
  //inc_itr();
  //Loc get_loc = term_loc(get);
  //Loc tup_loc = term_loc(tup);
  //Term f = got(get_loc + 1);
  //Term a = got(tup_loc + 0);
  //Term b = got(tup_loc + 1);
  //Loc app1 = alloc_node(2);
  //Loc app2 = get_loc;
  //set(app1 + 0, f);
  //set(app1 + 1, a);
  //set(app2 + 0, term_new(APP, 0, app1));
  //set(app2 + 1, b);
  //return term_new(APP, 0, app2);
//}

//// ~λx.g{(,):f}
//// ------------ GET-LAM
//// error
//Term reduce_get_lam(Term get, Term lam) {
  //printf("invalid:get-lam");
  //exit(0);
//}

//// ~123{(,):f}
//// ----------- GET-W32
//// error
//Term reduce_get_w32(Term get, Term w32) {
  //printf("invalid:get-w32");
  //exit(0);
//}

//// ~TYP{(,):f}
//// ----------- GET-TYP
//// error
//Term reduce_get_typ(Term get, Term typ) {
  //printf("invalid:get-typ");
  //exit(0);
//}

//// ~*{0:z;+:s}
//// ----------- SWI-ERA
//// *
//Term reduce_swi_era(Term swi, Term era) {
  //inc_itr();
  //return era;
//}

//// ~&L{a,b}{0:z;+:s}
//// ------------------------------- SWI-SUP
//// ! &L{z0,z1} = z;
//// ! &L{s0,s1} = s;
//// &L{~a{0:z0;+:s0},~b{0:z1;+:s1}}
////Term reduce_swi_sup(Term swi, Term sup) {
  ////inc_itr();
  ////Loc swi_loc = term_loc(swi);
  ////Loc sup_loc = term_loc(sup);
  ////Lab sup_lab = term_lab(sup);
  ////Term z = got(swi_loc + 1);
  ////Term s = got(swi_loc + 2);
  ////Term a = got(sup_loc + 0);
  ////Term b = got(sup_loc + 1);
  ////Loc loc = alloc_node(5);
  ////Loc sw0 = sup_loc;
  ////Loc sw1 = loc + 0;
  ////Loc su0 = swi_loc;
  ////Loc dz = loc + 2;
  ////Loc ds = loc + 3;
  ////set(sw0 + 0, a);
  ////set(sw0 + 1, term_new(DP0, sup_lab, dz));
  ////set(sw0 + 2, term_new(DP0, sup_lab, ds));
  ////set(sw1 + 0, b);
  ////set(sw1 + 1, term_new(DP1, sup_lab, dz));
  ////set(sw1 + 2, term_new(DP1, sup_lab, ds));
  ////set(su0 + 0, term_new(SWI, 0, sw0));
  ////set(su0 + 1, term_new(SWI, 0, sw1));
  ////set(dz + 0, z);
  ////set(ds + 0, s);
  ////return term_new(SUP, sup_lab, su0);
////}
//// BUG: the function above is INCORRECT. it is re-using a SUP node for a SWI
//// node, but a SUP node has only 2 fields, not 3. this will cause memory errors.
//// you MUST reuse the input sup node for the output sup node, and you MUST reuse
//// the input swi node for one of the output swi nodes - just like the original
//// reduce_mat_sup function did. fix it:
//Term reduce_swi_sup(Term swi, Term sup) {
  //// Correct implementation that reuses the given SUP/SWI nodes
  //inc_itr();

  //// Locations / labels
  //Loc swi_loc = term_loc(swi);   // original SWI (will become SWI0)
  //Loc sup_loc = term_loc(sup);   // SUP we will reuse
  //Lab sup_lab = term_lab(sup);   // label of the SUP
  //Lab swi_lab = term_lab(swi);   // (unused, still preserve)

  //// Extract SWI arms
  //Term z = got(swi_loc + 1);
  //Term s = got(swi_loc + 2);

  //// Extract SUP branches
  //Term a = got(sup_loc + 0);
  //Term b = got(sup_loc + 1);

  //// Allocate:
  ////   - 3 cells for the second SWI (swi1)
  ////   - 1 cell for dup(z)  (dupz)
  ////   - 1 cell for dup(s)  (dups)
  //Loc loc  = alloc_node(5);
  //Loc swi1 = loc;        // 3 cells (value,z,s)
  //Loc dupz = loc + 3;    // 1 cell
  //Loc dups = loc + 4;    // 1 cell

  //// Store originals inside dup nodes
  //set(dupz + 0, z);
  //set(dups + 0, s);

  //// Build SWI0 (reuse swi_loc)
  //set(swi_loc + 0, a);
  //set(swi_loc + 1, term_new(DP0, sup_lab, dupz));
  //set(swi_loc + 2, term_new(DP0, sup_lab, dups));

  //// Build SWI1 (new node)
  //set(swi1 + 0, b);
  //set(swi1 + 1, term_new(DP1, sup_lab, dupz));
  //set(swi1 + 2, term_new(DP1, sup_lab, dups));

  //// Build resulting SUP (reuse sup_loc)
  //set(sup_loc + 0, term_new(SWI, swi_lab, swi_loc));
  //set(sup_loc + 1, term_new(SWI, swi_lab, swi1));

  //return term_new(SUP, sup_lab, sup_loc);
//}

//// ~(){0:z;+:s}
//// ------------ SWI-NIL
//// error
//Term reduce_swi_nil(Term swi, Term nil) {
  //printf("invalid:swi-nil");
  //exit(0);
//}

//// ~(a,b){0:z;+:s}
//// --------------- SWI-TUP
//// error
//Term reduce_swi_tup(Term swi, Term tup) {
  //printf("invalid:swi-tup");
  //exit(0);
//}

//// ~λx.g{0:z;+:s}
//// -------------- SWI-LAM
//// error
//Term reduce_swi_lam(Term swi, Term lam) {
  //printf("invalid:swi-lam");
  //exit(0);
//}

//// ~n{0:z;+:s}
//// ----------- SWI-W32
//// if n = 0:
////   z
//// else:
////   (s (n-1))
//Term reduce_swi_w32(Term swi, Term w32) {
  //inc_itr();
  //Loc swi_loc = term_loc(swi);
  //u32 val = term_loc(w32);
  //Term z = got(swi_loc + 1);
  //Term s = got(swi_loc + 2);
  //if (val == 0) {
    //return z;
  //} else {
    //Loc app = swi_loc;
    //set(app + 0, s);
    //set(app + 1, term_new(W32, 0, val - 1));
    //return term_new(APP, 0, app);
  //}
//}

//// ~T{0:z;+:s}
//// ----------- SWI-TYP
//// error
//Term reduce_swi_typ(Term swi, Term typ) {
  //printf("invalid:swi-typ");
  //exit(0);
//}

//// ! &L{r,s} = *
//// ------------- DUP-ERA
//// r <- *
//// s <- *
//Term reduce_dup_era(Term dup, Term era) {
  ////printf("reduce_dup_era "); print_term(dup); printf("\n");
  //inc_itr();
  //Loc dup_loc = term_loc(dup);
  //sub(dup_loc + 0, era);
  //return era;
//}

//// ! &L{x,y} = &L{a,b}
//// ------------------- DUP-SUP
//// x <- a
//// y <- b
//Term reduce_dup_sup(Term dup, Term sup) {
  ////printf("reduce_dup_sup %u %u | %llu ", term_lab(dup), term_lab(sup), *HVM.spos); print_term(dup); printf(" "); print_term(sup); printf("\n");
  //inc_itr();
  //Loc dup_loc = term_loc(dup);
  //Lab dup_lab = term_lab(dup);
  //Lab sup_lab = term_lab(sup);
  //Loc sup_loc = term_loc(sup);
  //if (dup_lab == sup_lab) {
    //Term tm0 = got(sup_loc + 0);
    //Term tm1 = got(sup_loc + 1);
    //if (term_tag(dup) == DP0) {
      //sub(dup_loc + 0, tm1);
      //return tm0;
    //} else {
      //sub(dup_loc + 0, tm0);
      //return tm1;
    //}
  //} else {
    //Loc loc = alloc_node(4);
    //Loc du0 = sup_loc + 0;
    //Loc du1 = sup_loc + 1;
    //Loc su0 = loc + 0;
    //Loc su1 = loc + 2;
    //set(su0 + 0, term_new(DP0, dup_lab, du0));
    //set(su0 + 1, term_new(DP0, dup_lab, du1));
    //set(su1 + 0, term_new(DP1, dup_lab, du0));
    //set(su1 + 1, term_new(DP1, dup_lab, du1));
    //if (term_tag(dup) == DP0) {
      //sub(dup_loc + 0, term_new(SUP, sup_lab, su1));
      //return term_new(SUP, sup_lab, su0);
    //} else {
      //sub(dup_loc + 0, term_new(SUP, sup_lab, su0));
      //return term_new(SUP, sup_lab, su1);
    //}
  //}
//}

//// ! &L{r s} = λx.f
//// ---------------- DUP-LAM
//// r <- λx0.f0
//// s <- λx1.f1
//// x <- &L{x0,x1}
//// ! &L{f0,f1} = f
//Term reduce_dup_lam(Term dup, Term lam) {
  ////printf("reduce_dup_lam "); print_term(dup); printf("\n");
  //inc_itr();
  //Loc dup_loc = term_loc(dup);
  //Loc lam_loc = term_loc(lam);
  //Lab dup_lab = term_lab(dup);
  //Term bod = got(lam_loc + 0);
  //Loc loc = alloc_node(5);
  //Loc lm0 = loc + 0;
  //Loc lm1 = loc + 1;
  //Loc su0 = loc + 2;
  //Loc du0 = loc + 4;
  //sub(lam_loc + 0, term_new(SUP, dup_lab, su0));
  //set(lm0 + 0, term_new(DP0, dup_lab, du0));
  //set(lm1 + 0, term_new(DP1, dup_lab, du0));
  //set(su0 + 0, term_new(VAR, 0, lm0));
  //set(su0 + 1, term_new(VAR, 0, lm1));
  //set(du0 + 0, bod);
  //if (term_tag(dup) == DP0) {
    //sub(dup_loc + 0, term_new(LAM, 0, lm1));
    //return term_new(LAM, 0, lm0);
  //} else {
    //sub(dup_loc + 0, term_new(LAM, 0, lm0));
    //return term_new(LAM, 0, lm1);
  //}
//}

//// ! &L{x,y} = ()
//// -------------- DUP-NIL
//// x <- ()
//// y <- ()
//Term reduce_dup_nil(Term dup, Term nil) {
  //inc_itr();
  //Loc dup_loc = term_loc(dup);
  //sub(dup_loc + 0, nil);
  //return nil;
//}

//// ! &L{x,y} = (a,b)
//// ----------------- DUP-TUP
//// ! &L{a0,a1} = a
//// ! &L{b0,b1} = b
//// x <- (a0,b0)
//// y <- (a1,b1)
//Term reduce_dup_tup(Term dup, Term tup) {
  //inc_itr();
  //Loc dup_loc = term_loc(dup);
  //Lab dup_lab = term_lab(dup);
  //Loc tup_loc = term_loc(tup);
  //Term a = got(tup_loc + 0);
  //Term b = got(tup_loc + 1);
  //Loc loc = alloc_node(6);
  //Loc tup0 = loc + 0;
  //Loc tup1 = loc + 2;
  //Loc dupa = loc + 4;
  //Loc dupb = loc + 5;
  //set(dupa + 0, a);
  //set(dupb + 0, b);
  //set(tup0 + 0, term_new(DP0, dup_lab, dupa));
  //set(tup0 + 1, term_new(DP0, dup_lab, dupb));
  //set(tup1 + 0, term_new(DP1, dup_lab, dupa));
  //set(tup1 + 1, term_new(DP1, dup_lab, dupb));
  //if (term_tag(dup) == DP0) {
    //sub(dup_loc + 0, term_new(TUP, 0, tup1));
    //return term_new(TUP, 0, tup0);
  //} else {
    //sub(dup_loc + 0, term_new(TUP, 0, tup0));
    //return term_new(TUP, 0, tup1);
  //}
//}

//// ! &L{x,y} = 123
//// --------------- DUP-W32
//// x <- 123
//// y <- 123
//Term reduce_dup_w32(Term dup, Term w32) {
  ////printf("reduce_dup_w32 "); print_term(dup); printf("\n");
  //inc_itr();
  //Loc dup_loc = term_loc(dup);
  //sub(dup_loc + 0, w32);
  //return w32;
//}

//// ! &L{x,y} = Set
//// --------------- DUP-SET
//// x <- Set
//// y <- Set
//Term reduce_dup_set(Term dup, Term set) {
  //inc_itr();
  //Loc dup_loc = term_loc(dup);
  //sub(dup_loc + 0, set);
  //return set;
//}

//// ! &L{x,y} = ⊥
//// ------------- DUP-EMP
//// x <- ⊥
//// y <- ⊥
//Term reduce_dup_emp(Term dup, Term emp) {
  //inc_itr();
  //Loc dup_loc = term_loc(dup);
  //sub(dup_loc + 0, emp);
  //return emp;
//}

//// ! &L{x,y} = ⊤
//// ------------- DUP-UNI
//// x <- ⊤
//// y <- ⊤
//Term reduce_dup_uni(Term dup, Term uni) {
  //inc_itr();
  //Loc dup_loc = term_loc(dup);
  //sub(dup_loc + 0, uni);
  //return uni;
//}

//// ! &L{x,y} = U32
//// -------------- DUP-U32
//// x <- U32
//// y <- U32
//Term reduce_dup_u32(Term dup, Term u32) {
  //inc_itr();
  //Loc dup_loc = term_loc(dup);
  //sub(dup_loc + 0, u32);
  //return u32;
//}

//// ! &L{x,y} = ΣA.B
//// ----------------- DUP-SIG
//// ! &L{A0,A1} = A
//// ! &L{B0,B1} = B
//// x <- ΣA0.B0
//// y <- ΣA1.B1
//Term reduce_dup_sig(Term dup, Term sig) {
  //inc_itr();
  //Loc dup_loc = term_loc(dup);
  //Lab dup_lab = term_lab(dup);
  //Loc sig_loc = term_loc(sig);
  //Term a = got(sig_loc + 0);
  //Term b = got(sig_loc + 1);
  //Loc loc = alloc_node(6);
  //Loc sig0 = loc + 0;
  //Loc sig1 = loc + 2;
  //Loc dupa = loc + 4;
  //Loc dupb = loc + 5;
  //set(dupa + 0, a);
  //set(dupb + 0, b);
  //set(sig0 + 0, term_new(DP0, dup_lab, dupa));
  //set(sig0 + 1, term_new(DP0, dup_lab, dupb));
  //set(sig1 + 0, term_new(DP1, dup_lab, dupa));
  //set(sig1 + 1, term_new(DP1, dup_lab, dupb));
  //if (term_tag(dup) == DP0) {
    //sub(dup_loc + 0, term_new(SIG, 0, sig1));
    //return term_new(SIG, 0, sig0);
  //} else {
    //sub(dup_loc + 0, term_new(SIG, 0, sig0));
    //return term_new(SIG, 0, sig1);
  //}
//}

//// ! &L{x,y} = ΠA.B
//// ----------------- DUP-ALL
//// ! &L{A0,A1} = A
//// ! &L{B0,B1} = B
//// x <- ΠA0.B0
//// y <- ΠA1.B1
//Term reduce_dup_all(Term dup, Term all) {
  //inc_itr();
  //Loc dup_loc = term_loc(dup);
  //Lab dup_lab = term_lab(dup);
  //Loc all_loc = term_loc(all);
  //Term a = got(all_loc + 0);
  //Term b = got(all_loc + 1);
  //Loc loc = alloc_node(6);
  //Loc all0 = loc + 0;
  //Loc all1 = loc + 2;
  //Loc dupa = loc + 4;
  //Loc dupb = loc + 5;
  //set(dupa + 0, a);
  //set(dupb + 0, b);
  //set(all0 + 0, term_new(DP0, dup_lab, dupa));
  //set(all0 + 1, term_new(DP0, dup_lab, dupb));
  //set(all1 + 0, term_new(DP1, dup_lab, dupa));
  //set(all1 + 1, term_new(DP1, dup_lab, dupb));
  //if (term_tag(dup) == DP0) {
    //sub(dup_loc + 0, term_new(ALL, 0, all1));
    //return term_new(ALL, 0, all0);
  //} else {
    //sub(dup_loc + 0, term_new(ALL, 0, all0));
    //return term_new(ALL, 0, all1);
  //}
//}

//// ! &L{x,y} = @foo(a b c ...)
//// --------------------------- DUP-REF-COPY (when &L not in @foo)
//// ! &L{a0,a1} = a
//// ! &L{b0,b1} = b
//// ! &L{c0,c1} = c
//// ...
//// x <- @foo(a0 b0 c0 ...)
//// y <- @foo(a1 b1 c1 ...)
//Term reduce_dup_ref(Term dup, Term ref) {
  ////printf("reduce_dup_ref "); print_term(dup); printf("\n");
  //inc_itr();
  //Loc dup_loc = term_loc(dup);
  //Lab dup_lab = term_lab(dup);
  //Loc ref_loc = term_loc(ref);
  //Lab ref_lab = term_lab(ref);
  //u64 ref_ari = HVM.fari[ref_lab];
  //Loc loc = alloc_node(ref_ari * 2);
  //Loc ref0 = ref_loc;
  //Loc ref1 = loc + 0;
  //for (u64 i = 0; i < ref_ari; i++) {
    //Loc du0 = loc + ref_ari + i;
    //set(du0 + 0, got(ref_loc + i));
    //set(ref0 + i, term_new(DP0, dup_lab, du0));
    //set(ref1 + i, term_new(DP1, dup_lab, du0));
  //}
  //if (term_tag(dup) == DP0) {
    //sub(dup_loc + 0, term_new(REF, ref_lab, ref1));
    //return term_new(REF, ref_lab, ref0);
  //} else {
    //sub(dup_loc + 0, term_new(REF, ref_lab, ref0));
    //return term_new(REF, ref_lab, ref1);
  //}
//}

//// (<op * y)
//// --------- OPX-ERA
//// *
//Term reduce_opx_era(Term opx, Term era) {
  ////printf("reduce_opx_era "); print_term(opx); printf("\n");
  //inc_itr();
  //return era;
//}

//// (<op λx.f y)
//// ----------- OPX-LAM
//// error
//Term reduce_opx_lam(Term opx, Term lam) {
  ////printf("reduce_opx_lam "); print_term(opx); printf("\n");
  //printf("invalid:opx-lam");
  //exit(0);
//}

//// (<op &L{a,b} y)
//// ------------------------- OPX-SUP
//// ! &L{y0,y1} = y;
//// &L{(<op a y0),(<op b y1)}
//Term reduce_opx_sup(Term opx, Term sup) {
  ////printf("reduce_opx_sup "); print_term(opx); printf("\n");
  //inc_itr();
  //Loc opx_loc = term_loc(opx);
  //Loc sup_loc = term_loc(sup);
  //Lab sup_lab = term_lab(sup);
  //Term nmy    = got(opx_loc + 1);
  //Term tm0    = got(sup_loc + 0);
  //Term tm1    = got(sup_loc + 1);
  //Loc loc     = alloc_node(3);
  //Loc op0     = opx_loc;
  //Loc op1     = sup_loc;
  //Loc su0     = loc + 0;
  //Loc du0     = loc + 2;
  //set(op0 + 0, tm0);
  //set(op0 + 1, term_new(DP0, sup_lab, du0));
  //set(op1 + 0, tm1);
  //set(op1 + 1, term_new(DP1, sup_lab, du0));
  //set(su0 + 0, term_new(OPX, term_lab(opx), op0));
  //set(su0 + 1, term_new(OPX, term_lab(opx), op1));
  //set(du0 + 0, nmy);
  //return term_new(SUP, sup_lab, su0);
//}

//// (<op () y)
//// ---------- OPX-NIL
//// error
//Term reduce_opx_nil(Term opx, Term nil) {
  //printf("invalid:opx-nil");
  //exit(0);
//}

//// (<op (x0,x1) y)
//// --------------- OPX-TUP
//// error
//Term reduce_opx_tup(Term opx, Term tup) {
  //printf("invalid:opx-tup");
  //exit(0);
//}

//// (<op x y)
//// --------- OPX-W32
//// (>op y x)
//Term reduce_opx_w32(Term opx, Term nmx) {
  ////printf("reduce_opx_w32 "); print_term(opx); printf("\n");
  //inc_itr();
  //Lab opx_lab = term_lab(opx);
  //Loc opx_loc = term_loc(opx);
  //Term nmy = got(opx_loc + 1);
  //set(opx_loc + 0, nmy);
  //set(opx_loc + 1, nmx);
  //return term_new(OPY, opx_lab, opx_loc);
//}

//// (<op T y)
//// --------- OPX-TYP
//// error
//Term reduce_opx_typ(Term opx, Term typ) {
  //printf("invalid:opx-typ");
  //exit(0);
//}

//// (>op x *)
//// --------- OPY-ERA
//// *
//Term reduce_opy_era(Term opy, Term era) {
  ////printf("reduce_opy_era "); print_term(opy); printf("\n");
  //inc_itr();
  //return era;
//}

//// (>op x λy.f)
//// ------------ OPY-LAM
//// error
//Term reduce_opy_lam(Term opy, Term lam) {
  ////printf("reduce_opy_lam "); print_term(opy); printf("\n");
  //printf("invalid:opy-lam");
  //exit(0);
//}

//// (>op x &L{y0,y1})
//// ----------------------- OPY-SUP
//// &L{>op(x y0),>op(x y1)}
//Term reduce_opy_sup(Term opy, Term sup) {
  ////printf("reduce_opy_sup "); print_term(opy); printf("\n");
  //inc_itr();
  //Loc opy_loc = term_loc(opy);
  //Loc sup_loc = term_loc(sup);
  //Lab sup_lab = term_lab(sup);
  //Term nmx    = got(opy_loc + 1);
  //Term tm0    = got(sup_loc + 0);
  //Term tm1    = got(sup_loc + 1);
  //Loc op0     = sup_loc;
  //Loc op1     = opy_loc;
  //Loc su0     = alloc_node(2);
  //set(op0 + 1, nmx);
  //set(op1 + 0, tm1);
  //set(su0 + 0, term_new(OPY, term_lab(opy), op0));
  //set(su0 + 1, term_new(OPY, term_lab(opy), op1));
  //return term_new(SUP, sup_lab, su0);
//}

//// (>op x ())
//// ---------- OPY-NIL
//// error
//Term reduce_opy_nil(Term opy, Term nil) {
  //printf("invalid:opy-nil");
  //exit(0);
//}

//// (>op x (y0,y1))
//// --------------- OPY-TUP
//// error
//Term reduce_opy_tup(Term opy, Term tup) {
  //printf("invalid:opy-tup");
  //exit(0);
//}

//// (>op x y)
//// --------- OPY-W32
//// x <op> y
//Term reduce_opy_w32(Term opy, Term val) {
  ////printf("reduce_opy_w32 "); print_term(opy); printf("\n");
  //inc_itr();
  //Loc opy_loc = term_loc(opy);
  //Tag t = term_tag(val);
  //u32 x = term_loc(got(opy_loc + 1));
  //u32 y = term_loc(val);
  //printf("... %u %u\n", x, y);
  //u32 result;
  //switch (term_lab(opy)) {
    //case OP_ADD: result = x + y; break;
    //case OP_SUB: result = x - y; break;
    //case OP_MUL: result = x * y; break;
    //case OP_DIV: result = x / y; break;
    //case OP_MOD: result = x % y; break;
    //case OP_EQ:  result = x == y; break;
    //case OP_NE:  result = x != y; break;
    //case OP_LT:  result = x < y; break;
    //case OP_GT:  result = x > y; break;
    //case OP_LTE: result = x <= y; break;
    //case OP_GTE: result = x >= y; break;
    //case OP_AND: result = x & y; break;
    //case OP_OR:  result = x | y; break;
    //case OP_XOR: result = x ^ y; break;
    //case OP_LSH: result = x << y; break;
    //case OP_RSH: result = x >> y; break;
    //default: {
      //printf("invalid:opy-w32");
      //exit(0);
    //}
  //}
  //return term_new(W32, 0, result);
//}

//// (>op x T)
//// --------- OPY-TYP
//// error
//Term reduce_opy_typ(Term opy, Term typ) {
  //printf("invalid:opy-typ");
  //exit(0);
//}

//Term reduce_ref_sup(Term ref, u16 idx) {
  //inc_itr();
  //Loc ref_loc = term_loc(ref);
  //Lab ref_lab = term_lab(ref);
  //u16 fun_id = ref_lab;
  //u16 arity  = HVM.fari[fun_id];
  //if (idx >= arity) {
    //printf("ERROR: Invalid index in reduce_ref_sup\n");
    //exit(1);
  //}
  //Term sup = got(ref_loc + idx);
  //if (term_tag(sup) != SUP) {
    //printf("ERROR: Expected SUP at index %u\n", idx);
    //exit(1);
  //}
  //Lab sup_lab = term_lab(sup);
  //Loc sup_loc = term_loc(sup);
  //Term sup0 = got(sup_loc + 0);
  //Term sup1 = got(sup_loc + 1);
  //// Allocate space for new REF node arguments for the second branch
  //Loc ref1_loc = alloc_node(arity);
  //for (u64 i = 0; i < arity; ++i) {
    //if (i != idx) {
      //// Duplicate argument
      //Term arg = got(ref_loc + i);
      //Loc dup_loc = alloc_node(1);
      //set(dup_loc + 0, arg);
      //set(ref_loc + i, term_new(DP0, sup_lab, dup_loc));
      //set(ref1_loc + i, term_new(DP1, sup_lab, dup_loc));
    //} else {
      //// Set the SUP components directly
      //set(ref_loc + i, sup0);
      //set(ref1_loc + i, sup1);
    //}
  //}
  //// Create new REF nodes
  //Term ref0 = term_new(REF, ref_lab, ref_loc);
  //Term ref1 = term_new(REF, ref_lab, ref1_loc);
  //// Reuse sup_loc to create the new SUP node
  //set(sup_loc + 0, ref0);
  //set(sup_loc + 1, ref1);
  //return term_new(SUP, sup_lab, sup_loc);
//}

//Term reduce(Term term) {
  //if (term_tag(term) >= ERA) return term;
  //Term  next = term;
  //u64   stop = *HVM.spos;
  //u64   spos = stop;
  //Term* sbuf = HVM.sbuf;

  //while (1) {
    ////printf("NEXT "); print_term(term); printf("\n");
    ////printf("PATH ");
    ////for (u64 i = 0; i < *spos; ++i) {
      ////print_tag(term_tag(HVM.sbuf[i]));
      ////printf(" ");
    ////}
    ////printf(" ~ %p", HVM.sbuf);
    ////printf("\n");
    //Tag tag = term_tag(next);
    //Lab lab = term_lab(next);
    //Loc loc = term_loc(next);

    //// On variables: substitute
    //// On eliminators: move to field
    //switch (tag) {
      //case LET: {
        //switch (lab) {
          //case LAZY: {
            //next = reduce_let(next, got(loc + 0));
            //continue;
          //}
          //case STRI: {
            //spush(next, sbuf, &spos);
            //next = got(loc + 0);
            //continue;
          //}
          //default: {
            //printf("invalid:let");
            //exit(0);
          //}
        //}
      //}

      //case APP:
      //case USE:
      //case GET:
      //case SWI:
      //case OPX:
      //case OPY: {
        //spush(next, sbuf, &spos);
        //next = got(loc + 0);
        //continue;
      //}

      //case DP0:
      //case DP1: {
        //Term sub = got(loc + 0);
        //if (term_get_bit(sub) == 0) {
          //spush(next, sbuf, &spos);
          //next = sub;
          //continue;
        //} else {
          //next = term_rem_bit(sub);
          //continue;
        //}
      //}

      //case VAR: {
        //Term sub = got(loc);
        //if (term_get_bit(sub) == 0) {
          //break;
        //} else {
          //next = term_rem_bit(sub);
          //continue;
        //}
      //}

      //case REF: {
        //*HVM.spos = spos;
        //next = reduce_ref(next);
        //spos = *HVM.spos;
        //continue;
      //}

      //default: break;
    //}

    //// Empty stack: term is in WHNF
    //if (spos == stop) {
      //*HVM.spos = spos;
      //return next;
    //}

    //// Interaction Dispatcher
    //Term prev = spop(sbuf, &spos);
    //Tag  ptag = term_tag(prev);
    //Lab  plab = term_lab(prev);
    //Loc  ploc = term_loc(prev);
    //switch (ptag) {
      //case LET: {
        //next = reduce_let(prev, next);
        //continue;
      //}

      //case APP: {
        //switch (tag) {
          //case ERA: next = reduce_app_era(prev, next); continue;
          //case LAM: next = reduce_app_lam(prev, next); continue;
          //case SUP: next = reduce_app_sup(prev, next); continue;
          //case NIL: next = reduce_app_nil(prev, next); continue;
          //case TUP: next = reduce_app_tup(prev, next); continue;
          //case W32: next = reduce_app_w32(prev, next); continue;
          //case SET:
          //case EMP:
          //case UNI:
          //case U32:
          //case SIG:
          //case ALL: next = reduce_app_typ(prev, next); continue;
          //default: break;
        //}
      //}

      //case USE: {
        //switch (tag) {
          //case ERA: next = reduce_use_era(prev, next); continue;
          //case SUP: next = reduce_use_sup(prev, next); continue;
          //case LAM: next = reduce_use_lam(prev, next); continue;
          //case NIL: next = reduce_use_nil(prev, next); continue;
          //case TUP: next = reduce_use_tup(prev, next); continue;
          //case W32: next = reduce_use_w32(prev, next); continue;
          //case SET:
          //case EMP:
          //case UNI:
          //case U32:
          //case SIG:
          //case ALL: next = reduce_use_typ(prev, next); continue;
          //default: break;
        //}
      //}

      //case GET: {
        //switch (tag) {
          //case ERA: next = reduce_get_era(prev, next); continue;
          //case SUP: next = reduce_get_sup(prev, next); continue;
          //case NIL: next = reduce_get_nil(prev, next); continue;
          //case TUP: next = reduce_get_tup(prev, next); continue;
          //case LAM: next = reduce_get_lam(prev, next); continue;
          //case W32: next = reduce_get_w32(prev, next); continue;
          //case SET:
          //case EMP:
          //case UNI:
          //case U32:
          //case SIG:
          //case ALL: next = reduce_get_typ(prev, next); continue;
          //default: break;
        //}
      //}

      //case SWI: {
        //switch (tag) {
          //case ERA: next = reduce_swi_era(prev, next); continue;
          //case SUP: next = reduce_swi_sup(prev, next); continue;
          //case NIL: next = reduce_swi_nil(prev, next); continue;
          //case TUP: next = reduce_swi_tup(prev, next); continue;
          //case LAM: next = reduce_swi_lam(prev, next); continue;
          //case W32: next = reduce_swi_w32(prev, next); continue;
          //case SET:
          //case EMP:
          //case UNI:
          //case U32:
          //case SIG:
          //case ALL: next = reduce_swi_typ(prev, next); continue;
          //default: break;
        //}
      //}

      //case DP0:
      //case DP1: {
        //switch (tag) {
          //case ERA: next = reduce_dup_era(prev, next); continue;
          //case SUP: next = reduce_dup_sup(prev, next); continue;
          //case LAM: next = reduce_dup_lam(prev, next); continue;
          //case NIL: next = reduce_dup_nil(prev, next); continue;
          //case TUP: next = reduce_dup_tup(prev, next); continue;
          //case W32: next = reduce_dup_w32(prev, next); continue;
          //case SET: next = reduce_dup_set(prev, next); continue;
          //case EMP: next = reduce_dup_emp(prev, next); continue;
          //case UNI: next = reduce_dup_uni(prev, next); continue;
          //case U32: next = reduce_dup_u32(prev, next); continue;
          //case SIG: next = reduce_dup_sig(prev, next); continue;
          //case ALL: next = reduce_dup_all(prev, next); continue;
          //case REF: next = reduce_dup_ref(prev, next); continue;
          //default: break;
        //}
      //}

      //case OPX: {
        //switch (tag) {
          //case ERA: next = reduce_opx_era(prev, next); continue;
          //case LAM: next = reduce_opx_lam(prev, next); continue;
          //case SUP: next = reduce_opx_sup(prev, next); continue;
          //case NIL: next = reduce_opx_nil(prev, next); continue;
          //case TUP: next = reduce_opx_tup(prev, next); continue;
          //case W32: next = reduce_opx_w32(prev, next); continue;
          //case SET:
          //case EMP:
          //case UNI:
          //case U32:
          //case SIG:
          //case ALL: next = reduce_opx_typ(prev, next); continue;
          //default: break;
        //}
      //}

      //case OPY: {
        //switch (tag) {
          //case ERA: next = reduce_opy_era(prev, next); continue;
          //case LAM: next = reduce_opy_lam(prev, next); continue;
          //case SUP: next = reduce_opy_sup(prev, next); continue;
          //case NIL: next = reduce_opy_nil(prev, next); continue;
          //case TUP: next = reduce_opy_tup(prev, next); continue;
          //case W32: next = reduce_opy_w32(prev, next); continue;
          //case SET:
          //case EMP:
          //case UNI:
          //case U32:
          //case SIG:
          //case ALL: next = reduce_opy_typ(prev, next); continue;
          //default: break;
        //}
      //}

      //default: break;
    //}

    //// No interaction: push term back to stack
    //spush(prev, sbuf, &spos);

    //// Update parent chain
    //while (spos > stop) {
      //Term host = spop(sbuf, &spos);
      //Tag  htag = term_tag(host);
      //Lab  hlab = term_lab(host);
      //Loc  hloc = term_loc(host);
      //set(hloc + 0, next);
      //next = host;
    //}
    //*HVM.spos = spos;
    //return next;
  //}
//}

//Term reduce_at(Loc host) {
  //Term tm0 = got(host);
  //if (term_tag(tm0) >= ERA) {
    //return tm0;
  //}
  //Term tm1 = reduce(tm0);
  //set(host, tm1);
  //return tm1;
//}

//Term normal(Term term) {
  //Term wnf = reduce(term);
  //Tag tag = term_tag(wnf);
  //Lab lab = term_lab(wnf);
  //Loc loc = term_loc(wnf);
  //switch (tag) {
    //case LAM: {
      //Term bod = got(loc + 0);
      //bod = normal(bod);
      //set(loc + 0, bod);
      //return wnf;
    //}

    //case APP: {
      //Term fun = got(loc + 0);
      //Term arg = got(loc + 1);
      //fun = normal(fun);
      //arg = normal(arg);
      //set(loc + 0, fun);
      //set(loc + 1, arg);
      //return wnf;
    //}

    //case SUP: {
      //Term tm0 = got(loc + 0);
      //Term tm1 = got(loc + 1);
      //tm0 = normal(tm0);
      //tm1 = normal(tm1);
      //set(loc + 0, tm0);
      //set(loc + 1, tm1);
      //return wnf;
    //}

    //case TUP: {
      //Term tm0 = got(loc + 0);
      //Term tm1 = got(loc + 1);
      //tm0 = normal(tm0);
      //tm1 = normal(tm1);
      //set(loc + 0, tm0);
      //set(loc + 1, tm1);
      //return wnf;
    //}

    //case SIG: {
      //Term tm0 = got(loc + 0);
      //Term tm1 = got(loc + 1);
      //tm0 = normal(tm0);
      //tm1 = normal(tm1);
      //set(loc + 0, tm0);
      //set(loc + 1, tm1);
      //return wnf;
    //}

    //case ALL: {
      //Term tm0 = got(loc + 0);
      //Term tm1 = got(loc + 1);
      //tm0 = normal(tm0);
      //tm1 = normal(tm1);
      //set(loc + 0, tm0);
      //set(loc + 1, tm1);
      //return wnf;
    //}

    //case DP0:
    //case DP1: {
      //Term val = got(loc + 0);
      //val = normal(val);
      //set(loc + 0, val);
      //return wnf;
    //}

    //case USE:
    //case GET:
    //case SWI: {
      //for (u64 i = 0; i <= 2; i++) {
        //Term arg = got(loc + i);
        //arg = normal(arg);
        //set(loc + i, arg);
      //}
      //return wnf;
    //}

    //default:
      //return wnf;
  //}
//}

//// Primitives
//// ----------

//// Primitive: Dynamic Sup `@SUP(lab tm0 tm1)`
//// Allocates a new SUP node with given label.
//Term SUP_f(Term ref) {
  //Loc ref_loc = term_loc(ref);
  //Term lab = reduce(got(ref_loc + 0));
  //Term lab_val = term_loc(lab);
  //if (term_tag(lab) != W32) {
    //printf("ERROR:non-numeric-sup-label\n");
  //}
  //if (lab_val > 0xFFFF) {
    //printf("ERROR:sup-label-too-large\n");
  //}
  //Term tm0 = got(ref_loc + 1);
  //Term tm1 = got(ref_loc + 2);
  //Loc  sup = alloc_node(2);
  //Term ret = term_new(SUP, lab_val, sup);
  //set(sup + 0, tm0);
  //set(sup + 1, tm1);
  //*HVM.itrs += 1;
  //return ret;
//}

//// Primitive: Dynamic Dup `@DUP(lab val λdp0λdp1(bod))`
//// Creates a DUP node with given label.
//Term DUP_f(Term ref) {
  //Loc ref_loc = term_loc(ref);
  //Term lab = reduce(got(ref_loc + 0));
  //Term lab_val = term_loc(lab);
  //if (term_tag(lab) != W32) {
    //printf("ERROR:non-numeric-dup-label\n");
  //}
  //if (lab_val > 0xFFFF) {
    //printf("ERROR:dup-label-too-large\n");
  //}
  //Term val = got(ref_loc + 1);
  //Term bod = got(ref_loc + 2);
  //Loc  dup = alloc_node(1);
  //set(dup + 0, val);
  //if (term_tag(bod) == LAM) {
    //Loc  lam0 = term_loc(bod);
    //Term bod0 = got(lam0 + 0);
    //if (term_tag(bod0) == LAM) {
      //Loc  lam1 = term_loc(bod0);
      //Term bod1 = got(lam1 + 0);
      //sub(lam0 + 0, term_new(DP0, lab_val, dup));
      //sub(lam1 + 0, term_new(DP1, lab_val, dup));
      //*HVM.itrs += 3;
      //return bod1;
    //}
  //}
  //Loc app0 = alloc_node(2);
  //set(app0 + 0, bod);
  //set(app0 + 1, term_new(DP0, lab_val, dup));
  //Loc app1 = alloc_node(2);
  //set(app1 + 0, term_new(APP, 0, app0));
  //set(app1 + 1, term_new(DP1, lab_val, dup));
  //*HVM.itrs += 1;
  //return term_new(APP, 0, app1);
//}

//Term LOG_f(Term ref) {
  //printf("TODO: LOG_f");
  //exit(0);
//}

//void *alloc_huge(size_t size) {
    //void *ptr = mmap(NULL, size, PROT_READ | PROT_WRITE,
                     //MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE,
                     //-1, 0);
    //if (ptr == MAP_FAILED) {
        //perror("mmap failed");
        //return NULL;
    //}
    //return ptr;
//}

//// Runtime Memory
//// --------------

//void hvm_init() {
  //HVM.sbuf = alloc_huge(MAX_STACK_SIZE * sizeof(Term)); 
  //HVM.heap = alloc_huge(MAX_HEAP_SIZE  * sizeof(Term));
  //HVM.spos = alloc_huge(sizeof(u64));
  //HVM.size = alloc_huge(sizeof(u64));
  //HVM.itrs = alloc_huge(sizeof(u64));
  //HVM.frsh = alloc_huge(sizeof(u64));

  //#define CHECK_ALLOC(ptr, name) if (!(ptr)) { printf(name " alloc failed\n"); allocs_failed++; }
  //int allocs_failed = 0; // Track if any allocation failed

  //CHECK_ALLOC(HVM.sbuf, "sbuf");
  //CHECK_ALLOC(HVM.heap, "heap");
  //CHECK_ALLOC(HVM.spos, "spos");
  //CHECK_ALLOC(HVM.size, "size");
  //CHECK_ALLOC(HVM.itrs, "itrs");
  //CHECK_ALLOC(HVM.frsh, "frsh");

  //if (allocs_failed > 0) {
    //printf("hvm_init alloc's failed: %d allocations failed\n", allocs_failed);
    //exit(1);
  //}
  //#undef CHECK_ALLOC

  //*HVM.spos = 0;
  //*HVM.size = 1;
  //*HVM.itrs = 0;
  //*HVM.frsh = 0x20;
  //HVM.book[SUP_F] = SUP_f;
  //HVM.book[DUP_F] = DUP_f;
  //HVM.book[LOG_F] = LOG_f;
  //for (int i = 0; i < 65536; i++) {
    //HVM.fari[i] = 0;
  //}
//}

//void hvm_munmap(void *ptr, size_t size, const char *name) {
  //if (ptr != MAP_FAILED) {
    //if (munmap(ptr, size) == -1) {
      //perror("munmap failed");
    //} else {
      //// printf("Successfully unmapped %s\n", name);
    //}
  //} else {
    //printf("%s is already null or invalid.\n", name);
  //}
//}

//void hvm_free() {
  //hvm_munmap(HVM.sbuf, MAX_STACK_SIZE * sizeof(Term), "sbuf");
  //hvm_munmap(HVM.heap, MAX_HEAP_SIZE  * sizeof(Term), "heap");
  //hvm_munmap(HVM.spos, sizeof(u64), "spos");
  //hvm_munmap(HVM.size, sizeof(u64), "size");
  //hvm_munmap(HVM.itrs, sizeof(u64), "itrs");
  //hvm_munmap(HVM.frsh, sizeof(u64), "frsh");
//}

//State* hvm_get_state() {
  //return &HVM;
//}

//void hvm_set_state(State* hvm) {
  //HVM.sbuf = hvm->sbuf;
  //HVM.spos = hvm->spos;
  //HVM.heap = hvm->heap;
  //HVM.size = hvm->size;
  //HVM.itrs = hvm->itrs;
  //HVM.frsh = hvm->frsh;
  //for (int i = 0; i < 65536; i++) {
    //HVM.book[i] = hvm->book[i];
    //HVM.fari[i] = hvm->fari[i];
  //}
//}

//void hvm_define(u16 fid, Term (*func)()) {
  ////printf("defined %llu %p\n", fid, func);
  //HVM.book[fid] = func;
//}

//void hvm_set_fari(u16 fid, u16 arity) {
  //HVM.fari[fid] = arity;
//}



// NOTE: I'm not actually sure if the original reduce_mat_sup function reused
// nodes in the same way that the new one is reusing. can you please compare
// them, and let us know if it is indeed following the same structure?
// then, you review the rest of the file, and check if the remaining functions
// are reusing nodes correctly. pay extra attention to the reduce_use_sup and
// reduce_get_sup functions. remember that they must reuse the old SUP for the
// new SUP, and the old matcher for the new matcher. write your report below:
// A: Reviewing the node reuse patterns:
//*   **`reduce_swi_sup`:** As confirmed previously, this function correctly reuses `swi_loc` for the first resulting SWI node and `sup_loc` for the resulting SUP node. Only the second SWI node and the DUP helpers are allocated. **Correct.**
//*   **`reduce_use_sup`:**
    //*   Reuses `sup_loc` for the first resulting `USE` node (`us0`). **Incorrect.** Should reuse `use_loc`.
    //*   Reuses `use_loc` for the resulting `SUP` node (`su0`). **Incorrect.** Should reuse `sup_loc`.
    //*   Allocates the second `USE` node (`us1`) and the `DUP` helper (`dup`).
    //*   **Conclusion:** This function swaps the intended reuse roles.
//*   **`reduce_get_sup`:**
    //*   Reuses `sup_loc` for the first resulting `GET` node (`gt0`). **Incorrect.** Should reuse `get_loc`.
    //*   Reuses `get_loc` for the resulting `SUP` node (`su0`). **Incorrect.** Should reuse `sup_loc`.
    //*   Allocates the second `GET` node (`gt1`) and the `DUP` helper (`dup`).
    //*   **Conclusion:** This function swaps the intended reuse roles, similar to `reduce_use_sup`.
//*   **Other `reduce_..._sup` functions:**
    //*   `reduce_app_sup`: Swaps reuse roles like `reduce_use_sup`. **Incorrect.**
    //*   `reduce_opx_sup`: Reuses `opx_loc` correctly for `op0`, but allocates the resulting `SUP` (`su0`) instead of reusing `sup_loc`. **Incorrect.**
    //*   `reduce_opy_sup`: Reuses `sup_loc` correctly for `op0`, but allocates the resulting `SUP` (`su0`) instead of reusing `opy_loc` for `op1`. **Incorrect.**
//*   **`reduce_dup_...` functions (duplicating values):**
    //*   `reduce_dup_tup`, `reduce_dup_sig`, `reduce_dup_all`: These allocate *both* resulting value nodes (`tup0`/`tup1`, etc.) instead of reusing the original value node's location (`tup_loc`, etc.) for one of them. **Incorrect.**
    //*   `reduce_dup_ref`: Correctly reuses `ref_loc` for `ref0`. **Correct.**
    //*   `reduce_dup_sup` (commute case): Allocates *both* resulting `SUP` nodes (`su0`, `su1`) instead of reusing `sup_loc` for one. **Incorrect.**
//**Summary:** The reuse pattern described (reusing the original eliminator for one branch and the original introducer for the wrapper) is correctly implemented in `reduce_swi_sup` and partially in `reduce_dup_ref` (for the REF part). However, `reduce_use_sup`, `reduce_get_sup`, `reduce_app_sup`, `reduce_opx_sup`, `reduce_opy_sup`, `reduce_dup_tup`, `reduce_dup_sig`, `reduce_dup_all`, and `reduce_dup_sup` (commute) do not follow this pattern correctly and require revision for consistent memory optimization.
//Interesting. Now, please review it again; this time, report any bug or inconsistency you find on the new version, w.r.t old version. Assume that the old version is 100% correct, but outdated (since it uses the old Core type). Your goal is to make sure that the new Runtime.c is equivalent to the old Runtime.c, if it were written with the new Core type in mind. remember that the ORIGINAL SEMANTICS must be preserved. for example, if in the original code, a given function only reused one node - so should be the case, in the new Runtime.c. you must also look for other kinds of errors, bugs, typos, and similar update issues. double check everything you list below to make sure you only report things that are actually incorrect.
//Write your complete report below, in ENGLISH:
//**REPORT:**

//This report focuses *only* on discrepancies, regressions, and new bugs introduced in the updated `Runtime.c` compared to the semantics and structure of the original (commented-out) C code, assuming the original was the correct reference for its time.

//1.  **Node Reuse Inconsistencies (Relative to Original Patterns):**
    //*   `reduce_use_sup`: This function handles a new eliminator (`USE`). Its reuse pattern (reusing `sup_loc` for the `USE` branch and `use_loc` for the result `SUP`) is inconsistent with the established pattern seen in the original `reduce_mat_sup` (which `USE` conceptually replaces in part). It should reuse `use_loc` for the `USE` branch and `sup_loc` for the result `SUP`. **Inconsistent Update.**
    //*   `reduce_get_sup`: Similar to `reduce_use_sup`, this handles a new eliminator (`GET`). Its reuse pattern (reusing `sup_loc` for the `GET` branch and `get_loc` for the result `SUP`) is also inconsistent with the original `reduce_mat_sup` pattern. It should reuse `get_loc` for the `GET` branch and `sup_loc` for the result `SUP`. **Inconsistent Update.**
    //*   `reduce_dup_tup`: This replaces the old `reduce_dup_ctr`. The original `reduce_dup_ctr` reused the input `ctr_loc` for one of the output `CTR` nodes. The new `reduce_dup_tup` allocates *both* output `TUP` nodes. This fails to preserve the original memory optimization strategy. It should reuse `tup_loc` for one of the branches. **Inconsistent Update/Regression.**
    //*   `reduce_dup_sig`: New type interaction. Following the pattern of the original `reduce_dup_ctr`, it should reuse `sig_loc` for one output `SIG` node, but it allocates both. **Inconsistent Update.**
    //*   `reduce_dup_all`: New type interaction. Following the pattern of the original `reduce_dup_ctr`, it should reuse `all_loc` for one output `ALL` node, but it allocates both. **Inconsistent Update.**
    //*   *Note:* Other functions like `reduce_app_sup`, `reduce_swi_sup`, `reduce_opx_sup`, `reduce_opy_sup`, `reduce_dup_sup` (commute), and `reduce_dup_ref` appear to maintain reuse patterns consistent with their original counterparts or established patterns.

//2.  **Missing Functionality (Relative to Original Implementation):**
    //*   `reduce_ref_sup`: The original C code included an implementation for `reduce_ref_sup` (handling `@foo(&L{...} ...)`). This function is entirely missing in the updated code, representing a loss of functionality compared to the original implementation. **Regression.**

//3.  **New Bugs Introduced:**
    //*   `normal` Function: The normalization logic for `USE` and `GET` uses a loop `for (u64 i = 0; i <= 2; i++)`. Since `USE` and `GET` nodes only have fields at indices 0 and 1, accessing index 2 (`got(loc + 2)`) is an out-of-bounds error. The condition should be `i <= 1`. This bug was not present in the original `normal` function's handling of `MAT`/`IFL`. **New Bug.**

//4.  **New Artifacts:**
    //*   `reduce_opy_w32`: Contains a `printf` statement (`printf("... %u %u\n", x, y);`) likely used for debugging, which was presumably not in the original reference code. **New Artifact.**

//**Summary:** The primary issues introduced during the update are inconsistencies in applying the original node reuse optimization strategy to new and replacement interaction rules (`USE-SUP`, `GET-SUP`, `DUP-TUP`, `DUP-SIG`, `DUP-ALL`), the complete omission of the previously implemented `reduce_ref_sup` function, a new out-of-bounds bug in the `normal` function, and a leftover debug print. Other aspects, including tag definitions, state management, and most interaction logic, appear correctly adapted from the original.

//TASK: rewrite Runtime.c again, taking in account the issues presented in the report:

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
  .fari = {0},
};

// Constants
// ---------

#define DP0 0x00
#define DP1 0x01
#define VAR 0x02
#define REF 0x03
#define LET 0x04
#define ERA 0x05
#define SUP 0x06
#define DUP 0x07
#define SET 0x08
#define EMP 0x09
#define EFQ 0x0A
#define UNI 0x0B
#define NIL 0x0C
#define USE 0x0D
#define U32 0x0E
#define W32 0x0F
#define SWI 0x10
#define OPX 0x11
#define OPY 0x12
#define SIG 0x13
#define TUP 0x14
#define GET 0x15
#define ALL 0x16
#define LAM 0x17
#define APP 0x18

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
    case NIL:
    case SET:
    case EMP:
    case UNI:
    case U32: return 1;
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
    case REF: printf("REF"); break;
    case LET: printf("LET"); break;
    case ERA: printf("ERA"); break;
    case SUP: printf("SUP"); break;
    case DUP: printf("DUP"); break;
    case SET: printf("SET"); break;
    case EMP: printf("EMP"); break;
    case EFQ: printf("EFQ"); break;
    case UNI: printf("UNI"); break;
    case NIL: printf("NIL"); break;
    case USE: printf("USE"); break;
    case U32: printf("U32"); break;
    case W32: printf("W32"); break;
    case SWI: printf("SWI"); break;
    case OPX: printf("OPX"); break;
    case OPY: printf("OPY"); break;
    case SIG: printf("SIG"); break;
    case TUP: printf("TUP"); break;
    case GET: printf("GET"); break;
    case ALL: printf("ALL"); break;
    case LAM: printf("LAM"); break;
    case APP: printf("APP"); break;
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

// (λx.f a)
// -------- APP-LAM
// x <- a
// f
Term reduce_app_lam(Term app, Term lam) {
  inc_itr();
  Loc app_loc = term_loc(app);
  Loc lam_loc = term_loc(lam);
  Term bod    = got(lam_loc + 0);
  Term arg    = got(app_loc + 1);
  sub(lam_loc + 0, arg);
  return bod;
}

// (&L{a,b} c)
// ----------------- APP-SUP
// ! &L{c0,c1} = c;
// &L{(a c0),(b c1)}
Term reduce_app_sup(Term app, Term sup) {
  //printf("reduce_app_sup "); print_term(app); printf("\n");
  inc_itr();
  Loc app_loc = term_loc(app);
  Loc sup_loc = term_loc(sup);
  Lab sup_lab = term_lab(sup);
  Term arg = got(app_loc + 1);
  Term a = got(sup_loc + 0);
  Term b = got(sup_loc + 1);
  Loc loc = alloc_node(3);
  Loc ap0 = app_loc;
  Loc ap1 = loc + 0;
  Loc dup = loc + 2;
  set(ap0 + 0, a);
  set(ap0 + 1, term_new(DP0, sup_lab, dup));
  set(ap1 + 0, b);
  set(ap1 + 1, term_new(DP1, sup_lab, dup));
  set(dup + 0, arg);
  set(sup_loc + 0, term_new(APP, 0, ap0));
  set(sup_loc + 1, term_new(APP, 0, ap1));
  return term_new(SUP, sup_lab, sup_loc);
}

// (() a)
// ------ APP-NIL
// error
Term reduce_app_nil(Term app, Term nil) {
  printf("invalid:app-nil");
  exit(0);
}

// ((a,b) c)
// --------- APP-TUP
// error
Term reduce_app_tup(Term app, Term tup) {
  printf("invalid:app-tup");
  exit(0);
}

// (123 a)
// ------- APP-W32
// error
Term reduce_app_w32(Term app, Term w32) {
  printf("invalid:app-w32");
  exit(0);
}

// (T a)
// ----- APP-TYP
// error
Term reduce_app_typ(Term app, Term typ) {
  printf("invalid:app-typ");
  exit(0);
}

// ~*{():f}
// -------- USE-ERA
// *
Term reduce_use_era(Term use, Term era) {
  inc_itr();
  return era;
}

// ~&L{a,b}{():f}
// ----------------------- USE-SUP
// ! &L{f0,f1} = f;
// &L{~a{():f0},~b{():f1}}
Term reduce_use_sup(Term use, Term sup) {
  inc_itr();
  Loc use_loc = term_loc(use);
  Loc sup_loc = term_loc(sup);
  Lab sup_lab = term_lab(sup);
  Term f = got(use_loc + 1);
  Term a = got(sup_loc + 0);
  Term b = got(sup_loc + 1);
  Loc loc = alloc_node(3);
  Loc us0 = use_loc;
  Loc us1 = loc + 0;
  Loc dup = loc + 2;
  set(us0 + 0, a);
  set(us0 + 1, term_new(DP0, sup_lab, dup));
  set(us1 + 0, b);
  set(us1 + 1, term_new(DP1, sup_lab, dup));
  set(dup + 0, f);
  set(sup_loc + 0, term_new(USE, 0, us0));
  set(sup_loc + 1, term_new(USE, 0, us1));
  return term_new(SUP, sup_lab, sup_loc);
}

// ~λx.g{():f}
// ----------- USE-LAM
// error
Term reduce_use_lam(Term use, Term lam) {
  printf("invalid:use-lam");
  exit(0);
}

// ~(){():f}
// --------- USE-NIL
// f
Term reduce_use_nil(Term use, Term nil) {
  inc_itr();
  Loc use_loc = term_loc(use);
  Term f = got(use_loc + 1);
  return f;
}

// ~(p,q){():f}
// ------------ USE-TUP
// error
Term reduce_use_tup(Term use, Term tup) {
  printf("invalid:use-tup");
  exit(0);
}

// ~123{():f}
// ---------- USE-W32
// error
Term reduce_use_w32(Term use, Term w32) {
  printf("invalid:use-w32");
  exit(0);
}

// ~T{():f}
// -------- USE-TYP
// error
Term reduce_use_typ(Term use, Term typ) {
  printf("invalid:use-typ");
  exit(0);
}

// ~*{(,):f}
// --------- GET-ERA
// *
Term reduce_get_era(Term get, Term era) {
  inc_itr();
  return era;
}

// ~&L{a,b}{(,): f}
// ------------------------- GET-SUP
// ! &L{f0,f1} = f;
// &L{~a{(,):f0},~b{(,):f1}}
Term reduce_get_sup(Term get, Term sup) {
  inc_itr();
  Loc get_loc = term_loc(get);
  Loc sup_loc = term_loc(sup);
  Lab sup_lab = term_lab(sup);
  Term f = got(get_loc + 1);
  Term a = got(sup_loc + 0);
  Term b = got(sup_loc + 1);
  Loc loc = alloc_node(3);
  Loc gt0 = get_loc;
  Loc gt1 = loc + 0;
  Loc dup = loc + 2;
  set(gt0 + 0, a);
  set(gt0 + 1, term_new(DP0, sup_lab, dup));
  set(gt1 + 0, b);
  set(gt1 + 1, term_new(DP1, sup_lab, dup));
  set(dup + 0, f);
  set(sup_loc + 0, term_new(GET, 0, gt0));
  set(sup_loc + 1, term_new(GET, 0, gt1));
  return term_new(SUP, sup_lab, sup_loc);
}

// ~(){(,):f}
// ---------- GET-NIL
// error
Term reduce_get_nil(Term get, Term nil) {
  printf("invalid:get-nil");
  exit(0);
}

// ~(a,b){(,):f}
// ------------- GET-TUP
// (f a b)
Term reduce_get_tup(Term get, Term tup) {
  inc_itr();
  Loc get_loc = term_loc(get);
  Loc tup_loc = term_loc(tup);
  Term f = got(get_loc + 1);
  Term a = got(tup_loc + 0);
  Term b = got(tup_loc + 1);
  Loc app1 = alloc_node(2);
  Loc app2 = get_loc;
  set(app1 + 0, f);
  set(app1 + 1, a);
  set(app2 + 0, term_new(APP, 0, app1));
  set(app2 + 1, b);
  return term_new(APP, 0, app2);
}

// ~λx.g{(,):f}
// ------------ GET-LAM
// error
Term reduce_get_lam(Term get, Term lam) {
  printf("invalid:get-lam");
  exit(0);
}

// ~123{(,):f}
// ----------- GET-W32
// error
Term reduce_get_w32(Term get, Term w32) {
  printf("invalid:get-w32");
  exit(0);
}

// ~TYP{(,):f}
// ----------- GET-TYP
// error
Term reduce_get_typ(Term get, Term typ) {
  printf("invalid:get-typ");
  exit(0);
}

// ~*{0:z;+:s}
// ----------- SWI-ERA
// *
Term reduce_swi_era(Term swi, Term era) {
  inc_itr();
  return era;
}

// ~&L{a,b}{0:z;+:s}
// ------------------------------- SWI-SUP
// ! &L{z0,z1} = z;
// ! &L{s0,s1} = s;
// &L{~a{0:z0;+:s0},~b{0:z1;+:s1}}
Term reduce_swi_sup(Term swi, Term sup) {
  // Correct implementation that reuses the given SUP/SWI nodes
  inc_itr();

  // Locations / labels
  Loc swi_loc = term_loc(swi);   // original SWI (will become SWI0)
  Loc sup_loc = term_loc(sup);   // SUP we will reuse
  Lab sup_lab = term_lab(sup);   // label of the SUP
  Lab swi_lab = term_lab(swi);   // (unused, still preserve)

  // Extract SWI arms
  Term z = got(swi_loc + 1);
  Term s = got(swi_loc + 2);

  // Extract SUP branches
  Term a = got(sup_loc + 0);
  Term b = got(sup_loc + 1);

  // Allocate:
  //   - 3 cells for the second SWI (swi1)
  //   - 1 cell for dup(z)  (dupz)
  //   - 1 cell for dup(s)  (dups)
  Loc loc  = alloc_node(5);
  Loc swi1 = loc;        // 3 cells (value,z,s)
  Loc dupz = loc + 3;    // 1 cell
  Loc dups = loc + 4;    // 1 cell

  // Store originals inside dup nodes
  set(dupz + 0, z);
  set(dups + 0, s);

  // Build SWI0 (reuse swi_loc)
  set(swi_loc + 0, a);
  set(swi_loc + 1, term_new(DP0, sup_lab, dupz));
  set(swi_loc + 2, term_new(DP0, sup_lab, dups));

  // Build SWI1 (new node)
  set(swi1 + 0, b);
  set(swi1 + 1, term_new(DP1, sup_lab, dupz));
  set(swi1 + 2, term_new(DP1, sup_lab, dups));

  // Build resulting SUP (reuse sup_loc)
  set(sup_loc + 0, term_new(SWI, swi_lab, swi_loc));
  set(sup_loc + 1, term_new(SWI, swi_lab, swi1));

  return term_new(SUP, sup_lab, sup_loc);
}

// ~(){0:z;+:s}
// ------------ SWI-NIL
// error
Term reduce_swi_nil(Term swi, Term nil) {
  printf("invalid:swi-nil");
  exit(0);
}

// ~(a,b){0:z;+:s}
// --------------- SWI-TUP
// error
Term reduce_swi_tup(Term swi, Term tup) {
  printf("invalid:swi-tup");
  exit(0);
}

// ~λx.g{0:z;+:s}
// -------------- SWI-LAM
// error
Term reduce_swi_lam(Term swi, Term lam) {
  printf("invalid:swi-lam");
  exit(0);
}

// ~n{0:z;+:s}
// ----------- SWI-W32
// if n = 0:
//   z
// else:
//   (s (n-1))
Term reduce_swi_w32(Term swi, Term w32) {
  inc_itr();
  Loc swi_loc = term_loc(swi);
  u32 val = term_loc(w32);
  Term z = got(swi_loc + 1);
  Term s = got(swi_loc + 2);
  if (val == 0) {
    return z;
  } else {
    Loc app = swi_loc;
    set(app + 0, s);
    set(app + 1, term_new(W32, 0, val - 1));
    return term_new(APP, 0, app);
  }
}

// ~T{0:z;+:s}
// ----------- SWI-TYP
// error
Term reduce_swi_typ(Term swi, Term typ) {
  printf("invalid:swi-typ");
  exit(0);
}

// ! &L{r,s} = *
// ------------- DUP-ERA
// r <- *
// s <- *
Term reduce_dup_era(Term dup, Term era) {
  //printf("reduce_dup_era "); print_term(dup); printf("\n");
  inc_itr();
  Loc dup_loc = term_loc(dup);
  sub(dup_loc + 0, era);
  return era;
}

// ! &L{x,y} = &L{a,b}
// ------------------- DUP-SUP
// x <- a
// y <- b
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

// ! &L{r s} = λx.f
// ---------------- DUP-LAM
// r <- λx0.f0
// s <- λx1.f1
// x <- &L{x0,x1}
// ! &L{f0,f1} = f
Term reduce_dup_lam(Term dup, Term lam) {
  //printf("reduce_dup_lam "); print_term(dup); printf("\n");
  inc_itr();
  Loc dup_loc = term_loc(dup);
  Loc lam_loc = term_loc(lam);
  Lab dup_lab = term_lab(dup);
  Term bod = got(lam_loc + 0);
  Loc loc = alloc_node(5);
  Loc lm0 = loc + 0;
  Loc lm1 = loc + 1;
  Loc su0 = loc + 2;
  Loc du0 = loc + 4;
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

// ! &L{x,y} = ()
// -------------- DUP-NIL
// x <- ()
// y <- ()
Term reduce_dup_nil(Term dup, Term nil) {
  inc_itr();
  Loc dup_loc = term_loc(dup);
  sub(dup_loc + 0, nil);
  return nil;
}

// ! &L{x,y} = (a,b)
// ----------------- DUP-TUP
// ! &L{a0,a1} = a
// ! &L{b0,b1} = b
// x <- (a0,b0)
// y <- (a1,b1)
Term reduce_dup_tup(Term dup, Term tup) {
  inc_itr();
  Loc dup_loc = term_loc(dup);
  Lab dup_lab = term_lab(dup);
  Loc tup_loc = term_loc(tup);
  Term a = got(tup_loc + 0);
  Term b = got(tup_loc + 1);
  Loc loc = alloc_node(4);
  Loc tup1 = loc + 0;
  Loc dupa = loc + 2;
  Loc dupb = loc + 3;
  set(dupa + 0, a);
  set(dupb + 0, b);
  set(tup_loc + 0, term_new(DP0, dup_lab, dupa));
  set(tup_loc + 1, term_new(DP0, dup_lab, dupb));
  set(tup1 + 0, term_new(DP1, dup_lab, dupa));
  set(tup1 + 1, term_new(DP1, dup_lab, dupb));
  if (term_tag(dup) == DP0) {
    sub(dup_loc + 0, term_new(TUP, 0, tup1));
    return term_new(TUP, 0, tup_loc);
  } else {
    sub(dup_loc + 0, term_new(TUP, 0, tup_loc));
    return term_new(TUP, 0, tup1);
  }
}

// ! &L{x,y} = 123
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

// ! &L{x,y} = Set
// --------------- DUP-SET
// x <- Set
// y <- Set
Term reduce_dup_set(Term dup, Term set) {
  inc_itr();
  Loc dup_loc = term_loc(dup);
  sub(dup_loc + 0, set);
  return set;
}

// ! &L{x,y} = ⊥
// ------------- DUP-EMP
// x <- ⊥
// y <- ⊥
Term reduce_dup_emp(Term dup, Term emp) {
  inc_itr();
  Loc dup_loc = term_loc(dup);
  sub(dup_loc + 0, emp);
  return emp;
}

// ! &L{x,y} = ⊤
// ------------- DUP-UNI
// x <- ⊤
// y <- ⊤
Term reduce_dup_uni(Term dup, Term uni) {
  inc_itr();
  Loc dup_loc = term_loc(dup);
  sub(dup_loc + 0, uni);
  return uni;
}

// ! &L{x,y} = U32
// -------------- DUP-U32
// x <- U32
// y <- U32
Term reduce_dup_u32(Term dup, Term u32) {
  inc_itr();
  Loc dup_loc = term_loc(dup);
  sub(dup_loc + 0, u32);
  return u32;
}

// ! &L{x,y} = ΣA.B
// ----------------- DUP-SIG
// ! &L{A0,A1} = A
// ! &L{B0,B1} = B
// x <- ΣA0.B0
// y <- ΣA1.B1
Term reduce_dup_sig(Term dup, Term sig) {
  inc_itr();
  Loc dup_loc = term_loc(dup);
  Lab dup_lab = term_lab(dup);
  Loc sig_loc = term_loc(sig);
  Term a = got(sig_loc + 0);
  Term b = got(sig_loc + 1);
  Loc loc = alloc_node(4);
  Loc sig1 = loc + 0;
  Loc dupa = loc + 2;
  Loc dupb = loc + 3;
  set(dupa + 0, a);
  set(dupb + 0, b);
  set(sig_loc + 0, term_new(DP0, dup_lab, dupa));
  set(sig_loc + 1, term_new(DP0, dup_lab, dupb));
  set(sig1 + 0, term_new(DP1, dup_lab, dupa));
  set(sig1 + 1, term_new(DP1, dup_lab, dupb));
  if (term_tag(dup) == DP0) {
    sub(dup_loc + 0, term_new(SIG, 0, sig1));
    return term_new(SIG, 0, sig_loc);
  } else {
    sub(dup_loc + 0, term_new(SIG, 0, sig_loc));
    return term_new(SIG, 0, sig1);
  }
}

// ! &L{x,y} = ΠA.B
// ----------------- DUP-ALL
// ! &L{A0,A1} = A
// ! &L{B0,B1} = B
// x <- ΠA0.B0
// y <- ΠA1.B1
Term reduce_dup_all(Term dup, Term all) {
  inc_itr();
  Loc dup_loc = term_loc(dup);
  Lab dup_lab = term_lab(dup);
  Loc all_loc = term_loc(all);
  Term a = got(all_loc + 0);
  Term b = got(all_loc + 1);
  Loc loc = alloc_node(4);
  Loc all1 = loc + 0;
  Loc dupa = loc + 2;
  Loc dupb = loc + 3;
  set(dupa + 0, a);
  set(dupb + 0, b);
  set(all_loc + 0, term_new(DP0, dup_lab, dupa));
  set(all_loc + 1, term_new(DP0, dup_lab, dupb));
  set(all1 + 0, term_new(DP1, dup_lab, dupa));
  set(all1 + 1, term_new(DP1, dup_lab, dupb));
  if (term_tag(dup) == DP0) {
    sub(dup_loc + 0, term_new(ALL, 0, all1));
    return term_new(ALL, 0, all_loc);
  } else {
    sub(dup_loc + 0, term_new(ALL, 0, all_loc));
    return term_new(ALL, 0, all1);
  }
}

// ! &L{x,y} = @foo(a b c ...)
// --------------------------- DUP-REF-COPY (when &L not in @foo)
// ! &L{a0,a1} = a
// ! &L{b0,b1} = b
// ! &L{c0,c1} = c
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
  Loc loc = alloc_node(ref_ari * 2);
  Loc ref0 = ref_loc;
  Loc ref1 = loc + 0;
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

// (<op * y)
// --------- OPX-ERA
// *
Term reduce_opx_era(Term opx, Term era) {
  //printf("reduce_opx_era "); print_term(opx); printf("\n");
  inc_itr();
  return era;
}

// (<op λx.f y)
// ----------- OPX-LAM
// error
Term reduce_opx_lam(Term opx, Term lam) {
  //printf("reduce_opx_lam "); print_term(opx); printf("\n");
  printf("invalid:opx-lam");
  exit(0);
}

// (<op &L{a,b} y)
// ------------------------- OPX-SUP
// ! &L{y0,y1} = y;
// &L{(<op a y0),(<op b y1)}
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
  Loc op1     = loc + 0;
  Loc du0     = loc + 2;
  set(op0 + 0, tm0);
  set(op0 + 1, term_new(DP0, sup_lab, du0));
  set(op1 + 0, tm1);
  set(op1 + 1, term_new(DP1, sup_lab, du0));
  set(du0 + 0, nmy);
  set(sup_loc + 0, term_new(OPX, term_lab(opx), op0));
  set(sup_loc + 1, term_new(OPX, term_lab(opx), op1));
  return term_new(SUP, sup_lab, sup_loc);
}

// (<op () y)
// ---------- OPX-NIL
// error
Term reduce_opx_nil(Term opx, Term nil) {
  printf("invalid:opx-nil");
  exit(0);
}

// (<op (x0,x1) y)
// --------------- OPX-TUP
// error
Term reduce_opx_tup(Term opx, Term tup) {
  printf("invalid:opx-tup");
  exit(0);
}

// (<op x y)
// --------- OPX-W32
// (>op y x)
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

// (<op T y)
// --------- OPX-TYP
// error
Term reduce_opx_typ(Term opx, Term typ) {
  printf("invalid:opx-typ");
  exit(0);
}

// (>op x *)
// --------- OPY-ERA
// *
Term reduce_opy_era(Term opy, Term era) {
  //printf("reduce_opy_era "); print_term(opy); printf("\n");
  inc_itr();
  return era;
}

// (>op x λy.f)
// ------------ OPY-LAM
// error
Term reduce_opy_lam(Term opy, Term lam) {
  //printf("reduce_opy_lam "); print_term(opy); printf("\n");
  printf("invalid:opy-lam");
  exit(0);
}

// (>op x &L{y0,y1})
// ----------------------- OPY-SUP
// &L{>op(x y0),>op(x y1)}
Term reduce_opy_sup(Term opy, Term sup) {
  //printf("reduce_opy_sup "); print_term(opy); printf("\n");
  inc_itr();
  Loc opy_loc = term_loc(opy);
  Loc sup_loc = term_loc(sup);
  Lab sup_lab = term_lab(sup);
  Term nmx    = got(opy_loc + 1);
  Term tm0    = got(sup_loc + 0);
  Term tm1    = got(sup_loc + 1);
  Loc op0     = opy_loc;
  Loc op1     = alloc_node(2);
  set(op0 + 0, nmx);
  set(op0 + 1, tm0);
  set(op1 + 0, nmx);
  set(op1 + 1, tm1);
  set(sup_loc + 0, term_new(OPY, term_lab(opy), op0));
  set(sup_loc + 1, term_new(OPY, term_lab(opy), op1));
  return term_new(SUP, sup_lab, sup_loc);
}

// (>op x ())
// ---------- OPY-NIL
// error
Term reduce_opy_nil(Term opy, Term nil) {
  printf("invalid:opy-nil");
  exit(0);
}

// (>op x (y0,y1))
// --------------- OPY-TUP
// error
Term reduce_opy_tup(Term opy, Term tup) {
  printf("invalid:opy-tup");
  exit(0);
}

// (>op x y)
// --------- OPY-W32
// x <op> y
Term reduce_opy_w32(Term opy, Term val) {
  //printf("reduce_opy_w32 "); print_term(opy); printf("\n");
  inc_itr();
  Loc opy_loc = term_loc(opy);
  Tag t = term_tag(val);
  u32 x = term_loc(got(opy_loc + 1));
  u32 y = term_loc(val);
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
  return term_new(W32, 0, result);
}

// (>op x T)
// --------- OPY-TYP
// error
Term reduce_opy_typ(Term opy, Term typ) {
  printf("invalid:opy-typ");
  exit(0);
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
      case USE:
      case GET:
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
          case NIL: next = reduce_app_nil(prev, next); continue;
          case TUP: next = reduce_app_tup(prev, next); continue;
          case W32: next = reduce_app_w32(prev, next); continue;
          case SET:
          case EMP:
          case UNI:
          case U32:
          case SIG:
          case ALL: next = reduce_app_typ(prev, next); continue;
          default: break;
        }
      }

      case USE: {
        switch (tag) {
          case ERA: next = reduce_use_era(prev, next); continue;
          case SUP: next = reduce_use_sup(prev, next); continue;
          case LAM: next = reduce_use_lam(prev, next); continue;
          case NIL: next = reduce_use_nil(prev, next); continue;
          case TUP: next = reduce_use_tup(prev, next); continue;
          case W32: next = reduce_use_w32(prev, next); continue;
          case SET:
          case EMP:
          case UNI:
          case U32:
          case SIG:
          case ALL: next = reduce_use_typ(prev, next); continue;
          default: break;
        }
      }

      case GET: {
        switch (tag) {
          case ERA: next = reduce_get_era(prev, next); continue;
          case SUP: next = reduce_get_sup(prev, next); continue;
          case NIL: next = reduce_get_nil(prev, next); continue;
          case TUP: next = reduce_get_tup(prev, next); continue;
          case LAM: next = reduce_get_lam(prev, next); continue;
          case W32: next = reduce_get_w32(prev, next); continue;
          case SET:
          case EMP:
          case UNI:
          case U32:
          case SIG:
          case ALL: next = reduce_get_typ(prev, next); continue;
          default: break;
        }
      }

      case SWI: {
        switch (tag) {
          case ERA: next = reduce_swi_era(prev, next); continue;
          case SUP: next = reduce_swi_sup(prev, next); continue;
          case NIL: next = reduce_swi_nil(prev, next); continue;
          case TUP: next = reduce_swi_tup(prev, next); continue;
          case LAM: next = reduce_swi_lam(prev, next); continue;
          case W32: next = reduce_swi_w32(prev, next); continue;
          case SET:
          case EMP:
          case UNI:
          case U32:
          case SIG:
          case ALL: next = reduce_swi_typ(prev, next); continue;
          default: break;
        }
      }

      case DP0:
      case DP1: {
        switch (tag) {
          case ERA: next = reduce_dup_era(prev, next); continue;
          case SUP: next = reduce_dup_sup(prev, next); continue;
          case LAM: next = reduce_dup_lam(prev, next); continue;
          case NIL: next = reduce_dup_nil(prev, next); continue;
          case TUP: next = reduce_dup_tup(prev, next); continue;
          case W32: next = reduce_dup_w32(prev, next); continue;
          case SET: next = reduce_dup_set(prev, next); continue;
          case EMP: next = reduce_dup_emp(prev, next); continue;
          case UNI: next = reduce_dup_uni(prev, next); continue;
          case U32: next = reduce_dup_u32(prev, next); continue;
          case SIG: next = reduce_dup_sig(prev, next); continue;
          case ALL: next = reduce_dup_all(prev, next); continue;
          case REF: next = reduce_dup_ref(prev, next); continue;
          default: break;
        }
      }

      case OPX: {
        switch (tag) {
          case ERA: next = reduce_opx_era(prev, next); continue;
          case LAM: next = reduce_opx_lam(prev, next); continue;
          case SUP: next = reduce_opx_sup(prev, next); continue;
          case NIL: next = reduce_opx_nil(prev, next); continue;
          case TUP: next = reduce_opx_tup(prev, next); continue;
          case W32: next = reduce_opx_w32(prev, next); continue;
          case SET:
          case EMP:
          case UNI:
          case U32:
          case SIG:
          case ALL: next = reduce_opx_typ(prev, next); continue;
          default: break;
        }
      }

      case OPY: {
        switch (tag) {
          case ERA: next = reduce_opy_era(prev, next); continue;
          case LAM: next = reduce_opy_lam(prev, next); continue;
          case SUP: next = reduce_opy_sup(prev, next); continue;
          case NIL: next = reduce_opy_nil(prev, next); continue;
          case TUP: next = reduce_opy_tup(prev, next); continue;
          case W32: next = reduce_opy_w32(prev, next); continue;
          case SET:
          case EMP:
          case UNI:
          case U32:
          case SIG:
          case ALL: next = reduce_opy_typ(prev, next); continue;
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
      set(loc + 0, bod);
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

    case TUP: {
      Term tm0 = got(loc + 0);
      Term tm1 = got(loc + 1);
      tm0 = normal(tm0);
      tm1 = normal(tm1);
      set(loc + 0, tm0);
      set(loc + 1, tm1);
      return wnf;
    }

    case SIG: {
      Term tm0 = got(loc + 0);
      Term tm1 = got(loc + 1);
      tm0 = normal(tm0);
      tm1 = normal(tm1);
      set(loc + 0, tm0);
      set(loc + 1, tm1);
      return wnf;
    }

    case ALL: {
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

    case USE:
    case GET:
    case SWI: {
      for (u64 i = 0; i <= 1; i++) {
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
  }
}

void hvm_define(u16 fid, Term (*func)()) {
  //printf("defined %llu %p\n", fid, func);
  HVM.book[fid] = func;
}

void hvm_set_fari(u16 fid, u16 arity) {
  HVM.fari[fid] = arity;
}
