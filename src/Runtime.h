// Runtime.h
#ifndef RUNTIME_H
#define RUNTIME_H

#include <inttypes.h>
#include <stdatomic.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <stdbool.h> // Added for _Bool

typedef uint8_t  Tag;
typedef uint32_t Lab;
typedef uint32_t Loc;
typedef uint64_t Term;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef _Atomic(Term) ATerm;

typedef struct {
    Term*  sbuf;  // reduction stack buffer
    u64*   spos;  // reduction stack position
    ATerm* heap;  // global node buffer
    u64*   size;  // global node buffer position
    u64*   itrs;  // interaction count
    u64*   frsh;  // fresh dup label count
    Term (*book[65536])(Term); // functions
    u16    cari[65536]; // arity of each constructor
    u16    clen[65536]; // case length of each constructor
    u16    cadt[65536]; // ADT id of each constructor
    u16    fari[65536]; // arity of each function
} State;

extern State HVM;

// Heap
u64 get_len();
u64 get_itr();
u64 fresh();

// Terms
Term term_new(Tag tag, Lab lab, Loc loc);
Tag term_tag(Term x);
Lab term_lab(Term x);
Loc term_loc(Term x);
u64 term_get_bit(Term x);
Term term_set_bit(Term term);
Term term_rem_bit(Term term);
Term term_set_loc(Term x, Loc loc);
_Bool term_is_atom(Term term);

// Atomics
Term swap(Loc loc, Term term);
Term got(Loc loc);
void set(Loc loc, Term term);
void sub(Loc loc, Term term);
Term take(Loc loc);

// Allocation
Loc alloc_node(Loc arity);
Loc inc_itr();

// Stack
void spush(Term term);
Term spop();

// Stringification
void print_tag(Tag tag);
void print_term(Term term);
void print_heap();

// Evaluation
Term reduce_ref_sup(Term ref, u16 idx);
Term reduce_ref(Term ref);
Term reduce_let(Term let, Term val);
Term reduce_app_era(Term app, Term era);
Term reduce_app_lam(Term app, Term lam);
Term reduce_app_sup(Term app, Term sup);
Term reduce_app_ctr(Term app, Term ctr);
Term reduce_app_w32(Term app, Term w32);
Term reduce_dup_era(Term dup, Term era);
Term reduce_dup_lam(Term dup, Term lam);
Term reduce_dup_sup(Term dup, Term sup);
Term reduce_dup_ctr(Term dup, Term ctr);
Term reduce_dup_w32(Term dup, Term w32);
Term reduce_dup_ref(Term dup, Term ref);
Term reduce_mat_era(Term mat, Term era);
Term reduce_mat_lam(Term mat, Term lam);
Term reduce_mat_sup(Term mat, Term sup);
Term reduce_mat_ctr(Term mat, Term ctr);
Term reduce_mat_w32(Term mat, Term w32);
Term reduce_opx_era(Term opx, Term era);
Term reduce_opx_lam(Term opx, Term lam);
Term reduce_opx_sup(Term opx, Term sup);
Term reduce_opx_ctr(Term opx, Term ctr);
Term reduce_opx_w32(Term opx, Term w32);
Term reduce_opy_era(Term opy, Term era);
Term reduce_opy_lam(Term opy, Term lam);
Term reduce_opy_sup(Term opy, Term sup);
Term reduce_opy_ctr(Term opy, Term ctr);
Term reduce_opy_w32(Term opy, Term w32);
Term reduce(Term term);
Term reduce_at(Loc host);
Term normal(Term term);

// Primitives
Term SUP_f(Term ref);
Term DUP_f(Term ref);
Term LOG_f(Term ref);

// Memory Management
void *alloc_huge(size_t size);
void hvm_init();
void hvm_free();
void hvm_munmap(void *ptr, size_t size, const char *name);
State* hvm_get_state();
void hvm_set_state(State* hvm);
void hvm_define(u16 fid, Term (*func)());
void hvm_set_cari(u16 cid, u16 arity);
void hvm_set_fari(u16 fid, u16 arity);
void hvm_set_clen(u16 cid, u16 cases);
void hvm_set_cadt(u16 cid, u16 adt);

// Constants
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
#define PARA 0x2

#define VOID 0x00000000000000

#endif
