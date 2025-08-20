// Shared runtime declarations for HVM C runtime
#pragma once

#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <unistd.h>
#include <time.h>
#include <stdbool.h>

// mmap portability helpers (e.g., macOS)
#ifndef MAP_ANONYMOUS
#  ifdef MAP_ANON
#    define MAP_ANONYMOUS MAP_ANON
#  else
#    define MAP_ANONYMOUS 0
#  endif
#endif
#ifndef MAP_NORESERVE
#  define MAP_NORESERVE 0
#endif

// Limits
#define MAX_HEAP_SIZE (1ULL << 40)
#define MAX_STACK_SIZE (1ULL << 28)

// Basic types
typedef uint8_t  Tag;
typedef uint16_t Lab; // 16-bit label (fits 65536 ctors/ops)
typedef uint64_t Loc; // up to 40-bit heap index in term payload
typedef uint64_t Term;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

// Constants (tags)
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

// Operators
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

// Builtin function ids
#define DUP_F 0xFFFF
#define SUP_F 0xFFFE
#define LOG_F 0xFFFD

// Let flavours
#define LAZY 0x0
#define STRI 0x1

#define VOID 0x00000000000000ULL

// Runtime State
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
  // Heatmap instrumentation (shared; used by both host and compiled runtimes)
  _Bool  heat_enabled;
  u32    heat_w, heat_h;
  u64    heat_itrs_max;   // total interactions from dry run
  u64    heat_mem_max;    // max heap size from dry run
  u32*   heat_reads;      // [heat_h][heat_w] row-major
  u32*   heat_writes;     // [heat_h][heat_w] row-major
} State;

// Global runtime state
extern State HVM;

// Heap controls
void set_len(u64 size);
void set_itr(u64 itrs);
u64  get_len();
u64  get_itr();
u64  fresh();

// Term helpers
Term term_new(Tag tag, Lab lab, Loc loc);
Tag  term_tag(Term x);
Lab  term_lab(Term x);
Loc  term_loc(Term x);
u64  term_get_bit(Term x);
Term term_set_bit(Term term);
Term term_rem_bit(Term term);
Term term_set_loc(Term x, Loc loc);
_Bool term_is_atom(Term term);

// Heap read/write
Term swap(Loc loc, Term term);
Term got(Loc loc);
void set(Loc loc, Term term);
void sub(Loc loc, Term term);
Term take(Loc loc);

// Allocation and accounting
Loc  alloc_node(Loc arity);
void inc_itr();

// Stack
void spush(Term term, Term* sbuf, u64* spos);
Term spop(Term* sbuf, u64* spos);

// Debug printing
void print_tag(Tag tag);
void print_term(Term term);
void print_heap();

// Reductions (public API)
Term reduce(Term term);
Term reduce_at(Loc host);
Term normal(Term term);

// Interaction functions
Term reduce_ref_sup(Term ref, u16 idx);
Term reduce_ref(Term ref);
Term reduce_let(Term let, Term val);

// APP
Term reduce_app_era(Term app, Term era);
Term reduce_app_lam(Term app, Term lam);
Term reduce_app_sup(Term app, Term sup);
Term reduce_app_ctr(Term app, Term ctr);
Term reduce_app_w32(Term app, Term w32);
Term reduce_app_una(Term app, Term una, Tag tag);
Term reduce_app_inc(Term app, Term inc);
Term reduce_app_dec(Term app, Term dec);

// DUP
Term reduce_dup_era(Term dup, Term era);
Term reduce_dup_lam(Term dup, Term lam);
Term reduce_dup_sup(Term dup, Term sup);
Term reduce_dup_ctr(Term dup, Term ctr);
Term reduce_dup_w32(Term dup, Term w32);
Term reduce_dup_ref(Term dup, Term ref);
Term reduce_dup_una(Term dup, Term una, Tag tag);
Term reduce_dup_inc(Term dup, Term inc);
Term reduce_dup_dec(Term dup, Term dec);

// MAT
Term reduce_mat_era(Term mat, Term era);
Term reduce_mat_lam(Term mat, Term lam);
Term reduce_mat_sup(Term mat, Term sup);
Term reduce_mat_ctr(Term mat, Term ctr);
Term reduce_mat_w32(Term mat, Term w32);
Term reduce_mat_una(Term mat, Term una, Tag tag);
Term reduce_mat_inc(Term mat, Term inc);
Term reduce_mat_dec(Term mat, Term dec);

// OPX
Term reduce_opx_era(Term opx, Term era);
Term reduce_opx_lam(Term opx, Term lam);
Term reduce_opx_sup(Term opx, Term sup);
Term reduce_opx_ctr(Term opx, Term ctr);
Term reduce_opx_w32(Term opx, Term nmx);
Term reduce_opx_una(Term opx, Term una, Tag tag);
Term reduce_opx_inc(Term opx, Term inc);
Term reduce_opx_dec(Term opx, Term dec);

// OPY
Term reduce_opy_era(Term opy, Term era);
Term reduce_opy_lam(Term opy, Term lam);
Term reduce_opy_sup(Term opy, Term sup);
Term reduce_opy_ctr(Term opy, Term ctr);
Term reduce_opy_w32(Term opy, Term w32);
Term reduce_opy_una(Term opy, Term una, Tag tag);
Term reduce_opy_inc(Term opy, Term inc);
Term reduce_opy_dec(Term opy, Term dec);

// Primitives
Term SUP_f(Term ref);
Term DUP_f(Term ref);
Term LOG_f(Term ref);

// Runtime memory API
void hvm_init();
void hvm_free();
State* hvm_get_state();
void hvm_set_state(State* hvm);
void hvm_define(u16 fid, Term (*func)());
void hvm_set_cari(u16 cid, u16 arity);
void hvm_set_fari(u16 fid, u16 arity);
void hvm_set_clen(u16 cid, u16 cases);
void hvm_set_cadt(u16 cid, u16 adt);

// Heatmap control/query
void heatmap_begin(double total_secs, u64 mem_max, u64 itrs_max, u32 w, u32 h);
void heatmap_end();
u32  heatmap_get_width();
u32  heatmap_get_height();
u32* heatmap_get_reads();
u32* heatmap_get_writes();
void heatmap_on_read(Loc loc);
void heatmap_on_write(Loc loc);
State* hvm_get_state();
void hvm_set_state(State* hvm);
void hvm_define(u16 fid, Term (*func)());
void hvm_set_cari(u16 cid, u16 arity);
void hvm_set_fari(u16 fid, u16 arity);
void hvm_set_clen(u16 cid, u16 cases);
void hvm_set_cadt(u16 cid, u16 adt);

// Heatmap control/query
void heatmap_begin(double total_secs, u64 mem_max, u64 itrs_max, u32 w, u32 h);
void heatmap_end();
u32  heatmap_get_width();
u32  heatmap_get_height();
u32* heatmap_get_reads();
u32* heatmap_get_writes();
void heatmap_on_read(Loc loc);
void heatmap_on_write(Loc loc);
