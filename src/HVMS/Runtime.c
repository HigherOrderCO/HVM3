// HVM3 Core: single-thread, polarized, LAM/APP & DUP/SUP only

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdatomic.h>
#include <string.h>
#include <time.h>

typedef uint8_t  Tag;  //  8 bits
typedef uint32_t Lab;  // 24 bits
typedef uint32_t Loc;  // 32 bits
typedef uint64_t Term; // Loc | Lab | Tag
typedef uint32_t u32;
typedef uint64_t u64;
typedef int32_t  i32;
typedef float    f32;

// Constants
#define VAR 0x01
#define SUB 0x02
#define NUL 0x03
#define ERA 0x04
#define LAM 0x05
#define APP 0x06
#define SUP 0x07
#define DUP 0x08
#define REF 0x09
#define OPX 0x0A
#define OPY 0x0B
#define U32 0x0C
#define I32 0x0D
#define F32 0x0E
#define MAT 0x0F

const Term VOID = 0;

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

// Types
typedef uint64_t u64;
typedef _Atomic(u64) a64;

// Global heap
static a64* BUFF     = NULL;
static u64  RNOD_INI = 0;
static u64  RNOD_END = 0;
static u64  RBAG     = 0x1000;
static u64  RBAG_INI = 0;
static u64  RBAG_END = 0;

// Global book
typedef struct Def {
  char* name;
  Term* nodes;
  u64 nodes_len;
  Term* rbag;
  u64 rbag_len;
} Def;

typedef struct Book {
  Def* defs;
  u32 len;
  u32 cap;
} Book;

static Book BOOK = {
  .defs = NULL,
  .len = 0,
  .cap = 0,
};

// Debugging
static char* tag_to_str(Tag tag);
void dump_buff();

// Term operations
Term term_new(Tag tag, Lab lab, Loc loc) {
  Term tag_enc = tag;
  Term lab_enc = ((Term)lab) << 8;
  Term loc_enc = ((Term)loc) << 32;

  return loc_enc | lab_enc | tag_enc;
}

Tag term_tag(Term term) {
  return term & 0xFF;
}

Lab term_lab(Term term) {
  return (term >> 8) & 0xFFFFFF;
}

Loc term_loc(Term term) {
  return (term >> 32) & 0xFFFFFFFF;
}

Term term_offset_loc(Term term, Loc offset) {
  // do not offset terms that use loc for something other than
  // indices into the global buffer.
  switch (term_tag(term)) {
    case SUB:
    case NUL:
    case ERA:
    case REF:
    case U32:
      return term;
  }

  Term tag = term_tag(term);
  Term lab = term_lab(term);
  Term loc = term_loc(term) + offset;

  return term_new(tag, lab, loc);
}

// Memory operations
Term swap(Loc loc, Term term) {
  return atomic_exchange_explicit(&BUFF[loc], term, memory_order_relaxed);
}

Term get(Loc loc) {
  return atomic_load_explicit(&BUFF[loc], memory_order_relaxed);
}

Term take(Loc loc) {
  return atomic_exchange_explicit(&BUFF[loc], VOID, memory_order_relaxed);
}

void set(Loc loc, Term term) {
  atomic_store_explicit(&BUFF[loc], term, memory_order_relaxed);
}

Loc port(u64 n, Loc x) {
  return n + x - 1;
}

// Allocation
Loc alloc_node(u64 arity) {
  Loc loc = RNOD_END;
  RNOD_END += arity;
  return loc;
}

u64 inc_itr() {
  return RBAG_END / 2;
}

Loc rbag_push(Term neg, Term pos) {
  Loc loc = RBAG + RBAG_END;
  RBAG_END += 2;
  set(loc + 0, neg);
  set(loc + 1, pos);
  return loc;
}

Loc rbag_pop() {
  if (RBAG_INI < RBAG_END) {
    Loc loc = RBAG + RBAG_INI;
    RBAG_INI += 2;
    return loc;
  }

  return 0;
}

Loc rbag_ini() {
  return RBAG + RBAG_INI;
}

Loc rbag_end() {
  return RBAG + RBAG_END;
}

Loc rnod_end() {
  return RNOD_END;
}

// Book operations

// Moves the global buffer and redex bag into a new def and resets
// the global buffer and redex bag.
void def_new(char* name) {
  if (BOOK.len == BOOK.cap) {
    if (BOOK.cap == 0) {
      BOOK.cap = 32;
    } else {
      BOOK.cap *= 2;
    }

    BOOK.defs = realloc(BOOK.defs, sizeof(Def) * BOOK.cap);
  }

  Def def = {
    .name = name,
    .nodes = calloc(RNOD_END, sizeof(Term)),
    .nodes_len = RNOD_END,
    .rbag = calloc(RBAG_END, sizeof(Term)),
    .rbag_len = RBAG_END,
  };

  memcpy(def.nodes, BUFF, sizeof(Term) * def.nodes_len);
  memcpy(def.rbag, BUFF + RBAG, sizeof(Term) * def.rbag_len);

  // printf("NEW DEF '%s':\n", def.name);
  // dump_buff();
  // printf("\n");

  memset(BUFF, 0, sizeof(Term) * def.nodes_len);
  memset(BUFF + RBAG, 0, sizeof(Term) * def.rbag_len);

  RNOD_END = 0;
  RBAG_END = 0;

  BOOK.defs[BOOK.len] = def;
  BOOK.len++;
}

char* def_name(Loc def_idx) {
  return BOOK.defs[def_idx].name;
}

// Expands a ref's data into a linear block of nodes with its nodes' locs
// offset by the index where in the BUFF it was expanded.
//
// Returns the ref's root, the first node in its data.
Term expand_ref(Loc def_idx) {
  if (RNOD_END == 0) {
    printf("expand_ref: empty BUFF\n");
    exit(1);
  }

  Def def = BOOK.defs[def_idx];

  Loc offset = RNOD_END - 1;
  RNOD_END += def.nodes_len - 1;

  // insert non-root nodes
  Term root = term_offset_loc(def.nodes[0], offset);
  for (u32 i = 1; i < def.nodes_len; i++) {
    set(i + offset, term_offset_loc(def.nodes[i], offset));
  }

  // insert redexes
  for (u32 i = 0; i < def.rbag_len; i += 2) {
    rbag_push(term_offset_loc(def.rbag[i], offset), term_offset_loc(def.rbag[i + 1], offset));
  }

  return root;
}


// Atomic Linker
static void move(Loc neg_loc, u64 pos);

static void link(Term neg, Term pos) {
  if (term_tag(pos) == VAR) {
    Term far = swap(term_loc(pos), neg);
    if (term_tag(far) != SUB) {
      move(term_loc(pos), far);
    }
  } else {
    rbag_push(neg, pos);
  }
}

static void move(Loc neg_loc, Term pos) {
  Term neg = swap(neg_loc, pos);
  if (term_tag(neg) != SUB) {
    take(neg_loc);
    link(neg, pos);
  }
}

// Interactions
static void interact_applam(Loc a_loc, Loc b_loc) {
  Term arg = take(port(1, a_loc));
  Loc  ret = port(2, a_loc);
  Loc  var = port(1, b_loc);
  Term bod = take(port(2, b_loc));
  move(var, arg);
  move(ret, bod);
}

static void interact_appsup(Loc a_loc, Loc b_loc) {
  Term arg = take(port(1, a_loc));
  Loc  ret = port(2, a_loc);
  Term tm1 = take(port(1, b_loc));
  Term tm2 = take(port(2, b_loc));
  Loc  dp1 = alloc_node(2);
  Loc  dp2 = alloc_node(2);
  Loc  cn1 = alloc_node(2);
  Loc  cn2 = alloc_node(2);
  set(port(1, dp1), term_new(SUB, 0, 0));
  set(port(2, dp1), term_new(SUB, 0, 0));
  set(port(1, dp2), term_new(VAR, 0, port(2, cn1)));
  set(port(2, dp2), term_new(VAR, 0, port(2, cn2)));
  set(port(1, cn1), term_new(VAR, 0, port(1, dp1)));
  set(port(2, cn1), term_new(SUB, 0, 0));
  set(port(1, cn2), term_new(VAR, 0, port(2, dp1)));
  set(port(2, cn2), term_new(SUB, 0, 0));
  link(term_new(DUP, 0, dp1), arg);
  move(ret, term_new(SUP, 0, dp2));
  link(term_new(APP, 0, cn1), tm1);
  link(term_new(APP, 0, cn2), tm2);
}

static void interact_appnul(Loc a_loc) {
  Term arg = take(port(1, a_loc));
  Loc  ret = port(2, a_loc);
  link(term_new(ERA, 0, 0), arg);
  move(ret, term_new(NUL, 0, 0));
}

static void interact_appu32(Loc a_loc, u32 num) {
  Term arg = take(port(1, a_loc));
  Loc  ret = port(2, a_loc);
  link(term_new(U32, 0, num), arg);
  move(ret, term_new(U32, 0, num));
}

static void interact_opxnul(Loc a_loc) {
  Term arg = take(port(1, a_loc));
  Loc  ret = port(2, a_loc);
  link(term_new(ERA, 0, 0), arg);
  move(ret, term_new(NUL, 0, 0));
}

static void interact_opxnum(Loc a_loc, Lab op, u32 num, Tag num_type) {
  Term arg = swap(port(1, a_loc), term_new(num_type, 0, num));
  link(term_new(OPY, op, a_loc), arg);
}

static void interact_opxsup(Loc a_loc, Lab op, Loc b_loc) {
  Term arg = take(port(1, a_loc));
  Loc  ret = port(2, a_loc);
  Term tm1 = take(port(1, b_loc));
  Term tm2 = take(port(2, b_loc));
  Loc  dp1 = alloc_node(2);
  Loc  dp2 = alloc_node(2);
  Loc  cn1 = alloc_node(2);
  Loc  cn2 = alloc_node(2);
  set(port(1, dp1), term_new(SUB, 0, 0));
  set(port(2, dp1), term_new(SUB, 0, 0));
  set(port(1, dp2), term_new(VAR, 0, port(2, cn1)));
  set(port(2, dp2), term_new(VAR, 0, port(2, cn2)));
  set(port(1, cn1), term_new(VAR, 0, port(1, dp1)));
  set(port(2, cn1), term_new(SUB, 0, 0));
  set(port(1, cn2), term_new(VAR, 0, port(2, dp1)));
  set(port(2, cn2), term_new(SUB, 0, 0));
  link(term_new(DUP, 0, dp1), arg);
  move(ret, term_new(SUP, 0, dp2));
  link(term_new(OPX, op, cn1), tm1);
  link(term_new(OPX, op, cn2), tm2);
}

static void interact_opynul(Loc a_loc) {
  Term arg = take(port(1, a_loc));
  Loc  ret = port(2, a_loc);
  link(term_new(ERA, 0, 0), arg);
  move(ret, term_new(NUL, 0, 0));
}


// Utilities
u32 u32_to_u32(u32 u) { return         u; }
i32 u32_to_i32(u32 u) { return *(i32*)&u; }
f32 u32_to_f32(u32 u) { return *(f32*)&u; }
u32 i32_to_u32(i32 i) { return *(u32*)&i; }
u32 f32_to_u32(f32 f) { return *(u32*)&f; }

static void interact_opynum(Loc a_loc, Lab op, u32 y, Tag y_type) {
  #define CASES_u32(a, b)                     \
            case OP_MOD: val = a %  b; break; \
            case OP_AND: val = a &  b; break; \
            case OP_OR : val = a |  b; break; \
            case OP_XOR: val = a ^  b; break; \
            case OP_LSH: val = a << b; break; \
            case OP_RSH: val = a >> b; break;
  #define CASES_i32(a, b) CASES_u32(a, b)
  #define CASES_f32(a, b)

  #define PERFORM_OP(x, y, op, type)          \
    {                                         \
        type val;                             \
        type a = u32_to_##type(x);            \
        type b = u32_to_##type(y);            \
        switch (op) {                         \
            case OP_ADD: val = a +  b; break; \
            case OP_SUB: val = a -  b; break; \
            case OP_MUL: val = a *  b; break; \
            case OP_DIV: val = a /  b; break; \
            case OP_EQ : val = a == b; break; \
            case OP_NE : val = a != b; break; \
            case OP_LT : val = a <  b; break; \
            case OP_GT : val = a >  b; break; \
            case OP_LTE: val = a <= b; break; \
            case OP_GTE: val = a >= b; break; \
            CASES_##type(x, y)                \
        }                                     \
        res = type##_to_u32(val);             \
    }

  u32 x   = term_loc(take(port(1, a_loc)));
  Loc ret = port(2, a_loc);
  u32 res;

  switch (y_type) {
    case U32: PERFORM_OP(x, y, op, u32); break;
    case I32: PERFORM_OP(x, y, op, i32); break;
    case F32: PERFORM_OP(x, y, op, f32); break;
  }

  move(ret, term_new(y_type, 0, res));
}

static void interact_opysup(Loc a_loc, Loc b_loc) {
  Term arg = take(port(1, a_loc));
  Loc  ret = port(2, a_loc);
  Term tm1 = take(port(1, b_loc));
  Term tm2 = take(port(2, b_loc));
  Loc  dp1 = alloc_node(2);
  Loc  dp2 = alloc_node(2);
  Loc  cn1 = alloc_node(2);
  Loc  cn2 = alloc_node(2);
  set(port(1, dp1), term_new(SUB, 0, 0));
  set(port(2, dp1), term_new(SUB, 0, 0));
  set(port(1, dp2), term_new(VAR, 0, port(2, cn1)));
  set(port(2, dp2), term_new(VAR, 0, port(2, cn2)));
  set(port(1, cn1), term_new(VAR, 0, port(1, dp1)));
  set(port(2, cn1), term_new(SUB, 0, 0));
  set(port(1, cn2), term_new(VAR, 0, port(2, dp1)));
  set(port(2, cn2), term_new(SUB, 0, 0));
  link(term_new(DUP, 0, dp1), arg);
  move(ret, term_new(SUP, 0, dp2));
  link(term_new(OPY, 0, cn1), tm1);
  link(term_new(OPY, 0, cn2), tm2);
}


static void interact_dupsup(Loc a_loc, Loc b_loc) {
  Loc  dp1 = port(1, a_loc);
  Loc  dp2 = port(2, a_loc);
  Term tm1 = take(port(1, b_loc));
  Term tm2 = take(port(2, b_loc));
  move(dp1, tm1);
  move(dp2, tm2);
}

static void interact_duplam(Loc a_loc, Loc b_loc) {
  Loc  dp1 = port(1, a_loc);
  Loc  dp2 = port(2, a_loc);
  Loc  var = port(1, b_loc);
  // TODO(enricozb): why is this the only take?
  Term bod = take(port(2, b_loc));
  Loc  co1 = alloc_node(2);
  Loc  co2 = alloc_node(2);
  Loc  du1 = alloc_node(2);
  Loc  du2 = alloc_node(2);
  set(port(1, co1), term_new(SUB, 0, 0));
  set(port(2, co1), term_new(VAR, 0, port(1, du2)));
  set(port(1, co2), term_new(SUB, 0, 0));
  set(port(2, co2), term_new(VAR, 0, port(2, du2)));
  set(port(1, du1), term_new(VAR, 0, port(1, co1)));
  set(port(2, du1), term_new(VAR, 0, port(1, co2)));
  set(port(1, du2), term_new(SUB, 0, 0));
  set(port(2, du2), term_new(SUB, 0, 0));
  move(dp1, term_new(LAM, 0, co1));
  move(dp2, term_new(LAM, 0, co2));
  move(var, term_new(SUP, 0, du1));
  link(term_new(DUP, 0, du2), bod);
}

static void interact_dupnul(Loc a_loc) {
  Loc dp1 = port(1, a_loc);
  Loc dp2 = port(2, a_loc);
  move(dp1, term_new(NUL, 0, a_loc));
  move(dp2, term_new(NUL, 0, a_loc));
}

static void interact_dupnum(Loc a_loc, u32 n, Tag n_type) {
  Loc dp1 = port(1, a_loc);
  Loc dp2 = port(2, a_loc);
  move(dp1, term_new(n_type, 0, n));
  move(dp2, term_new(n_type, 0, n));
}

static void interact_dupref(Loc a_loc, Loc b_loc) {
  move(port(1, a_loc), term_new(REF, 0, b_loc));
  move(port(2, a_loc), term_new(REF, 0, b_loc));
}

static void interact_matnul(Loc a_loc, Lab mat_len) {
  move(port(1, a_loc), term_new(NUL, 0, 0));
  for (u32 i = 0; i < mat_len; i++) {
    link(term_new(ERA, 0, 0), take(port(i + 2, a_loc)));
  }
}

static void interact_matnum(Loc mat_loc, Lab mat_len, u32 n, Tag n_type) {
  if (n_type != U32) {
    printf("match with non-U32\n");
    exit(1);
  }

  u32 i_arm = (n < mat_len - 1) ? n : (mat_len - 1);
  for (u32 i = 0; i < mat_len; i++) {
    if (i != i_arm) {
      link(term_new(ERA, 0, 0), take(port(2 + i, mat_loc)));
    }
  }

  Loc ret = port(1, mat_loc);
  Term arm = take(port(2 + i_arm, mat_loc));

  if (i_arm < mat_len - 1) {
    move(ret, arm);
  } else {
    Loc app = alloc_node(2);
    set(app + 0, term_new(U32, 0, n - (mat_len - 1)));
    set(app + 1, term_new(SUB, 0, 0));
    move(ret, term_new(VAR, 0, port(2, app)));

    link(term_new(APP, 0, app), arm);
  }
}

static void interact_matsup(Loc mat_loc, Lab mat_len, Loc sup_loc) {
  Loc ma0 = alloc_node(1 + mat_len);
  Loc ma1 = alloc_node(1 + mat_len);

  Loc sup = alloc_node(2);

  set(port(1, sup), term_new(VAR, 0, port(1, ma1)));
  set(port(2, sup), term_new(VAR, 0, port(1, ma0)));
  set(port(1, ma0), term_new(SUB, 0, 0));
  set(port(1, ma1), term_new(SUB, 0, 0));

  for (u64 i = 0; i < mat_len; i++) {
    Loc dui = alloc_node(2);
    set(port(1, dui),     term_new(SUB, 0, 0));
    set(port(2, dui),     term_new(SUB, 0, 0));
    set(port(2 + i, ma0), term_new(VAR, 0, port(2, dui)));
    set(port(2 + i, ma1), term_new(VAR, 0, port(1, dui)));

    link(term_new(DUP, 0, dui), take(port(2 + i, mat_loc)));
  }

  move(port(1, mat_loc), term_new(SUP, 0, sup));
  link(term_new(MAT, mat_len, ma0), take(port(2, sup_loc)));
  link(term_new(MAT, mat_len, ma1), take(port(1, sup_loc)));
}


static void interact_eralam(Loc b_loc) {
  Loc  var = port(1, b_loc);
  Term bod = take(port(2, b_loc));
  move(var, term_new(NUL, 0, 0));
  link(term_new(ERA, 0, 0), bod);
}

static void interact_erasup(Loc b_loc) {
  Term tm1 = take(port(1, b_loc));
  Term tm2 = take(port(2, b_loc));
  link(term_new(ERA, 0, 0), tm1);
  link(term_new(ERA, 0, 0), tm2);
}

static char* tag_to_str(Tag tag);

static void interact(Term neg, Term pos) {
  Tag neg_tag = term_tag(neg);
  Tag pos_tag = term_tag(pos);
  Loc neg_loc = term_loc(neg);
  Loc pos_loc = term_loc(pos);

  switch (neg_tag) {
    case APP:
      switch (pos_tag) {
        case LAM: interact_applam(neg_loc, pos_loc); break;
        case NUL: interact_appnul(neg_loc); break;
        case U32: interact_appu32(neg_loc, pos_loc); break;
        case REF: link(neg, expand_ref(pos_loc)); break;
        case SUP: interact_appsup(neg_loc, pos_loc); break;
      }
      break;
    case OPX:
      switch (pos_tag) {
        case LAM: break;
        case NUL: interact_opxnul(neg_loc); break;
        case U32:
        case I32:
        case F32: interact_opxnum(neg_loc, term_lab(neg), pos_loc, pos_tag); break;
        case REF: link(neg, expand_ref(pos_loc)); break;
        case SUP: interact_opxsup(neg_loc, term_lab(neg), pos_loc); break;
      }
      break;
    case OPY:
      switch (pos_tag) {
        case LAM: break;
        case NUL: interact_opynul(neg_loc); break;
        case U32:
        case I32:
        case F32: interact_opynum(neg_loc, term_lab(neg), pos_loc, pos_tag); break;
        case REF: link(neg, expand_ref(pos_loc)); break;
        case SUP: interact_opysup(neg_loc, pos_loc); break;
      }
      break;
    case DUP:
      switch (pos_tag) {
        case LAM: interact_duplam(neg_loc, pos_loc); break;
        case NUL: interact_dupnul(neg_loc); break;
        case U32:
        case I32:
        case F32: interact_dupnum(neg_loc, pos_loc, pos_tag); break;
        // TODO(enricozb): dup-ref optimization
        case REF: interact_dupref(neg_loc, pos_loc); break;
        // case REF: link(neg, expand_ref(pos_loc)); break;
        case SUP: interact_dupsup(neg_loc, pos_loc); break;
      }
      break;
    case MAT:
      switch (pos_tag) {
        case LAM: break;
        case NUL: interact_matnul(neg_loc, term_lab(neg)); break;
        case U32:
        case I32:
        case F32: interact_matnum(neg_loc, term_lab(neg), pos_loc, pos_tag); break;
        case REF: link(neg, expand_ref(pos_loc)); break;
        case SUP: interact_matsup(neg_loc, term_lab(neg), pos_loc); break;
      }
      break;
    case ERA:
      switch (pos_tag) {
        case LAM: interact_eralam(pos_loc); break;
        case NUL: break;
        case U32: break;
        case REF: break;
        case SUP: interact_erasup(pos_loc); break;
      }
      break;
  }
}

// Evaluation
static int normal_step() {
  // dump_buff();

  Loc loc = rbag_pop();
  if (loc == 0) {
    // dump_buff();

    return 0;
  }

  Term neg = take(loc + 0);
  Term pos = take(loc + 1);

  // printf("\n\n%04lX: INTERACT %s ~ %s\n\n", inc_itr(), tag_to_str(neg), tag_to_str(pos));

  interact(neg, pos);

  return 1;
}

// FFI exports
void hvm_init() {
  if (BUFF == NULL) {
    BUFF = calloc((1ULL << 24), sizeof(a64));
  }
  RNOD_INI = 0;
  RNOD_END = 0;
  RBAG_INI = 0;
  RBAG_END = 0;
}

void hvm_free() {
  if (BUFF != NULL) {
    free(BUFF);
    BUFF = NULL;
  }
}

void boot(Loc def_idx) {
  if (RNOD_END || RBAG_END) {
    printf("booting on non-empty state\n");
    exit(1);
  }

  Loc root = alloc_node(1);
  set(root, expand_ref(def_idx));
}

Term normalize(Term term) {
  if (term_tag(term) != REF) {
    printf("normalizing non-ref\n");
    exit(1);
  }

  boot(term_loc(term));

  while (normal_step());

  return get(0);
}

// Debugging
static char* tag_to_str(Tag tag) {
  switch (tag) {
    case VOID: return "___";
    case VAR:  return "VAR";
    case SUB:  return "SUB";
    case NUL:  return "NUL";
    case ERA:  return "ERA";
    case LAM:  return "LAM";
    case APP:  return "APP";
    case SUP:  return "SUP";
    case DUP:  return "DUP";
    case REF:  return "REF";
    case OPX:  return "OPX";
    case OPY:  return "OPY";
    case U32:  return "U32";
    case I32:  return "I32";
    case F32:  return "F32";
    case MAT:  return "MAT";

    default:   return "???";
  }
}

void dump_buff() {
  printf("------------------\n");
  printf("      NODES\n");
  printf("ADDR   LOC LAB TAG\n");
  printf("------------------\n");
  for (Loc loc = RNOD_INI; loc < RNOD_END; loc++) {
    Term term = get(loc);
    Loc t_loc = term_loc(term);
    Lab t_lab = term_lab(term);
    Tag t_tag = term_tag(term);
    printf("%06X %03X %03X %s\n", loc, term_loc(term), term_lab(term), tag_to_str(term_tag(term)));
  }
  printf("------------------\n");
  printf("    REDEX BAG\n");
  printf("ADDR   LOC LAB TAG\n");
  printf("------------------\n");
  for (Loc loc = RBAG + RBAG_INI; loc < RBAG + RBAG_END; loc++) {
    Term term = get(loc);
    Loc t_loc = term_loc(term);
    Lab t_lab = term_lab(term);
    Tag t_tag = term_tag(term);
    printf("%06X %03X %03X %s\n", loc, term_loc(term), term_lab(term), tag_to_str(term_tag(term)));
  }
  printf("------------------\n");
}
