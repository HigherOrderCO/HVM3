#include "Runtime.h"

// Single translation unit aggregator for the C runtime.
// Keeping hot paths in one TU restores inlining and performance.

// Core state and memory
#include "runtime/state.c"
#include "runtime/memory.c"

// Heap, terms, stack, and debug printing
#include "runtime/heap.c"
#include "runtime/term.c"
#include "runtime/stack.c"
#include "runtime/print.c"

// Reductions dispatcher and helpers
#include "runtime/reduce.c"

// Interaction rules, grouped by tag
#include "runtime/reduce/let.c"

// APP
#include "runtime/reduce/app_era.c"
#include "runtime/reduce/app_lam.c"
#include "runtime/reduce/app_sup.c"
#include "runtime/reduce/app_ctr.c"
#include "runtime/reduce/app_w32.c"
#include "runtime/reduce/app_una.c"

// DUP
#include "runtime/reduce/dup_era.c"
#include "runtime/reduce/dup_lam.c"
#include "runtime/reduce/dup_sup.c"
#include "runtime/reduce/dup_ctr.c"
#include "runtime/reduce/dup_w32.c"
#include "runtime/reduce/dup_ref.c"
#include "runtime/reduce/dup_una.c"

// MAT / IFL / SWI
#include "runtime/reduce/mat_era.c"
#include "runtime/reduce/mat_lam.c"
#include "runtime/reduce/mat_sup.c"
#include "runtime/reduce/mat_ctr.c"
#include "runtime/reduce/mat_w32.c"
#include "runtime/reduce/mat_una.c"

// OPX/OPY
#include "runtime/reduce/opx_era.c"
#include "runtime/reduce/opx_lam.c"
#include "runtime/reduce/opx_sup.c"
#include "runtime/reduce/opx_ctr.c"
#include "runtime/reduce/opx_w32.c"
#include "runtime/reduce/opx_una.c"
#include "runtime/reduce/opy_era.c"
#include "runtime/reduce/opy_lam.c"
#include "runtime/reduce/opy_sup.c"
#include "runtime/reduce/opy_ctr.c"
#include "runtime/reduce/opy_w32.c"
#include "runtime/reduce/opy_una.c"

// Primitives
#include "runtime/prim/SUP.c"
#include "runtime/prim/DUP.c"
#include "runtime/prim/LOG.c"
