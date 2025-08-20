#include "Runtime.h"

static void *alloc_huge(size_t size) {
    void *ptr = mmap(NULL, size, PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE,
                     -1, 0);
    if (ptr == MAP_FAILED) {
        perror("mmap failed");
        return NULL;
    }
    return ptr;
}

void hvm_init() {
  HVM.sbuf = alloc_huge(MAX_STACK_SIZE * sizeof(Term));
  HVM.heap = alloc_huge(MAX_HEAP_SIZE  * sizeof(Term));
  HVM.spos = alloc_huge(sizeof(u64));
  HVM.size = alloc_huge(sizeof(u64));
  HVM.itrs = alloc_huge(sizeof(u64));
  HVM.frsh = alloc_huge(sizeof(u64));

  #define CHECK_ALLOC(ptr, name) if (!(ptr)) { printf(name " alloc failed\n"); allocs_failed++; }
  int allocs_failed = 0;
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

  // Heatmap defaults
  HVM.heat_enabled = 0;
  HVM.heat_w = 0;
  HVM.heat_h = 0;
  HVM.heat_mem_max = 0;
  HVM.heat_itrs_max = 0;
  HVM.heat_reads = NULL;
  HVM.heat_writes = NULL;
}

static void hvm_munmap(void *ptr, size_t size, const char *name) {
    if (ptr != MAP_FAILED) {
        if (munmap(ptr, size) == -1) {
            perror("munmap failed");
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

    if (HVM.heat_reads) { free(HVM.heat_reads); HVM.heat_reads = NULL; }
    if (HVM.heat_writes) { free(HVM.heat_writes); HVM.heat_writes = NULL; }
    HVM.heat_enabled = 0;
}

void heatmap_begin(double total_secs, u64 mem_max, u64 itrs_max, u32 w, u32 h) {
  (void)total_secs; // Y-axis uses interactions only
  if (HVM.heat_reads) { free(HVM.heat_reads); HVM.heat_reads = NULL; }
  if (HVM.heat_writes) { free(HVM.heat_writes); HVM.heat_writes = NULL; }
  HVM.heat_w = w;
  HVM.heat_h = h;
  HVM.heat_itrs_max = itrs_max;
  HVM.heat_mem_max  = mem_max;
  size_t count = (size_t)w * (size_t)h;
  HVM.heat_reads  = (u32*)calloc(count, sizeof(u32));
  HVM.heat_writes = (u32*)calloc(count, sizeof(u32));
  HVM.heat_enabled = (HVM.heat_reads && HVM.heat_writes);
}

void heatmap_end() {
  HVM.heat_enabled = 0;
}

u32 heatmap_get_width()  { return HVM.heat_w; }
u32 heatmap_get_height() { return HVM.heat_h; }
u32* heatmap_get_reads()  { return HVM.heat_reads; }
u32* heatmap_get_writes() { return HVM.heat_writes; }

static inline u32 map_x(Loc loc) {
  if (HVM.heat_mem_max == 0 || HVM.heat_w <= 1) return 0;
  u64 den = HVM.heat_mem_max - 1; // map [0..den] -> [0..W-1]
  if (den == 0) return 0;
  if (loc > den) loc = den;
  u64 num = loc * (u64)(HVM.heat_w - 1);
  // round-half-up to avoid systematic underfill at bin edges
  return (u32)((num + den/2) / den);
}

static inline u32 map_y() {
  if (HVM.heat_itrs_max == 0 || HVM.heat_h <= 1) return 0;
  u64 itrs = *HVM.itrs;
  u64 den  = HVM.heat_itrs_max - 1; // map [0..den] -> [0..H-1]
  if (den == 0) return 0;
  if (itrs > den) itrs = den;
  u64 num = itrs * (u64)(HVM.heat_h - 1);
  // round-half-up to reduce missed early bins on the diagonal
  return (u32)((num + den/2) / den);
}

void heatmap_on_read(Loc loc) {
  if (!HVM.heat_enabled) return;
  if (!HVM.heat_reads || !HVM.heat_writes) return;
  u32 x = map_x(loc);
  u32 y = map_y();
  size_t idx = (size_t)y * (size_t)HVM.heat_w + (size_t)x;
  HVM.heat_reads[idx] += 1;
}

void heatmap_on_write(Loc loc) {
  if (!HVM.heat_enabled) return;
  if (!HVM.heat_reads || !HVM.heat_writes) return;
  u32 x = map_x(loc);
  u32 y = map_y();
  size_t idx = (size_t)y * (size_t)HVM.heat_w + (size_t)x;
  HVM.heat_writes[idx] += 1;
}
