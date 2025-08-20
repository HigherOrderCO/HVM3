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
  HVM.interactions = alloc_huge(sizeof(Interactions));

  #define CHECK_ALLOC(ptr, name) if (!(ptr)) { printf(name " alloc failed\n"); allocs_failed++; }
  int allocs_failed = 0;
  CHECK_ALLOC(HVM.sbuf, "sbuf");
  CHECK_ALLOC(HVM.heap, "heap");
  CHECK_ALLOC(HVM.spos, "spos");
  CHECK_ALLOC(HVM.size, "size");
  CHECK_ALLOC(HVM.itrs, "itrs");
  CHECK_ALLOC(HVM.frsh, "frsh");
  CHECK_ALLOC(HVM.interactions, "interactions");
  if (allocs_failed > 0) {
    printf("hvm_init alloc's failed: %d allocations failed\n", allocs_failed);
    exit(1);
  }
  #undef CHECK_ALLOC

  *HVM.spos = 0;
  *HVM.size = 1;
  *HVM.itrs = 0;
  *HVM.frsh = 0x20;
  memset(HVM.interactions, 0, sizeof(Interactions));
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
    hvm_munmap(HVM.interactions, sizeof(Interactions), "interactions");
}

