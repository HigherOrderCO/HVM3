#include "Runtime.h"
#include <stdint.h>
#include <stdlib.h>
#include <math.h>

// Memory access heatmap tracker and minimal PNG writer
// Produces a 1280x1280 PNG where X = heap index bucket, Y = time buckets

#define MH_W 1280
#define MH_H 1280

static inline uint32_t* MH_READS_AT(uint32_t y, uint32_t x) {
  return &HVM.mh_reads[(uint64_t)y * MH_W + x];
}
static inline uint32_t* MH_WRITES_AT(uint32_t y, uint32_t x) {
  return &HVM.mh_writes[(uint64_t)y * MH_W + x];
}

static inline u64 mh_now_ns() {
  struct timespec ts; clock_gettime(CLOCK_MONOTONIC, &ts);
  return (u64)ts.tv_sec * 1000000000ULL + (u64)ts.tv_nsec;
}

void memheat_enable(const char* path) {
  if (!path) return;
  strncpy(HVM.mh_path, path, 1023);
  HVM.mh_path[1023] = '\0';
  *HVM.mh_enabled = 1;
  if (HVM.mh_path[0] == '\0') {
    strcpy(HVM.mh_path, "mem-heat.png");
  }
  // init time origin
  u64 ns = mh_now_ns();
  *HVM.mh_t0_sec = ns / 1000000000ULL;
  *HVM.mh_t0_nsec = ns % 1000000000ULL;
}

static inline void mh_touch(Loc loc, int is_write) {
  if (!HVM.mh_enabled) return;
  if (!*HVM.mh_enabled) return;
  if (!HVM.mh_reads || !HVM.mh_writes || !HVM.mh_access_count || !HVM.mh_access_per_row) return;
  // Use configured max_len and window; ignore out-of-range
  uint64_t denom = *HVM.mh_max_len; if (denom == 0) denom = 1;
  if (loc >= denom) return;
  uint64_t col = (uint64_t)((__uint128_t)loc * MH_W / denom);
  if (col >= MH_W) return;
  uint64_t row;
  if (*HVM.mh_ns_window) {
    u64 now = mh_now_ns();
    u64 t0 = (*HVM.mh_t0_sec) * 1000000000ULL + (*HVM.mh_t0_nsec);
    u64 dns = now >= t0 ? (now - t0) : 0;
    if (dns >= *HVM.mh_ns_window) return;
    row = (uint64_t)((__uint128_t)dns * MH_H / (*HVM.mh_ns_window));
  } else {
    uint64_t per_row = *HVM.mh_access_per_row;
    if (per_row == 0) {
      per_row = (denom + MH_H - 1) / MH_H;
      if (per_row == 0) per_row = 1;
    }
    row = (*HVM.mh_access_count) / per_row;
    if (row >= MH_H) return;
  }
  (*HVM.mh_access_count)++;
  if (is_write) {
    (*MH_WRITES_AT((uint32_t)row, (uint32_t)col))++;
  } else {
    (*MH_READS_AT((uint32_t)row, (uint32_t)col))++;
  }
}

void memheat_on_read(Loc loc)  { mh_touch(loc, 0); }
void memheat_on_write(Loc loc) { mh_touch(loc, 1); }

void memheat_disable() {
  if (HVM.mh_enabled) *HVM.mh_enabled = 0;
}

void memheat_set_access_per_row(u64 val) {
  if (HVM.mh_access_per_row) *HVM.mh_access_per_row = val ? val : 1;
}

void memheat_configure(u64 max_len, u64 ns_window) {
  if (HVM.mh_max_len) *HVM.mh_max_len = max_len ? max_len : 1;
  if (HVM.mh_ns_window) *HVM.mh_ns_window = ns_window; // 0 = auto/access-based
  u64 ns = mh_now_ns();
  if (HVM.mh_t0_sec) *HVM.mh_t0_sec = ns / 1000000000ULL;
  if (HVM.mh_t0_nsec) *HVM.mh_t0_nsec = ns % 1000000000ULL;
}

void memheat_reset() {
  if (HVM.mh_reads) memset(HVM.mh_reads, 0, MH_W * MH_H * sizeof(u32));
  if (HVM.mh_writes) memset(HVM.mh_writes, 0, MH_W * MH_H * sizeof(u32));
  if (HVM.mh_access_count) *HVM.mh_access_count = 0;
}

// Demo: fill heap then perform MH_H timed reads across ascending locations in ~1 second
void hvm_mem_heat_demo() {
  printf("[memheat-demo] start\n");
  // Disable tracking during seeding so only reads appear
  memheat_disable();
  set_len(MH_W);
  for (u32 i = 0; i < MH_W; ++i) {
    set(i, term_new(W32, 0, 1));
  }
  // Configure tracker
  if (HVM.mh_path && HVM.mh_path[0] == '\0') strcpy(HVM.mh_path, "mem-heat.png");
  if (HVM.mh_max_len) *HVM.mh_max_len = MH_W;
  memheat_set_access_per_row(1);
  if (HVM.mh_enabled) *HVM.mh_enabled = 1;

  // Timed reads: diagonal from (0,0) to (MH_W-1,MH_H-1)
  struct timespec ts; ts.tv_sec = 0; ts.tv_nsec = (long)(1000000000LL / MH_H);
  for (u32 y = 0; y < MH_H; ++y) {
    Loc loc = y; // direct mapping
    (void)got(loc);
    nanosleep(&ts, NULL);
  }
  memheat_finish();
  printf("[memheat-demo] finish\n");
}

// ---- PNG encoding helpers (zlib store blocks) ----

// CRC32 (IEEE 802.3, polynomial 0xEDB88320)
static uint32_t mh_crc32(const uint8_t* buf, size_t len) {
  uint32_t c = 0xFFFFFFFFu;
  for (size_t i = 0; i < len; ++i) {
    c ^= buf[i];
    for (int k = 0; k < 8; ++k) {
      c = (c & 1) ? (0xEDB88320u ^ (c >> 1)) : (c >> 1);
    }
  }
  return c ^ 0xFFFFFFFFu;
}

// Adler-32
static uint32_t mh_adler32(const uint8_t* buf, size_t len) {
  const uint32_t MOD_ADLER = 65521u;
  uint32_t a = 1, b = 0;
  for (size_t i = 0; i < len; ++i) {
    a = (a + buf[i]) % MOD_ADLER;
    b = (b + a) % MOD_ADLER;
  }
  return (b << 16) | a;
}

static void mh_write_u32be(uint8_t* out, uint32_t v) {
  out[0] = (uint8_t)((v >> 24) & 0xFF);
  out[1] = (uint8_t)((v >> 16) & 0xFF);
  out[2] = (uint8_t)((v >> 8)  & 0xFF);
  out[3] = (uint8_t)(v & 0xFF);
}

// Build zlib stream containing uncompressed deflate blocks of `raw` data.
// Allocates and returns buffer; size stored in *out_len. Caller frees.
static uint8_t* mh_build_zlib_stored(const uint8_t* raw, size_t raw_len, size_t* out_len) {
  // Worst case: header(2) + (raw_len / 65535 + 1) * (5) + raw_len + adler(4)
  size_t blocks = (raw_len + 65535u - 1u) / 65535u;
  size_t cap = 2 + blocks * 5 + raw_len + 4;
  uint8_t* z = (uint8_t*)malloc(cap);
  if (!z) return NULL;
  size_t p = 0;

  // zlib header: CMF/FLG with deflate and no dictionary, computed FCHECK
  uint8_t CMF = 0x78; // 0b01111000 => deflate, 32K window
  uint8_t FLG = 0;    // FLEVEL=0, FDICT=0, FCHECK set below
  uint16_t val = ((uint16_t)CMF << 8) | FLG;
  uint16_t rem = (uint16_t)(31 - (val % 31));
  FLG = (uint8_t)rem;
  z[p++] = CMF;
  z[p++] = FLG;

  // Stored blocks
  size_t off = 0;
  for (size_t b = 0; b < blocks; ++b) {
    size_t chunk = raw_len - off;
    if (chunk > 65535u) chunk = 65535u;
    uint8_t final = (b == blocks - 1) ? 1 : 0;
    z[p++] = (uint8_t)(final | 0x00); // BFINAL + BTYPE=00
    uint16_t LEN = (uint16_t)chunk;
    uint16_t NLEN = (uint16_t)(~LEN);
    z[p++] = (uint8_t)(LEN & 0xFF);
    z[p++] = (uint8_t)((LEN >> 8) & 0xFF);
    z[p++] = (uint8_t)(NLEN & 0xFF);
    z[p++] = (uint8_t)((NLEN >> 8) & 0xFF);
    // copy payload
    for (size_t i = 0; i < chunk; ++i) z[p++] = raw[off + i];
    off += chunk;
  }

  // Adler-32 over raw data
  uint32_t ad = mh_adler32(raw, raw_len);
  z[p++] = (uint8_t)((ad >> 24) & 0xFF);
  z[p++] = (uint8_t)((ad >> 16) & 0xFF);
  z[p++] = (uint8_t)((ad >> 8) & 0xFF);
  z[p++] = (uint8_t)(ad & 0xFF);

  *out_len = p;
  return z;
}

static int mh_write_png_rgb(const char* path, const uint8_t* rgb, uint32_t w, uint32_t h) {
  // Build raw scanlines with no filter
  size_t raw_stride = 1 + (size_t)w * 3;
  size_t raw_size   = raw_stride * (size_t)h;
  uint8_t* raw = (uint8_t*)malloc(raw_size);
  if (!raw) return -1;
  for (uint32_t y = 0; y < h; ++y) {
    uint8_t* row = raw + (size_t)y * raw_stride;
    row[0] = 0; // filter type 0
    const uint8_t* src = rgb + (size_t)y * (size_t)w * 3;
    for (uint32_t x = 0; x < w * 3; ++x) row[1 + x] = src[x];
  }

  size_t zlen = 0;
  uint8_t* zdat = mh_build_zlib_stored(raw, raw_size, &zlen);
  free(raw);
  if (!zdat) return -2;

  FILE* f = fopen(path, "wb");
  if (!f) { free(zdat); return -3; }

  uint8_t sig[8] = {137,80,78,71,13,10,26,10};
  fwrite(sig, 1, 8, f);

  // IHDR
  uint8_t ihdr[13];
  mh_write_u32be(ihdr + 0, w);
  mh_write_u32be(ihdr + 4, h);
  ihdr[8]  = 8; // bit depth
  ihdr[9]  = 2; // color type: truecolor
  ihdr[10] = 0; // compression
  ihdr[11] = 0; // filter
  ihdr[12] = 0; // interlace
  uint8_t hdrlen[4]; mh_write_u32be(hdrlen, 13);
  fwrite(hdrlen, 1, 4, f);
  uint8_t typeIHDR[4] = {'I','H','D','R'};
  fwrite(typeIHDR, 1, 4, f);
  fwrite(ihdr, 1, 13, f);
  {
    uint8_t buf[17];
    for (int i=0;i<4;i++) buf[i]=typeIHDR[i];
    for (int i=0;i<13;i++) buf[4+i]=ihdr[i];
    uint32_t crc = mh_crc32(buf, 17);
    uint8_t crcb[4]; mh_write_u32be(crcb, crc);
    fwrite(crcb, 1, 4, f);
  }

  // IDAT
  uint8_t idatlen[4]; mh_write_u32be(idatlen, (uint32_t)zlen);
  fwrite(idatlen, 1, 4, f);
  uint8_t typeIDAT[4] = {'I','D','A','T'};
  fwrite(typeIDAT, 1, 4, f);
  fwrite(zdat, 1, zlen, f);
  {
    // CRC over 'IDAT' + zlib data
    uint8_t* tmp = (uint8_t*)malloc(4 + zlen);
    for (int i=0;i<4;i++) tmp[i]=typeIDAT[i];
    for (size_t i=0;i<zlen;i++) tmp[4+i]=zdat[i];
    uint32_t crc = mh_crc32(tmp, 4 + zlen);
    free(tmp);
    uint8_t crcb[4]; mh_write_u32be(crcb, crc);
    fwrite(crcb, 1, 4, f);
  }
  free(zdat);

  // IEND
  uint8_t zero[4] = {0,0,0,0};
  fwrite(zero, 1, 4, f);
  uint8_t typeIEND[4] = {'I','E','N','D'};
  fwrite(typeIEND, 1, 4, f);
  {
    uint32_t crc = mh_crc32(typeIEND, 4);
    uint8_t crcb[4]; mh_write_u32be(crcb, crc);
    fwrite(crcb, 1, 4, f);
  }

  fclose(f);
  return 0;
}

void memheat_finish() {
  // Always write a small debug file to aid troubleshooting
  {
    FILE* df = fopen("mh-debug.txt", "w");
    if (df) {
      fprintf(df, "ptr_enabled=%p\nptr_reads=%p\nptr_writes=%p\nptr_acc=%p\nptr_row=%p\npath_set=%d\n",
              (void*)HVM.mh_enabled,
              (void*)HVM.mh_reads,
              (void*)HVM.mh_writes,
              (void*)HVM.mh_access_count,
              (void*)HVM.mh_access_per_row,
              HVM.mh_path ? (HVM.mh_path[0] != '\0') : 0);
      if (HVM.mh_enabled) fprintf(df, "enabled=%u\n", (unsigned)*HVM.mh_enabled);
      if (HVM.mh_access_count) fprintf(df, "access_count_now=%llu\n", (unsigned long long)*HVM.mh_access_count);
      fclose(df);
    }
  }
  if (!HVM.mh_enabled) return;
  if (!*HVM.mh_enabled) return;
  if (!HVM.mh_reads || !HVM.mh_writes) return;

  // Compute max count to scale intensity
  uint32_t max_tot = 0;
  uint64_t sum_tot = 0;
  uint64_t nonzero  = 0;
  for (uint32_t y = 0; y < MH_H; ++y) {
    for (uint32_t x = 0; x < MH_W; ++x) {
      uint32_t r = *MH_READS_AT(y,x);
      uint32_t w = *MH_WRITES_AT(y,x);
      uint32_t t = r + w;
      if (t > max_tot) max_tot = t;
      sum_tot += t;
      if (t > 0) nonzero++;
    }
  }
  if (max_tot == 0) max_tot = 1;

  // Build RGB buffer with white background mixed towards hue based on access ratio
  uint8_t* img = (uint8_t*)malloc((size_t)MH_W * MH_H * 3);
  if (!img) return;
  for (uint32_t y = 0; y < MH_H; ++y) {
    for (uint32_t x = 0; x < MH_W; ++x) {
      uint32_t r = *MH_READS_AT(y,x);
      uint32_t w = *MH_WRITES_AT(y,x);
      uint32_t t = r + w;
      double alpha = 0.0;
      if (t > 0) {
        // Log-scale intensity to avoid near-white images
        double lt = log1p((double)t);
        double lm = log1p((double)max_tot);
        alpha = lt / (lm > 0 ? lm : 1.0);
      }
      if (alpha > 1.0) alpha = 1.0;
      // Base color: 0% writes => green, 50% => gray, 100% => red
      double rb=0.5, gb=0.5, bb=0.5; // default gray
      if (t > 0) {
        double pw = (double)w / (double)t; // [0..1]
        if (pw < 0.5) {
          double k = pw / 0.5; // 0..1
          // Lerp from green (0,1,0) to gray (0.5,0.5,0.5)
          rb = 0.0 * (1.0 - k) + 0.5 * k;
          gb = 1.0 * (1.0 - k) + 0.5 * k;
          bb = 0.0 * (1.0 - k) + 0.5 * k;
        } else {
          double k = (pw - 0.5) / 0.5; // 0..1
          // Lerp from gray (0.5,0.5,0.5) to red (1,0,0)
          rb = 0.5 * (1.0 - k) + 1.0 * k;
          gb = 0.5 * (1.0 - k) + 0.0 * k;
          bb = 0.5 * (1.0 - k) + 0.0 * k;
        }
      }
      // High-contrast: any nonzero access uses full color (no white mixing)
      double rch, gch, bch;
      if (t > 0) {
        rch = rb; gch = gb; bch = bb;
      } else {
        rch = 1.0; gch = 1.0; bch = 1.0;
      }
      uint8_t* px = img + ((size_t)y * MH_W + x) * 3;
      px[0] = (uint8_t)(rch < 0 ? 0 : rch > 1 ? 255 : (int)(rch * 255.0 + 0.5));
      px[1] = (uint8_t)(gch < 0 ? 0 : gch > 1 ? 255 : (int)(gch * 255.0 + 0.5));
      px[2] = (uint8_t)(bch < 0 ? 0 : bch > 1 ? 255 : (int)(bch * 255.0 + 0.5));
    }
  }

  {
    FILE* df = fopen("mh-debug.txt", "w");
    if (df) {
      fprintf(df, "access_count=%llu\nsum_tot=%llu\nmax_tot=%u\nnonzero=%llu\n",
              (unsigned long long)(*HVM.mh_access_count),
              (unsigned long long)sum_tot,
              max_tot,
              (unsigned long long)nonzero);
      fclose(df);
    }
  }
  mh_write_png_rgb(HVM.mh_path[0] ? HVM.mh_path : "mem-heat.png", img, MH_W, MH_H);
  free(img);

  // Keep buffers allocated for potential reuse (single run typically)
}
