/*
 * Wasm_of_ocaml runtime support
 * http://www.ocsigen.org/js_of_ocaml/
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

/*
 * Thin wrapper around the freestanding build of zstddeclib.c.
 *
 * The amalgamation defines ZSTD_DEPS_NEED_MALLOC, so it calls malloc / free
 * / calloc / realloc through the C library.  We're built with -nostdlib, so
 * we supply those names ourselves using a bump allocator backed by the tail
 * of linear memory, rewound at the start of each decompression.
 *
 * wasm linear memory, once grown, is never released back to the host, so we
 * keep the high-water mark small in two complementary ways; zstd.wat picks
 * one per call by comparing out_len against the frame's declared window
 * (zstd_window_size):
 *
 *  - out_len <= window: one-shot (zstd_decompress).  Decode straight into an
 *    out_len-sized buffer, which zstd also uses as its history window, so no
 *    separate window buffer is allocated.  The output buffer is no larger than
 *    the window a stream would otherwise keep, so this is the cheaper choice.
 *
 *  - out_len > window: streaming (ZSTD_decompressStream).  Feed the input and
 *    drain the output through the two fixed chunk buffers below, blitting each
 *    piece to/from a GC byte array, so neither is ever resident whole; zstd
 *    keeps only its window-sized history buffer, bounding the footprint below
 *    out_len.
 */

#include <stddef.h>
#include <stdint.h>

/* Minimal declarations matching zstddeclib.c's public streaming API. */
typedef struct ZSTD_DStream_s ZSTD_DStream;
typedef struct {
  const void *src;
  size_t size;
  size_t pos;
} ZSTD_inBuffer;
typedef struct {
  void *dst;
  size_t size;
  size_t pos;
} ZSTD_outBuffer;
typedef struct {
  unsigned long long frameContentSize;
  unsigned long long windowSize;
  unsigned blockSizeMax;
  int frameType;
  unsigned headerSize;
  unsigned dictID;
  unsigned checksumFlag;
  unsigned _reserved1;
  unsigned _reserved2;
} ZSTD_FrameHeader;
extern ZSTD_DStream *ZSTD_createDStream(void);
extern size_t ZSTD_initDStream(ZSTD_DStream *zds);
extern size_t ZSTD_decompressStream(ZSTD_DStream *zds, ZSTD_outBuffer *output,
                                    ZSTD_inBuffer *input);
extern size_t ZSTD_decompress(void *dst, size_t dstCapacity, const void *src,
                              size_t compressedSize);
extern size_t ZSTD_getFrameHeader(ZSTD_FrameHeader *zfhPtr, const void *src,
                                  size_t srcSize);
extern unsigned ZSTD_isError(size_t code);

extern unsigned char __heap_base;

#define WASM_PAGE 65536u

static uintptr_t bump_cur = 0;

static void *bump_alloc(size_t n) {
  if (bump_cur == 0) bump_cur = (uintptr_t)&__heap_base;
  uintptr_t p = (bump_cur + 15) & ~(uintptr_t)15;
  uintptr_t end = p + n;
  uintptr_t mem_bytes = (uintptr_t)__builtin_wasm_memory_size(0) * WASM_PAGE;
  if (end > mem_bytes) {
    size_t pages = (end - mem_bytes + WASM_PAGE - 1) / WASM_PAGE;
    if ((int)__builtin_wasm_memory_grow(0, pages) < 0) return (void *)0;
  }
  bump_cur = end;
  return (void *)p;
}

/* memcpy/memmove/memset and other freestanding libc stubs live in stubs.c. */

void *malloc(size_t n) { return bump_alloc(n); }

void free(void *p) { (void)p; }

void *calloc(size_t nmemb, size_t size) {
  size_t total = nmemb * size;
  void *p = bump_alloc(total);
  if (p) __builtin_memset(p, 0, total);
  return p;
}

/*
 * Bump-allocator realloc: allocate a fresh buffer, copy.  We don't track
 * the old buffer's true size, so we may over-copy past its end — safe in
 * linear memory (the bytes after are either unused bump space or prior
 * allocations; both readable).  realloc is not on the decoder's hot path
 * for typical marshal payloads.
 */
void *realloc(void *old, size_t n) {
  void *p = bump_alloc(n);
  if (p && old) __builtin_memcpy(p, old, n);
  return p;
}

/*
 * Streaming state and the two fixed chunk buffers exchanged with zstd.wat.
 * 64 KiB matches zstd's recommended stream buffer sizes closely enough while
 * keeping the module's static footprint small.
 */
#define ZSTD_CHUNK 65536u

static char g_in[ZSTD_CHUNK];
static char g_out[ZSTD_CHUNK];
static ZSTD_DStream *g_zds;
static ZSTD_inBuffer g_inbuf;
static ZSTD_outBuffer g_outbuf;
static size_t g_produced;

/* Exports consumed by runtime/wasm/zstd.wat. */

/*
 * Window size declared by the frame header, read from its first [src_len]
 * bytes, or 0 if it can't be parsed.  zstd.wat compares it against out_len to
 * pick the path: one-shot when the output fits in the window, streaming
 * otherwise.  Clamped to INT_MAX so the i32 comparison stays well-defined; an
 * over-large window is rejected later by the decoder itself.
 */
__attribute__((export_name("zstd_window_size")))
int zstd_window_size(int src, int src_len) {
  ZSTD_FrameHeader zfh;
  if (ZSTD_getFrameHeader(&zfh, (const void *)(uintptr_t)src, (size_t)src_len)
      != 0)
    return 0;
  return zfh.windowSize > 0x7fffffffULL ? 0x7fffffff : (int)zfh.windowSize;
}

/* One-shot path (small values). */

__attribute__((export_name("zstd_reset")))
void zstd_reset(void) { bump_cur = (uintptr_t)&__heap_base; }

__attribute__((export_name("zstd_alloc")))
void *zstd_alloc(size_t n) { return bump_alloc(n); }

__attribute__((export_name("zstd_decompress")))
size_t zstd_decompress(void *dst, size_t dstCap, const void *src,
                       size_t srcSize) {
  return ZSTD_decompress(dst, dstCap, src, srcSize);
}

/* Streaming path (large values). */

__attribute__((export_name("zstd_in_buf")))
int zstd_in_buf(void) { return (int)(uintptr_t)g_in; }

__attribute__((export_name("zstd_in_cap")))
int zstd_in_cap(void) { return ZSTD_CHUNK; }

__attribute__((export_name("zstd_out_buf")))
int zstd_out_buf(void) { return (int)(uintptr_t)g_out; }

/* Rewind the arena and start a fresh frame.  Returns -1 on failure. */
__attribute__((export_name("zstd_stream_init")))
int zstd_stream_init(void) {
  bump_cur = (uintptr_t)&__heap_base;
  g_zds = ZSTD_createDStream();
  if (!g_zds) return -1;
  if (ZSTD_isError(ZSTD_initDStream(g_zds))) return -1;
  g_inbuf.src = g_in;
  g_inbuf.size = 0;
  g_inbuf.pos = 0;
  return 0;
}

/* Hand zstd the [n] compressed bytes now sitting in g_in. */
__attribute__((export_name("zstd_stream_set_input")))
void zstd_stream_set_input(int n) {
  g_inbuf.src = g_in;
  g_inbuf.size = (size_t)n;
  g_inbuf.pos = 0;
}

/*
 * Run one decompression step into g_out.  Returns -1 on error, 0 once the
 * frame is fully decoded and flushed, 1 if more work remains.
 */
__attribute__((export_name("zstd_stream_run")))
int zstd_stream_run(void) {
  g_outbuf.dst = g_out;
  g_outbuf.size = ZSTD_CHUNK;
  g_outbuf.pos = 0;
  size_t r = ZSTD_decompressStream(g_zds, &g_outbuf, &g_inbuf);
  g_produced = g_outbuf.pos;
  if (ZSTD_isError(r)) return -1;
  return r == 0 ? 0 : 1;
}

/* Plaintext bytes written into g_out by the last stream_run. */
__attribute__((export_name("zstd_out_produced")))
int zstd_out_produced(void) { return (int)g_produced; }

/* 1 once the current input chunk is fully consumed and needs refilling. */
__attribute__((export_name("zstd_in_done")))
int zstd_in_done(void) { return g_inbuf.pos >= g_inbuf.size; }
