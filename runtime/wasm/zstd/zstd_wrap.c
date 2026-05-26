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
 * of linear memory.  The same allocator hands out the per-call input and
 * output buffers (zstd_alloc) and is rewound between marshals (zstd_reset),
 * so there is no fragmentation and no free list.
 */

#include <stddef.h>
#include <stdint.h>

extern size_t ZSTD_decompress(void *dst, size_t dstCapacity,
                              const void *src, size_t compressedSize);

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

/*
 * Under -mbulk-memory clang lowers __builtin_mem{cpy,move,set} to inline
 * memory.copy / memory.fill instructions, so these wrappers expand to a
 * single bulk-memory op plus a return — no recursive call to themselves.
 */
void *memcpy(void *dst, const void *src, size_t n) {
  __builtin_memcpy(dst, src, n);
  return dst;
}
void *memmove(void *dst, const void *src, size_t n) {
  __builtin_memmove(dst, src, n);
  return dst;
}
void *memset(void *dst, int v, size_t n) {
  __builtin_memset(dst, v, n);
  return dst;
}

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

/* Freestanding stubs for symbols clang may emit. */
void __assert_fail(const char *a, const char *b, unsigned c, const char *d) {
  (void)a; (void)b; (void)c; (void)d;
  __builtin_trap();
}
void __stack_chk_fail(void) { __builtin_trap(); }

/* Exports consumed by runtime/wasm/zstd.wat. */

__attribute__((export_name("zstd_alloc")))
void *zstd_alloc(size_t n) { return bump_alloc(n); }

__attribute__((export_name("zstd_reset")))
void zstd_reset(void) { bump_cur = (uintptr_t)&__heap_base; }

__attribute__((export_name("zstd_decompress")))
size_t zstd_decompress(void *dst, size_t dstCap,
                       const void *src, size_t srcSize) {
  return ZSTD_decompress(dst, dstCap, src, srcSize);
}
