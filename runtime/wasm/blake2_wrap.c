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
 * Thin wrapper around the freestanding build of blake2b-ref.c.
 *
 * The C side keeps four fixed BSS buffers — state, key, output, and an
 * input chunk — and exposes their addresses to the WAT side via getter
 * functions.  The WAT side blits the OCaml-held state into the C state
 * buffer at the top of each operation, runs the algorithm, and blits the
 * mutated state back.  caml_blake2_update feeds arbitrary-size inputs
 * through the fixed-size chunk buffer in a WAT-side loop (BLAKE2 is
 * streamable — chunk boundaries don't affect the result).
 */

#include <stddef.h>
#include <stdint.h>

#include "blake2.h"

/*
 * Under -mbulk-memory clang lowers __builtin_mem{cpy,move,set} to inline
 * memory.copy / memory.fill instructions, so these wrappers expand to a
 * single bulk-memory op plus a return — no recursive call to themselves.
 * They're needed because LLVM's loop-idiom-recognize pass still emits
 * libcalls to memset/memcpy that we have to resolve under -nodefaultlibs.
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

/* Freestanding stubs for symbols clang may emit. */
void __assert_fail(const char *a, const char *b, unsigned c, const char *d) {
  (void)a; (void)b; (void)c; (void)d;
  __builtin_trap();
}
void __stack_chk_fail(void) { __builtin_trap(); }

/* Sizes — must match the constants in runtime/wasm/blake2.wat. */
#define BLAKE2_STATE_SIZE 248
#define BLAKE2_KEY_MAX    64    /* BLAKE2B_KEYBYTES */
#define BLAKE2_OUT_MAX    64    /* BLAKE2B_OUTBYTES */
#define BLAKE2_CHUNK_SIZE 8192

_Static_assert(sizeof(blake2b_state) <= BLAKE2_STATE_SIZE,
               "blake2b_state outgrew BLAKE2_STATE_SIZE; bump the constant");

static char g_state[BLAKE2_STATE_SIZE];
static char g_key  [BLAKE2_KEY_MAX];
static char g_out  [BLAKE2_OUT_MAX];
static char g_chunk[BLAKE2_CHUNK_SIZE];

/* Address getters — binaryen -O3 inlines them after wasm-merge so there's
   no per-call overhead. */

__attribute__((export_name("blake2_state_buf")))
int state_buf_addr(void) { return (int)(uintptr_t)g_state; }

__attribute__((export_name("blake2_key_buf")))
int key_buf_addr(void) { return (int)(uintptr_t)g_key; }

__attribute__((export_name("blake2_out_buf")))
int out_buf_addr(void) { return (int)(uintptr_t)g_out; }

__attribute__((export_name("blake2_chunk_buf")))
int chunk_buf_addr(void) { return (int)(uintptr_t)g_chunk; }

__attribute__((export_name("blake2_chunk_buf_size")))
int chunk_buf_size(void) { return BLAKE2_CHUNK_SIZE; }

/* The algorithm primitives all operate implicitly on g_state. */

__attribute__((export_name("blake2_init")))
void blake2_init(int hashlen, int keylen) {
  if (keylen == 0)
    blake2b_init((blake2b_state *)g_state, (size_t)hashlen);
  else
    blake2b_init_key((blake2b_state *)g_state, (size_t)hashlen, g_key,
                     (size_t)keylen);
}

__attribute__((export_name("blake2_update")))
void blake2_update(int len) {
  blake2b_update((blake2b_state *)g_state, g_chunk, (size_t)len);
}

__attribute__((export_name("blake2_finalize")))
void blake2_finalize(int outlen) {
  blake2b_final((blake2b_state *)g_state, g_out, (size_t)outlen);
}
