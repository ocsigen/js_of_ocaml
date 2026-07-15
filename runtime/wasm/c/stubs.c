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
 * Freestanding C runtime stubs shared by the decoders linked into this module
 * (zstd, blake2).  We build with -nodefaultlibs, so we supply the handful of
 * libc symbols the compiler may reference.
 *
 * Under -mbulk-memory clang lowers __builtin_mem{cpy,move,set} to inline
 * memory.copy / memory.fill instructions, so these wrappers expand to a single
 * bulk-memory op plus a return — no recursive call to themselves.  They're
 * needed because LLVM's loop-idiom-recognize pass still emits libcalls to
 * memset/memcpy that we have to resolve.
 */

#include <stddef.h>

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
