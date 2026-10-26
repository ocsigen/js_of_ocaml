/*
 * Js_of_ocaml compiler
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
 */

#include <caml/mlvalues.h>
#include <caml/alloc.h>

/* Only called under js_of_ocaml / wasm_of_ocaml, which provide the
   primitive. */
CAMLprim value caml_jsoo_flags_effects(value unit) {
  return caml_copy_string("native");
}
