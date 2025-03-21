;; Wasm_of_ocaml runtime support
;; http://www.ocsigen.org/js_of_ocaml/
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, with linking exception;
;; either version 2.1 of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

(module
   (import "bindings" "ta_new" (func $ta_new (param i32) (result (ref extern))))
   (import "bindings" "ta_blit_from_bytes"
      (func $ta_blit_from_bytes
         (param (ref $bytes)) (param i32) (param (ref extern)) (param i32)
         (param i32)))
   (import "bindings" "ta_blit_to_bytes"
      (func $ta_blit_to_bytes
         (param (ref extern)) (param i32) (param (ref $bytes)) (param i32)
         (param i32)))
   (import "js" "zstd_decompress"
      (func $zstd_decompress (param (ref extern)) (param (ref extern))))

   (type $bytes (array (mut i8)))
   (type $decompress (func (param (ref $bytes) (ref $bytes))))

   (global $decompress (export "caml_intern_decompress_input")
      (mut (ref null $decompress))
      (ref.null $decompress))

   (func $decompress (param $input (ref $bytes)) (param $output (ref $bytes))
      (local $in_buf (ref extern)) (local $out_buf (ref extern))
      (local.set $in_buf (call $ta_new (array.len (local.get $input))))
      (local.set $out_buf (call $ta_new (array.len (local.get $output))))
      (call $ta_blit_from_bytes
         (local.get $input) (i32.const 0)
         (local.get $in_buf) (i32.const 0)
         (array.len (local.get $input)))
      (call $zstd_decompress (local.get $in_buf) (local.get $out_buf))
      (call $ta_blit_to_bytes
         (local.get $out_buf) (i32.const 0)
         (local.get $output) (i32.const 0)
         (array.len (local.get $output))))

   (func (export "caml_zstd_initialize") (param (ref eq)) (result (ref eq))
      (global.set $decompress (ref.func $decompress))
      (ref.i31 (i32.const 1)))
)
