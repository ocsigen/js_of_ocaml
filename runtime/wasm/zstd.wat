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
(@if (>= $ocaml_version (5 1 0))
(@then
   (import "fail" "caml_failwith" (func $caml_failwith (param (ref eq))))
   (import "marshal" "caml_intern_decompress_input"
      (global $caml_intern_decompress_input (mut (ref null $decompress))))
   (import "zstd" "zstd_memory" (memory 2))
   (import "zstd" "zstd_decompress"
      (func $zstd_decompress (param i32 i32 i32 i32) (result i32)))
   (import "zstd" "zstd_alloc" (func $zstd_alloc (param i32) (result i32)))
   (import "zstd" "zstd_reset" (func $zstd_reset))

   (type $bytes (array (mut i8)))
   (type $decompress
      (func (param (ref $bytes) i32 i32 i32) (result (ref $bytes))))

   (@string $decompress_failed "input_value: zstd decompression failed")

   (func $decompress
      (param $input (ref $bytes)) (param $pos i32) (param $len i32)
      (param $out_len i32) (result (ref $bytes))
      (local $in_buf i32) (local $out_buf i32) (local $i i32)
      (local $output (ref $bytes))
      (call $zstd_reset)
      (local.set $in_buf (call $zstd_alloc (local.get $len)))
      (local.set $out_buf (call $zstd_alloc (local.get $out_len)))
      ;; Blit input bytes from the GC array into zstd's linear memory.
      (local.set $i (i32.const 0))
      (block $done_in
         (loop $loop_in
            (br_if $done_in (i32.eq (local.get $i) (local.get $len)))
            (i32.store8
               (i32.add (local.get $in_buf) (local.get $i))
               (array.get_u $bytes (local.get $input)
                  (i32.add (local.get $pos) (local.get $i))))
            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (br $loop_in)))
      (if (i32.ne
             (call $zstd_decompress
                (local.get $out_buf) (local.get $out_len)
                (local.get $in_buf) (local.get $len))
             (local.get $out_len))
         (then (call $caml_failwith (global.get $decompress_failed))))
      ;; Blit output bytes from linear memory back into a fresh GC array.
      (local.set $output
         (array.new $bytes (i32.const 0) (local.get $out_len)))
      (local.set $i (i32.const 0))
      (block $done_out
         (loop $loop_out
            (br_if $done_out (i32.eq (local.get $i) (local.get $out_len)))
            (array.set $bytes (local.get $output) (local.get $i)
               (i32.load8_u
                  (i32.add (local.get $out_buf) (local.get $i))))
            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (br $loop_out)))
      (local.get $output))

   (func (export "caml_zstd_initialize") (param (ref eq)) (result (ref eq))
      (global.set $caml_intern_decompress_input (ref.func $decompress))
      (ref.i31 (i32.const 1)))
)
(@else
   (func (export "caml_zstd_initialize") (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))
))
)
