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
   (import "jslib" "wrap" (func $wrap (param anyref) (result (ref eq))))
   (import "jslib" "unwrap" (func $unwrap (param (ref eq)) (result anyref)))
   (import "jslib" "caml_js_get"
      (func $caml_js_get (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "bigarray" "caml_ba_to_typed_array"
     (func $caml_ba_to_typed_array (param (ref eq)) (result (ref eq))))
   (import "bigarray" "caml_ba_from_typed_array"
      (func $caml_ba_from_typed_array (param (ref eq)) (result (ref eq))))
   (import "bigarray" "caml_ba_sub"
      (func $caml_ba_sub
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "bigarray" "caml_ba_fill"
      (func $caml_ba_fill (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "bigarray" "caml_ba_get_data"
      (func $caml_ba_get_data (param (ref eq)) (result (ref extern))))
   (import "bigarray" "caml_ba_get_view"
      (func $caml_ba_get_view (param (ref eq)) (result (ref extern))))
   (import "bindings" "ta_create"
      (func $ta_create (param i32) (param anyref) (result anyref)))
   (import "bindings" "dv_get_i32"
      (func $dv_get_i32 (param externref i32 i32) (result i32)))
   (import "bindings" "dv_get_ui8"
      (func $dv_get_ui8 (param externref i32) (result i32)))
   (import "bindings" "dv_set_i8"
      (func $dv_set_i8 (param externref i32 i32)))
   (import "bindings" "ta_subarray"
      (func $ta_subarray
         (param (ref extern)) (param i32) (param i32) (result (ref extern))))
   (import "bindings" "ta_set"
      (func $ta_set (param (ref extern)) (param (ref extern)) (param i32)))
   (import "bindings" "ta_length"
      (func $ta_length (param (ref extern)) (result i32)))
   (import "bindings" "ta_bytes"
      (func $ta_bytes (param anyref) (result anyref)))
   (import "bindings" "ta_blit_from_bytes"
      (func $ta_blit_from_bytes
         (param (ref $bytes)) (param i32) (param (ref extern)) (param i32)
         (param i32)))
   (import "bindings" "ta_blit_to_bytes"
      (func $ta_blit_to_bytes
         (param (ref extern)) (param i32) (param (ref $bytes)) (param i32)
         (param i32)))
   (import "hash" "caml_hash_mix_int"
      (func $caml_hash_mix_int (param i32) (param i32) (result i32)))

   (type $bytes (array (mut i8)))

   (func (export "caml_hash_mix_bigstring")
      (param $h i32) (param $b (ref eq)) (result i32)
      (local $data (ref extern))
      (local $view (ref extern))
      (local $len i32) (local $i i32) (local $w i32)
      (local.set $data (call $caml_ba_get_data (local.get $b)))
      (local.set $view (call $caml_ba_get_view (local.get $b)))
      (local.set $len (call $ta_length (local.get $data)))
      (loop $loop
         (if (i32.le_u (i32.add (local.get $i) (i32.const 4)) (local.get $len))
            (then
               (local.set $h
                  (call $caml_hash_mix_int
                     (local.get $h)
                     (call $dv_get_i32 (local.get $view) (local.get $i)
                        (i32.const 1))))
               (local.set $i (i32.add (local.get $i) (i32.const 4)))
               (br $loop))))
      (local.set $w (i32.const 0))
      (block $0_bytes
         (block $1_byte
            (block $2_bytes
               (block $3_bytes
                  (br_table $0_bytes $1_byte $2_bytes $3_bytes
                     (i32.and (local.get $len) (i32.const 3))))
               (local.set $w
                  (i32.shl (call $dv_get_ui8 (local.get $view)
                              (i32.add (local.get $i) (i32.const 2)))
                           (i32.const 16))))
            (local.set $w
               (i32.or (local.get $w)
                  (i32.shl (call $dv_get_ui8 (local.get $view)
                              (i32.add (local.get $i) (i32.const 1)))
                           (i32.const 8)))))
         (local.set $w
            (i32.or (local.get $w)
               (call $dv_get_ui8 (local.get $view) (local.get $i))))
         (local.set $h (call $caml_hash_mix_int (local.get $h) (local.get $w))))
      (i32.xor (local.get $h) (local.get $len)))

   (@string $buffer "buffer")

   (func (export "bigstring_to_array_buffer")
      (param $bs (ref eq)) (result (ref eq))
      (return_call $caml_js_get
         (call $caml_ba_to_typed_array (local.get $bs))
         (global.get $buffer)))

   (export "bigstring_to_typed_array" (func $caml_ba_to_typed_array))

   (func (export "bigstring_of_array_buffer") (param (ref eq)) (result (ref eq))
       (return_call $caml_ba_from_typed_array
          (call $wrap
             (call $ta_create (i32.const 12) (call $unwrap (local.get 0))))))

   (func (export "bigstring_of_typed_array") (param (ref eq)) (result (ref eq))
       (return_call $caml_ba_from_typed_array
          (call $wrap (call $ta_bytes (call $unwrap (local.get 0))))))

   (func (export "caml_bigstring_memset")
      (param $s (ref eq)) (param $pos (ref eq)) (param $len (ref eq))
      (param $v (ref eq)) (result (ref eq))
      (return_call $caml_ba_fill
         (call $caml_ba_sub (local.get $s) (local.get $pos) (local.get $len))
         (local.get $v)))

   (func (export "caml_bigstring_memcmp")
      (param $s1 (ref eq)) (param $vpos1 (ref eq))
      (param $s2 (ref eq)) (param $vpos2 (ref eq))
      (param $vlen (ref eq)) (result (ref eq))
      (local $i i32) (local $pos1 i32) (local $pos2 i32) (local $len i32)
      (local $c1 i32) (local $c2 i32)
      (local $v1 (ref extern)) (local $v2 (ref extern))
      (local.set $v1 (call $caml_ba_get_view (local.get $s1)))
      (local.set $pos1 (i31.get_s (ref.cast (ref i31) (local.get $vpos1))))
      (local.set $v2 (call $caml_ba_get_view (local.get $s2)))
      (local.set $pos2 (i31.get_s (ref.cast (ref i31) (local.get $vpos2))))
      (local.set $len (i31.get_s (ref.cast (ref i31) (local.get $vlen))))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $len))
            (then
               (local.set $c1
                  (call $dv_get_ui8 (local.get $v1)
                     (i32.add (local.get $pos1) (local.get $i))))
               (local.set $c2
                  (call $dv_get_ui8 (local.get $v2)
                     (i32.add (local.get $pos2) (local.get $i))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br_if $loop (i32.eq (local.get $c1) (local.get $c2)))
               (return
                  (select (ref.i31 (i32.const -1)) (ref.i31 (i32.const 1))
                     (i32.lt_u (local.get $c1) (local.get $c2)))))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_bigstring_memcmp_string")
      (param $s1 (ref eq)) (param $vpos1 (ref eq))
      (param $vs2 (ref eq)) (param $vpos2 (ref eq))
      (param $vlen (ref eq)) (result (ref eq))
      (local $i i32) (local $pos1 i32) (local $pos2 i32) (local $len i32)
      (local $c1 i32) (local $c2 i32)
      (local $v1 (ref extern))
      (local $s2 (ref $bytes))
      (local.set $v1 (call $caml_ba_get_view (local.get $s1)))
      (local.set $pos1 (i31.get_s (ref.cast (ref i31) (local.get $vpos1))))
      (local.set $s2 (ref.cast (ref $bytes) (local.get $vs2)))
      (local.set $pos2 (i31.get_s (ref.cast (ref i31) (local.get $vpos2))))
      (local.set $len (i31.get_s (ref.cast (ref i31) (local.get $vlen))))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $len))
            (then
               (local.set $c1
                  (call $dv_get_ui8 (local.get $v1)
                     (i32.add (local.get $pos1) (local.get $i))))
               (local.set $c2
                  (array.get_u $bytes (local.get $s2)
                     (i32.add (local.get $pos2) (local.get $i))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br_if $loop (i32.eq (local.get $c1) (local.get $c2)))
               (return
                  (select (ref.i31 (i32.const -1)) (ref.i31 (i32.const 1))
                     (i32.lt_u (local.get $c1) (local.get $c2)))))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_bigstring_memchr")
      (param $s (ref eq)) (param $vc (ref eq))
      (param $vpos (ref eq)) (param $vlen (ref eq)) (result (ref eq))
      (local $pos i32) (local $len i32) (local $c i32)
      (local $v (ref extern))
      (local.set $c (i31.get_s (ref.cast (ref i31) (local.get $vc))))
      (local.set $pos (i31.get_s (ref.cast (ref i31) (local.get $vpos))))
      (local.set $len (i31.get_s (ref.cast (ref i31) (local.get $vlen))))
      (local.set $v (call $caml_ba_get_view (local.get $s)))
      (loop $loop
         (if (i32.gt_s (local.get $len) (i32.const 0))
            (then
               (if (i32.eq (local.get $c)
                      (call $dv_get_ui8 (local.get $v) (local.get $pos)))
                  (then
                     (return (ref.i31 (local.get $pos)))))
               (local.set $len (i32.sub (local.get $len) (i32.const 1)))
               (local.set $pos (i32.add (local.get $pos) (i32.const 1)))
               (br $loop))))
      (ref.i31 (i32.const -1)))

   (func (export "caml_bigstring_memrchr")
      (param $s (ref eq)) (param $vc (ref eq))
      (param $vpos (ref eq)) (param $vlen (ref eq)) (result (ref eq))
      (local $pos i32) (local $len i32) (local $c i32) (local $cur i32)
      (local $v (ref extern))
      (local.set $c (i31.get_s (ref.cast (ref i31) (local.get $vc))))
      (local.set $pos (i31.get_s (ref.cast (ref i31) (local.get $vpos))))
      (local.set $len (i31.get_s (ref.cast (ref i31) (local.get $vlen))))
      (local.set $v (call $caml_ba_get_view (local.get $s)))
      (local.set $cur
         (i32.sub (i32.add (local.get $pos) (local.get $len)) (i32.const 1)))
      (loop $loop
         (if (i32.ge_s (local.get $cur) (local.get $pos))
            (then
               (if (i32.eq (local.get $c)
                      (call $dv_get_ui8 (local.get $v) (local.get $cur)))
                  (then
                     (return (ref.i31 (local.get $cur)))))
               (local.set $cur (i32.sub (local.get $cur) (i32.const 1)))
               (br $loop))))
      (ref.i31 (i32.const -1)))

   (export "caml_bigstring_blit_string_to_ba"
      (func $caml_bigstring_blit_bytes_to_ba))
   (func $caml_bigstring_blit_bytes_to_ba
      (export "caml_bigstring_blit_bytes_to_ba")
      (param $str1 (ref eq)) (param $vpos1 (ref eq))
      (param $ba2 (ref eq)) (param $vpos2 (ref eq))
      (param $vlen (ref eq)) (result (ref eq))
      (local $pos1 i32) (local $pos2 i32) (local $len i32)
      (local $s1 (ref $bytes))
      (local $d2 (ref extern))
      (local.set $s1 (ref.cast (ref $bytes) (local.get $str1)))
      (local.set $pos1 (i31.get_s (ref.cast (ref i31) (local.get $vpos1))))
      (local.set $d2 (call $caml_ba_get_data (local.get $ba2)))
      (local.set $pos2 (i31.get_s (ref.cast (ref i31) (local.get $vpos2))))
      (local.set $len (i31.get_s (ref.cast (ref i31) (local.get $vlen))))
      (call $ta_blit_from_bytes
         (local.get $s1) (local.get $pos1)
         (local.get $d2) (local.get $pos2)
         (local.get $len))
      (ref.i31 (i32.const 0)))

   (func (export "caml_bigstring_blit_ba_to_bytes")
      (param $ba1 (ref eq)) (param $vpos1 (ref eq))
      (param $str2 (ref eq)) (param $vpos2 (ref eq))
      (param $vlen (ref eq)) (result (ref eq))
      (local $pos1 i32) (local $pos2 i32) (local $len i32)
      (local $d1 (ref extern))
      (local $s2 (ref $bytes))
      (local.set $d1 (call $caml_ba_get_data (local.get $ba1)))
      (local.set $pos1 (i31.get_s (ref.cast (ref i31) (local.get $vpos1))))
      (local.set $s2 (ref.cast (ref $bytes) (local.get $str2)))
      (local.set $pos2 (i31.get_s (ref.cast (ref i31) (local.get $vpos2))))
      (local.set $len (i31.get_s (ref.cast (ref i31) (local.get $vlen))))
      (call $ta_blit_to_bytes
         (local.get $d1) (local.get $pos1)
         (local.get $s2) (local.get $pos2)
         (local.get $len))
      (ref.i31 (i32.const 0)))

   (func (export "caml_bigstring_blit_ba_to_ba")
      (param $ba1 (ref eq)) (param $vpos1 (ref eq))
      (param $ba2 (ref eq)) (param $vpos2 (ref eq))
      (param $vlen (ref eq)) (result (ref eq))
      (local $pos1 i32) (local $pos2 i32) (local $len i32)
      (local $d1 (ref extern))
      (local $d2 (ref extern))
      (local.set $d1 (call $caml_ba_get_data (local.get $ba1)))
      (local.set $pos1 (i31.get_s (ref.cast (ref i31) (local.get $vpos1))))
      (local.set $d2 (call $caml_ba_get_data (local.get $ba2)))
      (local.set $pos2 (i31.get_s (ref.cast (ref i31) (local.get $vpos2))))
      (local.set $len (i31.get_s (ref.cast (ref i31) (local.get $vlen))))
      (call $ta_set (local.get $d2)
         (call $ta_subarray (local.get $d1)
            (local.get $pos1) (i32.add (local.get $pos1) (local.get $len)))
         (local.get $pos2))
      (ref.i31 (i32.const 0)))
)
