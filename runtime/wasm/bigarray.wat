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
   (import "bindings" "ta_create"
      (func $ta_create (param i32) (param i32) (result (ref extern))))
   (import "bindings" "ta_normalize"
      (func $ta_normalize (param (ref extern)) (result (ref extern))))
   (import "bindings" "ta_kind"
      (func $ta_kind (param (ref extern)) (result i32)))
   (import "bindings" "ta_length"
      (func $ta_length (param (ref extern)) (result i32)))
   (import "bindings" "ta_fill"
      (func $ta_fill_int (param (ref extern)) (param i32)))
   (import "bindings" "ta_fill"
      (func $ta_fill_float (param (ref extern)) (param f64)))
   (import "bindings" "ta_blit"
      (func $ta_blit (param (ref extern)) (param (ref extern))))
   (import "bindings" "ta_subarray"
      (func $ta_subarray
         (param (ref extern)) (param i32) (param i32) (result (ref extern))))
   (import "bindings" "ta_blit_from_bytes"
      (func $ta_blit_from_bytes
         (param (ref $bytes)) (param i32) (param (ref extern)) (param i32)
         (param i32)))
   (import "bindings" "ta_blit_to_bytes"
      (func $ta_blit_to_bytes
         (param (ref extern)) (param i32) (param (ref $bytes)) (param i32)
         (param i32)))
   (import "bindings" "dv_make"
      (func $dv_make (param (ref extern)) (result (ref extern))))
   (import "bindings" "dv_get_f64"
      (func $dv_get_f64 (param externref i32 i32) (result f64)))
   (import "bindings" "dv_get_f32"
      (func $dv_get_f32 (param externref i32 i32) (result f32)))
   (import "bindings" "dv_get_i64"
      (func $dv_get_i64 (param externref i32 i32) (result i64)))
   (import "bindings" "dv_get_i32"
      (func $dv_get_i32 (param externref i32 i32) (result i32)))
   (import "bindings" "dv_get_i16"
      (func $dv_get_i16 (param externref i32 i32) (result i32)))
   (import "bindings" "dv_get_ui16"
      (func $dv_get_ui16 (param externref i32 i32) (result i32)))
   (import "bindings" "dv_get_i8"
      (func $dv_get_i8 (param externref i32) (result i32)))
   (import "bindings" "dv_get_ui8"
      (func $dv_get_ui8 (param externref i32) (result i32)))
   (import "bindings" "dv_set_f64"
      (func $dv_set_f64 (param externref i32 f64 i32)))
   (import "bindings" "dv_set_f32"
      (func $dv_set_f32 (param externref i32 f32 i32)))
   (import "bindings" "dv_set_i64"
      (func $dv_set_i64 (param externref i32 i64 i32)))
   (import "bindings" "dv_set_i32"
      (func $dv_set_i32 (param externref i32 i32 i32)))
   (import "bindings" "dv_set_i16"
      (func $dv_set_i16 (param externref i32 i32 i32)))
   (import "bindings" "dv_set_i8"
      (func $dv_set_i8 (param externref i32 i32)))
   (import "bindings" "littleEndian" (global $littleEndian i32))
   (import "fail" "caml_bound_error" (func $caml_bound_error))
   (import "fail" "caml_raise_out_of_memory" (func $caml_raise_out_of_memory))
   (import "fail" "caml_invalid_argument"
      (func $caml_invalid_argument (param (ref eq))))
   (import "fail" "caml_failwith" (func $caml_failwith (param (ref eq))))
   (import "jslib" "wrap" (func $wrap (param anyref) (result (ref eq))))
   (import "jslib" "unwrap" (func $unwrap (param (ref eq)) (result anyref)))
   (import "int32" "caml_copy_int32"
      (func $caml_copy_int32 (param i32) (result (ref eq))))
   (import "int32" "Int32_val"
      (func $Int32_val (param (ref eq)) (result i32)))
   (import "int32" "caml_copy_nativeint"
      (func $caml_copy_nativeint (param i32) (result (ref eq))))
   (import "int64" "caml_copy_int64"
      (func $caml_copy_int64 (param i64) (result (ref eq))))
   (import "int64" "Int64_val"
      (func $Int64_val (param (ref eq)) (result i64)))
   (import "obj" "double_array_tag" (global $double_array_tag i32))
   (import "compare" "unordered" (global $unordered i32))
   (import "hash" "caml_hash_mix_int"
      (func $caml_hash_mix_int (param i32) (param i32) (result i32)))
   (import "hash" "caml_hash_mix_int64"
      (func $caml_hash_mix_int64 (param i32) (param i64) (result i32)))
   (import "hash" "caml_hash_mix_double"
      (func $caml_hash_mix_double (param i32) (param f64) (result i32)))
   (import "hash" "caml_hash_mix_float"
      (func $caml_hash_mix_float (param i32) (param f32) (result i32)))
   (import "hash" "caml_hash_mix_float16"
      (func $caml_hash_mix_float16 (param i32) (param i32) (result i32)))
   (import "marshal" "caml_serialize_int_1"
      (func $caml_serialize_int_1 (param (ref eq)) (param i32)))
   (import "marshal" "caml_serialize_int_2"
      (func $caml_serialize_int_2 (param (ref eq)) (param i32)))
   (import "marshal" "caml_serialize_int_4"
      (func $caml_serialize_int_4 (param (ref eq)) (param i32)))
   (import "marshal" "caml_serialize_int_8"
      (func $caml_serialize_int_8 (param (ref eq)) (param i64)))
   (import "marshal" "caml_deserialize_uint_1"
      (func $caml_deserialize_uint_1 (param (ref eq)) (result i32)))
   (import "marshal" "caml_deserialize_sint_1"
      (func $caml_deserialize_sint_1 (param (ref eq)) (result i32)))
   (import "marshal" "caml_deserialize_uint_2"
      (func $caml_deserialize_uint_2 (param (ref eq)) (result i32)))
   (import "marshal" "caml_deserialize_sint_2"
      (func $caml_deserialize_sint_2 (param (ref eq)) (result i32)))
   (import "marshal" "caml_deserialize_int_4"
      (func $caml_deserialize_int_4 (param (ref eq)) (result i32)))
   (import "marshal" "caml_deserialize_int_8"
      (func $caml_deserialize_int_8 (param (ref eq)) (result i64)))

   (type $block (array (mut (ref eq))))
   (type $bytes (array (mut i8)))
   (type $float (struct (field f64)))
   (type $float_array (array (mut f64)))

   (type $compare
      (func (param (ref eq)) (param (ref eq)) (param i32) (result i32)))
   (type $hash
      (func (param (ref eq)) (result i32)))
   (type $fixed_length (struct (field $bsize_32 i32) (field $bsize_64 i32)))
   (type $serialize
      (func (param (ref eq)) (param (ref eq)) (result i32) (result i32)))
   (type $deserialize (func (param (ref eq)) (result (ref eq)) (result i32)))
   (type $dup (func (param (ref eq)) (result (ref eq))))
   (type $custom_operations
      (struct
         (field $id (ref $bytes))
         (field $compare (ref null $compare))
         (field $compare_ext (ref null $compare))
         (field $hash (ref null $hash))
         (field $fixed_length (ref null $fixed_length))
         (field $serialize (ref null $serialize))
         (field $deserialize (ref null $deserialize))
         (field $dup (ref null $dup))))
   (type $custom (sub (struct (field (ref $custom_operations)))))

   (global $bigarray_ops (export "bigarray_ops") (ref $custom_operations)
      (struct.new $custom_operations
         (@string "_bigarr02")
         (ref.func $caml_ba_compare)
         (ref.null $compare)
         (ref.func $bigarray_hash)
         (ref.null $fixed_length)
         (ref.func $bigarray_serialize)
         (ref.func $bigarray_deserialize)
         (ref.null $dup)))

   (type $int_array (array (mut i32)))

   (type $bigarray
      (sub final $custom
         (struct
            (field (ref $custom_operations))
            (field $ba_data (mut (ref extern))) ;; data
            (field $ba_view (mut (ref extern))) ;; view
            (field $ba_dim (ref $int_array)) ;; size in each dimension
            (field $ba_num_dims i8) ;; number of dimensions
            (field $ba_kind i8) ;; kind
            (field $ba_layout i8)))) ;; layout

   (func $double_to_float16 (param $f f64) (result i32)
      (local $x i32) (local $sign i32) (local $o i32)
      (local.set $x (i32.reinterpret_f32 (f32.demote_f64 (local.get $f))))
      (local.set $sign (i32.and (local.get $x) (i32.const 0x80000000)))
      (local.set $x (i32.xor (local.get $x) (local.get $sign)))
      (if (i32.ge_u (local.get $x) (i32.const 0x47800000))
         (then
            (local.set $o
               (select
                  (i32.const 0x7E00) ;; NaN
                  (i32.const 0x7C00) ;; infinity
                  (i32.gt_u (local.get $x) (i32.const 0x7f800000)))))
         (else
            (if (i32.lt_u (local.get $x) (i32.const 0x38800000))
               (then
                  (local.set $o
                     (i32.sub
                        (i32.reinterpret_f32
                            (f32.add (f32.reinterpret_i32 (local.get $x))
                               (f32.const 0.5)))
                        (i32.const 0x3f000000))))
               (else
                  (local.set $o
                     (i32.shr_u
                        (i32.add (i32.add (local.get $x) (i32.const 0xC8000FFF))
                           (i32.and (i32.shr_u (local.get $x) (i32.const 13))
                              (i32.const 1)))
                        (i32.const 13)))))))
      (i32.or (local.get $o) (i32.shr_u (local.get $sign) (i32.const 16))))

   (func $float16_to_double (param $d i32) (result f64)
      (local $f f32)
      (local.set $f
         (f32.mul
            (f32.reinterpret_i32
               ;; exponent and mantissa
               (i32.shl (i32.and (local.get $d) (i32.const 0x7FFF))
                  (i32.const 13)))
            (f32.const 0x1p+112)))
      (if (f32.ge (local.get $f) (f32.const 65536))
         (then
            ;; NaN / infinity
            (local.set $f
               (f32.reinterpret_i32
                  (i32.or (i32.reinterpret_f32 (local.get $f))
                     (i32.const 0x7f800000))))))
      (f64.promote_f32
         (f32.reinterpret_i32
            (i32.or (i32.reinterpret_f32 (local.get $f))
               ;; sign bit
               (i32.shl (i32.and (local.get $d) (i32.const 0x8000))
                  (i32.const 16))))))

   (func $bigarray_hash (param (ref eq)) (result i32)
      (local $b (ref $bigarray))
      (local $h i32) (local $len i32) (local $i i32) (local $w i32)
      (local $data (ref extern))
      (local $view (ref extern))
      (local.set $b (ref.cast (ref $bigarray) (local.get 0)))
      (local.set $data (struct.get $bigarray $ba_data (local.get $b)))
      (local.set $view (struct.get $bigarray $ba_view (local.get $b)))
      (local.set $len (call $ta_length (local.get $data)))
      (block $float64
       (block $float32
        (block $int8
         (block $int16
          (block $int32
           (block $int64
            (block $float16
             (br_table $float32 $float64 $int8 $int8 $int16 $int16
                       $int32 $int64 $int32 $int32
                       $float32 $float64 $int8 $float16
                (struct.get $bigarray $ba_kind (local.get $b))))
            ;; float16
            (local.set $len (i32.shl (local.get $len) (i32.const 1)))
            (if (i32.gt_u (local.get $len) (i32.const 256))
               (then (local.set $len (i32.const 256))))
            (loop $loop
               (if (i32.lt_u (local.get $i) (local.get $len))
                  (then
                     (local.set $h
                        (call $caml_hash_mix_float16 (local.get $h)
                           (call $dv_get_ui16
                              (local.get $view)
                              (local.get $i)
                              (global.get $littleEndian))))
                     (local.set $i (i32.add (local.get $i) (i32.const 2)))
                     (br $loop))))
            (return (local.get $h)))
           ;; int64
           (local.set $len (i32.shl (local.get $len) (i32.const 2)))
           (if (i32.gt_u (local.get $len) (i32.const 256))
              (then (local.set $len (i32.const 256))))
           (loop $loop
              (if (i32.lt_u (local.get $i) (local.get $len))
                 (then
                    (local.set $h
                       (call $caml_hash_mix_int64 (local.get $h)
                          (call $dv_get_i64
                             (local.get $view)
                             (local.get $i)
                             (global.get $littleEndian))))
                    (local.set $i (i32.add (local.get $i) (i32.const 8)))
                    (br $loop))))
           (return (local.get $h)))
          ;; int32
          (local.set $len (i32.shl (local.get $len) (i32.const 2)))
          (if (i32.gt_u (local.get $len) (i32.const 256))
             (then (local.set $len (i32.const 256))))
          (loop $loop
             (if (i32.lt_u (local.get $i) (local.get $len))
                (then
                   (local.set $h
                      (call $caml_hash_mix_int (local.get $h)
                         (call $dv_get_i32
                            (local.get $view)
                            (local.get $i)
                            (global.get $littleEndian))))
                   (local.set $i (i32.add (local.get $i) (i32.const 4)))
                   (br $loop))))
          (return (local.get $h)))
         ;; int16 / uint16
         (local.set $len (i32.shl (local.get $len) (i32.const 1)))
         (if (i32.gt_u (local.get $len) (i32.const 256))
            (then (local.set $len (i32.const 256))))
         (loop $loop
            (if (i32.le_u (i32.add (local.get $i) (i32.const 4))
                   (local.get $len))
               (then
                  (local.set $h
                     (call $caml_hash_mix_int
                        (local.get $h)
                        (i32.or
                           (call $dv_get_ui16
                              (local.get $view)
                              (local.get $i)
                              (global.get $littleEndian))
                           (i32.shl
                              (call $dv_get_ui16
                                 (local.get $view)
                                 (i32.add (local.get $i) (i32.const 2))
                                 (global.get $littleEndian))
                              (i32.const 16)))))
                  (local.set $i (i32.add (local.get $i) (i32.const 4)))
                  (br $loop))))
         (if (i32.and (local.get $len) (i32.const 2))
            (then
               (local.set $h
                  (call $caml_hash_mix_int (local.get $h)
                     (call $dv_get_ui16
                        (local.get $view)
                        (local.get $i)
                        (global.get $littleEndian))))))
         (return (local.get $h)))
        ;; int8 / uint8
        (if (i32.gt_u (local.get $len) (i32.const 256))
           (then (local.set $len (i32.const 256))))
        (loop $loop
           (if (i32.le_u (i32.add (local.get $i) (i32.const 4)) (local.get $len))
              (then
                 (local.set $h
                    (call $caml_hash_mix_int
                       (local.get $h)
                       (call $dv_get_i32
                          (local.get $view) (local.get $i) (i32.const 1))))
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
                 (call $dv_get_i8 (local.get $view) (local.get $i))))
           (local.set $h
              (call $caml_hash_mix_int (local.get $h) (local.get $w))))
        (return (local.get $h)))
       ;; float32
       (local.set $len (i32.shl (local.get $len) (i32.const 2)))
       (if (i32.gt_u (local.get $len) (i32.const 256))
          (then (local.set $len (i32.const 256))))
       (loop $loop
          (if (i32.lt_u (local.get $i) (local.get $len))
             (then
                (local.set $h
                   (call $caml_hash_mix_float (local.get $h)
                      (call $dv_get_f32 (local.get $view) (local.get $i)
                         (global.get $littleEndian))))
                (local.set $i (i32.add (local.get $i) (i32.const 4)))
                (br $loop))))
       (return (local.get $h)))
      ;; float64
      (local.set $len (i32.shl (local.get $len) (i32.const 3)))
      (if (i32.gt_u (local.get $len) (i32.const 256))
         (then (local.set $len (i32.const 256))))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $len))
            (then
               (local.set $h
                  (call $caml_hash_mix_double (local.get $h)
                     (call $dv_get_f64 (local.get $view) (local.get $i)
                        (global.get $littleEndian))))
               (local.set $i (i32.add (local.get $i) (i32.const 8)))
               (br $loop))))
      (return (local.get $h)))

   (func $bigarray_serialize
      (param $s (ref eq)) (param $v (ref eq)) (result i32) (result i32)
      (local $b (ref $bigarray))
      (local $num_dims i32) (local $dim (ref $int_array))
      (local $data (ref extern))
      (local $view (ref extern))
      (local $i i32) (local $len i32)
      (local.set $b (ref.cast (ref $bigarray) (local.get $v)))
      (local.set $num_dims (struct.get $bigarray $ba_num_dims (local.get $b)))
      (local.set $dim (struct.get $bigarray $ba_dim (local.get $b)))
      (call $caml_serialize_int_4 (local.get $s) (local.get $num_dims))
      (call $caml_serialize_int_4 (local.get $s)
         (i32.or (struct.get $bigarray $ba_kind (local.get $b))
            (i32.shl (struct.get $bigarray $ba_layout (local.get $b))
               (i32.const 8))))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $num_dims))
            (then
               (local.set $len
                  (array.get $int_array (local.get $dim) (local.get $i)))
               (if (i32.lt_u (local.get $len) (i32.const 0xffff))
                  (then
                     (call $caml_serialize_int_2 (local.get $s)
                        (local.get $len)))
                  (else
                     (call $caml_serialize_int_2 (local.get $s)
                        (i32.const 0xffff))
                     (call $caml_serialize_int_8 (local.get $s)
                        (i64.extend_i32_u (local.get $len)))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (block $done
       (local.set $data (struct.get $bigarray $ba_data (local.get $b)))
       (local.set $view (struct.get $bigarray $ba_view (local.get $b)))
       (local.set $len (call $ta_length (local.get $data)))
       (local.set $i (i32.const 0))
       (block $float64
        (block $int8
         (block $int16
          (block $int32
           (block $int
            (block $int64
             (br_table $int32 $float64 $int8 $int8 $int16 $int16
                       $int32 $int64 $int $int
                       $int32 $float64 $int8 $int16
                (struct.get $bigarray $ba_kind (local.get $b))))
            ;; int64
            (local.set $len (i32.shl (local.get $len) (i32.const 2)))
            (loop $loop
               (if (i32.lt_u (local.get $i) (local.get $len))
                  (then
                     (call $caml_serialize_int_8 (local.get $s)
                        (call $dv_get_i64 (local.get $view)
                           (local.get $i)
                           (global.get $littleEndian)))
                     (local.set $i (i32.add (local.get $i) (i32.const 8)))
                     (br $loop))))
            (br $done))
           ;; int
           (call $caml_serialize_int_1 (local.get $s) (i32.const 0)))
          ;; int32 / float32
          (local.set $len (i32.shl (local.get $len) (i32.const 2)))
          (loop $loop
             (if (i32.lt_u (local.get $i) (local.get $len))
                (then
                   (call $caml_serialize_int_4 (local.get $s)
                      (call $dv_get_i32 (local.get $view) (local.get $i)
                         (global.get $littleEndian)))
                   (local.set $i (i32.add (local.get $i) (i32.const 4)))
                   (br $loop))))
          (br $done))
          ;; int16 / uint16 / float16
          (local.set $len (i32.shl (local.get $len) (i32.const 1)))
          (loop $loop
             (if (i32.lt_u (local.get $i) (local.get $len))
                (then
                   (call $caml_serialize_int_2 (local.get $s)
                      (call $dv_get_i16 (local.get $view) (local.get $i)
                         (global.get $littleEndian)))
                   (local.set $i (i32.add (local.get $i) (i32.const 2)))
                   (br $loop))))
          (br $done))
        ;; int8 / uint8
        (loop $loop
           (if (i32.lt_u (local.get $i) (local.get $len))
              (then
                 (call $caml_serialize_int_1 (local.get $s)
                    (call $dv_get_i8 (local.get $view) (local.get $i)))
                 (local.set $i (i32.add (local.get $i) (i32.const 1)))
                 (br $loop))))
        (br $done))
       ;; float64
       (local.set $len (i32.shl (local.get $len) (i32.const 3)))
       (loop $loop
          (if (i32.lt_u (local.get $i) (local.get $len))
             (then
                (call $caml_serialize_int_8 (local.get $s)
                   (call $dv_get_i64 (local.get $view) (local.get $i)
                      (global.get $littleEndian)))
                (local.set $i (i32.add (local.get $i) (i32.const 8)))
                (br $loop)))))
      (tuple.make 2
         (i32.mul (i32.add (i32.const 4) (local.get $num_dims)) (i32.const 4))
         (i32.mul (i32.add (i32.const 4) (local.get $num_dims)) (i32.const 8))))

   (@string $intern_overflow
      "input_value: cannot read bigarray with 64-bit OCaml ints")

   (func $bigarray_deserialize
      (param $s (ref eq)) (result (ref eq)) (result i32)
      (local $b (ref $bigarray))
      (local $num_dims i32) (local $dim (ref $int_array))
      (local $flags i32) (local $kind i32)
      (local $data (ref extern))
      (local $view (ref extern))
      (local $i i32) (local $len i32)
      (local $l i64)
      (local.set $num_dims (call $caml_deserialize_int_4 (local.get $s)))
      (local.set $flags (call $caml_deserialize_int_4 (local.get $s)))
      (local.set $kind (i32.and (local.get $flags) (i32.const 0xff)))
      (local.set $dim (array.new $int_array (i32.const 0) (local.get $num_dims)))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $num_dims))
            (then
               (local.set $len
                  (call $caml_deserialize_uint_2 (local.get $s)))
               (if (i32.eq (local.get $len) (i32.const 0xffff))
                  (then
                     ;; ZZZ overflows?
                     (local.set $len
                        (i32.wrap_i64
                           (call $caml_deserialize_int_8 (local.get $s))))))
               (array.set $int_array (local.get $dim) (local.get $i)
                  (local.get $len))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (local.set $data
         (call $caml_ba_create_buffer (local.get $kind)
            (call $caml_ba_get_size (local.get $dim))))
      (local.set $view (call $dv_make (local.get $data)))
      (local.set $b
         (struct.new $bigarray
            (global.get $bigarray_ops)
            (local.get $data)
            (local.get $view)
            (local.get $dim)
            (local.get $num_dims)
            (local.get $kind)
            (i32.shr_u (local.get $flags) (i32.const 8))))
      (block $done
       (local.set $len (call $ta_length (local.get $data)))
       (local.set $i (i32.const 0))
       (block $float64
        (block $int8
         (block $int16
          (block $int32
           (block $int
            (block $int64
             (br_table $int32 $float64 $int8 $int8 $int16 $int16
                       $int32 $int64 $int $int
                       $int32 $float64 $int8 $int16
                (struct.get $bigarray $ba_kind (local.get $b))))
            ;; int64
            (local.set $len (i32.shl (local.get $len) (i32.const 2)))
            (loop $loop
               (if (i32.lt_u (local.get $i) (local.get $len))
                  (then
                     (call $dv_set_i64 (local.get $view) (local.get $i)
                        (call $caml_deserialize_int_8 (local.get $s))
                        (global.get $littleEndian))
                     (local.set $i (i32.add (local.get $i) (i32.const 8)))
                     (br $loop))))
            (br $done))
           ;; int
           (if (call $caml_deserialize_uint_1 (local.get $s))
              (then (call $caml_failwith (global.get $intern_overflow)))))
          ;; int32 / float32
          (local.set $len (i32.shl (local.get $len) (i32.const 2)))
          (loop $loop
             (if (i32.lt_u (local.get $i) (local.get $len))
                (then
                   (call $dv_set_i32 (local.get $view) (local.get $i)
                      (call $caml_deserialize_int_4 (local.get $s))
                      (global.get $littleEndian))
                   (local.set $i (i32.add (local.get $i) (i32.const 4)))
                   (br $loop))))
          (br $done))
         ;; int16 / uint16 / float16
         (local.set $len (i32.shl (local.get $len) (i32.const 1)))
         (loop $loop
            (if (i32.lt_u (local.get $i) (local.get $len))
               (then
                  (call $dv_set_i16 (local.get $view) (local.get $i)
                     (call $caml_deserialize_sint_2 (local.get $s))
                     (global.get $littleEndian))
                  (local.set $i (i32.add (local.get $i) (i32.const 2)))
                  (br $loop))))
         (br $done))
        ;; int8 / uint8
        (loop $loop
           (if (i32.lt_u (local.get $i) (local.get $len))
              (then
                 (call $dv_set_i8 (local.get $view) (local.get $i)
                    (call $caml_deserialize_sint_1 (local.get $s)))
                 (local.set $i (i32.add (local.get $i) (i32.const 1)))
                 (br $loop))))
        (br $done))
       ;; float64
       (local.set $len (i32.shl (local.get $len) (i32.const 3)))
       (loop $loop
          (if (i32.lt_u (local.get $i) (local.get $len))
             (then
                (call $dv_set_i64 (local.get $view) (local.get $i)
                   (call $caml_deserialize_int_8 (local.get $s))
                   (global.get $littleEndian))
                (local.set $i (i32.add (local.get $i) (i32.const 8)))
                (br $loop)))))
      (tuple.make 2
         (local.get $b)
         (i32.mul (i32.add (i32.const 4) (local.get $num_dims)) (i32.const 4))))

   (func $caml_ba_get_size (param $dim (ref $int_array)) (result i32)
      (local $i i32) (local $n i32) (local $sz i64)
      (local.set $n (array.len (local.get $dim)))
      (local.set $i (i32.const 0))
      (local.set $sz (i64.const 1))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $n))
            (then
               (local.set $sz
                   (i64.mul (local.get $sz)
                      (i64.extend_i32_s
                         (array.get $int_array
                            (local.get $dim) (local.get $i)))))
               (if (i64.ne (local.get $sz)
                      (i64.extend_i32_s (i32.wrap_i64 (local.get $sz))))
                  (then (call $caml_raise_out_of_memory)))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (i32.wrap_i64 (local.get $sz)))

  (func $caml_ba_size_per_element (param $kind i32) (result i32)
     (select (i32.const 2) (i32.const 1)
        (i32.or (i32.eq (local.get $kind) (i32.const 7))
                (i32.or (i32.eq (local.get $kind) (i32.const 10))
                        (i32.eq (local.get $kind) (i32.const 11))))))

  (func $caml_ba_create_buffer (export "caml_ba_create_buffer")
     (param $kind i32) (param $sz i32) (result (ref extern))
     (local $l i64)
     (local.set $l
        (i64.mul (i64.extend_i32_s (local.get $sz))
           (i64.extend_i32_s
              (call $caml_ba_size_per_element (local.get $kind)))))
     (if (i64.ne (local.get $l) (i64.extend_i32_s (i32.wrap_i64 (local.get $l))))
        (then (call $caml_raise_out_of_memory)))
     (return_call $ta_create (local.get $kind) (i32.wrap_i64 (local.get $l))))

   (global $CAML_BA_MAX_NUM_DIMS i32 (i32.const 16))

   (@string $ba_create_bad_dims "Bigarray.create: bad number of dimensions")
   (@string $ba_create_negative_dim "Bigarray.create: negative dimension")

   (func (export "caml_ba_create")
      (param $vkind (ref eq)) (param $layout (ref eq)) (param $d (ref eq))
      (result (ref eq))
      (local $vdim (ref $block))
      (local $data (ref extern)) (local $dim (ref $int_array))
      (local $kind i32) (local $num_dims i32) (local $i i32) (local $n i32)
      (local.set $kind (i31.get_s (ref.cast (ref i31) (local.get $vkind))))
      (local.set $vdim (ref.cast (ref $block) (local.get $d)))
      (local.set $num_dims (i32.sub (array.len (local.get $vdim)) (i32.const 1)))
      (if (i32.gt_u (local.get $num_dims) (global.get $CAML_BA_MAX_NUM_DIMS))
         (then (call $caml_invalid_argument (global.get $ba_create_bad_dims))))
      (local.set $dim
         (array.new $int_array (i32.const 0) (local.get $num_dims)))
      (local.set $i (i32.const 0))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $num_dims))
            (then
               (local.set $n
                  (i31.get_s
                     (ref.cast (ref i31)
                        (array.get $block (local.get $vdim)
                           (i32.add (local.get $i) (i32.const 1))))))
               (if (i32.lt_s (local.get $n) (i32.const 0))
                  (then
                     (call $caml_invalid_argument
                        (global.get $ba_create_negative_dim))))
               (array.set $int_array
                  (local.get $dim) (local.get $i) (local.get $n))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (local.set $data
         (call $caml_ba_create_buffer (local.get $kind)
            (call $caml_ba_get_size (local.get $dim))))
      (struct.new $bigarray
         (global.get $bigarray_ops)
         (local.get $data)
         (call $dv_make (local.get $data))
         (local.get $dim)
         (local.get $num_dims)
         (local.get $kind)
         (i31.get_s (ref.cast (ref i31) (local.get $layout)))))

   (@string $ta_unsupported_kind "Typed_array.to_genarray: unsupported kind")
   (@string $ta_too_large "Typed_array.to_genarray: too large")

   (func (export "caml_ba_from_typed_array") (param (ref eq)) (result (ref eq))
      (local $data (ref extern))
      (local $kind i32)
      (local $len i32)
      (local.set $data
         (call $ta_normalize
            (ref.as_non_null (extern.convert_any (call $unwrap (local.get 0))))))
      (local.set $kind (call $ta_kind (local.get $data)))
      (if (i32.lt_s (local.get $kind) (i32.const 0))
         (then (call $caml_invalid_argument (global.get $ta_unsupported_kind))))
      (if (i32.eq (local.get $kind) (i32.const 14)) ;; Uint8ClampedArray
         (then (local.set $kind (i32.const 3))))
      (local.set $len (call $ta_length (local.get $data)))
      (if (i32.lt_s (local.get $len) (i32.const 0))
         (then (call $caml_invalid_argument (global.get $ta_too_large))))
      (struct.new $bigarray
         (global.get $bigarray_ops)
         (local.get $data)
         (call $dv_make (local.get $data))
         (array.new_fixed $int_array 1 (local.get $len))
         (i32.const 1)
         (local.get $kind)
         (i32.const 0)))

   (func (export "caml_ba_to_typed_array") (param (ref eq)) (result (ref eq))
      (call $wrap
         (any.convert_extern
            (struct.get $bigarray $ba_data
               (ref.cast (ref $bigarray) (local.get 0))))))

   (func $caml_ba_get_at_offset
      (param $ba (ref $bigarray)) (param $i i32) (result (ref eq))
      (local $view (ref extern))
      (local.set $view (struct.get $bigarray $ba_view (local.get $ba)))
      (block $float32
       (block $float64
        (block $int8
         (block $uint8
          (block $int16
           (block $uint16
            (block $int32
             (block $int64
              (block $int
               (block $nativeint
                (block $complex32
                 (block $complex64
                  (block $float16
                   (br_table $float32 $float64 $int8 $uint8 $int16 $uint16
                             $int32 $int64 $int $nativeint
                             $complex32 $complex64 $uint8 $float16
                      (struct.get $bigarray $ba_kind (local.get $ba))))
                  ;; float16
                  (return
                     (struct.new $float
                        (call $float16_to_double
                           (call $dv_get_ui16
                              (local.get $view)
                              (i32.shl (local.get $i) (i32.const 1))
                              (global.get $littleEndian))))))
                 ;; complex64
                 (local.set $i (i32.shl (local.get $i) (i32.const 4)))
                 (return
                    (array.new_fixed $float_array 2
                       (call $dv_get_f64 (local.get $view) (local.get $i)
                          (global.get $littleEndian))
                       (call $dv_get_f64 (local.get $view)
                          (i32.add (local.get $i) (i32.const 8))
                          (global.get $littleEndian)))))
                ;; complex32
                (local.set $i (i32.shl (local.get $i) (i32.const 3)))
                (return
                   (array.new_fixed $float_array 2
                      (f64.promote_f32
                         (call $dv_get_f32 (local.get $view) (local.get $i)
                            (global.get $littleEndian)))
                      (f64.promote_f32
                         (call $dv_get_f32 (local.get $view)
                            (i32.add (local.get $i) (i32.const 4))
                            (global.get $littleEndian))))))
               ;; nativeint
               (return_call $caml_copy_nativeint
                  (call $dv_get_i32
                     (local.get $view) (i32.shl (local.get $i) (i32.const 2))
                     (global.get $littleEndian))))
              ;; int
              (return
                 (ref.i31
                    (call $dv_get_i32
                       (local.get $view) (i32.shl (local.get $i) (i32.const 2))
                       (global.get $littleEndian)))))
             ;; int64
             (return_call $caml_copy_int64
                (call $dv_get_i64
                   (local.get $view) (i32.shl (local.get $i) (i32.const 3))
                   (global.get $littleEndian))))
            ;; int32
            (return_call $caml_copy_int32
               (call $dv_get_i32
                  (local.get $view) (i32.shl (local.get $i) (i32.const 2))
                  (global.get $littleEndian))))
           ;; uint16
           (return
              (ref.i31
                 (call $dv_get_ui16
                    (local.get $view) (i32.shl (local.get $i) (i32.const 1))
                    (global.get $littleEndian)))))
          ;; int16
          (return
             (ref.i31
                (call $dv_get_i16
                   (local.get $view) (i32.shl (local.get $i) (i32.const 1))
                   (global.get $littleEndian)))))
         ;; uint8
         (return (ref.i31
                    (call $dv_get_ui8 (local.get $view) (local.get $i)))))
        ;; int8
        (return (ref.i31
                   (call $dv_get_i8 (local.get $view) (local.get $i)))))
       ;; float64
       (return
          (struct.new $float
             (call $dv_get_f64
                (local.get $view) (i32.shl (local.get $i) (i32.const 3))
                (global.get $littleEndian)))))
      ;; float32
      (return
         (struct.new $float
             (f64.promote_f32
                (call $dv_get_f32
                   (local.get $view) (i32.shl (local.get $i) (i32.const 2))
                   (global.get $littleEndian))))))

   (func $caml_ba_set_at_offset
      (param $ba (ref $bigarray)) (param $i i32) (param $v (ref eq))
      (local $view (ref extern))
      (local $b (ref $float_array)) (local $l i64)
      (local.set $view (struct.get $bigarray $ba_view (local.get $ba)))
      (block $float32
       (block $float64
        (block $int8
         (block $int16
          (block $int64
           (block $int
            (block $int32
             (block $complex32
              (block $complex64
               (block $float16
                (br_table $float32 $float64 $int8 $int8 $int16 $int16
                          $int32 $int64 $int $int32
                          $complex32 $complex64 $int8 $float16
                   (struct.get $bigarray $ba_kind (local.get $ba))))
               ;; float16
               (call $dv_set_i16
                 (local.get $view) (i32.shl (local.get $i) (i32.const 1))
                 (call $double_to_float16
                     (struct.get $float 0
                        (ref.cast (ref $float) (local.get $v))))
                 (global.get $littleEndian))
               (return))
              ;; complex64
              (local.set $i (i32.shl (local.get $i) (i32.const 4)))
              (local.set $b (ref.cast (ref $float_array) (local.get $v)))
              (call $dv_set_f64 (local.get $view) (local.get $i)
                 (array.get $float_array (local.get $b) (i32.const 0))
                 (global.get $littleEndian))
              (call $dv_set_f64 (local.get $view)
                 (i32.add (local.get $i) (i32.const 8))
                 (array.get $float_array (local.get $b) (i32.const 1))
                 (global.get $littleEndian))
              (return))
             ;; complex32
             (local.set $i (i32.shl (local.get $i) (i32.const 3)))
             (local.set $b (ref.cast (ref $float_array) (local.get $v)))
             (call $dv_set_f32 (local.get $view) (local.get $i)
                (f32.demote_f64
                   (array.get $float_array (local.get $b) (i32.const 0)))
                (global.get $littleEndian))
             (call $dv_set_f32 (local.get $view)
                (i32.add (local.get $i) (i32.const 4))
                (f32.demote_f64
                   (array.get $float_array (local.get $b) (i32.const 1)))
                (global.get $littleEndian))
             (return))
            ;; int32 / nativeint
            (call $dv_set_i32
               (local.get $view) (i32.shl (local.get $i) (i32.const 2))
               (call $Int32_val (local.get $v))
               (global.get $littleEndian))
            (return))
           ;; int
           (call $dv_set_i32
              (local.get $view) (i32.shl (local.get $i) (i32.const 2))
              (i31.get_s (ref.cast (ref i31) (local.get $v)))
              (global.get $littleEndian))
           (return))
          ;; int64
          (local.set $l (call $Int64_val (local.get $v)))
          (call $dv_set_i64
             (local.get $view) (i32.shl (local.get $i) (i32.const 3))
             (call $Int64_val (local.get $v))
             (global.get $littleEndian))
          (return))
         ;; int16/ uint16
         (call $dv_set_i16
            (local.get $view) (i32.shl (local.get $i) (i32.const 1))
            (i31.get_s (ref.cast (ref i31) (local.get $v)))
            (global.get $littleEndian))
         (return))
        ;; int8 / uint8
        (call $dv_set_i8 (local.get $view) (local.get $i)
           (i31.get_s (ref.cast (ref i31) (local.get $v))))
        (return))
       ;; float64
       (call $dv_set_f64 (local.get $view) (i32.shl (local.get $i) (i32.const 3))
          (struct.get $float 0 (ref.cast (ref $float) (local.get $v)))
          (global.get $littleEndian))
       (return))
      ;; float32
      (call $dv_set_f32 (local.get $view) (i32.shl (local.get $i) (i32.const 2))
         (f32.demote_f64
            (struct.get $float 0 (ref.cast (ref $float) (local.get $v))))
         (global.get $littleEndian))
      (return))

   (@string $Bigarray_dim "Bigarray.dim")

   (func $caml_ba_dim (export "caml_ba_dim")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (local $dim (ref $int_array))
      (local $i i32)
      (local.set $dim
         (struct.get $bigarray $ba_dim
            (ref.cast (ref $bigarray) (local.get 0))))
      (local.set $i (i31.get_s (ref.cast (ref i31) (local.get 1))))
      (if (i32.ge_u (local.get $i) (array.len (local.get $dim)))
         (then (call $caml_invalid_argument (global.get $Bigarray_dim))))
      (ref.i31 (array.get $int_array (local.get $dim) (local.get $i))))

   (func (export "caml_ba_dim_1") (param (ref eq)) (result (ref eq))
      (return_call $caml_ba_dim (local.get 0) (ref.i31 (i32.const 0))))

   (func (export "caml_ba_get_1")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (local $ba (ref $bigarray))
      (local $i i32)
      (local.set $ba (ref.cast (ref $bigarray) (local.get 0)))
      (local.set $i (i31.get_u (ref.cast (ref i31) (local.get 1))))
      (if (struct.get $bigarray $ba_layout (local.get $ba))
         (then (local.set $i (i32.sub (local.get $i) (i32.const 1)))))
      (if (i32.ge_u (local.get $i)
             (array.get $int_array (struct.get $bigarray $ba_dim (local.get $ba))
                (i32.const 0)))
         (then (call $caml_bound_error)))
      (return_call $caml_ba_get_at_offset (local.get $ba) (local.get $i)))

   (func (export "caml_ba_set_1")
      (param (ref eq)) (param (ref eq)) (param $v (ref eq)) (result (ref eq))
      (local $ba (ref $bigarray))
      (local $i i32)
      (local.set $ba (ref.cast (ref $bigarray) (local.get 0)))
      (local.set $i (i31.get_u (ref.cast (ref i31) (local.get 1))))
      (if (struct.get $bigarray $ba_layout (local.get $ba))
         (then (local.set $i (i32.sub (local.get $i) (i32.const 1)))))
      (if (i32.ge_u (local.get $i)
             (array.get $int_array (struct.get $bigarray $ba_dim (local.get $ba))
                (i32.const 0)))
         (then (call $caml_bound_error)))
      (call $caml_ba_set_at_offset
         (local.get $ba) (local.get $i) (local.get $v))
      (ref.i31 (i32.const 0)))

   (func (export "caml_ba_get_2")
      (param $vba (ref eq)) (param $vi (ref eq)) (param $vj (ref eq))
      (result (ref eq))
      (local $ba (ref $bigarray))
      (local $i i32)
      (local $j i32)
      (local $offset i32)
      (local $dim (ref $int_array))
      (local.set $ba (ref.cast (ref $bigarray) (local.get $vba)))
      (local.set $i (i31.get_u (ref.cast (ref i31) (local.get $vi))))
      (local.set $j (i31.get_u (ref.cast (ref i31) (local.get $vj))))
      (local.set $dim (struct.get $bigarray $ba_dim (local.get $ba)))
      (if (struct.get $bigarray $ba_layout (local.get $ba))
         (then
            (local.set $i (i32.sub (local.get $i) (i32.const 1)))
            (local.set $j (i32.sub (local.get $j) (i32.const 1)))
            (local.set $offset
               (i32.add
                  (i32.mul (local.get $j)
                     (array.get $int_array (local.get $dim) (i32.const 0)))
                  (local.get $i))))
         (else
            (local.set $offset
               (i32.add
                  (i32.mul (local.get $i)
                     (array.get $int_array (local.get $dim) (i32.const 1)))
                  (local.get $j)))))
      (if (i32.or
             (i32.ge_u (local.get $i)
                (array.get $int_array (local.get $dim) (i32.const 0)))
             (i32.ge_u (local.get $j)
                (array.get $int_array (local.get $dim) (i32.const 1))))
         (then
            (call $caml_bound_error)))
      (return_call $caml_ba_get_at_offset (local.get $ba) (local.get $offset)))

   (func (export "caml_ba_set_2")
      (param $vba (ref eq)) (param $vi (ref eq)) (param $vj (ref eq))
      (param $v (ref eq)) (result (ref eq))
      (local $ba (ref $bigarray))
      (local $i i32)
      (local $j i32)
      (local $offset i32)
      (local $dim (ref $int_array))
      (local.set $ba (ref.cast (ref $bigarray) (local.get $vba)))
      (local.set $i (i31.get_u (ref.cast (ref i31) (local.get $vi))))
      (local.set $j (i31.get_u (ref.cast (ref i31) (local.get $vj))))
      (local.set $dim (struct.get $bigarray $ba_dim (local.get $ba)))
      (if (struct.get $bigarray $ba_layout (local.get $ba))
         (then
            (local.set $i (i32.sub (local.get $i) (i32.const 1)))
            (local.set $j (i32.sub (local.get $j) (i32.const 1)))
            (local.set $offset
               (i32.add
                  (i32.mul (local.get $j)
                     (array.get $int_array (local.get $dim) (i32.const 0)))
                  (local.get $i))))
         (else
            (local.set $offset
               (i32.add
                  (i32.mul (local.get $i)
                     (array.get $int_array (local.get $dim) (i32.const 1)))
                  (local.get $j)))))
      (if (i32.or
             (i32.ge_u (local.get $i)
                (array.get $int_array (local.get $dim) (i32.const 0)))
             (i32.ge_u (local.get $j)
                (array.get $int_array (local.get $dim) (i32.const 1))))
         (then
            (call $caml_bound_error)))
      (call $caml_ba_set_at_offset
         (local.get $ba) (local.get $offset) (local.get $v))
      (ref.i31 (i32.const 0)))

   (func (export "caml_ba_dim_2") (param (ref eq)) (result (ref eq))
      (return_call $caml_ba_dim (local.get 0) (ref.i31 (i32.const 1))))

   (func (export "caml_ba_get_3")
      (param $vba (ref eq)) (param $vi (ref eq)) (param $vj (ref eq))
      (param $vk (ref eq))
      (result (ref eq))
      (local $ba (ref $bigarray))
      (local $i i32)
      (local $j i32)
      (local $k i32)
      (local $offset i32)
      (local $dim (ref $int_array))
      (local.set $ba (ref.cast (ref $bigarray) (local.get $vba)))
      (local.set $i (i31.get_u (ref.cast (ref i31) (local.get $vi))))
      (local.set $j (i31.get_u (ref.cast (ref i31) (local.get $vj))))
      (local.set $k (i31.get_u (ref.cast (ref i31) (local.get $vk))))
      (local.set $dim (struct.get $bigarray $ba_dim (local.get $ba)))
      (if (struct.get $bigarray $ba_layout (local.get $ba))
         (then
            (local.set $i (i32.sub (local.get $i) (i32.const 1)))
            (local.set $j (i32.sub (local.get $j) (i32.const 1)))
            (local.set $k (i32.sub (local.get $k) (i32.const 1)))
            (local.set $offset
               (i32.add
                  (i32.mul
                     (i32.add
                        (i32.mul
                           (local.get $k)
                           (array.get $int_array (local.get $dim) (i32.const 1)))
                        (local.get $j))
                     (array.get $int_array (local.get $dim) (i32.const 0)))
                  (local.get $i))))
         (else
            (local.set $offset
               (i32.add
                  (i32.mul
                     (i32.add
                        (i32.mul
                           (local.get $i)
                           (array.get $int_array (local.get $dim) (i32.const 1)))
                     (local.get $j))
                     (array.get $int_array (local.get $dim) (i32.const 2)))
                  (local.get $k)))))
      (if (i32.or
             (i32.ge_u (local.get $i)
                (array.get $int_array (local.get $dim) (i32.const 0)))
             (i32.or
                (i32.ge_u (local.get $j)
                   (array.get $int_array (local.get $dim) (i32.const 1)))
                (i32.ge_u (local.get $k)
                   (array.get $int_array (local.get $dim) (i32.const 2)))))
         (then
            (call $caml_bound_error)))
      (return_call $caml_ba_get_at_offset (local.get $ba) (local.get $offset)))

   (func (export "caml_ba_set_3")
      (param $vba (ref eq)) (param $vi (ref eq)) (param $vj (ref eq))
      (param $vk (ref eq)) (param $v (ref eq))
      (result (ref eq))
      (local $ba (ref $bigarray))
      (local $i i32)
      (local $j i32)
      (local $k i32)
      (local $offset i32)
      (local $dim (ref $int_array))
      (local.set $ba (ref.cast (ref $bigarray) (local.get $vba)))
      (local.set $i (i31.get_u (ref.cast (ref i31) (local.get $vi))))
      (local.set $j (i31.get_u (ref.cast (ref i31) (local.get $vj))))
      (local.set $k (i31.get_u (ref.cast (ref i31) (local.get $vk))))
      (local.set $dim (struct.get $bigarray $ba_dim (local.get $ba)))
      (if (struct.get $bigarray $ba_layout (local.get $ba))
         (then
            (local.set $i (i32.sub (local.get $i) (i32.const 1)))
            (local.set $j (i32.sub (local.get $j) (i32.const 1)))
            (local.set $k (i32.sub (local.get $k) (i32.const 1)))
            (local.set $offset
               (i32.add
                  (i32.mul
                     (i32.add
                        (i32.mul
                           (local.get $k)
                           (array.get $int_array (local.get $dim) (i32.const 1)))
                        (local.get $j))
                     (array.get $int_array (local.get $dim) (i32.const 0)))
                  (local.get $i))))
         (else
            (local.set $offset
               (i32.add
                  (i32.mul
                     (i32.add
                        (i32.mul
                           (local.get $i)
                           (array.get $int_array (local.get $dim) (i32.const 1)))
                     (local.get $j))
                     (array.get $int_array (local.get $dim) (i32.const 2)))
                  (local.get $k)))))
      (if (i32.or
             (i32.ge_u (local.get $i)
                (array.get $int_array (local.get $dim) (i32.const 0)))
             (i32.or
                (i32.ge_u (local.get $j)
                   (array.get $int_array (local.get $dim) (i32.const 1)))
                (i32.ge_u (local.get $k)
                   (array.get $int_array (local.get $dim) (i32.const 2)))))
         (then
            (call $caml_bound_error)))
      (call $caml_ba_set_at_offset
         (local.get $ba) (local.get $offset) (local.get $v))
      (ref.i31 (i32.const 0)))

   (func (export "caml_ba_dim_3") (param (ref eq)) (result (ref eq))
      (return_call $caml_ba_dim (local.get 0) (ref.i31 (i32.const 2))))

   (func $caml_ba_offset
      (param $b (ref $bigarray)) (param $index (ref $int_array)) (result i32)
      (local $dim (ref $int_array))
      (local $num_dims i32) (local $idx i32)
      (local $offset i32) (local $i i32) (local $l i32)
      (local.set $dim (struct.get $bigarray $ba_dim (local.get $b)))
      (if (struct.get $bigarray $ba_layout (local.get $b))
         (then
            (local.set $i
               (i32.sub (struct.get $bigarray $ba_num_dims (local.get $b))
                  (i32.const 1)))
            (loop $loop
               (if (i32.ge_s (local.get $i) (i32.const 0))
                  (then
                     (local.set $idx
                        (i32.sub
                           (array.get $int_array (local.get $index)
                              (local.get $i))
                           (i32.const 1)))
                     (local.set $l
                        (array.get $int_array (local.get $dim) (local.get $i)))
                     (if (i32.ge_u (local.get $idx) (local.get $l))
                        (then
                           (call $caml_bound_error)))
                     (local.set $offset
                        (i32.add (i32.mul (local.get $offset) (local.get $l))
                           (local.get $idx)))
                     (local.set $i (i32.sub (local.get $i) (i32.const 1)))
                     (br $loop)))))
         (else
            (local.set $num_dims
               (struct.get $bigarray $ba_num_dims (local.get $b)))
            (loop $loop
               (if (i32.lt_s (local.get $i) (local.get $num_dims))
                  (then
                     (local.set $idx
                        (array.get $int_array (local.get $index) (local.get $i)))
                     (local.set $l
                        (array.get $int_array (local.get $dim) (local.get $i)))
                     (if (i32.ge_u (local.get $idx) (local.get $l))
                        (then
                           (call $caml_bound_error)))
                     (local.set $offset
                        (i32.add (i32.mul (local.get $offset) (local.get $l))
                           (local.get $idx)))
                     (local.set $i (i32.add (local.get $i) (i32.const 1)))
                     (br $loop))))))
      (local.get $offset))

   (func $caml_ba_offset'
      (param $b (ref $bigarray)) (param $index (ref $block)) (result i32)
      (local $dim (ref $int_array))
      (local $num_dims i32) (local $idx i32)
      (local $offset i32) (local $i i32) (local $l i32)
      (local.set $dim (struct.get $bigarray $ba_dim (local.get $b)))
      (if (struct.get $bigarray $ba_layout (local.get $b))
         (then
            (local.set $i
               (i32.sub (struct.get $bigarray $ba_num_dims (local.get $b))
                  (i32.const 1)))
            (loop $loop
               (if (i32.ge_s (local.get $i) (i32.const 0))
                  (then
                     (local.set $idx
                        (i32.sub
                           (i31.get_s
                              (ref.cast (ref i31)
                                 (array.get $block (local.get $index)
                                    (i32.add (local.get $i) (i32.const 1)))))
                           (i32.const 1)))
                     (local.set $l
                        (array.get $int_array (local.get $dim) (local.get $i)))
                     (if (i32.ge_u (local.get $idx) (local.get $l))
                        (then
                           (call $caml_bound_error)))
                     (local.set $offset
                        (i32.add (i32.mul (local.get $offset) (local.get $l))
                           (local.get $idx)))
                     (local.set $i (i32.sub (local.get $i) (i32.const 1)))
                     (br $loop)))))
         (else
            (local.set $num_dims
               (struct.get $bigarray $ba_num_dims (local.get $b)))
            (loop $loop
               (if (i32.lt_s (local.get $i) (local.get $num_dims))
                  (then
                     (local.set $idx
                        (i31.get_s
                           (ref.cast (ref i31)
                              (array.get $block (local.get $index)
                                 (i32.add (local.get $i) (i32.const 1))))))
                     (local.set $l
                        (array.get $int_array (local.get $dim) (local.get $i)))
                     (if (i32.ge_u (local.get $idx) (local.get $l))
                        (then
                           (call $caml_bound_error)))
                     (local.set $offset
                        (i32.add (i32.mul (local.get $offset) (local.get $l))
                           (local.get $idx)))
                     (local.set $i (i32.add (local.get $i) (i32.const 1)))
                     (br $loop))))))
      (local.get $offset))

   (func (export "caml_ba_get_generic")
      (param $vba (ref eq)) (param $index (ref eq)) (result (ref eq))
      (local $ba (ref $bigarray))
      (local.set $ba (ref.cast (ref $bigarray) (local.get $vba)))
      (return_call $caml_ba_get_at_offset (local.get $ba)
         (call $caml_ba_offset' (local.get $ba)
            (ref.cast (ref $block) (local.get $index)))))

   (func (export "caml_ba_set_generic")
      (param $vba (ref eq)) (param $index (ref eq)) (param $v (ref eq))
      (result (ref eq))
      (local $ba (ref $bigarray))
      (local.set $ba (ref.cast (ref $bigarray) (local.get $vba)))
      (call $caml_ba_set_at_offset (local.get $ba)
         (call $caml_ba_offset' (local.get $ba)
            (ref.cast (ref $block) (local.get $index)))
         (local.get $v))
      (ref.i31 (i32.const 0)))

   (@string $too_many_indices "Bigarray.slice: too many indices")

   (func (export "caml_ba_slice")
      (param $vb (ref eq)) (param $vind (ref eq)) (result (ref eq))
      (local $b (ref $bigarray))
      (local $ind (ref $block))
      (local $index (ref $int_array)) (local $sub_dim (ref $int_array))
      (local $num_inds i32) (local $num_dims i32) (local $i i32)
      (local $idx i32) (local $mul i32) (local $offset i32) (local $size i32)
      (local $sub_data (ref extern))
      (local.set $b (ref.cast (ref $bigarray) (local.get $vb)))
      (local.set $ind (ref.cast (ref $block) (local.get $vind)))
      (local.set $num_inds (i32.sub (array.len (local.get $ind)) (i32.const 1)))
      (local.set $num_dims (struct.get $bigarray $ba_num_dims (local.get $b)))
      (if (i32.gt_u (local.get $num_inds)
             (struct.get $bigarray $ba_num_dims (local.get $b)))
         (then (call $caml_invalid_argument (global.get $too_many_indices))))
      (local.set $sub_dim
         (array.new $int_array (i32.const 0)
            (i32.sub (local.get $num_dims) (local.get $num_inds))))
      (if (struct.get $bigarray $ba_layout (local.get $b))
         (then
            (local.set $index
               (array.new $int_array (i32.const 1) (local.get $num_dims)))
            (loop $loop
               (if (i32.lt_u (local.get $i) (local.get $num_inds))
                  (then
                     (array.set $int_array (local.get $index)
                        (i32.sub (i32.add (local.get $num_dims) (local.get $i))
                           (local.get $num_inds))
                        (i31.get_u
                           (ref.cast (ref i31)
                              (array.get $block (local.get $ind)
                                 (i32.add (local.get $i) (i32.const 1))))))
                     (local.set $i (i32.add (local.get $i) (i32.const 1)))
                     (br $loop))))
            (local.set $offset
               (call $caml_ba_offset (local.get $b) (local.get $index)))
            (array.copy $int_array $int_array
               (local.get $sub_dim) (i32.const 0)
               (struct.get $bigarray $ba_dim (local.get $b)) (i32.const 0)
               (i32.sub (local.get $num_dims) (local.get $num_inds))))
         (else
            (local.set $index
               (array.new $int_array (i32.const 0) (local.get $num_dims)))
            (loop $loop
               (if (i32.lt_u (local.get $i) (local.get $num_inds))
                  (then
                     (array.set $int_array (local.get $index)
                        (local.get $i)
                        (i31.get_u
                           (ref.cast (ref i31)
                              (array.get $block (local.get $ind)
                                 (i32.add (local.get $i) (i32.const 1))))))
                     (local.set $i (i32.add (local.get $i) (i32.const 1)))
                     (br $loop))))
            (local.set $offset
               (call $caml_ba_offset (local.get $b) (local.get $index)))
            (array.copy $int_array $int_array
               (local.get $sub_dim) (i32.const 0)
               (struct.get $bigarray $ba_dim (local.get $b))
               (local.get $num_inds)
               (i32.sub (local.get $num_dims) (local.get $num_inds)))))
      (local.set $mul
         (call $caml_ba_size_per_element
            (struct.get $bigarray $ba_kind (local.get $b))))
      (local.set $size (call $caml_ba_get_size (local.get $sub_dim)))
      (local.set $sub_data
         (call $ta_subarray (struct.get $bigarray $ba_data (local.get $b))
            (i32.mul (local.get $offset) (local.get $mul))
            (i32.mul (i32.add (local.get $offset) (local.get $size))
               (local.get $mul))))
      (struct.new $bigarray
         (global.get $bigarray_ops)
         (local.get $sub_data)
         (call $dv_make (local.get $sub_data))
         (local.get $sub_dim)
         (array.len (local.get $sub_dim))
         (struct.get $bigarray $ba_kind (local.get $b))
         (struct.get $bigarray $ba_layout (local.get $b))))

   (@string $bad_subarray "Bigarray.sub: bad sub-array")

   (func (export "caml_ba_sub")
      (param $vba (ref eq)) (param $vofs (ref eq)) (param $vlen (ref eq))
      (result (ref eq))
      (local $ba (ref $bigarray))
      (local $ofs i32) (local $len i32)
      (local $changed_dim i32) (local $mul i32) (local $i i32)
      (local $num_dims i32)
      (local $dim (ref $int_array)) (local $new_dim (ref $int_array))
      (local $new_data (ref extern))
      (local.set $ba (ref.cast (ref $bigarray) (local.get $vba)))
      (local.set $ofs (i31.get_s (ref.cast (ref i31) (local.get $vofs))))
      (local.set $len (i31.get_s (ref.cast (ref i31) (local.get $vlen))))
      (local.set $num_dims (struct.get $bigarray $ba_num_dims (local.get $ba)))
      (local.set $dim (struct.get $bigarray $ba_dim (local.get $ba)))
      (local.set $mul (i32.const 1))
      (if (struct.get $bigarray $ba_layout (local.get $ba))
         (then
            (local.set $changed_dim
               (i32.sub (local.get $num_dims) (i32.const 1)))
            (local.set $ofs (i32.sub (local.get $ofs) (i32.const 1)))
            (local.set $i (i32.const 0))
            (loop $loop
               (if (i32.lt_u (local.get $i) (local.get $changed_dim))
                  (then
                     (local.set $mul
                        (i32.mul (local.get $mul)
                           (array.get $int_array
                              (local.get $dim) (local.get $i))))
                     (local.set $i (i32.add (local.get $i) (i32.const 1)))
                     (br $loop)))))
         (else
            (local.set $changed_dim (i32.const 0))
            (local.set $i (i32.const 1))
            (loop $loop
               (if (i32.lt_u (local.get $i) (local.get $num_dims))
                  (then
                     (local.set $mul
                        (i32.mul (local.get $mul)
                           (array.get $int_array
                              (local.get $dim) (local.get $i))))
                     (local.set $i (i32.add (local.get $i) (i32.const 1)))
                     (br $loop))))))
      (if (i32.or
             (i32.or (i32.lt_s (local.get $ofs) (i32.const 0))
                (i32.lt_s (local.get $len) (i32.const 0)))
             (i32.gt_s (i32.add (local.get $ofs) (local.get $len))
                (array.get $int_array (local.get $dim)
                   (local.get $changed_dim))))
         (then (call $caml_invalid_argument (global.get $bad_subarray))))
      (local.set $new_dim
         (array.new $int_array (i32.const 0) (local.get $num_dims)))
      (array.copy $int_array $int_array
         (local.get $new_dim) (i32.const 0)
         (local.get $dim) (i32.const 0)
         (local.get $num_dims))
      (array.set $int_array (local.get $new_dim) (local.get $changed_dim)
         (local.get $len))
      (local.set $mul (i32.mul (local.get $mul)
         (call $caml_ba_size_per_element
           (struct.get $bigarray $ba_kind (local.get $ba)))))
      (local.set $new_data
         (call $ta_subarray (struct.get $bigarray $ba_data (local.get $ba))
            (i32.mul (local.get $ofs) (local.get $mul))
            (i32.mul (i32.add (local.get $ofs) (local.get $len))
               (local.get $mul))))
      (struct.new $bigarray
         (global.get $bigarray_ops)
         (local.get $new_data)
         (call $dv_make (local.get $new_data))
         (local.get $new_dim)
         (local.get $num_dims)
         (struct.get $bigarray $ba_kind (local.get $ba))
         (struct.get $bigarray $ba_layout (local.get $ba))))

   (func (export "caml_ba_fill")
      (param $vba (ref eq)) (param $v (ref eq)) (result (ref eq))
      (local $ba (ref $bigarray))
      (local $data (ref extern))
      (local $view (ref extern))
      (local $l i64)
      (local $i i32) (local $len i32) (local $i1 i32) (local $i2 i32)
      (local $f1 f64) (local $f2 f64) (local $f1' f32) (local $f2' f32)
      (local $b (ref $float_array))
      (local.set $ba (ref.cast (ref $bigarray) (local.get $vba)))
      (local.set $data (struct.get $bigarray $ba_data (local.get $ba)))
      (block $float
       (block $int
        (block $int32
         (block $int64
          (block $complex32
           (block $complex64
            (block $float16
             (br_table $float $float $int $int $int $int $int32 $int64 $int
               $int32 $complex32 $complex64 $int $float16
               (struct.get $bigarray $ba_kind (local.get $ba))))
             ;; float16
             (call $ta_fill_int (local.get $data)
                (call $double_to_float16
                   (struct.get $float 0 (ref.cast (ref $float) (local.get $v)))))
             (return (ref.i31 (i32.const 0))))
            ;; complex64
            (local.set $view (struct.get $bigarray $ba_view (local.get $ba)))
            (local.set $len
               (i32.shl (call $ta_length (local.get $data)) (i32.const 3)))
            (local.set $b (ref.cast (ref $float_array) (local.get $v)))
            (local.set $f1
               (array.get $float_array (local.get $b) (i32.const 0)))
            (local.set $f2
               (array.get $float_array (local.get $b) (i32.const 1)))
            (loop $loop
               (if (i32.lt_u (local.get $i) (local.get $len))
                  (then
                     (call $dv_set_f64 (local.get $view) (local.get $i)
                        (local.get $f1)
                        (global.get $littleEndian))
                     (call $dv_set_f64 (local.get $view)
                        (i32.add (local.get $i) (i32.const 8))
                        (local.get $f2)
                        (global.get $littleEndian))
                     (local.set $i (i32.add (local.get $i) (i32.const 16)))
                     (br $loop))))
            (return (ref.i31 (i32.const 0))))
           ;; complex32
           (local.set $view (struct.get $bigarray $ba_view (local.get $ba)))
           (local.set $len
              (i32.shl (call $ta_length (local.get $data)) (i32.const 2)))
           (local.set $b (ref.cast (ref $float_array) (local.get $v)))
           (local.set $f1'
              (f32.demote_f64
                 (array.get $float_array (local.get $b) (i32.const 0))))
           (local.set $f2'
              (f32.demote_f64
                 (array.get $float_array (local.get $b) (i32.const 1))))
           (loop $loop
              (if (i32.lt_u (local.get $i) (local.get $len))
                 (then
                    (call $dv_set_f32 (local.get $view) (local.get $i)
                       (local.get $f1')
                       (global.get $littleEndian))
                    (call $dv_set_f32 (local.get $view)
                       (i32.add (local.get $i) (i32.const 4))
                       (local.get $f2')
                       (global.get $littleEndian))
                    (local.set $i (i32.add (local.get $i) (i32.const 8)))
                    (br $loop))))
           (return (ref.i31 (i32.const 0))))
          ;; int64
          (local.set $view (struct.get $bigarray $ba_view (local.get $ba)))
          (local.set $len
             ;; we currently use an Int32Array, so multiply by just 4
             (i32.shl (call $ta_length (local.get $data)) (i32.const 2)))
          (local.set $l (call $Int64_val (local.get $v)))
          (loop $loop
             (if (i32.lt_u (local.get $i) (local.get $len))
                (then
                   (call $dv_set_i64 (local.get $view) (local.get $i)
                      (local.get $l)
                      (global.get $littleEndian))
                   (local.set $i (i32.add (local.get $i) (i32.const 8)))
                   (br $loop))))
          (return (ref.i31 (i32.const 0))))
         ;; int32
         (call $ta_fill_int (local.get $data) (call $Int32_val (local.get $v)))
         (return (ref.i31 (i32.const 0))))
        ;; int
        (call $ta_fill_int (local.get $data)
           (i31.get_s (ref.cast (ref i31) (local.get $v))))
        (return (ref.i31 (i32.const 0))))
       ;; float
       (call $ta_fill_float (local.get $data)
          (struct.get $float 0 (ref.cast (ref $float) (local.get $v))))
       (return (ref.i31 (i32.const 0))))

   (@string $dim_mismatch "Bigarray.blit: dimension mismatch")

   (func (export "caml_ba_blit")
      (param $vsrc (ref eq)) (param $vdst (ref eq)) (result (ref eq))
      (local $src (ref $bigarray))
      (local $dst (ref $bigarray))
      (local $sdim (ref $int_array))
      (local $ddim (ref $int_array))
      (local $i i32) (local $len i32)
      (local.set $src (ref.cast (ref $bigarray) (local.get $vsrc)))
      (local.set $dst (ref.cast (ref $bigarray) (local.get $vdst)))
      (local.set $len (struct.get $bigarray $ba_num_dims (local.get $dst)))
      (if (i32.ne (local.get $len)
             (struct.get $bigarray $ba_num_dims (local.get $src)))
         (then (call $caml_invalid_argument (global.get $dim_mismatch))))
      (local.set $sdim (struct.get $bigarray $ba_dim (local.get $src)))
      (local.set $ddim (struct.get $bigarray $ba_dim (local.get $dst)))
      (loop $loop
         (if (i32.lt_s (local.get $i) (local.get $len))
            (then
               (if (i32.ne
                      (array.get $int_array (local.get $sdim) (local.get $i))
                      (array.get $int_array (local.get $ddim) (local.get $i)))
                  (then
                     (call $caml_invalid_argument (global.get $dim_mismatch))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (call $ta_blit
         (struct.get $bigarray $ba_data (local.get $src))
         (struct.get $bigarray $ba_data (local.get $dst)))
     (ref.i31 (i32.const 0)))

   (@string $bad_number_dim "Bigarray.reshape: bad number of dimensions")
   (@string $negative_dim "Bigarray.reshape: negative dimension")
   (@string $size_mismatch "Bigarray.reshape: size mismatch")

   (func (export "caml_ba_reshape")
      (param $vb (ref eq)) (param $vd (ref eq)) (result (ref eq))
      (local $vdim (ref $block))
      (local $num_dims i32) (local $num_elts i64) (local $i i32) (local $d i32)
      (local $b (ref $bigarray))
      (local $dim (ref $int_array))
      (local.set $vdim (ref.cast (ref $block) (local.get $vd)))
      (local.set $num_dims (i32.sub (array.len (local.get $vdim)) (i32.const 1)))
      (local.set $b (ref.cast (ref $bigarray) (local.get $vb)))
      (if (i32.gt_u (local.get $num_dims) (global.get $CAML_BA_MAX_NUM_DIMS))
         (then (call $caml_invalid_argument (global.get $bad_number_dim))))
      (local.set $num_elts (i64.const 1))
      (local.set $dim (array.new $int_array (i32.const 0) (local.get $num_dims)))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $num_dims))
            (then
               (local.set $d
                  (i31.get_s
                     (ref.cast (ref i31)
                        (array.get $block (local.get $vdim)
                           (i32.add (local.get $i) (i32.const 1))))))
               (if (i32.lt_s (local.get $d) (i32.const 0))
                  (then
                     (call $caml_invalid_argument (global.get $negative_dim))))
               (array.set $int_array (local.get $dim) (local.get $i)
                  (local.get $d))
               (local.set $num_elts
                  (i64.mul (local.get $num_elts)
                     (i64.extend_i32_s (local.get $d))))
               (if (i64.ne (local.get $num_elts)
                      (i64.extend_i32_s (i32.wrap_i64 (local.get $num_elts))))
                  (then (call $caml_raise_out_of_memory)))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (if (i32.ne (i32.wrap_i64 (local.get $num_elts))
             (call $caml_ba_get_size
                (struct.get $bigarray $ba_dim (local.get $b))))
         (then (call $caml_invalid_argument (global.get $size_mismatch))))
      (struct.new $bigarray
         (global.get $bigarray_ops)
         (struct.get $bigarray $ba_data (local.get $b))
         (struct.get $bigarray $ba_view (local.get $b))
         (local.get $dim)
         (local.get $num_dims)
         (struct.get $bigarray $ba_kind (local.get $b))
         (struct.get $bigarray $ba_layout (local.get $b))))

   (func (export "caml_ba_change_layout")
      (param $vb (ref eq)) (param $vlayout (ref eq)) (result (ref eq))
      (local $b (ref $bigarray))
      (local $layout i32) (local $num_dims i32) (local $i i32)
      (local $dim (ref $int_array)) (local $new_dim (ref $int_array))
      (local.set $b (ref.cast (ref $bigarray) (local.get $vb)))
      (local.set $layout (i31.get_s (ref.cast (ref i31) (local.get $vlayout))))
      (if (result (ref eq))
          (i32.ne (struct.get $bigarray $ba_layout (local.get $b))
             (local.get $layout))
         (then
            (local.set $num_dims
               (struct.get $bigarray $ba_num_dims (local.get $b)))
            (local.set $dim
               (struct.get $bigarray $ba_dim (local.get $b)))
            (local.set $new_dim
               (array.new $int_array (i32.const 0) (local.get $num_dims)))
            (loop $loop
               (if (i32.lt_u (local.get $i) (local.get $num_dims))
                  (then
                     (array.set $int_array (local.get $new_dim) (local.get $i)
                        (array.get $int_array (local.get $dim)
                           (i32.sub
                              (i32.sub (local.get $num_dims) (local.get $i))
                              (i32.const 1))))
                     (local.set $i (i32.add (local.get $i) (i32.const 1)))
                     (br $loop))))
            (struct.new $bigarray
               (global.get $bigarray_ops)
               (struct.get $bigarray $ba_data (local.get $b))
               (struct.get $bigarray $ba_view (local.get $b))
               (local.get $new_dim)
               (local.get $num_dims)
               (struct.get $bigarray $ba_kind (local.get $b))
               (local.get $layout)))
         (else
            (local.get $vb))))

   (func (export "caml_ba_num_dims") (param (ref eq)) (result (ref eq))
      (ref.i31
         (struct.get $bigarray $ba_num_dims
            (ref.cast (ref $bigarray) (local.get 0)))))

   (func (export "caml_ba_kind") (param (ref eq)) (result (ref eq))
      (ref.i31
         (struct.get $bigarray $ba_kind
            (ref.cast (ref $bigarray) (local.get 0)))))

   (func (export "caml_ba_layout") (param (ref eq)) (result (ref eq))
      (ref.i31
         (struct.get $bigarray $ba_layout
            (ref.cast (ref $bigarray) (local.get 0)))))

   (func $caml_ba_compare
      (param $v1 (ref eq)) (param $v2 (ref eq)) (param $total i32) (result i32)
      (local $b1 (ref $bigarray)) (local $b2 (ref $bigarray))
      (local $i1 i32) (local $i2 i32) (local $i i32) (local $len i32)
      (local $l1 i64) (local $l2 i64)
      (local $f1 f64) (local $f2 f64) (local $f1' f32) (local $f2' f32)
      (local $d1 (ref extern)) (local $d2 (ref extern))
      (local $view1 (ref extern)) (local $view2 (ref extern))
      (local.set $b1 (ref.cast (ref $bigarray) (local.get $v1)))
      (local.set $b2 (ref.cast (ref $bigarray) (local.get $v2)))
      (if (i32.ne (struct.get $bigarray $ba_layout (local.get $b2))
                  (struct.get $bigarray $ba_layout (local.get $b1)))
         (then
            (return
               (i32.sub (struct.get $bigarray $ba_layout (local.get $b2))
                        (struct.get $bigarray $ba_layout (local.get $b1))))))
      (if (i32.ne (struct.get $bigarray $ba_kind (local.get $b2))
                  (struct.get $bigarray $ba_kind (local.get $b1)))
         (then
            (return
               (i32.sub (struct.get $bigarray $ba_kind (local.get $b2))
                        (struct.get $bigarray $ba_kind (local.get $b1))))))
      (if (i32.ne (struct.get $bigarray $ba_num_dims (local.get $b2))
                  (struct.get $bigarray $ba_num_dims (local.get $b1)))
         (then
            (return
               (i32.sub (struct.get $bigarray $ba_num_dims (local.get $b2))
                        (struct.get $bigarray $ba_num_dims (local.get $b1))))))
      (local.set $len (struct.get $bigarray $ba_num_dims (local.get $b2)))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $len))
            (then
                (local.set $i1
                   (array.get $int_array
                      (struct.get $bigarray $ba_dim (local.get $b1))
                      (local.get $i)))
                (local.set $i2
                   (array.get $int_array
                      (struct.get $bigarray $ba_dim (local.get $b2))
                      (local.get $i)))
                (if (i32.ne (local.get $i1) (local.get $i2))
                   (then
                      (return
                         (select (i32.const -1) (i32.const 1)
                            (i32.lt_u (local.get $i1) (local.get $i2))))))
                (local.set $i (i32.add (local.get $i) (i32.const 1)))
                (br $loop))))
      (local.set $d1 (struct.get $bigarray $ba_data (local.get $b1)))
      (local.set $view1 (struct.get $bigarray $ba_view (local.get $b1)))
      (local.set $d2 (struct.get $bigarray $ba_data (local.get $b2)))
      (local.set $view2 (struct.get $bigarray $ba_view (local.get $b2)))
      (local.set $len (call $ta_length (local.get $d1)))
      (local.set $i (i32.const 0))
      (block $float32
       (block $float64
        (block $int8
         (block $uint8
          (block $int16
           (block $uint16
            (block $int32
             (block $int64
              (block $float16
               (br_table $float32 $float64 $int8 $uint8 $int16 $uint16
                         $int32 $int64 $int32 $int32
                         $float32 $float64 $uint8 $float16
                  (struct.get $bigarray $ba_kind (local.get $b1))))
              ;; float16
              (local.set $len (i32.shl (local.get $len) (i32.const 1)))
              (loop $loop
                 (if (i32.lt_u (local.get $i) (local.get $len))
                    (then
                       (local.set $f1
                          (call $float16_to_double
                             (call $dv_get_ui16 (local.get $view1) (local.get $i)
                                (global.get $littleEndian))))
                       (local.set $f2
                          (call $float16_to_double
                             (call $dv_get_ui16 (local.get $view2) (local.get $i)
                                (global.get $littleEndian))))
                       (if (f64.lt (local.get $f1) (local.get $f2))
                          (then (return (i32.const -1))))
                       (if (f64.gt (local.get $f1) (local.get $f2))
                          (then (return (i32.const 1))))
                       (if (f64.ne (local.get $f1) (local.get $f2))
                          (then
                             (if (i32.eqz (local.get $total))
                                (then (return (global.get $unordered))))
                             (if (f64.eq (local.get $f1) (local.get $f1))
                                (then (return (i32.const 1))))
                             (if (f64.eq (local.get $f2) (local.get $f2))
                                (then (return (i32.const -1))))))
                       (local.set $i (i32.add (local.get $i) (i32.const 2)))
                       (br $loop))))
              (return (i32.const 0)))
             ;; int64
             (local.set $len (i32.shl (local.get $len) (i32.const 2)))
             (loop $loop
                (if (i32.lt_u (local.get $i) (local.get $len))
                   (then
                      (local.set $l1
                         (call $dv_get_i64 (local.get $view1)
                            (local.get $i)
                            (global.get $littleEndian)))
                      (local.set $l2
                         (call $dv_get_i64 (local.get $view2)
                            (local.get $i)
                            (global.get $littleEndian)))
                      (if (i64.lt_s (local.get $l1) (local.get $l2))
                         (then (return (i32.const -1))))
                      (if (i64.gt_s (local.get $l1) (local.get $l2))
                         (then (return (i32.const 1))))
                      (local.set $i (i32.add (local.get $i) (i32.const 8)))
                      (br $loop))))
             (return (i32.const 0)))
            ;; int32
            (local.set $len (i32.shl (local.get $len) (i32.const 2)))
            (loop $loop
               (if (i32.lt_u (local.get $i) (local.get $len))
                  (then
                     (local.set $i1
                        (call $dv_get_i32 (local.get $view1) (local.get $i)
                           (global.get $littleEndian)))
                     (local.set $i2
                        (call $dv_get_i32 (local.get $view2) (local.get $i)
                           (global.get $littleEndian)))
                     (if (i32.lt_s (local.get $i1) (local.get $i2))
                        (then (return (i32.const -1))))
                     (if (i32.gt_s (local.get $i1) (local.get $i2))
                        (then (return (i32.const 1))))
                     (local.set $i (i32.add (local.get $i) (i32.const 4)))
                     (br $loop))))
            (return (i32.const 0)))
           ;; uint16
           (local.set $len (i32.shl (local.get $len) (i32.const 1)))
           (loop $loop
              (if (i32.lt_u (local.get $i) (local.get $len))
                 (then
                    (local.set $i1
                       (call $dv_get_ui16 (local.get $view1) (local.get $i)
                          (global.get $littleEndian)))
                    (local.set $i2
                       (call $dv_get_ui16 (local.get $view2) (local.get $i)
                          (global.get $littleEndian)))
                    (if (i32.lt_s (local.get $i1) (local.get $i2))
                       (then (return (i32.const -1))))
                    (if (i32.gt_s (local.get $i1) (local.get $i2))
                       (then (return (i32.const 1))))
                    (local.set $i (i32.add (local.get $i) (i32.const 2)))
                    (br $loop))))
           (return (i32.const 0)))
          ;; int16
          (local.set $len (i32.shl (local.get $len) (i32.const 1)))
          (loop $loop
             (if (i32.lt_u (local.get $i) (local.get $len))
                (then
                   (local.set $i1
                      (call $dv_get_i16 (local.get $view1) (local.get $i)
                         (global.get $littleEndian)))
                   (local.set $i2
                      (call $dv_get_i16 (local.get $view2) (local.get $i)
                         (global.get $littleEndian)))
                   (if (i32.lt_s (local.get $i1) (local.get $i2))
                      (then (return (i32.const -1))))
                   (if (i32.gt_s (local.get $i1) (local.get $i2))
                      (then (return (i32.const 1))))
                   (local.set $i (i32.add (local.get $i) (i32.const 2)))
                   (br $loop))))
          (return (i32.const 0)))
         ;; uint8
         (loop $loop
            (if (i32.lt_u (local.get $i) (local.get $len))
               (then
                  (local.set $i1
                     (call $dv_get_ui8 (local.get $view1) (local.get $i)))
                  (local.set $i2
                     (call $dv_get_ui8 (local.get $view2) (local.get $i)))
                  (if (i32.lt_s (local.get $i1) (local.get $i2))
                     (then (return (i32.const -1))))
                  (if (i32.gt_s (local.get $i1) (local.get $i2))
                     (then (return (i32.const 1))))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (br $loop))))
         (return (i32.const 0)))
        ;; int8
        (loop $loop
           (if (i32.lt_u (local.get $i) (local.get $len))
              (then
                 (local.set $i1
                    (call $dv_get_i8 (local.get $view1) (local.get $i)))
                 (local.set $i2
                    (call $dv_get_i8 (local.get $view2) (local.get $i)))
                 (if (i32.lt_s (local.get $i1) (local.get $i2))
                    (then (return (i32.const -1))))
                 (if (i32.gt_s (local.get $i1) (local.get $i2))
                    (then (return (i32.const 1))))
                 (local.set $i (i32.add (local.get $i) (i32.const 1)))
                 (br $loop))))
        (return (i32.const 0)))
       ;; float64
       (local.set $len (i32.shl (local.get $len) (i32.const 3)))
       (loop $loop
          (if (i32.lt_u (local.get $i) (local.get $len))
             (then
                (local.set $f1
                   (call $dv_get_f64 (local.get $view1) (local.get $i)
                      (global.get $littleEndian)))
                (local.set $f2
                   (call $dv_get_f64 (local.get $view2) (local.get $i)
                      (global.get $littleEndian)))
                (if (f64.lt (local.get $f1) (local.get $f2))
                   (then (return (i32.const -1))))
                (if (f64.gt (local.get $f1) (local.get $f2))
                   (then (return (i32.const 1))))
                (if (f64.ne (local.get $f1) (local.get $f2))
                   (then
                      (if (i32.eqz (local.get $total))
                         (then (return (global.get $unordered))))
                      (if (f64.eq (local.get $f1) (local.get $f1))
                         (then (return (i32.const 1))))
                      (if (f64.eq (local.get $f2) (local.get $f2))
                         (then (return (i32.const -1))))))
                (local.set $i (i32.add (local.get $i) (i32.const 8)))
                (br $loop))))
       (return (i32.const 0)))
      ;; float32
      (local.set $len (i32.shl (local.get $len) (i32.const 2)))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $len))
            (then
               (local.set $f1'
                  (call $dv_get_f32 (local.get $view1) (local.get $i)
                     (global.get $littleEndian)))
               (local.set $f2'
                  (call $dv_get_f32 (local.get $view2) (local.get $i)
                     (global.get $littleEndian)))
               (if (f32.lt (local.get $f1') (local.get $f2'))
                  (then (return (i32.const -1))))
               (if (f32.gt (local.get $f1') (local.get $f2'))
                  (then (return (i32.const 1))))
               (if (f32.ne (local.get $f1') (local.get $f2'))
                  (then
                     (if (i32.eqz (local.get $total))
                        (then (return (global.get $unordered))))
                     (if (f32.eq (local.get $f1') (local.get $f1'))
                        (then (return (i32.const 1))))
                     (if (f32.eq (local.get $f2') (local.get $f2'))
                        (then (return (i32.const -1))))))
               (local.set $i (i32.add (local.get $i) (i32.const 4)))
               (br $loop))))
      (return (i32.const 0)))

   (func (export "caml_ba_uint8_get16")
      (param $vba (ref eq)) (param $i (ref eq)) (result (ref eq))
      (local $ba (ref $bigarray))
      (local $view (ref extern))
      (local $p i32)
      (local.set $ba (ref.cast (ref $bigarray) (local.get $vba)))
      (local.set $view (struct.get $bigarray $ba_view (local.get $ba)))
      (local.set $p (i31.get_s (ref.cast (ref i31) (local.get $i))))
      (if (i32.lt_s (local.get $p) (i32.const 0))
         (then (call $caml_bound_error)))
      (if (i32.ge_u (i32.add (local.get $p) (i32.const 1))
             (array.get $int_array
                (struct.get $bigarray $ba_dim (local.get $ba))
                (i32.const 0)))
         (then (call $caml_bound_error)))
      (ref.i31
         (call $dv_get_ui16 (local.get $view) (local.get $p) (i32.const 1))))

   (func (export "caml_ba_uint8_get32")
      (param $vba (ref eq)) (param $i (ref eq)) (result i32)
      (local $ba (ref $bigarray))
      (local $view (ref extern))
      (local $p i32)
      (local.set $ba (ref.cast (ref $bigarray) (local.get $vba)))
      (local.set $view (struct.get $bigarray $ba_view (local.get $ba)))
      (local.set $p (i31.get_s (ref.cast (ref i31) (local.get $i))))
      (if (i32.lt_s (local.get $p) (i32.const 0))
         (then (call $caml_bound_error)))
      (if (i32.ge_u (i32.add (local.get $p) (i32.const 3))
             (array.get $int_array
                (struct.get $bigarray $ba_dim (local.get $ba))
                (i32.const 0)))
         (then (call $caml_bound_error)))
      (return_call $dv_get_i32 (local.get $view) (local.get $p) (i32.const 1)))

   (func (export "caml_ba_uint8_get64")
      (param $vba (ref eq)) (param $i (ref eq)) (result i64)
      (local $ba (ref $bigarray))
      (local $view (ref extern))
      (local $p i32)
      (local.set $ba (ref.cast (ref $bigarray) (local.get $vba)))
      (local.set $view (struct.get $bigarray $ba_view (local.get $ba)))
      (local.set $p (i31.get_s (ref.cast (ref i31) (local.get $i))))
      (if (i32.lt_s (local.get $p) (i32.const 0))
         (then (call $caml_bound_error)))
      (if (i32.ge_u (i32.add (local.get $p) (i32.const 7))
             (array.get $int_array
                (struct.get $bigarray $ba_dim (local.get $ba))
                (i32.const 0)))
         (then (call $caml_bound_error)))
      (call $dv_get_i64
         (local.get $view) (local.get $p) (i32.const 1)))

   (func (export "caml_ba_uint8_set16")
      (param $vba (ref eq)) (param $i (ref eq)) (param $v (ref eq))
      (result (ref eq))
      (local $ba (ref $bigarray))
      (local $view (ref extern))
      (local $p i32) (local $d i32)
      (local.set $ba (ref.cast (ref $bigarray) (local.get $vba)))
      (local.set $view (struct.get $bigarray $ba_view (local.get $ba)))
      (local.set $p (i31.get_s (ref.cast (ref i31) (local.get $i))))
      (local.set $d (i31.get_s (ref.cast (ref i31) (local.get $v))))
      (if (i32.lt_s (local.get $p) (i32.const 0))
         (then (call $caml_bound_error)))
      (if (i32.ge_u (i32.add (local.get $p) (i32.const 1))
             (array.get $int_array
                (struct.get $bigarray $ba_dim (local.get $ba))
                (i32.const 0)))
         (then (call $caml_bound_error)))
      (call $dv_set_i16
         (local.get $view) (local.get $p) (local.get $d) (i32.const 1))
      (ref.i31 (i32.const 0)))

   (func (export "caml_ba_uint8_set32")
      (param $vba (ref eq)) (param $i (ref eq)) (param $d i32)
      (result (ref eq))
      (local $ba (ref $bigarray))
      (local $view (ref extern))
      (local $p i32)
      (local.set $ba (ref.cast (ref $bigarray) (local.get $vba)))
      (local.set $view (struct.get $bigarray $ba_view (local.get $ba)))
      (local.set $p (i31.get_s (ref.cast (ref i31) (local.get $i))))
      (if (i32.lt_s (local.get $p) (i32.const 0))
         (then (call $caml_bound_error)))
      (if (i32.ge_u (i32.add (local.get $p) (i32.const 3))
             (array.get $int_array
                (struct.get $bigarray $ba_dim (local.get $ba))
                (i32.const 0)))
         (then (call $caml_bound_error)))
      (call $dv_set_i32
         (local.get $view) (local.get $p) (local.get $d) (i32.const 1))
      (ref.i31 (i32.const 0)))

   (func (export "caml_ba_uint8_set64")
      (param $vba (ref eq)) (param $i (ref eq)) (param $d i64)
      (result (ref eq))
      (local $ba (ref $bigarray))
      (local $view (ref extern))
      (local $p i32)
      (local.set $ba (ref.cast (ref $bigarray) (local.get $vba)))
      (local.set $view (struct.get $bigarray $ba_view (local.get $ba)))
      (local.set $p (i31.get_s (ref.cast (ref i31) (local.get $i))))
      (if (i32.lt_s (local.get $p) (i32.const 0))
         (then (call $caml_bound_error)))
      (if (i32.ge_u (i32.add (local.get $p) (i32.const 7))
             (array.get $int_array
                (struct.get $bigarray $ba_dim (local.get $ba))
                (i32.const 0)))
         (then (call $caml_bound_error)))
      (call $dv_set_i64
         (local.get $view) (local.get $p) (local.get $d) (i32.const 1))
      (ref.i31 (i32.const 0)))

   (export "caml_bytes_of_uint8_array" (func $caml_string_of_uint8_array))
   (func $caml_string_of_uint8_array (export "caml_string_of_uint8_array")
      (param (ref eq)) (result (ref eq))
      ;; used to convert a typed array to a string
      (local $a (ref extern)) (local $len i32)
      (local $s (ref $bytes))
      (local.set $a
         (ref.as_non_null (extern.convert_any (call $unwrap (local.get 0)))))
      (local.set $len (call $ta_length (local.get $a)))
      (local.set $s (array.new $bytes (i32.const 0) (local.get $len)))
      (call $ta_blit_to_bytes
         (local.get $a) (i32.const 0) (local.get $s) (i32.const 0)
         (local.get $len))
      (local.get $s))

   (export "caml_uint8_array_of_bytes" (func $caml_uint8_array_of_string))
   (func $caml_uint8_array_of_string (export "caml_uint8_array_of_string")
      (param (ref eq)) (result (ref eq))
      ;; Convert bytes to a typed array
      (local $ta (ref extern)) (local $len i32)
      (local $s (ref $bytes))
      (local.set $s (ref.cast (ref $bytes) (local.get 0)))
      (local.set $len (array.len (local.get $s)))
      (local.set $ta
         (call $ta_create
            (i32.const 3) ;; Uint8Array
            (local.get $len)))
      (call $ta_blit_from_bytes
         (local.get $s) (i32.const 0) (local.get $ta) (i32.const 0)
         (local.get $len))
      (call $wrap (any.convert_extern (local.get $ta))))

   (func (export "caml_ba_get_kind") (param (ref eq)) (result i32)
      (struct.get $bigarray $ba_kind (ref.cast (ref $bigarray) (local.get 0))))

   (func (export "caml_ba_get_layout") (param (ref eq)) (result i32)
      (struct.get $bigarray $ba_layout
         (ref.cast (ref $bigarray) (local.get 0))))

   (func (export "caml_ba_get_data") (param (ref eq)) (result (ref extern))
      (struct.get $bigarray $ba_data (ref.cast (ref $bigarray) (local.get 0))))

   (func (export "caml_ba_get_view") (param (ref eq)) (result (ref extern))
      (struct.get $bigarray $ba_view (ref.cast (ref $bigarray) (local.get 0))))

   (func (export "caml_ba_set_data") (param (ref eq)) (param (ref extern))
      (struct.set $bigarray $ba_data (ref.cast (ref $bigarray) (local.get 0))
         (local.get 1)))

   (func (export "caml_ba_get_dim") (param (ref eq)) (result (ref $int_array))
      (struct.get $bigarray $ba_dim (ref.cast (ref $bigarray) (local.get 0))))

   (func (export "caml_ba_alloc")
      (param $kind i32) (param $layout i32) (param $num_dims i32)
      (param $data (ref extern)) (param $dim (ref $int_array))
      (result (ref eq))
      (struct.new $bigarray
         (global.get $bigarray_ops)
         (local.get $data)
         (call $dv_make (local.get $data))
         (local.get $dim)
         (local.get $num_dims)
         (local.get $kind)
         (local.get $layout)))

   (func (export "bytes_set")
      (param $s externref) (param $i i32) (param $v i32)
      (array.set $bytes
         (ref.cast (ref null $bytes) (any.convert_extern (local.get $s)))
         (local.get $i) (local.get $v)))

   (func (export "bytes_get")
      (param $s externref) (param $i i32) (result i32)
      (array.get $bytes
         (ref.cast (ref null $bytes) (any.convert_extern (local.get $s)))
         (local.get $i)))
)
