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
   (import "marshal" "caml_serialize_int_4"
      (func $caml_serialize_int_4 (param (ref eq)) (param i32)))
   (import "marshal" "caml_deserialize_int_4"
      (func $caml_deserialize_int_4 (param (ref eq)) (result i32)))
   (import "float" "caml_parse_float"
      (func $caml_parse_float (param (ref eq) (ref eq)) (result (ref eq))))
   (import "float" "caml_format_float"
      (func $caml_format_float (param (ref eq)) (param (ref eq)) (result (ref eq))))
(@if $wasi
(@then
   (import "libc" "strtof" (func $strtof (param i32) (param i32) (result f32)))
   (import "io" "IO_BUFFER_SIZE" (global $IO_BUFFER_SIZE i32))
   (import "wasi_memory" "get_buffer" (func $get_buffer (result i32)))
   (import "wasi_memory" "write_string_to_memory"
      (func $write_string_to_memory (param i32 i32 (ref eq)) (result i32)))
   (import "wasi_memory" "release_memory"
      (func $release_memory (param i32 i32)))
)
(@else
   (import "bindings" "float32_exact_compare"
      (func $float32_exact_compare (param externref) (param f64) (result i32)))
   (import "jsstring" "jsstring_of_bytes"
      (func $jsstring_of_bytes (param (ref $bytes)) (result externref)))
))
   (import "float" "caml_erf_float"
      (func $caml_erf_float (param f64) (result f64)))
   (import "float" "caml_erfc_float"
      (func $caml_erfc_float (param f64) (result f64)))
   (import "float" "caml_frexp_float"
      (func $caml_frexp_float (param (ref eq)) (result (ref eq))))
   (import "float" "caml_ldexp_float"
      (func $caml_ldexp_float (param f64) (param i32) (result f64)))
   (import "bigarray" "caml_ba_uint8_get32"
      (func $caml_ba_uint8_get32 (param (ref eq)) (param i32) (result i32)))
   (import "bigarray" "caml_ba_uint8_get32u"
      (func $caml_ba_uint8_get32u (param (ref eq)) (param i32) (result i32)))
   (import "bigarray" "caml_ba_uint8_set32"
      (func $caml_ba_uint8_set32 (param (ref eq)) (param i32) (param i32) (result (ref eq))))
   (import "bigarray" "caml_ba_uint8_set32u"
      (func $caml_ba_uint8_set32u (param (ref eq)) (param i32) (param i32) (result (ref eq))))
   (import "string" "caml_string_get32"
      (func $caml_string_get32 (param (ref eq)) (param i32) (result i32)))
   (import "string" "caml_string_get32u"
      (func $caml_string_get32u (param (ref eq)) (param i32) (result i32)))
   (import "string" "caml_bytes_get32"
      (func $caml_bytes_get32 (param (ref eq)) (param i32) (result i32)))
   (import "string" "caml_bytes_get32u"
      (func $caml_bytes_get32u (param (ref eq)) (param i32) (result i32)))
   (import "string" "caml_bytes_set32"
      (func $caml_bytes_set32 (param (ref eq)) (param i32) (param i32) (result (ref eq))))
   (import "string" "caml_bytes_set32u"
      (func $caml_bytes_set32u (param (ref eq)) (param i32) (param i32) (result (ref eq))))

   (type $float (struct (field $f f64)))

   (func $box_float (param $f f64) (result (ref eq))
      (struct.new $float (local.get $f)))

   (func $unbox_float (param $f (ref eq)) (result f64)
      (struct.get $float 0 (ref.cast (ref $float) (local.get $f))))

   (type $block (array (mut (ref eq))))
   (type $bytes (array (mut i8)))
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
   (type $custom (sub (struct (field $ops (ref $custom_operations)))))

   (global $float32_ops (export "float32_ops") (ref $custom_operations)
      (struct.new $custom_operations
         (@string "_f32")
         (ref.func $float32_cmp)
         (ref.null $compare)
         (ref.func $float32_hash)
         (struct.new $fixed_length (i32.const 4) (i32.const 4))
         (ref.func $float32_serialize)
         (ref.func $float32_deserialize)
         (ref.func $float32_dup)))

   (type $float32
      (sub final $custom (struct (field (ref $custom_operations)) (field $f32 f32))))

   (func $box_float32 (param $f32 f32) (result (ref eq))
      (struct.new $float32 (global.get $float32_ops) (local.get $f32)))

   (func $unbox_float32 (param $f (ref eq)) (result f32)
      (struct.get $float32 1 (ref.cast (ref $float32) (local.get $f))))

   (func $float32_cmp
      (param $v1 (ref eq)) (param $v2 (ref eq)) (param i32) (result i32)
      (local $x f32) (local $y f32)
      (local.set $x (call $unbox_float32 (local.get $v1)))
      (local.set $y (call $unbox_float32 (local.get $v2)))
      (i32.add
         (i32.sub (f32.gt (local.get $x) (local.get $y))
                  (f32.lt (local.get $x) (local.get $y)))
         (i32.sub (f32.eq (local.get $x) (local.get $x))
                  (f32.eq (local.get $y) (local.get $y)))))

   (func $float32_hash (param $v (ref eq)) (result i32)
      (local $i i32)
      (local.set $i (i32.reinterpret_f32 (call $unbox_float32 (local.get $v))))
      ;; Normalize NaNs and negative zero so that equal float32 values hash
      ;; identically, like caml_hash_mix_float.
      (if (i32.eq (i32.and (local.get $i) (i32.const 0x7F800000))
                  (i32.const 0x7F800000))
         (then
            (if (i32.ne (i32.and (local.get $i) (i32.const 0x7FFFFF))
                        (i32.const 0))
               (then (local.set $i (i32.const 0x7F800001))))))
      (if (i32.eq (local.get $i) (i32.const 0x80000000))
         (then (local.set $i (i32.const 0))))
      (local.get $i))

   (func $float32_serialize
       (param $s (ref eq)) (param $v (ref eq)) (result i32) (result i32)
       (call $caml_serialize_int_4 (local.get $s)
           (i32.reinterpret_f32 (call $unbox_float32 (local.get $v))))
       (i32.const 4) (i32.const 4))

   (func $float32_deserialize (param $s (ref eq)) (result (ref eq) i32)
      (call $box_float32
         (f32.reinterpret_i32 (call $caml_deserialize_int_4 (local.get $s))))
      (i32.const 4))

   (func $float32_dup
      (param $v (ref eq)) (result (ref eq))
      (local $d (ref $float32))
      (local.set $d (ref.cast (ref $float32) (local.get $v)))
      (struct.new $float32
         (struct.get $float32 0 (local.get $d))
         (struct.get $float32 1 (local.get $d))))

   (func $caml_float_of_float32
      (param $f32 (ref eq)) (result (ref eq))
      (call $box_float (f64.promote_f32 (call $unbox_float32 (local.get $f32)))))

   (func $caml_float32_of_float
      (param $f64 (ref eq)) (result (ref eq))
      (call $box_float32 (f32.demote_f64 (call $unbox_float (local.get $f64)))))

   (func (export "caml_round_float32_bytecode") (param $x f32) (result f32)
      (local $y f32)
      (if (result f32) (f32.ge (local.get $x) (f32.const 0))
         (then
            (local.set $y (f32.floor (local.get $x)))
            (if (result f32)
                (f32.ge (f32.sub (local.get $x) (local.get $y)) (f32.const 0.5))
               (then (f32.add (local.get $y) (f32.const 1)))
               (else (local.get $y))))
         (else
            (local.set $y (f32.ceil (local.get $x)))
            (if (result f32)
                (f32.ge (f32.sub (local.get $y) (local.get $x)) (f32.const 0.5))
               (then (f32.sub (local.get $y) (f32.const 1)))
               (else (local.get $y))))))

   (@string $float32_of_string "float32_of_string")

(@if $wasi
(@then
   ;; strtof on the absolute value of the literal (without its leading
   ;; whitespace, its sign and its underscores): wasi-libc's strtof returns
   ;; +0 for some negative values that round to the smallest subnormal or
   ;; to zero
   (func $strtof_bytes (param $s (ref $bytes)) (result f32)
      (local $t (ref $bytes)) (local $len i32) (local $i i32) (local $j i32)
      (local $c i32) (local $buffer i32) (local $buf i32) (local $f f32)
      (local.set $len (array.len (local.get $s)))
      (local.set $t (array.new $bytes (i32.const 0) (local.get $len)))
      (loop $copy
         (if (i32.lt_u (local.get $i) (local.get $len))
            (then
               (local.set $c (array.get_u $bytes (local.get $s) (local.get $i)))
               (if (i32.and (i32.ne (local.get $c) (@char "_"))
                      (i32.or (i32.ne (local.get $j) (i32.const 0))
                         (i32.eqz
                            (i32.or
                               (i32.or (i32.eq (local.get $c) (@char "-"))
                                  (i32.eq (local.get $c) (@char "+")))
                               (i32.or (i32.eq (local.get $c) (@char " "))
                                  (i32.lt_u (i32.sub (local.get $c) (i32.const 9))
                                     (i32.const 5)))))))
                  (then
                     (array.set $bytes (local.get $t) (local.get $j)
                        (local.get $c))
                     (local.set $j (i32.add (local.get $j) (i32.const 1)))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $copy))))
      (local.set $s (array.new $bytes (i32.const 0) (local.get $j)))
      (array.copy $bytes $bytes (local.get $s) (i32.const 0)
         (local.get $t) (i32.const 0) (local.get $j))
      (local.set $buffer (call $get_buffer))
      (local.set $buf
         (call $write_string_to_memory
            (i32.add (local.get $buffer) (i32.const 4))
            (global.get $IO_BUFFER_SIZE)
            (local.get $s)))
      (local.set $f (call $strtof (local.get $buf) (local.get $buffer)))
      (call $release_memory (i32.add (local.get $buffer) (i32.const 4))
         (local.get $buf))
      (local.get $f))
))

   (func (export "caml_float32_of_string") (param $s (ref eq)) (result (ref eq))
      (local $d f64) (local $a f64) (local $mid f64)
      (local $f f32) (local $lo f32) (local $hi f32) (local $c i32)
      (local.set $d
         (call $unbox_float
            (call $caml_parse_float (global.get $float32_of_string)
               (local.get $s))))
      (local.set $f (f32.demote_f64 (local.get $d)))
      (if (i32.or (f64.eq (f64.promote_f32 (local.get $f)) (local.get $d))
             (f64.ne (local.get $d) (local.get $d)))
         (then (return (call $box_float32 (local.get $f)))))
      ;; Rounding the literal to the double d may have moved it to the
      ;; midpoint between two float32 values; rounding d to a float32 then
      ;; picks the even one, which is wrong if the literal is on the other
      ;; side of the midpoint. In that case, look at the literal again.
      (local.set $a (f64.abs (local.get $d)))
      (if (f64.gt (f64.promote_f32 (f32.abs (local.get $f))) (local.get $a))
         (then
            (local.set $hi (f32.abs (local.get $f)))
            (local.set $lo
               (f32.reinterpret_i32
                  (i32.sub (i32.reinterpret_f32 (local.get $hi)) (i32.const 1)))))
         (else
            (local.set $lo (f32.abs (local.get $f)))
            (local.set $hi
               (f32.reinterpret_i32
                  (i32.add (i32.reinterpret_f32 (local.get $lo)) (i32.const 1))))))
      (local.set $mid
         (if (result f64) (f32.eq (local.get $hi) (f32.const inf))
            (then (f64.const 0x1.ffffffp127)) ;; 2^128 - 2^103
            (else
               (f64.mul
                  (f64.add (f64.promote_f32 (local.get $lo))
                     (f64.promote_f32 (local.get $hi)))
                  (f64.const 0.5)))))
      (if (f64.ne (local.get $a) (local.get $mid))
         (then (return (call $box_float32 (local.get $f)))))
(@if $wasi
(@then
      (call $box_float32
         (f32.copysign
            (call $strtof_bytes (ref.cast (ref $bytes) (local.get $s)))
            (local.get $f)))
)
(@else
      ;; sign of |literal| - a
      (local.set $c
         (call $float32_exact_compare
            (call $jsstring_of_bytes (ref.cast (ref $bytes) (local.get $s)))
            (local.get $a)))
      (if (i32.eqz (local.get $c))
         (then (return (call $box_float32 (local.get $f)))))
      (call $box_float32
         (f32.copysign
            (select (local.get $hi) (local.get $lo)
               (i32.gt_s (local.get $c) (i32.const 0)))
            (local.get $f)))
)))

   (func (export "caml_format_float32")
      (param $s (ref eq)) (param $f (ref eq)) (result (ref eq))
      (call $caml_format_float
         (local.get $s) (call $caml_float_of_float32 (local.get $f))))

   (func (export "caml_float32_compare")
      (param $x f32) (param $y f32) (result i32)
      (i32.add
         (i32.sub (f32.gt (local.get $x) (local.get $y))
                  (f32.lt (local.get $x) (local.get $y)))
         (i32.sub (f32.eq (local.get $x) (local.get $x))
                  (f32.eq (local.get $y) (local.get $y)))))

   (func (export "caml_modf_float32") (param $vx (ref eq)) (result (ref eq))
      (local $x f32) (local $a f32) (local $i f32) (local $f f32)
      (local.set $x (call $unbox_float32 (local.get $vx)))
      (local.set $a (f32.abs (local.get $x)))
      (if (f32.ge (local.get $a) (f32.const 0))
         (then
            (if (f32.lt (local.get $a) (f32.const inf))
               (then ;; normal
                  (local.set $i (f32.floor (local.get $a)))
                  (local.set $f (f32.sub (local.get $a) (local.get $i)))
                  (local.set $i (f32.copysign (local.get $i) (local.get $x)))
                  (local.set $f (f32.copysign (local.get $f) (local.get $x))))
               (else ;; infinity
                  (local.set $i (local.get $x))
                  (local.set $f (f32.copysign (f32.const 0) (local.get $x))))))
         (else ;; zero or nan
            (local.set $i (local.get $x))
            (local.set $f (local.get $x))))
      (array.new_fixed $block 3 (ref.i31 (i32.const 0))
         (call $box_float32 (local.get $f)) (call $box_float32 (local.get $i))))

   (func (export "caml_nextafter_float32_bytecode")
      (param $x f32) (param $y f32) (result f32)
      (local $i i32) (local $j i32)
      (if (f32.ne (local.get $x) (local.get $x)) (then (return (local.get $x))))
      (if (f32.ne (local.get $y) (local.get $y)) (then (return (local.get $y))))
      (if (f32.eq (local.get $x) (local.get $y))
         (then (return (local.get $y))))
      (if (result f32) (f32.eq (local.get $x) (f32.const 0))
         (then
            (if (result f32) (f32.ge (local.get $y) (f32.const 0))
               (then (return (f32.const 0x1p-149)))
               (else (return (f32.const -0x1p-149)))))
         (else
            (local.set $i (i32.reinterpret_f32 (local.get $x)))
            (local.set $j (i32.reinterpret_f32 (local.get $y)))
            (if (i32.and (i32.lt_s (local.get $i) (local.get $j))
                         (i32.lt_u (local.get $i) (local.get $j)))
               (then (local.set $i (i32.add (local.get $i) (i32.const 1))))
               (else (local.set $i (i32.sub (local.get $i) (i32.const 1)))))
            (return (f32.reinterpret_i32 (local.get $i))))))

   (func (export "caml_classify_float32_bytecode")
      (param $a f32) (result i32)
      (local.set $a (f32.abs (local.get $a)))
      (if (result i32) (f32.ge (local.get $a) (f32.const 0x1p-126))
         (then
            (if (result i32) (f32.lt (local.get $a) (f32.const inf))
               (then (i32.const 0)) ;; normal
               (else (i32.const 3)))) ;; infinity
         (else
            (if (result i32) (f32.eq (local.get $a) (f32.const 0))
               (then (i32.const 2)) ;; zero
               (else
                  (if (result i32) (f32.eq (local.get $a) (local.get $a))
                     (then (i32.const 1)) ;; subnormal
                     (else (i32.const 4)))))))) ;; nan

   (func (export "caml_ldexp_float32_bytecode")
      (param $x f32) (param $n i32) (result f32)
      (f32.demote_f64
         (call $caml_ldexp_float
            (f64.promote_f32 (local.get $x)) (local.get $n))))

   (func (export "caml_frexp_float32")
      (param $v (ref eq)) (result (ref eq))
      (local $frexp (ref $block))
      (local.set $frexp (ref.cast (ref $block)
         (call $caml_frexp_float (call $caml_float_of_float32 (local.get $v)))))
      (array.new_fixed $block 3 (ref.i31 (i32.const 0))
         (call $caml_float32_of_float
            (array.get $block (local.get $frexp) (i32.const 1)))
            (array.get $block (local.get $frexp) (i32.const 2))))

   (func (export "caml_erf_float32_bytecode") (param $x f32) (result f32)
      (f32.demote_f64 (call $caml_erf_float (f64.promote_f32 (local.get $x)))))

   (func (export "caml_erfc_float32_bytecode") (param $x f32) (result f32)
      (f32.demote_f64 (call $caml_erfc_float (f64.promote_f32 (local.get $x)))))

   ;; Rounding a value to a double, then to a float32, can give a different
   ;; result than rounding it directly to a float32: the first rounding can
   ;; land exactly halfway between two float32 values. This does not happen
   ;; if the first rounding is to odd (the double with an odd significand
   ;; among the two surrounding the value, when it is not representable).
   ;; Given the double [s] nearest to a value and their difference [err]
   ;; (value - s), this returns the value rounded to odd.
   (func $round_odd (param $s f64) (param $err f64) (result f64)
      (local $b i64)
      (if (i32.or (f64.eq (local.get $err) (f64.const 0))
             (i32.or (f64.eq (local.get $s) (f64.const 0))
                (f64.ne (f64.sub (local.get $s) (local.get $s))
                   (f64.const 0)))) ;; not finite
         (then (return (local.get $s))))
      (local.set $b (i64.reinterpret_f64 (local.get $s)))
      (if (i32.wrap_i64 (i64.and (local.get $b) (i64.const 1)))
         (then (return (local.get $s))))
      ;; move one ulp toward the value: up in magnitude iff err and s have
      ;; the same sign
      (f64.reinterpret_i64
         (select (i64.add (local.get $b) (i64.const 1))
            (i64.sub (local.get $b) (i64.const 1))
            (i32.eq (f64.gt (local.get $err) (f64.const 0))
               (f64.gt (local.get $s) (f64.const 0))))))

   (func (export "caml_fma_float32_bytecode")
      (param $x f32) (param $y f32) (param $z32 f32) (result f32)
      (local $p f64) (local $z f64) (local $s f64) (local $b f64)
      ;; The product of two float32 values is exact as a double; s + err is
      ;; the exact value of x * y + z (TwoSum)
      (local.set $p
         (f64.mul (f64.promote_f32 (local.get $x))
            (f64.promote_f32 (local.get $y))))
      (local.set $z (f64.promote_f32 (local.get $z32)))
      (local.set $s (f64.add (local.get $p) (local.get $z)))
      (local.set $b (f64.sub (local.get $s) (local.get $p)))
      (f32.demote_f64
         (call $round_odd (local.get $s)
            (f64.add
               (f64.sub (local.get $p)
                  (f64.sub (local.get $s) (local.get $b)))
               (f64.sub (local.get $z) (local.get $b))))))

   (func (export "caml_ba_uint8_getf32")
      (param $a (ref eq)) (param $i i32) (result f32)
      (f32.reinterpret_i32
         (call $caml_ba_uint8_get32 (local.get $a) (local.get $i))))

   (func (export "caml_ba_uint8_getf32u")
      (param $a (ref eq)) (param $i i32) (result f32)
      (f32.reinterpret_i32
         (call $caml_ba_uint8_get32u (local.get $a) (local.get $i))))

   (func (export "caml_ba_uint8_setf32")
      (param $a (ref eq)) (param $i i32) (param $v f32) (result (ref eq))
      (call $caml_ba_uint8_set32 (local.get $a) (local.get $i)
         (i32.reinterpret_f32 (local.get $v))))

   (func (export "caml_ba_uint8_setf32u")
      (param $a (ref eq)) (param $i i32) (param $v f32) (result (ref eq))
      (call $caml_ba_uint8_set32u (local.get $a) (local.get $i)
         (i32.reinterpret_f32 (local.get $v))))

   (func (export "caml_string_getf32")
      (param $a (ref eq)) (param $i i32) (result f32)
      (f32.reinterpret_i32
         (call $caml_string_get32 (local.get $a) (local.get $i))))

   (func (export "caml_string_getf32u")
      (param $a (ref eq)) (param $i i32) (result f32)
      (f32.reinterpret_i32
         (call $caml_string_get32u (local.get $a) (local.get $i))))

   (func (export "caml_bytes_getf32")
      (param $a (ref eq)) (param $i i32) (result f32)
      (f32.reinterpret_i32
         (call $caml_bytes_get32 (local.get $a) (local.get $i))))

   (func (export "caml_bytes_getf32u")
      (param $a (ref eq)) (param $i i32) (result f32)
      (f32.reinterpret_i32
         (call $caml_bytes_get32u (local.get $a) (local.get $i))))

   (func (export "caml_bytes_setf32")
      (param $a (ref eq)) (param $i i32) (param $v f32) (result (ref eq))
      (call $caml_bytes_set32 (local.get $a) (local.get $i)
         (i32.reinterpret_f32 (local.get $v))))

   ;; Mirrors x86 MINSS/MAXSS: returns $y when the comparison fails
   ;; (NaN or equal, including ±0). Unlike f32.min/f32.max, which
   ;; propagate NaN and canonicalise -0 vs +0.
   (func (export "caml_simd_float32_min_bytecode")
      (param $x f32) (param $y f32) (result f32)
      (if (result f32) (f32.lt (local.get $x) (local.get $y))
         (then (local.get $x))
         (else (local.get $y))))

   (func (export "caml_simd_float32_max_bytecode")
      (param $x f32) (param $y f32) (result f32)
      (if (result f32) (f32.gt (local.get $x) (local.get $y))
         (then (local.get $x))
         (else (local.get $y))))

   (func (export "caml_bytes_setf32u")
      (param $a (ref eq)) (param $i i32) (param $v f32) (result (ref eq))
      (call $caml_bytes_set32u (local.get $a) (local.get $i)
         (i32.reinterpret_f32 (local.get $v))))
)
