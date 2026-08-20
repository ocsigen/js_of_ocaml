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
   (import "fail" "caml_failwith" (func $caml_failwith (param (ref eq))))
   (import "marshal" "caml_serialize_int_1"
      (func $caml_serialize_int_1 (param (ref eq)) (param i32)))
   (import "marshal" "caml_serialize_int_4"
      (func $caml_serialize_int_4 (param (ref eq)) (param i32)))
   (import "marshal" "caml_deserialize_uint_1"
      (func $caml_deserialize_uint_1 (param (ref eq)) (result i32)))
   (import "marshal" "caml_deserialize_int_4"
      (func $caml_deserialize_int_4 (param (ref eq)) (result i32)))
   (@if $portable-int
   (@then
      (import "ints" "parse_sign_and_base"
         (func $parse_sign_and_base
            (param (ref $bytes)) (result i32 i32 i32 i32)))
      (import "int64" "caml_i64_of_digits"
         (func $caml_i64_of_digits
            (param i32) (param i32) (param i32) (param (ref $bytes)) (param i32)
            (param (ref eq)) (result i64)))
      (import "int64" "caml_int64_compare"
         (func $caml_int64_compare (param i64) (param i64) (result i32)))
      (import "int64" "caml_int64_format"
         (func $caml_int64_format (param (ref eq)) (param (ref eq)) (result (ref eq))))
      (import "int64" "caml_portability_int64_cmp"
         (func $int64_cmp (param (ref eq)) (param (ref eq)) (param i32) (result i32)))
      (import "int64" "caml_portability_int64_dup"
         (func $int64_dup (param (ref eq)) (result (ref eq))))
      (import "int64" "Int64_val"
         (func $Int64_val (param (ref eq)) (result i64)))
      (import "int64" "caml_int64_bswap"
         (func $caml_int64_bswap (param i64) (result i64)))

      (import "marshal" "caml_serialize_int_8"
         (func $caml_serialize_int_8 (param (ref eq)) (param i64)))
      (import "marshal" "caml_deserialize_int_8"
         (func $caml_deserialize_int_8 (param (ref eq)) (result i64)))
   )
   (@else
   (import "ints" "parse_int"
      (func $parse_int
         (param (ref eq)) (param i32) (param (ref eq)) (result i32)))
   (import "int32" "int32_cmp"
      (func $int32_cmp
         (param (ref eq)) (param (ref eq)) (param i32) (result i32)))
   (import "int32" "int32_hash"
      (func $int32_hash (param (ref eq)) (result i32)))
   (import "int32" "int32_dup"
      (func $int32_dup (param (ref eq)) (result (ref eq))))
   (import "int32" "Int32_val"
      (func $Int32_val (param (ref eq)) (result i32)))
   (import "int32" "caml_int32_compare"
      (func $caml_int32_compare (param i32) (param i32) (result i32)))
   (import "int32" "caml_int32_bswap"
      (func $caml_int32_bswap (param i32) (result i32)))
   (import "int32" "caml_int32_format"
      (func $caml_int32_format
         (param (ref eq)) (param (ref eq)) (result (ref eq))))
   ))

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
   (type $custom (sub (struct (field (ref $custom_operations)))))

   (type $int32
      (sub final $custom (struct (field (ref $custom_operations)) (field i32))))

   (@if $portable-int
   (@then
      (type $int64
         (sub final $custom (struct (field (ref $custom_operations)) (field i64))))

      (func (export "Nativeint_val") (param $v (ref eq)) (result i64)
         (return_call $Int64_val (local.get $v)))
      (func (export "caml_nativeint_bswap") (param $i i64) (result i64)
         (return_call $caml_int64_bswap (local.get $i)))
      (func (export "caml_nativeint_compare")
         (param $i1 i64) (param $i2 i64) (result i32)
         (return_call $caml_int64_compare (local.get $i1) (local.get $i2)))

      (func $nativeint_serialize
         (param $s (ref eq)) (param $v (ref eq)) (result i32) (result i32)
         (local $l i64)
         (local.set $l
            (struct.get $int64 1 (ref.cast (ref $int64) (local.get $v))))
         (if (i64.eq (local.get $l)
                (i64.extend_i32_s (i32.wrap_i64 (local.get $l))))
            (then
               (call $caml_serialize_int_1 (local.get $s) (i32.const 1))
               (call $caml_serialize_int_4 (local.get $s)
                  (i32.wrap_i64 (local.get $l))))
            (else
               (call $caml_serialize_int_1 (local.get $s) (i32.const 2))
               (call $caml_serialize_int_8 (local.get $s) (local.get $l))))
         (i32.const 4) (i32.const 8))

      (func $nativeint_hash (param $v (ref eq)) (result i32)
         (local $n i64)
         (local.set $n
            (struct.get $int64 1 (ref.cast (ref $int64) (local.get $v))))
         ;; C runtime hashes nativeint as an intnat: ints.c nativeint_hash
         ;; returns (n >> 32) ^ (n >> 63) ^ n, and hash.c then truncates the
         ;; custom hash result to its low 32 bits before mixing.
         (i32.wrap_i64
            (i64.xor
               (i64.xor (i64.shr_s (local.get $n) (i64.const 32))
                        (i64.shr_s (local.get $n) (i64.const 63)))
               (local.get $n))))


      (global $nativeint_ops (export "nativeint_ops") (ref $custom_operations)
      (struct.new $custom_operations
         (@string "_n")
         (ref.func $int64_cmp)
         (ref.null $compare)
         (ref.func $nativeint_hash)
         (struct.new $fixed_length (i32.const 4) (i32.const 8))
         (ref.func $nativeint_serialize)
         (ref.func $nativeint_deserialize)
         (ref.func $int64_dup)))
   )
   (@else
   (export "Nativeint_val" (func $Int32_val))

   (export "caml_nativeint_bswap" (func $caml_int32_bswap))

   (export "caml_nativeint_compare" (func $caml_int32_compare))

   (func $nativeint_serialize
      (param $s (ref eq)) (param $v (ref eq)) (result i32) (result i32)
      (call $caml_serialize_int_1 (local.get $s) (i32.const 1))
      (call $caml_serialize_int_4 (local.get $s)
         (struct.get $int32 1 (ref.cast (ref $int32) (local.get $v))))
      (i32.const 4) (i32.const 8))

   (global $nativeint_ops (export "nativeint_ops") (ref $custom_operations)
      (struct.new $custom_operations
         (@string "_n")
         (ref.func $int32_cmp)
         (ref.null $compare)
         (ref.func $int32_hash)
         (struct.new $fixed_length (i32.const 4) (i32.const 8))
         (ref.func $nativeint_serialize)
         (ref.func $nativeint_deserialize)
         (ref.func $int32_dup)))
   ))


   (@if $portable-int
   (@then
      (@string $ill_formed "input_value: ill-formed native integer")

      (func $nativeint_deserialize
         (param $s (ref eq)) (result (ref eq)) (result i32)
         (local $tag i32) (local $l i64)
         (local.set $tag (call $caml_deserialize_uint_1 (local.get $s)))
         (if (i32.eq (local.get $tag) (i32.const 1))
            (then
               (local.set $l
                  (i64.extend_i32_s
                     (call $caml_deserialize_int_4 (local.get $s)))))
            (else
               (if (i32.eq (local.get $tag) (i32.const 2))
                  (then
                     (local.set $l
                        (call $caml_deserialize_int_8 (local.get $s))))
                  (else
                     (call $caml_failwith (global.get $ill_formed))))))
         (struct.new $int64 (global.get $nativeint_ops) (local.get $l))
         (i32.const 8))

      (func $caml_copy_nativeint (export "caml_copy_nativeint")
         (param $i i64) (result (ref eq))
         (struct.new $int64 (global.get $nativeint_ops) (local.get $i)))

      (func (export "caml_nativeint_of_string")
         (param $v (ref eq)) (result (ref eq))
         (local $s (ref $bytes))
         (local $i i32) (local $signedness i32) (local $sign i32)
         (local $base i32)
         (local.set $s (ref.cast (ref $bytes) (local.get $v)))
         (call $parse_sign_and_base (local.get $s))
         (local.set $base)
         (local.set $sign)
         (local.set $signedness)
         (local.set $i)
         (return_call $caml_copy_nativeint
            (call $caml_i64_of_digits (local.get $base)
               (local.get $signedness) (local.get $sign)
               (local.get $s) (local.get $i)
               (global.get $NATIVEINT_ERRMSG)))))
   (@else
   (@string $integer_too_large "input_value: native integer value too large")

   (func $nativeint_deserialize
      (param $s (ref eq)) (result (ref eq)) (result i32)
      (if (i32.ne (call $caml_deserialize_uint_1 (local.get $s)) (i32.const 1))
         (then (call $caml_failwith (global.get $integer_too_large))))
      (struct.new $int32 (global.get $nativeint_ops)
         (call $caml_deserialize_int_4 (local.get $s)))
      (i32.const 4))

   (func $caml_copy_nativeint (export "caml_copy_nativeint")
      (param $i i32) (result (ref eq))
      (struct.new $int32 (global.get $nativeint_ops) (local.get $i)))
   ))

   (@string $NATIVEINT_ERRMSG "Nativeint.of_string")

   (@if $portable-int
   (@then
      (export "caml_nativeint_format" (func $caml_int64_format)))
   (@else
   (func (export "caml_nativeint_of_string")
      (param $v (ref eq)) (result (ref eq))
      (return_call $caml_copy_nativeint
         (call $parse_int
            (local.get $v) (i32.const 32) (global.get $NATIVEINT_ERRMSG))))

   (export "caml_nativeint_format" (func $caml_int32_format))

   ))
)
