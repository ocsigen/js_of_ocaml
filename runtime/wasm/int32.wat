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
   (import "ints" "parse_int"
      (func $parse_int
         (param (ref eq)) (param i32) (param (ref eq)) (result i32)))
   (import "ints" "format_int"
      (func $format_int
         (param (ref eq)) (param i32) (param i32) (result (ref eq))))
   (import "fail" "caml_failwith" (func $caml_failwith (param (ref eq))))
   (import "marshal" "caml_serialize_int_1"
      (func $caml_serialize_int_1 (param (ref eq)) (param i32)))
   (import "marshal" "caml_serialize_int_4"
      (func $caml_serialize_int_4 (param (ref eq)) (param i32)))
   (import "marshal" "caml_deserialize_uint_1"
      (func $caml_deserialize_uint_1 (param (ref eq)) (result i32)))
   (import "marshal" "caml_deserialize_int_4"
      (func $caml_deserialize_int_4 (param (ref eq)) (result i32)))

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

   (global $int32_ops (export "int32_ops") (ref $custom_operations)
      (struct.new $custom_operations
         (@string "_i")
         (ref.func $int32_cmp)
         (ref.null $compare)
         (ref.func $int32_hash)
         (struct.new $fixed_length (i32.const 4) (i32.const 4))
         (ref.func $int32_serialize)
         (ref.func $int32_deserialize)
         (ref.func $int32_dup)))

   (type $int32
      (sub final $custom (struct (field (ref $custom_operations)) (field i32))))

   (func $int32_cmp
      (param $v1 (ref eq)) (param $v2 (ref eq)) (param i32) (result i32)
      (local $i1 i32) (local $i2 i32)
      (local.set $i1
         (struct.get $int32 1 (ref.cast (ref $int32) (local.get $v1))))
      (local.set $i2
         (struct.get $int32 1 (ref.cast (ref $int32) (local.get $v2))))
      (i32.sub (i32.gt_s (local.get $i1) (local.get $i2))
               (i32.lt_s (local.get $i1) (local.get $i2))))

   (func $int32_hash (param $v (ref eq)) (result i32)
      (struct.get $int32 1 (ref.cast (ref $int32) (local.get $v))))

   (func $int32_serialize
      (param $s (ref eq)) (param $v (ref eq)) (result i32) (result i32)
      (call $caml_serialize_int_4 (local.get $s)
         (struct.get $int32 1 (ref.cast (ref $int32) (local.get $v))))
      (tuple.make 2 (i32.const 4) (i32.const 4)))

   (func $int32_deserialize (param $s (ref eq)) (result (ref eq)) (result i32)
      (tuple.make 2
         (struct.new $int32 (global.get $int32_ops)
            (call $caml_deserialize_int_4 (local.get $s)))
         (i32.const 4)))

   (func $int32_dup (param $v (ref eq)) (result (ref eq))
      (local $d (ref $int32))
      (local.set $d (ref.cast (ref $int32) (local.get $v)))
      (struct.new $int32
         (struct.get $int32 0 (local.get $d))
         (struct.get $int32 1 (local.get $d))))

   (func $caml_copy_int32 (export "caml_copy_int32")
      (param $i i32) (result (ref eq))
      (struct.new $int32 (global.get $int32_ops) (local.get $i)))

   (export "Nativeint_val" (func $Int32_val))
   (func $Int32_val (export "Int32_val") (param (ref eq)) (result i32)
      (struct.get $int32 1 (ref.cast (ref $int32) (local.get 0))))

   (export "caml_nativeint_bswap" (func $caml_int32_bswap))
   (func $caml_int32_bswap (export "caml_int32_bswap")
      (param $i i32) (result i32)
      (i32.or
         (i32.rotr (i32.and (local.get $i) (i32.const 0x00FF00FF))
                   (i32.const 8))
         (i32.rotl (i32.and (local.get $i) (i32.const 0xFF00FF00))
                   (i32.const 8))))

   (@string $INT32_ERRMSG "Int32.of_string")

   (func (export "caml_int32_of_string") (param $v (ref eq)) (result (ref eq))
      (return_call $caml_copy_int32
         (call $parse_int
            (local.get $v) (i32.const 32) (global.get $INT32_ERRMSG))))

   (data $integer_conversion_error "error while converting from int32")

   (func $caml_checked_int32_to_int (export "caml_checked_int32_to_int")
      (param i32) (result (ref eq))
      (if (i32.or (i32.gt_s (local.get 0) (i32.const  0x3FFFFFFF))
                  (i32.lt_s (local.get 0) (i32.const -0x40000000)))
          (then (call $caml_failwith
                      (array.new_data $bytes $integer_conversion_error
                                      (i32.const 0) (i32.const 33)))))
      (ref.i31 (local.get 0)))

   (func $caml_checked_nativeint_to_int (export "caml_checked_nativeint_to_int")
      (param i32) (result (ref eq))
      (call $caml_checked_int32_to_int (local.get 0)))

   (export "caml_nativeint_compare" (func $caml_int32_compare))
   (func $caml_int32_compare (export "caml_int32_compare")
      (param $i1 i32) (param $i2 i32) (result (ref eq))
      (ref.i31 (i32.sub (i32.gt_s (local.get $i1) (local.get $i2))
                        (i32.lt_s (local.get $i1) (local.get $i2)))))

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

   (func $nativeint_serialize
      (param $s (ref eq)) (param $v (ref eq)) (result i32) (result i32)
      (call $caml_serialize_int_1 (local.get $s) (i32.const 1))
      (call $caml_serialize_int_4 (local.get $s)
         (struct.get $int32 1 (ref.cast (ref $int32) (local.get $v))))
      (tuple.make 2 (i32.const 4) (i32.const 8)))

   (@string $integer_too_large "input_value: native integer value too large")

   (func $nativeint_deserialize
      (param $s (ref eq)) (result (ref eq)) (result i32)
      (if (i32.ne (call $caml_deserialize_uint_1 (local.get $s)) (i32.const 1))
         (then (call $caml_failwith (global.get $integer_too_large))))
      (tuple.make 2
         (struct.new $int32 (global.get $nativeint_ops)
            (call $caml_deserialize_int_4 (local.get $s)))
         (i32.const 4)))

   (func $caml_copy_nativeint (export "caml_copy_nativeint")
      (param $i i32) (result (ref eq))
      (struct.new $int32 (global.get $nativeint_ops) (local.get $i)))

   (@string $NATIVEINT_ERRMSG "Nativeint.of_string")

   (func (export "caml_nativeint_of_string")
      (param $v (ref eq)) (result (ref eq))
      (return_call $caml_copy_nativeint
         (call $parse_int
            (local.get $v) (i32.const 32) (global.get $NATIVEINT_ERRMSG))))

   (export "caml_nativeint_format" (func $caml_int32_format))
   (func $caml_int32_format (export "caml_int32_format")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (return_call $format_int (local.get 0)
         (struct.get $int32 1
            (ref.cast (ref $int32) (local.get 1))) (i32.const 0)))


)
