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
   (import "ints" "parse_sign_and_base"
      (func $parse_sign_and_base
         (param (ref $bytes)) (result i32 i32 i32 i32)))
   (import "ints" "parse_digit" (func $parse_digit (param i32) (result i32)))
   (import "ints" "parse_int_format"
      (func $parse_int_format
         (param (ref $bytes)) (result i32 i32 i32 i32 i32)))
   (import "fail" "caml_failwith" (func $caml_failwith (param (ref eq))))
   (import "marshal" "caml_serialize_int_8"
      (func $caml_serialize_int_8 (param (ref eq)) (param i64)))
   (import "marshal" "caml_deserialize_int_8"
      (func $caml_deserialize_int_8 (param (ref eq)) (result i64)))
   (import "ints" "lowercase_hex_table"
      (global $lowercase_hex_table (ref $chars)))
   (import "ints" "uppercase_hex_table"
      (global $uppercase_hex_table (ref $chars)))

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

   (global $int64_ops (export "int64_ops") (ref $custom_operations)
      (struct.new $custom_operations
         (@string "_j")
         (ref.func $int64_cmp)
         (ref.null $compare)
         (ref.func $int64_hash)
         (struct.new $fixed_length (i32.const 8) (i32.const 8))
         (ref.func $int64_serialize)
         (ref.func $int64_deserialize)
         (ref.func $int64_dup)))

   (type $int64
      (sub final $custom (struct (field (ref $custom_operations)) (field i64))))

   (func $int64_cmp
      (param $v1 (ref eq)) (param $v2 (ref eq)) (param i32) (result i32)
      (local $i1 i64) (local $i2 i64)
      (local.set $i1
         (struct.get $int64 1 (ref.cast (ref $int64) (local.get $v1))))
      (local.set $i2
         (struct.get $int64 1 (ref.cast (ref $int64) (local.get $v2))))
      (i32.sub (i64.gt_s (local.get $i1) (local.get $i2))
               (i64.lt_s (local.get $i1) (local.get $i2))))

   (func $int64_hash (param $v (ref eq)) (result i32)
      (local $i i64)
      (local.set $i
         (struct.get $int64 1 (ref.cast (ref $int64) (local.get $v))))
      (i32.xor
         (i32.wrap_i64 (local.get $i))
         (i32.wrap_i64 (i64.shr_u (local.get $i) (i64.const 32)))))

   (func $int64_serialize
      (param $s (ref eq)) (param $v (ref eq)) (result i32) (result i32)
      (call $caml_serialize_int_8 (local.get $s)
         (struct.get $int64 1 (ref.cast (ref $int64) (local.get $v))))
      (tuple.make 2 (i32.const 8) (i32.const 8)))

   (func $int64_deserialize (param $s (ref eq)) (result (ref eq)) (result i32)
      (tuple.make 2
         (struct.new $int64 (global.get $int64_ops)
            (call $caml_deserialize_int_8 (local.get $s)))
         (i32.const 8)))

   (func $int64_dup (param $v (ref eq)) (result (ref eq))
      (struct.new $int64 (global.get $int64_ops)
         (struct.get $int64 1 (ref.cast (ref $int64) (local.get $v)))))

   (func $caml_copy_int64 (export "caml_copy_int64")
      (param $i i64) (result (ref eq))
      (struct.new $int64 (global.get $int64_ops) (local.get $i)))

   (func (export "Int64_val") (param (ref eq)) (result i64)
      (struct.get $int64 1 (ref.cast (ref $int64) (local.get 0))))

   (func (export "caml_int64_bswap") (param $i i64) (result i64)
      (i64.or
         (i64.or
            (i64.rotr (i64.and (local.get $i) (i64.const 0x000000FF000000FF))
                      (i64.const 8))
            (i64.rotr (i64.and (local.get $i) (i64.const 0x0000FF000000FF00))
                      (i64.const 24)))
         (i64.or
            (i64.rotl (i64.and (local.get $i) (i64.const 0x00FF000000FF0000))
                      (i64.const 24))
            (i64.rotl (i64.and (local.get $i) (i64.const 0xFF000000FF000000))
                      (i64.const 8)))))

   (func (export "caml_int64_compare")
      (param $i1 i64) (param $i2 i64) (result (ref eq))
      (ref.i31 (i32.sub (i64.gt_s (local.get $i1) (local.get $i2))
                        (i64.lt_s (local.get $i1) (local.get $i2)))))

   (@string $INT64_ERRMSG "Int64.of_string")

   ;; Parse a sequence of digits into an i64 as dicted by $base,
   ;; $signedness and $sign. The sequence is read in $s starting from $i.
   ;; In case of failure raise [Failure $errmsg].
   ;; Used by $caml_int64_of_string below and by $caml_uint64_of_string in
   ;; package "integers".
   (func $caml_i64_of_digits (export "caml_i64_of_digits")
      (param $base i32) (param $signedness i32) (param $sign i32)
      (param $s (ref $bytes)) (param $i i32) (param $errmsg (ref eq))
      (result i64)
      (local $len i32) (local $d i32) (local $c i32)
      (local $res i64) (local $threshold i64)
      (local.set $len (array.len (local.get $s)))
      (if (i32.eqz (local.get $len))
        (then (call $caml_failwith (local.get $errmsg))))
      (local.set $threshold
         (i64.div_u (i64.const -1) (i64.extend_i32_u (local.get $base))))
      (local.set $d
         (call $parse_digit (array.get_u $bytes (local.get $s) (local.get $i))))
      (if (i32.ge_u (local.get $d) (local.get $base))
         (then (call $caml_failwith (local.get $errmsg))))
      (local.set $res (i64.extend_i32_u (local.get $d)))
      (loop $loop
         (local.set $i (i32.add (local.get $i) (i32.const 1)))
         (if (i32.lt_s (local.get $i) (local.get $len))
            (then
               (local.set $c (array.get_u $bytes (local.get $s) (local.get $i)))
               (br_if $loop (i32.eq (local.get $c) (@char "_")))
               (local.set $d (call $parse_digit (local.get $c)))
               (if (i32.ge_u (local.get $d) (local.get $base))
                  (then (call $caml_failwith (local.get $errmsg))))
               (if (i64.gt_u (local.get $res) (local.get $threshold))
                  (then (call $caml_failwith (local.get $errmsg))))
               (local.set $res
                  (i64.add (i64.mul (local.get $res)
                              (i64.extend_i32_u (local.get $base)))
                           (i64.extend_i32_u (local.get $d))))
               (if (i64.lt_u (local.get $res) (i64.extend_i32_u (local.get $d)))
                  (then (call $caml_failwith (local.get $errmsg))))
               (br $loop))))
      (if (local.get $signedness)
         (then
            (if (i32.gt_s (local.get $sign) (i32.const 0))
               (then
                  (if (i64.ge_u (local.get $res)
                                (i64.shl (i64.const 1) (i64.const 63)))
                     (then (call $caml_failwith (local.get $errmsg)))))
               (else
                  (if (i64.gt_u (local.get $res)
                                (i64.shl (i64.const 1) (i64.const 63)))
                     (then
                        (call $caml_failwith (local.get $errmsg))))))))
      (if (i32.lt_s (local.get $sign) (i32.const 0))
         (then (local.set $res (i64.sub (i64.const 0) (local.get $res)))))
      (local.get $res))

   (func (export "caml_int64_of_string") (param $v (ref eq)) (result (ref eq))
      (local $s (ref $bytes))
      (local $i i32) (local $signedness i32) (local $sign i32) (local $base i32)
      (local $t (tuple i32 i32 i32 i32))
      (local.set $s (ref.cast (ref $bytes) (local.get $v)))
      (local.set $t (call $parse_sign_and_base (local.get $s)))
      (local.set $i (tuple.extract 4 0 (local.get $t)))
      (local.set $signedness (tuple.extract 4 1 (local.get $t)))
      (local.set $sign (tuple.extract 4 2 (local.get $t)))
      (local.set $base (tuple.extract 4 3 (local.get $t)))
      (return_call
        $caml_copy_int64
        (call $caml_i64_of_digits (local.get $base)
                                  (local.get $signedness)
                                  (local.get $sign)
                                  (local.get $s)
                                  (local.get $i)
                                  (global.get $INT64_ERRMSG))))

   (func $format_int64_default (param $d i64) (result (ref eq))
      (local $s (ref $bytes))
      (local $negative i32) (local $i i32) (local $n i64)
      (if (i64.lt_s (local.get $d) (i64.const 0))
         (then
            (local.set $negative (i32.const 1))
            (local.set $i (i32.const 1))
            (local.set $d (i64.sub (i64.const 0) (local.get $d)))))
      (local.set $n (local.get $d))
      (loop $count
         (local.set $i (i32.add (local.get $i) (i32.const 1)))
         (local.set $n (i64.div_u (local.get $n) (i64.const 10)))
         (br_if $count (i64.ne (local.get $n) (i64.const 0))))
      (local.set $s (array.new $bytes (i32.const 0) (local.get $i)))
      (loop $write
         (local.set $i (i32.sub (local.get $i) (i32.const 1)))
         (array.set $bytes (local.get $s) (local.get $i)
            (i32.add (@char "0")
               (i32.wrap_i64 (i64.rem_u (local.get $d) (i64.const 10)))))
         (local.set $d (i64.div_u (local.get $d) (i64.const 10)))
         (br_if $write (i64.ne (local.get $d) (i64.const 0))))
      (if (local.get $negative)
         (then (array.set $bytes (local.get $s) (i32.const 0) (@char "-"))))
      (local.get $s))

   (type $chars (array i8))

   (func (export "caml_int64_format")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (local $d i64)
      (local $s (ref $bytes))
      (local $format (tuple i32 i32 i32 i32 i32))
      (local $sign_style i32) (local $alternate i32) (local $signed i32)
      (local $base i64) (local $uppercase i32)
      (local $negative i32)
      (local $i i32)
      (local $n i64)
      (local $chars (ref $chars))
      (local.set $s (ref.cast (ref $bytes) (local.get 0)))
      (local.set $d (struct.get $int64 1 (ref.cast (ref $int64) (local.get 1))))
      (if (i32.eq (array.len (local.get $s)) (i32.const 2))
         (then
            (if (i32.eq (array.get_u $bytes (local.get $s) (i32.const 1))
                        (@char "d"))
               (then (return_call $format_int64_default (local.get $d))))))
      (local.set $format (call $parse_int_format (local.get $s)))
      (local.set $sign_style (tuple.extract 5 0 (local.get $format)))
      (local.set $alternate (tuple.extract 5 1 (local.get $format)))
      (local.set $signed (tuple.extract 5 2 (local.get $format)))
      (local.set $base
         (i64.extend_i32_u (tuple.extract 5 3 (local.get $format))))
      (local.set $uppercase (tuple.extract 5 4 (local.get $format)))
      (if (i32.and (local.get $signed) (i64.lt_s (local.get $d) (i64.const 0)))
         (then
            (local.set $negative (i32.const 1))
            (local.set $d (i64.sub (i64.const 0) (local.get $d)))))
      (local.set $n (local.get $d))
      (loop $count
         (local.set $i (i32.add (local.get $i) (i32.const 1)))
         (local.set $n (i64.div_u (local.get $n) (local.get $base)))
         (br_if $count (i64.ne (local.get $n) (i64.const 0))))
      (if (i32.or (local.get $negative)
                  (local.get $sign_style))
         (then (local.set $i (i32.add (local.get $i) (i32.const 1)))))
      (if (local.get $alternate)
         (then
            (if (i64.ne (local.get $d) (i64.const 0))
               (then
                  (if (i64.eq (local.get $base) (i64.const 16))
                     (then
                        (local.set $i (i32.add (local.get $i) (i32.const 2)))))
                  (if (i64.eq (local.get $base) (i64.const 8))
                     (then
                        (local.set $i
                           (i32.add (local.get $i) (i32.const 1)))))))))
      (local.set $chars
         (select (result (ref $chars))
            (global.get $uppercase_hex_table)
            (global.get $lowercase_hex_table)
            (local.get $uppercase)))
      (local.set $s (array.new $bytes (i32.const 0) (local.get $i)))
      (loop $write
         (local.set $i (i32.sub (local.get $i) (i32.const 1)))
         (array.set $bytes (local.get $s) (local.get $i)
            (array.get_u $chars (local.get $chars)
               (i32.wrap_i64 (i64.rem_u (local.get $d) (local.get $base)))))
         (local.set $d (i64.div_u (local.get $d) (local.get $base)))
         (br_if $write (i64.ne (local.get $d) (i64.const 0))))
      (if (local.get $negative)
         (then
            (array.set $bytes (local.get $s) (i32.const 0) (@char "-")))
         (else
            (if (local.get $sign_style)
               (then
                  (if (i32.eq (local.get $sign_style) (i32.const 1))
                     (then
                        (array.set $bytes (local.get $s) (i32.const 0)
                           (@char "+")))
                     (else
                        (array.set $bytes (local.get $s) (i32.const 0)
                           (@char " "))))))))
      (if (local.get $alternate)
         (then
            (if (local.get $i)
               (then
                  (array.set $bytes (local.get $s) (i32.const 0) (@char "0"))
                  (if (i64.eq (local.get $base) (i64.const 16))
                     (then
                        (array.set $bytes (local.get $s) (i32.const 1)
                           (select (@char "X") (@char "x")
                              (local.get $uppercase)))))))))
      (local.get $s))

)
