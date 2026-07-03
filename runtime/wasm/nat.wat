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

;; Legacy [num] library: the [nat] (natural number) primitives, a port of the
;; JavaScript runtime in runtime/js/nat.js. A nat is a custom block ("_nat")
;; backed by an array of 32-bit digits, least significant first; the digits
;; are unsigned. Carry-sensitive arithmetic is done in i64 to mirror the
;; JS implementation's use of doubles.

(module
   (import "marshal" "caml_serialize_int_4"
      (func $caml_serialize_int_4 (param (ref eq)) (param i32)))
   (import "marshal" "caml_deserialize_int_4"
      (func $caml_deserialize_int_4 (param (ref eq)) (result i32)))
   (import "hash" "caml_hash_mix_int"
      (func $caml_hash_mix_int (param i32) (param i32) (result i32)))
   (import "int32" "caml_copy_nativeint"
      (func $caml_copy_nativeint (param i32) (result (ref eq))))
   (import "int32" "Nativeint_val"
      (func $Nativeint_val (param (ref eq)) (result i32)))

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

   (type $digits (array (mut i32)))
   (type $nat
      (sub final $custom
         (struct (field (ref $custom_operations)) (field $dat (ref $digits)))))

   (global $nat_ops (export "nat_ops") (ref $custom_operations)
      (struct.new $custom_operations
         (@string "_nat")
         (ref.null $compare)
         (ref.null $compare)
         (ref.func $caml_hash_nat)
         (ref.null $fixed_length)
         (ref.func $serialize_nat)
         (ref.func $deserialize_nat)
         (ref.null $dup)))

   ;; Helpers

   (func $get_data (param $v (ref eq)) (result (ref $digits))
      (struct.get $nat $dat (ref.cast (ref $nat) (local.get $v))))

   (func $int (param $v (ref eq)) (result i32)
      (i31.get_s (ref.cast (ref i31) (local.get $v))))

   ;; A nat of [len] digits, all set to [fill].
   (func $alloc_nat (param $len i32) (param $fill i32) (result (ref $nat))
      (struct.new $nat (global.get $nat_ops)
         (array.new $digits (local.get $fill) (local.get $len))))

   ;; A one-digit nat holding [v] (the [nat_of_array [|v|]] of the JS runtime).
   (func $single (param $v i32) (result (ref $nat))
      (return_call $alloc_nat (i32.const 1) (local.get $v)))

   (func $num_digits
      (param $d (ref $digits)) (param $ofs i32) (param $len i32) (result i32)
      (local $i i32)
      (local.set $i (i32.sub (local.get $len) (i32.const 1)))
      (loop $loop
         (if (i32.ge_s (local.get $i) (i32.const 0))
            (then
               (if (i32.ne
                      (array.get $digits (local.get $d)
                         (i32.add (local.get $ofs) (local.get $i)))
                      (i32.const 0))
                  (then (return (i32.add (local.get $i) (i32.const 1)))))
               (local.set $i (i32.sub (local.get $i) (i32.const 1)))
               (br $loop))))
      (i32.const 1)) ;; 0 counts as 1 digit

   ;; Assuming c > a, returns the quotient and remainder of (a<<32 + b)/c.
   (func $div_helper
      (param $a i32) (param $b i32) (param $c i32) (result i32) (result i32)
      (local $x i64) (local $c64 i64) (local $y i64) (local $z i64) (local $w i64)
      (local.set $c64 (i64.extend_i32_u (local.get $c)))
      ;; x = a*65536 + (b >>> 16)
      (local.set $x
         (i64.add
            (i64.mul (i64.extend_i32_u (local.get $a)) (i64.const 65536))
            (i64.extend_i32_u (i32.shr_u (local.get $b) (i32.const 16)))))
      ;; y = floor(x/c)*65536
      (local.set $y
         (i64.mul (i64.div_u (local.get $x) (local.get $c64)) (i64.const 65536)))
      ;; z = (x%c)*65536
      (local.set $z
         (i64.mul (i64.rem_u (local.get $x) (local.get $c64)) (i64.const 65536)))
      ;; w = z + (b & 0xffff)
      (local.set $w
         (i64.add (local.get $z)
            (i64.extend_i32_u (i32.and (local.get $b) (i32.const 0xffff)))))
      ;; quotient, remainder
      (i32.wrap_i64 (i64.add (local.get $y) (i64.div_u (local.get $w) (local.get $c64))))
      (i32.wrap_i64 (i64.rem_u (local.get $w) (local.get $c64))))

   ;; Custom operations

   (func $caml_hash_nat (param $v (ref eq)) (result i32)
      (local $d (ref $digits)) (local $len i32) (local $h i32) (local $i i32)
      (local.set $d (call $get_data (local.get $v)))
      (local.set $len
         (call $num_digits (local.get $d) (i32.const 0)
            (array.len (local.get $d))))
      (loop $loop
         (if (i32.lt_s (local.get $i) (local.get $len))
            (then
               (local.set $h
                  (call $caml_hash_mix_int (local.get $h)
                     (array.get $digits (local.get $d) (local.get $i))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (local.get $h))

   (func $serialize_nat
      (param $s (ref eq)) (param $v (ref eq)) (result i32) (result i32)
      (local $d (ref $digits)) (local $len i32) (local $i i32)
      (local.set $d (call $get_data (local.get $v)))
      (local.set $len (array.len (local.get $d)))
      (call $caml_serialize_int_4 (local.get $s) (local.get $len))
      (loop $loop
         (if (i32.lt_s (local.get $i) (local.get $len))
            (then
               (call $caml_serialize_int_4 (local.get $s)
                  (array.get $digits (local.get $d) (local.get $i)))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (i32.mul (local.get $len) (i32.const 4))
      (i32.mul (local.get $len) (i32.const 8)))

   (func $deserialize_nat (param $s (ref eq)) (result (ref eq)) (result i32)
      (local $dat (ref $digits)) (local $len i32) (local $i i32)
      (local.set $len (call $caml_deserialize_int_4 (local.get $s)))
      (local.set $dat (array.new $digits (i32.const 0) (local.get $len)))
      (loop $loop
         (if (i32.lt_s (local.get $i) (local.get $len))
            (then
               (array.set $digits (local.get $dat) (local.get $i)
                  (call $caml_deserialize_int_4 (local.get $s)))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (struct.new $nat (global.get $nat_ops) (local.get $dat))
      (i32.mul (local.get $len) (i32.const 4)))

   ;; Primitives

   (func (export "initialize_nat") (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "create_nat") (param $size (ref eq)) (result (ref eq))
      (return_call $alloc_nat (call $int (local.get $size)) (i32.const -1)))

   (func (export "length_nat") (param $nat (ref eq)) (result (ref eq))
      (ref.i31 (array.len (call $get_data (local.get $nat)))))

   (func (export "set_to_zero_nat")
      (param $nat (ref eq)) (param $ofs (ref eq)) (param $len (ref eq))
      (result (ref eq))
      (local $d (ref $digits)) (local $o i32) (local $i i32) (local $l i32)
      (local.set $d (call $get_data (local.get $nat)))
      (local.set $o (call $int (local.get $ofs)))
      (local.set $l (call $int (local.get $len)))
      (loop $loop
         (if (i32.lt_s (local.get $i) (local.get $l))
            (then
               (array.set $digits (local.get $d)
                  (i32.add (local.get $o) (local.get $i)) (i32.const 0))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (ref.i31 (i32.const 0)))

   (func (export "blit_nat")
      (param $nat1 (ref eq)) (param $ofs1 (ref eq))
      (param $nat2 (ref eq)) (param $ofs2 (ref eq)) (param $len (ref eq))
      (result (ref eq))
      (array.copy $digits $digits
         (call $get_data (local.get $nat1)) (call $int (local.get $ofs1))
         (call $get_data (local.get $nat2)) (call $int (local.get $ofs2))
         (call $int (local.get $len)))
      (ref.i31 (i32.const 0)))

   (func (export "set_digit_nat")
      (param $nat (ref eq)) (param $ofs (ref eq)) (param $digit (ref eq))
      (result (ref eq))
      (array.set $digits (call $get_data (local.get $nat))
         (call $int (local.get $ofs)) (call $int (local.get $digit)))
      (ref.i31 (i32.const 0)))

   (func (export "nth_digit_nat")
      (param $nat (ref eq)) (param $ofs (ref eq)) (result (ref eq))
      (ref.i31
         (array.get $digits (call $get_data (local.get $nat))
            (call $int (local.get $ofs)))))

   (func (export "set_digit_nat_native")
      (param $nat (ref eq)) (param $ofs (ref eq)) (param $digit (ref eq))
      (result (ref eq))
      (array.set $digits (call $get_data (local.get $nat))
         (call $int (local.get $ofs)) (call $Nativeint_val (local.get $digit)))
      (ref.i31 (i32.const 0)))

   (func (export "nth_digit_nat_native")
      (param $nat (ref eq)) (param $ofs (ref eq)) (result (ref eq))
      (return_call $caml_copy_nativeint
         (array.get $digits (call $get_data (local.get $nat))
            (call $int (local.get $ofs)))))

   (func (export "num_digits_nat")
      (param $nat (ref eq)) (param $ofs (ref eq)) (param $len (ref eq))
      (result (ref eq))
      (ref.i31
         (call $num_digits (call $get_data (local.get $nat))
            (call $int (local.get $ofs)) (call $int (local.get $len)))))

   (func (export "num_leading_zero_bits_in_digit")
      (param $nat (ref eq)) (param $ofs (ref eq)) (result (ref eq))
      (ref.i31
         (i32.clz
            (array.get $digits (call $get_data (local.get $nat))
               (call $int (local.get $ofs))))))

   (func (export "is_digit_int")
      (param $nat (ref eq)) (param $ofs (ref eq)) (result (ref eq))
      ;; Does the digit fit in a positive OCaml int? An int on wasm is 31 bits
      ;; (like the native 32-bit platform that the num library targets), so the
      ;; digit must have its top two bits clear, i.e. be < 2^30. (The JS runtime
      ;; only clears the top bit because there an int is 32 bits.)
      (ref.i31
         (i32.lt_u
            (array.get $digits (call $get_data (local.get $nat))
               (call $int (local.get $ofs)))
            (i32.const 0x40000000))))

   (func (export "is_digit_zero")
      (param $nat (ref eq)) (param $ofs (ref eq)) (result (ref eq))
      (ref.i31
         (i32.eqz
            (array.get $digits (call $get_data (local.get $nat))
               (call $int (local.get $ofs))))))

   (func (export "is_digit_normalized")
      (param $nat (ref eq)) (param $ofs (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 1)))

   (func (export "is_digit_odd")
      (param $nat (ref eq)) (param $ofs (ref eq)) (result (ref eq))
      (ref.i31
         (i32.and
            (array.get $digits (call $get_data (local.get $nat))
               (call $int (local.get $ofs)))
            (i32.const 1))))

   (func $incr (param $d (ref $digits)) (param $ofs i32) (param $len i32)
      (param $carry i32) (result i32)
      (local $i i32) (local $x i64) (local $idx i32)
      (loop $loop
         (if (i32.lt_s (local.get $i) (local.get $len))
            (then
               (local.set $idx (i32.add (local.get $ofs) (local.get $i)))
               (local.set $x
                  (i64.add
                     (i64.extend_i32_u (array.get $digits (local.get $d) (local.get $idx)))
                     (i64.extend_i32_u (local.get $carry))))
               (array.set $digits (local.get $d) (local.get $idx)
                  (i32.wrap_i64 (local.get $x)))
               (local.set $carry
                  (i32.wrap_i64 (i64.shr_u (local.get $x) (i64.const 32))))
               (br_if $loop
                  (i32.and (i32.ne (local.get $carry) (i32.const 0))
                     (i32.lt_s
                        (local.tee $i (i32.add (local.get $i) (i32.const 1)))
                        (local.get $len)))))))
      (local.get $carry))

   (func (export "incr_nat")
      (param $nat (ref eq)) (param $ofs (ref eq)) (param $len (ref eq))
      (param $carry (ref eq)) (result (ref eq))
      (ref.i31
         (call $incr (call $get_data (local.get $nat)) (call $int (local.get $ofs))
            (call $int (local.get $len)) (call $int (local.get $carry)))))

   (func $add
      (param $d1 (ref $digits)) (param $o1 i32) (param $l1 i32)
      (param $d2 (ref $digits)) (param $o2 i32) (param $l2 i32) (param $carry i32)
      (result i32)
      (local $i i32) (local $x i64)
      (loop $loop
         (if (i32.lt_s (local.get $i) (local.get $l2))
            (then
               (local.set $x
                  (i64.add
                     (i64.add
                        (i64.extend_i32_u
                           (array.get $digits (local.get $d1)
                              (i32.add (local.get $o1) (local.get $i))))
                        (i64.extend_i32_u
                           (array.get $digits (local.get $d2)
                              (i32.add (local.get $o2) (local.get $i)))))
                     (i64.extend_i32_u (local.get $carry))))
               (array.set $digits (local.get $d1)
                  (i32.add (local.get $o1) (local.get $i))
                  (i32.wrap_i64 (local.get $x)))
               (local.set $carry
                  (i32.wrap_i64 (i64.shr_u (local.get $x) (i64.const 32))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (return_call $incr (local.get $d1) (i32.add (local.get $o1) (local.get $l2))
         (i32.sub (local.get $l1) (local.get $l2)) (local.get $carry)))

   (func (export "add_nat")
      (param $nat1 (ref eq)) (param $ofs1 (ref eq)) (param $len1 (ref eq))
      (param $nat2 (ref eq)) (param $ofs2 (ref eq)) (param $len2 (ref eq))
      (param $carry (ref eq)) (result (ref eq))
      (ref.i31
         (call $add (call $get_data (local.get $nat1)) (call $int (local.get $ofs1))
            (call $int (local.get $len1)) (call $get_data (local.get $nat2))
            (call $int (local.get $ofs2)) (call $int (local.get $len2))
            (call $int (local.get $carry)))))

   (func (export "complement_nat")
      (param $nat (ref eq)) (param $ofs (ref eq)) (param $len (ref eq))
      (result (ref eq))
      (local $d (ref $digits)) (local $o i32) (local $l i32) (local $i i32)
      (local $idx i32)
      (local.set $d (call $get_data (local.get $nat)))
      (local.set $o (call $int (local.get $ofs)))
      (local.set $l (call $int (local.get $len)))
      (loop $loop
         (if (i32.lt_s (local.get $i) (local.get $l))
            (then
               (local.set $idx (i32.add (local.get $o) (local.get $i)))
               (array.set $digits (local.get $d) (local.get $idx)
                  (i32.sub (i32.const -1)
                     (array.get $digits (local.get $d) (local.get $idx))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (ref.i31 (i32.const 0)))

   ;; [carry_in] follows the OCaml convention (1 means no incoming borrow).
   (func $decr (param $d (ref $digits)) (param $ofs i32) (param $len i32)
      (param $carry_in i32) (result i32)
      (local $i i32) (local $borrow i32) (local $x i64) (local $idx i32)
      (local.set $borrow (i32.eqz (local.get $carry_in)))
      (loop $loop
         (if (i32.lt_s (local.get $i) (local.get $len))
            (then
               (local.set $idx (i32.add (local.get $ofs) (local.get $i)))
               (local.set $x
                  (i64.sub
                     (i64.extend_i32_u (array.get $digits (local.get $d) (local.get $idx)))
                     (i64.extend_i32_u (local.get $borrow))))
               (array.set $digits (local.get $d) (local.get $idx)
                  (i32.wrap_i64 (local.get $x)))
               (local.set $borrow (i64.lt_s (local.get $x) (i64.const 0)))
               (br_if $loop
                  (i32.and (local.get $borrow)
                     (i32.lt_s
                        (local.tee $i (i32.add (local.get $i) (i32.const 1)))
                        (local.get $len)))))))
      (i32.eqz (local.get $borrow)))

   (func (export "decr_nat")
      (param $nat (ref eq)) (param $ofs (ref eq)) (param $len (ref eq))
      (param $carry (ref eq)) (result (ref eq))
      (ref.i31
         (call $decr (call $get_data (local.get $nat)) (call $int (local.get $ofs))
            (call $int (local.get $len)) (call $int (local.get $carry)))))

   (func $sub
      (param $d1 (ref $digits)) (param $o1 i32) (param $l1 i32)
      (param $d2 (ref $digits)) (param $o2 i32) (param $l2 i32) (param $carry_in i32)
      (result i32)
      (local $i i32) (local $borrow i32) (local $x i64)
      (local.set $borrow (i32.eqz (local.get $carry_in)))
      (loop $loop
         (if (i32.lt_s (local.get $i) (local.get $l2))
            (then
               (local.set $x
                  (i64.sub
                     (i64.sub
                        (i64.extend_i32_u
                           (array.get $digits (local.get $d1)
                              (i32.add (local.get $o1) (local.get $i))))
                        (i64.extend_i32_u
                           (array.get $digits (local.get $d2)
                              (i32.add (local.get $o2) (local.get $i)))))
                     (i64.extend_i32_u (local.get $borrow))))
               (array.set $digits (local.get $d1)
                  (i32.add (local.get $o1) (local.get $i))
                  (i32.wrap_i64 (local.get $x)))
               (local.set $borrow (i64.lt_s (local.get $x) (i64.const 0)))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (return_call $decr (local.get $d1) (i32.add (local.get $o1) (local.get $l2))
         (i32.sub (local.get $l1) (local.get $l2)) (i32.eqz (local.get $borrow))))

   (func (export "sub_nat")
      (param $nat1 (ref eq)) (param $ofs1 (ref eq)) (param $len1 (ref eq))
      (param $nat2 (ref eq)) (param $ofs2 (ref eq)) (param $len2 (ref eq))
      (param $carry (ref eq)) (result (ref eq))
      (ref.i31
         (call $sub (call $get_data (local.get $nat1)) (call $int (local.get $ofs1))
            (call $int (local.get $len1)) (call $get_data (local.get $nat2))
            (call $int (local.get $ofs2)) (call $int (local.get $len2))
            (call $int (local.get $carry)))))

   (func $mult_digit
      (param $d1 (ref $digits)) (param $o1 i32) (param $l1 i32)
      (param $d2 (ref $digits)) (param $o2 i32) (param $l2 i32)
      (param $d3 (ref $digits)) (param $o3 i32) (result i32)
      (local $i i32) (local $a i32) (local $b i64) (local $carry i64)
      (local $x1 i64) (local $x2 i64) (local $x3 i64)
      (local.set $a (array.get $digits (local.get $d3) (local.get $o3)))
      (loop $loop
         (if (i32.lt_s (local.get $i) (local.get $l2))
            (then
               (local.set $b
                  (i64.extend_i32_u
                     (array.get $digits (local.get $d2)
                        (i32.add (local.get $o2) (local.get $i)))))
               ;; x1 = d1[o1+i] + b*(a & 0xffff) + carry
               (local.set $x1
                  (i64.add
                     (i64.add
                        (i64.extend_i32_u
                           (array.get $digits (local.get $d1)
                              (i32.add (local.get $o1) (local.get $i))))
                        (i64.mul (local.get $b)
                           (i64.extend_i32_u (i32.and (local.get $a) (i32.const 0xffff)))))
                     (local.get $carry)))
               ;; x2 = b*(a >>> 16)
               (local.set $x2
                  (i64.mul (local.get $b)
                     (i64.extend_i32_u (i32.shr_u (local.get $a) (i32.const 16)))))
               (local.set $carry (i64.shr_u (local.get $x2) (i64.const 16)))
               ;; x3 = x1 + (x2 % 65536) * 65536
               (local.set $x3
                  (i64.add (local.get $x1)
                     (i64.shl (i64.and (local.get $x2) (i64.const 0xffff))
                        (i64.const 16))))
               (array.set $digits (local.get $d1)
                  (i32.add (local.get $o1) (local.get $i))
                  (i32.wrap_i64 (local.get $x3)))
               (local.set $carry
                  (i64.add (local.get $carry)
                     (i64.shr_u (local.get $x3) (i64.const 32))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (if (i32.and (i32.lt_s (local.get $l2) (local.get $l1))
             (i64.ne (local.get $carry) (i64.const 0)))
         (then
            (return_call $add (local.get $d1)
               (i32.add (local.get $o1) (local.get $l2))
               (i32.sub (local.get $l1) (local.get $l2))
               (struct.get $nat $dat (call $single (i32.wrap_i64 (local.get $carry))))
               (i32.const 0) (i32.const 1) (i32.const 0))))
      (i32.wrap_i64 (local.get $carry)))

   (func (export "mult_digit_nat")
      (param $nat1 (ref eq)) (param $ofs1 (ref eq)) (param $len1 (ref eq))
      (param $nat2 (ref eq)) (param $ofs2 (ref eq)) (param $len2 (ref eq))
      (param $nat3 (ref eq)) (param $ofs3 (ref eq)) (result (ref eq))
      (ref.i31
         (call $mult_digit
            (call $get_data (local.get $nat1)) (call $int (local.get $ofs1))
            (call $int (local.get $len1)) (call $get_data (local.get $nat2))
            (call $int (local.get $ofs2)) (call $int (local.get $len2))
            (call $get_data (local.get $nat3)) (call $int (local.get $ofs3)))))

   (func $mult
      (param $d1 (ref $digits)) (param $o1 i32) (param $l1 i32)
      (param $d2 (ref $digits)) (param $o2 i32) (param $l2 i32)
      (param $d3 (ref $digits)) (param $o3 i32) (param $l3 i32) (result i32)
      (local $i i32) (local $carry i32)
      (loop $loop
         (if (i32.lt_s (local.get $i) (local.get $l3))
            (then
               (local.set $carry
                  (i32.add (local.get $carry)
                     (call $mult_digit
                        (local.get $d1) (i32.add (local.get $o1) (local.get $i))
                        (i32.sub (local.get $l1) (local.get $i))
                        (local.get $d2) (local.get $o2) (local.get $l2)
                        (local.get $d3) (i32.add (local.get $o3) (local.get $i)))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (local.get $carry))

   (func (export "mult_nat")
      (param $nat1 (ref eq)) (param $ofs1 (ref eq)) (param $len1 (ref eq))
      (param $nat2 (ref eq)) (param $ofs2 (ref eq)) (param $len2 (ref eq))
      (param $nat3 (ref eq)) (param $ofs3 (ref eq)) (param $len3 (ref eq))
      (result (ref eq))
      (ref.i31
         (call $mult
            (call $get_data (local.get $nat1)) (call $int (local.get $ofs1))
            (call $int (local.get $len1)) (call $get_data (local.get $nat2))
            (call $int (local.get $ofs2)) (call $int (local.get $len2))
            (call $get_data (local.get $nat3)) (call $int (local.get $ofs3))
            (call $int (local.get $len3)))))

   (func (export "square_nat")
      (param $nat1 (ref eq)) (param $ofs1 (ref eq)) (param $len1 (ref eq))
      (param $nat2 (ref eq)) (param $ofs2 (ref eq)) (param $len2 (ref eq))
      (result (ref eq))
      (local $d1 (ref $digits)) (local $o1 i32) (local $l1 i32)
      (local $d2 (ref $digits)) (local $o2 i32) (local $l2 i32) (local $carry i32)
      (local.set $d1 (call $get_data (local.get $nat1)))
      (local.set $o1 (call $int (local.get $ofs1)))
      (local.set $l1 (call $int (local.get $len1)))
      (local.set $d2 (call $get_data (local.get $nat2)))
      (local.set $o2 (call $int (local.get $ofs2)))
      (local.set $l2 (call $int (local.get $len2)))
      (local.set $carry
         (call $add (local.get $d1) (local.get $o1) (local.get $l1)
            (local.get $d1) (local.get $o1) (local.get $l1) (i32.const 0)))
      (local.set $carry
         (i32.add (local.get $carry)
            (call $mult (local.get $d1) (local.get $o1) (local.get $l1)
               (local.get $d2) (local.get $o2) (local.get $l2)
               (local.get $d2) (local.get $o2) (local.get $l2))))
      (ref.i31 (local.get $carry)))

   (func $shift_left
      (param $d1 (ref $digits)) (param $o1 i32) (param $l1 i32)
      (param $d2 (ref $digits)) (param $o2 i32) (param $nbits i32)
      (local $i i32) (local $a i32) (local $wrap i32) (local $idx i32)
      (if (i32.eqz (local.get $nbits))
         (then
            (array.set $digits (local.get $d2) (local.get $o2) (i32.const 0))
            (return)))
      (loop $loop
         (if (i32.lt_s (local.get $i) (local.get $l1))
            (then
               (local.set $idx (i32.add (local.get $o1) (local.get $i)))
               (local.set $a (array.get $digits (local.get $d1) (local.get $idx)))
               (array.set $digits (local.get $d1) (local.get $idx)
                  (i32.or (i32.shl (local.get $a) (local.get $nbits))
                     (local.get $wrap)))
               (local.set $wrap
                  (i32.shr_u (local.get $a) (i32.sub (i32.const 32) (local.get $nbits))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (array.set $digits (local.get $d2) (local.get $o2) (local.get $wrap)))

   (func (export "shift_left_nat")
      (param $nat1 (ref eq)) (param $ofs1 (ref eq)) (param $len1 (ref eq))
      (param $nat2 (ref eq)) (param $ofs2 (ref eq)) (param $nbits (ref eq))
      (result (ref eq))
      (call $shift_left (call $get_data (local.get $nat1)) (call $int (local.get $ofs1))
         (call $int (local.get $len1)) (call $get_data (local.get $nat2))
         (call $int (local.get $ofs2)) (call $int (local.get $nbits)))
      (ref.i31 (i32.const 0)))

   (func $shift_right
      (param $d1 (ref $digits)) (param $o1 i32) (param $l1 i32)
      (param $d2 (ref $digits)) (param $o2 i32) (param $nbits i32)
      (local $i i32) (local $a i32) (local $wrap i32) (local $idx i32)
      (if (i32.eqz (local.get $nbits))
         (then
            (array.set $digits (local.get $d2) (local.get $o2) (i32.const 0))
            (return)))
      (local.set $i (i32.sub (local.get $l1) (i32.const 1)))
      (loop $loop
         (if (i32.ge_s (local.get $i) (i32.const 0))
            (then
               (local.set $idx (i32.add (local.get $o1) (local.get $i)))
               (local.set $a (array.get $digits (local.get $d1) (local.get $idx)))
               (array.set $digits (local.get $d1) (local.get $idx)
                  (i32.or (i32.shr_u (local.get $a) (local.get $nbits))
                     (local.get $wrap)))
               (local.set $wrap
                  (i32.shl (local.get $a) (i32.sub (i32.const 32) (local.get $nbits))))
               (local.set $i (i32.sub (local.get $i) (i32.const 1)))
               (br $loop))))
      (array.set $digits (local.get $d2) (local.get $o2) (local.get $wrap)))

   (func (export "shift_right_nat")
      (param $nat1 (ref eq)) (param $ofs1 (ref eq)) (param $len1 (ref eq))
      (param $nat2 (ref eq)) (param $ofs2 (ref eq)) (param $nbits (ref eq))
      (result (ref eq))
      (call $shift_right (call $get_data (local.get $nat1)) (call $int (local.get $ofs1))
         (call $int (local.get $len1)) (call $get_data (local.get $nat2))
         (call $int (local.get $ofs2)) (call $int (local.get $nbits)))
      (ref.i31 (i32.const 0)))

   (func $compare
      (param $d1 (ref $digits)) (param $o1 i32) (param $l1 i32)
      (param $d2 (ref $digits)) (param $o2 i32) (param $l2 i32) (result i32)
      (local $a i32) (local $b i32) (local $i i32) (local $x i32) (local $y i32)
      (local.set $a (call $num_digits (local.get $d1) (local.get $o1) (local.get $l1)))
      (local.set $b (call $num_digits (local.get $d2) (local.get $o2) (local.get $l2)))
      (if (i32.gt_s (local.get $a) (local.get $b)) (then (return (i32.const 1))))
      (if (i32.lt_s (local.get $a) (local.get $b)) (then (return (i32.const -1))))
      (local.set $i (i32.sub (local.get $a) (i32.const 1)))
      (loop $loop
         (if (i32.ge_s (local.get $i) (i32.const 0))
            (then
               (local.set $x
                  (array.get $digits (local.get $d1) (i32.add (local.get $o1) (local.get $i))))
               (local.set $y
                  (array.get $digits (local.get $d2) (i32.add (local.get $o2) (local.get $i))))
               (if (i32.gt_u (local.get $x) (local.get $y)) (then (return (i32.const 1))))
               (if (i32.lt_u (local.get $x) (local.get $y)) (then (return (i32.const -1))))
               (local.set $i (i32.sub (local.get $i) (i32.const 1)))
               (br $loop))))
      (i32.const 0))

   (func (export "compare_nat")
      (param $nat1 (ref eq)) (param $ofs1 (ref eq)) (param $len1 (ref eq))
      (param $nat2 (ref eq)) (param $ofs2 (ref eq)) (param $len2 (ref eq))
      (result (ref eq))
      (ref.i31
         (call $compare
            (call $get_data (local.get $nat1)) (call $int (local.get $ofs1))
            (call $int (local.get $len1)) (call $get_data (local.get $nat2))
            (call $int (local.get $ofs2)) (call $int (local.get $len2)))))

   (func (export "compare_digits_nat")
      (param $nat1 (ref eq)) (param $ofs1 (ref eq))
      (param $nat2 (ref eq)) (param $ofs2 (ref eq)) (result (ref eq))
      (local $x i32) (local $y i32)
      (local.set $x
         (array.get $digits (call $get_data (local.get $nat1)) (call $int (local.get $ofs1))))
      (local.set $y
         (array.get $digits (call $get_data (local.get $nat2)) (call $int (local.get $ofs2))))
      ;; signed comparison, matching the JS runtime
      (ref.i31
         (i32.sub (i32.gt_s (local.get $x) (local.get $y))
                  (i32.lt_s (local.get $x) (local.get $y)))))

   (func (export "land_digit_nat")
      (param $nat1 (ref eq)) (param $ofs1 (ref eq))
      (param $nat2 (ref eq)) (param $ofs2 (ref eq)) (result (ref eq))
      (local $d1 (ref $digits)) (local $o1 i32)
      (local.set $d1 (call $get_data (local.get $nat1)))
      (local.set $o1 (call $int (local.get $ofs1)))
      (array.set $digits (local.get $d1) (local.get $o1)
         (i32.and (array.get $digits (local.get $d1) (local.get $o1))
            (array.get $digits (call $get_data (local.get $nat2))
               (call $int (local.get $ofs2)))))
      (ref.i31 (i32.const 0)))

   (func (export "lor_digit_nat")
      (param $nat1 (ref eq)) (param $ofs1 (ref eq))
      (param $nat2 (ref eq)) (param $ofs2 (ref eq)) (result (ref eq))
      (local $d1 (ref $digits)) (local $o1 i32)
      (local.set $d1 (call $get_data (local.get $nat1)))
      (local.set $o1 (call $int (local.get $ofs1)))
      (array.set $digits (local.get $d1) (local.get $o1)
         (i32.or (array.get $digits (local.get $d1) (local.get $o1))
            (array.get $digits (call $get_data (local.get $nat2))
               (call $int (local.get $ofs2)))))
      (ref.i31 (i32.const 0)))

   (func (export "lxor_digit_nat")
      (param $nat1 (ref eq)) (param $ofs1 (ref eq))
      (param $nat2 (ref eq)) (param $ofs2 (ref eq)) (result (ref eq))
      (local $d1 (ref $digits)) (local $o1 i32)
      (local.set $d1 (call $get_data (local.get $nat1)))
      (local.set $o1 (call $int (local.get $ofs1)))
      (array.set $digits (local.get $d1) (local.get $o1)
         (i32.xor (array.get $digits (local.get $d1) (local.get $o1))
            (array.get $digits (call $get_data (local.get $nat2))
               (call $int (local.get $ofs2)))))
      (ref.i31 (i32.const 0)))

   ;; natq[ofsq..] := natq / nat2[ofs2], natr[ofsr] := remainder
   (func $div_digit
      (param $dq (ref $digits)) (param $oq i32)
      (param $dr (ref $digits)) (param $or i32)
      (param $d1 (ref $digits)) (param $o1 i32) (param $len i32)
      (param $d2 (ref $digits)) (param $o2 i32)
      (local $i i32) (local $rem i32) (local $div i32) (local $q i32)
      (local.set $rem
         (array.get $digits (local.get $d1)
            (i32.sub (i32.add (local.get $o1) (local.get $len)) (i32.const 1))))
      (local.set $div (array.get $digits (local.get $d2) (local.get $o2)))
      (local.set $i (i32.sub (local.get $len) (i32.const 2)))
      (loop $loop
         (if (i32.ge_s (local.get $i) (i32.const 0))
            (then
               (call $div_helper (local.get $rem)
                  (array.get $digits (local.get $d1) (i32.add (local.get $o1) (local.get $i)))
                  (local.get $div))
               (local.set $rem)
               (local.set $q)
               (array.set $digits (local.get $dq)
                  (i32.add (local.get $oq) (local.get $i)) (local.get $q))
               (local.set $i (i32.sub (local.get $i) (i32.const 1)))
               (br $loop))))
      (array.set $digits (local.get $dr) (local.get $or) (local.get $rem)))

   (func (export "div_digit_nat")
      (param $natq (ref eq)) (param $ofsq (ref eq))
      (param $natr (ref eq)) (param $ofsr (ref eq))
      (param $nat1 (ref eq)) (param $ofs1 (ref eq)) (param $len (ref eq))
      (param $nat2 (ref eq)) (param $ofs2 (ref eq)) (result (ref eq))
      (call $div_digit
         (call $get_data (local.get $natq)) (call $int (local.get $ofsq))
         (call $get_data (local.get $natr)) (call $int (local.get $ofsr))
         (call $get_data (local.get $nat1)) (call $int (local.get $ofs1))
         (call $int (local.get $len))
         (call $get_data (local.get $nat2)) (call $int (local.get $ofs2)))
      (ref.i31 (i32.const 0)))

   (func (export "div_nat")
      (param $nat1 (ref eq)) (param $ofs1 (ref eq)) (param $len1 (ref eq))
      (param $nat2 (ref eq)) (param $ofs2 (ref eq)) (param $len2 (ref eq))
      (result (ref eq))
      (local $d1 (ref $digits)) (local $o1 i32) (local $l1 i32)
      (local $d2 (ref $digits)) (local $o2 i32) (local $l2 i32)
      (local $s i32) (local $d i64) (local $a (ref $digits)) (local $i i32)
      (local $quo i32) (local $zero (ref $digits))
      (local.set $d1 (call $get_data (local.get $nat1)))
      (local.set $o1 (call $int (local.get $ofs1)))
      (local.set $l1 (call $int (local.get $len1)))
      (local.set $d2 (call $get_data (local.get $nat2)))
      (local.set $o2 (call $int (local.get $ofs2)))
      (local.set $l2 (call $int (local.get $len2)))
      (if (i32.eq (local.get $l2) (i32.const 1))
         (then
            (call $div_digit
               (local.get $d1) (i32.add (local.get $o1) (i32.const 1))
               (local.get $d1) (local.get $o1)
               (local.get $d1) (local.get $o1) (local.get $l1)
               (local.get $d2) (local.get $o2))
            (return (ref.i31 (i32.const 0)))))
      (local.set $zero (array.new $digits (i32.const 0) (i32.const 1)))
      (local.set $s
         (i32.clz
            (array.get $digits (local.get $d2)
               (i32.sub (i32.add (local.get $o2) (local.get $l2)) (i32.const 1)))))
      (call $shift_left (local.get $d2) (local.get $o2) (local.get $l2)
         (local.get $zero) (i32.const 0) (local.get $s))
      (call $shift_left (local.get $d1) (local.get $o1) (local.get $l1)
         (local.get $zero) (i32.const 0) (local.get $s))
      (local.set $d
         (i64.add
            (i64.extend_i32_u
               (array.get $digits (local.get $d2)
                  (i32.sub (i32.add (local.get $o2) (local.get $l2)) (i32.const 1))))
            (i64.const 1)))
      (local.set $a (array.new $digits (i32.const -1) (i32.add (local.get $l2) (i32.const 1))))
      (local.set $i (i32.sub (local.get $l1) (i32.const 1)))
      (loop $loop
         (if (i32.ge_s (local.get $i) (local.get $l2))
            (then
               ;; lower bound on the quotient digit
               (if (i64.eq (local.get $d) (i64.const 0x100000000))
                  (then
                     (local.set $quo
                        (array.get $digits (local.get $d1)
                           (i32.add (local.get $o1) (local.get $i)))))
                  (else
                     ;; div_helper returns (quotient, remainder); keep quotient
                     (call $div_helper
                        (array.get $digits (local.get $d1)
                           (i32.add (local.get $o1) (local.get $i)))
                        (array.get $digits (local.get $d1)
                           (i32.sub (i32.add (local.get $o1) (local.get $i)) (i32.const 1)))
                        (i32.wrap_i64 (local.get $d)))
                     (drop)
                     (local.set $quo)))
               ;; a := nat2 * quo
               (call $set_zero (local.get $a) (i32.const 0)
                  (i32.add (local.get $l2) (i32.const 1)))
               (drop (call $mult_digit (local.get $a) (i32.const 0)
                  (i32.add (local.get $l2) (i32.const 1))
                  (local.get $d2) (local.get $o2) (local.get $l2)
                  (struct.get $nat $dat (call $single (local.get $quo)))
                  (i32.const 0)))
               (drop (call $sub (local.get $d1)
                  (i32.sub (i32.add (local.get $o1) (local.get $i)) (local.get $l2))
                  (i32.add (local.get $l2) (i32.const 1))
                  (local.get $a) (i32.const 0) (i32.add (local.get $l2) (i32.const 1))
                  (i32.const 1)))
               (loop $correct
                  (if (i32.or
                         (i32.ne
                            (array.get $digits (local.get $d1)
                               (i32.add (local.get $o1) (local.get $i)))
                            (i32.const 0))
                         (i32.ge_s
                            (call $compare (local.get $d1)
                               (i32.sub (i32.add (local.get $o1) (local.get $i)) (local.get $l2))
                               (local.get $l2)
                               (local.get $d2) (local.get $o2) (local.get $l2))
                            (i32.const 0)))
                     (then
                        (local.set $quo (i32.add (local.get $quo) (i32.const 1)))
                        (drop (call $sub (local.get $d1)
                           (i32.sub (i32.add (local.get $o1) (local.get $i)) (local.get $l2))
                           (i32.add (local.get $l2) (i32.const 1))
                           (local.get $d2) (local.get $o2) (local.get $l2)
                           (i32.const 1)))
                        (br $correct))))
               (array.set $digits (local.get $d1)
                  (i32.add (local.get $o1) (local.get $i)) (local.get $quo))
               (local.set $i (i32.sub (local.get $i) (i32.const 1)))
               (br $loop))))
      (call $shift_right (local.get $d1) (local.get $o1) (local.get $l2)
         (local.get $zero) (i32.const 0) (local.get $s))
      (call $shift_right (local.get $d2) (local.get $o2) (local.get $l2)
         (local.get $zero) (i32.const 0) (local.get $s))
      (ref.i31 (i32.const 0)))

   ;; internal set-to-zero on a raw digit array
   (func $set_zero (param $d (ref $digits)) (param $ofs i32) (param $len i32)
      (local $i i32)
      (loop $loop
         (if (i32.lt_s (local.get $i) (local.get $len))
            (then
               (array.set $digits (local.get $d)
                  (i32.add (local.get $ofs) (local.get $i)) (i32.const 0))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop)))))
)
