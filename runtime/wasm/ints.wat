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
   (import "fail" "caml_invalid_argument"
      (func $caml_invalid_argument (param (ref eq))))
   (import "string" "caml_string_of_bytes"
      (func $caml_string_of_bytes (param (ref eq)) (result (ref eq))))

(@if use-js-string
(@then
   (import "wasm:js-string" "length"
      (func $string_length (param externref) (result i32)))
   (import "wasm:js-string" "charCodeAt"
      (func $string_get (param externref i32) (result i32)))

   (func $string_val (param $s (ref eq)) (result externref)
      (extern.convert_any
         (struct.get $string 0 (ref.cast (ref $string) (local.get $s)))))
)
(@else
   (func $string_length (param $s (ref $bytes)) (result i32)
      (array.len (local.get $s)))
   (func $string_get (param $s (ref $bytes)) (param $i i32) (result i32)
      (array.get $bytes (local.get $s) (local.get $i)))
   (func $string_val (param $s (ref eq)) (result (ref $bytes))
      (ref.cast (ref $bytes) (local.get $s)))
))

   (type $bytes (array (mut i8)))
   (type $string (struct (field anyref)))

   (func (export "caml_format_int")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (return_call $format_int
         (local.get 0)
         (i31.get_s (ref.cast (ref i31) (local.get 1))) (i32.const 1)))

   (func $parse_sign_and_base (export "parse_sign_and_base")
(@if use-js-string
(@then
      (param $s externref)
)
(@else
      (param $s (ref $bytes))
))
      (result i32 i32 i32 i32)
      (local $i i32) (local $len i32) (local $c i32)
      (local $signedness i32) (local $sign i32) (local $base i32)
      (local.set $i (i32.const 0))
      (local.set $len (call $string_length (local.get $s)))
      (local.set $signedness (i32.const 1))
      (local.set $sign (i32.const 1))
      (local.set $base (i32.const 10))
      (if (i32.ne (local.get $len) (i32.const 0))
         (then
            (local.set $c (call $string_get (local.get $s) (i32.const 0)))
            (if (i32.eq (local.get $c) (@char "-"))
               (then
                  (local.set $sign (i32.const -1))
                  (local.set $i (i32.const 1)))
               (else (if (i32.eq (local.get $c) (@char "+"))
                  (then (local.set $i (i32.const 1))))))))
      (if (i32.lt_s (i32.add (local.get $i) (i32.const 1)) (local.get $len))
         (then (if (i32.eq (call $string_get (local.get $s) (local.get $i))
                           (i32.const 48))
            (then
               (local.set $c
                  (call $string_get (local.get $s)
                     (i32.add (local.get $i) (i32.const 1))))
               (if (i32.or (i32.eq (local.get $c) (@char "X"))
                           (i32.eq (local.get $c) (@char "x")))
                  (then
                     (local.set $base (i32.const 16))
                     (local.set $signedness (i32.const 0))
                     (local.set $i (i32.add (local.get $i) (i32.const 2))))
               (else (if (i32.or (i32.eq (local.get $c) (@char "O"))
                                 (i32.eq (local.get $c) (@char "o")))
                  (then
                     (local.set $base (i32.const 8))
                     (local.set $signedness (i32.const 0))
                     (local.set $i (i32.add (local.get $i) (i32.const 2))))
               (else (if (i32.or (i32.eq (local.get $c) (@char "B"))
                                 (i32.eq (local.get $c) (@char "b")))
                  (then
                     (local.set $base (i32.const 2))
                     (local.set $signedness (i32.const 0))
                     (local.set $i (i32.add (local.get $i) (i32.const 2))))
               (else (if (i32.or (i32.eq (local.get $c) (@char "U"))
                                 (i32.eq (local.get $c) (@char "u")))
                  (then
                     (local.set $signedness (i32.const 0))
                     (local.set $i (i32.add (local.get $i)
                        (i32.const 2)))))))))))))))
      (tuple.make 4
         (local.get $i) (local.get $signedness) (local.get $sign)
         (local.get $base)))

   (func $parse_digit (export "parse_digit") (param $c i32) (result i32)
      (if (i32.and (i32.ge_u (local.get $c) (@char "0"))
                   (i32.le_u (local.get $c) (@char "9")))
         (then (return (i32.sub (local.get $c) (@char "0")))))
      (if (i32.and (i32.ge_u (local.get $c) (@char "A"))
                   (i32.le_u (local.get $c) (@char "Z")))
         (then (return (i32.sub (local.get $c) (i32.const 55)))))
      (if (i32.and (i32.ge_u (local.get $c) (@char "a"))
                   (i32.le_u (local.get $c) (@char "z")))
         (then (return (i32.sub (local.get $c) (i32.const 87)))))
      (return (i32.const -1)))

   (func $parse_int (export "parse_int")
      (param $v (ref eq)) (param $nbits i32) (param $errmsg (ref eq))
      (result i32)
(@if use-js-string
(@then
      (local $s externref)
)
(@else
      (local $s (ref $bytes))
))
      (local $i i32) (local $len i32) (local $d i32) (local $c i32)
      (local $signedness i32) (local $sign i32) (local $base i32)
      (local $res i32) (local $threshold i32)
      (local $t (tuple i32 i32 i32 i32))
      (local.set $s (call $string_val (local.get $v)))
      (local.set $len (call $string_length (local.get $s)))
      (if (i32.eqz (local.get $len))
        (then (call $caml_failwith (local.get $errmsg))))
      (local.set $t (call $parse_sign_and_base (local.get $s)))
      (local.set $i (tuple.extract 4 0 (local.get $t)))
      (local.set $signedness (tuple.extract 4 1 (local.get $t)))
      (local.set $sign (tuple.extract 4 2 (local.get $t)))
      (local.set $base (tuple.extract 4 3 (local.get $t)))
      (local.set $threshold (i32.div_u (i32.const -1) (local.get $base)))
      (if (i32.ge_s (local.get $i) (local.get $len))
         (then (call $caml_failwith (local.get $errmsg))))
      (local.set $d
         (call $parse_digit (call $string_get (local.get $s) (local.get $i))))
      (if (i32.ge_u (local.get $d) (local.get $base))
         (then (call $caml_failwith (local.get $errmsg))))
      (local.set $res (local.get $d))
      (loop $loop
         (local.set $i (i32.add (local.get $i) (i32.const 1)))
         (if (i32.lt_s (local.get $i) (local.get $len))
            (then
               (local.set $c (call $string_get (local.get $s) (local.get $i)))
               (br_if $loop (i32.eq (local.get $c) (@char "_")))
               (local.set $d (call $parse_digit (local.get $c)))
               (if (i32.ge_u (local.get $d) (local.get $base))
                  (then (call $caml_failwith (local.get $errmsg))))
               (if (i32.gt_u (local.get $res) (local.get $threshold))
                  (then (call $caml_failwith (local.get $errmsg))))
               (local.set $res
                  (i32.add (i32.mul (local.get $res) (local.get $base))
                           (local.get $d)))
               (if (i32.lt_u (local.get $res) (local.get $d))
                  (then (call $caml_failwith (local.get $errmsg))))
               (br $loop))))
      (if (local.get $signedness)
         (then
            (local.set $threshold
               (i32.shl (i32.const 1)
                  (i32.sub (local.get $nbits) (i32.const 1))))
            (if (i32.gt_s (local.get $sign) (i32.const 0))
               (then
                  (if (i32.ge_u (local.get $res) (local.get $threshold))
                     (then (call $caml_failwith (local.get $errmsg)))))
               (else
                  (if (i32.gt_u (local.get $res) (local.get $threshold))
                     (then (call $caml_failwith (local.get $errmsg)))))))
         (else
            (if (i32.and
                   (i32.lt_u (local.get $nbits) (i32.const 32))
                   (i32.ge_u (local.get $res)
                     (i32.shl (i32.const 1) (local.get $nbits))))
               (then (call $caml_failwith (local.get $errmsg))))))
      (if (i32.lt_s (local.get $sign) (i32.const 0))
         (then (local.set $res (i32.sub (i32.const 0) (local.get $res)))))
      (local.get $res))

   (@string $INT_ERRMSG "int_of_string")

   (func (export "caml_int_of_string")
      (param $v (ref eq)) (result (ref eq))
      (ref.i31
         (call $parse_int
            (local.get $v) (i32.const 31) (global.get $INT_ERRMSG))))

   (func (export "caml_bswap16") (param (ref eq)) (result (ref eq))
      (local $x i32)
      (local.set $x (i31.get_s (ref.cast (ref i31) (local.get 0))))
      (ref.i31
         (i32.or
            (i32.shl (i32.and (local.get $x) (i32.const 0xFF)) (i32.const 8))
            (i32.and
               (i32.shr_u (local.get $x) (i32.const 8)) (i32.const 0xFF)))))

   (type $chars (array i8))

   (global $lowercase_hex_table (export "lowercase_hex_table") (ref $chars)
      (array.new_fixed $chars 16
         (@char "0") (@char "1") (@char "2") (@char "3")
         (@char "4") (@char "5") (@char "6") (@char "7")
         (@char "8") (@char "9") (@char "a") (@char "b")
         (@char "c") (@char "d") (@char "e") (@char "f")))

   (global $uppercase_hex_table (export "uppercase_hex_table") (ref $chars)
      (array.new_fixed $chars 16
         (@char "0") (@char "1") (@char "2") (@char "3")
         (@char "4") (@char "5") (@char "6") (@char "7")
         (@char "8") (@char "9") (@char "A") (@char "B")
         (@char "C") (@char "D") (@char "E") (@char "F")))

   (func $format_int_default (param $d i32) (result (ref eq))
      (local $s (ref $bytes))
      (local $negative i32) (local $i i32) (local $n i32)
      (if (i32.lt_s (local.get $d) (i32.const 0))
         (then
            (local.set $negative (i32.const 1))
            (local.set $i (i32.const 1))
            (local.set $d (i32.sub (i32.const 0) (local.get $d)))))
      (local.set $n (local.get $d))
      (loop $count
         (local.set $i (i32.add (local.get $i) (i32.const 1)))
         (local.set $n (i32.div_u (local.get $n) (i32.const 10)))
         (br_if $count (local.get $n)))
      (local.set $s (array.new $bytes (i32.const 0) (local.get $i)))
      (loop $write
         (local.set $i (i32.sub (local.get $i) (i32.const 1)))
         (array.set $bytes (local.get $s) (local.get $i)
            (i32.add (@char "0")
               (i32.rem_u (local.get $d) (i32.const 10))))
         (local.set $d (i32.div_u (local.get $d) (i32.const 10)))
         (br_if $write (local.get $d)))
      (if (local.get $negative)
         (then (array.set $bytes (local.get $s) (i32.const 0) (@char "-"))))
      (return_call $caml_string_of_bytes (local.get $s)))

   (@string $format_error "format_int: bad format")

   (func $parse_int_format (export "parse_int_format")
(@if use-js-string
(@then
      (param $s externref)
)
(@else
      (param $s (ref $bytes))
))
      (result i32 i32 i32 i32 i32)
      (local $i i32) (local $len i32) (local $c i32)
      (local $sign_style i32) (local $alternate i32) (local $base i32)
      (local $signed i32) (local $uppercase i32)
      (local.set $len (call $string_length (local.get $s)))
      (local.set $i (i32.const 1))
      (block $return
         (block $bad_format
            (br_if $bad_format (i32.lt_u (local.get $len) (i32.const 2)))
            (br_if $bad_format
               (i32.ne (call $string_get (local.get $s) (i32.const 0))
                       (@char "%")))
            (local.set $c (call $string_get (local.get $s) (i32.const 1)))
            (if (i32.eq (local.get $c) (@char "+"))
               (then
                  (local.set $sign_style (i32.const 1))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))))
            (if (i32.eq (local.get $c) (@char " "))
               (then
                  (local.set $sign_style (i32.const 2))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))))
            (if (i32.eq (local.get $c) (@char "#"))
               (then
                  (local.set $alternate (i32.const 1))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))))
            (br_if $bad_format (i32.eq (local.get $i) (local.get $len)))
            (local.set $c (call $string_get (local.get $s) (local.get $i)))
            (if (i32.or (i32.or (i32.eq (local.get $c) (@char "L"))
                                (i32.eq (local.get $c) (@char "l")))
                        (i32.eq (local.get $c) (@char "n")))
               (then
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (br_if $bad_format (i32.eq (local.get $i) (local.get $len)))
                  (local.set $c
                     (call $string_get (local.get $s) (local.get $i)))))
            (br_if $bad_format
              (i32.ne (i32.add (local.get $i) (i32.const 1)) (local.get $len)))
            (if (i32.or (i32.eq (local.get $c) (@char "d"))
                        (i32.eq (local.get $c) (@char "i")))
               (then
                  (local.set $base (i32.const 10))
                  (local.set $signed (i32.const 1)))
            (else (if (i32.eq (local.get $c) (@char "u"))
               (then
                  (local.set $base (i32.const 10)))
            (else (if (i32.eq (local.get $c) (@char "x"))
               (then
                  (local.set $base (i32.const 16)))
            (else (if (i32.eq (local.get $c) (@char "X"))
               (then
                  (local.set $base (i32.const 16))
                  (local.set $uppercase (i32.const 1)))
            (else (if (i32.eq (local.get $c) (@char "o"))
               (then
                  (local.set $base (i32.const 8)))
            (else
               (br $bad_format)))))))))))
            (br $return))
         (call $caml_invalid_argument (global.get $format_error)))
      (tuple.make 5
         (local.get $sign_style)
         (local.get $alternate)
         (local.get $signed)
         (local.get $base)
         (local.get $uppercase)))

   (func $format_int (export "format_int")
      (param (ref eq)) (param $d i32) (param $small i32) (result (ref eq))
(@if use-js-string
(@then
      (local $fmt externref)
)
(@else
      (local $fmt (ref $bytes))
))
      (local $s (ref $bytes))
      (local $format (tuple i32 i32 i32 i32 i32))
      (local $sign_style i32) (local $alternate i32) (local $signed i32)
      (local $base i32) (local $uppercase i32)
      (local $negative i32)
      (local $i i32)
      (local $n i32)
      (local $chars (ref $chars))
      (local.set $fmt (call $string_val (local.get 0)))
      (if (i32.eq (call $string_length (local.get $fmt)) (i32.const 2))
         (then
            (if (i32.eq (call $string_get (local.get $fmt) (i32.const 1))
                        (@char "d"))
               (then (return_call $format_int_default (local.get $d))))))
      (local.set $format (call $parse_int_format (local.get $fmt)))
      (local.set $sign_style (tuple.extract 5 0 (local.get $format)))
      (local.set $alternate (tuple.extract 5 1 (local.get $format)))
      (local.set $signed (tuple.extract 5 2 (local.get $format)))
      (local.set $base (tuple.extract 5 3 (local.get $format)))
      (local.set $uppercase (tuple.extract 5 4 (local.get $format)))
      (if (i32.lt_s (local.get $d) (i32.const 0))
         (then
            (if (local.get $signed)
               (then
                  (local.set $negative (i32.const 1))
                  (local.set $d (i32.sub (i32.const 0) (local.get $d))))
               (else
                  (if (local.get $small)
                     (then
                        (local.set $d
                           (i32.and (local.get $d) (i32.const 0x7fffffff)))))))))
      (local.set $n (local.get $d))
      (loop $count
         (local.set $i (i32.add (local.get $i) (i32.const 1)))
         (local.set $n (i32.div_u (local.get $n) (local.get $base)))
         (br_if $count (local.get $n)))
      (if (i32.or (local.get $negative)
                  (local.get $sign_style))
         (then (local.set $i (i32.add (local.get $i) (i32.const 1)))))
      (if (local.get $alternate)
         (then
            (if (local.get $d)
               (then
                  (if (i32.eq (local.get $base) (i32.const 16))
                     (then
                        (local.set $i (i32.add (local.get $i) (i32.const 2)))))
                  (if (i32.eq (local.get $base) (i32.const 8))
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
               (i32.rem_u (local.get $d) (local.get $base))))
         (local.set $d (i32.div_u (local.get $d) (local.get $base)))
         (br_if $write (local.get $d)))
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
                  (if (i32.eq (local.get $base) (i32.const 16))
                     (then
                        (array.set $bytes (local.get $s) (i32.const 1)
                           (select (@char "X") (@char "x")
                              (local.get $uppercase)))))))))
      (return_call $caml_string_of_bytes (local.get $s)))
)
