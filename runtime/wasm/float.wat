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
   (import "bindings" "format_float"
      (func $format_float
         (param i32) (param i32) (param i32) (param f64) (result anyref)))
   (import "bindings" "identity"
      (func $parse_float (param anyref) (result f64)))
   (import "Math" "exp" (func $exp (param f64) (result f64)))
   (import "fail" "caml_failwith" (func $caml_failwith (param (ref eq))))
   (import "fail" "caml_invalid_argument"
      (func $caml_invalid_argument (param (ref eq))))
   (import "ints" "lowercase_hex_table"
      (global $lowercase_hex_table (ref $chars)))
   (import "jsstring" "jsstring_of_bytes"
      (func $jsstring_of_bytes (param (ref $bytes)) (result anyref)))
   (import "jsstring" "bytes_of_jsstring"
      (func $bytes_of_jsstring (param anyref) (result (ref $bytes))))
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

  (type $float (struct (field f64)))
   (type $bytes (array (mut i8)))
   (type $string (struct (field anyref)))
   (type $block (array (mut (ref eq))))

   (type $chars (array i8))

   (global $infinity (ref $chars)
      (array.new_fixed $chars 8
         (@char "i") (@char "n") (@char "f") (@char"i")
         (@char "n") (@char "i") (@char "t") (@char "y")))

   (global $nan (ref $chars)
      (array.new_fixed $chars 3 (@char "n") (@char "a") (@char "n")))

   (func (export "Double_val") (param (ref eq)) (result f64)
      (struct.get $float 0 (ref.cast (ref $float) (local.get 0))))

   (func (export "caml_hexstring_of_float")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
      (local $b i64) (local $prec i32) (local $style i32)
      (local $sign i32) (local $exp i32) (local $m i64)
      (local $i i32) (local $j i32) (local $d i32) (local $txt (ref $chars))
      (local $len i32) (local $s (ref $bytes))
      (local $unit i64) (local $half i64) (local $mask i64) (local $frac i64)
      (local.set $prec (i31.get_s (ref.cast (ref i31) (local.get 1))))
      (local.set $style (i31.get_s (ref.cast (ref i31) (local.get 2))))
      (local.set $b
         (i64.reinterpret_f64
            (struct.get $float 0 (ref.cast (ref $float) (local.get 0)))))
      (local.set $sign (i32.wrap_i64 (i64.shr_u (local.get $b) (i64.const 63))))
      (local.set $exp
          (i32.and (i32.wrap_i64 (i64.shr_u (local.get $b) (i64.const 52)))
                   (i32.const 0x7FF)))
      (local.set $m
         (i64.and (local.get $b)
            (i64.sub (i64.shl (i64.const 1) (i64.const 52)) (i64.const 1))))
      (local.set $i
         (i32.or (local.get $sign) (i32.ne (local.get $style) (@char "-"))))
      (local.set $s
         (block $sign (result (ref $bytes))
            (if (i32.eq (local.get $exp) (i32.const 0x7FF))
               (then
                  (local.set $txt
                     (if (result (ref $chars)) (i64.eqz (local.get $m))
                        (then
                           (global.get $infinity))
                        (else
                           (global.get $nan))))
                  (local.set $len (array.len (local.get $txt)))
                  (local.set $s
                     (array.new $bytes (i32.const 0)
                        (i32.add (local.get $i) (local.get $len))))
                  (array.copy $bytes $chars
                     (local.get $s) (local.get $i) (local.get $txt) (i32.const 0)
                     (local.get $len))
                  (br $sign (local.get $s))))
            (if (i32.eqz (local.get $exp))
               (then
                  (if (i64.ne (local.get $m) (i64.const 0))
                     (then (local.set $exp (i32.const -1022)))))
               (else
                  (local.set $exp (i32.sub (local.get $exp) (i32.const 1023)))
                  (local.set $m
                     (i64.or (local.get $m)
                             (i64.shl (i64.const 1) (i64.const 52))))))
            (if (i32.and (i32.ge_s (local.get $prec) (i32.const 0))
                         (i32.lt_s (local.get $prec) (i32.const 13)))
               (then
                  (local.set $unit
                     (i64.shl (i64.const 1)
                        (i64.extend_i32_s
                           (i32.sub (i32.const 52)
                              (i32.shl (local.get $prec) (i32.const 2))))))
                  (local.set $half
                     (i64.shr_u (local.get $unit) (i64.const 1)))
                  (local.set $mask (i64.sub (local.get $unit) (i64.const 1)))
                  (local.set $frac (i64.and (local.get $m) (local.get $mask)))
                  (local.set $m
                     (i64.and (local.get $m)
                        (i64.xor (i64.const -1) (local.get $mask))))
                  (if (i32.or (i64.gt_u (local.get $frac) (local.get $half))
                         (i32.and (i64.eq (local.get $frac) (local.get $half))
                                  (i64.ne (i64.and (local.get $m)
                                                   (local.get $unit))
                                          (i64.const 0))))
                     (then
                        (local.set $m
                           (i64.add (local.get $m) (local.get $unit)))))))
         (local.set $frac (i64.shl (local.get $m) (i64.const 12)))
         (local.set $j (i32.const 0))
         (loop $prec
            (if (i64.ne (local.get $frac) (i64.const 0))
               (then
                  (local.set $j (i32.add (local.get $j) (i32.const 1)))
                  (local.set $frac (i64.shl (local.get $frac) (i64.const 4)))
                  (br $prec))))
         (if (i32.lt_s (local.get $prec) (local.get $j))
            (then (local.set $prec (local.get $j))))
         (if (i32.ge_s (local.get $exp) (i32.const 0))
            (then (local.set $d (local.get $exp)))
            (else (local.set $d (i32.sub (i32.const 0) (local.get $exp)))))
         (local.set $j (i32.const 0))
         (loop $count
            (local.set $j (i32.add (local.get $j) (i32.const 1)))
            (local.set $d (i32.div_u (local.get $d) (i32.const 10)))
            (br_if $count (local.get $d)))
         (local.set $len (i32.add (i32.add (local.get $i) (local.get $prec))
                                  (i32.add (i32.const 6) (local.get $j))))
         (if (i32.eqz (local.get $prec))
            (then (local.set $len (i32.sub (local.get $len) (i32.const 1)))))
         (local.set $s (array.new $bytes (i32.const 0) (local.get $len)))
         (if (i32.ge_s (local.get $exp) (i32.const 0))
            (then (local.set $d (local.get $exp)))
            (else (local.set $d (i32.sub (i32.const 0) (local.get $exp)))))
         (loop $write
            (local.set $len (i32.sub (local.get $len) (i32.const 1)))
            (array.set $bytes (local.get $s) (local.get $len)
               (i32.add (@char "0")
                  (i32.rem_u (local.get $d) (i32.const 10))))
            (local.set $d (i32.div_u (local.get $d) (i32.const 10)))
            (br_if $write (local.get $d)))
         (array.set $bytes (local.get $s)
            (i32.sub (local.get $len) (i32.const 1))
            (select (@char "+") (@char "-")
               (i32.ge_s (local.get $exp) (i32.const 0))))
         (array.set $bytes (local.get $s) (local.get $i) (@char "0"))
         (array.set $bytes (local.get $s) (i32.add (local.get $i) (i32.const 1))
            (@char "x"))
         (array.set $bytes (local.get $s) (i32.add (local.get $i) (i32.const 2))
            (i32.add
               (i32.wrap_i64 (i64.shr_u (local.get $m) (i64.const 52)))
               (@char "0")))
         (local.set $i (i32.add (local.get $i) (i32.const 3)))
         (if (i32.gt_s (local.get $prec) (i32.const 0))
            (then
               (array.set $bytes (local.get $s) (local.get $i) (@char "."))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (local.set $frac (i64.shl (local.get $m) (i64.const 12)))
               (loop $write
                  (array.set $bytes (local.get $s) (local.get $i)
                     (array.get_u $chars (global.get $lowercase_hex_table)
                        (i32.wrap_i64
                           (i64.shr_u (local.get $frac) (i64.const 60)))))
                  (local.set $frac (i64.shl (local.get $frac) (i64.const 4)))
                  (local.set $prec (i32.sub (local.get $prec) (i32.const 1)))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (br_if $write (i32.gt_s (local.get $prec) (i32.const 0))))))
         (array.set $bytes (local.get $s) (local.get $i) (@char "p"))
         (local.get $s)))
      (if (local.get $sign)
         (then
            (array.set $bytes (local.get $s) (i32.const 0) (@char "-")))
         (else
            (if (i32.ne (local.get $style) (@char "-"))
               (then
                  (array.set $bytes (local.get $s) (i32.const 0)
                     (local.get $style))))))
      (return_call $caml_string_of_bytes (local.get $s)))

   (@string $format_error "format_float: bad format")

   (func $parse_format
      (param $v (ref eq)) (result i32 i32 i32 i32)
(@if use-js-string
(@then
      (local $s externref)
)
(@else
      (local $s (ref $bytes))
))
      (local $i i32) (local $len i32) (local $c i32)
      (local $sign_style i32) (local $precision i32)
      (local $conversion i32) (local $uppercase i32)
      (local.set $s (call $string_val (local.get $v)))
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
            (br_if $bad_format (i32.eq (local.get $i) (local.get $len)))
            (br_if $bad_format
               (i32.ne (call $string_get (local.get $s) (local.get $i))
                       (@char "."))) ;; '.'
            (loop $precision
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br_if $bad_format (i32.eq (local.get $i) (local.get $len)))
               (local.set $c
                  (call $string_get (local.get $s) (local.get $i)))
               (if (i32.and (i32.ge_u (local.get $c) (@char "0"))
                            (i32.le_u (local.get $c) (@char "9")))
                  (then
                     (local.set $precision
                        (i32.add (i32.mul (local.get $precision) (i32.const 10))
                                 (i32.sub (local.get $c) (@char "0"))))
                     (br $precision))))
            (br_if $bad_format
              (i32.ne (i32.add (local.get $i) (i32.const 1)) (local.get $len)))
            (local.set $uppercase (i32.lt_s (local.get $c) (@char "a")))
            (local.set $conversion
               (i32.sub (i32.and (local.get $c) (i32.const 0xdf)) (@char "E")))
            (br_if $return (i32.le_u (local.get $conversion) (i32.const 2))))
         (call $caml_invalid_argument (global.get $format_error)))
      (tuple.make 4
         (local.get $sign_style)
         (local.get $precision)
         (local.get $conversion)
         (local.get $uppercase)))

   (global $inf (ref $chars)
      (array.new_fixed $chars 3 (@char "i") (@char "n") (@char "f")))

   (func (export "caml_format_float")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (local $f f64) (local $b i64) (local $format (tuple i32 i32 i32 i32))
      (local $sign_style i32) (local $precision i32)
      (local $conversion i32) (local $uppercase i32)
      (local $negative i32)
      (local $exp i32) (local $m i64)
      (local $i i32) (local $len i32) (local $c i32)
      (local $s (ref $bytes)) (local $txt (ref $chars))
      (local $num anyref)
      (local.set $f (struct.get $float 0 (ref.cast (ref $float) (local.get 1))))
      (local.set $b (i64.reinterpret_f64 (local.get $f)))
      (local.set $format (call $parse_format (local.get 0)))
      (local.set $sign_style (tuple.extract 4 0 (local.get $format)))
      (local.set $precision (tuple.extract 4 1 (local.get $format)))
      (local.set $conversion (tuple.extract 4 2 (local.get $format)))
      (local.set $uppercase (tuple.extract 4 3 (local.get $format)))
      (local.set $negative
         (i32.wrap_i64 (i64.shr_u (local.get $b) (i64.const 63))))
      (local.set $i
         (i32.or (local.get $negative)
                 (i32.ne (local.get $sign_style) (i32.const 0))))
      (local.set $s
         (block $sign (result (ref $bytes))
            (local.set $exp
                (i32.and (i32.wrap_i64 (i64.shr_u (local.get $b) (i64.const 52)))
                         (i32.const 0x7FF)))
            (if (i32.eq (local.get $exp) (i32.const 0x7FF))
               (then
                  (local.set $m (i64.shl (local.get $b) (i64.const 12)))
                  (local.set $txt
                     (if (result (ref $chars)) (i64.eqz (local.get $m))
                        (then
                           (global.get $inf))
                        (else
                           (local.set $negative (i32.const 0))
                           (local.set $i
                              (i32.ne (local.get $sign_style) (i32.const 0)))
                           (global.get $nan))))
                  (local.set $len (array.len (local.get $txt)))
                  (local.set $s
                     (array.new $bytes (i32.const 0)
                        (i32.add (local.get $i) (local.get $len))))
                  (array.copy $bytes $chars
                     (local.get $s) (local.get $i) (local.get $txt) (i32.const 0)
                     (local.get $len))
                  (br $sign (local.get $s))))
            (local.set $num
               (call $format_float
                  (local.get $precision) (local.get $conversion)
                  (local.get $i)
                  (f64.abs (local.get $f))))
            (local.set $s (call $bytes_of_jsstring (local.get $num)))
            (br $sign (local.get $s))))
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
      (if (local.get $uppercase)
         (then
            (local.set $i (i32.const 0))
            (local.set $len (array.len (local.get $s)))
            (loop $uppercase
               (local.set $c (array.get_u $bytes (local.get $s) (local.get $i)))
               (if (i32.and (i32.ge_u (local.get $c) (@char "a"))
                            (i32.le_u (local.get $c) (@char "z")))
                  (then
                     (array.set $bytes (local.get $s) (local.get $i)
                        (i32.sub (local.get $c) (i32.const 32)))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br_if $uppercase (i32.lt_u (local.get $i) (local.get $len))))))
      (return_call $caml_string_of_bytes (local.get $s)))

   (@string $float_of_string "float_of_string")

   (func $caml_float_of_hex
(@if use-js-string
(@then
      (param $s externref)
)
(@else
      (param $s (ref $bytes))
))
      (param $i i32) (result f64)
      (local $len i32) (local $c i32) (local $d i32) (local $m i64)
      (local $f f64) (local $negative i32)
      (local $dec_point i32) (local $exp i32) (local $adj i32)
      (local $n_bits i32) (local $m_bits i32) (local $x_bits i32)
      (local.set $len (call $string_length (local.get $s)))
      (local.set $dec_point (i32.const -1))
      (block $error
         (loop $parse
            (if (i32.lt_u (local.get $i) (local.get $len))
               (then
                  (local.set $c
                     (call $string_get (local.get $s) (local.get $i)))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (if (i32.eq (local.get $c) (@char "."))
                     (then
                        (br_if $error
                           (i32.ge_s (local.get $dec_point) (i32.const 0)))
                        (local.set $dec_point (local.get $n_bits))
                        (br $parse)))
                  (if (i32.or (i32.eq (local.get $c) (@char "p"))
                              (i32.eq (local.get $c) (@char "P")))
                     (then
                        (br_if $error (i32.eq (local.get $i) (local.get $len)))
                        (local.set $c
                           (call $string_get (local.get $s) (local.get $i)))
                        (local.set $i (i32.add (local.get $i) (i32.const 1)))
                        (if (i32.eq (local.get $c) (@char "-"))
                           (then
                              (local.set $negative (i32.const 1))
                              (br_if $error
                                 (i32.eq (local.get $i) (local.get $len)))
                              (local.set $c
                                 (call $string_get
                                    (local.get $s) (local.get $i)))
                              (local.set $i
                                 (i32.add (local.get $i) (i32.const 1)))))
                        (if (i32.eq (local.get $c) (@char "+"))
                           (then
                              (br_if $error
                                 (i32.eq (local.get $i) (local.get $len)))
                              (local.set $c
                                 (call $string_get
                                    (local.get $s) (local.get $i)))
                              (local.set $i
                                 (i32.add (local.get $i) (i32.const 1)))))
                        (block $overflow
                           (loop $parse_exponent
                              (br_if $error
                                 (i32.or (i32.lt_u (local.get $c) (@char "0"))
                                    (i32.gt_u (local.get $c) (@char "9"))))
                              (local.set $d
                                 (i32.sub (local.get $c) (@char "0")))
                              (local.set $exp
                                 (i32.add
                                    (i32.mul (local.get $exp) (i32.const 10))
                                    (local.get $d)))
                              (br_if $overflow
                                 (i32.lt_u (local.get $exp) (local.get $d)))
                              (if (i32.ne (local.get $i) (local.get $len))
                                 (then
                                    (local.set $c
                                       (call $string_get
                                          (local.get $s) (local.get $i)))
                                    (local.set $i
                                       (i32.add (local.get $i) (i32.const 1)))
                                    (br $parse_exponent))))
                           (if (local.get $negative)
                              (then
                                 (br_if $overflow
                                    (i32.ge_u (local.get $exp)
                                       (i32.const 0x80000000)))
                                 (local.set $exp
                                    (i32.sub (i32.const 0) (local.get $exp))))
                              (else
                                 (br_if $overflow
                                    (i32.ge_u (local.get $exp)
                                       (i32.const 0x80000000)))))
                           (br $parse))
                        (if (i32.or (local.get $negative)
                                    (i64.eqz (local.get $m)))
                           (then
                              (return (f64.const 0)))
                           (else
                              (return (f64.const inf))))))
                  (if (i32.and (i32.ge_u (local.get $c) (@char "0"))
                               (i32.le_u (local.get $c) (@char "9")))
                     (then
                        (local.set $d (i32.sub (local.get $c) (@char "0"))))
                  (else (if (i32.and (i32.ge_u (local.get $c) (@char "a"))
                                     (i32.le_u (local.get $c) (@char "f")))
                     (then
                        (local.set $d (i32.sub (local.get $c) (i32.const 87))))
                  (else (if (i32.and (i32.ge_u (local.get $c) (@char "A"))
                                     (i32.le_u (local.get $c) (@char "F")))
                     (then
                        (local.set $d (i32.sub (local.get $c) (i32.const 55))))
                     (else
                        (br $error)))))))
                  (local.set $n_bits
                     (i32.add (local.get $n_bits) (i32.const 4)))
                  (br_if $parse
                     (i32.and (i32.eqz (local.get $d)) (i64.eqz (local.get $m))))
                  (if (i32.lt_u (local.get $m_bits) (i32.const 60))
                     (then
                        (local.set $m
                           (i64.add (i64.shl (local.get $m) (i64.const 4))
                                    (i64.extend_i32_u (local.get $d))))
                        (local.set $m_bits
                           (i32.add (local.get $m_bits) (i32.const 4))))
                     (else
                        (if (local.get $d)
                           (then
                              (local.set $m
                                 (i64.or (local.get $m) (i64.const 1)))))
                        (local.set $x_bits
                           (i32.add (local.get $x_bits) (i32.const 4)))))
                  (br $parse))))
         (br_if $error (i32.eqz (local.get $n_bits)))
         (local.set $f (f64.convert_i64_s (local.get $m)))
         (local.set $adj (local.get $x_bits))
         (if (i32.ge_s (local.get $dec_point) (i32.const 0))
            (then
               (local.set $adj
                  (i32.add (local.get $adj)
                     (i32.sub (local.get $dec_point) (local.get $n_bits))))))
         (if (i32.and (i32.gt_s (local.get $adj) (i32.const 0))
                      (i32.gt_s (local.get $exp)
                         (i32.sub (i32.const 0x7fffffff) (local.get $adj))))
            (then (local.set $exp (i32.const 0x7fffffff)))
         (else (if (i32.and (i32.lt_s (local.get $adj) (i32.const 0))
                      (i32.lt_s (local.get $exp)
                         (i32.sub (i32.const 0x80000000) (local.get $adj))))
            (then (local.set $exp (i32.const 0x80000000)))
            (else
              (local.set $exp (i32.add (local.get $exp) (local.get $adj)))))))
         (if (local.get $exp)
            (then (local.set $f (call $ldexp (local.get $f) (local.get $exp)))))
         (return (local.get $f)))
      (call $caml_failwith (global.get $float_of_string))
      (f64.const 0))

   (func $on_whitespace
(@if use-js-string
(@then
      (param $s externref)
)
(@else
      (param $s (ref $bytes))
))
      (param $i i32) (result i32)
      (local $c i32)
      (local.set $c (call $string_get (local.get $s) (local.get $i)))
      (i32.or (i32.eq (local.get $c) (@char " "))
         (i32.le_u (i32.sub (local.get $c) (i32.const 9)) (i32.const 4))))

   (func (export "caml_float_of_string") (param (ref eq)) (result (ref eq))
(@if use-js-string
(@then
      (local $s externref)
)
(@else
      (local $s (ref $bytes))
))
      (local $len i32) (local $i i32) (local $j i32)
      (local $s' (ref $bytes))
      (local $negative i32) (local $c i32)
      (local $f f64)
      (local.set $s (call $string_val (local.get 0)))
      (local.set $len (call $string_length (local.get $s)))
      (loop $count
         (if (i32.lt_u (local.get $i) (local.get $len))
            (then
               (if (i32.eq (@char "_")
                      (call $string_get (local.get $s) (local.get $i)))
                  (then
                     (local.set $j (i32.add (local.get $j) (i32.const 1)))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $count))))
      (if (local.get $j)
         (then
            (local.set $s'
               (array.new $bytes (i32.const 0)
                  (i32.sub (local.get $len) (local.get $j))))
            (local.set $i (i32.const 0))
            (local.set $j (i32.const 0))
            (loop $copy
               (if (i32.lt_u (local.get $i) (local.get $len))
                  (then
                     (local.set $c
                        (call $string_get (local.get $s) (local.get $i)))
                     (local.set $i (i32.add (local.get $i) (i32.const 1)))
                     (if (i32.ne (local.get $c) (@char "_"))
                        (then
                           (array.set $bytes (local.get $s')
                              (local.get $j) (local.get $c))
                           (local.set $j
                              (i32.add (local.get $j) (i32.const 1)))))
                     (br $copy))))
            (local.set $len (array.len (local.get $s')))
(@if use-js-string
(@then
            (local.set $s
               (extern.convert_any (call $jsstring_of_bytes (local.get $s'))))
)
(@else
            (local.set $s (local.get $s'))
))
         ))
      (local.set $i (i32.const 0))
      (loop $skip_spaces
         (if (i32.lt_u (local.get $i) (local.get $len))
            (then
               (if (call $on_whitespace (local.get $s) (local.get $i))
                  (then
                     (local.set $i (i32.add (local.get $i) (i32.const 1)))
                     (br $skip_spaces))))))
      (block $error
         (br_if $error (i32.eq (local.get $i) (local.get $len)))
         (br_if $error
            (call $on_whitespace
               (local.get $s) (i32.sub (local.get $len) (i32.const 1))))
         (local.set $c (call $string_get (local.get $s) (i32.const 0)))
         (if (i32.eq (local.get $c) (@char "-"))
            (then
               (local.set $negative (i32.const 1))
               (local.set $i (i32.const 1))))
         (if (i32.eq (local.get $c) (@char "+"))
            (then
               (local.set $i (i32.const 1))))
         (if (i32.lt_u (i32.add (local.get $i) (i32.const 2)) (local.get $len))
            (then
               (if (i32.eq (call $string_get (local.get $s) (local.get $i))
                           (@char "0"))
                  (then
                     (if (i32.eq (i32.and
                                    (call $string_get (local.get $s)
                                       (i32.add (local.get $i) (i32.const 1)))
                                    (i32.const 0xdf))
                                 (@char "X"))
                        (then
                           (local.set $f
                              (call $caml_float_of_hex (local.get $s)
                                 (i32.add (local.get $i) (i32.const 2))))
                           (if (local.get $negative)
                              (then (local.set $f (f64.neg (local.get $f)))))
                           (return (struct.new $float (local.get $f)))))))))
         (if (i32.eq (i32.add (local.get $i) (i32.const 3)) (local.get $len))
            (then
               (local.set $c (call $string_get (local.get $s) (local.get $i)))
               (if (i32.eq (i32.and (local.get $c) (i32.const 0xdf))
                           (@char "N")) (then
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (local.set $c
                     (call $string_get (local.get $s) (local.get $i)))
                  (if (i32.eq (i32.and (local.get $c) (i32.const 0xdf))
                              (@char "A")) (then
                     (local.set $i (i32.add (local.get $i) (i32.const 1)))
                     (local.set $c
                        (call $string_get (local.get $s) (local.get $i)))
                     (if (i32.eq (i32.and (local.get $c) (i32.const 0xdf))
                                 (@char "N"))
                        (then
                           (return
                              (struct.new $float (f64.const nan)))))))))
               (if (i32.eq (i32.and (local.get $c) (i32.const 0xdf))
                           (@char "I")) (then
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (local.set $c
                     (call $string_get (local.get $s) (local.get $i)))
                  (if (i32.eq (i32.and (local.get $c) (i32.const 0xdf))
                              (@char "N")) (then
                     (local.set $i (i32.add (local.get $i) (i32.const 1)))
                     (local.set $c
                        (call $string_get (local.get $s) (local.get $i)))
                     (if (i32.eq (i32.and (local.get $c) (i32.const 0xdf))
                                 (@char "F"))
                        (then
                           (return
                              (struct.new $float
                                 (select
                                    (f64.const -inf)
                                    (f64.const inf)
                                    (local.get $negative))))))))))))
         (if (i32.eq (i32.add (local.get $i) (i32.const 8)) (local.get $len))
            (then
               (local.set $c (call $string_get (local.get $s) (local.get $i)))
               (if (i32.eq (i32.and (local.get $c) (i32.const 0xdf))
                           (@char "I")) (then
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (local.set $c
                     (call $string_get (local.get $s) (local.get $i)))
                  (if (i32.eq (i32.and (local.get $c) (i32.const 0xdf))
                              (@char "N")) (then
                     (local.set $i (i32.add (local.get $i) (i32.const 1)))
                     (local.set $c
                        (call $string_get (local.get $s) (local.get $i)))
                     (if (i32.eq (i32.and (local.get $c) (i32.const 0xdf))
                                 (@char "F")) (then
                        (local.set $i (i32.add (local.get $i) (i32.const 1)))
                        (local.set $c
                           (call $string_get (local.get $s) (local.get $i)))
                        (if (i32.eq (i32.and (local.get $c) (i32.const 0xdf))
                                    (@char "I")) (then
                           (local.set $i (i32.add (local.get $i) (i32.const 1)))
                           (local.set $c
                              (call $string_get
                                 (local.get $s) (local.get $i)))
                           (if (i32.eq (i32.and (local.get $c) (i32.const 0xdf))
                                       (@char "N")) (then
                              (local.set $i
                                 (i32.add (local.get $i) (i32.const 1)))
                              (local.set $c
                                 (call $string_get
                                     (local.get $s) (local.get $i)))
                              (if (i32.eq
                                     (i32.and (local.get $c) (i32.const 0xdf))
                                     (@char "I")) (then
                                 (local.set $i
                                    (i32.add (local.get $i) (i32.const 1)))
                                 (local.set $c
                                    (call $string_get
                                       (local.get $s) (local.get $i)))
                                 (if (i32.eq
                                        (i32.and (local.get $c) (i32.const 0xdf))
                                             (@char "T")) (then
                                    (local.set $i
                                       (i32.add (local.get $i) (i32.const 1)))
                                    (local.set $c
                                       (call $string_get
                                           (local.get $s) (local.get $i)))
                                    (if (i32.eq
                                           (i32.and (local.get $c)
                                              (i32.const 0xdf))
                                           (@char "Y")) (then
                                       (return
                                          (struct.new $float
                                             (select
                                                (f64.const -inf)
                                                (f64.const inf)
                                                (local.get $negative))))
                                       ))))))))))))))))))
         (local.set $f
            (call $parse_float
(@if use-js-string
(@then
               (any.convert_extern (local.get $s))
)
(@else
               (call $jsstring_of_bytes (local.get $s))
))
            ))
         (br_if $error (f64.ne (local.get $f) (local.get $f)))
         (return (struct.new $float (local.get $f))))
      (call $caml_failwith (global.get $float_of_string))
      (return (ref.i31 (i32.const 0))))

   (func (export "caml_nextafter_float")
      (param $x f64) (param $y f64) (result f64)
      (local $i i64) (local $j i64)
      (if (f64.ne (local.get $x) (local.get $x)) (then (return (local.get $x))))
      (if (f64.ne (local.get $y) (local.get $y)) (then (return (local.get $y))))
      (if (f64.eq (local.get $x) (local.get $y))
         (then (return (local.get $y))))
      (if (f64.eq (local.get $x) (f64.const 0))
         (then
            (if (f64.ge (local.get $y) (f64.const 0))
               (then (return (f64.const 0x1p-1074)))
               (else (return (f64.const -0x1p-1074)))))
         (else
            (local.set $i (i64.reinterpret_f64 (local.get $x)))
            (local.set $j (i64.reinterpret_f64 (local.get $y)))
            (if (i32.and (i64.lt_s (local.get $i) (local.get $j))
                         (i64.lt_u (local.get $i) (local.get $j)))
               (then (local.set $i (i64.add (local.get $i) (i64.const 1))))
               (else (local.set $i (i64.sub (local.get $i) (i64.const 1)))))
            (return (f64.reinterpret_i64 (local.get $i))))))

   (func (export "caml_classify_float") (param $x f64) (result (ref eq))
      (local $a f64)
      (local.set $a (f64.abs (local.get $x)))
      (ref.i31
         (if (result i32) (f64.ge (local.get $a) (f64.const 0x1p-1022))
            (then
               (if (result i32) (f64.lt (local.get $a) (f64.const inf))
                  (then (i32.const 0)) ;; normal
                  (else (i32.const 3)))) ;; infinity
            (else
               (if (result i32) (f64.eq (local.get $a) (f64.const 0))
                  (then (i32.const 2)) ;; zero
                  (else
                     (if (result i32) (f64.eq (local.get $a) (local.get $a))
                        (then (i32.const 1)) ;; subnormal
                        (else (i32.const 4))))))))) ;; nan

   (func (export "caml_modf_float") (param (ref eq)) (result (ref eq))
      (local $x f64) (local $a f64) (local $i f64) (local $f f64)
      (local.set $x (struct.get $float 0 (ref.cast (ref $float) (local.get 0))))
      (local.set $a (f64.abs (local.get $x)))
      (if (f64.ge (local.get $a) (f64.const 0))
         (then
            (if (f64.lt (local.get $a) (f64.const inf))
               (then ;; normal
                  (local.set $i (f64.floor (local.get $a)))
                  (local.set $f (f64.sub (local.get $a) (local.get $i)))
                  (local.set $i (f64.copysign (local.get $i) (local.get $x)))
                  (local.set $f (f64.copysign (local.get $f) (local.get $x))))
               (else ;; infinity
                  (local.set $i (local.get $x))
                  (local.set $f (f64.copysign (f64.const 0) (local.get $x))))))
         (else ;; zero or nan
            (local.set $i (local.get $x))
            (local.set $f (local.get $x))))
      (array.new_fixed $block 3 (ref.i31 (i32.const 0))
         (struct.new $float (local.get $f)) (struct.new $float (local.get $i))))

   (func $ldexp (param $x f64) (param $n i32) (result f64)
      (if (i32.gt_s (local.get $n) (i32.const 1023))
         (then
            (local.set $x (f64.mul (local.get $x) (f64.const 0x1p1023)))
            (local.set $n (i32.sub (local.get $n) (i32.const 1023)))
            (if (i32.gt_s (local.get $n) (i32.const 1023))
               (then
                  ;; subnormal
                  (local.set $x (f64.mul (local.get $x) (f64.const 0x1p1023)))
                  (local.set $n (i32.sub (local.get $n) (i32.const 1023)))
                  (if (i32.gt_s (local.get $n) (i32.const 1023))
                     (then (local.set $n (i32.const 1023)))))))
         (else
            (if (i32.lt_s (local.get $n) (i32.const -1022))
               (then
                  (local.set $x (f64.mul (local.get $x) (f64.const 0x1p-969)))
                  (local.set $n (i32.add (local.get $n) (i32.const 969)))
                  (if (i32.lt_s (local.get $n) (i32.const -1022))
                     (then
                        (local.set $x
                           (f64.mul (local.get $x) (f64.const 0x1p-969)))
                        (local.set $n (i32.add (local.get $n) (i32.const 969)))
                        (if (i32.lt_s (local.get $n) (i32.const -1022))
                           (then (local.set $n (i32.const -1022))))))))))
      (f64.mul (local.get $x)
         (f64.reinterpret_i64
            (i64.shl (i64.add (i64.extend_i32_s (local.get $n))
                              (i64.const 0x3ff))
                     (i64.const 52)))))

   (func (export "caml_ldexp_float")
      (param $x f64) (param $i (ref eq)) (result f64)
      (call $ldexp
         (local.get $x)
         (i31.get_s (ref.cast (ref i31) (local.get $i)))))

   (func $frexp (param $x f64) (result f64 i32)
      (local $y i64)
      (local $e i32)
      (local $r (tuple f64 i32))
      (local.set $y (i64.reinterpret_f64 (local.get $x)))
      (local.set $e
         (i32.and (i32.const 0x7ff)
            (i32.wrap_i64 (i64.shr_u (local.get $y) (i64.const 52)))))
      (if (i32.eqz (local.get $e))
         (then
            (if (f64.ne (local.get $x) (f64.const 0))
               (then
                  (local.set $r
                     (call $frexp (f64.mul (local.get $x) (f64.const 0x1p64))))
                  (return
                     (tuple.make 2
                        (tuple.extract 2 0 (local.get $r))
                        (i32.sub (tuple.extract 2 1 (local.get $r))
                           (i32.const 64)))))
               (else
                  (return (tuple.make 2 (local.get $x) (i32.const 0))))))
         (else
            (if (i32.eq (local.get $e) (i32.const 0x7ff))
               (then
                  (return (tuple.make 2 (local.get $x) (i32.const 0)))))))
      (tuple.make 2
         (f64.reinterpret_i64
            (i64.or (i64.and (local.get $y) (i64.const 0x800fffffffffffff))
               (i64.const 0x3fe0000000000000)))
         (i32.sub (local.get $e) (i32.const 0x3fe))))

   (func (export "caml_frexp_float") (param (ref eq)) (result (ref eq))
      (local $r (tuple f64 i32))
      (local.set $r
         (call $frexp
            (struct.get $float 0 (ref.cast (ref $float) (local.get 0)))))
      (array.new_fixed $block 3 (ref.i31 (i32.const 0))
         (struct.new $float (tuple.extract 2 0 (local.get $r)))
         (ref.i31 (tuple.extract 2 1 (local.get $r)))))

   (func $erf (export "caml_erf_float") (param $x f64) (result f64)
      (local $a1 f64) (local $a2 f64) (local $a3 f64)
      (local $a4 f64) (local $a5 f64) (local $p f64)
      (local $t f64) (local $y f64)
      (local.set $a1 (f64.const 0.254829592))
      (local.set $a2 (f64.const -0.284496736))
      (local.set $a3 (f64.const 1.421413741))
      (local.set $a4 (f64.const -1.453152027))
      (local.set $a5 (f64.const 1.061405429))
      (local.set $p (f64.const 0.3275911))
      (local.set $t
         (f64.div (f64.const 1)
            (f64.add (f64.const 1)
               (f64.mul (local.get $p) (f64.abs (local.get $x))))))
      (local.set $y
          (f64.sub (f64.const 1)
             (f64.mul
                (f64.add
                   (f64.mul
                      (f64.add
                         (f64.mul
                            (f64.add
                               (f64.mul
                                  (f64.add
                                     (f64.mul (local.get $a5) (local.get $t))
                                        (local.get $a4))
                                  (local.get $t))
                               (local.get $a3))
                            (local.get $t))
                         (local.get $a2))
                      (local.get $t))
                   (local.get $a1))
                (f64.mul (local.get $t)
                   (call $exp
                      (f64.neg (f64.mul (local.get $x) (local.get $x))))))))
      (f64.copysign (local.get $y) (local.get $x)))

   (func (export "caml_erfc_float") (param $x f64) (result f64)
      (f64.sub (f64.const 1) (call $erf (local.get $x))))

   (func (export "caml_fma_float")
      (param $vx (ref eq)) (param $vy (ref eq)) (param $vz (ref eq))
      (result (ref eq))
      (local $x f64)
      (local $y f64)
      (local $z f64)
      (local $3 i64)
      (local $4 i64)
      (local $5 i64)
      (local $6 i64)
      (local $7 i64)
      (local $8 i64)
      (local $9 i32)
      (local $10 i32)
      (local $11 f64)
      (local $12 f64)
      (local $13 f64)
      (local $14 f64)
      (local $15 f64)
      (local.set $x
         (struct.get $float 0 (ref.cast (ref $float) (local.get $vx))))
      (local.set $y
         (struct.get $float 0 (ref.cast (ref $float) (local.get $vy))))
      (local.set $z
         (struct.get $float 0 (ref.cast (ref $float) (local.get $vz))))
      (local.set $7
         (i64.add
            (local.tee $4
               (i64.and
                  (i64.shr_u
                     (local.tee $3 (i64.reinterpret_f64 (local.get $y)))
                     (i64.const 52))
                  (i64.const 2047)))
         (local.tee $6
            (i64.and
               (i64.shr_u
                  (local.tee $5 (i64.reinterpret_f64 (local.get $x)))
                  (i64.const 52))
               (i64.const 2047)))))
      (local.set $8 (i64.reinterpret_f64 (local.get $z)))
      (block $label$1
         (block $label$2
            (br_if $label$2 (i64.gt_u (local.get $4) (i64.const 1993)))
            (br_if $label$2 (i64.gt_u (local.get $6) (i64.const 1993)))
            (br_if $label$2 (i64.gt_u (local.get $7) (i64.const 3016)))
            (br_if $label$2
               (i64.gt_u
                  (i64.and (local.get $8) (i64.const 0x7fe0000000000000))
                  (i64.const 0x7c90000000000000)))
            (local.set $9 (i32.const 0))
            (br_if $label$2 (i64.le_u (local.get $7) (i64.const 1076)))
            (local.set $10 (i32.const 0))
            (br $label$1))
         (local.set $8
            (i64.and (i64.shr_u (local.get $8) (i64.const 52))
               (i64.const 2047)))
         (block $cont
            (br_if $cont (i64.eq (local.get $4) (i64.const 2047)))
            (br_if $cont (i64.eq (local.get $6) (i64.const 2047)))
            (br_if $cont (i64.ne (local.get $8) (i64.const 2047)))
            (return
               (struct.new $float
                  (f64.add (f64.add (local.get $x) (local.get $z))
                     (local.get $y)))))
         (block $cont
            (br_if $cont (f64.eq (local.get $y) (f64.const 0)))
            (br_if $cont (f64.eq (local.get $x) (f64.const 0)))
            (br_if $cont (f64.ne (local.get $z) (f64.const 0)))
            (return
               (struct.new $float
                  (f64.mul (local.get $x) (local.get $y)))))
         (block $cont
            (block $then
               (br_if $then (i64.eq (local.get $6) (i64.const 2047)))
               (br_if $then (i64.eq (local.get $4) (i64.const 2047)))
               (br_if $then (f64.eq (local.get $y) (f64.const 0)))
               (br_if $then (f64.eq (local.get $x) (f64.const 0)))
               (br_if $cont (i64.ne (local.get $8) (i64.const 2047))))
            (return
               (struct.new $float
                  (f64.add (f64.mul (local.get $x) (local.get $y))
                     (local.get $z)))))
         (block $cont
            (br_if $cont (i64.lt_u (local.get $7) (i64.const 3071)))
            (return
               (struct.new $float (f64.mul (local.get $x) (local.get $y)))))
         (block $cont
            (br_if $cont (i64.gt_u (local.get $7) (i64.const 967)))
            (local.set $y
               (select
                  (f64.const 0x1p-1074)
                  (f64.const -0x1p-1074)
                  (i64.gt_s (i64.xor (local.get $3) (local.get $5))
                     (i64.const -1))))
            (block $cont2
               (br_if $cont2 (i64.lt_u (local.get $8) (i64.const 3)))
               (return
                  (struct.new $float(f64.add (local.get $y) (local.get $z)))))
            (return
               (struct.new $float
                  (f64.mul
                     (f64.add (f64.mul (local.get $z) (f64.const 0x1p54))
                        (local.get $y))
                     (f64.const 0x1p-54)))))
         (block $label$10
            (block $label$11
               (block $label$12
                  (br_if $label$12 (i64.lt_u (local.get $7) (i64.const 3017)))
                  (local.set $z
                     (select
                        (f64.mul (local.get $z) (f64.const 0x1p-53))
                        (local.get $z)
                        (i64.gt_u (local.get $8) (i64.const 53))))
                  (local.set $x
                     (select
                        (f64.mul (local.get $x) (f64.const 0x1p-53))
                        (local.get $x)
                        (local.tee $9 (i64.gt_u (local.get $6) (local.get $4)))))
                  (local.set $y
                     (select
                        (local.get $y)
                        (f64.mul (local.get $y) (f64.const 0x1p-53))
                        (local.get $9)))
                  (br $label$11))
               (br_if $label$10 (i64.lt_u (local.get $8) (i64.const 1994)))
               (block $label$13
                  (block $label$14
                     (br_if $label$14 (i64.gt_u (local.get $7) (i64.const 1129)))
                     (block $label$15
                        (br_if $label$15
                           (i64.le_u (local.get $6) (local.get $4)))
                        (local.set $x
                           (f64.mul (local.get $x) (f64.const 0x1p108)))
                        (br $label$13))
                     (local.set $y (f64.mul (local.get $y) (f64.const 0x1p108)))
                     (br $label$13))
                  (block $label$16
                     (br_if $label$16 (i64.le_u (local.get $6) (local.get $4)))
                     (local.set $x
                        (select
                           (f64.mul (local.get $x) (f64.const 0x1p-53))
                           (local.get $x)
                           (i64.gt_u (local.get $6) (i64.const 53))))
                     (br $label$13))
                  (local.set $y
                     (select
                        (f64.mul (local.get $y) (f64.const 0x1p-53))
                        (local.get $y)
                        (i64.gt_u (local.get $4) (i64.const 53)))))
               (local.set $z (f64.mul (local.get $z) (f64.const 0x1p-53))))
            (local.set $10 (i32.const 0))
            (local.set $9 (i32.const 1))
            (br $label$1))
         (block $label$17
            (block $label$18
               (br_if $label$18 (i64.lt_u (local.get $6) (i64.const 1994)))
               (local.set $y (f64.mul (local.get $y) (f64.const 0x1p53)))
               (local.set $x (f64.mul (local.get $x) (f64.const 0x1p-53)))
               (br $label$17))
            (block $label$19
               (br_if $label$19 (i64.lt_u (local.get $4) (i64.const 1994)))
               (local.set $x (f64.mul (local.get $x) (f64.const 0x1p53)))
               (local.set $y (f64.mul (local.get $y) (f64.const 0x1p-53)))
               (br $label$17))
            (local.set $z
               (select
                  (f64.mul (local.get $z) (f64.const 0x1p108))
                  (local.get $z)
                  (local.tee $10 (i64.lt_u (local.get $8) (i64.const 219)))))
            (local.set $x
               (select
                  (f64.mul (local.get $x) (f64.const 0x1p108))
                  (local.get $x)
                  (local.tee $9 (i64.gt_u (local.get $6) (local.get $4)))))
            (local.set $y
               (select
                  (local.get $y)
                  (f64.mul (local.get $y) (f64.const 0x1p108))
                  (local.get $9)))
            (local.set $9 (i32.const 0))
            (br $label$1))
         (local.set $9 (i32.const 0))
         (local.set $10 (i32.const 0)))
      (block $cont
         (br_if $cont (f64.ne (local.get $z) (f64.const 0)))
         (br_if $cont
            (i32.eqz
               (i32.or
                  (f64.eq (local.get $y) (f64.const 0))
                  (f64.eq (local.get $x) (f64.const 0)))))
         (return
            (struct.new $float
               (f64.add
                  (f64.mul (local.get $x) (local.get $y)) (local.get $z)))))
      (local.set $x
         (f64.sub
            (f64.mul
               (local.tee $12
                  (f64.sub
                     (local.get $x)
                     (local.tee $11
                        (f64.sub
                           (local.tee $11
                              (f64.mul (local.get $x) (f64.const 0x8000001)))
                        (f64.sub (local.get $11) (local.get $x))))))
               (local.tee $14
                  (f64.sub
                     (local.get $y)
                     (local.tee $13
                     (f64.sub
                        (local.tee $13
                           (f64.mul (local.get $y) (f64.const 0x8000001)))
                        (f64.sub (local.get $13) (local.get $y)))))))
            (f64.sub
               (f64.sub
                  (f64.sub
                     (local.tee $15 (f64.mul (local.get $y) (local.get $x)))
                     (f64.mul (local.get $11) (local.get $13)))
                  (f64.mul (local.get $12) (local.get $13)))
               (f64.mul (local.get $11) (local.get $14)))))
      (block $label$21
         (block $label$22
            (br_if $label$22
               (f64.ne
                  (local.tee $y (f64.add (local.get $z) (local.get $15)))
                  (f64.const 0)))
            (br_if $label$21 (f64.eq (local.get $x) (f64.const 0))))
         (block $cont
            (br_if $cont
               (f64.eq
                  (local.tee $z
                     (f64.add
                        (local.tee $11
                           (f64.add
                              (f64.sub (local.get $x)
                                 (local.tee $13
                                    (f64.sub
                                       (local.tee $z
                                          (f64.add
                                             (local.tee $11
                                                (f64.add
                                                   (f64.sub (local.get $15)
                                                      (local.tee $11
                                                         (f64.sub
                                                            (local.get $y)
                                                            (local.get $z))))
                                                   (f64.sub (local.get $z)
                                                      (f64.sub
                                                         (local.get $y)
                                                         (local.get $11)))))
                                             (local.get $x)))
                                          (local.get $11))))
                              (f64.sub (local.get $11)
                                 (f64.sub (local.get $z) (local.get $13)))))
                        (f64.sub
                           (local.tee $y
                              (f64.add
                                 (f64.sub (local.get $z)
                                    (local.tee $13
                                       (f64.sub
                                          (local.tee $x
                                             (f64.add (local.get $y)
                                                (local.get $z)))
                                          (local.get $y))))
                                 (f64.sub
                                    (local.get $y)
                                    (f64.sub (local.get $x) (local.get $13)))))
                           (local.tee $y
                              (f64.add (local.get $11) (local.get $y))))))
                  (f64.const 0)))
            (br_if $cont
               (i32.and
                  (i32.wrap_i64
                     (local.tee $4 (i64.reinterpret_f64 (local.get $y))))
                  (i32.const 1)))
            (local.set $y
               (f64.reinterpret_i64
                  (i64.add
                     (select
                        (i64.const 1)
                        (i64.const -1)
                        (i32.xor
                           (f64.lt (local.get $y) (f64.const 0))
                           (f64.gt (local.get $z) (f64.const 0))))
                     (local.get $4)))))
         (local.set $y (f64.add (local.get $x) (local.get $y)))
         (block $cont
            (br_if $cont (i32.eqz (local.get $9)))
            (return
               (struct.new $float
                  (f64.mul (local.get $y) (f64.const 0x1p53)))))
         (local.set $y
            (select
               (f64.mul (local.get $y) (f64.const 0x1p-108))
               (local.get $y)
               (local.get $10))))
      (struct.new $float (local.get $y)))

   (func (export "caml_float_compare")
      (param $x f64) (param $y f64) (result (ref eq))
      (ref.i31
         (i32.add
            (i32.sub (f64.gt (local.get $x) (local.get $y))
                     (f64.lt (local.get $x) (local.get $y)))
            (i32.sub (f64.eq (local.get $x) (local.get $x))
                     (f64.eq (local.get $y) (local.get $y))))))

   (func (export "caml_round") (param $x f64) (result f64)
      (local $y f64)
      (if (result f64) (f64.ge (local.get $x) (f64.const 0))
         (then
            (local.set $y (f64.floor (local.get $x)))
            (if (result f64)
                (f64.ge (f64.sub (local.get $x) (local.get $y)) (f64.const 0.5))
               (then (f64.add (local.get $y) (f64.const 1)))
               (else (local.get $y))))
         (else
            (local.set $y (f64.ceil (local.get $x)))
            (if (result f64)
                (f64.ge (f64.sub (local.get $y) (local.get $x)) (f64.const 0.5))
               (then (f64.sub (local.get $y) (f64.const 1)))
               (else (local.get $y))))))
)
