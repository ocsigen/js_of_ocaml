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
(@if $wasi
(@then
   (import "io" "IO_BUFFER_SIZE" (global $IO_BUFFER_SIZE i32))
   (import "libc" "memory" (memory 2))
   (import "libc" "free" (func $free (param i32)))
   (import "libc" "format_float"
      (func $format_float (param i32 i32 i32 f64) (result i32)))
   (import "libc" "strtod" (func $strtod (param i32) (param i32) (result f64)))
   (import "libc" "exp" (func $exp (param f64) (result f64)))
   (import "wasi_memory" "checked_malloc"
     (func $checked_malloc (param i32) (result i32)))
   (import "wasi_memory" "get_buffer" (func $get_buffer (result i32)))
   (import "wasi_memory" "write_string_to_memory"
      (func $write_string_to_memory (param i32 i32 (ref eq)) (result i32)))
   (import "wasi_memory" "release_memory"
      (func $release_memory (param i32 i32)))
   (import "wasi_memory" "blit_string_to_memory"
      (func $blit_string_to_memory (param i32 (ref $bytes))))
   (import "wasi_memory" "blit_memory_to_string"
      (func $blit_memory_to_string (param i32 i32) (result (ref $bytes))))
)
(@else
   (import "bindings" "format_float"
      (func $format_float
         (param i32) (param i32) (param i32) (param f64) (result externref)))
   (import "bindings" "identity"
      (func $parse_float (param externref) (result f64)))
   (import "Math" "exp" (func $exp (param f64) (result f64)))
))
   (import "fail" "caml_failwith" (func $caml_failwith (param (ref eq))))
   (import "fail" "caml_invalid_argument"
      (func $caml_invalid_argument (param (ref eq))))
   (import "ints" "lowercase_hex_table"
      (global $lowercase_hex_table (ref $chars)))
   (import "jsstring" "jsstring_of_bytes"
      (func $jsstring_of_bytes (param (ref $bytes)) (result externref)))
   (import "jsstring" "bytes_of_jsstring"
      (func $bytes_of_jsstring (param externref) (result (ref $bytes))))

   (type $float (struct (field $f f64)))
   (type $bytes (array (mut i8)))
   (type $block (array (mut (ref eq))))

   (type $chars (array i8))

   (global $infinity (ref $chars)
      (array.new_fixed $chars 8
         (@char "i") (@char "n") (@char "f") (@char"i")
         (@char "n") (@char "i") (@char "t") (@char "y")))

   (global $nan_chars (ref $chars)
      (array.new_fixed $chars 3 (@char "n") (@char "a") (@char "n")))

   (func (export "Double_val") (param $v (ref eq)) (result f64)
      (struct.get $float 0 (ref.cast (ref $float) (local.get $v))))

   (func (export "caml_hexstring_of_float")
      (param $v (ref eq)) (param $vprec (ref eq)) (param $vstyle (ref eq))
      (result (ref eq))
      (local $b i64) (local $prec i32) (local $style i32)
      (local $sign i32) (local $exp i32) (local $m i64)
      (local $i i32) (local $j i32) (local $d i32) (local $txt (ref $chars))
      (local $len i32) (local $s (ref $bytes))
      (local $unit i64) (local $half i64) (local $mask i64) (local $frac i64)
      (local.set $prec (i31.get_s (ref.cast (ref i31) (local.get $vprec))))
      (local.set $style (i31.get_s (ref.cast (ref i31) (local.get $vstyle))))
      (local.set $b
         (i64.reinterpret_f64
            (struct.get $float 0 (ref.cast (ref $float) (local.get $v)))))
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
                           (global.get $nan_chars))))
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
            (if (i32.lt_u (local.get $prec) (i32.const 13))
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
      (local.get $s))

   (@string $format_error "format_float: bad format")

   (func $parse_format
      (param $s (ref $bytes)) (result i32 i32 i32 i32)
      (local $i i32) (local $len i32) (local $c i32)
      (local $sign_style i32) (local $precision i32)
      (local $conversion i32) (local $uppercase i32)
      (local.set $len (array.len (local.get $s)))
      (local.set $i (i32.const 1))
      (block $return
         (block $bad_format
            (br_if $bad_format (i32.lt_u (local.get $len) (i32.const 2)))
            (br_if $bad_format
               (i32.ne (array.get_u $bytes (local.get $s) (i32.const 0))
                       (@char "%")))
            (local.set $c (array.get_u $bytes (local.get $s) (i32.const 1)))
            (if (i32.eq (local.get $c) (@char "+"))
               (then
                  (local.set $sign_style (i32.const 1))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))))
            (if (i32.eq (local.get $c) (@char " "))
               (then
                  (local.set $sign_style (i32.const 2))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))))
            (br_if $bad_format (i32.eq (local.get $i) (local.get $len)))
            (if (i32.eq (array.get_u $bytes (local.get $s) (local.get $i))
                   (@char "."))
               (then
                  (loop $precision
                     (local.set $i (i32.add (local.get $i) (i32.const 1)))
                     (br_if $bad_format
                        (i32.eq (local.get $i) (local.get $len)))
                     (local.set $c
                        (array.get_u $bytes (local.get $s) (local.get $i)))
                     (if (i32.lt_u (i32.sub (local.get $c) (@char "0"))
                                   (i32.const 10))
                        (then
                           (local.set $precision
                              (i32.add
                                 (i32.mul (local.get $precision) (i32.const 10))
                                 (i32.sub (local.get $c) (@char "0"))))
                           (br $precision)))))
               (else
                  ;; No precision: [$c] still holds the char read for the "."
                  ;; test (a sign flag like "+"/" " when one was consumed), not
                  ;; the conversion letter. Load it so the checks below see the
                  ;; real conversion, otherwise "%+f"/"% f" wrongly raise.
                  (local.set $c (array.get_u $bytes (local.get $s) (local.get $i)))
                  (local.set $precision (i32.const 6))))
            (br_if $bad_format
              (i32.ne (i32.add (local.get $i) (i32.const 1)) (local.get $len)))
            (local.set $uppercase (i32.lt_s (local.get $c) (@char "a")))
            (local.set $conversion
               (i32.sub (i32.and (local.get $c) (i32.const 0xdf)) (@char "E")))
            (br_if $return (i32.le_u (local.get $conversion) (i32.const 2))))
         (call $caml_invalid_argument (global.get $format_error)))
      (local.get $sign_style)
      (local.get $precision)
      (local.get $conversion)
      (local.get $uppercase))

   (global $inf_chars (ref $chars)
      (array.new_fixed $chars 3 (@char "i") (@char "n") (@char "f")))

(@if $wasi
(@then
   (func (export "caml_format_float")
      (param $vfmt (ref eq)) (param $arg (ref eq)) (result (ref eq))
      (local $fmt (ref $bytes)) (local $res (ref $bytes))
      (local $d f64)
      (local $buffer i32) (local $out_buffer i32)
      (local $fmt_len i32) (local $avail i32) (local $len i32)
      (local.set $fmt (ref.cast (ref $bytes) (local.get $vfmt)))
      (local.set $d
         (struct.get $float 0 (ref.cast (ref $float) (local.get $arg))))
      (local.set $buffer (call $get_buffer))
      (local.set $fmt_len (array.len (local.get $fmt)))
      (call $blit_string_to_memory (local.get $buffer) (local.get $fmt))
      (i32.store8
         (i32.add (local.get $buffer) (local.get $fmt_len)) (i32.const 0))
      (local.set $out_buffer
         (i32.add (local.get $buffer)
            (i32.add (local.get $fmt_len) (i32.const 1))))
      (local.set $avail
         (i32.sub (global.get $IO_BUFFER_SIZE) (local.get $fmt_len)))
      (local.set $len
         (call $format_float
            (local.get $out_buffer) (local.get $avail)
            (local.get $buffer) (local.get $d)))
      (if (i32.ge_u (local.get $len) (local.get $avail))
         (then
            (local.set $out_buffer
               (call $checked_malloc (i32.add (local.get $len) (i32.const 1))))
            (drop
               (call $format_float
                  (local.get $out_buffer)
                  (i32.add (local.get $len) (i32.const 1))
                  (local.get $buffer) (local.get $d)))))
      (local.set $res
         (call $blit_memory_to_string (local.get $out_buffer) (local.get $len)))
      (if (i32.ge_u (local.get $len) (local.get $avail))
         (then
            (call $free (local.get $out_buffer))))
      (local.get $res)
  )
)
(@else
   (func (export "caml_format_float")
      (param $vfmt (ref eq)) (param $arg (ref eq)) (result (ref eq))
      (local $f f64) (local $b i64)
      (local $sign_style i32) (local $precision i32)
      (local $conversion i32) (local $uppercase i32)
      (local $negative i32)
      (local $exp i32) (local $m i64)
      (local $i i32) (local $len i32) (local $c i32)
      (local $s (ref $bytes)) (local $txt (ref $chars))
      (local $num externref)
      (local.set $f (struct.get $float 0 (ref.cast (ref $float) (local.get $arg))))
      (local.set $b (i64.reinterpret_f64 (local.get $f)))
      (call $parse_format (ref.cast (ref $bytes) (local.get $vfmt)))
      (local.set $uppercase)
      (local.set $conversion)
      (local.set $precision)
      (local.set $sign_style)
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
                           (global.get $inf_chars))
                        (else
                           (local.set $negative (i32.const 0))
                           (local.set $i
                              (i32.ne (local.get $sign_style) (i32.const 0)))
                           (global.get $nan_chars))))
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
               (if (i32.lt_u (i32.sub (local.get $c) (@char "a"))
                             (i32.const 26))
                  (then
                     (array.set $bytes (local.get $s) (local.get $i)
                        (i32.sub (local.get $c) (i32.const 32)))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br_if $uppercase (i32.lt_u (local.get $i) (local.get $len))))))
      (local.get $s))
))

   (func $caml_float_of_hex
      (param $err_msg (ref eq)) (param $s (ref $bytes)) (param $i i32)
      (result f64)
      (local $len i32) (local $c i32) (local $d i32) (local $m i64)
      (local $f f64) (local $negative i32)
      (local $dec_point i32) (local $exp i32) (local $adj i32)
      (local $n_bits i32) (local $m_bits i32) (local $x_bits i32)
      (local.set $len (array.len (local.get $s)))
      (local.set $dec_point (i32.const -1))
      (block $error
         (loop $parse
            (if (i32.lt_u (local.get $i) (local.get $len))
               (then
                  (local.set $c
                     (array.get_u $bytes (local.get $s) (local.get $i)))
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
                           (array.get_u $bytes (local.get $s) (local.get $i)))
                        (local.set $i (i32.add (local.get $i) (i32.const 1)))
                        ;; an optional sign (only one)
                        (if (i32.or (i32.eq (local.get $c) (@char "-"))
                               (i32.eq (local.get $c) (@char "+")))
                           (then
                              (local.set $negative
                                 (i32.eq (local.get $c) (@char "-")))
                              (br_if $error
                                 (i32.eq (local.get $i) (local.get $len)))
                              (local.set $c
                                 (array.get_u $bytes
                                    (local.get $s) (local.get $i)))
                              (local.set $i
                                 (i32.add (local.get $i) (i32.const 1)))))
                        (block $overflow
                           (loop $parse_exponent
                              (br_if $error
                                 (i32.ge_u (i32.sub (local.get $c) (@char "0"))
                                           (i32.const 10)))
                              (local.set $d
                                 (i32.sub (local.get $c) (@char "0")))
                              (local.set $exp
                                 (i32.add
                                    (i32.mul (local.get $exp) (i32.const 10))
                                    (local.get $d)))
                              (br_if $overflow
                                 (i32.lt_u (local.get $exp) (local.get $d)))
                              ;; Any exponent this large saturates the double
                              ;; to inf/0; stop accumulating before [exp * 10]
                              ;; can wrap around 2^32 and slip back into range.
                              (br_if $overflow
                                 (i32.ge_u (local.get $exp)
                                    (i32.const 0x10000000)))
                              (if (i32.ne (local.get $i) (local.get $len))
                                 (then
                                    (local.set $c
                                       (array.get_u $bytes
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
                  (if (i32.lt_u (i32.sub (local.get $c) (@char "0"))
                                (i32.const 10))
                     (then
                        (local.set $d (i32.sub (local.get $c) (@char "0"))))
                  (else (if (i32.lt_u (i32.sub (local.get $c) (@char "a"))
                                      (i32.const 6))
                     (then
                        (local.set $d (i32.sub (local.get $c) (i32.const 87))))
                  (else (if (i32.lt_u (i32.sub (local.get $c) (@char "A"))
                                      (i32.const 6))
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
         ;; If the result is subnormal, m would be rounded by the conversion
         ;; below and then again by ldexp. To avoid this, round m to odd at
         ;; two bits below the subnormal precision (the exponent of these two
         ;; extra bits is DBL_MIN_EXP - DBL_MANT_DIG - 2 = -1076). If the
         ;; result is subnormal, the conversion is then exact or a tie, and
         ;; ties round to even, which ldexp then scales exactly. Otherwise,
         ;; the conversion rounds once (round to odd keeps the information
         ;; needed) and ldexp is exact.
         (if (i32.lt_s (local.get $exp) (i32.const -1076))
            (then
               (local.set $adj (i32.sub (i32.const -1076) (local.get $exp)))
               (local.set $m
                  (if (result i64) (i32.lt_u (local.get $adj) (i32.const 60))
                     (then
                        (i64.or
                           (i64.shr_u (local.get $m)
                              (i64.extend_i32_u (local.get $adj)))
                           (i64.extend_i32_u
                              (i64.ne
                                 (i64.and (local.get $m)
                                    (i64.sub
                                       (i64.shl (i64.const 1)
                                          (i64.extend_i32_u (local.get $adj)))
                                       (i64.const 1)))
                                 (i64.const 0)))))
                     (else
                        (i64.extend_i32_u
                           (i64.ne (local.get $m) (i64.const 0))))))
               (local.set $exp (i32.const -1076))))
         ;; Convert the mantissa to FP (m has 60 bits at most)
         (local.set $f (f64.convert_i64_s (local.get $m)))
         (if (local.get $exp)
            (then (local.set $f (call $ldexp (local.get $f) (local.get $exp)))))
         (return (local.get $f)))
      (call $caml_failwith (local.get $err_msg))
      (f64.const 0))

   (func $on_whitespace (param $s (ref $bytes)) (param $i i32) (result i32)
      (local $c i32)
      (local.set $c (array.get_u $bytes (local.get $s) (local.get $i)))
      (i32.or (i32.eq (local.get $c) (@char " "))
         (i32.lt_u (i32.sub (local.get $c) (i32.const 9)) (i32.const 5))))

   (func $caml_parse_float (export "caml_parse_float")
      (param $err_msg (ref eq)) (param $v (ref eq)) (result (ref eq))
      (local $s (ref $bytes)) (local $len i32) (local $i i32) (local $j i32)
      (local $s' (ref $bytes))
      (local $negative i32) (local $c i32)
      (local $f f64)
      (local $buffer i32) (local $buf i32) (local $start i32)
      (local.set $s (ref.cast (ref $bytes) (local.get $v)))
      (local.set $len (array.len (local.get $s)))
      (loop $count
         (if (i32.lt_u (local.get $i) (local.get $len))
            (then
               (if (i32.eq (@char "_")
                      (array.get_u $bytes (local.get $s) (local.get $i)))
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
                        (array.get_u $bytes (local.get $s) (local.get $i)))
                     (local.set $i (i32.add (local.get $i) (i32.const 1)))
                     (if (i32.ne (local.get $c) (@char "_"))
                        (then
                           (array.set $bytes (local.get $s')
                              (local.get $j) (local.get $c))
                           (local.set $j
                              (i32.add (local.get $j) (i32.const 1)))))
                     (br $copy))))
            (local.set $len (array.len (local.get $s')))
            (local.set $s (local.get $s'))))
      (local.set $i (i32.const 0))
      (loop $skip_spaces
         (if (i32.lt_u (local.get $i) (local.get $len))
            (then
               (if (call $on_whitespace (local.get $s) (local.get $i))
                  (then
                     (local.set $i (i32.add (local.get $i) (i32.const 1)))
                     (br $skip_spaces))))))
      (local.set $start (local.get $i))
      (block $error
         (br_if $error (i32.eq (local.get $i) (local.get $len)))
         (br_if $error
            (call $on_whitespace
               (local.get $s) (i32.sub (local.get $len) (i32.const 1))))
         (local.set $c (array.get_u $bytes (local.get $s) (local.get $i)))
         (if (i32.eq (local.get $c) (@char "-"))
            (then
               (local.set $negative (i32.const 1))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))))
         (if (i32.eq (local.get $c) (@char "+"))
            (then
               (local.set $i (i32.add (local.get $i) (i32.const 1)))))
         (if (i32.lt_u (i32.add (local.get $i) (i32.const 2)) (local.get $len))
            (then
               (if (i32.eq (array.get_u $bytes (local.get $s) (local.get $i))
                           (@char "0"))
                  (then
                     (if (i32.eq (i32.and
                                    (array.get_u $bytes (local.get $s)
                                       (i32.add (local.get $i) (i32.const 1)))
                                    (i32.const 0xdf))
                                 (@char "X"))
                        (then
                           (local.set $f
                              (call $caml_float_of_hex (local.get $err_msg)
                                 (local.get $s)
                                 (i32.add (local.get $i) (i32.const 2))))
                           (if (local.get $negative)
                              (then (local.set $f (f64.neg (local.get $f)))))
                           (return (struct.new $float (local.get $f)))))))))
         (if (i32.eq (i32.add (local.get $i) (i32.const 3)) (local.get $len))
            (then
               (local.set $c (array.get_u $bytes (local.get $s) (local.get $i)))
               (if (i32.eq (i32.and (local.get $c) (i32.const 0xdf))
                           (@char "N")) (then
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (local.set $c
                     (array.get_u $bytes (local.get $s) (local.get $i)))
                  (if (i32.eq (i32.and (local.get $c) (i32.const 0xdf))
                              (@char "A")) (then
                     (local.set $i (i32.add (local.get $i) (i32.const 1)))
                     (local.set $c
                        (array.get_u $bytes (local.get $s) (local.get $i)))
                     (if (i32.eq (i32.and (local.get $c) (i32.const 0xdf))
                                 (@char "N"))
                        (then
                           (return
                              (struct.new $float (f64.const nan))))))))
               (else
               (if (i32.eq (i32.and (local.get $c) (i32.const 0xdf))
                           (@char "I")) (then
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (local.set $c
                     (array.get_u $bytes (local.get $s) (local.get $i)))
                  (if (i32.eq (i32.and (local.get $c) (i32.const 0xdf))
                              (@char "N")) (then
                     (local.set $i (i32.add (local.get $i) (i32.const 1)))
                     (local.set $c
                        (array.get_u $bytes (local.get $s) (local.get $i)))
                     (if (i32.eq (i32.and (local.get $c) (i32.const 0xdf))
                                 (@char "F"))
                        (then
                           (return
                              (struct.new $float
                                 (select
                                    (f64.const -inf)
                                    (f64.const inf)
                                    (local.get $negative))))))))))))))
         (if (i32.eq (i32.add (local.get $i) (i32.const 8)) (local.get $len))
            (then
               (local.set $c (array.get_u $bytes (local.get $s) (local.get $i)))
               (if (i32.eq (i32.and (local.get $c) (i32.const 0xdf))
                           (@char "I")) (then
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (local.set $c
                     (array.get_u $bytes (local.get $s) (local.get $i)))
                  (if (i32.eq (i32.and (local.get $c) (i32.const 0xdf))
                              (@char "N")) (then
                     (local.set $i (i32.add (local.get $i) (i32.const 1)))
                     (local.set $c
                        (array.get_u $bytes (local.get $s) (local.get $i)))
                     (if (i32.eq (i32.and (local.get $c) (i32.const 0xdf))
                                 (@char "F")) (then
                        (local.set $i (i32.add (local.get $i) (i32.const 1)))
                        (local.set $c
                           (array.get_u $bytes (local.get $s) (local.get $i)))
                        (if (i32.eq (i32.and (local.get $c) (i32.const 0xdf))
                                    (@char "I")) (then
                           (local.set $i (i32.add (local.get $i) (i32.const 1)))
                           (local.set $c
                              (array.get_u $bytes
                                 (local.get $s) (local.get $i)))
                           (if (i32.eq (i32.and (local.get $c) (i32.const 0xdf))
                                       (@char "N")) (then
                              (local.set $i
                                 (i32.add (local.get $i) (i32.const 1)))
                              (local.set $c
                                 (array.get_u $bytes
                                     (local.get $s) (local.get $i)))
                              (if (i32.eq
                                     (i32.and (local.get $c) (i32.const 0xdf))
                                     (@char "I")) (then
                                 (local.set $i
                                    (i32.add (local.get $i) (i32.const 1)))
                                 (local.set $c
                                    (array.get_u $bytes
                                       (local.get $s) (local.get $i)))
                                 (if (i32.eq
                                        (i32.and (local.get $c) (i32.const 0xdf))
                                             (@char "T")) (then
                                    (local.set $i
                                       (i32.add (local.get $i) (i32.const 1)))
                                    (local.set $c
                                       (array.get_u $bytes
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
(@if $wasi
(@then
         ;; wasi-libc's strtod returns +0 for some negative values that round
         ;; to the smallest subnormal or to zero: for a negative decimal
         ;; literal, parse its absolute value instead
         (local.set $j (i32.const 0))
         (if (i32.and
                (i32.eq (array.get_u $bytes (local.get $s) (local.get $start))
                   (@char "-"))
                (i32.lt_u (i32.add (local.get $start) (i32.const 1))
                   (local.get $len)))
            (then
               (local.set $c
                  (array.get_u $bytes (local.get $s)
                     (i32.add (local.get $start) (i32.const 1))))
               (if (i32.or
                      (i32.lt_u (i32.sub (local.get $c) (@char "0"))
                         (i32.const 10))
                      (i32.eq (local.get $c) (@char ".")))
                  (then
                     (local.set $j (i32.add (local.get $start) (i32.const 1)))
                     (local.set $len (i32.sub (local.get $len) (local.get $j)))
                     (local.set $s'
                        (array.new $bytes (i32.const 0) (local.get $len)))
                     (array.copy $bytes $bytes
                        (local.get $s') (i32.const 0)
                        (local.get $s) (local.get $j) (local.get $len))
                     (local.set $s (local.get $s'))))))
         (local.set $buffer (call $get_buffer))
         (local.set $buf
            (call $write_string_to_memory
               (i32.add (local.get $buffer) (i32.const 4))
               (global.get $IO_BUFFER_SIZE)
               (local.get $s)))
         (local.set $f (call $strtod (local.get $buf) (local.get $buffer)))
         (call $release_memory (i32.add (local.get $buffer) (i32.const 4))
            (local.get $buf))
         (br_if $error
            (i32.ne (i32.load (local.get $buffer))
              (i32.add (local.get $buf) (local.get $len))))
         (if (local.get $j) (then (local.set $f (f64.neg (local.get $f)))))
)
(@else
         ;; The JS number coercion below accepts things OCaml does not,
         ;; e.g. binary/octal literals ("0b101", "0o17"); only a plain
         ;; decimal float can reach this point, so reject any character
         ;; that cannot appear in one. Start after the leading spaces already
         ;; skipped above.
         (local.set $i (local.get $start))
         (loop $check_decimal
            (if (i32.lt_u (local.get $i) (local.get $len))
               (then
                  (local.set $c
                     (array.get_u $bytes (local.get $s) (local.get $i)))
                  (br_if $error
                     (i32.eqz
                        (i32.or
                           (i32.lt_u (i32.sub (local.get $c) (@char "0"))
                                     (i32.const 10))
                           (i32.or
                              (i32.eq (local.get $c) (@char "."))
                              (i32.or
                                 (i32.eq
                                    (i32.and (local.get $c) (i32.const 0xdf))
                                    (@char "E"))
                                 (i32.or
                                    (i32.eq (local.get $c) (@char "+"))
                                    (i32.eq (local.get $c) (@char "-"))))))))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (br $check_decimal))))
         (local.set $f
            (call $parse_float (call $jsstring_of_bytes (local.get $s))))
         (br_if $error (f64.ne (local.get $f) (local.get $f)))
))
         (return (struct.new $float (local.get $f))))
      (call $caml_failwith (local.get $err_msg))
      (return (ref.i31 (i32.const 0))))

   (@string $float_of_string "float_of_string")

   (func (export "caml_float_of_string") (param $s (ref eq)) (result (ref eq))
      (return_call $caml_parse_float
         (global.get $float_of_string) (local.get $s)))

   (func (export "caml_nextafter_float")
      (param $x f64) (param $y f64) (result f64)
      (local $i i64) (local $j i64)
      (if (f64.ne (local.get $x) (local.get $x)) (then (return (local.get $x))))
      (if (f64.ne (local.get $y) (local.get $y)) (then (return (local.get $y))))
      (if (f64.eq (local.get $x) (local.get $y))
         (then (return (local.get $y))))
      (if (result f64) (f64.eq (local.get $x) (f64.const 0))
         (then
            (if (result f64) (f64.ge (local.get $y) (f64.const 0))
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

   (func (export "caml_classify_float") (param $x f64) (result i32)
      (local $a f64)
      (local.set $a (f64.abs (local.get $x)))
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
                     (else (i32.const 4)))))))) ;; nan

   (func (export "caml_modf_float") (param $v (ref eq)) (result (ref eq))
      (local $x f64) (local $a f64) (local $i f64) (local $f f64)
      (local.set $x (struct.get $float 0 (ref.cast (ref $float) (local.get $v))))
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

   ;; a * b mod m, for a, b < m < 2^53. The quotient is estimated with
   ;; floating-point arithmetic; it is off by at most a few units, so the
   ;; remainder computed with wrapping 64-bit arithmetic is exact (its
   ;; absolute value is below 2^56) and only needs a small adjustment.
   (func $mulmod (param $a i64) (param $b i64) (param $m i64) (result i64)
      (local $r i64)
      (local.set $r
         (i64.sub (i64.mul (local.get $a) (local.get $b))
            (i64.mul (local.get $m)
               (i64.trunc_f64_u
                  (f64.div
                     (f64.mul (f64.convert_i64_u (local.get $a))
                        (f64.convert_i64_u (local.get $b)))
                     (f64.convert_i64_u (local.get $m)))))))
      (loop $neg
         (if (i64.lt_s (local.get $r) (i64.const 0))
            (then
               (local.set $r (i64.add (local.get $r) (local.get $m)))
               (br $neg))))
      (loop $big
         (if (i64.ge_s (local.get $r) (local.get $m))
            (then
               (local.set $r (i64.sub (local.get $r) (local.get $m)))
               (br $big))))
      (local.get $r))

   ;; 2^d mod m, for d > 0 and 1 < m < 2^53 (binary exponentiation)
   (func $pow2mod (param $d i32) (param $m i64) (result i64)
      (local $r i64) (local $i i32)
      (local.set $r (i64.const 1))
      (local.set $i (i32.sub (i32.const 31) (i32.clz (local.get $d))))
      (loop $bit
         (local.set $r (call $mulmod (local.get $r) (local.get $r) (local.get $m)))
         (if (i32.and (i32.shr_u (local.get $d) (local.get $i)) (i32.const 1))
            (then
               (local.set $r (i64.shl (local.get $r) (i64.const 1)))
               (if (i64.ge_u (local.get $r) (local.get $m))
                  (then
                     (local.set $r (i64.sub (local.get $r) (local.get $m)))))))
         (local.set $i (i32.sub (local.get $i) (i32.const 1)))
         (br_if $bit (i32.ge_s (local.get $i) (i32.const 0))))
      (local.get $r))

   ;; The remainder of x / y, rounded toward zero (as C's fmod): with
   ;; x = mx * 2^ex and y = my * 2^ey (mx and my integers, ex >= ey when
   ;; |x| > |y|), it is (mx * 2^(ex - ey) mod my) * 2^ey, with the sign
   ;; of x. This is computed exactly with integer arithmetic, up to 63
   ;; bits at a time, or with modular exponentiation.
   (func (export "caml_fmod_f64") (param $x f64) (param $y f64) (result f64)
      (local $ax i64) (local $ay i64) (local $mx i64) (local $my i64)
      (local $ex i32) (local $ey i32) (local $d i32) (local $s i32)
      (local.set $ax
         (i64.and (i64.reinterpret_f64 (local.get $x))
            (i64.const 0x7fffffffffffffff)))
      (local.set $ay
         (i64.and (i64.reinterpret_f64 (local.get $y))
            (i64.const 0x7fffffffffffffff)))
      ;; x infinite or nan, y zero or nan
      (if (i32.or
             (i64.ge_u (local.get $ax) (i64.const 0x7ff0000000000000))
             (i32.or (i64.eqz (local.get $ay))
                (i64.gt_u (local.get $ay) (i64.const 0x7ff0000000000000))))
         (then (return (f64.const nan))))
      (if (i64.le_u (local.get $ax) (local.get $ay))
         (then
            (return
               (select (f64.mul (f64.const 0) (local.get $x)) (local.get $x)
                  (i64.eq (local.get $ax) (local.get $ay))))))
      (local.set $ex (i32.wrap_i64 (i64.shr_u (local.get $ax) (i64.const 52))))
      (local.set $mx (i64.and (local.get $ax) (i64.const 0xfffffffffffff)))
      (if (local.get $ex)
         (then
            (local.set $mx
               (i64.or (local.get $mx) (i64.const 0x10000000000000)))
            (local.set $ex (i32.sub (local.get $ex) (i32.const 1075))))
         (else
            (local.set $ex (i32.const -1074))))
      (local.set $ey (i32.wrap_i64 (i64.shr_u (local.get $ay) (i64.const 52))))
      (local.set $my (i64.and (local.get $ay) (i64.const 0xfffffffffffff)))
      (if (local.get $ey)
         (then
            (local.set $my
               (i64.or (local.get $my) (i64.const 0x10000000000000)))
            (local.set $ey (i32.sub (local.get $ey) (i32.const 1075))))
         (else
            (local.set $ey (i32.const -1074))))
      (local.set $d (i32.sub (local.get $ex) (local.get $ey)))
      ;; Remove the trailing zeros of my (as long as ey <= ex), so that
      ;; more bits are processed at each step
      (local.set $s (i32.wrap_i64 (i64.ctz (local.get $my))))
      (if (i32.gt_s (local.get $s) (local.get $d))
         (then (local.set $s (local.get $d))))
      (local.set $my
         (i64.shr_u (local.get $my) (i64.extend_i32_u (local.get $s))))
      (local.set $ey (i32.add (local.get $ey) (local.get $s)))
      (local.set $d (i32.sub (local.get $d) (local.get $s)))
      (local.set $mx (i64.rem_u (local.get $mx) (local.get $my)))
      ;; as mx < my, mx * 2^s does not overflow for s <= clz(my)
      (local.set $s (i32.wrap_i64 (i64.clz (local.get $my))))
      ;; When many steps would be needed (more than 20: large x / y and y
      ;; with many significant bits), use mx * (2^d mod my) mod my instead
      (if (i32.and (i64.ne (local.get $mx) (i64.const 0))
             (i32.gt_s (local.get $d)
                (i32.mul (local.get $s) (i32.const 20))))
         (then
            (local.set $mx
               (call $mulmod (local.get $mx)
                  (call $pow2mod (local.get $d) (local.get $my))
                  (local.get $my)))
            (local.set $d (i32.const 0))))
      (loop $reduce
         (if (i32.and (i32.gt_s (local.get $d) (i32.const 0))
                (i64.ne (local.get $mx) (i64.const 0)))
            (then
               (if (i32.lt_s (local.get $d) (local.get $s))
                  (then (local.set $s (local.get $d))))
               (local.set $mx
                  (i64.rem_u
                     (i64.shl (local.get $mx) (i64.extend_i32_u (local.get $s)))
                     (local.get $my)))
               (local.set $d (i32.sub (local.get $d) (local.get $s)))
               (br $reduce))))
      ;; mx * 2^ey is exact: mx < 2^53 and ey >= -1074
      (f64.copysign
         (f64.mul (f64.convert_i64_u (local.get $mx))
            (f64.reinterpret_i64
               (if (result i64) (i32.ge_s (local.get $ey) (i32.const -1022))
                  (then
                     (i64.shl
                        (i64.extend_i32_u
                           (i32.add (local.get $ey) (i32.const 1023)))
                        (i64.const 52)))
                  (else
                     (i64.shl (i64.const 1)
                        (i64.extend_i32_u
                           (i32.add (local.get $ey) (i32.const 1074))))))))
         (local.get $x)))

   (func $ldexp (export "caml_ldexp_float")
      (param $x f64) (param $n i32) (result f64)
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

   (func $frexp (param $x f64) (result f64 i32)
      (local $y i64)
      (local $e i32)
      (local $r_0 f64)
      (local $r_1 i32)
      (local.set $y (i64.reinterpret_f64 (local.get $x)))
      (local.set $e
         (i32.and (i32.const 0x7ff)
            (i32.wrap_i64 (i64.shr_u (local.get $y) (i64.const 52)))))
      (if (i32.eqz (local.get $e))
         (then
            (if (f64.ne (local.get $x) (f64.const 0))
               (then
                  (call $frexp (f64.mul (local.get $x) (f64.const 0x1p64)))
                  (local.set $r_1)
                  (local.set $r_0)
                  (return
                     (local.get $r_0)
                     (i32.sub (local.get $r_1)
                        (i32.const 64))))
               (else
                  (return (local.get $x) (i32.const 0)))))
         (else
            (if (i32.eq (local.get $e) (i32.const 0x7ff))
               (then
                  (return (local.get $x) (i32.const 0))))))
      (f64.reinterpret_i64
         (i64.or (i64.and (local.get $y) (i64.const 0x800fffffffffffff))
            (i64.const 0x3fe0000000000000)))
      (i32.sub (local.get $e) (i32.const 0x3fe)))

   (func (export "caml_frexp_float") (param $v (ref eq)) (result (ref eq))
      (local $r_0 f64)
      (local $r_1 i32)
      (call $frexp
         (struct.get $float 0 (ref.cast (ref $float) (local.get $v))))
      (local.set $r_1)
      (local.set $r_0)
      (array.new_fixed $block 3 (ref.i31 (i32.const 0))
         (struct.new $float (local.get $r_0))
         (ref.i31 (local.get $r_1))))

   ;; erf and erfc are ported from FreeBSD's msun (s_erf.c), through musl:
   ;;
   ;; Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
   ;;
   ;; Developed at SunPro, a Sun Microsystems, Inc. business.
   ;; Permission to use, copy, modify, and distribute this
   ;; software is freely granted, provided that this notice
   ;; is preserved.
   ;;
   ;; The JavaScript runtime (ieee_754.js) performs the same operations in
   ;; the same order.

   ;; (erf(x) - x) / x, for |x| < 0.84375, as a function of z = x * x
   (func $erf_ratio (param $z f64) (result f64)
      (local $r f64) (local $s f64)
      ;; r = pp0 + z * (pp1 + z * (pp2 + z * (pp3 + z * pp4)))
      (local.set $r (f64.const -2.37630166566501626084e-5))
      (local.set $r
         (f64.add (f64.const -5.77027029648944159157e-3)
            (f64.mul (local.get $z) (local.get $r))))
      (local.set $r
         (f64.add (f64.const -2.84817495755985104766e-2)
            (f64.mul (local.get $z) (local.get $r))))
      (local.set $r
         (f64.add (f64.const -3.2504210724700149937e-1)
            (f64.mul (local.get $z) (local.get $r))))
      (local.set $r
         (f64.add (f64.const 1.28379167095512558561e-1)
            (f64.mul (local.get $z) (local.get $r))))
      ;; s = 1 + z * (qq1 + z * (qq2 + z * (qq3 + z * (qq4 + z * qq5))))
      (local.set $s (f64.const -3.9602282787753681232e-6))
      (local.set $s
         (f64.add (f64.const 1.32494738004321644526e-4)
            (f64.mul (local.get $z) (local.get $s))))
      (local.set $s
         (f64.add (f64.const 5.08130628187576562776e-3)
            (f64.mul (local.get $z) (local.get $s))))
      (local.set $s
         (f64.add (f64.const 6.50222499887672944485e-2)
            (f64.mul (local.get $z) (local.get $s))))
      (local.set $s
         (f64.add (f64.const 3.97917223959155352819e-1)
            (f64.mul (local.get $z) (local.get $s))))
      (local.set $s
         (f64.add (f64.const 1) (f64.mul (local.get $z) (local.get $s))))
      (f64.div (local.get $r) (local.get $s)))

   ;; erfc(x), for 0.84375 <= x < 28
   (func $erfc_abs (param $x f64) (result f64)
      (local $s f64) (local $r f64) (local $q f64) (local $z f64)
      (if (f64.lt (local.get $x) (f64.const 1.25))
         (then
            ;; erf(1 + s) - erx
            (local.set $s (f64.sub (local.get $x) (f64.const 1)))
            ;; r = pa0 + s * (pa1 + ... + s * pa6)
            (local.set $r (f64.const -2.166375594868790843e-3))
            (local.set $r
               (f64.add (f64.const 3.54783043256182359371e-2)
                  (f64.mul (local.get $s) (local.get $r))))
            (local.set $r
               (f64.add (f64.const -1.10894694282396677476e-1)
                  (f64.mul (local.get $s) (local.get $r))))
            (local.set $r
               (f64.add (f64.const 3.18346619901161753674e-1)
                  (f64.mul (local.get $s) (local.get $r))))
            (local.set $r
               (f64.add (f64.const -3.72207876035701323847e-1)
                  (f64.mul (local.get $s) (local.get $r))))
            (local.set $r
               (f64.add (f64.const 4.14856118683748331666e-1)
                  (f64.mul (local.get $s) (local.get $r))))
            (local.set $r
               (f64.add (f64.const -2.36211856075265944077e-3)
                  (f64.mul (local.get $s) (local.get $r))))
            ;; q = 1 + s * (qa1 + ... + s * qa6)
            (local.set $q (f64.const 1.1984499846799107417e-2))
            (local.set $q
               (f64.add (f64.const 1.36370839120290507362e-2)
                  (f64.mul (local.get $s) (local.get $q))))
            (local.set $q
               (f64.add (f64.const 1.26171219808761642112e-1)
                  (f64.mul (local.get $s) (local.get $q))))
            (local.set $q
               (f64.add (f64.const 7.18286544141962662868e-2)
                  (f64.mul (local.get $s) (local.get $q))))
            (local.set $q
               (f64.add (f64.const 5.40397917702171048937e-1)
                  (f64.mul (local.get $s) (local.get $q))))
            (local.set $q
               (f64.add (f64.const 1.06420880400844228286e-1)
                  (f64.mul (local.get $s) (local.get $q))))
            (local.set $q
               (f64.add (f64.const 1) (f64.mul (local.get $s) (local.get $q))))
            ;; 1 - erx - r / q
            (return
               (f64.sub
                  (f64.sub (f64.const 1) (f64.const 8.45062911510467529297e-1))
                  (f64.div (local.get $r) (local.get $q))))))
      ;; erfc(x) = exp(-x * x - 0.5625 + r / q) / x
      (local.set $s
         (f64.div (f64.const 1) (f64.mul (local.get $x) (local.get $x))))
      (if (f64.lt (local.get $x) (f64.const 2.8571414947509766))
         (then
            ;; x < 1 / 0.35
            ;; r = ra0 + s * (ra1 + ... + s * ra7)
            (local.set $r (f64.const -9.81432934416914548592))
            (local.set $r
               (f64.add (f64.const -8.12874355063065934246e1)
                  (f64.mul (local.get $s) (local.get $r))))
            (local.set $r
               (f64.add (f64.const -1.84605092906711035994e2)
                  (f64.mul (local.get $s) (local.get $r))))
            (local.set $r
               (f64.add (f64.const -1.62396669462573470355e2)
                  (f64.mul (local.get $s) (local.get $r))))
            (local.set $r
               (f64.add (f64.const -6.23753324503260060396e1)
                  (f64.mul (local.get $s) (local.get $r))))
            (local.set $r
               (f64.add (f64.const -1.05586262253232909814e1)
                  (f64.mul (local.get $s) (local.get $r))))
            (local.set $r
               (f64.add (f64.const -6.93858572707181764372e-1)
                  (f64.mul (local.get $s) (local.get $r))))
            (local.set $r
               (f64.add (f64.const -9.86494403484714822705e-3)
                  (f64.mul (local.get $s) (local.get $r))))
            ;; q = 1 + s * (sa1 + ... + s * sa8)
            (local.set $q (f64.const -6.04244152148580987438e-2))
            (local.set $q
               (f64.add (f64.const 6.57024977031928170135)
                  (f64.mul (local.get $s) (local.get $q))))
            (local.set $q
               (f64.add (f64.const 1.08635005541779435134e2)
                  (f64.mul (local.get $s) (local.get $q))))
            (local.set $q
               (f64.add (f64.const 4.29008140027567833386e2)
                  (f64.mul (local.get $s) (local.get $q))))
            (local.set $q
               (f64.add (f64.const 6.45387271733267880336e2)
                  (f64.mul (local.get $s) (local.get $q))))
            (local.set $q
               (f64.add (f64.const 4.34565877475229228821e2)
                  (f64.mul (local.get $s) (local.get $q))))
            (local.set $q
               (f64.add (f64.const 1.376577541435190426e2)
                  (f64.mul (local.get $s) (local.get $q))))
            (local.set $q
               (f64.add (f64.const 1.96512716674392571292e1)
                  (f64.mul (local.get $s) (local.get $q))))
            (local.set $q
               (f64.add (f64.const 1) (f64.mul (local.get $s) (local.get $q)))))
         (else
            ;; r = rb0 + s * (rb1 + ... + s * rb6)
            (local.set $r (f64.const -4.83519191608651397019e2))
            (local.set $r
               (f64.add (f64.const -1.02509513161107724954e3)
                  (f64.mul (local.get $s) (local.get $r))))
            (local.set $r
               (f64.add (f64.const -6.37566443368389627722e2)
                  (f64.mul (local.get $s) (local.get $r))))
            (local.set $r
               (f64.add (f64.const -1.60636384855821916062e2)
                  (f64.mul (local.get $s) (local.get $r))))
            (local.set $r
               (f64.add (f64.const -1.77579549177547519889e1)
                  (f64.mul (local.get $s) (local.get $r))))
            (local.set $r
               (f64.add (f64.const -7.99283237680523006574e-1)
                  (f64.mul (local.get $s) (local.get $r))))
            (local.set $r
               (f64.add (f64.const -9.86494292470009928597e-3)
                  (f64.mul (local.get $s) (local.get $r))))
            ;; q = 1 + s * (sb1 + ... + s * sb7)
            (local.set $q (f64.const -2.24409524465858183362e1))
            (local.set $q
               (f64.add (f64.const 4.74528541206955367215e2)
                  (f64.mul (local.get $s) (local.get $q))))
            (local.set $q
               (f64.add (f64.const 2.55305040643316442583e3)
                  (f64.mul (local.get $s) (local.get $q))))
            (local.set $q
               (f64.add (f64.const 3.19985821950859553908e3)
                  (f64.mul (local.get $s) (local.get $q))))
            (local.set $q
               (f64.add (f64.const 1.53672958608443695994e3)
                  (f64.mul (local.get $s) (local.get $q))))
            (local.set $q
               (f64.add (f64.const 3.25792512996573918826e2)
                  (f64.mul (local.get $s) (local.get $q))))
            (local.set $q
               (f64.add (f64.const 3.03380607434824582924e1)
                  (f64.mul (local.get $s) (local.get $q))))
            (local.set $q
               (f64.add (f64.const 1) (f64.mul (local.get $s) (local.get $q))))))
      ;; z is x with the low 32 bits of its significand cleared, so that
      ;; z * z is exact and -x * x = -z * z + (z - x) * (z + x)
      (local.set $z
         (f64.reinterpret_i64
            (i64.and (i64.reinterpret_f64 (local.get $x))
               (i64.const 0xffffffff00000000))))
      (f64.div
         (f64.mul
            (call $exp
               (f64.sub (f64.mul (f64.neg (local.get $z)) (local.get $z))
                  (f64.const 0.5625)))
            (call $exp
               (f64.add
                  (f64.mul (f64.sub (local.get $z) (local.get $x))
                     (f64.add (local.get $z) (local.get $x)))
                  (f64.div (local.get $r) (local.get $q)))))
         (local.get $x)))

   (func (export "caml_erf_float") (param $x f64) (result f64)
      (local $a f64) (local $y f64)
      (local.set $a (f64.abs (local.get $x)))
      (if (f64.lt (local.get $a) (f64.const 0.84375))
         (then
            (if (f64.lt (local.get $a) (f64.const 0x1p-28))
               (then
                  ;; the scaling avoids underflows
                  (return
                     (f64.mul (f64.const 0.125)
                        (f64.add (f64.mul (f64.const 8) (local.get $x))
                           (f64.mul (f64.const 1.02703333676410069053)
                              (local.get $x)))))))
            (return
               (f64.add (local.get $x)
                  (f64.mul (local.get $x)
                     (call $erf_ratio
                        (f64.mul (local.get $x) (local.get $x))))))))
      (if (f64.lt (local.get $a) (f64.const 6))
         (then
            (local.set $y
               (f64.sub (f64.const 1) (call $erfc_abs (local.get $a)))))
         (else
            (if (f64.ne (local.get $x) (local.get $x))
               (then (return (local.get $x))))
            (local.set $y (f64.const 1))))
      (f64.copysign (local.get $y) (local.get $x)))

   (func (export "caml_erfc_float") (param $x f64) (result f64)
      (local $a f64) (local $y f64)
      (local.set $a (f64.abs (local.get $x)))
      (if (f64.lt (local.get $a) (f64.const 0.84375))
         (then
            (if (f64.lt (local.get $a) (f64.const 0x1p-56))
               (then (return (f64.sub (f64.const 1) (local.get $x)))))
            (local.set $y
               (call $erf_ratio (f64.mul (local.get $x) (local.get $x))))
            (if (f64.lt (local.get $x) (f64.const 0.25))
               (then
                  (return
                     (f64.sub (f64.const 1)
                        (f64.add (local.get $x)
                           (f64.mul (local.get $x) (local.get $y)))))))
            (return
               (f64.sub (f64.const 0.5)
                  (f64.add (f64.sub (local.get $x) (f64.const 0.5))
                     (f64.mul (local.get $x) (local.get $y)))))))
      (if (f64.lt (local.get $a) (f64.const 28))
         (then
            (local.set $y (call $erfc_abs (local.get $a)))
            (return
               (if (result f64) (f64.lt (local.get $x) (f64.const 0))
                  (then (f64.sub (f64.const 2) (local.get $y)))
                  (else (local.get $y))))))
      (if (f64.ne (local.get $x) (local.get $x))
         (then (return (local.get $x))))
      (if (result f64) (f64.lt (local.get $x) (f64.const 0))
         (then (f64.const 2))
         (else (f64.const 0))))

   ;; When x has at most 26 significant bits (so that it is not the result
   ;; of a rounding) and y is nonzero, move x slightly in the direction of y
   (func $fma_adjust (param $x f64) (param $y f64) (result f64)
      (local $t f64)
      (if (result f64)
         (i32.and
            (i32.and (f64.ne (local.get $x) (f64.const 0))
               (f64.ne (local.get $y) (f64.const 0)))
            (f64.eq
               (f64.sub
                  (local.tee $t (f64.mul (f64.const 0x8000001) (local.get $x)))
                  (f64.sub (local.get $t) (local.get $x)))
               (local.get $x)))
         (then
            (f64.mul (local.get $x)
               (f64.add (f64.const 1)
                  (select (f64.const 0x1p-52) (f64.const -0x1p-52)
                     (i32.eq (f64.lt (local.get $x) (f64.const 0))
                        (f64.lt (local.get $y) (f64.const 0)))))))
         (else
            (local.get $x))))

   ;; x * y + z, rounded once, also when the result is subnormal. The
   ;; operands are scaled so that x * y does
   ;; not overflow or underflow; the exact product is computed with
   ;; Dekker's algorithm, summed with z with error-free transformations
   ;; (TwoSum), and the sum is rounded once (with a final correction for
   ;; subnormal results). Same algorithm as in the JavaScript runtime.
   (func $fma (param $x f64) (param $y f64) (param $z f64) (result f64)
      (local $scale f64) (local $zs f64) (local $xy f64)
      (local $t f64) (local $xhi f64) (local $xlo f64) (local $yhi f64)
      (local $ylo f64)
      (local $p f64) (local $e f64) (local $s f64) (local $se f64)
      (local $u f64) (local $ue f64) (local $i f64) (local $ie f64)
      (local $v f64) (local $f f64) (local $fs f64)
      (if (i32.or
             (i32.or (f64.eq (local.get $x) (f64.const 0))
                (f64.eq (local.get $y) (f64.const 0)))
             (i32.or
                (f64.ne (f64.sub (local.get $x) (local.get $x)) (f64.const 0))
                (f64.ne (f64.sub (local.get $y) (local.get $y)) (f64.const 0))))
         (then
            (return
               (f64.add (f64.mul (local.get $x) (local.get $y)) (local.get $z)))))
      (if (f64.eq (local.get $z) (f64.const 0))
         (then (return (f64.mul (local.get $x) (local.get $y)))))
      (if (f64.ne (f64.sub (local.get $z) (local.get $z)) (f64.const 0))
         (then (return (local.get $z))))
      ;; scale x and y to [2^-416, 2^416]
      (local.set $scale (f64.const 1))
      (loop $x_big
         (if (f64.gt (f64.abs (local.get $x)) (f64.const 0x1p416))
            (then
               (local.set $scale (f64.mul (local.get $scale) (f64.const 0x1p416)))
               (local.set $x (f64.mul (local.get $x) (f64.const 0x1p-416)))
               (br $x_big))))
      (loop $y_big
         (if (f64.gt (f64.abs (local.get $y)) (f64.const 0x1p416))
            (then
               (local.set $scale (f64.mul (local.get $scale) (f64.const 0x1p416)))
               (local.set $y (f64.mul (local.get $y) (f64.const 0x1p-416)))
               (br $y_big))))
      (if (f64.eq (local.get $scale) (f64.const inf))
         (then
            (return
               (f64.mul (f64.mul (local.get $x) (local.get $y))
                  (local.get $scale)))))
      (loop $x_small
         (if (f64.lt (f64.abs (local.get $x)) (f64.const 0x1p-416))
            (then
               (local.set $scale
                  (f64.mul (local.get $scale) (f64.const 0x1p-416)))
               (local.set $x (f64.mul (local.get $x) (f64.const 0x1p416)))
               (br $x_small))))
      (loop $y_small
         (if (f64.lt (f64.abs (local.get $y)) (f64.const 0x1p-416))
            (then
               (local.set $scale
                  (f64.mul (local.get $scale) (f64.const 0x1p-416)))
               (local.set $y (f64.mul (local.get $y) (f64.const 0x1p416)))
               (br $y_small))))
      (if (f64.eq (local.get $scale) (f64.const 0))
         (then (return (local.get $z))))
      (local.set $zs
         (if (result f64) (f64.eq (local.get $scale) (f64.const 1))
            (then (local.get $z))
            (else (f64.div (local.get $z) (local.get $scale)))))
      (local.set $xy (f64.abs (f64.mul (local.get $x) (local.get $y))))
      ;; z much larger than x * y (|zs| > |xy| * 4 / EPSILON)
      (if (f64.gt (f64.abs (local.get $zs))
             (f64.mul (local.get $xy) (f64.const 0x1p54)))
         (then (return (local.get $z))))
      ;; z much smaller than x * y: only its sign matters
      ;; (|zs| < |xy| * EPSILON / 4 * EPSILON / 4)
      (if (f64.lt (f64.abs (local.get $zs))
             (f64.mul (local.get $xy) (f64.const 0x1p-108)))
         (then
            (local.set $zs
               (f64.copysign (f64.const 0x1p-1022) (local.get $z)))))
      ;; p + e = x * y (Dekker)
      (local.set $xhi
         (f64.sub
            (local.tee $t (f64.mul (f64.const 0x8000001) (local.get $x)))
            (f64.sub (local.get $t) (local.get $x))))
      (local.set $xlo (f64.sub (local.get $x) (local.get $xhi)))
      (local.set $yhi
         (f64.sub
            (local.tee $t (f64.mul (f64.const 0x8000001) (local.get $y)))
            (f64.sub (local.get $t) (local.get $y))))
      (local.set $ylo (f64.sub (local.get $y) (local.get $yhi)))
      (local.set $p (f64.mul (local.get $x) (local.get $y)))
      (local.set $e
         (f64.add
            (f64.add
               (f64.add
                  (f64.sub (f64.mul (local.get $xhi) (local.get $yhi))
                     (local.get $p))
                  (f64.mul (local.get $xhi) (local.get $ylo)))
               (f64.mul (local.get $xlo) (local.get $yhi)))
            (f64.mul (local.get $xlo) (local.get $ylo))))
      ;; s + se = p + zs
      (local.set $s (f64.add (local.get $p) (local.get $zs)))
      (local.set $v (f64.sub (local.get $s) (local.get $p)))
      (local.set $se
         (f64.add
            (f64.sub (local.get $p) (f64.sub (local.get $s) (local.get $v)))
            (f64.sub (local.get $zs) (local.get $v))))
      ;; u + ue = e + se
      (local.set $u (f64.add (local.get $e) (local.get $se)))
      (local.set $v (f64.sub (local.get $u) (local.get $e)))
      (local.set $ue
         (f64.add
            (f64.sub (local.get $e) (f64.sub (local.get $u) (local.get $v)))
            (f64.sub (local.get $se) (local.get $v))))
      ;; i + ie = s + u
      (local.set $i (f64.add (local.get $s) (local.get $u)))
      (local.set $v (f64.sub (local.get $i) (local.get $s)))
      (local.set $ie
         (f64.add
            (f64.sub (local.get $s) (f64.sub (local.get $i) (local.get $v)))
            (f64.sub (local.get $u) (local.get $v))))
      (local.set $f
         (f64.add (local.get $i)
            (call $fma_adjust (local.get $ie) (local.get $ue))))
      (if (f64.eq (local.get $f) (f64.const 0))
         (then (return (local.get $f))))
      (local.set $fs (f64.mul (local.get $f) (local.get $scale)))
      (if (f64.gt (f64.abs (local.get $fs)) (f64.const 0x1p-1022))
         (then (return (local.get $fs))))
      ;; subnormal result: correct the rounding of the scaling, in the
      ;; direction of the rest of the exact sum, i + ie + ue - f (ie alone
      ;; can have the wrong sign when ue moved f past a tie)
      (local.set $v
         (f64.add (f64.sub (local.get $i) (local.get $f)) (local.get $ie)))
      (f64.add (local.get $fs)
         (f64.mul
            (call $fma_adjust
               (f64.sub (local.get $f)
                  (f64.div (local.get $fs) (local.get $scale)))
               (select (local.get $v) (local.get $ue)
                  (f64.ne (local.get $v) (f64.const 0))))
            (local.get $scale))))

   ;; x * y + z, rounded once, with the algorithm of Boldo and Melquiond
   ;; (exact product, TwoSum, rounding to odd), after scaling the operands
   ;; into a safe range. Scaling the result back would round a second time
   ;; when it is subnormal: the slower $fma is used when it can be.
   (func (export "caml_fma_float")
      (param $vx (ref eq)) (param $vy (ref eq)) (param $vz (ref eq))
      (result (ref eq))
      (local $x f64)
      (local $y f64)
      (local $z f64)
      (local $v3 i64)
      (local $v4 i64)
      (local $v5 i64)
      (local $v6 i64)
      (local $v7 i64)
      (local $v8 i64)
      (local $v9 i32)
      (local $v10 i32)
      (local $v11 f64)
      (local $v12 f64)
      (local $v13 f64)
      (local $v14 f64)
      (local $v15 f64)
      (local $exact i32)
      (local.set $x
         (struct.get $float 0 (ref.cast (ref $float) (local.get $vx))))
      (local.set $y
         (struct.get $float 0 (ref.cast (ref $float) (local.get $vy))))
      (local.set $z
         (struct.get $float 0 (ref.cast (ref $float) (local.get $vz))))
      (local.set $v7
         (i64.add
            (local.tee $v4
               (i64.and
                  (i64.shr_u
                     (local.tee $v3 (i64.reinterpret_f64 (local.get $y)))
                     (i64.const 52))
                  (i64.const 2047)))
         (local.tee $v6
            (i64.and
               (i64.shr_u
                  (local.tee $v5 (i64.reinterpret_f64 (local.get $x)))
                  (i64.const 52))
               (i64.const 2047)))))
      (local.set $v8 (i64.reinterpret_f64 (local.get $z)))
      ;; A nonzero x * y + z below 2^-1022 in magnitude implies
      ;; |x * y| < 2^-916 and |z| < 2^-916 (the exact sum is a multiple
      ;; of ulp(x) * ulp(y) and of ulp(z)). Use the slower algorithm
      ;; directly in that case rather than after the fast path: with
      ;; subnormal values, each multiplication costs a microcode assist.
      (if (i32.and (i64.le_u (local.get $v7) (i64.const 1129))
             (i64.le_u (i64.and (local.get $v8) (i64.const 0x7ff0000000000000))
                (i64.const 0x06b0000000000000)))
         (then
            (return
               (struct.new $float
                  (call $fma (local.get $x) (local.get $y) (local.get $z))))))
      (block $label_1
         (block $label_2
            (br_if $label_2 (i64.gt_u (local.get $v4) (i64.const 1993)))
            (br_if $label_2 (i64.gt_u (local.get $v6) (i64.const 1993)))
            (br_if $label_2 (i64.gt_u (local.get $v7) (i64.const 3016)))
            (br_if $label_2
               (i64.gt_u
                  (i64.and (local.get $v8) (i64.const 0x7fe0000000000000))
                  (i64.const 0x7c90000000000000)))
            (local.set $v9 (i32.const 0))
            (br_if $label_2 (i64.le_u (local.get $v7) (i64.const 1076)))
            (local.set $v10 (i32.const 0))
            (br $label_1))
         (local.set $v8
            (i64.and (i64.shr_u (local.get $v8) (i64.const 52))
               (i64.const 2047)))
         (block $cont
            (br_if $cont (i64.eq (local.get $v4) (i64.const 2047)))
            (br_if $cont (i64.eq (local.get $v6) (i64.const 2047)))
            (br_if $cont (i64.ne (local.get $v8) (i64.const 2047)))
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
               (br_if $then (i64.eq (local.get $v6) (i64.const 2047)))
               (br_if $then (i64.eq (local.get $v4) (i64.const 2047)))
               (br_if $then (f64.eq (local.get $y) (f64.const 0)))
               (br_if $then (f64.eq (local.get $x) (f64.const 0)))
               (br_if $cont (i64.ne (local.get $v8) (i64.const 2047))))
            (return
               (struct.new $float
                  (f64.add (f64.mul (local.get $x) (local.get $y))
                     (local.get $z)))))
         (block $cont
            (br_if $cont (i64.lt_u (local.get $v7) (i64.const 3071)))
            (return
               (struct.new $float (f64.mul (local.get $x) (local.get $y)))))
         (block $cont
            (br_if $cont (i64.gt_u (local.get $v7) (i64.const 967)))
            (local.set $y
               (select
                  (f64.const 0x1p-1074)
                  (f64.const -0x1p-1074)
                  (i64.gt_s (i64.xor (local.get $v3) (local.get $v5))
                     (i64.const -1))))
            (block $cont2
               (br_if $cont2 (i64.lt_u (local.get $v8) (i64.const 3)))
               (return
                  (struct.new $float(f64.add (local.get $y) (local.get $z)))))
            (return
               (struct.new $float
                  (f64.mul
                     (f64.add (f64.mul (local.get $z) (f64.const 0x1p54))
                        (local.get $y))
                     (f64.const 0x1p-54)))))
         (block $label_10
            (block $label_11
               (block $label_12
                  (br_if $label_12 (i64.lt_u (local.get $v7) (i64.const 3017)))
                  (local.set $z
                     (select
                        (f64.mul (local.get $z) (f64.const 0x1p-53))
                        (local.get $z)
                        (i64.gt_u (local.get $v8) (i64.const 53))))
                  (local.set $x
                     (select
                        (f64.mul (local.get $x) (f64.const 0x1p-53))
                        (local.get $x)
                        (local.tee $v9 (i64.gt_u (local.get $v6) (local.get $v4)))))
                  (local.set $y
                     (select
                        (local.get $y)
                        (f64.mul (local.get $y) (f64.const 0x1p-53))
                        (local.get $v9)))
                  (br $label_11))
               (br_if $label_10 (i64.lt_u (local.get $v8) (i64.const 1994)))
               (block $label_13
                  (block $label_14
                     (br_if $label_14 (i64.gt_u (local.get $v7) (i64.const 1129)))
                     (block $label_15
                        (br_if $label_15
                           (i64.le_u (local.get $v6) (local.get $v4)))
                        (local.set $x
                           (f64.mul (local.get $x) (f64.const 0x1p108)))
                        (br $label_13))
                     (local.set $y (f64.mul (local.get $y) (f64.const 0x1p108)))
                     (br $label_13))
                  (block $label_16
                     (br_if $label_16 (i64.le_u (local.get $v6) (local.get $v4)))
                     (local.set $x
                        (select
                           (f64.mul (local.get $x) (f64.const 0x1p-53))
                           (local.get $x)
                           (i64.gt_u (local.get $v6) (i64.const 53))))
                     (br $label_13))
                  (local.set $y
                     (select
                        (f64.mul (local.get $y) (f64.const 0x1p-53))
                        (local.get $y)
                        (i64.gt_u (local.get $v4) (i64.const 53)))))
               (local.set $z (f64.mul (local.get $z) (f64.const 0x1p-53))))
            (local.set $v10 (i32.const 0))
            (local.set $v9 (i32.const 1))
            (br $label_1))
         (block $label_17
            (block $label_18
               (br_if $label_18 (i64.lt_u (local.get $v6) (i64.const 1994)))
               (local.set $y (f64.mul (local.get $y) (f64.const 0x1p53)))
               (local.set $x (f64.mul (local.get $x) (f64.const 0x1p-53)))
               (br $label_17))
            (block $label_19
               (br_if $label_19 (i64.lt_u (local.get $v4) (i64.const 1994)))
               (local.set $x (f64.mul (local.get $x) (f64.const 0x1p53)))
               (local.set $y (f64.mul (local.get $y) (f64.const 0x1p-53)))
               (br $label_17))
            (local.set $z
               (select
                  (f64.mul (local.get $z) (f64.const 0x1p108))
                  (local.get $z)
                  (local.tee $v10 (i64.lt_u (local.get $v8) (i64.const 219)))))
            (local.set $x
               (select
                  (f64.mul (local.get $x) (f64.const 0x1p108))
                  (local.get $x)
                  (local.tee $v9 (i64.gt_u (local.get $v6) (local.get $v4)))))
            (local.set $y
               (select
                  (local.get $y)
                  (f64.mul (local.get $y) (f64.const 0x1p108))
                  (local.get $v9)))
            (local.set $v9 (i32.const 0))
            (br $label_1))
         (local.set $v9 (i32.const 0))
         (local.set $v10 (i32.const 0)))
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
               (local.tee $v12
                  (f64.sub
                     (local.get $x)
                     (local.tee $v11
                        (f64.sub
                           (local.tee $v11
                              (f64.mul (local.get $x) (f64.const 0x8000001)))
                        (f64.sub (local.get $v11) (local.get $x))))))
               (local.tee $v14
                  (f64.sub
                     (local.get $y)
                     (local.tee $v13
                     (f64.sub
                        (local.tee $v13
                           (f64.mul (local.get $y) (f64.const 0x8000001)))
                        (f64.sub (local.get $v13) (local.get $y)))))))
            (f64.sub
               (f64.sub
                  (f64.sub
                     (local.tee $v15 (f64.mul (local.get $y) (local.get $x)))
                     (f64.mul (local.get $v11) (local.get $v13)))
                  (f64.mul (local.get $v12) (local.get $v13)))
               (f64.mul (local.get $v11) (local.get $v14)))))
      (block $label_21
         (block $label_22
            (br_if $label_22
               (f64.ne
                  (local.tee $y (f64.add (local.get $z) (local.get $v15)))
                  (f64.const 0)))
            ;; exact zero
            (if (f64.eq (local.get $x) (f64.const 0))
               (then
                  (local.set $exact (i32.const 1))
                  (br $label_21))))
         (block $cont
            (br_if $cont
               (f64.eq
                  (local.tee $z
                     (f64.add
                        (local.tee $v11
                           (f64.add
                              (f64.sub (local.get $x)
                                 (local.tee $v13
                                    (f64.sub
                                       (local.tee $z
                                          (f64.add
                                             (local.tee $v11
                                                (f64.add
                                                   (f64.sub (local.get $v15)
                                                      (local.tee $v11
                                                         (f64.sub
                                                            (local.get $y)
                                                            (local.get $z))))
                                                   (f64.sub (local.get $z)
                                                      (f64.sub
                                                         (local.get $y)
                                                         (local.get $v11)))))
                                             (local.get $x)))
                                          (local.get $v11))))
                              (f64.sub (local.get $v11)
                                 (f64.sub (local.get $z) (local.get $v13)))))
                        (f64.sub
                           (local.tee $y
                              (f64.add
                                 (f64.sub (local.get $z)
                                    (local.tee $v13
                                       (f64.sub
                                          (local.tee $x
                                             (f64.add (local.get $y)
                                                (local.get $z)))
                                          (local.get $y))))
                                 (f64.sub
                                    (local.get $y)
                                    (f64.sub (local.get $x) (local.get $v13)))))
                           (local.tee $y
                              (f64.add (local.get $v11) (local.get $y))))))
                  (f64.const 0)))
            (br_if $cont
               (i32.and
                  (i32.wrap_i64
                     (local.tee $v4 (i64.reinterpret_f64 (local.get $y))))
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
                     (local.get $v4)))))
         (local.set $y (f64.add (local.get $x) (local.get $y)))
         (block $cont
            (br_if $cont (i32.eqz (local.get $v9)))
            (return
               (struct.new $float
                  (f64.mul (local.get $y) (f64.const 0x1p53)))))
         (local.set $y
            (select
               (f64.mul (local.get $y) (f64.const 0x1p-108))
               (local.get $y)
               (local.get $v10))))
      ;; If the result is subnormal (it can be one ulp off, so this includes
      ;; 2^-1022 and zero), use the slower algorithm
      (if (i32.and (i32.eqz (local.get $exact))
             (f64.le (f64.abs (local.get $y)) (f64.const 0x1p-1022)))
         (then
            (return
               (struct.new $float
                  (call $fma
                     (struct.get $float 0 (ref.cast (ref $float) (local.get $vx)))
                     (struct.get $float 0 (ref.cast (ref $float) (local.get $vy)))
                     (struct.get $float 0
                        (ref.cast (ref $float) (local.get $vz))))))))
      (struct.new $float (local.get $y)))

   (func (export "caml_float_compare")
      (param $x f64) (param $y f64) (result i32)
      (i32.add
         (i32.sub (f64.gt (local.get $x) (local.get $y))
                  (f64.lt (local.get $x) (local.get $y)))
         (i32.sub (f64.eq (local.get $x) (local.get $x))
                  (f64.eq (local.get $y) (local.get $y)))))

   (func (export "caml_round_float") (param $x f64) (result f64)
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
