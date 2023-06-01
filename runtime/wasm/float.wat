(module
   (import "bindings" "log" (func $log_js (param anyref)))
   (import "jslib" "wrap" (func $wrap (param anyref) (result (ref eq))))
   (import "jslib" "unwrap" (func $unwrap (param (ref eq)) (result anyref)))
   (import "jslib" "caml_string_of_jsstring"
      (func $caml_string_of_jsstring (param (ref eq)) (result (ref eq))))
   (import "jslib" "caml_jsstring_of_string"
      (func $caml_jsstring_of_string (param (ref eq)) (result (ref eq))))
   (import "bindings" "format_float"
      (func $format_float
         (param i32) (param i32) (param f64) (result (ref string))))
   (import "Math" "exp" (func $exp (param f64) (result f64)))
   (import "fail" "caml_invalid_argument"
      (func $caml_invalid_argument (param (ref eq))))
   (import "ints" "lowercase_hex_table"
      (global $lowercase_hex_table (ref $chars)))

   (type $float (struct (field f64)))
   (type $string (array (mut i8)))
   (type $block (array (mut (ref eq))))

   (type $chars (array i8))

   (global $infinity (ref $chars)
      (array.new_fixed $chars
         (i32.const 105) (i32.const 110) (i32.const 102) (i32.const 105)
         (i32.const 110) (i32.const 105) (i32.const 116) (i32.const 121)))

   (global $nan (ref $chars)
      (array.new_fixed $chars (i32.const 110) (i32.const 97) (i32.const 110)))

   (func (export "caml_hexstring_of_float")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
      (local $b i64) (local $prec i32) (local $style i32)
      (local $sign i32) (local $exp i32) (local $m i64)
      (local $i i32) (local $j i32) (local $d i32) (local $txt (ref $chars))
      (local $len i32) (local $s (ref $string))
      (local $unit i64) (local $half i64) (local $mask i64) (local $frac i64)
      (local.set $prec (i31.get_s (ref.cast i31 (local.get 1))))
      (local.set $style (i31.get_s (ref.cast i31 (local.get 2))))
      (local.set $b
         (i64.reinterpret_f64
            (struct.get $float 0 (ref.cast $float (local.get 0)))))
      (local.set $sign (i32.wrap_i64 (i64.shr_u (local.get $b) (i64.const 63))))
      (local.set $exp
          (i32.and (i32.wrap_i64 (i64.shr_u (local.get $b) (i64.const 52)))
                   (i32.const 0x7FF)))
      (local.set $m
         (i64.and (local.get $b)
            (i64.sub (i64.shl (i64.const 1) (i64.const 52)) (i64.const 1))))
      (local.set $i
         (i32.or (local.get $sign)
            (i32.ne (local.get $style) (i32.const 45)))) ;; '-'
      (local.set $s
         (block $sign (result (ref $string))
            (if (i32.eq (local.get $exp) (i32.const 0x7FF))
               (then
                  (local.set $txt
                     (if (result (ref $chars)) (i64.eqz (local.get $m))
                        (then
                           (global.get $infinity))
                        (else
                           (local.set $sign (i32.const 0))
                           (local.set $i
                              (i32.ne (local.get $style) (i32.const 45)))
                           (global.get $nan))))
                  (local.set $len (array.len (local.get $txt)))
                  (local.set $s
                     (array.new $string (i32.const 0)
                        (i32.add (local.get $i) (local.get $len))))
                  (array.copy $string $chars
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
            (br_if $count (i32.ne (local.get $d) (i32.const 0))))
         (local.set $len (i32.add (i32.add (local.get $i) (local.get $prec))
                                  (i32.add (i32.const 6) (local.get $j))))
         (if (i32.eqz (local.get $prec))
            (then (local.set $len (i32.sub (local.get $len) (i32.const 1)))))
         (local.set $s (array.new $string (i32.const 0) (local.get $len)))
         (if (i32.ge_s (local.get $exp) (i32.const 0))
            (then (local.set $d (local.get $exp)))
            (else (local.set $d (i32.sub (i32.const 0) (local.get $exp)))))
         (loop $write
            (local.set $len (i32.sub (local.get $len) (i32.const 1)))
            (array.set $string (local.get $s) (local.get $len)
               (i32.add (i32.const 48)
                  (i32.rem_u (local.get $d) (i32.const 10))))
            (local.set $d (i32.div_u (local.get $d) (i32.const 10)))
            (br_if $write (i32.ne (local.get $d) (i32.const 0))))
         (array.set $string (local.get $s)
            (i32.sub (local.get $len) (i32.const 1))
            (select (i32.const 43) (i32.const 45)
               (i32.ge_s (local.get $exp) (i32.const 0))))
         (array.set $string (local.get $s) (local.get $i) (i32.const 48)) ;; '0'
         (array.set $string (local.get $s) (i32.add (local.get $i) (i32.const 1))
            (i32.const 120)) ;; 'x'
         (array.set $string (local.get $s) (i32.add (local.get $i) (i32.const 2))
            (i32.add
               (i32.wrap_i64 (i64.shr_u (local.get $m) (i64.const 52)))
               (i32.const 48))) ;; '0'
         (local.set $i (i32.add (local.get $i) (i32.const 3)))
         (if (i32.gt_s (local.get $prec) (i32.const 0))
            (then
               (array.set $string (local.get $s) (local.get $i)
                  (i32.const 46)) ;; '.'
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (local.set $frac (i64.shl (local.get $m) (i64.const 12)))
               (loop $write
                  (array.set $string (local.get $s) (local.get $i)
                     (array.get_u $chars (global.get $lowercase_hex_table)
                        (i32.wrap_i64
                           (i64.shr_u (local.get $frac) (i64.const 60)))))
                  (local.set $frac (i64.shl (local.get $frac) (i64.const 4)))
                  (local.set $prec (i32.sub (local.get $prec) (i32.const 1)))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (br_if $write (i32.gt_s (local.get $prec) (i32.const 0))))))
         (array.set $string (local.get $s) (local.get $i) (i32.const 112))
         (local.get $s)))
      (if (local.get $sign)
         (then
            (array.set $string (local.get $s) (i32.const 0)
               (i32.const 45))) ;; '-'
         (else
            (if (i32.ne (local.get $style) (i32.const 45)) ;; '-'
               (then
                  (array.set $string (local.get $s) (i32.const 0)
                     (local.get $style))))))
      (local.get $s))

   (data $format_error "format_float: bad format")

   (func $parse_format
      (param $s (ref $string)) (result i32 i32 i32 i32)
      (local $i i32) (local $len i32) (local $c i32)
      (local $sign_style i32) (local $precision i32)
      (local $conversion i32) (local $uppercase i32)
      (local.set $len (array.len (local.get $s)))
      (local.set $i (i32.const 1))
      (block $return
         (block $bad_format
            (br_if $bad_format (i32.lt_u (local.get $len) (i32.const 2)))
            (br_if $bad_format
               (i32.ne (array.get $string (local.get $s) (i32.const 0))
                       (i32.const 37))) ;; '%'
            (local.set $c (array.get $string (local.get $s) (i32.const 1)))
            (if (i32.eq (local.get $c) (i32.const 43)) ;; '+'
               (then
                  (local.set $sign_style (i32.const 1))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))))
            (if (i32.eq (local.get $c) (i32.const 32)) ;; ' '
               (then
                  (local.set $sign_style (i32.const 2))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))))
            (br_if $bad_format (i32.eq (local.get $i) (local.get $len)))
            (br_if $bad_format
               (i32.ne (array.get $string (local.get $s) (local.get $i))
                       (i32.const 46))) ;; '.'
            (loop $precision
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br_if $bad_format (i32.eq (local.get $i) (local.get $len)))
               (local.set $c
                  (array.get $string (local.get $s) (local.get $i)))
               (if (i32.and (i32.ge_u (local.get $c) (i32.const 48))  ;; '0'
                            (i32.le_u (local.get $c) (i32.const 57))) ;; '9'
                  (then
                     (local.set $precision
                        (i32.add (i32.mul (local.get $precision) (i32.const 10))
                                 (i32.sub (local.get $c) (i32.const 48))))
                     (br $precision))))
            (br_if $bad_format
              (i32.ne (i32.add (local.get $i) (i32.const 1)) (local.get $len)))
            (local.set $uppercase (i32.lt_s (local.get $c) (i32.const 96)))
            (local.set $conversion
               (i32.sub
                  (i32.and (local.get $c) (i32.const 0xdf))
                  (i32.const 69))) ;; 'E'
            (br_if $return (i32.le_u (local.get $conversion) (i32.const 2))))
         (call $caml_invalid_argument
            (array.new_data $string $format_error
               (i32.const 0) (i32.const 22))))
      (tuple.make
         (local.get $sign_style)
         (local.get $precision)
         (local.get $conversion)
         (local.get $uppercase)))

   (global $inf (ref $chars)
      (array.new_fixed $chars (i32.const 105) (i32.const 110) (i32.const 102)))

   (func (export "caml_format_float")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (local $f f64) (local $b i64) (local $format (i32 i32 i32 i32))
      (local $sign_style i32) (local $precision i32)
      (local $conversion i32) (local $uppercase i32)
      (local $negative i32)
      (local $exp i32) (local $m i64)
      (local $i i32) (local $len i32) (local $c i32)
      (local $s (ref $string)) (local $txt (ref $chars))
      (local $num (ref string))
      (local.set $f (struct.get $float 0 (ref.cast $float (local.get 1))))
      (local.set $b (i64.reinterpret_f64 (local.get $f)))
      (local.set $format (call $parse_format (ref.cast $string (local.get 0))))
      (local.set $sign_style (tuple.extract 0 (local.get $format)))
      (local.set $precision (tuple.extract 1 (local.get $format)))
      (local.set $conversion (tuple.extract 2 (local.get $format)))
      (local.set $uppercase (tuple.extract 3 (local.get $format)))
      (local.set $negative
         (i32.wrap_i64 (i64.shr_u (local.get $b) (i64.const 63))))
      (local.set $i
         (i32.or (local.get $negative) (local.get $sign_style)))
      (local.set $s
         (block $sign (result (ref $string))
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
                           (local.set $i (local.get $sign_style))
                           (global.get $nan))))
                  (local.set $len (array.len (local.get $txt)))
                  (local.set $s
                     (array.new $string (i32.const 0)
                        (i32.add (local.get $i) (local.get $len))))
                  (array.copy $string $chars
                     (local.get $s) (local.get $i) (local.get $txt) (i32.const 0)
                     (local.get $len))
                  (br $sign (local.get $s))))
            (local.set $num
               (call $format_float
                  (local.get $precision) (local.get $conversion)
                  (f64.abs (local.get $f))))
            (local.set $len (string.measure_wtf8 wtf8 (local.get $num)))
            (local.set $s
               (array.new $string (i32.const 0)
                  (i32.add (local.get $len) (local.get $i))))
            (drop (string.encode_wtf8_array replace
                     (local.get $num) (local.get $s) (local.get $i)))
            (br $sign (local.get $s))))
      (if (local.get $negative)
         (then
            (array.set $string (local.get $s) (i32.const 0)
               (i32.const 45))) ;; '-'
         (else
            (if (local.get $sign_style)
               (then
                  (if (i32.eq (local.get $sign_style) (i32.const 1))
                     (then
                        (array.set $string (local.get $s) (i32.const 0)
                           (i32.const 43))) ;; '+'
                     (else
                        (array.set $string (local.get $s) (i32.const 0)
                           (i32.const 32)))))))) ;; ' '
      (if (local.get $uppercase)
         (then
            (local.set $i (i32.const 0))
            (local.set $len (array.len (local.get $s)))
            (loop $uppercase
               (local.set $c (array.get $string (local.get $s) (local.get $i)))
               (if (i32.and (i32.ge_u (local.get $c) (i32.const 97))   ;; 'a'
                            (i32.le_u (local.get $c) (i32.const 122))) ;; 'z'
                  (then
                     (array.set $string (local.get $s) (local.get $i)
                        (i32.sub (local.get $c) (i32.const 32)))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br_if $uppercase (i32.lt_u (local.get $i) (local.get $len))))))
      (local.get $s))

   (func (export "caml_nextafter")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (local $x f64) (local $y f64) (local $i i64) (local $j i64)
      (local.set $x (struct.get $float 0 (ref.cast $float (local.get 0))))
      (local.set $y (struct.get $float 0 (ref.cast $float (local.get 1))))
      (if (f64.ne (local.get $x) (local.get $x)) (then (return (local.get 0))))
      (if (f64.ne (local.get $y) (local.get $y)) (then (return (local.get 1))))
      (if (f64.eq (local.get $x) (local.get $y))
         (then (return (local.get 1))))
      (if (result (ref eq)) (f64.eq (local.get $x) (f64.const 0))
         (then
            (if (f64.ge (local.get $y) (f64.const 0))
               (then (return (struct.new $float (f64.const 0x1p-1074))))
               (else (return (struct.new $float (f64.const -0x1p-1074))))))
         (else
            (local.set $i (i64.reinterpret_f64 (local.get $x)))
            (local.set $j (i64.reinterpret_f64 (local.get $y)))
            (if (i32.and (i64.lt_s (local.get $i) (local.get $j))
                         (i64.lt_u (local.get $i) (local.get $j)))
               (then (local.set $i (i64.add (local.get $i) (i64.const 1))))
               (else (local.set $i (i64.sub (local.get $i) (i64.const 1)))))
            (return (struct.new $float (f64.reinterpret_i64 (local.get $i)))))))

   (func (export "caml_classify_float") (param (ref eq)) (result (ref eq))
      (local $a f64)
      (local.set $a
         (f64.abs (struct.get $float 0 (ref.cast $float (local.get 0)))))
      (i31.new
         (if (result i32) (f64.ge (local.get $a) (f64.const 0x1p-1022))
            (then
               (if (result i32) (f64.lt (local.get $a) (f64.const infinity))
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
      (local.set $x (struct.get $float 0 (ref.cast $float (local.get 0))))
      (local.set $a (f64.abs (local.get $x)))
      (if (f64.ge (local.get $a) (f64.const 0))
         (then
            (if (f64.lt (local.get $a) (f64.const infinity))
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
      (array.new_fixed $block (i31.new (i32.const 0))
         (struct.new $float (local.get $f)) (struct.new $float (local.get $i))))

   (func (export "caml_ldexp")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (local $x f64) (local $n i32)
      (local.set $x (struct.get $float 0 (ref.cast $float (local.get 0))))
      (local.set $n (i31.get_s (ref.cast i31 (local.get 1))))
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
                     (then (local.set $n (i32.const 1023))))))
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
                           (then (local.set $n (i32.const -1022)))))))))))
      (struct.new $float
         (f64.mul (local.get $x)
            (f64.reinterpret_i64
               (i64.shl (i64.add (i64.extend_i32_s (local.get $n))
                                 (i64.const 0x3ff))
                        (i64.const 52))))))

   (func $frexp (param $x f64) (result f64 i32)
      (local $y i64)
      (local $e i32)
      (local $r (f64 i32))
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
                     (tuple.make (tuple.extract 0 (local.get $r))
                        (i32.sub (tuple.extract 1 (local.get $r))
                           (i32.const 64)))))
               (else
                  (return (tuple.make (local.get $x) (i32.const 0))))))
         (else
            (if (i32.eq (local.get $e) (i32.const 0x7ff))
               (then
                  (return (tuple.make (local.get $x) (i32.const 0)))))))
      (tuple.make
         (f64.reinterpret_i64
            (i64.or (i64.and (local.get $y) (i64.const 0x800fffffffffffff))
               (i64.const 0x3fe0000000000000)))
         (i32.sub (local.get $e) (i32.const 0x3fe))))

   (func (export "caml_frexp_float") (param (ref eq)) (result (ref eq))
      (local $r (f64 i32))
      (local.set $r
         (call $frexp (struct.get $float 0 (ref.cast $float (local.get 0)))))
      (array.new_fixed $block (i31.new (i32.const 0))
         (struct.new $float (tuple.extract 0 (local.get $r)))
         (i31.new (tuple.extract 1 (local.get $r)))))

   (func (export "caml_signbit_float") (param (ref eq)) (result (ref eq))
      (i31.new
         (i32.wrap_i64
            (i64.shr_u
               (i64.reinterpret_f64
                  (struct.get $float 0 (ref.cast $float (local.get 0))))
               (i64.const 63)))))

   (func $erf (param $x f64) (result f64)
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

   (func (export "caml_erf_float") (param (ref eq)) (result (ref eq))
      (struct.new $float
         (call $erf (struct.get $float 0 (ref.cast $float (local.get 0))))))

   (func (export "caml_erfc_float") (param (ref eq)) (result (ref eq))
      (struct.new $float
         (f64.sub (f64.const 1)
            (call $erf (struct.get $float 0 (ref.cast $float (local.get 0)))))))

   (func (export "caml_fma_float")
      (param $x (ref eq)) (param $y (ref eq)) (param $z (ref eq))
      (result (ref eq))
      ;; ZZZ not accurate
      (struct.new $float
         (f64.add
            (f64.mul (struct.get $float 0 (ref.cast $float (local.get $x)))
                     (struct.get $float 0 (ref.cast $float (local.get $y))))
            (struct.get $float 0 (ref.cast $float (local.get $z))))))

   (func (export "caml_float_of_string") (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_float_of_string"))
      (unreachable))

   (func (export "caml_float_compare")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (local $x f64) (local $y f64)
      (local.set $x (struct.get $float 0 (ref.cast $float (local.get 0))))
      (local.set $y (struct.get $float 0 (ref.cast $float (local.get 1))))
      (i31.new
         (i32.add
            (i32.sub (f64.gt (local.get $x) (local.get $y))
                     (f64.lt (local.get $y) (local.get $x)))
            (i32.sub (f64.eq (local.get $x) (local.get $x))
                     (f64.eq (local.get $y) (local.get $y))))))
)
