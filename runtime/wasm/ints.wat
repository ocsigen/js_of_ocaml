(module
   (import "bindings" "log" (func $log_js (param anyref)))
   (import "jslib" "caml_string_of_jsstring"
      (func $caml_string_of_jsstring (param (ref eq)) (result (ref eq))))
   (import "jslib" "wrap" (func $wrap (param anyref) (result (ref eq))))
   (import "bindings" "format" (func $format_int (param (ref eq)) (result anyref)))
   (import "fail" "caml_failwith" (func $caml_failwith (param (ref eq))))

   (type $string (array (mut i8)))

   (func (export "caml_format_int") (param (ref eq)) (param (ref eq)) (result (ref eq))
      (call $caml_string_of_jsstring (call $wrap (call $format_int (local.get 1)))))

   (func $parse_sign_and_base (export "parse_sign_and_base")
      (param $s (ref $string)) (result i32 i32 i32 i32)
      (local $i i32) (local $len i32) (local $c i32)
      (local $signedness i32) (local $sign i32) (local $base i32)
      (local.set $i (i32.const 0))
      (local.set $len (array.len (local.get $s)))
      (local.set $signedness (i32.const 1))
      (local.set $sign (i32.const 1))
      (local.set $base (i32.const 10))
      (if (i32.eqz (local.get $len))
         (then
            (local.set $c (array.get $string (local.get $s) (i32.const 0)))
            (if (i32.eq (local.get $c) (i32.const 45))
               (then
                  (local.set $sign (i32.const -1))
                  (local.set $i (i32.const 1))))
               (else (if (i32.eq (local.get $c) (i32.const 43))
                  (then (local.set $i (i32.const 1)))))))
      (if (i32.lt_s (i32.add (local.get $i) (i32.const 1)) (local.get $len))
         (then (if (i32.eq (array.get $string (local.get $s) (local.get $i))
                           (i32.const 48))
            (then
               (local.set $c
                  (array.get $string (local.get $s)
                     (i32.add (local.get $i) (i32.const 1))))
               (if (i32.or (i32.eq (local.get $c) (i32.const 88))
                           (i32.eq (local.get $c) (i32.const 120)))
                  (then
                     (local.set $base (i32.const 16))
                     (local.set $signedness (i32.const 0))
                     (local.set $i (i32.add (local.get $i) (i32.const 2))))
               (else (if (i32.or (i32.eq (local.get $c) (i32.const 79))
                                 (i32.eq (local.get $c) (i32.const 111)))
                  (then
                     (local.set $base (i32.const 8))
                     (local.set $signedness (i32.const 0))
                     (local.set $i (i32.add (local.get $i) (i32.const 2))))
               (else (if (i32.or (i32.eq (local.get $c) (i32.const 66))
                                 (i32.eq (local.get $c) (i32.const 98)))
                  (then
                     (local.set $base (i32.const 2))
                     (local.set $signedness (i32.const 0))
                     (local.set $i (i32.add (local.get $i) (i32.const 2))))
               (else (if (i32.or (i32.eq (local.get $c) (i32.const 85))
                                 (i32.eq (local.get $c) (i32.const 117)))
                  (then
                     (local.set $signedness (i32.const 0))
                     (local.set $i (i32.add (local.get $i)
                        (i32.const 2)))))))))))))))
      (tuple.make
         (local.get $i) (local.get $signedness) (local.get $sign)
         (local.get $base)))

   (func $parse_digit (export "parse_digit") (param $c i32) (result i32)
      (if (i32.and (i32.ge_u (local.get $c) (i32.const 48))
                   (i32.le_u (local.get $c) (i32.const 57)))
         (then (return (i32.sub (local.get $c) (i32.const 48)))))
      (if (i32.and (i32.ge_u (local.get $c) (i32.const 65))
                   (i32.le_u (local.get $c) (i32.const 90)))
         (then (return (i32.sub (local.get $c) (i32.const 55)))))
      (if (i32.and (i32.ge_u (local.get $c) (i32.const 97))
                   (i32.le_u (local.get $c) (i32.const 122)))
         (then (return (i32.sub (local.get $c) (i32.const 87)))))
      (return (i32.const -1)))

   (func $parse_int (export "parse_int")
      (param $v (ref eq)) (param $nbits i32) (param $errmsg (ref $string))
      (result i32)
      (local $s (ref $string))
      (local $i i32) (local $len i32) (local $d i32) (local $c i32)
      (local $signedness i32) (local $sign i32) (local $base i32)
      (local $res i32) (local $threshold i32)
      (local $t (i32 i32 i32 i32))
      (local.set $s (ref.cast $string (local.get $v)))
      (local.set $len (array.len (local.get $s)))
      (local.set $t (call $parse_sign_and_base (local.get $s)))
      (local.set $i (tuple.extract 0 (local.get $t)))
      (local.set $signedness (tuple.extract 1 (local.get $t)))
      (local.set $sign (tuple.extract 2 (local.get $t)))
      (local.set $base (tuple.extract 3 (local.get $t)))
      (local.set $threshold (i32.div_u (i32.const -1) (local.get $base)))
      (local.set $d
         (call $parse_digit (array.get $string (local.get $s) (local.get $i))))
      (if (i32.ge_u (local.get $d) (local.get $base))
         (then (call $caml_failwith (local.get $errmsg))))
      (local.set $res (local.get $d))
      (loop $loop
         (local.set $i (i32.add (local.get $i) (i32.const 1)))
         (if (i32.lt_s (local.get $i) (local.get $len))
            (then
               (local.set $c (array.get $string (local.get $s) (local.get $i)))
               (br_if $loop (i32.eq (local.get $c) (i32.const 95))) ;; '_'
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

   (global $INT_ERRMSG (ref $string)
      (array.new_fixed $string ;; "Int.of_string"
         (i32.const 73) (i32.const 110) (i32.const 116) (i32.const 46)
         (i32.const 111) (i32.const 102) (i32.const 95) (i32.const 115)
         (i32.const 116) (i32.const 114) (i32.const 105) (i32.const 110)
         (i32.const 103)))

   (func (export "caml_int_of_string")
      (param $v (ref eq)) (result (ref eq))
      (i31.new
         (call $parse_int
            (local.get $v) (i32.const 31) (global.get $INT_ERRMSG))))

   (func (export "caml_bswap16") (param (ref eq)) (result (ref eq))
      (local $x i32)
      (local.set $x (i31.get_s (ref.cast i31 (local.get 0))))
      (i31.new
         (i32.or
            (i32.shl (i32.and (local.get $x) (i32.const 0xFF)) (i32.const 8))
            (i32.shr_u (i32.and (local.get $x) (i32.const 0x00FF))
               (i32.const 8)))))

   (func (export "%caml_format_int_special") (param (ref eq)) (result (ref eq))
      (call $log_js (string.const "%caml_format_int_special"))
      (call $caml_string_of_jsstring (call $wrap (call $format_int (local.get 0)))))
)
