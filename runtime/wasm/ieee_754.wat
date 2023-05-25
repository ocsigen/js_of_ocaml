(module
   (import "bindings" "log" (func $log_js (param anyref)))
   (import "jslib" "wrap" (func $wrap (param anyref) (result (ref eq))))
   (import "jslib" "caml_string_of_jsstring"
      (func $caml_string_of_jsstring (param (ref eq)) (result (ref eq))))
   (import "bindings" "format" (func $format_float (param f64) (result anyref)))

   (type $float (struct (field f64)))
   (type $string (array (mut i8)))
   (type $block (array (mut (ref eq))))

   (func (export "caml_hexstring_of_float")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (array.new_fixed $string (i32.const 64)))

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

   (func (export "caml_float_of_string") (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_float_of_string"))
      (unreachable))

   (func (export "caml_float_compare")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (local $x f64) (local $y f64)
      (local.set $x (struct.get $float 0 (ref.cast $float (local.get 0))))
      (local.set $y (struct.get $float 0 (ref.cast $float (local.get 1))))
      (if (f64.eq (local.get $x) (local.get $y))
         (then (return (i31.new (i32.const 0)))))
      (if (f64.lt (local.get $x) (local.get $y))
         (then (return (i31.new (i32.const -1)))))
      (if (f64.gt (local.get $x) (local.get $y))
         (then (return (i31.new (i32.const -1)))))
      (if (f64.eq (local.get $x) (local.get $x))
         (then (return (i31.new (i32.const 1)))))
      (if (f64.eq (local.get $y) (local.get $y))
         (then (return (i31.new (i32.const -1)))))
      (i31.new (i32.const 0)))

   (func (export "caml_format_float")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $caml_string_of_jsstring (call $wrap (call $format_float (struct.get $float 0 (ref.cast $float (local.get 1)))))))
)
