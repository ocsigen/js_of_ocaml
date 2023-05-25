(module
   (import "bindings" "log" (func $log_js (param anyref)))
   (import "ints" "parse_sign_and_base"
      (func $parse_sign_and_base
         (param (ref $string)) (result i32 i32 i32 i32)))
   (import "ints" "parse_digit" (func $parse_digit (param i32) (result i32)))
   (import "fail" "caml_failwith" (func $caml_failwith (param (ref eq))))

   (type $string (array (mut i8)))
   (type $value->value->int
      (func (param (ref eq)) (param (ref eq)) (result i32)))
   (type $value->int
      (func (param (ref eq)) (result i32)))
   (type $custom_operations
      (struct
         (field (ref $string)) ;; identifier
         (field (ref $value->value->int)) ;; compare
         (field (ref null $value->int)) ;; hash
         ;; ZZZ
      ))
   (type $custom (struct (field (ref $custom_operations))))

   (global $int64_ops (export "int64_ops") (ref $custom_operations)
      (struct.new $custom_operations
         (array.new_fixed $string (i32.const 95) (i32.const 106)) ;; "_j"
         (ref.func $int64_cmp)
         (ref.func $int64_hash)))

   (type $int64
      (sub $custom (struct (field (ref $custom_operations)) (field i64))))

   (func $int64_cmp (param $v1 (ref eq)) (param $v2 (ref eq)) (result i32)
      (local $i1 i64) (local $i2 i64)
      (local.set $i1 (struct.get $int64 1 (ref.cast $int64 (local.get $v1))))
      (local.set $i2 (struct.get $int64 1 (ref.cast $int64 (local.get $v2))))
      (i32.sub (i64.gt_s (local.get $i1) (local.get $i2))
               (i64.lt_s (local.get $i1) (local.get $i2))))

   (func $int64_hash (param $v (ref eq)) (result i32)
      (local $i i64)
      (local.set $i (struct.get $int64 1 (ref.cast $int64 (local.get $v))))
      (i32.xor
         (i32.wrap_i64 (local.get $i))
         (i32.wrap_i64 (i64.shr_u (local.get $i) (i64.const 32)))))

   (func $caml_copy_int64 (export "caml_copy_int64")
      (param $i i64) (result (ref eq))
      (struct.new $int64 (global.get $int64_ops) (local.get $i)))

   (func (export "caml_int64_bswap") (param (ref eq)) (result (ref eq))
      (local $i i64)
      (local.set $i (struct.get $int64 1 (ref.cast $int64 (local.get 0))))
      (return_call $caml_copy_int64
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
                         (i64.const 8))))))

   (func (export "caml_int64_compare")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (local $i1 i64) (local $i2 i64)
      (local.set $i1 (struct.get $int64 1 (ref.cast $int64 (local.get 0))))
      (local.set $i2 (struct.get $int64 1 (ref.cast $int64 (local.get 1))))
      (i31.new (i32.sub (i64.gt_s (local.get $i1) (local.get $i2))
                        (i64.lt_s (local.get $i1) (local.get $i2)))))

   (global $INT64_ERRMSG (ref $string)
      (array.new_fixed $string ;; "Int64.of_string"
         (i32.const 73) (i32.const 110) (i32.const 116) (i32.const 54)
         (i32.const 52) (i32.const 46) (i32.const 111) (i32.const 102)
         (i32.const 95) (i32.const 115) (i32.const 116) (i32.const 114)
         (i32.const 105) (i32.const 110) (i32.const 103)))

   (func (export "caml_int64_of_string") (param $v (ref eq)) (result (ref eq))
      (local $s (ref $string))
      (local $i i32) (local $len i32) (local $d i32) (local $c i32)
      (local $signedness i32) (local $sign i32) (local $base i32)
      (local $res i64) (local $threshold i64)
      (local $t (i32 i32 i32 i32))
      (local.set $s (ref.cast $string (local.get $v)))
      (local.set $len (array.len (local.get $s)))
      (local.set $t (call $parse_sign_and_base (local.get $s)))
      (local.set $i (tuple.extract 0 (local.get $t)))
      (local.set $signedness (tuple.extract 1 (local.get $t)))
      (local.set $sign (tuple.extract 2 (local.get $t)))
      (local.set $base (tuple.extract 3 (local.get $t)))
      (local.set $threshold
         (i64.div_u (i64.const -1) (i64.extend_i32_u (local.get $base))))
      (local.set $d
         (call $parse_digit (array.get $string (local.get $s) (local.get $i))))
      (if (i32.ge_u (local.get $d) (local.get $base))
         (then (call $caml_failwith (global.get $INT64_ERRMSG))))
      (local.set $res (i64.extend_i32_u (local.get $d)))
      (loop $loop
         (local.set $i (i32.add (local.get $i) (i32.const 1)))
         (if (i32.lt_s (local.get $i) (local.get $len))
            (then
               (local.set $c (array.get $string (local.get $s) (local.get $i)))
               (br_if $loop (i32.eq (local.get $c) (i32.const 95))) ;; '_'
               (local.set $d (call $parse_digit (local.get $c)))
               (if (i32.ge_u (local.get $d) (local.get $base))
                  (then (call $caml_failwith (global.get $INT64_ERRMSG))))
               (if (i64.gt_u (local.get $res) (local.get $threshold))
                  (then (call $caml_failwith (global.get $INT64_ERRMSG))))
               (local.set $res
                  (i64.add (i64.mul (local.get $res)
                              (i64.extend_i32_u (local.get $base)))
                           (i64.extend_i32_u (local.get $d))))
               (if (i64.lt_u (local.get $res) (i64.extend_i32_u (local.get $d)))
                  (then (call $caml_failwith (global.get $INT64_ERRMSG))))
               (br $loop))))
      (if (local.get $signedness)
         (then
            (if (i32.gt_s (local.get $sign) (i32.const 0))
               (then
                  (if (i64.ge_u (local.get $res)
                                (i64.shl (i64.const 1) (i64.const 63)))
                     (then (call $caml_failwith (global.get $INT64_ERRMSG)))))
               (else
                  (if (i64.gt_u (local.get $res)
                                (i64.shl (i64.const 1) (i64.const 63)))
                     (then
                        (call $caml_failwith (global.get $INT64_ERRMSG))))))))
      (if (i32.lt_s (local.get $sign) (i32.const 0))
         (then (local.set $res (i64.sub (i64.const 0) (local.get $res)))))
      (return_call $caml_copy_int64 (local.get $res)))

   (func (export "caml_int64_create_lo_mi_hi")
      (param (ref eq) (ref eq) (ref eq)) (result (ref eq))
      ;; ZZZ does not really make sense
      (call $log_js (string.const "caml_int64_create_lo_mi_hi"))
      (i31.new (i32.const 0)))

   ;; ZZZ
   (func $dummy_format_fun (param (ref eq)) (param (ref eq)) (result (ref eq))
      (call $log_js (string.const "dummy_format_fun"))
      (array.new_fixed $string (i32.const 64)))
   (export "caml_int64_format" (func $dummy_format_fun))
)
