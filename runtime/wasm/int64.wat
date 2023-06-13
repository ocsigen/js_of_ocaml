(module
   (import "bindings" "log" (func $log_js (param anyref)))
   (import "ints" "parse_sign_and_base"
      (func $parse_sign_and_base
         (param (ref $string)) (result i32 i32 i32 i32)))
   (import "ints" "parse_digit" (func $parse_digit (param i32) (result i32)))
   (import "ints" "parse_int_format"
      (func $parse_int_format
         (param (ref $string)) (result i32 i32 i32 i32 i32)))
   (import "fail" "caml_failwith" (func $caml_failwith (param (ref eq))))

   (type $string (array (mut i8)))
   (type $value->value->int->int
      (func (param (ref eq)) (param (ref eq)) (param i32) (result i32)))
   (type $value->int
      (func (param (ref eq)) (result i32)))
   (type $custom_operations
      (struct
         (field $cust_id (ref $string))
         (field $cust_compare (ref null $value->value->int->int))
         (field $cust_compare_ext (ref null $value->value->int->int))
         (field $cust_hash (ref null $value->int))
         ;; ZZZ
      ))
   (type $custom (struct (field (ref $custom_operations))))

   (global $int64_ops (export "int64_ops") (ref $custom_operations)
      (struct.new $custom_operations
         (array.new_fixed $string (i32.const 95) (i32.const 106)) ;; "_j"
         (ref.func $int64_cmp)
         (ref.null $value->value->int->int)
         (ref.func $int64_hash)))

   (type $int64
      (sub $custom (struct (field (ref $custom_operations)) (field i64))))

   (func $int64_cmp
      (param $v1 (ref eq)) (param $v2 (ref eq)) (param i32) (result i32)
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
      (if (i32.eqz (local.get $len))
        (then (call $caml_failwith (global.get $INT64_ERRMSG))))
      (local.set $t (call $parse_sign_and_base (local.get $s)))
      (local.set $i (tuple.extract 0 (local.get $t)))
      (local.set $signedness (tuple.extract 1 (local.get $t)))
      (local.set $sign (tuple.extract 2 (local.get $t)))
      (local.set $base (tuple.extract 3 (local.get $t)))
      (local.set $threshold
         (i64.div_u (i64.const -1) (i64.extend_i32_u (local.get $base))))
      (local.set $d
         (call $parse_digit (array.get_u $string (local.get $s) (local.get $i))))
      (if (i32.ge_u (local.get $d) (local.get $base))
         (then (call $caml_failwith (global.get $INT64_ERRMSG))))
      (local.set $res (i64.extend_i32_u (local.get $d)))
      (loop $loop
         (local.set $i (i32.add (local.get $i) (i32.const 1)))
         (if (i32.lt_s (local.get $i) (local.get $len))
            (then
               (local.set $c (array.get_u $string (local.get $s) (local.get $i)))
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

   (func $format_int64_default (param $d i64) (result (ref eq))
      (local $s (ref $string))
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
      (local.set $s (array.new $string (i32.const 0) (local.get $i)))
      (loop $write
         (local.set $i (i32.sub (local.get $i) (i32.const 1)))
         (array.set $string (local.get $s) (local.get $i)
            (i32.add (i32.const 48)
               (i32.wrap_i64 (i64.rem_u (local.get $d) (i64.const 10)))))
         (local.set $d (i64.div_u (local.get $d) (i64.const 10)))
         (br_if $write (i64.ne (local.get $d) (i64.const 0))))
      (if (local.get $negative)
         (then
            (array.set $string (local.get $s) (i32.const 0)
               (i32.const 45)))) ;; '-'
      (local.get $s))

   (type $chars (array i8))

   (import "ints" "lowercase_hex_table"
      (global $lowercase_hex_table (ref $chars)))

   (import "ints" "uppercase_hex_table"
      (global $uppercase_hex_table (ref $chars)))

   (func (export "caml_int64_format")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (local $d i64)
      (local $s (ref $string))
      (local $format (i32 i32 i32 i32 i32))
      (local $sign_style i32) (local $alternate i32) (local $signed i32)
      (local $base i64) (local $uppercase i32)
      (local $negative i32)
      (local $i i32)
      (local $n i64)
      (local $chars (ref $chars))
      (local.set $s (ref.cast $string (local.get 0)))
      (local.set $d (struct.get $int64 1 (ref.cast $int64 (local.get 1))))
      (if (i32.eq (array.len (local.get $s)) (i32.const 2))
         (then
            (if (i32.eq (array.get_u $string (local.get $s) (i32.const 1))
                        (i32.const 100)) ;; 'd'
               (then (return_call $format_int64_default (local.get $d))))))
      (local.set $format (call $parse_int_format (local.get $s)))
      (local.set $sign_style (tuple.extract 0 (local.get $format)))
      (local.set $alternate (tuple.extract 1 (local.get $format)))
      (local.set $signed (tuple.extract 2 (local.get $format)))
      (local.set $base (i64.extend_i32_u (tuple.extract 3 (local.get $format))))
      (local.set $uppercase (tuple.extract 4 (local.get $format)))
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
      (local.set $s (array.new $string (i32.const 0) (local.get $i)))
      (loop $write
         (local.set $i (i32.sub (local.get $i) (i32.const 1)))
         (array.set $string (local.get $s) (local.get $i)
            (array.get_u $chars (local.get $chars)
               (i32.wrap_i64 (i64.rem_u (local.get $d) (local.get $base)))))
         (local.set $d (i64.div_u (local.get $d) (local.get $base)))
         (br_if $write (i64.ne (local.get $d) (i64.const 0))))
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
      (if (local.get $alternate)
         (then
            (if (local.get $i)
               (then
                  (array.set $string (local.get $s) (i32.const 0)
                     (i32.const 48)) ;; '0'
                  (if (i64.eq (local.get $base) (i64.const 16))
                     (then
                        (array.set $string (local.get $s) (i32.const 1)
                           (select (i32.const 88) (i32.const 120) ;; 'X' 'x'
                              (local.get $uppercase)))))))))
      (local.get $s))

)
