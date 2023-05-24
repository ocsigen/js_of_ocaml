(module
   (tag $ocaml_exception (export "ocaml_exception") (param (ref eq)))
   (tag $ocaml_exit (export "ocaml_exit") (param i32))

   (import "bindings" "log" (func $log (param i32)))
   (import "bindings" "log" (func $log_js (param anyref)))

   (type $float (struct (field f64)))

   (type $block (array (mut (ref eq))))

   (type $string (array (mut i8)))

   (type $function_1 (func (param (ref eq) (ref eq)) (result (ref eq))))

   (type $closure (struct (;(field i32);) (field (ref $function_1))))

   (type $closure_last_arg
      (sub $closure (struct (;(field i32);) (field (ref $function_1)))))

   (type $dummy_closure_1
      (sub $closure_last_arg
         (struct (field (ref $function_1)) (field (mut (ref null $closure))))))

   (type $function_2
      (func (param (ref eq) (ref eq) (ref eq)) (result (ref eq))))

   (type $closure_2
      (sub $closure
         (struct (field (ref $function_1)) (field (ref $function_2)))))

   (type $dummy_closure_2
      (sub $closure_2
         (struct (field (ref $function_1)) (field (ref $function_2))
            (field (mut (ref null $closure_2))))))

   (type $function_3
      (func (param (ref eq) (ref eq) (ref eq) (ref eq)) (result (ref eq))))

   (type $closure_3
      (sub $closure
         (struct (field (ref $function_1)) (field (ref $function_3)))))

   (type $dummy_closure_3
      (sub $closure_3
         (struct (field (ref $function_1)) (field (ref $function_3))
            (field (mut (ref null $closure_3))))))

   (type $function_4
      (func (param (ref eq) (ref eq) (ref eq) (ref eq)) (result (ref eq))))

   (type $closure_4
      (sub $closure
         (struct (field (ref $function_1)) (field (ref $function_4)))))

   (type $dummy_closure_4
      (sub $closure_4
         (struct (field (ref $function_1)) (field (ref $function_4))
            (field (mut (ref null $closure_4))))))

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

   (global $caml_global_data (mut (ref $block))
      (array.new $block (i31.new (i32.const 0)) (i32.const 12)))

   (func (export "caml_register_global")
      (param (ref eq)) (param $v (ref eq)) (param (ref eq)) (result (ref eq))
      (local $i i32)
      (local.set $i (i31.get_u (ref.cast i31 (local.get 0))))
      (if (i32.lt_u (local.get $i) (array.len (global.get $caml_global_data)))
         (then
            (array.set $block (global.get $caml_global_data)
               (local.get $i) (local.get $v))))
      (i31.new (i32.const 0)))

   (func $caml_raise_constant (param (ref eq))
      (throw $ocaml_exception (local.get 0)))

   (func $caml_raise_with_arg (param $tag (ref eq)) (param $arg (ref eq))
      (throw $ocaml_exception
         (array.new_fixed $block
            (i31.new (i32.const 0)) (local.get $tag) (local.get $arg))))

   (global $FAILURE_EXN i32 (i32.const 2))

   (func $caml_failwith (param $arg (ref eq))
       (return_call $caml_raise_with_arg
           (array.get $block (global.get $caml_global_data)
              (global.get $FAILURE_EXN))
           (local.get 0)))

   (global $INVALID_EXN i32 (i32.const 3))

   (func $caml_invalid_argument (param $arg (ref eq))
       (return_call $caml_raise_with_arg
           (array.get $block (global.get $caml_global_data)
              (global.get $INVALID_EXN))
           (local.get 0)))

   (data $index_out_of_bounds "index out of bounds")

   (func $caml_bound_error (export "caml_bound_error")
      (return_call $caml_invalid_argument
         (array.new_data $string $index_out_of_bounds
            (i32.const 0) (i32.const 19))))

   (global $ZERO_DIVIDE_EXN i32 (i32.const 5))

   (func (export "caml_raise_zero_divide")
      (return_call $caml_raise_constant
         (array.get $block (global.get $caml_global_data)
                    (global.get $ZERO_DIVIDE_EXN))))

   (global $NOT_FOUND_EXN i32 (i32.const 6))

   (func $caml_raise_not_found
      (return_call $caml_raise_constant
         (array.get $block (global.get $caml_global_data)
                    (global.get $NOT_FOUND_EXN))))

   (func (export "caml_bswap16") (param (ref eq)) (result (ref eq))
      (local $x i32)
      (local.set $x (i31.get_s (ref.cast i31 (local.get 0))))
      (i31.new
         (i32.or
            (i32.shl (i32.and (local.get $x) (i32.const 0xFF)) (i32.const 8))
            (i32.shr_u (i32.and (local.get $x) (i32.const 0x00FF))
               (i32.const 8)))))

   (global $int32_ops (export "int32_ops") (ref $custom_operations)
      (struct.new $custom_operations
         (array.new_fixed $string (i32.const 95) (i32.const 105)) ;; "_i"
         (ref.func $int32_cmp)
         (ref.func $int32_hash)))

   (type $int32
      (sub $custom (struct (field (ref $custom_operations)) (field i32))))

   (func $int32_cmp (param $v1 (ref eq)) (param $v2 (ref eq)) (result i32)
      (local $i1 i32) (local $i2 i32)
      (local.set $i1 (struct.get $int32 1 (ref.cast $int32 (local.get $v1))))
      (local.set $i2 (struct.get $int32 1 (ref.cast $int32 (local.get $v2))))
      (i32.sub (i32.gt_s (local.get $i1) (local.get $i2))
               (i32.lt_s (local.get $i1) (local.get $i2))))

   (func $int32_hash (param $v (ref eq)) (result i32)
      (struct.get $int32 1 (ref.cast $int32 (local.get $v))))

   (func $caml_copy_int32 (param $i i32) (result (ref eq))
      (struct.new $int32 (global.get $int32_ops) (local.get $i)))

   (func (export "caml_int32_bswap") (param (ref eq)) (result (ref eq))
      (local $i i32)
      (local.set $i (struct.get $int32 1 (ref.cast $int32 (local.get 0))))
      (return_call $caml_copy_int32
         (i32.or
            (i32.rotr (i32.and (local.get $i) (i32.const 0x00FF00FF))
                      (i32.const 8))
            (i32.rotl (i32.and (local.get $i) (i32.const 0xFF00FF00))
                      (i32.const 8)))))

   (global $INT32_ERRMSG (ref $string)
      (array.new_fixed $string ;; "Int32.of_string"
         (i32.const 73) (i32.const 110) (i32.const 116) (i32.const 51)
         (i32.const 50) (i32.const 46) (i32.const 111) (i32.const 102)
         (i32.const 95) (i32.const 115) (i32.const 116) (i32.const 114)
         (i32.const 105) (i32.const 110) (i32.const 103)))

   (func (export "caml_int32_of_string") (param $v (ref eq)) (result (ref eq))
      (return_call $caml_copy_int32
         (call $parse_int
            (local.get $v) (i32.const 32) (global.get $INT32_ERRMSG))))

   (export "caml_nativeint_compare" (func $caml_int32_compare))
   (func $caml_int32_compare (export "caml_int32_compare")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (local $i1 i32) (local $i2 i32)
      (local.set $i1 (struct.get $int32 1 (ref.cast $int32 (local.get 0))))
      (local.set $i2 (struct.get $int32 1 (ref.cast $int32 (local.get 1))))
      (i31.new (i32.sub (i32.gt_s (local.get $i1) (local.get $i2))
                        (i32.lt_s (local.get $i1) (local.get $i2)))))

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

   (func $caml_copy_int64 (param $i i64) (result (ref eq))
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

   (global $nativeint_ops (export "nativeint_ops") (ref $custom_operations)
      (struct.new $custom_operations
         (array.new_fixed $string (i32.const 95) (i32.const 110)) ;; "_n"
         (ref.func $int32_cmp)
         (ref.func $int32_hash)))

   (func $caml_copy_nativeint (param $i i32) (result (ref eq))
      (struct.new $int32 (global.get $nativeint_ops) (local.get $i)))

   (global $NATIVEINT_ERRMSG (ref $string)
      (array.new_fixed $string ;; "Nativeint.of_string"
         (i32.const 78) (i32.const 97) (i32.const 116) (i32.const 105)
         (i32.const 118) (i32.const 101) (i32.const 46) (i32.const 111)
         (i32.const 102) (i32.const 95) (i32.const 115) (i32.const 116)
         (i32.const 114) (i32.const 105) (i32.const 110) (i32.const 103)))

   (func (export "caml_nativeint_of_string")
      (param $v (ref eq)) (result (ref eq))
      (return_call $caml_copy_int32
         (call $parse_int
            (local.get $v) (i32.const 32) (global.get $NATIVEINT_ERRMSG))))

   (data $Array_make "Array.make")

   (func $caml_make_vect (export "caml_make_vect")
      (param $n (ref eq)) (param $v (ref eq)) (result (ref eq))
      (local $sz i32) (local $b (ref $block))
      (local.set $sz (i32.add (i31.get_s (ref.cast i31 (local.get $n)))
                              (i32.const 1)))
      (if (i32.lt_s (local.get $sz) (i32.const 1))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $Array_make
                               (i32.const 0) (i32.const 10)))))
      (local.set $b (array.new $block (local.get $v) (local.get $sz)))
      ;; ZZZ float array
      (array.set $block (local.get $b) (i32.const 0) (i31.new (i32.const 0)))
      (local.get $b))

   (export "caml_make_float_vect" (func $caml_floatarray_create))
   (func $caml_floatarray_create (export "caml_floatarray_create")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ float array
      (return_call $caml_make_vect
         (local.get 0) (struct.new $float (f64.const 0))))

   (func (export "caml_array_sub")
      (param $a (ref eq)) (param $i (ref eq)) (param $vlen (ref eq))
      (result (ref eq))
      (local $a1 (ref $block)) (local $a2 (ref $block)) (local $len i32)
      (local.set $len (i31.get_u (ref.cast i31 (local.get $vlen))))
      (local.set $a1 (ref.cast $block (local.get $a)))
      (local.set $a2 (array.new $block (i31.new (i32.const 0))
                        (i32.add (local.get $len) (i32.const 1))))
      (array.copy $block $block
         (local.get $a2) (i32.const 1) (local.get $a1)
         (i32.add (i31.get_u (ref.cast i31 (local.get $i))) (i32.const 1))
         (local.get $len))
      (local.get $a2))

   (func (export "caml_array_append")
      (param $va1 (ref eq)) (param $va2 (ref eq)) (result (ref eq))
      (local $a1 (ref $block)) (local $a2 (ref $block)) (local $a (ref $block))
      (local $l1 i32) (local $l2 i32)
      (local.set $a1 (ref.cast $block (local.get $va1)))
      (local.set $l1 (array.len (local.get $a1)))
      (local.set $a2 (ref.cast $block (local.get $va2)))
      (local.set $l2 (array.len (local.get $a2)))
      (local.set $a
         (array.new $block (i31.new (i32.const 0))
            (i32.sub (i32.add (local.get $l1) (local.get $l2)) (i32.const 1))))
      ;; ZZZ float array
      (array.copy $block $block
         (local.get $a) (i32.const 1) (local.get $a1) (i32.const 1)
         (i32.sub (local.get $l1) (i32.const 1)))
      (array.copy $block $block
         (local.get $a) (i32.const 1) (local.get $a2) (local.get $l1)
         (i32.sub (local.get $l2) (i32.const 1)))
      (local.get $a))

   (func (export "caml_array_concat") (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_array_concat"))
      (unreachable))

   (export "caml_floatarray_blit" (func $caml_array_blit))
   (func $caml_array_blit (export "caml_array_blit")
      (param $a1 (ref eq)) (param $i1 (ref eq))
      (param $a2 (ref eq)) (param $i2 (ref eq))
      (param $len (ref eq))
      (result (ref eq))
      (array.copy $block $block
         (ref.cast $block (local.get $a2))
         (i31.get_s (ref.cast i31 (local.get $i2)))
         (ref.cast $block (local.get $a1))
         (i31.get_s (ref.cast i31 (local.get $i1)))
         (i31.get_s (ref.cast i31 (local.get $len))))
      (i31.new (i32.const 0)))

   (func (export "caml_array_fill")
      (param $a (ref eq)) (param $i (ref eq)) (param $len (ref eq))
      (param $v (ref eq)) (result (ref eq))
      (array.fill $block (ref.cast $block (local.get $a))
         (i31.get_u (ref.cast i31 (local.get $i)))
         (local.get $v)
         (i31.get_u (ref.cast i31 (local.get $len))))
      (i31.new (i32.const 0)))

   (func (export "caml_fs_init") (result (ref eq))
      (i31.new (i32.const 0)))

   (export "caml_sys_time_include_children" (func $caml_sys_time))
   (func $caml_sys_time (export "caml_sys_time")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_sys_time"))
      (i31.new (i32.const 0)))

   (func (export "caml_sys_argv") (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_sys_argv"))
      (array.new_fixed $block (i31.new (i32.const 0))
         (array.new_fixed $string (i32.const 97))))

   (func (export "caml_ml_flush") (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_flush"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_open_descriptor_in")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_open_descriptor_in"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_open_descriptor_out")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_open_descriptor_out"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_pos_in")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_pos_in"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_pos_out")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_pos_out"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_seek_in")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_seek_in"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_seek_in_64")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_seek_in_64"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_seek_out")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_seek_out"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_close_channel")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_close_channel"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_set_channel_name")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_set_channel_name"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_out_channels_list")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_out_channels_list"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_input")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
      (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_input"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_output")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
      (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_output"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_output_bytes")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
      (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_output_bytes"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_input_char")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_input_char"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_input_int")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_input_int"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_input_scan_line")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_input_scan_line"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_output_char")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_output_char"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_output_int")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_output_int"))
      (i31.new (i32.const 0)))

   (func (export "caml_sys_open")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_sys_open"))
      (i31.new (i32.const 0)))

   (func (export "caml_sys_close")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_sys_close"))
      (i31.new (i32.const 0)))

   (func (export "caml_sys_read_directory")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_sys_read_directory"))
      (i31.new (i32.const 0)))

   (func (export "caml_sys_remove")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_sys_remove"))
      (i31.new (i32.const 0)))

   (func (export "caml_sys_rename")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_sys_rename"))
      (i31.new (i32.const 0)))

   (func (export "caml_sys_system_command")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_sys_system_command"))
      (i31.new (i32.const 0)))

   (func (export "caml_sys_random_seed")
      (param (ref eq)) (result (ref eq))
      (local $r externref)
      (local $a (ref $block))
      (local $i i32) (local $n i32)
      (local.set $r (call $random_seed))
      (local.set $n (call $ta_length (local.get $r)))
      (local.set $a
         (array.new $block (i31.new (i32.const 0))
            (i32.add (local.get $n) (i32.const 1))))
      (local.set $i (i32.const 0))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $n))
            (then
               (array.set $block
                  (local.get $a) (i32.add (local.get $i) (i32.const 1))
                  (i31.new (call $ta_get_i32 (local.get $r) (local.get $i))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (local.get $a))

   (func (export "caml_sys_file_exists")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_sys_file_exists"))
      (i31.new (i32.const 0)))

   (data $Unix "Unix")

   (func (export "caml_sys_get_config")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_sys_get_config"))
      (array.new_fixed $block (i31.new (i32.const 0))
         (array.new_data $string $Unix (i32.const 0) (i32.const 4))
         (i31.new (i32.const 32))
         (i31.new (i32.const 0))))

   (func (export "caml_sys_getcwd")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_sys_getcwd"))
      (i31.new (i32.const 0)))

   (func (export "caml_sys_mkdir")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_sys_mkdir"))
      (i31.new (i32.const 0)))

   (func (export "caml_sys_getenv")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_sys_getenv"))
      (call $log_js
         (call $unwrap (call $caml_jsstring_of_string (local.get 0))))
      (call $caml_raise_not_found)
      (i31.new (i32.const 0)))

   (func (export "caml_sys_isatty")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_sys_isatty"))
      (i31.new (i32.const 0)))

   (func (export "caml_terminfo_rows")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_terminfo_rows"))
      (i31.new (i32.const 0)))

   (func (export "caml_sys_const_ostype_cygwin")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_sys_const_ostype_cygwin"))
      (i31.new (i32.const 0)))

   (func (export "caml_sys_const_ostype_win32")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_sys_const_ostype_win32"))
      (i31.new (i32.const 0)))

   (func (export "caml_md5_string")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_md5_string"))
      (i31.new (i32.const 0)))

   (func (export "caml_md5_chan")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_md5_chan"))
      (i31.new (i32.const 0)))

   (func (export "caml_register_named_value")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_register_named_value"))
      (call $log_js
         (call $unwrap (call $caml_jsstring_of_string (local.get $0))))
      (i31.new (i32.const 0)))

   (func (export "caml_dynlink_close_lib")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_dynlink_close_lib"))
      (i31.new (i32.const 0)))

   (func (export "caml_dynlink_lookup_symbol")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_dynlink_lookup_symbol"))
      (i31.new (i32.const 0)))

   (func (export "caml_new_lex_engine")
      (param (ref eq) (ref eq) (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_new_lex_engine"))
      (i31.new (i32.const 0)))

   (func (export "caml_lex_engine")
      (param (ref eq) (ref eq) (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_lex_engine"))
      (i31.new (i32.const 0)))

   (func (export "caml_gc_quick_stat")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_gc_quick_stat"))
      (i31.new (i32.const 0)))

   (func (export "caml_final_register")
     (param (ref eq) (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))

   (func $parse_sign_and_base (param $s (ref $string)) (result i32 i32 i32 i32)
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

   (func $parse_digit (param $c i32) (result i32)
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

   (func $parse_int
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

   (func (export "caml_sys_exit") (param (ref eq)) (result (ref eq))
      (throw $ocaml_exit (i31.get_s (ref.cast i31 (local.get 0)))))

   (global $caml_oo_last_id (mut i32) (i32.const 0))

   (func (export "caml_set_oo_id") (param (ref eq)) (result (ref eq))
      (local $id i32)
      (local.set $id (global.get $caml_oo_last_id))
      (array.set $block (ref.cast $block (local.get 0)) (i32.const 2)
         (i31.new (local.get $id)))
      (global.set $caml_oo_last_id (i32.add (local.get $id) (i32.const 1)))
      (local.get $0))

   (func (export "caml_fresh_oo_id") (param (ref eq)) (result (ref eq))
      (local $id i32)
      (local.set $id (global.get $caml_oo_last_id))
      (global.set $caml_oo_last_id (i32.add (local.get $id) (i32.const 1)))
      (i31.new (local.get $id)))

   (func (export "caml_obj_dup") (param (ref eq)) (result (ref eq))
      ;; ZZZ Deal with non-block values?
      (local $orig (ref $block))
      (local $res (ref $block))
      (local $len i32)
      (local.set $orig (ref.cast $block (local.get 0)))
      (local.set $len (array.len (local.get $orig)))
      (local.set $res
         (array.new $block (array.get $block (local.get $orig) (i32.const 0))
            (local.get $len)))
      (array.copy $block $block
         (local.get $res) (i32.const 1) (local.get $orig) (i32.const 1)
         (i32.sub (local.get $len) (i32.const 1)))
      (local.get $res))

   (func (export "caml_obj_block")
      (param $tag (ref eq)) (param $size (ref eq)) (result (ref eq))
      (local $res (ref $block))
      ;; ZZZ float array / specific types
      (local.set $res
         (array.new $block
            (i31.new (i32.const 0))
            (i32.add (i31.get_s (ref.cast i31 (local.get $size)))
                     (i32.const 1))))
      (array.set $block (local.get $res) (i32.const 0) (local.get $tag))
      (local.get $res))

   (global $forcing_tag i32 (i32.const 244))
   (global $cont_tag i32 (i32.const 245))
   (global $lazy_tag i32 (i32.const 246))
   (global $closure_tag i32 (i32.const 247))
   (global $object_tag i32 (i32.const 248))
   (global $forward_tag i32 (i32.const 250))
   (global $abstract_tag i32 (i32.const 251))
   (global $string_tag i32 (i32.const 252))
   (global $float_tag i32 (i32.const 253))
   (global $double_array_tag i32 (i32.const 254))
   (global $custom_tag i32 (i32.const 255))

   (func (export "caml_lazy_make_forward")
      (param (ref eq)) (result (ref eq))
      (array.new_fixed $block (i31.new (global.get $forward_tag))
         (local.get $0)))

   (func $obj_update_tag
      (param (ref eq)) (param $o i32) (param $n i32) (result i32)
      (local $b (ref $block))
      (local.set $b (ref.cast $block (local.get $0)))
      (if (result i32) (ref.eq (array.get $block (local.get $b) (i32.const 0))
                               (i31.new (local.get $o)))
         (then
            (array.set $block (local.get $b) (i32.const 0)
               (i31.new (local.get $n)))
            (i32.const 1))
         (else
            (i32.const 0))))

   (func (export "caml_lazy_reset_to_lazy") (param (ref eq)) (result (ref eq))
      (drop (call $obj_update_tag (local.get 0)
               (global.get $forcing_tag) (global.get $lazy_tag)))
      (i31.new (i32.const 0)))

   (func (export "caml_lazy_update_to_forward") (param (ref eq)) (result (ref eq))
      (drop (call $obj_update_tag (local.get 0)
               (global.get $forcing_tag) (global.get $forward_tag)))
      (i31.new (i32.const 0)))

   (func (export "caml_lazy_update_to_forcing")
      (param (ref eq)) (result (ref eq))
      (if (ref.test $block (local.get $0))
         (then
            (if (call $obj_update_tag (local.get 0)
                   (global.get $lazy_tag) (global.get $forcing_tag))
               (then (return (i31.new (i32.const 0)))))))
      (i31.new (i32.const 1)))

   (func (export "caml_obj_tag") (param $v (ref eq)) (result (ref eq))
      (if (ref.test i31 (local.get $v))
         (then (return (i31.new (i32.const 1000)))))
      (drop (block $not_block (result (ref eq))
         (return (array.get $block
                    (br_on_cast_fail $not_block $block (local.get $v))
                    (i32.const 0)))))
      (if (ref.test $string (local.get $v))
         (then (return (i31.new (global.get $string_tag)))))
      (if (ref.test $float (local.get $v))
         (then (return (i31.new (global.get $float_tag)))))
      (if (ref.test $custom (local.get $v))
         (then (return (i31.new (global.get $custom_tag)))))
      (if (ref.test $closure (local.get $v))
         (then (return (i31.new (global.get $closure_tag)))))
      ;; ZZZ float array
      (if (ref.test $js (local.get $v))
         (then (return (i31.new (global.get $abstract_tag)))))
      (unreachable))

   (func (export "caml_obj_make_forward")
      (param $b (ref eq)) (param $v (ref eq)) (result (ref eq))
      (local $block (ref $block))
      (local.set $block (ref.cast $block (local.get $b)))
      (array.set $block (local.get $block)
         (i32.const 0) (i31.new (global.get $forward_tag)))
      (array.set $block (local.get $block) (i32.const 1) (local.get $v))
      (i31.new (i32.const 0)))

   (func (export "caml_alloc_dummy") (param $size (ref eq)) (result (ref eq))
      (array.new $block (i31.new (i32.const 0))
                 (i32.add (i31.get_u (ref.cast i31 (local.get $size)))
                          (i32.const 1))))

   (func (export "caml_alloc_dummy_function") (param $size (ref eq))  (param (ref eq)) (result (ref eq))
      ;;ZZZ
      (call $log_js (string.const "caml_alloc_dummy_function"))
      (array.new $block (i31.new (i32.const 0))
                 (i32.add (i31.get_u (ref.cast i31 (local.get $size)))
                          (i32.const 1))))

   (func (export "caml_update_dummy")
      (param $dummy (ref eq)) (param $newval (ref eq)) (result (ref eq))
      (local $i i32)
      (local $dst (ref $block)) (local $src (ref $block))
      (drop (block $not_block (result (ref eq))
         (local.set $dst
            (br_on_cast_fail $not_block $block (local.get $dummy)))
         (local.set $src (ref.cast $block (local.get $newval)))
         (array.copy $block $block
            (local.get $dst) (i32.const 0) (local.get $src) (i32.const 0)
            (array.len (local.get $dst)))
         (return (i31.new (i32.const 0)))))
      (drop (block $not_closure_1 (result (ref eq))
         (struct.set $dummy_closure_1 1
            (br_on_cast_fail $not_closure_1 $dummy_closure_1 (local.get $dummy))
            (ref.cast $closure (local.get $newval)))
         (return (i31.new (i32.const 0)))))
      (drop (block $not_closure_2 (result (ref eq))
         (struct.set $dummy_closure_2 2
            (br_on_cast_fail $not_closure_2 $dummy_closure_2 (local.get $dummy))
            (ref.cast $closure_2 (local.get $newval)))
         (return (i31.new (i32.const 0)))))
      (drop (block $not_closure_3 (result (ref eq))
         (struct.set $dummy_closure_3 2
            (br_on_cast_fail $not_closure_3 $dummy_closure_3 (local.get $dummy))
            (ref.cast $closure_3 (local.get $newval)))
         (return (i31.new (i32.const 0)))))
      (drop (block $not_closure_4 (result (ref eq))
         (struct.set $dummy_closure_4 2
            (br_on_cast_fail $not_closure_4 $dummy_closure_4 (local.get $dummy))
            (ref.cast $closure_4 (local.get $newval)))
         (return (i31.new (i32.const 0)))))
      ;; ZZZ float array
      (unreachable))

   (export "caml_bytes_equal" (func $caml_string_equal))
   (func $caml_string_equal (export "caml_string_equal")
      (param $p1 (ref eq)) (param $p2 (ref eq)) (result (ref eq))
      (local $s1 (ref $string)) (local $s2 (ref $string))
      (local $len i32) (local $i i32)
      (if (ref.eq (local.get $p1) (local.get $p2))
         (then (return (i31.new (i32.const 1)))))
      (local.set $s1 (ref.cast $string (local.get $p1)))
      (local.set $s2 (ref.cast $string (local.get $p2)))
      (local.set $len (array.len $string (local.get $s1)))
      (if (i32.ne (local.get $len) (array.len $string (local.get $s2)))
         (then (return (i31.new (i32.const 0)))))
      (local.set $i (i32.const 0))
      (loop $loop
         (if (i32.lt_s (local.get $i) (local.get $len))
            (then
               (if (i32.ne (array.get_u $string (local.get $s1) (local.get $i))
                           (array.get_u $string (local.get $s2) (local.get $i)))
                  (then (return (i31.new (i32.const 0)))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (i31.new (i32.const 1)))

   (export "caml_bytes_notequal" (func $caml_string_notequal))
   (func $caml_string_notequal (export "caml_string_notequal")
      (param $p1 (ref eq)) (param $p2 (ref eq)) (result (ref eq))
      (return
         (i31.new (i32.eqz (i31.get_u (ref.cast i31
            (call $caml_string_equal (local.get $p1) (local.get $p2))))))))

   (func $string_compare
      (param $p1 (ref eq)) (param $p2 (ref eq)) (result i32)
      (local $s1 (ref $string)) (local $s2 (ref $string))
      (local $l1 i32) (local $l2 i32) (local $len i32) (local $i i32)
      (local $c1 i32) (local $c2 i32)
      (if (ref.eq (local.get $p1) (local.get $p2))
         (then (return (i32.const 0))))
      (local.set $s1 (ref.cast $string (local.get $p1)))
      (local.set $s2 (ref.cast $string (local.get $p2)))
      (local.set $l1 (array.len $string (local.get $s1)))
      (local.set $l2 (array.len $string (local.get $s2)))
      (local.set $len (select (local.get $l1) (local.get $l2)
                          (i32.le_u (local.get $l1) (local.get $l2))))
      (local.set $i (i32.const 0))
      (loop $loop
         (if (i32.lt_s (local.get $i) (local.get $len))
            (then
               (local.set $c1
                  (array.get_u $string (local.get $s1) (local.get $i)))
               (local.set $c2
                  (array.get_u $string (local.get $s2) (local.get $i)))
               (if (i32.lt_u (local.get $c1) (local.get $c2))
                  (then (return (i32.const -1))))
               (if (i32.gt_u (local.get $c1) (local.get $c2))
                  (then (return (i32.const 1))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (if (i32.lt_u (local.get $l1) (local.get $l2))
         (then (return (i32.const -1))))
      (if (i32.gt_u (local.get $l1) (local.get $l2))
         (then (return (i32.const 1))))
      (i32.const 0))

   (export "caml_bytes_compare" (func $caml_string_compare))
   (func $caml_string_compare (export "caml_string_compare")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (i31.new (call $string_compare (local.get 0) (local.get 1))))

   (export "caml_bytes_lessequal" (func $caml_string_lessequal))
   (func $caml_string_lessequal (export "caml_string_lessequal")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (i31.new (i32.le_s (call $string_compare (local.get 0) (local.get 1))
                         (i32.const 0))))

   (export "caml_bytes_lessthan" (func $caml_string_lessthan))
   (func $caml_string_lessthan (export "caml_string_lessthan")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (i31.new (i32.lt_s (call $string_compare (local.get 0) (local.get 1))
                         (i32.const 0))))

   (export "caml_bytes_greaterequal" (func $caml_string_greaterequal))
   (func $caml_string_greaterequal (export "caml_string_greaterequal")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (i31.new (i32.ge_s (call $string_compare (local.get 0) (local.get 1))
                         (i32.const 0))))

   (export "caml_bytes_greaterthan" (func $caml_string_greaterthan))
   (func $caml_string_greaterthan (export "caml_string_greaterthan")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (i31.new (i32.gt_s (call $string_compare (local.get 0) (local.get 1))
                         (i32.const 0))))

   (export "caml_bytes_of_string" (func $caml_string_of_bytes))
   (func $caml_string_of_bytes (export "caml_string_of_bytes")
      (param $v (ref eq)) (result (ref eq))
      (local.get $v))

   (data $Bytes_create "Bytes.create")

   (func (export "caml_create_bytes")
      (param $len (ref eq)) (result (ref eq))
      (local $l i32)
      (local.set $l (i31.get_u (ref.cast i31 (local.get $len))))
      (if (i32.lt_s (local.get $l) (i32.const 0))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $Bytes_create
                               (i32.const 0) (i32.const 12)))))
      (array.new $string (i32.const 0) (local.get $l)))

   (export "caml_blit_bytes" (func $caml_blit_string))
   (func $caml_blit_string (export "caml_blit_string")
      (param $v1 (ref eq)) (param $i1 (ref eq))
      (param $v2 (ref eq)) (param $i2 (ref eq))
      (param $n (ref eq)) (result (ref eq))
      (array.copy $string $string
         (ref.cast $string (local.get $v2))
         (i31.get_s (ref.cast i31 (local.get $i2)))
         (ref.cast $string (local.get $v1))
         (i31.get_s (ref.cast i31 (local.get $i1)))
         (i31.get_s (ref.cast i31 (local.get $n))))
      (i31.new (i32.const 0)))

   (func (export "caml_fill_bytes")
      (param $v (ref eq)) (param $offset (ref eq))
      (param $len (ref eq)) (param $init (ref eq))
      (result (ref eq))
(;ZZZ V8 bug
      (array.fill $string (ref.cast $string (local.get $v))
         (i31.get_u (ref.cast i31 (local.get $offset)))
         (i31.get_u (ref.cast i31 (local.get $init)))
         (i31.get_u (ref.cast i31 (local.get $len))))
;)
      (local $s (ref $string)) (local $i i32) (local $limit i32) (local $c i32)
      (local.set $s (ref.cast $string (local.get $v)))
      (local.set $i (i31.get_u (ref.cast i31 (local.get $offset))))
      (local.set $limit
         (i32.add (local.get $i) (i31.get_u (ref.cast i31 (local.get $len)))))
      (local.set $c (i31.get_u (ref.cast i31 (local.get $init))))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $limit))
            (then
               (array.set $string (local.get $s) (local.get $i) (local.get $c))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (i31.new (i32.const 0)))

   (export "caml_string_get16" (func $caml_bytes_get16))
   (func $caml_bytes_get16 (export "caml_bytes_get16")
      (param $v (ref eq)) (param $i (ref eq)) (result (ref eq))
      (local $s (ref $string)) (local $p i32)
      (local.set $s (ref.cast $string (local.get $v)))
      (local.set $p (i31.get_s (ref.cast i31 (local.get $i))))
      (if (i32.lt_s (local.get $p) (i32.const 0))
         (then (call $caml_bound_error)))
      (if (i32.ge_u (i32.add (local.get $p) (i32.const 1))
                    (array.len (local.get $s)))
         (then (call $caml_bound_error)))
      (i31.new (i32.or
                  (array.get_u $string (local.get $s) (local.get $p))
                  (i32.shl (array.get_u $string (local.get $s)
                              (i32.add (local.get $p) (i32.const 1)))
                           (i32.const 8)))))

   (export "caml_string_get32" (func $caml_bytes_get32))
   (func $caml_bytes_get32 (export "caml_bytes_get32")
      (param $v (ref eq)) (param $i (ref eq)) (result (ref eq))
      (local $s (ref $string)) (local $p i32)
      (local.set $s (ref.cast $string (local.get $v)))
      (local.set $p (i31.get_s (ref.cast i31 (local.get $i))))
      (if (i32.lt_s (local.get $p) (i32.const 0))
         (then (call $caml_bound_error)))
      (if (i32.ge_u (i32.add (local.get $p) (i32.const 3))
                    (array.len (local.get $s)))
         (then (call $caml_bound_error)))
      (return_call $caml_copy_int32
         (i32.or
            (i32.or
               (array.get_u $string (local.get $s) (local.get $p))
               (i32.shl (array.get_u $string (local.get $s)
                           (i32.add (local.get $p) (i32.const 1)))
                        (i32.const 8)))
            (i32.or
               (i32.shl (array.get_u $string (local.get $s)
                           (i32.add (local.get $p) (i32.const 2)))
                        (i32.const 16))
               (i32.shl (array.get_u $string (local.get $s)
                           (i32.add (local.get $p) (i32.const 3)))
                        (i32.const 24))))))

   (export "caml_string_get64" (func $caml_bytes_get64))
   (func $caml_bytes_get64 (export "caml_bytes_get64")
      (param $v (ref eq)) (param $i (ref eq)) (result (ref eq))
      (local $s (ref $string)) (local $p i32)
      (local.set $s (ref.cast $string (local.get $v)))
      (local.set $p (i31.get_s (ref.cast i31 (local.get $i))))
      (if (i32.lt_s (local.get $p) (i32.const 0))
         (then (call $caml_bound_error)))
      (if (i32.ge_u (i32.add (local.get $p) (i32.const 7))
                    (array.len (local.get $s)))
         (then (call $caml_bound_error)))
      (return_call $caml_copy_int64
         (i64.or
            (i64.or
               (i64.or
                  (i64.extend_i32_u
                     (array.get_u $string (local.get $s) (local.get $p)))
                  (i64.shl (i64.extend_i32_u
                              (array.get_u $string (local.get $s)
                                 (i32.add (local.get $p) (i32.const 1))))
                           (i64.const 8)))
               (i64.or
                  (i64.shl (i64.extend_i32_u
                              (array.get_u $string (local.get $s)
                                 (i32.add (local.get $p) (i32.const 2))))
                           (i64.const 16))
                  (i64.shl (i64.extend_i32_u
                              (array.get_u $string (local.get $s)
                                 (i32.add (local.get $p) (i32.const 3))))
                           (i64.const 24))))
            (i64.or
               (i64.or
                  (i64.shl (i64.extend_i32_u
                              (array.get_u $string (local.get $s)
                                 (i32.add (local.get $p) (i32.const 4))))
                           (i64.const 32))
                  (i64.shl (i64.extend_i32_u
                              (array.get_u $string (local.get $s)
                                 (i32.add (local.get $p) (i32.const 5))))
                           (i64.const 40)))
               (i64.or
                  (i64.shl (i64.extend_i32_u
                              (array.get_u $string (local.get $s)
                                 (i32.add (local.get $p) (i32.const 6))))
                           (i64.const 48))
                  (i64.shl (i64.extend_i32_u
                              (array.get_u $string (local.get $s)
                                 (i32.add (local.get $p) (i32.const 7))))
                           (i64.const 56)))))))

   (func (export "caml_bytes_set16")
      (param (ref eq) (ref eq) (ref eq)) (result (ref eq))
      (local $s (ref $string)) (local $p i32) (local $v i32)
      (local.set $s (ref.cast $string (local.get 0)))
      (local.set $p (i31.get_s (ref.cast i31 (local.get 1))))
      (local.set $v (i31.get_s (ref.cast i31 (local.get 2))))
      (if (i32.lt_s (local.get $p) (i32.const 0))
         (then (call $caml_bound_error)))
      (if (i32.ge_u (i32.add (local.get $p) (i32.const 1))
                    (array.len (local.get $s)))
         (then (call $caml_bound_error)))
      (array.set $string (local.get $s) (local.get $p) (local.get $v))
      (array.set $string (local.get $s)
         (i32.add (local.get $p) (i32.const 1))
         (i32.shr_u (local.get $v) (i32.const 8)))
      (i31.new (i32.const 0)))

   (func (export "caml_bytes_set32")
      (param (ref eq) (ref eq) (ref eq)) (result (ref eq))
      (local $s (ref $string)) (local $p i32) (local $v i32)
      (local.set $s (ref.cast $string (local.get 0)))
      (local.set $p (i31.get_s (ref.cast i31 (local.get 1))))
      (local.set $v (struct.get $int32 1 (ref.cast $int32 (local.get 2))))
      (if (i32.lt_s (local.get $p) (i32.const 0))
         (then (call $caml_bound_error)))
      (if (i32.ge_u (i32.add (local.get $p) (i32.const 3))
                    (array.len (local.get $s)))
         (then (call $caml_bound_error)))
      (array.set $string (local.get $s) (local.get $p) (local.get $v))
      (array.set $string (local.get $s)
         (i32.add (local.get $p) (i32.const 1))
         (i32.shr_u (local.get $v) (i32.const 8)))
      (array.set $string (local.get $s)
         (i32.add (local.get $p) (i32.const 2))
         (i32.shr_u (local.get $v) (i32.const 16)))
      (array.set $string (local.get $s)
         (i32.add (local.get $p) (i32.const 3))
         (i32.shr_u (local.get $v) (i32.const 24)))
      (i31.new (i32.const 0)))

   (func (export "caml_bytes_set64")
      (param (ref eq) (ref eq) (ref eq)) (result (ref eq))
      (local $s (ref $string)) (local $p i32) (local $v i64)
      (local.set $s (ref.cast $string (local.get 0)))
      (local.set $p (i31.get_s (ref.cast i31 (local.get 1))))
      (local.set $v (struct.get $int64 1 (ref.cast $int64 (local.get 2))))
      (if (i32.lt_s (local.get $p) (i32.const 0))
         (then (call $caml_bound_error)))
      (if (i32.ge_u (i32.add (local.get $p) (i32.const 3))
                    (array.len (local.get $s)))
         (then (call $caml_bound_error)))
      (array.set $string (local.get $s) (local.get $p)
         (i32.wrap_i64 (local.get $v)))
      (array.set $string (local.get $s)
         (i32.add (local.get $p) (i32.const 1))
         (i32.wrap_i64 (i64.shr_u (local.get $v) (i64.const 8))))
      (array.set $string (local.get $s)
         (i32.add (local.get $p) (i32.const 2))
         (i32.wrap_i64 (i64.shr_u (local.get $v) (i64.const 16))))
      (array.set $string (local.get $s)
         (i32.add (local.get $p) (i32.const 3))
         (i32.wrap_i64 (i64.shr_u (local.get $v) (i64.const 24))))
      (array.set $string (local.get $s)
         (i32.add (local.get $p) (i32.const 4))
         (i32.wrap_i64 (i64.shr_u (local.get $v) (i64.const 32))))
      (array.set $string (local.get $s)
         (i32.add (local.get $p) (i32.const 5))
         (i32.wrap_i64 (i64.shr_u (local.get $v) (i64.const 40))))
      (array.set $string (local.get $s)
         (i32.add (local.get $p) (i32.const 6))
         (i32.wrap_i64 (i64.shr_u (local.get $v) (i64.const 48))))
      (array.set $string (local.get $s)
         (i32.add (local.get $p) (i32.const 7))
         (i32.wrap_i64 (i64.shr_u (local.get $v) (i64.const 56))))
      (i31.new (i32.const 0)))

   (type $int_array (array (mut i32)))
   (type $block_array (array (mut (ref $block))))
   (type $compare_stack
      (struct (field (mut i32))          ;; position in stack
              (field (ref $block_array)) ;; first value
              (field (ref $block_array)) ;; second value
              (field (ref $int_array)))) ;; position in value

   (global $dummy_block (ref $block)
      (array.new $block (i31.new (i32.const 0)) (i32.const 0)))

   (global $default_compare_stack (ref $compare_stack)
      (struct.new $compare_stack (i32.const -1)
         (array.new $block_array (global.get $dummy_block) (i32.const 8))
         (array.new $block_array (global.get $dummy_block) (i32.const 8))
         (array.new $int_array (i32.const 0) (i32.const 8))))

   (func $compare_stack_is_not_empty
      (param $stack (ref $compare_stack)) (result i32)
      (i32.ge_s (struct.get $compare_stack 0 (local.get $stack)) (i32.const 0)))

   (func $pop_compare_stack (param $stack (ref $compare_stack))
      (result (ref eq)) (result (ref eq))
      (local $i i32) (local $p i32) (local $p' i32)
      (local $v1 (ref $block)) (local $v2 (ref $block))
      (local.set $i (struct.get $compare_stack 0 (local.get $stack)))
      (local.set $p
         (array.get $int_array (struct.get $compare_stack 3 (local.get $stack))
            (local.get $i)))
      (local.set $p' (i32.add (local.get $p) (i32.const 1)))
      (array.set $int_array (struct.get $compare_stack 3 (local.get $stack))
            (local.get $i) (local.get $p'))
      (local.set $v1
         (array.get $block_array
            (struct.get $compare_stack 1 (local.get $stack)) (local.get $i)))
      (local.set $v2
         (array.get $block_array
            (struct.get $compare_stack 2 (local.get $stack)) (local.get $i)))
      (if (i32.eq (local.get $p') (array.len (local.get $v1)))
         (then
            (array.set $block_array
               (struct.get $compare_stack 1 (local.get $stack))
               (local.get $i) (global.get $dummy_block))
            (array.set $block_array
               (struct.get $compare_stack 2 (local.get $stack))
               (local.get $i) (global.get $dummy_block))
            (struct.set $compare_stack 0 (local.get $stack)
               (i32.sub (local.get $i) (i32.const 1)))))
      (tuple.make (array.get $block (local.get $v1) (local.get $p))
                  (array.get $block (local.get $v2) (local.get $p))))

   (func $push_compare_stack (param $stack (ref $compare_stack))
      (param $v1 (ref $block)) (param $v2 (ref $block)) (param $p i32)
      (result (ref $compare_stack))
      (local $i i32) (local $len i32) (local $len' i32)
      (local $stack' (ref $compare_stack))
      (local.set $i
         (i32.add (struct.get $compare_stack 0 (local.get $stack))
            (i32.const 1)))
      (local.set $len
         (array.len (struct.get $compare_stack 1 (local.get $stack))))
      (if (i32.ge_u (local.get $i) (local.get $len))
         (then
            (local.set $len' (i32.shl (local.get $len) (i32.const 1)))
            (local.set $stack'
               (struct.new $compare_stack (local.get $i)
                  (array.new $block_array
                     (global.get $dummy_block) (i32.const 8))
                  (array.new $block_array
                     (global.get $dummy_block) (i32.const 8))
                  (array.new $int_array (i32.const 0) (i32.const 8))))
            (array.copy $block_array $block_array
               (struct.get $compare_stack 1 (local.get $stack')) (i32.const 0)
               (struct.get $compare_stack 1 (local.get $stack)) (i32.const 0)
               (local.get $len))
            (array.copy $block_array $block_array
               (struct.get $compare_stack 2 (local.get $stack')) (i32.const 0)
               (struct.get $compare_stack 2 (local.get $stack)) (i32.const 0)
               (local.get $len))
            (array.copy $int_array $int_array
               (struct.get $compare_stack 3 (local.get $stack')) (i32.const 0)
               (struct.get $compare_stack 3 (local.get $stack)) (i32.const 0)
               (local.get $len))
            (local.set $stack (local.get $stack'))))
      (struct.set $compare_stack 0 (local.get $stack) (local.get $i))
      (array.set $block_array (struct.get $compare_stack 1 (local.get $stack))
         (local.get $i) (local.get $v1))
      (array.set $block_array (struct.get $compare_stack 2 (local.get $stack))
         (local.get $i) (local.get $v2))
      (array.set $int_array (struct.get $compare_stack 3 (local.get $stack))
         (local.get $i) (local.get $p))
      (local.get $stack))

   (global $unordered i32 (i32.const 0x80000000))

   (func $compare_strings
      (param $s1 (ref $string)) (param $s2 (ref $string)) (result i32)
      (local $l1 i32) (local $l2 i32) (local $len i32) (local $i i32)
      (local $c1 i32) (local $c2 i32)
      (if (ref.eq (local.get $s1) (local.get $s2))
         (then (return (i32.const 0))))
      (local.set $l1 (array.len $string (local.get $s1)))
      (local.set $l2 (array.len $string (local.get $s2)))
      (local.set $len (select (local.get $l1) (local.get $l2)
                         (i32.le_u (local.get $l1) (local.get $l2))))
      (local.set $i (i32.const 0))
      (loop $loop
         (if (i32.lt_s (local.get $i) (local.get $len))
            (then
               (local.set $c1
                  (array.get_u $string (local.get $s1) (local.get $i)))
               (local.set $c2
                  (array.get_u $string (local.get $s2) (local.get $i)))
               (if (i32.ne (local.get $c1) (local.get $c2))
                  (then
                     (if (i32.le_u (local.get $c1) (local.get $c2))
                        (then (return (i32.const -1)))
                        (else (return (i32.const 1))))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (i32.sub (local.get $l1) (local.get $l2)))

   (func $compare_val
      (param $v1 (ref eq)) (param $v2 (ref eq)) (param $total i32)
      (result i32)
      (local $stack (ref $compare_stack)) (local $n i32) (local $res i32)
      (local.set $stack (global.get $default_compare_stack))
      (struct.set $compare_stack 0 (local.get $stack) (i32.const -1))
      (local.set $res
         (call $do_compare_val
            (local.get $stack) (local.get $v1) (local.get $v2)
            (local.get $total)))
;;      (if (i32.gt_s (local.get $res) (i32.const 0)) (then (local.set $res (i32.const 1))))
;;      (if (i32.lt_s (local.get $res) (i32.const 0)) (then (local.set $res (i32.const -1))))
;;      (call $log (local.get $res))
      ;; clear stack (to avoid memory leaks)
      (local.set $n (struct.get $compare_stack 0 (local.get $stack)))
      (if (i32.ge_s (local.get $n) (i32.const 0))
         (then
(; ZZZ
            (local.set $n (i32.add (local.get $n) (i32.const 1)))
            (array.fill $block_array
               (struct.get $compare_stack 1 (local.get $stack))
               (i32.const 0) (global.get $dummy_block) (local.get $n))
            (array.fill $block_array
               (struct.get $compare_stack 2 (local.get $stack))
               (i32.const 0) (global.get $dummy_block) (local.get $n))
;)
            (loop $loop
               (if (i32.ge_s (local.get $n) (i32.const 0))
                  (then
                     (array.set $block_array
                        (struct.get $compare_stack 1 (local.get $stack))
                        (local.get $n) (global.get $dummy_block))
                     (array.set $block_array
                        (struct.get $compare_stack 2 (local.get $stack))
                        (local.get $n) (global.get $dummy_block))
                     (local.set $n (i32.sub (local.get $n) (i32.const 1)))
                     (br $loop))))
          ))
      (local.get $res))

   (func $do_compare_val
      (param $stack (ref $compare_stack))
      (param $v1 (ref eq)) (param $v2 (ref eq)) (param $total i32) (result i32)
      (local $i1 (ref i31)) (local $i2 (ref i31))
      (local $b1 (ref $block)) (local $b2 (ref $block))
      (local $t1 i32) (local $t2 i32)
      (local $s1 i32) (local $s2 i32)
      (local $f1 f64) (local $f2 f64)
      (local $str1 (ref $string)) (local $str2 (ref $string))
      (local $c1 (ref $custom)) (local $c2 (ref $custom))
      (local $tuple ((ref eq) (ref eq)))
      (local $res i32)
      (loop $loop
         (block $next_item
            (br_if $next_item
               (i32.and (ref.eq (local.get $v1) (local.get $v2))
                        (local.get $total)))
            (drop (block $v1_is_not_int (result (ref eq))
               (local.set $i1
                  (br_on_cast_fail $v1_is_not_int i31 (local.get $v1)))
               (br_if $next_item (ref.eq (local.get $v1) (local.get $v2)))
               (drop (block $v2_is_not_int (result (ref eq))
                  (local.set $i2
                     (br_on_cast_fail $v2_is_not_int i31 (local.get $v2)))
                  ;; v1 and v2 are both integers
                  (return (i32.sub (i31.get_s (local.get $i1))
                                   (i31.get_s (local.get $i2))))))
               ;; check for forward tag
               (drop (block $v2_not_forward (result (ref eq))
                  (local.set $b2
                     (br_on_cast_fail $v2_not_forward $block (local.get $v2)))
                  (local.set $t2
                     (i31.get_u (ref.cast i31 (array.get $block (local.get $b2)
                                                 (i32.const 0)))))
                  (if (i32.eq (local.get $t2) (global.get $forward_tag))
                     (then
                        (local.set $v2
                           (array.get $block (local.get $b2) (i32.const 1)))
                        (br $loop)))
                  (i31.new (i32.const 1))))
               ;; ZZZ custom tag
               ;; v1 long < v2 block
               (return (i32.const -1))))
            (if (ref.test i31 (local.get $v2))
               (then
                  ;; check for forward tag
                  (drop (block $v1_not_forward (result (ref eq))
                     (local.set $b1
                        (br_on_cast_fail
                           $v1_not_forward $block (local.get $v1)))
                     (local.set $t1
                        (i31.get_u (ref.cast i31
                                      (array.get $block (local.get $b1)
                                         (i32.const 0)))))
                     (if (i32.eq (local.get $t1) (global.get $forward_tag))
                        (then
                           (local.set $v1
                              (array.get $block (local.get $b1) (i32.const 1)))
                           (br $loop)))
                     (i31.new (i32.const 1))))
                  ;; ZZZ custom tag
                  ;; v1 block > v1 long
                  (return (i32.const 1))))
            (drop (block $v1_not_block (result (ref eq))
               (local.set $b1
                  (br_on_cast_fail $v1_not_block $block (local.get $v1)))
               (local.set $t1
                  (i31.get_u (ref.cast i31 (array.get $block (local.get $b1)
                                              (i32.const 0)))))
               (drop (block $v2_not_block (result (ref eq))
                  (local.set $b2
                     (br_on_cast_fail $v2_not_block $block (local.get $v2)))
                  (local.set $t2
                     (i31.get_u (ref.cast i31 (array.get $block (local.get $b2)
                                                 (i32.const 0)))))
                  (if (i32.ne (local.get $t1) (local.get $t2))
                     (then
                        ;; check for forward tag
                        (if (i32.eq (local.get $t1) (global.get $forward_tag))
                           (then
                              (local.set $v1
                                 (array.get $block
                                    (local.get $b1) (i32.const 1)))
                              (br $loop)))
                        (if (i32.eq (local.get $t2) (global.get $forward_tag))
                           (then
                              (local.set $v2
                                 (array.get
                                    $block (local.get $b2) (i32.const 1)))
                              (br $loop)))
                        ;; compare tags
                        (return (i32.sub (local.get $t1) (local.get $t2)))))
                  ;; forward tag
                  (if (i32.eq (local.get $t1) (global.get $forward_tag))
                     (then
                        (local.set $v1
                           (array.get $block (local.get $b1) (i32.const 1)))
                        (local.set $v2
                           (array.get $block (local.get $b2) (i32.const 1)))
                        (br $loop)))
                  ;; ZZZ object tag
                  (local.set $s1 (array.len (local.get $b1)))
                  (local.set $s2 (array.len (local.get $b2)))
                  ;; compare size first
                  (if (i32.ne (local.get $s1) (local.get $s2))
                     (then (return (i32.sub (local.get $s1) (local.get $s2)))))
                  (br_if $next_item (i32.eq (local.get $s1) (i32.const 1)))
                  (if (i32.gt_u (local.get $s1) (i32.const 2))
                     (then
                       (local.set $stack
                          (call $push_compare_stack (local.get $stack)
                             (local.get $b1) (local.get $b2) (i32.const 2)))))
                  (local.set $v1
                     (array.get $block (local.get $b1) (i32.const 1)))
                  (local.set $v2
                     (array.get $block (local.get $b2) (i32.const 1)))
                  (br $loop)))
               ;; check for forward tag
               (if (i32.eq (local.get $t1) (global.get $forward_tag))
                  (then
                     (local.set $v1
                        (array.get $block (local.get $b1) (i32.const 1)))
                     (br $loop)))
               ;; v1 float array > v2 not represented as block
               (if (i32.eq (local.get $t1) (global.get $double_array_tag))
                   (then (return (i32.const 1))))
               (return (i32.const -1))))
            (drop (block $v1_not_float (result (ref eq))
               (local.set $f1
                  (struct.get $float 0
                     (br_on_cast_fail $v1_not_float $float (local.get $v1))))
               (drop (block $v2_not_float (result (ref eq))
                  (local.set $f2
                     (struct.get $float 0
                        (br_on_cast_fail $v2_not_float $float (local.get $v2))))
                  (if (f64.lt (local.get $f1) (local.get $f2))
                     (then (return (i32.const -1))))
                  (if (f64.gt (local.get $f1) (local.get $f2))
                     (then (return (i32.const 1))))
                  (if (f64.ne (local.get $f1) (local.get $f2))
                     (then
                        (if (i32.eqz (local.get $total))
                           (then (return (global.get $unordered))))
                        (if (f64.eq (local.get $f1) (local.get $f1))
                           (then (return (i32.const 1))))
                        (if (f64.eq (local.get $f2) (local.get $f2))
                           (then (return (i32.const -1))))))
                  (br $next_item)))
               ;; ZZZ forward tag
               ;; ZZZ float array
      (call $log (i32.const 2))
               (unreachable)
               (return (i32.const 1))))
            (if (ref.test $float (local.get $v2))
               (then
                  ;; ZZZ forward tag
                  ;; ZZZ float array
      (call $log (i32.const 3))
                  (unreachable)
                  (return (i32.const -1))))
            (drop (block $v1_not_string (result (ref eq))
               (local.set $str1
                  (br_on_cast_fail $v1_not_string $string (local.get $v1)))
               (drop (block $v2_not_string (result (ref eq))
                  (local.set $str2
                      (br_on_cast_fail $v2_not_string $string (local.get $v2)))
                  (local.set $res
                     (call $compare_strings
                        (local.get $str1) (local.get $str2)))
                  (br_if $next_item (i32.eqz (local.get $res)))
                  (return (local.get $res))))
               ;; ZZZ forward tag
               ;; ZZZ float array
      (call $log (i32.const 4))
               (unreachable)
               (return (i32.const 1))))
            (drop (block $v1_not_custom (result (ref eq))
               (local.set $c1
                  (br_on_cast_fail $v1_not_custom $custom (local.get $v1)))
               (drop (block $v2_not_custom (result (ref eq))
                  (local.set $c2
                      (br_on_cast_fail $v2_not_custom $custom (local.get $v2)))
                  ;; ZZZ compare types
                  ;; ZZZ abstract value?
                  (local.set $res
                     (call_ref $value->value->int
                        (local.get $v1) (local.get $v2)
                        (struct.get $custom_operations 1
                           (struct.get $custom 0 (local.get $c1)))
                        ))
                  (br_if $next_item (i32.eqz (local.get $res)))
                  (return (local.get $res))))
               ;; ZZZ forward tag
               ;; ZZZ float array
      (call $log (i32.const 5))
               (unreachable)
               (return (i32.const 1))))
      (call $log (i32.const 6))
            (unreachable)
            ;; ZZZ forward tag
            ;; ZZZ float array
            (return (i32.const 1)))
         (if (call $compare_stack_is_not_empty (local.get $stack))
            (then
               (local.set $tuple (call $pop_compare_stack (local.get $stack)))
               (local.set $v1 (tuple.extract 0 (local.get $tuple)))
               (local.set $v2 (tuple.extract 1 (local.get $tuple)))
               (br $loop))))
     (i32.const 0))

   (func (export "caml_compare")
      (param $v1 (ref eq)) (param $v2 (ref eq)) (result (ref eq))
      (local $res i32)
      (local.set $res
         (call $compare_val (local.get $v1) (local.get $v2) (i32.const 1)))
      (if (i32.lt_s (local.get $res) (i32.const 0))
         (then (return (i31.new (i32.const -1)))))
      (if (i32.gt_s (local.get $res) (i32.const 0))
         (then (return (i31.new (i32.const 1)))))
      (i31.new (i32.const 0)))

   (func (export "caml_equal")
      (param $v1 (ref eq)) (param $v2 (ref eq)) (result (ref eq))
      (i31.new
         (i32.eqz
            (call $compare_val (local.get $v1) (local.get $v2) (i32.const 0)))))

   (func (export "caml_notequal")
      (param $v1 (ref eq)) (param $v2 (ref eq)) (result (ref eq))
      (i31.new
         (i32.ne (i32.const 0)
            (call $compare_val (local.get $v1) (local.get $v2) (i32.const 0)))))

   (func (export "caml_lessthan")
      (param $v1 (ref eq)) (param $v2 (ref eq)) (result (ref eq))
      (local $res i32)
      (local.set $res
         (call $compare_val (local.get $v1) (local.get $v2) (i32.const 0)))
      (i31.new
         (i32.and (i32.lt_s (local.get $res) (i32.const 0))
                  (i32.ne (local.get $res) (global.get $unordered)))))

   (func (export "caml_lessequal")
      (param $v1 (ref eq)) (param $v2 (ref eq)) (result (ref eq))
      (local $res i32)
      (local.set $res
         (call $compare_val (local.get $v1) (local.get $v2) (i32.const 0)))
      (i31.new
         (i32.and (i32.le_s (local.get $res) (i32.const 0))
                  (i32.ne (local.get $res) (global.get $unordered)))))

   (func (export "caml_greaterthan")
      (param $v1 (ref eq)) (param $v2 (ref eq)) (result (ref eq))
      (i31.new (i32.lt_s (i32.const 0)
         (call $compare_val (local.get $v1) (local.get $v2) (i32.const 0)))))

   (func (export "caml_greaterequal")
      (param $v1 (ref eq)) (param $v2 (ref eq)) (result (ref eq))
      (i31.new (i32.le_s (i32.const 0)
         (call $compare_val (local.get $v1) (local.get $v2) (i32.const 0)))))

   (func $caml_hash_mix_int (param $h i32) (param $d i32) (result i32)
      (i32.add
         (i32.mul
            (i32.rotl
               (i32.xor
                  (i32.mul
                     (i32.rotl
                        (i32.mul (local.get $d) (i32.const 0xcc9e2d51))
                        (i32.const 15))
                     (i32.const 0x1b873593))
                  (local.get $h))
               (i32.const 13))
            (i32.const 5))
         (i32.const 0xe6546b64)))

   (func $caml_hash_mix_final (param $h i32) (result i32)
      (local.set $h
         (i32.xor (local.get $h) (i32.shr_u (local.get $h) (i32.const 16))))
      (local.set $h (i32.mul (local.get $h) (i32.const 0x85ebca6b)))
      (local.set $h
         (i32.xor (local.get $h) (i32.shr_u (local.get $h) (i32.const 13))))
      (local.set $h (i32.mul (local.get $h) (i32.const 0xc2b2ae35)))
      (i32.xor (local.get $h) (i32.shr_u (local.get $h) (i32.const 16))))

   (func $caml_hash_mix_int64 (param $h i32) (param $d i64) (result i32)
      (return_call $caml_hash_mix_int
         (call $caml_hash_mix_int (local.get $h) (i32.wrap_i64 (local.get $d)))
         (i32.wrap_i64 (i64.shr_u (local.get $d) (i64.const 32)))))

   (func $caml_hash_mix_float (param $h i32) (param $d f64) (result i32)
      (local $i i64)
      (local.set $i (i64.reinterpret_f64 (local.get $d)))
      (if (i64.eq (i64.and (local.get $i) (i64.const 0x7FF0000000000000))
                  (i64.const 0x7ff0000000000000))
         (then
            (if (i64.ne (i64.and (local.get $i) (i64.const 0xFFFFFFFFFFFFF))
                        (i64.const 0))
               (then (local.set $i (i64.const 0x7ff0000000000001))))))
      (if (i64.eq (local.get $i) (i64.const 0x8000000000000000))
         (then (local.set $i (i64.const 0))))
      (return_call $caml_hash_mix_int64 (local.get $h) (local.get $i)))

   (func $caml_hash_mix_string
      (param $h i32) (param $s (ref $string)) (result i32)
      (local $i i32) (local $len i32) (local $w i32)
      (local.set $len (array.len (local.get $s)))
      (local.set $i (i32.const 0))
      (loop $loop
         (if (i32.le_u (i32.add (local.get $i) (i32.const 4)) (local.get $len))
            (then
               (local.set $h
                  (call $caml_hash_mix_int
                     (local.get $h)
                     (i32.or
                        (i32.or
                           (array.get_u $string (local.get $s) (local.get $i))
                           (i32.shl (array.get_u $string (local.get $s)
                                       (i32.add (local.get $i) (i32.const 1)))
                                    (i32.const 8)))
                        (i32.or
                           (i32.shl (array.get_u $string (local.get $s)
                                       (i32.add (local.get $i) (i32.const 2)))
                                    (i32.const 16))
                           (i32.shl (array.get_u $string (local.get $s)
                                       (i32.add (local.get $i) (i32.const 3)))
                                    (i32.const 24))))))
               (local.set $i (i32.add (local.get $i) (i32.const 4)))
               (br $loop))))
      (local.set $w (i32.const 0))
      (block $0_bytes
         (block $1_byte
            (block $2_bytes
               (block $3_bytes
                  (br_table $0_bytes $1_byte $2_bytes $3_bytes
                     (i32.and (local.get $len) (i32.const 3))))
               (local.set $w
                  (i32.shl (array.get_u $string (local.get $s)
                              (i32.add (local.get $i) (i32.const 2)))
                           (i32.const 16))))
            (local.set $w
               (i32.or (local.get $w)
                  (i32.shl (array.get_u $string (local.get $s)
                              (i32.add (local.get $i) (i32.const 1)))
                           (i32.const 8)))))
         (local.set $w
            (i32.or (local.get $w)
               (array.get_u $string (local.get $s) (local.get $i))))
         (local.set $h (call $caml_hash_mix_int (local.get $h) (local.get $w))))
      (i32.xor (local.get $h) (local.get $len)))

   (global $HASH_QUEUE_SIZE i32 (i32.const 256))

   (global $caml_hash_queue (ref $block)
      (array.new $block (i31.new (i32.const 0)) (global.get $HASH_QUEUE_SIZE)))

   (func (export "caml_hash")
      (param $count (ref eq)) (param $limit (ref eq)) (param $seed (ref eq))
      (param $obj (ref eq)) (result (ref eq))
      (local $sz i32) (local $num i32) (local $h i32)
      (local $rd i32) (local $wr i32)
      (local $v (ref eq))
      (local $b (ref $block))
      (local $i i32)
      (local $len i32)
      (local $tag i32)
      (local.set $sz (i31.get_u (ref.cast i31 (local.get $limit))))
      (if (i32.gt_u (local.get $sz) (global.get $HASH_QUEUE_SIZE))
         (then (local.set $sz (global.get $HASH_QUEUE_SIZE))))
      (local.set $num (i31.get_u (ref.cast i31 (local.get $count))))
      (local.set $h (i31.get_s (ref.cast i31 (local.get $seed))))
      (array.set $block
         (global.get $caml_hash_queue) (i32.const 0) (local.get $obj))
      (local.set $rd (i32.const 0))
      (local.set $wr (i32.const 1))
      (loop $loop
         (if (i32.and (i32.lt_u (local.get $rd) (local.get $wr))
                      (i32.gt_u (local.get $num) (i32.const 0)))
            (then
               (local.set $v
                  (array.get $block (global.get $caml_hash_queue)
                     (local.get $rd)))
               (local.set $rd (i32.add (local.get $rd) (i32.const 1)))
               (block $again
                  (drop (block $not_int (result (ref eq))
                     (local.set $h
                        (call $caml_hash_mix_int (local.get $h)
                           (i31.get_s
                              (br_on_cast_fail $not_int i31 (local.get $v)))))
                     (local.set $num (i32.sub (local.get $num) (i32.const 1)))
                     (br $loop)))
                  (drop (block $not_string (result (ref eq))
                     (local.set $h
                        (call $caml_hash_mix_string (local.get $h)
                           (br_on_cast_fail $not_string $string (local.get $v))))
                     (local.set $num (i32.sub (local.get $num) (i32.const 1)))
                     (br $loop)))
                  (drop (block $not_block (result (ref eq))
                     (local.set $b
                        (br_on_cast_fail $not_block $block (local.get $v)))
                     (local.set $tag
                        (i31.get_u
                           (ref.cast i31
                              (array.get $block (local.get $b) (i32.const 0)))))
                     ;; ZZZ Special tags (forward / object)
                     (local.set $len (array.len (local.get $b)))
                     (local.set $h
                        (call $caml_hash_mix_int (local.get $h)
                           (i32.or
                              (i32.sub (local.get $len) (i32.const 1))
                              (local.get $tag))))
                     (local.set $i (i32.const 1))
                     (loop $block_iter
                        (br_if $loop (i32.ge_u (local.get $i) (local.get $len)))
                        (br_if $loop (i32.ge_u (local.get $wr) (local.get $sz)))
                        (array.set $block (global.get $caml_hash_queue)
                           (local.get $wr)
                           (array.get $block (local.get $b) (local.get $i)))
                        (local.set $wr (i32.add (local.get $wr) (i32.const 1)))
                        (local.set $i (i32.add (local.get $i) (i32.const 1)))
                        (br $block_iter))))
                  (drop (block $not_float (result (ref eq))
                     (local.set $h
                        (call $caml_hash_mix_float (local.get $h)
                           (struct.get $float 0
                              (br_on_cast_fail $not_float $float
                                 (local.get $v)))))
                     (local.set $num (i32.sub (local.get $num) (i32.const 1)))
                     (br $loop)))
                  (drop (block $not_custom (result (ref eq))
                     (local.set $h
                        (call $caml_hash_mix_int (local.get $h)
                           (call_ref $value->int
                              (local.get $v)
                              (struct.get $custom_operations 2
                                 (br_on_null $loop
                                    (struct.get $custom 0
                                       (br_on_cast_fail $not_custom $custom
                                          (local.get $v))))))))
                     (local.set $num (i32.sub (local.get $num) (i32.const 1)))
                     (br $loop)))
                  ;; ZZZ other cases? (closures, javascript values)
                  (unreachable)
                  (br $loop)))))
      ;; clear the queue to avoid a memory leak
      (array.fill $block (global.get $caml_hash_queue)
         (i32.const 0) (i31.new (i32.const 0)) (local.get $wr))
      (i31.new (call $caml_hash_mix_final (local.get $h))))

   (func (export "caml_marshal_data_size")
      (param (ref eq) (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_marshal_data_size"))
      (unreachable))

   (func (export "caml_input_value") (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_input_value"))
      (unreachable))

   (func (export "caml_input_value_from_bytes")
      (param (ref eq) (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_input_value_from_bytes"))
      (unreachable))

   (func (export "caml_output_value")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_output_value"))
      (unreachable))

   (func (export "caml_output_value_to_buffer")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_output_value_to_buffer"))
      (unreachable))

   (func (export "caml_output_value_to_string")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_output_value_to_string"))
      (unreachable))

   ;; ZZZ
   (func $dummy_format_fun (param (ref eq)) (param (ref eq)) (result (ref eq))
      (call $log_js (string.const "dummy_format_fun"))
      (array.new_fixed $string (i32.const 64)))
   (func (export "%caml_format_int_special") (param (ref eq)) (result (ref eq))
      (call $log_js (string.const "%caml_format_int_special"))
      (call $caml_string_of_jsstring (call $wrap (call $format_int (local.get 0)))))
   (func (export "caml_format_int") (param (ref eq)) (param (ref eq)) (result (ref eq))
      (call $caml_string_of_jsstring (call $wrap (call $format_int (local.get 1)))))
   (export "caml_int32_format" (func $dummy_format_fun))
   (export "caml_int64_format" (func $dummy_format_fun))
   (export "caml_nativeint_format" (func $dummy_format_fun))
   (func (export "caml_hexstring_of_float")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (array.new_fixed $string (i32.const 64)))
   (func (export "caml_format_float")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $caml_string_of_jsstring (call $wrap (call $format_float (struct.get $float 0 (ref.cast $float (local.get 1)))))))

   (func (export "caml_get_exception_raw_backtrace")
      (param (ref eq)) (result (ref eq))
      (array.new_fixed $block (i31.new (i32.const 0))))

   (func (export "caml_backtrace_status")
      (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))

   (func (export "caml_convert_raw_backtrace")
      (param (ref eq)) (result (ref eq))
      (array.new_fixed $block (i31.new (i32.const 0))))

   (data $raw_backtrace_slot_err
      "Printexc.get_raw_backtrace_slot: index out of bounds")

   (func (export "caml_raw_backtrace_slot")
      (param (ref eq) (ref eq)) (result (ref eq))
      (call $caml_invalid_argument
          (array.new_data $string $raw_backtrace_slot_err
             (i32.const 0) (i32.const 52)))
      (i31.new (i32.const 0)))

   (func (export "caml_convert_raw_backtrace_slot")
      (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))

   (func (export "caml_restore_raw_backtrace")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))

   (func (export "caml_get_current_callstack")
      (param (ref eq)) (result (ref eq))
      (array.new_fixed $block (i31.new (i32.const 0))))

   (func (export "caml_get_public_method")
      (param (ref eq) (ref eq) (ref eq)) (result (ref eq))
      ;;ZZZ
      (call $log_js (string.const "caml_get_public_method"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_debug_info_status")
      (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))

   (func (export "caml_sys_const_max_wosize")
      (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0xfffffff)))

   (func (export "caml_ephe_create")
      (param (ref eq)) (result (ref eq))
      ;;ZZZ
      (call $log_js (string.const "caml_ephe_create"))
      (i31.new (i32.const 0)))

   (func (export "caml_ephe_get_data")
      (param (ref eq)) (result (ref eq))
      ;;ZZZ
      (call $log_js (string.const "caml_ephe_get_data"))
      (i31.new (i32.const 0)))

   (func (export "caml_ephe_set_data")
      (param (ref eq) (ref eq)) (result (ref eq))
      ;;ZZZ
      (call $log_js (string.const "caml_ephe_set_data"))
      (i31.new (i32.const 0)))

   (func (export "caml_ephe_set_key")
      (param (ref eq) (ref eq) (ref eq)) (result (ref eq))
      ;;ZZZ
      (call $log_js (string.const "caml_ephe_set_key"))
      (i31.new (i32.const 0)))

   (func (export "caml_ephe_unset_key")
      (param (ref eq) (ref eq)) (result (ref eq))
      ;;ZZZ
      (call $log_js (string.const "caml_ephe_unset_key"))
      (i31.new (i32.const 0)))

   (global $caml_ephe_none (ref eq)
      (array.new_fixed $block (i31.new (global.get $abstract_tag))))

   (data $Weak_create "Weak.create")

   (func (export "caml_weak_create")
      (param $vlen (ref eq)) (result (ref eq))
      (local $len i32)
      (local $res (ref $block))
      (local.set $len (i31.get_s (ref.cast i31 (local.get $vlen))))
      (if (i32.lt_s (local.get $len) (i32.const 0))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $Weak_create
                  (i32.const 0) (i32.const 11)))))
      (local.set $res
         (array.new $block (global.get $caml_ephe_none)
            (i32.add (local.get $len) (i32.const 3))))
      (array.set $block (local.get $res) (i32.const 0)
         (i31.new (global.get $abstract_tag)))
      ;;ZZZ
      (call $log_js (string.const "caml_weak_create"))
      (local.get $res))

   (func (export "caml_weak_blit")
      (param (ref eq) (ref eq) (ref eq) (ref eq) (ref eq)) (result (ref eq))
      ;;ZZZ
      (call $log_js (string.const "caml_weak_blit"))
      (i31.new (i32.const 0)))

   (func (export "caml_weak_check")
      (param (ref eq) (ref eq)) (result (ref eq))
      ;;ZZZ
      (call $log_js (string.const "caml_weak_check"))
      (i31.new (i32.const 0)))

   (func (export "caml_weak_get")
      (param (ref eq) (ref eq)) (result (ref eq))
      ;;ZZZ
      (call $log_js (string.const "caml_weak_get"))
      (i31.new (i32.const 0)))

   (func (export "caml_weak_get_copy")
      (param (ref eq) (ref eq)) (result (ref eq))
      ;;ZZZ
      (call $log_js (string.const "caml_weak_get_copy"))
      (i31.new (i32.const 0)))

   (global $bigarray_ops (ref $custom_operations)
      ;; ZZZ
      (struct.new $custom_operations
         (array.new_fixed $string ;; "_bigarr02"
            (i32.const 95) (i32.const 98) (i32.const 105) (i32.const 103)
            (i32.const 97) (i32.const 114) (i32.const 114) (i32.const 48)
            (i32.const 50))
         (ref.func $int64_cmp) (ref.func $int64_hash)))

   (type $bigarray
      (sub $custom
         (struct
            (field (ref $custom_operations))
            (field externref) ;; data
            (field (ref $int_array)) ;; size in each dimension
            (field i8) ;; number of dimensions
            (field i8) ;; kind
            (field i8)))) ;; layout

   (func $caml_ba_get_size (param $dim (ref $int_array)) (result i32)
      (local $i i32) (local $n i32) (local $sz i32)
      (local.set $n (array.len (local.get $dim)))
      (local.set $i (i32.const 0))
      (local.set $sz (i32.const 1))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $n))
            (then
               ;; ZZZ Check for overflow
               (local.set $sz
                   (i32.mul (local.get $sz)
                      (array.get $int_array
                         (local.get $dim) (local.get $i))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (local.get $sz))

  (func $caml_ba_size_per_element (param $kind i32) (result i32)
     (select (i32.const 2) (i32.const 1)
        (i32.or (i32.eq (local.get $kind) (i32.const 7))
                (i32.or (i32.eq (local.get $kind) (i32.const 10))
                        (i32.eq (local.get $kind) (i32.const 11))))))

  (func $caml_ba_create_buffer
     (param $kind i32) (param $sz i32) (result externref)
     (return_call $ta_create (local.get $kind)
        ;; ZZZ Check for overflow
        (i32.mul (local.get $sz)
           (call $caml_ba_size_per_element (local.get $kind)))))

   (global $CAML_BA_MAX_NUM_DIMS i32 (i32.const 16))

   (data $ba_create_bad_dims "Bigarray.create: bad number of dimensions")
   (data $ba_create_negative_dim "Bigarray.create: negative dimension")

   (func (export "caml_ba_create")
      (param $vkind (ref eq)) (param $layout (ref eq)) (param $d (ref eq))
      (result (ref eq))
      (local $vdim (ref $block))
      (local $dim (ref $int_array))
      (local $kind i32) (local $num_dims i32) (local $i i32) (local $n i32)
      (local.set $kind (i31.get_s (ref.cast i31 (local.get $vkind))))
      (local.set $vdim (ref.cast $block (local.get $d)))
      (local.set $num_dims (i32.sub (array.len (local.get $vdim)) (i32.const 1)))
      (if (i32.gt_u (local.get $num_dims) (global.get $CAML_BA_MAX_NUM_DIMS))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $ba_create_bad_dims
                  (i32.const 0) (i32.const 41)))))
      (local.set $dim
         (array.new $int_array (i32.const 0) (local.get $num_dims)))
      (local.set $i (i32.const 0))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $num_dims))
            (then
               (local.set $n
                  (i31.get_s
                     (ref.cast i31
                        (array.get $block (local.get $vdim)
                           (i32.add (local.get $i) (i32.const 1))))))
               (if (i32.lt_s (local.get $n) (i32.const 0))
                  (then
                     (call $caml_invalid_argument
                        (array.new_data $string $ba_create_negative_dim
                              (i32.const 0) (i32.const 35)))))
               (array.set $int_array
                  (local.get $dim) (local.get $i) (local.get $n))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (struct.new $bigarray
         (global.get $bigarray_ops)
         (call $caml_ba_create_buffer (local.get $kind)
            (call $caml_ba_get_size (local.get $dim)))
         (local.get $dim)
         (local.get $num_dims)
         (local.get $kind)
         (i31.get_s (ref.cast i31 (local.get $layout)))))

   (data $ta_unsupported_kind "Typed_array.to_genarray: unsupported kind")
   (data $ta_too_large "Typed_array.to_genarray: too large")

   (func (export "caml_ba_from_typed_array") (param (ref eq)) (result (ref eq))
      (local $data externref)
      (local $kind i32)
      (local $len i32)
      (local.set $data
         (call $ta_normalize (extern.externalize (call $unwrap (local.get 0)))))
      (local.set $kind (call $ta_kind (local.get $data)))
      (if (i32.lt_s (local.get $kind) (i32.const 0))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $ta_unsupported_kind
                  (i32.const 0) (i32.const 41)))))
      (local.set $len (call $ta_length (local.get $data)))
      (if (i32.lt_s (local.get $len) (i32.const 0))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $ta_too_large
                  (i32.const 0) (i32.const 34)))))
      (struct.new $bigarray
         (global.get $bigarray_ops)
         (local.get $data)
         (array.new_fixed $int_array (local.get $len))
         (i32.const 1)
         (local.get $kind)
         (i32.const 0)))

   (func (export "caml_ba_to_typed_array") (param (ref eq)) (result (ref eq))
      (call $wrap
         (extern.internalize
            (struct.get $bigarray 1 (ref.cast $bigarray (local.get $0))))))

   (func $caml_ba_get_at_offset
      (param $ba (ref $bigarray)) (param $i i32) (result (ref eq))
      (local $data externref)
      (local.set $data (struct.get $bigarray 1 (local.get $ba)))
      (block $float32
       (block $float64
        (block $int8
         (block $uint8
          (block $int16
           (block $uint16
            (block $int32
             (block $int64
              (block $nativeint
               (block $int
                (block $complex32
                 (block $complex64
                  (br_table $float32 $float64 $int8 $uint8 $int16 $uint16
                            $int32 $int64 $nativeint $int
                            $complex32 $complex64 $uint8
                     (struct.get $bigarray 4 (local.get $ba))))
                 (local.set $i (i32.shl (local.get $i) (i32.const 1)))
                 (return
                    (array.new_fixed $block
                       (i31.new (global.get $double_array_tag))
                       (struct.new $float
                          (call $ta_get_f64 (local.get $data) (local.get $i)))
                       (struct.new $float
                          (call $ta_get_f64 (local.get $data)
                             (i32.add (local.get $i) (i32.const 1)))))))
                (local.set $i (i32.shl (local.get $i) (i32.const 1)))
                (return
                   (array.new_fixed $block
                      (i31.new (global.get $double_array_tag))
                      (struct.new $float
                         (call $ta_get_f32 (local.get $data) (local.get $i)))
                      (struct.new $float
                         (call $ta_get_f32 (local.get $data)
                            (i32.add (local.get $i) (i32.const 1)))))))
               (return
                  (i31.new
                     (call $ta_get_i32 (local.get $data) (local.get $i)))))
              (return_call $caml_copy_nativeint
                 (call $ta_get_i32 (local.get $data) (local.get $i))))
             (local.set $i (i32.shl (local.get $i) (i32.const 1)))
             (return_call $caml_copy_int64
                (i64.or
                   (i64.extend_i32_u
                      (call $ta_get_i32 (local.get $data) (local.get $i)))
                   (i64.shl
                      (i64.extend_i32_u
                         (call $ta_get_i32 (local.get $data)
                            (i32.add (local.get $i) (i32.const 1))))
                      (i64.const 32)))))
            (return_call $caml_copy_int32
               (call $ta_get_i32 (local.get $data) (local.get $i))))
           (return (i31.new
                      (call $ta_get_ui16 (local.get $data) (local.get $i)))))
          (return (i31.new
                     (call $ta_get_i16 (local.get $data) (local.get $i)))))
         (return (i31.new
                    (call $ta_get_ui8 (local.get $data) (local.get $i)))))
        (return (i31.new
                   (call $ta_get_i8 (local.get $data) (local.get $i)))))
       (return (struct.new $float
                  (call $ta_get_f64 (local.get $data) (local.get $i)))))
      (return (struct.new $float
                 (call $ta_get_f32 (local.get $data) (local.get $i)))))

   (func $caml_ba_set_at_offset
      (param $ba (ref $bigarray)) (param $i i32) (param $v (ref eq))
      (local $data externref)
      (local $b (ref $block)) (local $l i64)
      (local.set $data (struct.get $bigarray 1 (local.get $ba)))
      (block $float32
       (block $float64
        (block $int8
         (block $uint8
          (block $int16
           (block $uint16
            (block $int32
             (block $int64
              (block $nativeint
               (block $int
                (block $complex32
                 (block $complex64
                  (br_table $float32 $float64 $int8 $uint8 $int16 $uint16
                            $int32 $int64 $nativeint $int
                            $complex32 $complex64 $uint8
                     (struct.get $bigarray 4 (local.get $ba))))
                 (local.set $i (i32.shl (local.get $i) (i32.const 1)))
                 (local.set $b (ref.cast $block (local.get $v)))
                 (call $ta_set_f64 (local.get $data) (local.get $i)
                    (struct.get $float 0
                       (ref.cast $float
                          (array.get $block (local.get $b) (i32.const 1)))))
                 (call $ta_set_f64 (local.get $data)
                    (i32.add (local.get $i) (i32.const 1))
                    (struct.get $float 0
                       (ref.cast $float
                         (array.get $block (local.get $b) (i32.const 2)))))
                 (return))
                (local.set $i (i32.shl (local.get $i) (i32.const 1)))
                (local.set $b (ref.cast $block (local.get $v)))
                (call $ta_set_f32 (local.get $data) (local.get $i)
                   (struct.get $float 0
                      (ref.cast $float
                         (array.get $block (local.get $b) (i32.const 1)))))
                (call $ta_set_f32 (local.get $data)
                   (i32.add (local.get $i) (i32.const 1))
                   (struct.get $float 0
                      (ref.cast $float
                         (array.get $block (local.get $b) (i32.const 2)))))
                (return))
               (call $ta_set_i32 (local.get $data) (local.get $i)
                  (i31.get_s (ref.cast i31 (local.get $v))))
               (return))
              (call $ta_set_i32 (local.get $data) (local.get $i)
                 (struct.get $int32 1 (ref.cast $int32 (local.get $v))))
              (return))
             (local.set $i (i32.shl (local.get $i) (i32.const 1)))
             (local.set $l
                (struct.get $int64 1 (ref.cast $int64 (local.get $v))))
             (call $ta_set_i32 (local.get $data) (local.get $i)
                (i32.wrap_i64 (local.get $l)))
             (call $ta_set_i32 (local.get $data)
                (i32.add (local.get $i) (i32.const 1))
                (i32.wrap_i64 (i64.shr_u (local.get $l) (i64.const 32))))
             (return))
            (call $ta_set_i32 (local.get $data) (local.get $i)
               (struct.get $int32 1 (ref.cast $int32 (local.get $v))))
            (return))
           (call $ta_set_ui16 (local.get $data) (local.get $i)
              (ref.cast i31 (local.get $v)))
           (return))
          (call $ta_set_i16 (local.get $data) (local.get $i)
             (ref.cast i31 (local.get $v)))
          (return))
         (call $ta_set_ui8 (local.get $data) (local.get $i)
            (ref.cast i31 (local.get $v)))
         (return))
        (call $ta_set_i8 (local.get $data) (local.get $i)
           (ref.cast i31 (local.get $v)))
        (return))
       (call $ta_set_f64 (local.get $data) (local.get $i)
          (struct.get $float 0 (ref.cast $float (local.get 0))))
       (return))
      (call $ta_set_f32 (local.get $data) (local.get $i)
         (struct.get $float 0 (ref.cast $float (local.get 0))))
      (return))

   (data $Bigarray_dim "Bigarray.dim")

   (func $caml_ba_dim (export "caml_ba_dim")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (local $dim (ref $int_array))
      (local $i i32)
      (local.set $dim
         (struct.get $bigarray 2 (ref.cast $bigarray (local.get 0))))
      (local.set $i (i31.get_s (ref.cast i31 (local.get $1))))
      (if (i32.ge_u (local.get $i) (array.len (local.get $dim)))
         (then (call $caml_invalid_argument
                  (array.new_data $string $Bigarray_dim
                     (i32.const 0) (i32.const 12)))))
      (i31.new (array.get $int_array (local.get $dim) (local.get $i))))

   (func (export "caml_ba_dim_1")
      (param (ref eq)) (result (ref eq))
      (return_call $caml_ba_dim (local.get 0) (i31.new (i32.const 0))))

   (func (export "caml_ba_get_1")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (local $ba (ref $bigarray))
      (local $i i32)
      (local.set $ba (ref.cast $bigarray (local.get 0)))
      (local.set $i (i31.get_u (ref.cast i31 (local.get 1))))
      (if (struct.get $bigarray 5 (local.get $ba))
         (then (local.set $i (i32.sub (local.get $i) (i32.const 1)))))
      (if (i32.ge_u (local.get $i)
             (array.get $int_array (struct.get $bigarray 2 (local.get $ba))
                (i32.const 0)))
         (call $caml_bound_error))
      (return_call $caml_ba_get_at_offset (local.get $ba) (local.get $i)))

   (func (export "caml_ba_set_1")
      (param (ref eq)) (param (ref eq)) (param $v (ref eq)) (result (ref eq))
      (local $ba (ref $bigarray))
      (local $i i32)
      (local.set $ba (ref.cast $bigarray (local.get 0)))
      (local.set $i (i31.get_u (ref.cast i31 (local.get 1))))
      (if (struct.get $bigarray 5 (local.get $ba))
         (then (local.set $i (i32.sub (local.get $i) (i32.const 1)))))
      (if (i32.ge_u (local.get $i)
             (array.get $int_array (struct.get $bigarray 2 (local.get $ba))
                (i32.const 0)))
         (call $caml_bound_error))
      (call $caml_ba_set_at_offset
         (local.get $ba) (local.get $i) (local.get $v))
      (i31.new (i32.const 0)))

   (func (export "caml_string_of_array") (param (ref eq)) (result (ref eq))
      ;; ZZZ used to convert a typed array to a string...
      (call $log_js (string.const "caml_string_of_array"))
      (unreachable))

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

   (func (export "caml_atomic_cas")
      (param $ref (ref eq)) (param $o (ref eq)) (param $n (ref eq))
      (result (ref eq))
      (local $b (ref $block))
      (local.set $b (ref.cast $block (local.get $ref)))
      (if (result (ref eq))
         (ref.eq (array.get $block (local.get $b) (i32.const 1))
                 (local.get $o))
         (then
            (array.set $block (local.get $b) (i32.const 1) (local.get $n))
            (i31.new (i32.const 1)))
         (else
            (i31.new (i32.const 0)))))

   (func (export "caml_atomic_load") (param (ref eq)) (result (ref eq))
      (array.get $block (ref.cast $block (local.get 0)) (i32.const 1)))

   (func (export "caml_atomic_fetch_add")
     (param $ref (ref eq)) (param $i (ref eq)) (result (ref eq))
     (local $b (ref $block))
     (local $old (ref eq))
     (local.set $b (ref.cast $block (local.get $ref)))
     (local.set $old (array.get $block (local.get $b) (i32.const 1)))
     (array.set $block (local.get $b) (i32.const 1)
        (i31.new (i32.add (i31.get_s (ref.cast i31 (local.get $old)))
                          (i31.get_s (ref.cast i31 (local.get $i))))))
     (local.get $old))

   (global $caml_domain_dls (mut (ref eq))
      (array.new_fixed $block (i31.new (i32.const 0))))

   (func (export "caml_domain_dls_set") (param $a (ref eq)) (result (ref eq))
      (global.set $caml_domain_dls (local.get $a))
      (i31.new (i32.const 0)))

   (func (export "caml_domain_dls_get") (param (ref eq)) (result (ref eq))
      (global.get $caml_domain_dls))

   (func (export "caml_lxm_next") (param $v (ref eq)) (result (ref eq))
      (local $data externref)
      (local $a i64) (local $s i64) (local $q0 i64) (local $q1 i64)
      (local $z i64)
      (local.set $data
         (struct.get $bigarray 1 (ref.cast $bigarray (local.get $v))))
      (local.set $a
         (i64.or
            (i64.extend_i32_u
               (call $ta_get_i32 (local.get $data) (i32.const 0)))
            (i64.shl
               (i64.extend_i32_u
                  (call $ta_get_i32 (local.get $data) (i32.const 1)))
               (i64.const 32))))
      (local.set $s
         (i64.or
            (i64.extend_i32_u
               (call $ta_get_i32 (local.get $data) (i32.const 2)))
            (i64.shl
               (i64.extend_i32_u
                  (call $ta_get_i32 (local.get $data) (i32.const 3)))
               (i64.const 32))))
      (local.set $q0
         (i64.or
            (i64.extend_i32_u
               (call $ta_get_i32 (local.get $data) (i32.const 4)))
            (i64.shl
               (i64.extend_i32_u
                  (call $ta_get_i32 (local.get $data) (i32.const 5)))
               (i64.const 32))))
      (local.set $q1
         (i64.or
            (i64.extend_i32_u
               (call $ta_get_i32 (local.get $data) (i32.const 6)))
            (i64.shl
               (i64.extend_i32_u
                  (call $ta_get_i32 (local.get $data) (i32.const 7)))
               (i64.const 32))))
      (local.set $z (i64.add (local.get $s) (local.get $q0)))
      (local.set $z
         (i64.mul (i64.xor (local.get $z)
                           (i64.shr_u (local.get $z) (i64.const 32)))
                  (i64.const 0xdaba0b6eb09322e3)))
      (local.set $z
         (i64.mul (i64.xor (local.get $z)
                           (i64.shr_u (local.get $z) (i64.const 32)))
                  (i64.const 0xdaba0b6eb09322e3)))
      (local.set $z
         (i64.xor (local.get $z) (i64.shr_u (local.get $z) (i64.const 32))))
      (local.set $s
         (i64.add (i64.mul (local.get $s) (i64.const 0xd1342543de82ef95))
                  (local.get $a)))
      (call $ta_set_i32 (local.get $data) (i32.const 2)
         (i32.wrap_i64 (local.get $s)))
      (call $ta_set_i32 (local.get $data) (i32.const 3)
         (i32.wrap_i64 (i64.shr_u (local.get $s) (i64.const 32))))
      (local.set $q1 (i64.xor (local.get $q1) (local.get $q0)))
      (local.set $q0 (i64.rotl (local.get $q0) (i64.const 24)))
      (local.set $q0 (i64.xor (i64.xor (local.get $q0) (local.get $q1))
                              (i64.shl (local.get $q1) (i64.const 16))))
      (local.set $q1 (i64.rotl (local.get $q1) (i64.const 37)))
      (call $ta_set_i32 (local.get $data) (i32.const 4)
         (i32.wrap_i64 (local.get $q0)))
      (call $ta_set_i32 (local.get $data) (i32.const 5)
         (i32.wrap_i64 (i64.shr_u (local.get $q0) (i64.const 32))))
      (call $ta_set_i32 (local.get $data) (i32.const 6)
         (i32.wrap_i64 (local.get $q1)))
      (call $ta_set_i32 (local.get $data) (i32.const 7)
         (i32.wrap_i64 (i64.shr_u (local.get $q1) (i64.const 32))))
      (return_call $caml_copy_int64 (local.get $z)))

   (func (export "create_nat")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "create_nat"))
      (i31.new (i32.const 0)))

   (func (export "incr_nat")
      (param (ref eq) (ref eq) (ref eq) (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "incr_nat"))
      (i31.new (i32.const 0)))

   (func (export "initialize_nat")
      (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))

   (func (export "set_digit_nat")
      (param (ref eq) (ref eq) (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "set_digit_nat"))
      (i31.new (i32.const 0)))

   (func (export "set_to_zero_nat")
      (param (ref eq) (ref eq) (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "set_to_zero_nat"))
      (i31.new (i32.const 0)))

   (func (export "unix_gettimeofday")
      (param (ref eq)) (result (ref eq))
      (struct.new $float (call $gettimeofday)))

   (func (export "caml_alloc_tm")
      (param $sec i32) (param $min i32) (param $hour i32) (param $mday i32)
      (param $mon i32) (param $year i32) (param $wday i32) (param $yday $i32)
      (param $isdst i32) (result (ref eq))
      (array.new_fixed $block (i31.new (i32.const 0))
         (i31.new (local.get $sec))
         (i31.new (local.get $min))
         (i31.new (local.get $hour))
         (i31.new (local.get $mday))
         (i31.new (local.get $mon))
         (i31.new (local.get $year))
         (i31.new (local.get $wday))
         (i31.new (local.get $yday))
         (i31.new (local.get $isdst))))

   (func (export "unix_gmtime") (param (ref eq)) (result (ref eq))
      (call $gmtime))

   (func (export "unix_localtime") (param (ref eq)) (result (ref eq))
      (call $localtime))

   (func (export "unix_time") (param (ref eq)) (result (ref eq))
      (struct.new $float (f64.floor (call $gettimeofday))))

   (func (export "unix_inet_addr_of_string")
      (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))

   (type $js (struct (field anyref)))

   (func $wrap (param anyref) (result (ref eq))
      (block $is_eq (result (ref eq))
         (return (struct.new $js (br_on_cast $is_eq eq (local.get 0))))))

   (func $unwrap (param (ref eq)) (result anyref)
      (block $not_js (result anyref)
         (return (struct.get $js 0
                    (br_on_cast_fail $not_js $js (local.get 0))))))

   (import "bindings" "identity" (func $to_float (param anyref) (result f64)))
   (import "bindings" "identity" (func $from_float (param f64) (result anyref)))
   (import "bindings" "identity" (func $to_bool (param anyref) (result i32)))
   (import "bindings" "identity" (func $ref_cast_string (param anyref) (result stringref)))
   (import "bindings" "from_bool" (func $from_bool (param i32) (result anyref)))
   (import "bindings" "eval" (func $eval (param anyref) (result anyref)))
   (import "bindings" "get" (func $get (param externref) (param anyref) (result anyref)))
   (import "bindings" "set" (func $set (param anyref) (param anyref) (param anyref)))
   (import "bindings" "delete" (func $delete (param anyref) (param anyref)))
   (import "bindings" "instanceof"
      (func $instanceof (param anyref) (param anyref) (result i32)))
   (import "bindings" "typeof" (func $typeof (param anyref) (result anyref)))
   (import "bindings" "equals" (func $equals (param anyref) (param anyref) (result i32)))
   (import "bindings" "strict_equals" (func $strict_equals (param anyref) (param anyref) (result i32)))
   (import "bindings" "fun_call"
      (func $fun_call
         (param anyref) (param anyref) (param anyref) (result anyref)))
   (import "bindings" "meth_call" (func $meth_call (param anyref) (param anyref) (param anyref) (result anyref)))
   (import "bindings" "new" (func $new (param anyref) (param anyref) (result anyref)))
   (import "bindings" "new_obj" (func $new_obj (result anyref)))
   (import "bindings" "new_array" (func $new_array (param i32) (result externref)))
   (import "bindings" "iter_props"
      (func $iter_props (param anyref) (param anyref)))
   (import "bindings" "array_length"
      (func $array_length (param externref) (result i32)))
   (import "bindings" "array_get"
      (func $array_get (param externref) (param i32) (result anyref)))
   (import "bindings" "array_set"
      (func $array_set (param externref) (param i32) (param anyref)))
   (import "bindings" "wrap_callback"
      (func $wrap_callback (param (ref eq)) (result anyref)))
   (import "bindings" "wrap_callback_args"
      (func $wrap_callback_args (param (ref eq)) (result anyref)))
   (import "bindings" "wrap_callback_strict"
      (func $wrap_callback_strict (param i32) (param (ref eq)) (result anyref)))
   (import "bindings" "wrap_callback_unsafe"
      (func $wrap_callback_unsafe (param (ref eq)) (result anyref)))
   (import "bindings" "wrap_meth_callback"
      (func $wrap_meth_callback (param (ref eq)) (result anyref)))
   (import "bindings" "wrap_meth_callback_args"
      (func $wrap_meth_callback_args (param (ref eq)) (result anyref)))
   (import "bindings" "wrap_meth_callback_strict"
      (func $wrap_meth_callback_strict (param i32) (param (ref eq)) (result anyref)))
   (import "bindings" "wrap_meth_callback_unsafe"
      (func $wrap_meth_callback_unsafe (param (ref eq)) (result anyref)))
   (import "bindings" "wrap_fun_arguments"
      (func $wrap_fun_arguments (param anyref) (result anyref)))
   (import "bindings" "get_int" (func $get_int (param externref) (param i32) (result i32)))
   (import "bindings" "format" (func $format_float (param f64) (result anyref)))
   (import "bindings" "format" (func $format_int (param (ref eq)) (result anyref)))
   (import "bindings" "ta_create"
      (func $ta_create (param i32) (param i32) (result externref)))
   (import "bindings" "ta_normalize"
      (func $ta_normalize (param externref) (result externref)))
   (import "bindings" "ta_kind" (func $ta_kind (param externref) (result i32)))
   (import "bindings" "ta_length"
      (func $ta_length (param externref) (result i32)))
   (import "bindings" "ta_get_f64"
      (func $ta_get_f64 (param externref) (param i32) (result f64)))
   (import "bindings" "ta_get_f32"
      (func $ta_get_f32 (param externref) (param i32) (result f64)))
   (import "bindings" "ta_get_i32"
      (func $ta_get_i32 (param externref) (param i32) (result i32)))
   (import "bindings" "ta_get_i16"
      (func $ta_get_i16 (param externref) (param i32) (result i32)))
   (import "bindings" "ta_get_ui16"
      (func $ta_get_ui16 (param externref) (param i32) (result i32)))
   (import "bindings" "ta_get_i8"
      (func $ta_get_i8 (param externref) (param i32) (result i32)))
   (import "bindings" "ta_get_ui8"
      (func $ta_get_ui8 (param externref) (param i32) (result i32)))
   (import "bindings" "ta_set_f64"
      (func $ta_set_f64 (param externref) (param i32) (param f64)))
   (import "bindings" "ta_set_f32"
      (func $ta_set_f32 (param externref) (param i32) (param f64)))
   (import "bindings" "ta_set_i32"
      (func $ta_set_i32 (param externref) (param i32) (param i32)))
   (import "bindings" "ta_set_i16"
      (func $ta_set_i16 (param externref) (param i32) (param (ref i31))))
   (import "bindings" "ta_set_ui16"
      (func $ta_set_ui16 (param externref) (param i32) (param (ref i31))))
   (import "bindings" "ta_set_i8"
      (func $ta_set_i8 (param externref) (param i32) (param (ref i31))))
   (import "bindings" "ta_set_ui8"
      (func $ta_set_ui8 (param externref) (param i32) (param (ref i31))))
   (import "bindings" "gettimeofday" (func $gettimeofday (result f64)))
   (import "bindings" "gmtime" (func $gmtime (result (ref eq))))
   (import "bindings" "localtime" (func $localtime (result (ref eq))))
   (import "bindings" "random_seed" (func $random_seed (result externref)))

   (func (export "caml_js_equals")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (i31.new (call $equals
                  (call $unwrap (local.get 0)) (call $unwrap (local.get 1)))))

   (func (export "caml_js_strict_equals")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (i31.new (call $strict_equals
                  (call $unwrap (local.get 0)) (call $unwrap (local.get 1)))))

   ;; ZZZ We should generate JavaScript code instead of using 'eval'
   (export "caml_pure_js_expr" (func $caml_js_expr))
   (export "caml_js_var" (func $caml_js_expr))
   (export "caml_js_eval_string" (func $caml_js_expr))
   (func $caml_js_expr (export "caml_js_expr")
      (param (ref eq)) (result (ref eq))
      (local $s (ref $string))
      (local.set $s (ref.cast $string (local.get 0)))
      (return_call $wrap
         (call $eval
            (string.new_wtf8_array replace
               (local.get $s) (i32.const 0) (array.len (local.get $s))))))

   (func (export "caml_js_to_float") (param (ref eq)) (result (ref eq))
      (struct.new $float (call $to_float (call $unwrap (local.get 0)))))

   (func (export "caml_js_from_float") (param (ref eq)) (result (ref eq))
      (return_call $wrap
         (call $from_float
            (struct.get $float 0 (ref.cast $float (local.get 0))))))

   (func (export "caml_js_to_bool") (param (ref eq)) (result (ref eq))
      (i31.new (call $to_bool (struct.get $js 0 (ref.cast $js (local.get 0))))))

   (func (export "caml_js_from_bool") (param (ref eq)) (result (ref eq))
      (struct.new $js
         (call $from_bool (i31.get_s (ref.cast i31 (local.get 0))))))

  (func (export "caml_js_pure_expr")
     (param (ref eq)) (result (ref eq))
     (return_call_ref $function_1
        (i31.new (i32.const 0))
        (local.get 0)
        (struct.get $closure 0
           (ref.cast $closure (local.get 0)))))

   (func (export "caml_js_fun_call")
      (param $f (ref eq)) (param $args (ref eq)) (result (ref eq))
      (return_call $wrap
         (call $fun_call (call $unwrap (local.get $f)) (ref.null any)
            (call $unwrap (call $caml_js_from_array (local.get $args))))))

   (func (export "caml_js_call")
      (param $f (ref eq)) (param $o (ref eq)) (param $args (ref eq))
      (result (ref eq))
      (return_call $wrap
         (call $fun_call (call $unwrap (local.get $f))
            (call $unwrap (local.get $o))
            (call $unwrap (call $caml_js_from_array (local.get $args))))))

   (func (export "caml_js_meth_call")
      (param $o (ref eq)) (param $f (ref eq)) (param $args (ref eq))
      (result (ref eq))
      (return_call $wrap
         (call $meth_call (call $unwrap (local.get $o))
            (call $unwrap (call $caml_jsstring_of_string (local.get $f)))
            (call $unwrap (call $caml_js_from_array (local.get $args))))))

   (func (export "caml_js_get")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (if (ref.test $string (local.get 1))
         (then
            (local.set 1 (call $caml_jsstring_of_string (local.get 1)))))
      (return_call $wrap
         (call $get (extern.externalize (call $unwrap (local.get 0)))
            (call $unwrap (local.get 1)))))

   (func (export "caml_js_set")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
      (if (ref.test $string (local.get 1))
         (then
            (local.set 1 (call $caml_jsstring_of_string (local.get 1)))))
      (call $set (call $unwrap (local.get 0)) (call $unwrap (local.get 1))
         (call $unwrap (local.get 2)))
      (i31.new (i32.const 0)))

   (func (export "caml_js_delete")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (if (ref.test $string (local.get 1))
         (then
            (local.set 1 (call $caml_jsstring_of_string (local.get 1)))))
      (call $delete (call $unwrap (local.get 0)) (call $unwrap (local.get 1)))
      (i31.new (i32.const 0)))

   (func (export "caml_js_instanceof")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (i31.new (call $instanceof
                  (call $unwrap (local.get 0)) (call $unwrap (local.get 1)))))

   (func (export "caml_js_typeof")
      (param (ref eq)) (result (ref eq))
      (struct.new $js (call $typeof (call $unwrap (local.get 0)))))

   (func (export "caml_js_new")
      (param $c (ref eq)) (param $args (ref eq)) (result (ref eq))
      (return_call $wrap
         (call $new (call $unwrap (local.get $c))
            (call $unwrap (call $caml_js_from_array (local.get $args))))))

   (func (export "caml_ojs_new_arr")
      (param $c (ref eq)) (param $args (ref eq)) (result (ref eq))
      (return_call $wrap
         (call $new (call $unwrap (local.get $c))
            (call $unwrap (local.get $args)))))

   (func (export "caml_ojs_iterate_properties")
      (param $o (ref eq)) (param $f (ref eq)) (result (ref eq))
      (call $iter_props
         (call $unwrap (local.get $o)) (call $unwrap (local.get $f)))
      (i31.new (i32.const 0)))

   (func (export "caml_js_object")
      (param (ref eq)) (result (ref eq))
      (local $a (ref $block)) (local $p (ref $block))
      (local $i i32) (local $l i32)
      (local $o anyref)
      (local.set $a (ref.cast $block (local.get 0)))
      (local.set $l (array.len (local.get $a)))
      (local.set $i (i32.const 1))
      (local.set $o (call $new_obj))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $l))
            (then
               (local.set $p
                  (ref.cast $block
                     (array.get $block (local.get $a) (local.get $i))))
               (call $set (local.get $o)
                  (call $unwrap
                     (call $caml_jsstring_of_string
                        (array.get $block (local.get $p) (i32.const 1))))
                  (call $unwrap
                        (array.get $block (local.get $p) (i32.const 2))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (struct.new $js (local.get $o)))

   (func $caml_js_from_array (export "caml_js_from_array")
      (param (ref eq)) (result (ref eq))
      (local $a (ref $block))
      (local $a' externref)
      (local $i i32) (local $l i32)
      (local.set $a (ref.cast $block (local.get 0)))
      (local.set $l (i32.sub (array.len (local.get $a)) (i32.const 1)))
      (local.set $a' (call $new_array (local.get $l)))
      (local.set $i (i32.const 0))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $l))
            (then
               (call $array_set (local.get $a') (local.get $i)
                  (call $unwrap (array.get $block (local.get $a)
                                   (i32.add (local.get $i) (i32.const 1)))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (struct.new $js (extern.internalize (local.get $a'))))

   (func (export "caml_js_to_array")
      (param (ref eq)) (result (ref eq))
      (local $a externref)
      (local $a' (ref $block))
      (local $i i32) (local $l i32)
      (local.set $a (extern.externalize (call $unwrap (local.get 0))))
      (local.set $l (call $array_length (local.get $a)))
      (local.set $a'
         (array.new $block (i31.new (i32.const 0))
            (i32.add (local.get $l) (i32.const 1))))
      (local.set $i (i32.const 0))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $l))
            (then
               (array.set $block (local.get $a')
                  (i32.add (local.get $i) (i32.const 1))
                  (call $wrap (call $array_get (local.get $a) (local.get $i))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (local.get $a'))

   (func $caml_js_wrap_callback (export "caml_js_wrap_callback")
      (param (ref eq)) (result (ref eq))
      (return_call $wrap (call $wrap_callback (local.get 0))))

   (func (export "caml_js_wrap_callback_arguments")
      (param (ref eq)) (result (ref eq))
      (return_call $wrap (call $wrap_callback_args (local.get 0))))

   (func (export "caml_js_wrap_callback_strict")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (return_call $wrap
         (call $wrap_callback_strict
            (i31.get_u (ref.cast i31 (local.get 0))) (local.get 1))))

   (func (export "caml_js_wrap_callback_unsafe")
      (param (ref eq)) (result (ref eq))
      (return_call $wrap (call $wrap_callback_unsafe (local.get 0))))

   (func (export "caml_js_wrap_meth_callback")
      (param (ref eq)) (result (ref eq))
      (return_call $wrap (call $wrap_meth_callback (local.get 0))))

   (func (export "caml_js_wrap_meth_callback_arguments")
      (param (ref eq)) (result (ref eq))
      (return_call $wrap (call $wrap_meth_callback_args (local.get 0))))

   (func (export "caml_js_wrap_meth_callback_strict")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (return_call $wrap
         (call $wrap_meth_callback_strict
            (i31.get_u (ref.cast i31 (local.get 0))) (local.get 1))))

   (func (export "caml_js_wrap_meth_callback_unsafe")
      (param (ref eq)) (result (ref eq))
      (return_call $wrap (call $wrap_meth_callback_unsafe (local.get 0))))

   (func (export "caml_ojs_wrap_fun_arguments")
      (param (ref eq)) (result (ref eq))
      (return_call $wrap
         (call $wrap_fun_arguments
            (call $wrap_callback_strict (i32.const 1) (local.get 0)))))

   (func (export "caml_callback")
      (param $f (ref eq)) (param $count i32) (param $args (ref extern))
      (param $kind i32) ;; 0 ==> strict / 2 ==> unsafe
      (result anyref)
      (local $acc (ref eq)) (local $i i32)
      (local.set $acc (local.get $f))
      (if (i32.eq (local.get $kind) (i32.const 2))
         (then
            (loop $loop
               (local.set $f (local.get $acc))
               (local.set $acc
                  (call_ref $function_1
                     (call $wrap
                        (call $get (local.get $args)
                           (i31.new (local.get $i))))
                     (local.get $acc)
                     (struct.get $closure 0
                        (ref.cast $closure (local.get $acc)))))
               (br_if $loop
                  (i32.eqz (ref.test $closure_last_arg (local.get $f))))))
         (else
            (local.set $i (i32.const 0))
            (loop $loop
               (if (i32.lt_u (local.get $i) (local.get $count))
                  (then
                     (local.set $acc
                        (call_ref $function_1
                           (call $wrap
                              (call $get (local.get $args)
                                 (i31.new (local.get $i))))
                           (local.get $acc)
                           (struct.get $closure 0
                              (ref.cast $closure (local.get $acc)))))
                     (local.set $i (i32.add (local.get $i) (i32.const 1)))
                     (br $loop))))
            (if (local.get $kind)
               (then
                  (if (ref.test $closure (local.get $acc))
                     (then (local.set $acc
                              (call $caml_js_wrap_callback
                                 (local.get $acc)))))))))
      (return_call $unwrap (local.get $acc)))

   (export "caml_js_from_string" (func $caml_jsstring_of_string))
   (func $caml_jsstring_of_string (export "caml_jsstring_of_string")
      (param (ref eq)) (result (ref eq))
      (local $s (ref $string))
      (local.set $s (ref.cast $string (local.get 0)))
      (struct.new $js
         (string.new_wtf8_array replace (local.get $s) (i32.const 0)
           (array.len (local.get $s)))))

   (func $caml_jsbytes_of_string (export "caml_jsbytes_of_string")
      (param (ref eq)) (result (ref eq))
      (local $s (ref $string))
      (local $s' (ref $string))
      (local $l i32) (local $i i32) (local $n i32) (local $c i32)
      (local.set $s (ref.cast $string (local.get 0)))
      (local.set $l (array.len (local.get $s)))
      (local.set $i (i32.const 0))
      (local.set $n (i32.const 0))
      (loop $count
         (if (i32.lt_u (local.get $i) (local.get $l))
            (then
               (if (i32.ge_u (array.get_u $string (local.get $s) (local.get $i))
                      (i32.const 128))
                  (then (local.set $n (i32.add (local.get $n) (i32.const 1)))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $count))))
      (if (i32.eqz (local.get $n))
         (then
            (return
               (struct.new $js
                  (string.new_wtf8_array utf8 (local.get $s) (i32.const 0)
                     (local.get $i))))))
      (local.set $s'
         (array.new $string (i32.const 0)
            (i32.add (local.get $i) (local.get $n))))
      (local.set $i (i32.const 0))
      (local.set $n (i32.const 0))
      (loop $fill
         (if (i32.lt_u (local.get $i) (local.get $l))
            (then
               (local.set $c (array.get_u $string (local.get $s) (local.get $i)))
               (if (i32.lt_u (local.get $c) (i32.const 128))
                  (then
                     (array.set $string
                        (local.get $s') (local.get $n) (local.get $c))
                     (local.set $n (i32.add (local.get $n) (i32.const 1))))
                  (else
                     (array.set $string (local.get $s')
                        (local.get $n)
                        (i32.or (i32.shr_u (local.get $c) (i32.const 6))
                           (i32.const 0xC0)))
                     (array.set $string (local.get $s')
                        (i32.add (local.get $n) (i32.const 1))
                        (i32.and (local.get $c) (i32.const 0x3F)))
                     (local.set $n (i32.add (local.get $n) (i32.const 2)))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $fill))))
      (struct.new $js
         (string.new_wtf8_array utf8 (local.get $s') (i32.const 0)
            (local.get $n))))

   (export "caml_js_to_string" (func $caml_string_of_jsstring))
   (func $caml_string_of_jsstring (export "caml_string_of_jsstring")
      (param (ref eq)) (result (ref eq))
      (local $s stringref)
      (local $l i32)
      (local $s' (ref $string))
      ;; ZZZ ref.cast string not yet implemented by V8
      (local.set $s
         (call $ref_cast_string (struct.get $js 0 (ref.cast $js (local.get 0)))))
      (local.set $l (string.measure_wtf8 wtf8 (local.get $s)))
      (local.set $s' (array.new $string (i32.const 0) (local.get $l)))
      (drop (string.encode_wtf8_array replace
               (local.get $s) (local.get $s') (i32.const 0)))
      (local.get $s'))

   (func (export "caml_string_of_jsbytes")
      (param (ref eq)) (result (ref eq))
      (local $s stringref)
      (local $l i32) (local $i i32) (local $n i32) (local $c i32)
      (local $s' (ref $string)) (local $s'' (ref $string))
      ;; ZZZ ref.cast string not yet implemented by V8
      (local.set $s
         (call $ref_cast_string (struct.get $js 0 (ref.cast $js (local.get 0)))))
      (local.set $l (string.measure_wtf8 wtf8 (local.get $s)))
      (local.set $s' (array.new $string (i32.const 0) (local.get $l)))
      (drop (string.encode_wtf8_array replace
               (local.get $s) (local.get $s') (i32.const 0)))
      (local.set $i (i32.const 0))
      (local.set $n (i32.const 0))
      (loop $count
         (if (i32.lt_u (local.get $i) (local.get $l))
            (then
               (if (i32.ge_u (array.get_u $string (local.get $s') (local.get $i))
                      (i32.const 0xC0))
                  (then (local.set $n (i32.add (local.get $n) (i32.const 1)))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $count))))
      (if (i32.eqz (local.get $n)) (then (return (local.get $s'))))
      (local.set $s''
         (array.new $string (i32.const 0)
            (i32.add (local.get $i) (local.get $n))))
      (local.set $i (i32.const 0))
      (local.set $n (i32.const 0))
      (loop $fill
         (if (i32.lt_u (local.get $i) (local.get $l))
            (then
               (local.set $c
                  (array.get_u $string (local.get $s') (local.get $i)))
               (if (i32.lt_u (local.get $c) (i32.const 0xC0))
                  (then
                     (array.set $string
                        (local.get $s'') (local.get $n) (local.get $c))
                     (local.set $i (i32.add (local.get $i) (i32.const 1))))
                  (else
                     (array.set $string (local.get $s'')
                        (local.get $n)
                        (i32.sub
                           (i32.or
                              (i32.shl (local.get $c) (i32.const 6))
                              (array.get_u $string (local.get $s')
                                 (i32.add (local.get $i) (i32.const 1))))
                           (i32.const 0X3080)))
                     (local.set $i (i32.add (local.get $i) (i32.const 2)))))
               (local.set $n (i32.add (local.get $n) (i32.const 1)))
               (br $fill))))
      (local.get $s''))

   (func (export "caml_list_to_js_array")
      (param (ref eq)) (result (ref eq))
      (local $i i32)
      (local $a externref)
      (local $l (ref eq))
      (local $b (ref $block))
      (local.set $i (i32.const 0))
      (local.set $l (local.get 0))
      (drop (block $done (result (ref eq))
         (loop $compute_length
            (local.set $l
               (array.get $block
                  (br_on_cast_fail $done $block (local.get $l)) (i32.const 2)))
            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (br $compute_length))))
      (local.set $a (call $new_array (local.get $i)))
      (local.set $i (i32.const 0))
      (local.set $l (local.get 0))
      (drop (block $exit (result (ref eq))
         (loop $loop
            (local.set $b (br_on_cast_fail $exit $block (local.get $l)))
            (call $array_set (local.get $a) (local.get $i)
               (call $unwrap (array.get $block (local.get $b) (i32.const 1))))
            (local.set $l (array.get $block (local.get $b) (i32.const 2)))
            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (br $loop))))
      (struct.new $js (extern.internalize (local.get $a))))

   (func (export "caml_list_of_js_array")
      (param (ref eq)) (result (ref eq))
      (local $l (ref eq))
      (local $i i32)
      (local $len i32)
      (local $a externref)
      (local.set $a (extern.externalize (call $unwrap (local.get 0))))
      (local.set $len (call $array_length (local.get $a)))
      (local.set $i (i32.const 0))
      (local.set $l (i31.new (i32.const 0)))
      (loop $loop
         (if (i32.le_u (local.get $i) (local.get $len))
            (then
               (local.set $l
                  (array.new_fixed $block (i31.new (i32.const 0))
                     (call $wrap
                        (call $array_get (local.get $a) (local.get $i)))
                     (local.get $l)))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (local.get $l))

   (func (export "caml_js_error_option_of_exception")
      (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))

   (func (export "caml_js_get_console")
     (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_js_get_console"))
      (i31.new (i32.const 0)))

  (func (export "caml_js_html_entities")
     (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_js_html_entities"))
      (i31.new (i32.const 0)))

  (func (export "caml_js_html_escape")
     (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_js_html_escape"))
      (i31.new (i32.const 0)))

  (func (export "caml_xmlhttprequest_create") (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_xmlhttprequest_create"))
      (i31.new (i32.const 0)))

  (func (export "caml_js_on_ie")
     (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_js_on_ie"))
      (i31.new (i32.const 0)))

  (func (export "bigstringaf_blit_from_bytes")
     (param (ref eq) (ref eq) (ref eq) (ref eq) (ref eq)) (result (ref eq))
     ;; ZZZ
     (call $log_js (string.const "bigstringaf_blit_from_bytes"))
     (i31.new (i32.const 0)))

  (func (export "bigstringaf_blit_to_bytes")
     (param (ref eq) (ref eq) (ref eq) (ref eq) (ref eq)) (result (ref eq))
     ;; ZZZ
     (call $log_js (string.const "bigstringaf_blit_to_bytes"))
     (i31.new (i32.const 0)))

  (func (export "caml_unwrap_value_from_string")
     (param (ref eq) (ref eq) (ref eq)) (result (ref eq))
     ;; ZZZ
     (call $log_js (string.const "caml_unwrap_value_from_string"))
     (i31.new (i32.const 0)))
)
