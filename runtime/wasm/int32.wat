(module
   (import "ints" "parse_int"
      (func $parse_int
         (param (ref eq)) (param i32) (param (ref $string)) (result i32)))
   (import "ints" "format_int"
      (func $format_int
         (param (ref eq)) (param i32) (param i32) (result (ref eq))))

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

   (global $int32_ops (export "int32_ops") (ref $custom_operations)
      (struct.new $custom_operations
         (array.new_fixed $string (i32.const 95) (i32.const 105)) ;; "_i"
         (ref.func $int32_cmp)
         (ref.null $value->value->int->int)
         (ref.func $int32_hash)))

   (type $int32
      (sub $custom (struct (field (ref $custom_operations)) (field i32))))

   (func $int32_cmp
      (param $v1 (ref eq)) (param $v2 (ref eq)) (param i32) (result i32)
      (local $i1 i32) (local $i2 i32)
      (local.set $i1 (struct.get $int32 1 (ref.cast $int32 (local.get $v1))))
      (local.set $i2 (struct.get $int32 1 (ref.cast $int32 (local.get $v2))))
      (i32.sub (i32.gt_s (local.get $i1) (local.get $i2))
               (i32.lt_s (local.get $i1) (local.get $i2))))

   (func $int32_hash (param $v (ref eq)) (result i32)
      (struct.get $int32 1 (ref.cast $int32 (local.get $v))))

   (func $caml_copy_int32 (export "caml_copy_int32")
      (param $i i32) (result (ref eq))
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

   (global $nativeint_ops (export "nativeint_ops") (ref $custom_operations)
      (struct.new $custom_operations
         (array.new_fixed $string (i32.const 95) (i32.const 110)) ;; "_n"
         (ref.func $int32_cmp)
         (ref.null $value->value->int->int)
         (ref.func $int32_hash)))

   (func $caml_copy_nativeint (export "caml_copy_nativeint")
      (param $i i32) (result (ref eq))
      (struct.new $int32 (global.get $nativeint_ops) (local.get $i)))

   (global $NATIVEINT_ERRMSG (ref $string)
      (array.new_fixed $string ;; "Nativeint.of_string"
         (i32.const 78) (i32.const 97) (i32.const 116) (i32.const 105)
         (i32.const 118) (i32.const 101) (i32.const 46) (i32.const 111)
         (i32.const 102) (i32.const 95) (i32.const 115) (i32.const 116)
         (i32.const 114) (i32.const 105) (i32.const 110) (i32.const 103)))

   (func (export "caml_nativeint_of_string")
      (param $v (ref eq)) (result (ref eq))
      (return_call $caml_copy_nativeint
         (call $parse_int
            (local.get $v) (i32.const 32) (global.get $NATIVEINT_ERRMSG))))

   (export "caml_nativeint_format" (func $caml_int32_format))
   (func $caml_int32_format (export "caml_int32_format")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (return_call $format_int (local.get 0)
         (struct.get $int32 1 (ref.cast $int32 (local.get 1))) (i32.const 0)))

   (func (export "caml_nativeint_of_int32") (param (ref eq)) (result (ref eq))
      (return_call $caml_copy_nativeint
         (struct.get $int32 1 (ref.cast $int32 (local.get 0)))))

   (func (export "caml_nativeint_to_int32") (param (ref eq)) (result (ref eq))
      (return_call $caml_copy_int32
         (struct.get $int32 1 (ref.cast $int32 (local.get 0)))))
)
