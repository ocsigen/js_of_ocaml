(module
   (import "ints" "parse_int"
      (func $parse_int
         (param (ref eq)) (param i32) (param (ref $string)) (result i32)))
   (import "ints" "format_int"
      (func $format_int
         (param (ref eq)) (param i32) (param i32) (result (ref eq))))
   (import "fail" "caml_failwith" (func $caml_failwith (param (ref eq))))
   (import "marshal" "caml_serialize_int_1"
      (func $caml_serialize_int_1 (param (ref eq)) (param i32)))
   (import "marshal" "caml_serialize_int_4"
      (func $caml_serialize_int_4 (param (ref eq)) (param i32)))
   (import "marshal" "caml_deserialize_uint_1"
      (func $caml_deserialize_uint_1 (param (ref eq)) (result i32)))
   (import "marshal" "caml_deserialize_int_4"
      (func $caml_deserialize_int_4 (param (ref eq)) (result i32)))

   (type $string (array (mut i8)))
   (type $compare
      (func (param (ref eq)) (param (ref eq)) (param i32) (result i32)))
   (type $hash
      (func (param (ref eq)) (result i32)))
   (type $fixed_length (struct (field $bsize_32 i32) (field $bsize_64 i32)))
   (type $serialize
      (func (param (ref eq)) (param (ref eq)) (result i32) (result i32)))
   (type $deserialize (func (param (ref eq)) (result (ref eq)) (result i32)))
   (type $custom_operations
      (struct
         (field $id (ref $string))
         (field $compare (ref null $compare))
         (field $compare_ext (ref null $compare))
         (field $hash (ref null $hash))
         (field $fixed_length (ref null $fixed_length))
         (field $serialize (ref null $serialize))
         (field $deserialize (ref null $deserialize))))
   (type $custom (sub (struct (field (ref $custom_operations)))))

   (global $int32_ops (export "int32_ops") (ref $custom_operations)
      (struct.new $custom_operations
         (array.new_fixed $string 2 (i32.const 95) (i32.const 105)) ;; "_i"
         (ref.func $int32_cmp)
         (ref.null $compare)
         (ref.func $int32_hash)
         (struct.new $fixed_length (i32.const 4) (i32.const 4))
         (ref.func $int32_serialize)
         (ref.func $int32_deserialize)))

   (type $int32
      (sub final $custom (struct (field (ref $custom_operations)) (field i32))))

   (func $int32_cmp
      (param $v1 (ref eq)) (param $v2 (ref eq)) (param i32) (result i32)
      (local $i1 i32) (local $i2 i32)
      (local.set $i1
         (struct.get $int32 1 (ref.cast (ref $int32) (local.get $v1))))
      (local.set $i2
         (struct.get $int32 1 (ref.cast (ref $int32) (local.get $v2))))
      (i32.sub (i32.gt_s (local.get $i1) (local.get $i2))
               (i32.lt_s (local.get $i1) (local.get $i2))))

   (func $int32_hash (param $v (ref eq)) (result i32)
      (struct.get $int32 1 (ref.cast (ref $int32) (local.get $v))))

   (func $int32_serialize
      (param $s (ref eq)) (param $v (ref eq)) (result i32) (result i32)
      (call $caml_serialize_int_4 (local.get $s)
         (struct.get $int32 1 (ref.cast (ref $int32) (local.get $v))))
      (tuple.make (i32.const 4) (i32.const 4)))

   (func $int32_deserialize (param $s (ref eq)) (result (ref eq)) (result i32)
      (tuple.make
         (struct.new $int32 (global.get $int32_ops)
            (call $caml_deserialize_int_4 (local.get $s)))
         (i32.const 4)))

   (func $caml_copy_int32 (export "caml_copy_int32")
      (param $i i32) (result (ref eq))
      (struct.new $int32 (global.get $int32_ops) (local.get $i)))

   (export "Nativeint_val" (func $Int32_val))
   (func $Int32_val (export "Int32_val") (param (ref eq)) (result i32)
      (struct.get $int32 1 (ref.cast (ref $int32) (local.get 0))))

   (func (export "caml_int32_bswap") (param (ref eq)) (result (ref eq))
      (local $i i32)
      (local.set $i (struct.get $int32 1 (ref.cast (ref $int32) (local.get 0))))
      (return_call $caml_copy_int32
         (i32.or
            (i32.rotr (i32.and (local.get $i) (i32.const 0x00FF00FF))
                      (i32.const 8))
            (i32.rotl (i32.and (local.get $i) (i32.const 0xFF00FF00))
                      (i32.const 8)))))

   (global $INT32_ERRMSG (ref $string)
      (array.new_fixed $string 15 ;; "Int32.of_string"
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
      (local.set $i1
         (struct.get $int32 1 (ref.cast (ref $int32) (local.get 0))))
      (local.set $i2
         (struct.get $int32 1 (ref.cast (ref $int32) (local.get 1))))
      (i31.new (i32.sub (i32.gt_s (local.get $i1) (local.get $i2))
                        (i32.lt_s (local.get $i1) (local.get $i2)))))

   (global $nativeint_ops (export "nativeint_ops") (ref $custom_operations)
      (struct.new $custom_operations
         (array.new_fixed $string 2 (i32.const 95) (i32.const 110)) ;; "_n"
         (ref.func $int32_cmp)
         (ref.null $compare)
         (ref.func $int32_hash)
         (struct.new $fixed_length (i32.const 4) (i32.const 8))
         (ref.func $nativeint_serialize)
         (ref.func $nativeint_deserialize)))

   (func $nativeint_serialize
      (param $s (ref eq)) (param $v (ref eq)) (result i32) (result i32)
      (call $caml_serialize_int_1 (local.get $s) (i32.const 1))
      (call $caml_serialize_int_4 (local.get $s)
         (struct.get $int32 1 (ref.cast (ref $int32) (local.get $v))))
      (tuple.make (i32.const 4) (i32.const 4)))

   (data $integer_too_large "input_value: native integer value too large")

   (func $nativeint_deserialize
      (param $s (ref eq)) (result (ref eq)) (result i32)
      (if (i32.ne (call $caml_deserialize_uint_1 (local.get $s)) (i32.const 1))
         (then
            (call $caml_failwith
               (array.new_data $string $integer_too_large
                  (i32.const 0) (i32.const 43)))))
      (tuple.make
         (struct.new $int32 (global.get $nativeint_ops)
            (call $caml_deserialize_int_4 (local.get $s)))
         (i32.const 4)))

   (func $caml_copy_nativeint (export "caml_copy_nativeint")
      (param $i i32) (result (ref eq))
      (struct.new $int32 (global.get $nativeint_ops) (local.get $i)))

   (global $NATIVEINT_ERRMSG (ref $string)
      (array.new_fixed $string 16 ;; "Nativeint.of_string"
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
         (struct.get $int32 1
            (ref.cast (ref $int32) (local.get 1))) (i32.const 0)))

   (func (export "caml_nativeint_of_int32") (param (ref eq)) (result (ref eq))
      (return_call $caml_copy_nativeint
         (struct.get $int32 1 (ref.cast (ref $int32) (local.get 0)))))

   (func (export "caml_nativeint_to_int32") (param (ref eq)) (result (ref eq))
      (return_call $caml_copy_int32
         (struct.get $int32 1 (ref.cast (ref $int32) (local.get 0)))))
)
