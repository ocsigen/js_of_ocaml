(module
   (import "bindings" "log" (func $log_js (param anyref)))
   (import "bindings" "ta_create"
      (func $ta_create (param i32) (param i32) (result (ref extern))))
   (import "bindings" "ta_normalize"
      (func $ta_normalize (param (ref extern)) (result (ref extern))))
   (import "bindings" "ta_kind"
      (func $ta_kind (param (ref extern)) (result i32)))
   (import "bindings" "ta_length"
      (func $ta_length (param (ref extern)) (result i32)))
   (import "bindings" "ta_get_f64"
      (func $ta_get_f64 (param (ref extern)) (param i32) (result f64)))
   (import "bindings" "ta_get_f32"
      (func $ta_get_f32 (param (ref extern)) (param i32) (result f64)))
   (import "bindings" "ta_get_i32"
      (func $ta_get_i32 (param (ref extern)) (param i32) (result i32)))
   (import "bindings" "ta_get_i16"
      (func $ta_get_i16 (param (ref extern)) (param i32) (result i32)))
   (import "bindings" "ta_get_ui16"
      (func $ta_get_ui16 (param (ref extern)) (param i32) (result i32)))
   (import "bindings" "ta_get_i8"
      (func $ta_get_i8 (param (ref extern)) (param i32) (result i32)))
   (import "bindings" "ta_get_ui8"
      (func $ta_get_ui8 (param (ref extern)) (param i32) (result i32)))
   (import "bindings" "ta_set_f64"
      (func $ta_set_f64 (param (ref extern)) (param i32) (param f64)))
   (import "bindings" "ta_set_f32"
      (func $ta_set_f32 (param (ref extern)) (param i32) (param f64)))
   (import "bindings" "ta_set_i32"
      (func $ta_set_i32 (param (ref extern)) (param i32) (param i32)))
   (import "bindings" "ta_set_i16"
      (func $ta_set_i16 (param (ref extern)) (param i32) (param (ref i31))))
   (import "bindings" "ta_set_ui16"
      (func $ta_set_ui16 (param (ref extern)) (param i32) (param (ref i31))))
   (import "bindings" "ta_set_i8"
      (func $ta_set_i8 (param (ref extern)) (param i32) (param (ref i31))))
   (import "bindings" "ta_set_ui8"
      (func $ta_set_ui8 (param (ref extern)) (param i32) (param (ref i31))))
   (import "fail" "caml_bound_error" (func $caml_bound_error))
   (import "fail" "caml_invalid_argument"
      (func $caml_invalid_argument (param (ref eq))))
   (import "jslib" "wrap" (func $wrap (param anyref) (result (ref eq))))
   (import "jslib" "unwrap" (func $unwrap (param (ref eq)) (result anyref)))
   (import "int32" "caml_copy_int32"
      (func $caml_copy_int32 (param i32) (result (ref eq))))
   (import "int32" "caml_copy_nativeint"
      (func $caml_copy_nativeint (param i32) (result (ref eq))))
   (import "int64" "caml_copy_int64"
      (func $caml_copy_int64 (param i64) (result (ref eq))))
   (import "obj" "double_array_tag" (global $double_array_tag i32))
   (import "compare" "unordered" (global $unordered i32))

   (type $block (array (mut (ref eq))))
   (type $string (array (mut i8)))
   (type $float (struct (field f64)))
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
   (type $int32
      (sub $custom (struct (field (ref $custom_operations)) (field i32))))
   (type $int64
      (sub $custom (struct (field (ref $custom_operations)) (field i64))))
   (type $int_array (array (mut i32)))

   (global $bigarray_ops (ref $custom_operations)
      ;; ZZZ
      (struct.new $custom_operations
         (array.new_fixed $string ;; "_bigarr02"
            (i32.const 95) (i32.const 98) (i32.const 105) (i32.const 103)
            (i32.const 97) (i32.const 114) (i32.const 114) (i32.const 48)
            (i32.const 50))
         (ref.func $caml_ba_compare)
         (ref.null $value->value->int->int)
         (ref.func $bigarray_hash)))

   (type $bigarray
      (sub $custom
         (struct
            (field (ref $custom_operations))
            (field $ba_data (mut (ref extern))) ;; data
            (field $ba_dim (ref $int_array)) ;; size in each dimension
            (field $ba_num_dims i8) ;; number of dimensions
            (field $ba_kind i8) ;; kind
            (field $ba_layout i8)))) ;; layout

   (func $bigarray_hash (param (ref eq)) (result i32)
      ;; ZZZ
      (call $log_js (string.const "bigarray_hash"))
      (i32.const 1))

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
     (param $kind i32) (param $sz i32) (result (ref extern))
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
      (local $data (ref extern))
      (local $kind i32)
      (local $len i32)
      (local.set $data
         (call $ta_normalize
            (ref.as_non_null (extern.externalize (call $unwrap (local.get 0))))))
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
            (struct.get $bigarray 1 (ref.cast $bigarray (local.get 0))))))

   (func $caml_ba_get_at_offset
      (param $ba (ref $bigarray)) (param $i i32) (result (ref eq))
      (local $data (ref extern))
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
      (local $data (ref extern))
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
      (local.set $i (i31.get_s (ref.cast i31 (local.get 1))))
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

   (func $caml_ba_compare
      (param $v1 (ref eq)) (param $v2 (ref eq)) (param $total i32) (result i32)
      (local $b1 (ref $bigarray)) (local $b2 (ref $bigarray))
      (local $i1 i32) (local $i2 i32) (local $i i32) (local $len i32)
      (local $f1 f64) (local $f2 f64)
      (local $d1 (ref extern)) (local $d2 (ref extern))
      (local.set $b1 (ref.cast $bigarray (local.get $v1)))
      (local.set $b2 (ref.cast $bigarray (local.get $v2)))
      (if (i32.ne (struct.get $bigarray $ba_layout (local.get $b2))
                  (struct.get $bigarray $ba_layout (local.get $b1)))
         (then
            (return
               (i32.sub (struct.get $bigarray $ba_layout (local.get $b2))
                        (struct.get $bigarray $ba_layout (local.get $b1))))))
      (if (i32.ne (struct.get $bigarray $ba_kind (local.get $b2))
                  (struct.get $bigarray $ba_kind (local.get $b1)))
         (then
            (return
               (i32.sub (struct.get $bigarray $ba_kind (local.get $b2))
                        (struct.get $bigarray $ba_kind (local.get $b1))))))
      (if (i32.ne (struct.get $bigarray $ba_num_dims (local.get $b2))
                  (struct.get $bigarray $ba_num_dims (local.get $b1)))
         (then
            (return
               (i32.sub (struct.get $bigarray $ba_num_dims (local.get $b2))
                        (struct.get $bigarray $ba_num_dims (local.get $b1))))))
      (local.set $len (struct.get $bigarray $ba_num_dims (local.get $b2)))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $len))
            (then
                (local.set $i1
                   (array.get $int_array
                      (struct.get $bigarray $ba_dim (local.get $b1))
                      (local.get $i)))
                (local.set $i2
                   (array.get $int_array
                      (struct.get $bigarray $ba_dim (local.get $b2))
                      (local.get $i)))
                (if (i32.ne (local.get $i1) (local.get $i2))
                   (return
                      (select (i32.const -1) (i32.const 1)
                         (i32.lt_u (local.get $i1) (local.get $i2)))))
                (local.set $i (i32.add (local.get $i) (i32.const 1)))
                (br $loop))))
      (local.set $d1 (struct.get $bigarray $ba_data (local.get $b1)))
      (local.set $d2 (struct.get $bigarray $ba_data (local.get $b2)))
      (local.set $len (call $ta_length (local.get $d1)))
      (local.set $i (i32.const 0))
      (block $float32
       (block $float64
        (block $int8
         (block $uint8
          (block $int16
           (block $uint16
            (block $int32
             (block $int64
              (br_table $float32 $float64 $int8 $uint8 $int16 $uint16
                        $int32 $int64 $int32 $int32
                        $float32 $float64 $uint8
                 (struct.get $bigarray $ba_kind (local.get $b1))))
             ;; int64
             (loop $loop
                (if (i32.lt_u (local.get $i) (local.get $len))
                   (then
                      (local.set $i1
                         (call $ta_get_i32 (local.get $d1)
                            (i32.add (local.get $i) (i32.const 1))))
                      (local.set $i2
                         (call $ta_get_i32 (local.get $d2)
                            (i32.add (local.get $i) (i32.const 1))))
                      (if (i32.lt_s (local.get $i1) (local.get $i2))
                         (then (return (i32.const -1))))
                      (if (i32.gt_s (local.get $i1) (local.get $i2))
                         (then (return (i32.const 1))))
                      (local.set $i1
                         (call $ta_get_i32 (local.get $d1) (local.get $i)))
                      (local.set $i2
                         (call $ta_get_i32 (local.get $d2) (local.get $i)))
                      (if (i32.lt_u (local.get $i1) (local.get $i2))
                         (then (return (i32.const -1))))
                      (if (i32.gt_u (local.get $i1) (local.get $i2))
                         (then (return (i32.const 1))))
                      (local.set $i (i32.add (local.get $i) (i32.const 2)))
                      (br $loop))))
             (return (i32.const 0)))
            ;; int32
            (loop $loop
               (if (i32.lt_u (local.get $i) (local.get $len))
                  (then
                     (local.set $i1
                        (call $ta_get_i32 (local.get $d1) (local.get $i)))
                     (local.set $i2
                        (call $ta_get_i32 (local.get $d2) (local.get $i)))
                     (if (i32.lt_s (local.get $i1) (local.get $i2))
                        (then (return (i32.const -1))))
                     (if (i32.gt_s (local.get $i1) (local.get $i2))
                        (then (return (i32.const 1))))
                     (local.set $i (i32.add (local.get $i) (i32.const 1)))
                     (br $loop))))
            (return (i32.const 0)))
           ;; uint16
           (loop $loop
              (if (i32.lt_u (local.get $i) (local.get $len))
                 (then
                    (local.set $i1
                       (call $ta_get_ui16 (local.get $d1) (local.get $i)))
                    (local.set $i2
                       (call $ta_get_ui16 (local.get $d2) (local.get $i)))
                    (if (i32.lt_s (local.get $i1) (local.get $i2))
                       (then (return (i32.const -1))))
                    (if (i32.gt_s (local.get $i1) (local.get $i2))
                       (then (return (i32.const 1))))
                    (local.set $i (i32.add (local.get $i) (i32.const 1)))
                    (br $loop))))
           (return (i32.const 0)))
          ;; int16
          (loop $loop
             (if (i32.lt_u (local.get $i) (local.get $len))
                (then
                   (local.set $i1
                      (call $ta_get_i16 (local.get $d1) (local.get $i)))
                   (local.set $i2
                      (call $ta_get_i16 (local.get $d2) (local.get $i)))
                   (if (i32.lt_s (local.get $i1) (local.get $i2))
                      (then (return (i32.const -1))))
                   (if (i32.gt_s (local.get $i1) (local.get $i2))
                      (then (return (i32.const 1))))
                   (local.set $i (i32.add (local.get $i) (i32.const 1)))
                   (br $loop))))
          (return (i32.const 0)))
         ;; uint8
         (loop $loop
            (if (i32.lt_u (local.get $i) (local.get $len))
               (then
                  (local.set $i1
                     (call $ta_get_ui8 (local.get $d1) (local.get $i)))
                  (local.set $i2
                     (call $ta_get_ui8 (local.get $d2) (local.get $i)))
                  (if (i32.lt_s (local.get $i1) (local.get $i2))
                     (then (return (i32.const -1))))
                  (if (i32.gt_s (local.get $i1) (local.get $i2))
                     (then (return (i32.const 1))))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (br $loop))))
         (return (i32.const 0)))
        ;; int8
        (loop $loop
           (if (i32.lt_u (local.get $i) (local.get $len))
              (then
                 (local.set $i1
                    (call $ta_get_i8 (local.get $d1) (local.get $i)))
                 (local.set $i2
                    (call $ta_get_i8 (local.get $d2) (local.get $i)))
                 (if (i32.lt_s (local.get $i1) (local.get $i2))
                    (then (return (i32.const -1))))
                 (if (i32.gt_s (local.get $i1) (local.get $i2))
                    (then (return (i32.const 1))))
                 (local.set $i (i32.add (local.get $i) (i32.const 1)))
                 (br $loop))))
        (return (i32.const 0)))
       ;; float64
       (loop $loop
          (if (i32.lt_u (local.get $i) (local.get $len))
             (then
                (local.set $f1
                   (call $ta_get_f64 (local.get $d1) (local.get $i)))
                (local.set $f2
                   (call $ta_get_f64 (local.get $d2) (local.get $i)))
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
                (local.set $i (i32.add (local.get $i) (i32.const 1)))
                (br $loop))))
       (return (i32.const 0)))
      ;; float32
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $len))
            (then
               (local.set $f1
                  (call $ta_get_f32 (local.get $d1) (local.get $i)))
               (local.set $f2
                  (call $ta_get_f32 (local.get $d2) (local.get $i)))
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
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (return (i32.const 0)))

   (func (export "caml_string_of_array") (param (ref eq)) (result (ref eq))
      ;; used to convert a typed array to a string
      (local $a (ref extern)) (local $len i32) (local $i i32)
      (local $s (ref $string))
      (local.set $a
         (ref.as_non_null (extern.externalize (call $unwrap (local.get 0)))))
      (local.set $len (call $ta_length (local.get $a)))
      (local.set $s (array.new $string (i32.const 0) (local.get $len)))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $len))
            (then
               (array.set $string (local.get $s) (local.get $i)
                  (call $ta_get_ui8 (local.get $a) (local.get $i)))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (local.get $s))
)
