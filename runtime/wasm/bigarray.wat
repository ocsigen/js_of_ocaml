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
   (import "bindings" "ta_fill"
      (func $ta_fill_int (param (ref extern)) (param i32)))
   (import "bindings" "ta_fill"
      (func $ta_fill_float (param (ref extern)) (param f64)))
   (import "bindings" "ta_blit"
      (func $ta_blit (param (ref extern)) (param (ref extern))))
   (import "bindings" "ta_subarray"
      (func $ta_subarray
         (param (ref extern)) (param i32) (param i32) (result (ref extern))))
   (import "fail" "caml_bound_error" (func $caml_bound_error))
   (import "fail" "caml_raise_out_of_memory" (func $caml_raise_out_of_memory))
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
   (import "hash" "caml_hash_mix_int"
      (func $caml_hash_mix_int (param i32) (param i32) (result i32)))
   (import "hash" "caml_hash_mix_int64"
      (func $caml_hash_mix_int64 (param i32) (param i64) (result i32)))
   (import "hash" "caml_hash_mix_float"
      (func $caml_hash_mix_float (param i32) (param f64) (result i32)))
   (import "hash" "caml_hash_mix_float32"
      (func $caml_hash_mix_float32 (param i32) (param f32) (result i32)))

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
      (sub final $custom (struct (field (ref $custom_operations)) (field i32))))
   (type $int64
      (sub final $custom (struct (field (ref $custom_operations)) (field i64))))
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
      (sub final $custom
         (struct
            (field (ref $custom_operations))
            (field $ba_data (mut (ref extern))) ;; data
            (field $ba_dim (ref $int_array)) ;; size in each dimension
            (field $ba_num_dims i8) ;; number of dimensions
            (field $ba_kind i8) ;; kind
            (field $ba_layout i8)))) ;; layout

   (func $bigarray_hash (param (ref eq)) (result i32)
      (local $b (ref $bigarray))
      (local $h i32) (local $len i32) (local $i i32) (local $w i32)
      (local $data (ref extern))
      (local.set $b (ref.cast $bigarray (local.get 0)))
      (local.set $data (struct.get $bigarray $ba_data (local.get $b)))
      (local.set $len (call $ta_length (local.get $data)))
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
                 (struct.get $bigarray $ba_kind (local.get $b))))
             ;; int64
             (if (i32.gt_u (local.get $len) (i32.const 32))
                (then (local.set $len (i32.const 32))))
             (loop $loop
                (if (i32.lt_u (local.get $i) (local.get $len))
                   (then
                      (local.set $h
                         (call $caml_hash_mix_int64 (local.get $h)
                            (i64.or
                               (i64.extend_i32_u
                                  (call $ta_get_i32 (local.get $data)
                                     (local.get $i)))
                               (i64.shl
                                  (i64.extend_i32_u
                                     (call $ta_get_i32 (local.get $data)
                                        (i32.add (local.get $i) (i32.const 1))))
                                  (i64.const 32)))))
                      (local.set $i (i32.add (local.get $i) (i32.const 2)))
                      (br $loop))))
             (return (local.get $h)))
            ;; int32
            (if (i32.gt_u (local.get $len) (i32.const 64))
               (then (local.set $len (i32.const 64))))
            (loop $loop
               (if (i32.lt_u (local.get $i) (local.get $len))
                  (then
                     (local.set $h
                        (call $caml_hash_mix_int (local.get $h)
                           (call $ta_get_i32 (local.get $data) (local.get $i))))
                     (local.set $i (i32.add (local.get $i) (i32.const 1)))
                     (br $loop))))
            (return (local.get $h)))
           ;; uint16
           (if (i32.gt_u (local.get $len) (i32.const 128))
              (then (local.set $len (i32.const 182))))
           (loop $loop
              (if (i32.le_u (i32.add (local.get $i) (i32.const 2))
                     (local.get $len))
                 (then
                    (local.set $h
                       (call $caml_hash_mix_int
                          (local.get $h)
                          (i32.or
                             (call $ta_get_ui16 (local.get $data) (local.get $i))
                             (i32.shl (call $ta_get_ui16 (local.get $data)
                                         (i32.add (local.get $i) (i32.const 1)))
                                      (i32.const 16)))))
                    (local.set $i (i32.add (local.get $i) (i32.const 2)))
                    (br $loop))))
           (if (i32.and (local.get $len) (i32.const 1))
              (then
                 (local.set $h
                    (call $caml_hash_mix_int (local.get $h)
                       (call $ta_get_ui16 (local.get $data) (local.get $i))))))
           (return (local.get $h)))
          ;; int16
          (if (i32.gt_u (local.get $len) (i32.const 128))
             (then (local.set $len (i32.const 182))))
          (loop $loop
             (if (i32.le_u (i32.add (local.get $i) (i32.const 2))
                    (local.get $len))
                (then
                   (local.set $h
                      (call $caml_hash_mix_int
                         (local.get $h)
                         (i32.or
                            (call $ta_get_i16 (local.get $data) (local.get $i))
                            (i32.shl (call $ta_get_i16 (local.get $data)
                                        (i32.add (local.get $i) (i32.const 1)))
                                     (i32.const 16)))))
                   (local.set $i (i32.add (local.get $i) (i32.const 2)))
                   (br $loop))))
          (if (i32.and (local.get $len) (i32.const 1))
             (then
                (local.set $h
                   (call $caml_hash_mix_int (local.get $h)
                      (call $ta_get_i16 (local.get $data) (local.get $i))))))
          (return (local.get $h)))
         ;; uint8
         (if (i32.gt_u (local.get $len) (i32.const 256))
            (then (local.set $len (i32.const 256))))
         (loop $loop
            (if (i32.le_u (i32.add (local.get $i) (i32.const 4))
                   (local.get $len))
               (then
                  (local.set $h
                     (call $caml_hash_mix_int
                        (local.get $h)
                        (i32.or
                           (i32.or
                              (call $ta_get_ui8 (local.get $data) (local.get $i))
                              (i32.shl (call $ta_get_ui8 (local.get $data)
                                          (i32.add (local.get $i) (i32.const 1)))
                                       (i32.const 8)))
                           (i32.or
                              (i32.shl (call $ta_get_ui8 (local.get $data)
                                          (i32.add (local.get $i) (i32.const 2)))
                                       (i32.const 16))
                              (i32.shl (call $ta_get_ui8 (local.get $data)
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
                     (i32.shl (call $ta_get_ui8 (local.get $data)
                                 (i32.add (local.get $i) (i32.const 2)))
                              (i32.const 16))))
               (local.set $w
                  (i32.or (local.get $w)
                     (i32.shl (call $ta_get_ui8 (local.get $data)
                                 (i32.add (local.get $i) (i32.const 1)))
                              (i32.const 8)))))
            (local.set $w
               (i32.or (local.get $w)
                  (call $ta_get_ui8 (local.get $data) (local.get $i))))
            (local.set $h
               (call $caml_hash_mix_int (local.get $h) (local.get $w))))
         (return (local.get $h)))
        ;; int8
        (if (i32.gt_u (local.get $len) (i32.const 256))
           (then (local.set $len (i32.const 256))))
        (loop $loop
           (if (i32.le_u (i32.add (local.get $i) (i32.const 4)) (local.get $len))
              (then
                 (local.set $h
                    (call $caml_hash_mix_int
                       (local.get $h)
                       (i32.or
                          (i32.or
                             (call $ta_get_i8 (local.get $data) (local.get $i))
                             (i32.shl (call $ta_get_i8 (local.get $data)
                                         (i32.add (local.get $i) (i32.const 1)))
                                      (i32.const 8)))
                          (i32.or
                             (i32.shl (call $ta_get_i8 (local.get $data)
                                         (i32.add (local.get $i) (i32.const 2)))
                                      (i32.const 16))
                             (i32.shl (call $ta_get_i8 (local.get $data)
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
                    (i32.shl (call $ta_get_i8 (local.get $data)
                                (i32.add (local.get $i) (i32.const 2)))
                             (i32.const 16))))
              (local.set $w
                 (i32.or (local.get $w)
                    (i32.shl (call $ta_get_i8 (local.get $data)
                                (i32.add (local.get $i) (i32.const 1)))
                             (i32.const 8)))))
           (local.set $w
              (i32.or (local.get $w)
                 (call $ta_get_i8 (local.get $data) (local.get $i))))
           (local.set $h
              (call $caml_hash_mix_int (local.get $h) (local.get $w))))
        (return (local.get $h)))
       ;; float32
       (if (i32.gt_u (local.get $len) (i32.const 64))
          (then (local.set $len (i32.const 64))))
       (loop $loop
          (if (i32.lt_u (local.get $i) (local.get $len))
             (then
                (local.set $h
                   (call $caml_hash_mix_float32 (local.get $h)
                      (f32.demote_f64
                         (call $ta_get_f32 (local.get $data) (local.get $i)))))
                (local.set $i (i32.add (local.get $i) (i32.const 1)))
                (br $loop))))
       (return (local.get $h)))
      ;; float64
      (if (i32.gt_u (local.get $len) (i32.const 32))
         (then (local.set $len (i32.const 32))))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $len))
            (then
               (local.set $h
                  (call $caml_hash_mix_float (local.get $h)
                     (call $ta_get_f64 (local.get $data) (local.get $i))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (return (local.get $h)))

   (func $caml_ba_get_size (param $dim (ref $int_array)) (result i32)
      (local $i i32) (local $n i32) (local $sz i64)
      (local.set $n (array.len (local.get $dim)))
      (local.set $i (i32.const 0))
      (local.set $sz (i64.const 1))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $n))
            (then
               (local.set $sz
                   (i64.mul (local.get $sz)
                      (i64.extend_i32_s
                         (array.get $int_array
                            (local.get $dim) (local.get $i)))))
               (if (i64.ne (local.get $sz)
                      (i64.extend_i32_s (i32.wrap_i64 (local.get $sz))))
                  (then (call $caml_raise_out_of_memory)))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (i32.wrap_i64 (local.get $sz)))

  (func $caml_ba_size_per_element (param $kind i32) (result i32)
     (select (i32.const 2) (i32.const 1)
        (i32.or (i32.eq (local.get $kind) (i32.const 7))
                (i32.or (i32.eq (local.get $kind) (i32.const 10))
                        (i32.eq (local.get $kind) (i32.const 11))))))

  (func $caml_ba_create_buffer (export "caml_ba_create_buffer")
     (param $kind i32) (param $sz i32) (result (ref extern))
     (local $l i64)
     (local.set $l
        (i64.mul (i64.extend_i32_s (local.get $sz))
           (i64.extend_i32_s
              (call $caml_ba_size_per_element (local.get $kind)))))
     (if (i64.ne (local.get $l) (i64.extend_i32_s (i32.wrap_i64 (local.get $l))))
        (then (call $caml_raise_out_of_memory)))
     (return_call $ta_create (local.get $kind) (i32.wrap_i64 (local.get $l))))

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
            (struct.get $bigarray $ba_data (ref.cast $bigarray (local.get 0))))))

   (func $caml_ba_get_at_offset
      (param $ba (ref $bigarray)) (param $i i32) (result (ref eq))
      (local $data (ref extern))
      (local.set $data (struct.get $bigarray $ba_data (local.get $ba)))
      (block $float32
       (block $float64
        (block $int8
         (block $uint8
          (block $int16
           (block $uint16
            (block $int32
             (block $int64
              (block $int
               (block $nativeint
                (block $complex32
                 (block $complex64
                  (br_table $float32 $float64 $int8 $uint8 $int16 $uint16
                            $int32 $int64 $int $nativeint
                            $complex32 $complex64 $uint8
                     (struct.get $bigarray $ba_kind (local.get $ba))))
                 ;; complex64
                 (local.set $i (i32.shl (local.get $i) (i32.const 1)))
                 (return
                    (array.new_fixed $block
                       (i31.new (global.get $double_array_tag))
                       (struct.new $float
                          (call $ta_get_f64 (local.get $data) (local.get $i)))
                       (struct.new $float
                          (call $ta_get_f64 (local.get $data)
                             (i32.add (local.get $i) (i32.const 1)))))))
                ;; complex32
                (local.set $i (i32.shl (local.get $i) (i32.const 1)))
                (return
                   (array.new_fixed $block
                      (i31.new (global.get $double_array_tag))
                      (struct.new $float
                         (call $ta_get_f32 (local.get $data) (local.get $i)))
                      (struct.new $float
                         (call $ta_get_f32 (local.get $data)
                            (i32.add (local.get $i) (i32.const 1)))))))
               ;; nativeint
               (return_call $caml_copy_nativeint
                  (call $ta_get_i32 (local.get $data) (local.get $i))))
              ;; int
              (return
                 (i31.new
                    (call $ta_get_i32 (local.get $data) (local.get $i)))))
             ;; int64
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
            ;; int32
            (return_call $caml_copy_int32
               (call $ta_get_i32 (local.get $data) (local.get $i))))
           ;; uint16
           (return (i31.new
                      (call $ta_get_ui16 (local.get $data) (local.get $i)))))
          ;; int16
          (return (i31.new
                     (call $ta_get_i16 (local.get $data) (local.get $i)))))
         ;; uint8
         (return (i31.new
                    (call $ta_get_ui8 (local.get $data) (local.get $i)))))
        ;; int8
        (return (i31.new
                   (call $ta_get_i8 (local.get $data) (local.get $i)))))
       ;; float64
       (return (struct.new $float
                  (call $ta_get_f64 (local.get $data) (local.get $i)))))
      ;; float32
      (return (struct.new $float
                 (call $ta_get_f32 (local.get $data) (local.get $i)))))

   (func $caml_ba_set_at_offset
      (param $ba (ref $bigarray)) (param $i i32) (param $v (ref eq))
      (local $data (ref extern))
      (local $b (ref $block)) (local $l i64)
      (local.set $data (struct.get $bigarray $ba_data (local.get $ba)))
      (block $float32
       (block $float64
        (block $int8
         (block $uint8
          (block $int16
           (block $uint16
            (block $int32
             (block $int64
              (block $int
               (block $nativeint
                (block $complex32
                 (block $complex64
                  (br_table $float32 $float64 $int8 $uint8 $int16 $uint16
                            $int32 $int64 $int $nativeint
                            $complex32 $complex64 $uint8
                     (struct.get $bigarray $ba_kind (local.get $ba))))
                 ;; complex64
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
                ;; complex32
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
               ;; nativeint
               (call $ta_set_i32 (local.get $data) (local.get $i)
                  (struct.get $int32 1 (ref.cast $int32 (local.get $v))))
               (return))
              ;; int
              (call $ta_set_i32 (local.get $data) (local.get $i)
                 (i31.get_s (ref.cast i31 (local.get $v))))
              (return))
             ;; int64
             (local.set $i (i32.shl (local.get $i) (i32.const 1)))
             (local.set $l
                (struct.get $int64 1 (ref.cast $int64 (local.get $v))))
             (call $ta_set_i32 (local.get $data) (local.get $i)
                (i32.wrap_i64 (local.get $l)))
             (call $ta_set_i32 (local.get $data)
                (i32.add (local.get $i) (i32.const 1))
                (i32.wrap_i64 (i64.shr_u (local.get $l) (i64.const 32))))
             (return))
            ;; int32
            (call $ta_set_i32 (local.get $data) (local.get $i)
               (struct.get $int32 1 (ref.cast $int32 (local.get $v))))
            (return))
           ;; uint16
           (call $ta_set_ui16 (local.get $data) (local.get $i)
              (ref.cast i31 (local.get $v)))
           (return))
          ;; int16
          (call $ta_set_i16 (local.get $data) (local.get $i)
             (ref.cast i31 (local.get $v)))
          (return))
         ;; uint8
         (call $ta_set_ui8 (local.get $data) (local.get $i)
            (ref.cast i31 (local.get $v)))
         (return))
        ;; int8
        (call $ta_set_i8 (local.get $data) (local.get $i)
           (ref.cast i31 (local.get $v)))
        (return))
       ;; float64
       (call $ta_set_f64 (local.get $data) (local.get $i)
          (struct.get $float 0 (ref.cast $float (local.get $v))))
       (return))
      ;; float32
      (call $ta_set_f32 (local.get $data) (local.get $i)
         (struct.get $float 0 (ref.cast $float (local.get $v))))
      (return))

   (data $Bigarray_dim "Bigarray.dim")

   (func $caml_ba_dim (export "caml_ba_dim")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (local $dim (ref $int_array))
      (local $i i32)
      (local.set $dim
         (struct.get $bigarray $ba_dim (ref.cast $bigarray (local.get 0))))
      (local.set $i (i31.get_s (ref.cast i31 (local.get 1))))
      (if (i32.ge_u (local.get $i) (array.len (local.get $dim)))
         (then (call $caml_invalid_argument
                  (array.new_data $string $Bigarray_dim
                     (i32.const 0) (i32.const 12)))))
      (i31.new (array.get $int_array (local.get $dim) (local.get $i))))

   (func (export "caml_ba_dim_1") (param (ref eq)) (result (ref eq))
      (return_call $caml_ba_dim (local.get 0) (i31.new (i32.const 0))))

   (func (export "caml_ba_get_1")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (local $ba (ref $bigarray))
      (local $i i32)
      (local.set $ba (ref.cast $bigarray (local.get 0)))
      (local.set $i (i31.get_u (ref.cast i31 (local.get 1))))
      (if (struct.get $bigarray $ba_layout (local.get $ba))
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
      (if (struct.get $bigarray $ba_layout (local.get $ba))
         (then (local.set $i (i32.sub (local.get $i) (i32.const 1)))))
      (if (i32.ge_u (local.get $i)
             (array.get $int_array (struct.get $bigarray $ba_dim (local.get $ba))
                (i32.const 0)))
         (call $caml_bound_error))
      (call $caml_ba_set_at_offset
         (local.get $ba) (local.get $i) (local.get $v))
      (i31.new (i32.const 0)))

   (func (export "caml_ba_get_2")
      (param $vba (ref eq)) (param $vi (ref eq)) (param $vj (ref eq))
      (result (ref eq))
      (local $ba (ref $bigarray))
      (local $i i32)
      (local $j i32)
      (local $offset i32)
      (local $dim (ref $int_array))
      (local.set $ba (ref.cast $bigarray (local.get $vba)))
      (local.set $i (i31.get_u (ref.cast i31 (local.get $vi))))
      (local.set $j (i31.get_u (ref.cast i31 (local.get $vj))))
      (local.set $dim (struct.get $bigarray $ba_dim (local.get $ba)))
      (if (struct.get $bigarray $ba_layout (local.get $ba))
         (then
            (local.set $i (i32.sub (local.get $i) (i32.const 1)))
            (local.set $j (i32.sub (local.get $j) (i32.const 1)))
            (local.set $offset
               (i32.add
                  (i32.mul (local.get $j)
                     (array.get $int_array (local.get $dim) (i32.const 0)))
                  (local.get $i))))
         (else
            (local.set $offset
               (i32.add
                  (i32.mul (local.get $i)
                     (array.get $int_array (local.get $dim) (i32.const 1)))
                  (local.get $j)))))
      (if (i32.or
             (i32.ge_u (local.get $i)
                (array.get $int_array (local.get $dim) (i32.const 0)))
             (i32.ge_u (local.get $j)
                (array.get $int_array (local.get $dim) (i32.const 1))))
         (then
            (call $caml_bound_error)))
      (return_call $caml_ba_get_at_offset (local.get $ba) (local.get $offset)))

   (func (export "caml_ba_set_2")
      (param $vba (ref eq)) (param $vi (ref eq)) (param $vj (ref eq))
      (param $v (ref eq)) (result (ref eq))
      (local $ba (ref $bigarray))
      (local $i i32)
      (local $j i32)
      (local $offset i32)
      (local $dim (ref $int_array))
      (local.set $ba (ref.cast $bigarray (local.get $vba)))
      (local.set $i (i31.get_u (ref.cast i31 (local.get $vi))))
      (local.set $j (i31.get_u (ref.cast i31 (local.get $vj))))
      (local.set $dim (struct.get $bigarray $ba_dim (local.get $ba)))
      (if (struct.get $bigarray $ba_layout (local.get $ba))
         (then
            (local.set $i (i32.sub (local.get $i) (i32.const 1)))
            (local.set $j (i32.sub (local.get $j) (i32.const 1)))
            (local.set $offset
               (i32.add
                  (i32.mul (local.get $j)
                     (array.get $int_array (local.get $dim) (i32.const 0)))
                  (local.get $i))))
         (else
            (local.set $offset
               (i32.add
                  (i32.mul (local.get $i)
                     (array.get $int_array (local.get $dim) (i32.const 1)))
                  (local.get $j)))))
      (if (i32.or
             (i32.ge_u (local.get $i)
                (array.get $int_array (local.get $dim) (i32.const 0)))
             (i32.ge_u (local.get $j)
                (array.get $int_array (local.get $dim) (i32.const 1))))
         (then
            (call $caml_bound_error)))
      (call $caml_ba_set_at_offset
         (local.get $ba) (local.get $offset) (local.get $v))
      (i31.new (i32.const 0)))

   (func (export "caml_ba_dim_2") (param (ref eq)) (result (ref eq))
      (return_call $caml_ba_dim (local.get 0) (i31.new (i32.const 1))))

   (func (export "caml_ba_get_3")
      (param $vba (ref eq)) (param $vi (ref eq)) (param $vj (ref eq))
      (param $vk (ref eq))
      (result (ref eq))
      (local $ba (ref $bigarray))
      (local $i i32)
      (local $j i32)
      (local $k i32)
      (local $offset i32)
      (local $dim (ref $int_array))
      (local.set $ba (ref.cast $bigarray (local.get $vba)))
      (local.set $i (i31.get_u (ref.cast i31 (local.get $vi))))
      (local.set $j (i31.get_u (ref.cast i31 (local.get $vj))))
      (local.set $k (i31.get_u (ref.cast i31 (local.get $vk))))
      (local.set $dim (struct.get $bigarray $ba_dim (local.get $ba)))
      (if (struct.get $bigarray $ba_layout (local.get $ba))
         (then
            (local.set $i (i32.sub (local.get $i) (i32.const 1)))
            (local.set $j (i32.sub (local.get $j) (i32.const 1)))
            (local.set $k (i32.sub (local.get $k) (i32.const 1)))
            (local.set $offset
               (i32.add
                  (i32.mul
                     (i32.add
                        (i32.mul
                           (local.get $k)
                           (array.get $int_array (local.get $dim) (i32.const 1)))
                        (local.get $j))
                     (array.get $int_array (local.get $dim) (i32.const 0)))
                  (local.get $i))))
         (else
            (local.set $offset
               (i32.add
                  (i32.mul
                     (i32.add
                        (i32.mul
                           (local.get $i)
                           (array.get $int_array (local.get $dim) (i32.const 1)))
                     (local.get $j))
                     (array.get $int_array (local.get $dim) (i32.const 2)))
                  (local.get $k)))))
      (if (i32.or
             (i32.ge_u (local.get $i)
                (array.get $int_array (local.get $dim) (i32.const 0)))
             (i32.or
                (i32.ge_u (local.get $j)
                   (array.get $int_array (local.get $dim) (i32.const 1)))
                (i32.ge_u (local.get $j)
                   (array.get $int_array (local.get $dim) (i32.const 2)))))
         (then
            (call $caml_bound_error)))
      (return_call $caml_ba_get_at_offset (local.get $ba) (local.get $offset)))

   (func (export "caml_ba_set_3")
      (param $vba (ref eq)) (param $vi (ref eq)) (param $vj (ref eq))
      (param $vk (ref eq)) (param $v (ref eq))
      (result (ref eq))
      (local $ba (ref $bigarray))
      (local $i i32)
      (local $j i32)
      (local $k i32)
      (local $offset i32)
      (local $dim (ref $int_array))
      (local.set $ba (ref.cast $bigarray (local.get $vba)))
      (local.set $i (i31.get_u (ref.cast i31 (local.get $vi))))
      (local.set $j (i31.get_u (ref.cast i31 (local.get $vj))))
      (local.set $k (i31.get_u (ref.cast i31 (local.get $vk))))
      (local.set $dim (struct.get $bigarray $ba_dim (local.get $ba)))
      (if (struct.get $bigarray $ba_layout (local.get $ba))
         (then
            (local.set $i (i32.sub (local.get $i) (i32.const 1)))
            (local.set $j (i32.sub (local.get $j) (i32.const 1)))
            (local.set $k (i32.sub (local.get $k) (i32.const 1)))
            (local.set $offset
               (i32.add
                  (i32.mul
                     (i32.add
                        (i32.mul
                           (local.get $k)
                           (array.get $int_array (local.get $dim) (i32.const 1)))
                        (local.get $j))
                     (array.get $int_array (local.get $dim) (i32.const 0)))
                  (local.get $i))))
         (else
            (local.set $offset
               (i32.add
                  (i32.mul
                     (i32.add
                        (i32.mul
                           (local.get $i)
                           (array.get $int_array (local.get $dim) (i32.const 1)))
                     (local.get $j))
                     (array.get $int_array (local.get $dim) (i32.const 2)))
                  (local.get $k)))))
      (if (i32.or
             (i32.ge_u (local.get $i)
                (array.get $int_array (local.get $dim) (i32.const 0)))
             (i32.or
                (i32.ge_u (local.get $j)
                   (array.get $int_array (local.get $dim) (i32.const 1)))
                (i32.ge_u (local.get $k)
                   (array.get $int_array (local.get $dim) (i32.const 2)))))
         (then
            (call $caml_bound_error)))
      (call $caml_ba_set_at_offset
         (local.get $ba) (local.get $offset) (local.get $v))
      (i31.new (i32.const 0)))

   (func (export "caml_ba_dim_3") (param (ref eq)) (result (ref eq))
      (return_call $caml_ba_dim (local.get 0) (i31.new (i32.const 2))))

   (func $caml_ba_offset
      (param $b (ref $bigarray)) (param $index (ref $int_array)) (result i32)
      (local $dim (ref $int_array))
      (local $num_dims i32) (local $idx i32)
      (local $offset i32) (local $i i32) (local $l i32)
      (local.set $dim (struct.get $bigarray $ba_dim (local.get $b)))
      (if (struct.get $bigarray $ba_layout (local.get $b))
         (then
            (local.set $i
               (i32.sub (struct.get $bigarray $ba_num_dims (local.get $b))
                  (i32.const 1)))
            (loop $loop
               (if (i32.ge_s (local.get $i) (i32.const 0))
                  (then
                     (local.set $idx
                        (i32.sub
                           (array.get $int_array (local.get $index)
                              (local.get $i))
                           (i32.const 1)))
                     (local.set $l
                        (array.get $int_array (local.get $dim) (local.get $i)))
                     (if (i32.ge_u (local.get $idx) (local.get $l))
                        (then
                           (call $caml_bound_error)))
                     (local.set $offset
                        (i32.add (i32.mul (local.get $offset) (local.get $l))
                           (local.get $idx)))
                     (local.set $i (i32.sub (local.get $i) (i32.const 1)))
                     (br $loop)))))
         (else
            (local.set $num_dims
               (struct.get $bigarray $ba_num_dims (local.get $b)))
            (loop $loop
               (if (i32.lt_s (local.get $i) (local.get $num_dims))
                  (then
                     (local.set $idx
                        (array.get $int_array (local.get $index) (local.get $i)))
                     (local.set $l
                        (array.get $int_array (local.get $dim) (local.get $i)))
                     (if (i32.ge_u (local.get $idx) (local.get $l))
                        (then
                           (call $caml_bound_error)))
                     (local.set $offset
                        (i32.add (i32.mul (local.get $offset) (local.get $l))
                           (local.get $idx)))
                     (local.set $i (i32.add (local.get $i) (i32.const 1)))
                     (br $loop))))))
      (local.get $offset))

   (func $caml_ba_offset'
      (param $b (ref $bigarray)) (param $index (ref $block)) (result i32)
      (local $dim (ref $int_array))
      (local $num_dims i32) (local $idx i32)
      (local $offset i32) (local $i i32) (local $l i32)
      (local.set $dim (struct.get $bigarray $ba_dim (local.get $b)))
      (if (struct.get $bigarray $ba_layout (local.get $b))
         (then
            (local.set $i
               (i32.sub (struct.get $bigarray $ba_num_dims (local.get $b))
                  (i32.const 1)))
            (loop $loop
               (if (i32.ge_s (local.get $i) (i32.const 0))
                  (then
                     (local.set $idx
                        (i32.sub
                           (i31.get_s
                              (ref.cast i31
                                 (array.get $block (local.get $index)
                                    (i32.add (local.get $i) (i32.const 1)))))
                           (i32.const 1)))
                     (local.set $l
                        (array.get $int_array (local.get $dim) (local.get $i)))
                     (if (i32.ge_u (local.get $idx) (local.get $l))
                        (then
                           (call $caml_bound_error)))
                     (local.set $offset
                        (i32.add (i32.mul (local.get $offset) (local.get $l))
                           (local.get $idx)))
                     (local.set $i (i32.sub (local.get $i) (i32.const 1)))
                     (br $loop)))))
         (else
            (local.set $num_dims
               (struct.get $bigarray $ba_num_dims (local.get $b)))
            (loop $loop
               (if (i32.lt_s (local.get $i) (local.get $num_dims))
                  (then
                     (local.set $idx
                        (i31.get_s
                           (ref.cast i31
                              (array.get $block (local.get $index)
                                 (i32.add (local.get $i) (i32.const 1))))))
                     (local.set $l
                        (array.get $int_array (local.get $dim) (local.get $i)))
                     (if (i32.ge_u (local.get $idx) (local.get $l))
                        (then
                           (call $caml_bound_error)))
                     (local.set $offset
                        (i32.add (i32.mul (local.get $offset) (local.get $l))
                           (local.get $idx)))
                     (local.set $i (i32.add (local.get $i) (i32.const 1)))
                     (br $loop))))))
      (local.get $offset))

   (func (export "caml_ba_get_generic")
      (param $vba (ref eq)) (param $index (ref eq)) (result (ref eq))
      (local $ba (ref $bigarray))
      (local.set $ba (ref.cast $bigarray (local.get $vba)))
      (return_call $caml_ba_get_at_offset (local.get $ba)
         (call $caml_ba_offset' (local.get $ba)
            (ref.cast $block (local.get $index)))))

   (func (export "caml_ba_set_generic")
      (param $vba (ref eq)) (param $index (ref eq)) (param $v (ref eq))
      (result (ref eq))
      (local $ba (ref $bigarray))
      (local.set $ba (ref.cast $bigarray (local.get $vba)))
      (call $caml_ba_set_at_offset (local.get $ba)
         (call $caml_ba_offset' (local.get $ba)
            (ref.cast $block (local.get $index)))
         (local.get $v))
      (i31.new (i32.const 0)))

   (data $too_many_indices "Bigarray.slice: too many indices")

   (func (export "caml_ba_slice")
      (param $vb (ref eq)) (param $vind (ref eq)) (result (ref eq))
      (local $b (ref $bigarray))
      (local $ind (ref $block))
      (local $index (ref $int_array)) (local $sub_dim (ref $int_array))
      (local $num_inds i32) (local $num_dims i32) (local $i i32)
      (local $idx i32) (local $mul i32) (local $offset i32) (local $size i32)
      (local $sub_data (ref extern))
      (local.set $b (ref.cast $bigarray (local.get $vb)))
      (local.set $ind (ref.cast $block (local.get $vind)))
      (local.set $num_inds (i32.sub (array.len (local.get $ind)) (i32.const 1)))
      (local.set $num_dims (struct.get $bigarray $ba_num_dims (local.get $b)))
      (if (i32.gt_u (local.get $num_inds)
             (struct.get $bigarray $ba_num_dims (local.get $b)))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $too_many_indices
                  (i32.const 0) (i32.const 32)))))
      (local.set $sub_dim
         (array.new $int_array (i32.const 0)
            (i32.sub (local.get $num_dims) (local.get $num_inds))))
      (if (struct.get $bigarray $ba_layout (local.get $b))
         (then
            (local.set $index
               (array.new $int_array (i32.const 1) (local.get $num_dims)))
            (loop $loop
               (if (i32.lt_u (local.get $i) (local.get $num_inds))
                  (then
                     (array.set $int_array (local.get $index)
                        (i32.sub (i32.add (local.get $num_dims) (local.get $i))
                           (local.get $num_inds))
                        (i31.get_u
                           (ref.cast i31
                              (array.get $block (local.get $ind)
                                 (i32.add (local.get $i) (i32.const 1))))))
                     (local.set $i (i32.add (local.get $i) (i32.const 1)))
                     (br $loop))))
            (local.set $offset
               (call $caml_ba_offset (local.get $b) (local.get $index)))
            (array.copy $int_array $int_array
               (local.get $sub_dim) (i32.const 0)
               (struct.get $bigarray $ba_dim (local.get $b)) (i32.const 0)
               (i32.sub (local.get $num_dims) (local.get $num_inds))))
         (else
            (local.set $index
               (array.new $int_array (i32.const 0) (local.get $num_dims)))
            (loop $loop
               (if (i32.lt_u (local.get $i) (local.get $num_inds))
                  (then
                     (array.set $int_array (local.get $index)
                        (local.get $i)
                        (i31.get_u
                           (ref.cast i31
                              (array.get $block (local.get $ind)
                                 (i32.add (local.get $i) (i32.const 1))))))
                     (local.set $i (i32.add (local.get $i) (i32.const 1)))
                     (br $loop))))
            (local.set $offset
               (call $caml_ba_offset (local.get $b) (local.get $index)))
            (array.copy $int_array $int_array
               (local.get $sub_dim) (i32.const 0)
               (struct.get $bigarray $ba_dim (local.get $b))
               (local.get $num_inds)
               (i32.sub (local.get $num_dims) (local.get $num_inds)))))
      (local.set $mul
         (call $caml_ba_size_per_element
            (struct.get $bigarray $ba_kind (local.get $b))))
      (local.set $size (call $caml_ba_get_size (local.get $sub_dim)))
      (local.set $sub_data
         (call $ta_subarray (struct.get $bigarray $ba_data (local.get $b))
            (i32.mul (local.get $offset) (local.get $mul))
            (i32.mul (i32.add (local.get $offset) (local.get $size))
               (local.get $mul))))
      (struct.new $bigarray
         (global.get $bigarray_ops)
         (local.get $sub_data)
         (local.get $sub_dim)
         (array.len (local.get $sub_dim))
         (struct.get $bigarray $ba_kind (local.get $b))
         (struct.get $bigarray $ba_layout (local.get $b))))

   (data $bad_subarray "Bigarray.sub: bad sub-array")

   (func (export "caml_ba_sub")
      (param $vba (ref eq)) (param $vofs (ref eq)) (param $vlen (ref eq))
      (result (ref eq))
      (local $ba (ref $bigarray))
      (local $ofs i32) (local $len i32)
      (local $changed_dim i32) (local $mul i32) (local $i i32)
      (local $num_dims i32)
      (local $dim (ref $int_array)) (local $new_dim (ref $int_array))
      (local $new_data (ref extern))
      (local.set $ba (ref.cast $bigarray (local.get $vba)))
      (local.set $ofs (i31.get_s (ref.cast i31 (local.get $vofs))))
      (local.set $len (i31.get_s (ref.cast i31 (local.get $vlen))))
      (local.set $num_dims (struct.get $bigarray $ba_num_dims (local.get $ba)))
      (local.set $dim (struct.get $bigarray $ba_dim (local.get $ba)))
      (local.set $mul (i32.const 1))
      (if (struct.get $bigarray $ba_layout (local.get $ba))
         (then
            (local.set $changed_dim
               (i32.sub (local.get $num_dims) (i32.const 1)))
            (local.set $ofs (i32.sub (local.get $ofs) (i32.const 1)))
            (local.set $i (i32.const 0))
            (loop $loop
               (if (i32.lt_u (local.get $i) (local.get $changed_dim))
                  (then
                     (local.set $mul
                        (i32.mul (local.get $mul)
                           (array.get $int_array
                              (local.get $dim) (local.get $i))))
                     (local.set $i (i32.add (local.get $i) (i32.const 1)))
                     (br $loop)))))
         (else
            (local.set $changed_dim (i32.const 0))
            (local.set $i (i32.const 1))
            (loop $loop
               (if (i32.lt_u (local.get $i) (local.get $num_dims))
                  (then
                     (local.set $mul
                        (i32.mul (local.get $mul)
                           (array.get $int_array
                              (local.get $dim) (local.get $i))))
                     (local.set $i (i32.add (local.get $i) (i32.const 1)))
                     (br $loop))))))
      (if (i32.or
             (i32.or (i32.lt_s (local.get $ofs) (i32.const 0))
                (i32.lt_s (local.get $len) (i32.const 0)))
             (i32.gt_s (i32.add (local.get $ofs) (local.get $len))
                (array.get $int_array (local.get $dim)
                   (local.get $changed_dim))))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $bad_subarray
                  (i32.const 0) (i32.const 27)))))
      (local.set $new_dim
         (array.new $int_array (i32.const 0) (local.get $num_dims)))
      (array.copy $int_array $int_array
         (local.get $new_dim) (i32.const 0)
         (local.get $dim) (i32.const 0)
         (local.get $num_dims))
      (array.set $int_array (local.get $new_dim) (local.get $changed_dim)
         (local.get $len))
      (local.set $mul (i32.mul (local.get $mul)
         (call $caml_ba_size_per_element
           (struct.get $bigarray $ba_kind (local.get $ba)))))
      (local.set $new_data
         (call $ta_subarray (struct.get $bigarray $ba_data (local.get $ba))
            (i32.mul (local.get $ofs) (local.get $mul))
            (i32.mul (i32.add (local.get $ofs) (local.get $len))
               (local.get $mul))))
      (struct.new $bigarray
         (global.get $bigarray_ops)
         (local.get $new_data)
         (local.get $new_dim)
         (local.get $num_dims)
         (struct.get $bigarray $ba_kind (local.get $ba))
         (struct.get $bigarray $ba_layout (local.get $ba))))

   (func (export "caml_ba_fill")
      (param $vba (ref eq)) (param $v (ref eq)) (result (ref eq))
      (local $ba (ref $bigarray))
      (local $data (ref extern))
      (local $l i64)
      (local $i i32) (local $len i32) (local $i1 i32) (local $i2 i32)
      (local $f1 f64) (local $f2 f64)
      (local $b (ref $block))
      (local.set $ba (ref.cast $bigarray (local.get $vba)))
      (local.set $data (struct.get $bigarray $ba_data (local.get $ba)))
      (block $float
       (block $int
        (block $int32
         (block $int64
          (block $complex32
           (block $complex64
            (br_table $float $float $int $int $int $int $int32 $int64 $int
               $int32 $complex32 $complex64 $int
               (struct.get $bigarray $ba_kind (local.get $ba))))
            ;; complex64
            (local.set $len (call $ta_length (local.get $data)))
            (local.set $b (ref.cast $block (local.get $v)))
            (local.set $f1
               (struct.get $float 0
                  (ref.cast $float
                     (array.get $block (local.get $b) (i32.const 1)))))
            (local.set $f2
               (struct.get $float 0
                  (ref.cast $float
                     (array.get $block (local.get $b) (i32.const 2)))))
            (loop $loop
               (if (i32.lt_u (local.get $i) (local.get $len))
                  (then
                     (call $ta_set_f64 (local.get $data) (local.get $i)
                        (local.get $f1))
                     (call $ta_set_f64 (local.get $data)
                        (i32.add (local.get $i) (i32.const 1))
                        (local.get $f2))
                     (local.set $i (i32.add (local.get $i) (i32.const 2)))
                     (br $loop))))
            (return (i31.new (i32.const 0))))
           ;; complex32
           (local.set $len (call $ta_length (local.get $data)))
           (local.set $b (ref.cast $block (local.get $v)))
           (local.set $f1
              (struct.get $float 0
                 (ref.cast $float
                    (array.get $block (local.get $b) (i32.const 1)))))
           (local.set $f2
              (struct.get $float 0
                 (ref.cast $float
                    (array.get $block (local.get $b) (i32.const 2)))))
           (loop $loop
              (if (i32.lt_u (local.get $i) (local.get $len))
                 (then
                    (call $ta_set_f32 (local.get $data) (local.get $i)
                       (local.get $f1))
                    (call $ta_set_f32 (local.get $data)
                       (i32.add (local.get $i) (i32.const 1))
                       (local.get $f2))
                    (local.set $i (i32.add (local.get $i) (i32.const 2)))
                    (br $loop))))
           (return (i31.new (i32.const 0))))
          ;; int64
          (local.set $len (call $ta_length (local.get $data)))
          (local.set $l
             (struct.get $int64 1 (ref.cast $int64 (local.get $v))))
          (local.set $i1 (i32.wrap_i64 (local.get $l)))
          (local.set $i2
             (i32.wrap_i64 (i64.shr_u (local.get $l) (i64.const 32))))
          (loop $loop
             (if (i32.lt_u (local.get $i) (local.get $len))
                (then
                   (call $ta_set_i32 (local.get $data) (local.get $i)
                      (local.get $i1))
                   (call $ta_set_i32 (local.get $data)
                      (i32.add (local.get $i) (i32.const 1))
                      (local.get $i2))
                   (local.set $i (i32.add (local.get $i) (i32.const 2)))
                   (br $loop))))
          (return (i31.new (i32.const 0))))
         ;; int32
         (call $ta_fill_int (local.get $data)
            (struct.get $int32 1 (ref.cast $int32 (local.get $v))))
         (return (i31.new (i32.const 0))))
        ;; int
        (call $ta_fill_int (local.get $data)
           (i31.get_s (ref.cast i31 (local.get $v))))
        (return (i31.new (i32.const 0))))
       ;; float
       (call $ta_fill_float (local.get $data)
          (struct.get $float 0 (ref.cast $float (local.get $v))))
       (return (i31.new (i32.const 0))))

   (data $dim_mismatch "Bigarray.blit: dimension mismatch")

   (func (export "caml_ba_blit")
      (param $vsrc (ref eq)) (param $vdst (ref eq)) (result (ref eq))
      (local $src (ref $bigarray))
      (local $dst (ref $bigarray))
      (local $sdim (ref $int_array))
      (local $ddim (ref $int_array))
      (local $i i32) (local $len i32)
      (local.set $src (ref.cast $bigarray (local.get $vsrc)))
      (local.set $dst (ref.cast $bigarray (local.get $vdst)))
      (local.set $len (struct.get $bigarray $ba_num_dims (local.get $dst)))
      (if (i32.ne (local.get $len)
             (struct.get $bigarray $ba_num_dims (local.get $src)))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $dim_mismatch
                  (i32.const 0) (i32.const 33)))))
      (local.set $sdim (struct.get $bigarray $ba_dim (local.get $src)))
      (local.set $ddim (struct.get $bigarray $ba_dim (local.get $dst)))
      (loop $loop
         (if (i32.lt_s (local.get $i) (local.get $len))
            (if (i32.ne (array.get $int_array (local.get $sdim) (local.get $i))
                        (array.get $int_array (local.get $ddim) (local.get $i)))
               (then
                  (call $caml_invalid_argument
                     (array.new_data $string $dim_mismatch
                        (i32.const 0) (i32.const 33)))))
            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (br $loop)))
      (call $ta_blit
         (struct.get $bigarray $ba_data (local.get $src))
         (struct.get $bigarray $ba_data (local.get $dst)))
     (i31.new (i32.const 0)))

   (data $bad_number_dim "Bigarray.reshape: bad number of dimensions")
   (data $negative_dim "Bigarray.reshape: negative dimension")
   (data $size_mismatch "Bigarray.reshape: size mismatch")

   (func (export "caml_ba_reshape")
      (param $vb (ref eq)) (param $vd (ref eq)) (result (ref eq))
      (local $vdim (ref $block))
      (local $num_dims i32) (local $num_elts i64) (local $i i32) (local $d i32)
      (local $b (ref $bigarray))
      (local $dim (ref $int_array))
      (local.set $vdim (ref.cast $block (local.get $vd)))
      (local.set $num_dims (i32.sub (array.len (local.get $vdim)) (i32.const 1)))
      (local.set $b (ref.cast $bigarray (local.get $vb)))
      (if (i32.gt_u (local.get $num_dims) (global.get $CAML_BA_MAX_NUM_DIMS))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $bad_number_dim
                  (i32.const 0) (i32.const 42)))))
      (local.set $num_elts (i64.const 1))
      (local.set $dim (array.new $int_array (i32.const 0) (local.get $num_dims)))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $num_dims))
            (then
               (local.set $d
                  (i31.get_s
                     (ref.cast i31
                        (array.get $block (local.get $vdim)
                           (i32.add (local.get $i) (i32.const 1))))))
               (if (i32.lt_s (local.get $d) (i32.const 0))
                  (then
                     (call $caml_invalid_argument
                        (array.new_data $string $negative_dim
                           (i32.const 0) (i32.const 36)))))
               (array.set $int_array (local.get $dim) (local.get $i)
                  (local.get $d))
               (local.set $num_elts
                  (i64.mul (local.get $num_elts)
                     (i64.extend_i32_s (local.get $d))))
               (if (i64.ne (local.get $num_elts)
                      (i64.extend_i32_s (i32.wrap_i64 (local.get $num_elts))))
                  (then (call $caml_raise_out_of_memory)))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (if (i32.ne (i32.wrap_i64 (local.get $num_elts))
             (call $caml_ba_get_size
                (struct.get $bigarray $ba_dim (local.get $b))))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $size_mismatch
                  (i32.const 0) (i32.const 31)))))
      (struct.new $bigarray
         (global.get $bigarray_ops)
         (struct.get $bigarray $ba_data (local.get $b))
         (local.get $dim)
         (local.get $num_dims)
         (struct.get $bigarray $ba_kind (local.get $b))
         (struct.get $bigarray $ba_layout (local.get $b))))

   (func (export "caml_ba_change_layout")
      (param $vb (ref eq)) (param $vlayout (ref eq)) (result (ref eq))
      (local $b (ref $bigarray))
      (local $layout i32) (local $num_dims i32) (local $i i32)
      (local $dim (ref $int_array)) (local $new_dim (ref $int_array))
      (local.set $b (ref.cast $bigarray (local.get $vb)))
      (local.set $layout (i31.get_s (ref.cast i31 (local.get $vlayout))))
      (if (result (ref eq))
          (i32.ne (struct.get $bigarray $ba_layout (local.get $b))
             (local.get $layout))
         (then
            (local.set $num_dims
               (struct.get $bigarray $ba_num_dims (local.get $b)))
            (local.set $dim
               (struct.get $bigarray $ba_dim (local.get $b)))
            (local.set $new_dim
               (array.new $int_array (i32.const 0) (local.get $num_dims)))
            (loop $loop
               (if (i32.lt_u (local.get $i) (local.get $num_dims))
                  (then
                     (array.set $int_array (local.get $new_dim) (local.get $i)
                        (array.get $int_array (local.get $dim)
                           (i32.sub
                              (i32.sub (local.get $num_dims) (local.get $i))
                              (i32.const 1))))
                     (local.set $i (i32.add (local.get $i) (i32.const 1)))
                     (br $loop))))
            (struct.new $bigarray
               (global.get $bigarray_ops)
               (struct.get $bigarray $ba_data (local.get $b))
               (local.get $new_dim)
               (local.get $num_dims)
               (struct.get $bigarray $ba_kind (local.get $b))
               (local.get $layout)))
         (else
            (local.get $vb))))

   (func (export "caml_ba_num_dims") (param (ref eq)) (result (ref eq))
      (i31.new
         (struct.get $bigarray $ba_num_dims (ref.cast $bigarray (local.get 0)))))

   (func (export "caml_ba_kind") (param (ref eq)) (result (ref eq))
      (i31.new
         (struct.get $bigarray $ba_kind (ref.cast $bigarray (local.get 0)))))

   (func (export "caml_ba_layout") (param (ref eq)) (result (ref eq))
      (i31.new
         (struct.get $bigarray $ba_layout (ref.cast $bigarray (local.get 0)))))

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

   (func (export "caml_ba_uint8_get16")
      (param $vba (ref eq)) (param $i (ref eq)) (result (ref eq))
      (local $ba (ref $bigarray))
      (local $data (ref extern))
      (local $p i32)
      (local.set $ba (ref.cast $bigarray (local.get $vba)))
      (local.set $data (struct.get $bigarray $ba_data (local.get $ba)))
      (local.set $p (i31.get_s (ref.cast i31 (local.get $i))))
      (if (i32.lt_s (local.get $p) (i32.const 0))
         (then (call $caml_bound_error)))
      (if (i32.ge_u (i32.add (local.get $p) (i32.const 1))
             (array.get $int_array
                (struct.get $bigarray $ba_dim (local.get $ba))
                (i32.const 0)))
         (then (call $caml_bound_error)))
      (i31.new (i32.or
                  (call $ta_get_ui8 (local.get $data) (local.get $p))
                  (i32.shl (call $ta_get_ui8 (local.get $data)
                              (i32.add (local.get $p) (i32.const 1)))
                           (i32.const 8)))))

   (func (export "caml_ba_uint8_get32")
      (param $vba (ref eq)) (param $i (ref eq)) (result (ref eq))
      (local $ba (ref $bigarray))
      (local $data (ref extern))
      (local $p i32)
      (local.set $ba (ref.cast $bigarray (local.get $vba)))
      (local.set $data (struct.get $bigarray $ba_data (local.get $ba)))
      (local.set $p (i31.get_s (ref.cast i31 (local.get $i))))
      (if (i32.lt_s (local.get $p) (i32.const 0))
         (then (call $caml_bound_error)))
      (if (i32.ge_u (i32.add (local.get $p) (i32.const 3))
             (array.get $int_array
                (struct.get $bigarray $ba_dim (local.get $ba))
                (i32.const 0)))
         (then (call $caml_bound_error)))
      (return_call $caml_copy_int32
         (i32.or
            (i32.or
               (call $ta_get_ui8 (local.get $data) (local.get $p))
               (i32.shl (call $ta_get_ui8 (local.get $data)
                           (i32.add (local.get $p) (i32.const 1)))
                        (i32.const 8)))
            (i32.or
               (i32.shl (call $ta_get_ui8 (local.get $data)
                           (i32.add (local.get $p) (i32.const 2)))
                        (i32.const 16))
               (i32.shl (call $ta_get_ui8 (local.get $data)
                           (i32.add (local.get $p) (i32.const 3)))
                        (i32.const 24))))))

   (func (export "caml_ba_uint8_get64")
      (param $vba (ref eq)) (param $i (ref eq)) (result (ref eq))
      (local $ba (ref $bigarray))
      (local $data (ref extern))
      (local $p i32)
      (local.set $ba (ref.cast $bigarray (local.get $vba)))
      (local.set $data (struct.get $bigarray $ba_data (local.get $ba)))
      (local.set $p (i31.get_s (ref.cast i31 (local.get $i))))
      (if (i32.lt_s (local.get $p) (i32.const 0))
         (then (call $caml_bound_error)))
      (if (i32.ge_u (i32.add (local.get $p) (i32.const 7))
             (array.get $int_array
                (struct.get $bigarray $ba_dim (local.get $ba))
                (i32.const 0)))
         (then (call $caml_bound_error)))
      (return_call $caml_copy_int64
         (i64.or
            (i64.or
               (i64.or
                  (i64.extend_i32_u
                     (call $ta_get_ui8 (local.get $data) (local.get $p)))
                  (i64.shl (i64.extend_i32_u
                              (call $ta_get_ui8 (local.get $data)
                                 (i32.add (local.get $p) (i32.const 1))))
                           (i64.const 8)))
               (i64.or
                  (i64.shl (i64.extend_i32_u
                              (call $ta_get_ui8 (local.get $data)
                                 (i32.add (local.get $p) (i32.const 2))))
                           (i64.const 16))
                  (i64.shl (i64.extend_i32_u
                              (call $ta_get_ui8 (local.get $data)
                                 (i32.add (local.get $p) (i32.const 3))))
                           (i64.const 24))))
            (i64.or
               (i64.or
                  (i64.shl (i64.extend_i32_u
                              (call $ta_get_ui8 (local.get $data)
                                 (i32.add (local.get $p) (i32.const 4))))
                           (i64.const 32))
                  (i64.shl (i64.extend_i32_u
                              (call $ta_get_ui8 (local.get $data)
                                 (i32.add (local.get $p) (i32.const 5))))
                           (i64.const 40)))
               (i64.or
                  (i64.shl (i64.extend_i32_u
                              (call $ta_get_ui8 (local.get $data)
                                 (i32.add (local.get $p) (i32.const 6))))
                           (i64.const 48))
                  (i64.shl (i64.extend_i32_u
                              (call $ta_get_ui8 (local.get $data)
                                 (i32.add (local.get $p) (i32.const 7))))
                           (i64.const 56)))))))

   (func (export "caml_ba_uint8_set16")
      (param $vba (ref eq)) (param $i (ref eq)) (param $v (ref eq))
      (result (ref eq))
      (local $ba (ref $bigarray))
      (local $data (ref extern))
      (local $p i32) (local $d (ref i31))
      (local.set $ba (ref.cast $bigarray (local.get $vba)))
      (local.set $data (struct.get $bigarray $ba_data (local.get $ba)))
      (local.set $p (i31.get_s (ref.cast i31 (local.get $i))))
      (local.set $d (ref.cast i31 (local.get $v)))
      (if (i32.lt_s (local.get $p) (i32.const 0))
         (then (call $caml_bound_error)))
      (if (i32.ge_u (i32.add (local.get $p) (i32.const 1))
             (array.get $int_array
                (struct.get $bigarray $ba_dim (local.get $ba))
                (i32.const 0)))
         (then (call $caml_bound_error)))
      (call $ta_set_ui8 (local.get $data) (local.get $p) (local.get $d))
      (call $ta_set_ui8 (local.get $data)
         (i32.add (local.get $p) (i32.const 1))
         (i31.new (i32.shr_u (i31.get_s (local.get $d)) (i32.const 8))))
      (i31.new (i32.const 0)))

   (func (export "caml_ba_uint8_set32")
      (param $vba (ref eq)) (param $i (ref eq)) (param $v (ref eq))
      (result (ref eq))
      (local $ba (ref $bigarray))
      (local $data (ref extern))
      (local $p i32) (local $d i32)
      (local.set $ba (ref.cast $bigarray (local.get $vba)))
      (local.set $data (struct.get $bigarray $ba_data (local.get $ba)))
      (local.set $p (i31.get_s (ref.cast i31 (local.get $i))))
      (local.set $d (struct.get $int32 1 (ref.cast $int32 (local.get $v))))
      (if (i32.lt_s (local.get $p) (i32.const 0))
         (then (call $caml_bound_error)))
      (if (i32.ge_u (i32.add (local.get $p) (i32.const 3))
             (array.get $int_array
                (struct.get $bigarray $ba_dim (local.get $ba))
                (i32.const 0)))
         (then (call $caml_bound_error)))
      (call $ta_set_ui8 (local.get $data) (local.get $p)
         (i31.new (local.get $d)))
      (call $ta_set_ui8 (local.get $data)
         (i32.add (local.get $p) (i32.const 1))
         (i31.new (i32.shr_u (local.get $d) (i32.const 8))))
      (call $ta_set_ui8 (local.get $data)
         (i32.add (local.get $p) (i32.const 2))
         (i31.new (i32.shr_u (local.get $d) (i32.const 16))))
      (call $ta_set_ui8 (local.get $data)
         (i32.add (local.get $p) (i32.const 3))
         (i31.new (i32.shr_u (local.get $d) (i32.const 24))))
      (i31.new (i32.const 0)))

   (func (export "caml_ba_uint8_set64")
      (param $vba (ref eq)) (param $i (ref eq)) (param $v (ref eq))
      (result (ref eq))
      (local $ba (ref $bigarray))
      (local $data (ref extern))
      (local $p i32) (local $d i64)
      (local.set $ba (ref.cast $bigarray (local.get $vba)))
      (local.set $data (struct.get $bigarray $ba_data (local.get $ba)))
      (local.set $p (i31.get_s (ref.cast i31 (local.get $i))))
      (local.set $d (struct.get $int64 1 (ref.cast $int64 (local.get $v))))
      (if (i32.lt_s (local.get $p) (i32.const 0))
         (then (call $caml_bound_error)))
      (if (i32.ge_u (i32.add (local.get $p) (i32.const 7))
             (array.get $int_array
                (struct.get $bigarray $ba_dim (local.get $ba))
                (i32.const 0)))
         (then (call $caml_bound_error)))
      (call $ta_set_ui8 (local.get $data) (local.get $p)
         (i31.new (i32.wrap_i64 (local.get $d))))
      (call $ta_set_ui8 (local.get $data)
         (i32.add (local.get $p) (i32.const 1))
         (i31.new (i32.wrap_i64 (i64.shr_u (local.get $d) (i64.const 8)))))
      (call $ta_set_ui8 (local.get $data)
         (i32.add (local.get $p) (i32.const 2))
         (i31.new (i32.wrap_i64 (i64.shr_u (local.get $d) (i64.const 16)))))
      (call $ta_set_ui8 (local.get $data)
         (i32.add (local.get $p) (i32.const 3))
         (i31.new (i32.wrap_i64 (i64.shr_u (local.get $d) (i64.const 24)))))
      (call $ta_set_ui8 (local.get $data)
         (i32.add (local.get $p) (i32.const 4))
         (i31.new (i32.wrap_i64 (i64.shr_u (local.get $d) (i64.const 32)))))
      (call $ta_set_ui8 (local.get $data)
         (i32.add (local.get $p) (i32.const 5))
         (i31.new (i32.wrap_i64 (i64.shr_u (local.get $d) (i64.const 40)))))
      (call $ta_set_ui8 (local.get $data)
         (i32.add (local.get $p) (i32.const 6))
         (i31.new (i32.wrap_i64 (i64.shr_u (local.get $d) (i64.const 48)))))
      (call $ta_set_ui8 (local.get $data)
         (i32.add (local.get $p) (i32.const 7))
         (i31.new (i32.wrap_i64 (i64.shr_u (local.get $d) (i64.const 56)))))
      (i31.new (i32.const 0)))

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

   (func (export "caml_ba_get_kind") (param (ref eq)) (result i32)
      (struct.get $bigarray $ba_kind (ref.cast $bigarray (local.get 0))))

   (func (export "caml_ba_get_layout") (param (ref eq)) (result i32)
      (struct.get $bigarray $ba_layout (ref.cast $bigarray (local.get 0))))

   (func (export "caml_ba_get_data") (param (ref eq)) (result (ref extern))
      (struct.get $bigarray $ba_data (ref.cast $bigarray (local.get 0))))

   (func (export "caml_ba_set_data") (param (ref eq)) (param (ref extern))
      (struct.set $bigarray $ba_data (ref.cast $bigarray (local.get 0))
         (local.get $1)))

   (func (export "caml_ba_get_dim") (param (ref eq)) (result (ref $int_array))
      (struct.get $bigarray $ba_dim (ref.cast $bigarray (local.get 0))))

   (func (export "caml_ba_alloc")
      (param $kind i32) (param $layout i32) (param $num_dims i32)
      (param $data (ref extern)) (param $dim (ref $int_array))
      (result (ref eq))
      (struct.new $bigarray
         (global.get $bigarray_ops)
         (local.get $data)
         (local.get $dim)
         (local.get $num_dims)
         (local.get $kind)
         (local.get $layout)))
)
