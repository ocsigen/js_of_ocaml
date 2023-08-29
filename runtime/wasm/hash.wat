(module
   (import "obj" "object_tag" (global $object_tag i32))
   (import "obj" "forward_tag" (global $forward_tag i32))
   (import "jslib" "unwrap" (func $unwrap (param (ref eq)) (result anyref)))

   (type $block (array (mut (ref eq))))
   (type $string (array (mut i8)))
   (type $float (struct (field f64)))
   (type $js (struct (field anyref)))
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

   (func $caml_hash_mix_int (export "caml_hash_mix_int")
      (param $h i32) (param $d i32) (result i32)
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

   (func $caml_hash_mix_final (export "caml_hash_mix_final")
      (param $h i32) (result i32)
      (local.set $h
         (i32.xor (local.get $h) (i32.shr_u (local.get $h) (i32.const 16))))
      (local.set $h (i32.mul (local.get $h) (i32.const 0x85ebca6b)))
      (local.set $h
         (i32.xor (local.get $h) (i32.shr_u (local.get $h) (i32.const 13))))
      (local.set $h (i32.mul (local.get $h) (i32.const 0xc2b2ae35)))
      (i32.xor (local.get $h) (i32.shr_u (local.get $h) (i32.const 16))))

   (func $caml_hash_mix_int64 (export "caml_hash_mix_int64")
      (param $h i32) (param $d i64) (result i32)
      (return_call $caml_hash_mix_int
         (call $caml_hash_mix_int (local.get $h) (i32.wrap_i64 (local.get $d)))
         (i32.wrap_i64 (i64.shr_u (local.get $d) (i64.const 32)))))

   (func $caml_hash_mix_float (export "caml_hash_mix_float")
      (param $h i32) (param $d f64) (result i32)
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

   (func $caml_hash_mix_float32 (export "caml_hash_mix_float32")
      (param $h i32) (param $d f32) (result i32)
      (local $i i32)
      (local.set $i (i32.reinterpret_f32 (local.get $d)))
      (if (i32.eq (i32.and (local.get $i) (i32.const 0x7F800000))
                  (i32.const 0x7F800000))
         (then
            (if (i32.ne (i32.and (local.get $i) (i32.const 0x7FFFFF))
                        (i32.const 0))
               (then (local.set $i (i32.const 0x7F800001))))))
      (if (i32.eq (local.get $i) (i32.const 0x80000000))
         (then (local.set $i (i32.const 0))))
      (return_call $caml_hash_mix_int (local.get $h) (local.get $i)))

   (func $caml_hash_mix_string (export "caml_hash_mix_string")
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

   (func $caml_hash_mix_jsstring
      (param $h i32) (param $s (ref eq)) (result i32)
      (return_call $caml_hash_mix_int (local.get $h)
         (string.hash (ref.cast (ref string) (call $unwrap (local.get $s))))))

   (global $HASH_QUEUE_SIZE i32 (i32.const 256))
   (global $MAX_FORWARD_DEREFERENCE i32 (i32.const 1000))

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
      (local $str anyref)
      (local.set $sz (i31.get_u (ref.cast (ref i31) (local.get $limit))))
      (if (i32.gt_u (local.get $sz) (global.get $HASH_QUEUE_SIZE))
         (then (local.set $sz (global.get $HASH_QUEUE_SIZE))))
      (local.set $num (i31.get_u (ref.cast (ref i31) (local.get $count))))
      (local.set $h (i31.get_s (ref.cast (ref i31) (local.get $seed))))
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
                           (i32.add
                              (i32.shl
                                 (i31.get_s
                                    (br_on_cast_fail
                                       $not_int (ref eq) (ref i31)
                                       (local.get $v)))
                                 (i32.const 1))
                              (i32.const 1))))
                     (local.set $num (i32.sub (local.get $num) (i32.const 1)))
                     (br $loop)))
                  (drop (block $not_string (result (ref eq))
                     (local.set $h
                        (call $caml_hash_mix_string (local.get $h)
                           (br_on_cast_fail $not_string (ref eq) (ref $string)
                              (local.get $v))))
                     (local.set $num (i32.sub (local.get $num) (i32.const 1)))
                     (br $loop)))
                  (drop (block $not_block (result (ref eq))
                     (local.set $b
                        (br_on_cast_fail $not_block (ref eq) (ref $block)
                           (local.get $v)))
                     (local.set $tag
                        (i31.get_u
                           (ref.cast (ref i31)
                              (array.get $block (local.get $b) (i32.const 0)))))
                     (if (i32.eq (local.get $tag) (global.get $forward_tag))
                        (then
                           (local.set $i (i32.const 0))
                           (loop $forward
                              (local.set $v
                                 (array.get $block
                                    (local.get $b) (i32.const 1)))
                              (drop (block $not_block' (result (ref eq))
                                 (local.set $b
                                    (br_on_cast_fail
                                       $not_block' (ref eq) (ref $block)
                                       (local.get $v)))
                                 (br_if $again
                                    (i32.eqz
                                       (ref.eq
                                          (array.get $block (local.get $b)
                                             (i32.const 0))
                                          (i31.new (global.get $forward_tag)))))
                                 (local.set $i
                                    (i32.add (local.get $i) (i32.const 1)))
                                 (br_if $loop
                                    (i32.eq
                                       (local.get $i)
                                       (global.get $MAX_FORWARD_DEREFERENCE)))
                                 (br $forward)))
                              (br $again))))
                     (if (i32.eq (local.get $tag) (global.get $object_tag))
                        (then
                           (local.set $h
                              (call $caml_hash_mix_int (local.get $h)
                                 (i31.get_s
                                    (ref.cast (ref i31)
                                       (array.get $block
                                          (local.get $b) (i32.const 2))))))
                           (br $loop)))
                     (local.set $len (array.len (local.get $b)))
                     (local.set $h
                        (call $caml_hash_mix_int (local.get $h)
                           (i32.or
                              (i32.shl (i32.sub (local.get $len) (i32.const 1))
                                 (i32.const 10))
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
                              (br_on_cast_fail $not_float (ref eq) (ref $float)
                                 (local.get $v)))))
                     (local.set $num (i32.sub (local.get $num) (i32.const 1)))
                     (br $loop)))
                  (drop (block $not_custom (result (ref eq))
                     (local.set $h
                        (call $caml_hash_mix_int (local.get $h)
                           (call_ref $value->int
                              (local.get $v)
                              (br_on_null $loop
                                 (struct.get $custom_operations $cust_hash
                                    (struct.get $custom 0
                                       (br_on_cast_fail $not_custom
                                          (ref eq) (ref $custom)
                                          (local.get $v))))))))
                     (local.set $num (i32.sub (local.get $num) (i32.const 1)))
                     (br $loop)))
                  (drop (block $not_jsstring anyref
                     (local.set $str
                        (struct.get $js 0
                           (br_on_cast_fail $not_jsstring (ref eq) (ref $js)
                              (local.get $v))))
                     (local.set $h
                        (call $caml_hash_mix_int (local.get $h)
                           (string.hash
                              (br_on_cast_fail $not_jsstring anyref (ref string)
                                 (local.get $str)))))
                     (i31.new (i32.const 0))))
                  ;; closures and continuations and other js values are ignored
                  (br $loop)))))
      ;; clear the queue to avoid a memory leak
      (array.fill $block (global.get $caml_hash_queue)
         (i32.const 0) (i31.new (i32.const 0)) (local.get $wr))
      (i31.new (i32.and (call $caml_hash_mix_final (local.get $h))
                        (i32.const 0x3FFFFFFF))))

   (func (export "caml_string_hash")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (local $h i32)
      (i31.new
         (i32.and
            (call $caml_hash_mix_final
               (call $caml_hash_mix_string
                  (i31.get_s (ref.cast (ref i31) (local.get 0)))
                  (ref.cast (ref $string) (local.get 1))))
            (i32.const 0x3FFFFFFF))))
)
