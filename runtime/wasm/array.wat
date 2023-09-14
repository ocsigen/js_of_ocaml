(module
   (import "fail" "caml_invalid_argument"
      (func $caml_invalid_argument (param (ref eq))))

   (type $block (array (mut (ref eq))))
   (type $string (array (mut i8)))
   (type $float (struct (field f64)))
   (type $float_array (array (mut f64)))

   (data $Array_make "Array.make")

   (global $empty_array (ref eq)
      (array.new_fixed $block 1 (ref.i31 (i32.const 0))))

   (func $caml_make_vect (export "caml_make_vect")
      (param $n (ref eq)) (param $v (ref eq)) (result (ref eq))
      (local $sz i32) (local $b (ref $block)) (local $f f64)
      (local.set $sz (i31.get_s (ref.cast (ref i31) (local.get $n))))
      (if (i32.lt_s (local.get $sz) (i32.const 0))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $Array_make
                               (i32.const 0) (i32.const 10)))))
      (if (i32.eqz (local.get $sz)) (then (return (global.get $empty_array))))
      (drop (block $not_float (result (ref eq))
         (local.set $f
            (struct.get $float 0
               (br_on_cast_fail $not_float (ref eq) (ref $float)
                  (local.get $v))))
         (return (array.new $float_array (local.get $f) (local.get $sz)))))
      (local.set $b
         (array.new $block (local.get $v)
            (i32.add (local.get $sz) (i32.const 1))))
      (array.set $block (local.get $b) (i32.const 0) (ref.i31 (i32.const 0)))
      (local.get $b))

   (export "caml_make_float_vect" (func $caml_floatarray_create))
   (func $caml_floatarray_create (export "caml_floatarray_create")
      (param $n (ref eq)) (result (ref eq))
      (local $sz i32)
      (local.set $sz (i31.get_s (ref.cast (ref i31) (local.get $n))))
      (if (i32.lt_s (local.get $sz) (i32.const 0))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $Array_make
                               (i32.const 0) (i32.const 10)))))
      (if (i32.eqz (local.get $sz)) (then (return (global.get $empty_array))))
      (array.new $float_array (f64.const 0) (local.get $sz)))

   (func (export "caml_floatarray_unsafe_get")
      (param $a (ref eq)) (param $i (ref eq)) (result (ref eq))
      (struct.new $float
         (array.get $float_array (ref.cast (ref $float_array) (local.get $a))
            (i31.get_s (ref.cast (ref i31) (local.get $i))))))

   (func (export "caml_floatarray_unsafe_set")
      (param $a (ref eq)) (param $i (ref eq)) (param $v (ref eq))
      (result (ref eq))
      (array.set $float_array (ref.cast (ref $float_array) (local.get $a))
         (i31.get_s (ref.cast (ref i31) (local.get $i)))
         (struct.get $float 0 (ref.cast (ref $float) (local.get $v))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_array_sub")
      (param $a (ref eq)) (param $i (ref eq)) (param $vlen (ref eq))
      (result (ref eq))
      (local $a1 (ref $block)) (local $a2 (ref $block)) (local $len i32)
      (local $fa1 (ref $float_array)) (local $fa2 (ref $float_array))
      (local.set $len (i31.get_u (ref.cast (ref i31) (local.get $vlen))))
      (if (i32.eqz (local.get $len)) (then (return (global.get $empty_array))))
      (drop (block $not_block (result (ref eq))
         (local.set $a1
            (br_on_cast_fail $not_block (ref eq) (ref $block) (local.get $a)))
         (local.set $a2 (array.new $block (ref.i31 (i32.const 0))
                           (i32.add (local.get $len) (i32.const 1))))
         (array.set $block (local.get $a2) (i32.const 0)
            (array.get $block (local.get $a1) (i32.const 0)))
         (array.copy $block $block
            (local.get $a2) (i32.const 1) (local.get $a1)
            (i32.add (i31.get_u (ref.cast (ref i31) (local.get $i)))
               (i32.const 1))
            (local.get $len))
         (return (local.get $a2))))
      (local.set $fa1 (ref.cast (ref $float_array) (local.get $a)))
      (local.set $fa2 (array.new $float_array (f64.const 0) (local.get $len)))
      (array.copy $float_array $float_array
         (local.get $fa2) (i32.const 0) (local.get $fa1)
         (i31.get_u (ref.cast (ref i31) (local.get $i)))
         (local.get $len))
      (local.get $fa2))

   (func $caml_floatarray_dup (param $a (ref $float_array)) (result (ref eq))
      (local $a' (ref $float_array))
      (local $len i32)
      (local.set $len (array.len (local.get $a)))
      (local.set $a' (array.new $float_array (f64.const 0) (local.get $len)))
      (array.copy $float_array $float_array
         (local.get $a') (i32.const 0) (local.get $a) (i32.const 0)
         (local.get $len))
      (local.get $a'))

   (func (export "caml_array_append")
      (param $va1 (ref eq)) (param $va2 (ref eq)) (result (ref eq))
      (local $a1 (ref $block)) (local $a2 (ref $block)) (local $a (ref $block))
      (local $fa1 (ref $float_array)) (local $fa2 (ref $float_array))
      (local $fa (ref $float_array))
      (local $l1 i32) (local $l2 i32)
      (drop (block $a1_not_block (result (ref eq))
         (local.set $a1
            (br_on_cast_fail $a1_not_block (ref eq) (ref $block)
               (local.get $va1)))
         (drop (block $a2_not_block (result (ref eq))
            (local.set $a2
               (br_on_cast_fail $a2_not_block (ref eq) (ref $block)
                  (local.get $va2)))
            (local.set $l1 (array.len (local.get $a1)))
            (local.set $l2 (array.len (local.get $a2)))
            (local.set $a
               (array.new $block (ref.i31 (i32.const 0))
                  (i32.sub (i32.add (local.get $l1) (local.get $l2))
                     (i32.const 1))))
            (array.copy $block $block
               (local.get $a) (i32.const 1) (local.get $a1) (i32.const 1)
               (i32.sub (local.get $l1) (i32.const 1)))
            (array.copy $block $block
               (local.get $a) (local.get $l1) (local.get $a2) (i32.const 1)
               (i32.sub (local.get $l2) (i32.const 1)))
            (return (local.get $a))))
         (return_call $caml_floatarray_dup
            (ref.cast (ref $float_array) (local.get $va2)))))
      (local.set $fa1 (ref.cast (ref $float_array) (local.get $va1)))
      (drop (block $a2_not_float_array (result (ref eq))
         (local.set $fa2
            (br_on_cast_fail $a2_not_float_array (ref eq) (ref $float_array)
               (local.get $va2)))
         (local.set $l1 (array.len (local.get $fa1)))
         (local.set $l2 (array.len (local.get $fa2)))
         (local.set $fa
            (array.new $float_array (f64.const 0)
               (i32.add (local.get $l1) (local.get $l2))))
         (array.copy $float_array $float_array
            (local.get $fa) (i32.const 0) (local.get $fa1) (i32.const 0)
            (local.get $l1))
         (array.copy $float_array $float_array
            (local.get $fa) (local.get $l1) (local.get $fa2) (i32.const 0)
            (local.get $l2))
         (return (local.get $fa))))
      (return_call $caml_floatarray_dup (local.get $fa1)))

   (func (export "caml_array_concat") (param (ref eq)) (result (ref eq))
      (local $i i32) (local $len i32)
      (local $l (ref eq)) (local $v (ref eq))
      (local $isfloat i32)
      (local $b (ref $block))
      (local $a (ref $block)) (local $a' (ref $block))
      (local $fa (ref $float_array)) (local $fa' (ref $float_array))
      (local.set $l (local.get 0))
      (local.set $len (i32.const 0))
      (loop $compute_length
         (drop (block $exit (result (ref eq))
             (local.set $b
                (br_on_cast_fail $exit (ref eq) (ref $block) (local.get $l)))
             (local.set $v (array.get $block (local.get $b) (i32.const 1)))
             (block $continue
                (drop (block $not_block (result (ref eq))
                   (local.set $len
                      (i32.add (local.get $len)
                         (i32.sub
                            (array.len
                               (br_on_cast_fail $not_block (ref eq) (ref $block)
                                  (local.get $v)))
                            (i32.const 1))))
                   (br $continue)))
                (local.set $len
                   (i32.add (local.get $len)
                      (array.len (ref.cast (ref $float_array) (local.get $v)))))
                (local.set $isfloat (i32.const 1)))
             (local.set $l (array.get $block (local.get $b) (i32.const 2)))
             (br $compute_length))))
      (if (result (ref eq)) (local.get $isfloat)
         (then
            (local.set $fa
               (array.new $float_array (f64.const 0) (local.get $len)))
            (local.set $l (local.get 0))
            (local.set $i (i32.const 0))
            (loop $fill
               (drop (block $exit (result (ref eq))
                   (local.set $b
                      (br_on_cast_fail $exit (ref eq) (ref $block)
                         (local.get $l)))
                   (local.set $l (array.get $block (local.get $b) (i32.const 2)))
                   (drop (block $not_float (result (ref eq))
                      (local.set $fa'
                         (br_on_cast_fail $not_float (ref eq) (ref $float_array)
                            (array.get $block (local.get $b) (i32.const 1))))
                      (local.set $len (array.len (local.get $fa')))
                      (array.copy $float_array $float_array
                         (local.get $fa) (local.get $i)
                         (local.get $fa') (i32.const 0)
                         (local.get $len))
                      (local.set $i (i32.add (local.get $i) (local.get $len)))
                      (br $fill)))
                   (br $fill))))
            (local.get $fa))
         (else
            (local.set $a
               (array.new $block (ref.i31 (i32.const 0))
                  (i32.add (local.get $len) (i32.const 1))))
            (local.set $l (local.get 0))
            (local.set $i (i32.const 1))
            (loop $fill
               (drop (block $exit (result (ref eq))
                   (local.set $b
                      (br_on_cast_fail $exit (ref eq) (ref $block)
                         (local.get $l)))
                   (local.set $a'
                      (ref.cast (ref $block)
                         (array.get $block (local.get $b) (i32.const 1))))
                   (local.set $len
                      (i32.sub (array.len (local.get $a')) (i32.const 1)))
                   (array.copy $block $block
                      (local.get $a) (local.get $i)
                      (local.get $a') (i32.const 1)
                      (local.get $len))
                   (local.set $i (i32.add (local.get $i) (local.get $len)))
                   (local.set $l (array.get $block (local.get $b) (i32.const 2)))
                   (br $fill))))
            (local.get $a))))

   (func $caml_floatarray_blit (export "caml_floatarray_blit")
      (param $a1 (ref eq)) (param $i1 (ref eq))
      (param $a2 (ref eq)) (param $i2 (ref eq))
      (param $vlen (ref eq))
      (result (ref eq))
      (local $len i32)
      (local.set $len (i31.get_s (ref.cast (ref i31) (local.get $vlen))))
      (if (local.get $len)
         (then
            (array.copy $float_array $float_array
               (ref.cast (ref $float_array) (local.get $a2))
               (i31.get_s (ref.cast (ref i31) (local.get $i2)))
               (ref.cast (ref $float_array) (local.get $a1))
               (i31.get_s (ref.cast (ref i31) (local.get $i1)))
               (local.get $len))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_array_blit")
      (param $a1 (ref eq)) (param $i1 (ref eq))
      (param $a2 (ref eq)) (param $i2 (ref eq))
      (param $vlen (ref eq))
      (result (ref eq))
      (local $len i32)
      (local.set $len (i31.get_s (ref.cast (ref i31) (local.get $vlen))))
      (if (local.get $len)
         (then
            (if (ref.test (ref $float_array) (local.get $a1))
               (then
                  (return_call $caml_floatarray_blit
                     (local.get $a1) (local.get $i1)
                     (local.get $a2) (local.get $i2) (local.get $vlen)))
               (else
                  (array.copy $block $block
                     (ref.cast (ref $block) (local.get $a2))
                     (i32.add
                        (i31.get_s
                           (ref.cast (ref i31) (local.get $i2))) (i32.const 1))
                     (ref.cast (ref $block) (local.get $a1))
                     (i32.add
                        (i31.get_s
                           (ref.cast (ref i31) (local.get $i1))) (i32.const 1))
                     (local.get $len))))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_array_fill")
      (param $a (ref eq)) (param $i (ref eq)) (param $vlen (ref eq))
      (param $v (ref eq)) (result (ref eq))
      (local $len i32)
      (local.set $len (i31.get_u (ref.cast (ref i31) (local.get $vlen))))
      (if $done (local.get $len)
         (then
            (drop (block $not_block (result (ref eq))
               (array.fill $block
                  (br_on_cast_fail $not_block (ref eq) (ref $block)
                     (local.get $a))
                  (i32.add (i31.get_u (ref.cast (ref i31) (local.get $i)))
                     (i32.const 1))
                  (local.get $v)
                  (local.get $len))
               (br $done)))
            (array.fill $float_array
               (ref.cast (ref $float_array) (local.get $a))
               (i31.get_u (ref.cast (ref i31) (local.get $i)))
               (struct.get $float 0 (ref.cast (ref $float) (local.get $v)))
               (local.get $len))))
      (ref.i31 (i32.const 0)))
)
