(module
   (import "fail" "caml_invalid_argument"
      (func $caml_invalid_argument (param (ref eq))))

   (type $block (array (mut (ref eq))))
   (type $string (array (mut i8)))
   (type $float (struct (field f64)))

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
         (local.get $a) (local.get $l1) (local.get $a2) (i32.const 1)
         (i32.sub (local.get $l2) (i32.const 1)))
      (local.get $a))

   (func (export "caml_array_concat") (param (ref eq)) (result (ref eq))
      ;; ZZZ float array
      (local $i i32) (local $len i32)
      (local $l (ref eq))
      (local $a (ref $block)) (local $a' (ref $block)) (local $b (ref $block))
      (local.set $l (local.get 0))
      (local.set $len (i32.const 1))
      (loop $compute_length
         (drop (block $exit (result (ref eq))
             (local.set $b (br_on_cast_fail $exit $block (local.get $l)))
             (local.set $len
                (i32.add (local.get $len)
                   (i32.sub
                      (array.len
                         (ref.cast $block
                            (array.get $block (local.get $b) (i32.const 1))))
                      (i32.const 1))))
             (local.set $l (array.get $block (local.get $b) (i32.const 2)))
             (br $compute_length))))
      (local.set $a
         (array.new $block (i31.new (i32.const 0)) (local.get $len)))
      (local.set $l (local.get 0))
      (local.set $i (i32.const 1))
      (loop $fill
         (drop (block $exit (result (ref eq))
             (local.set $b (br_on_cast_fail $exit $block (local.get $l)))
             (local.set $a'
                (ref.cast $block
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
      (local.get $a))

   (export "caml_floatarray_blit" (func $caml_array_blit))
   (func $caml_array_blit (export "caml_array_blit")
      (param $a1 (ref eq)) (param $i1 (ref eq))
      (param $a2 (ref eq)) (param $i2 (ref eq))
      (param $len (ref eq))
      (result (ref eq))
      (array.copy $block $block
         (ref.cast $block (local.get $a2))
         (i32.add (i31.get_s (ref.cast i31 (local.get $i2))) (i32.const 1))
         (ref.cast $block (local.get $a1))
         (i32.add (i31.get_s (ref.cast i31 (local.get $i1))) (i32.const 1))
         (i31.get_s (ref.cast i31 (local.get $len))))
      (i31.new (i32.const 0)))

   (func (export "caml_array_fill")
      (param $a (ref eq)) (param $i (ref eq)) (param $len (ref eq))
      (param $v (ref eq)) (result (ref eq))
      (array.fill $block (ref.cast $block (local.get $a))
         (i32.add (i31.get_u (ref.cast i31 (local.get $i))) (i32.const 1))
         (local.get $v)
         (i31.get_u (ref.cast i31 (local.get $len))))
      (i31.new (i32.const 0)))
)
