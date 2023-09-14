(module
   (import "obj" "abstract_tag" (global $abstract_tag i32))
   (import "obj" "caml_obj_dup"
      (func $caml_obj_dup (param (ref eq)) (result (ref eq))))
   (import "fail" "caml_invalid_argument"
      (func $caml_invalid_argument (param $arg (ref eq))))
   (import "bindings" "weak_new"
      (func $weak_new (param (ref eq)) (result anyref)))
   (import "bindings" "weak_deref"
      (func $weak_deref (param anyref) (result eqref)))
   (import "bindings" "weak_map_new" (func $weak_map_new (result (ref any))))
   (import "bindings" "map_get"
      (func $map_get (param (ref any)) (param (ref eq)) (result anyref)))
   (import "bindings" "map_set"
      (func $map_set (param (ref any)) (param (ref eq)) (param (ref any))))
   (import "jslib" "unwrap" (func $unwrap (param (ref eq)) (result anyref)))
   (import "jslib" "wrap" (func $wrap (param anyref) (result (ref eq))))
   (type $block (array (mut (ref eq))))
   (type $string (array (mut i8)))
   (type $js (struct (field anyref)))

   ;; A weak array is a an abstract value composed of possibly some
   ;; data and an array of keys.
   ;; Keys are either caml_ephe_none (unset), a 31-bit integer, or a
   ;; weak reference.
   ;; To access the data, we need to traverse a series of weak maps
   ;; indexed by the keys (omitting integers).

   (global $caml_ephe_data_offset i32 (i32.const 2))
   (global $caml_ephe_key_offset i32 (i32.const 3))

   (global $caml_ephe_none (ref eq)
      (array.new_fixed $block 1 (ref.i31 (global.get $abstract_tag))))

   (func $caml_ephe_get_data (export "caml_ephe_get_data")
      (param $vx (ref eq)) (result (ref eq))
      (local $x (ref $block))
      (local $d (ref eq)) (local $v (ref eq))
      (local $m (ref any))
      (local $i i32) (local $len i32)
      (local.set $x (ref.cast (ref $block) (local.get $vx)))
      (local.set $d
         (array.get $block (local.get $x) (global.get $caml_ephe_data_offset)))
      (block $no_data
         (block $released
            (br_if $no_data
               (ref.eq (local.get $d) (global.get $caml_ephe_none)))
            (local.set $i (global.get $caml_ephe_key_offset))
            (local.set $len (array.len (local.get $x)))
            (local.set $m (ref.as_non_null (call $unwrap (local.get $d))))
            (loop $loop
               (if (i32.lt_u (local.get $i) (local.get $len))
                  (then
                     (local.set $v
                        (array.get $block (local.get $x) (local.get $i)))
                     (local.set $i (i32.add (local.get $i) (i32.const 1)))
                     (br_if $loop
                        (ref.eq (local.get $v) (global.get $caml_ephe_none)))
                     (br_if $loop (ref.test (ref i31) (local.get $v)))
                     (local.set $v
                        (br_on_null $released
                           (call $weak_deref (call $unwrap (local.get $v)))))
                     (local.set $m
                        (br_on_null $released
                           (call $map_get (local.get $m) (local.get $v))))
                     (br $loop))))
            (return
              (array.new_fixed $block 2 (ref.i31 (i32.const 0))
                 (ref.cast (ref eq) (local.get $m)))))
         (array.set $block (local.get $x) (global.get $caml_ephe_data_offset)
            (global.get $caml_ephe_none)))
      (ref.i31 (i32.const 0)))

   (func (export "caml_ephe_get_data_copy")
      (param $x (ref eq)) (result (ref eq))
      (local $r (ref eq))
      (local.set $r (call $caml_ephe_get_data (local.get $x)))
      (drop (block $no_copy (result (ref eq))
         (return
            (array.new_fixed $block 2 (ref.i31 (i32.const 0))
               (call $caml_obj_dup
                  (br_on_cast_fail $no_copy (ref eq) (ref $block)
                     (array.get $block
                        (br_on_cast_fail $no_copy (ref eq) (ref $block)
                           (local.get $r))
                        (i32.const 1))))))))
      (local.get $r))

   (func $caml_ephe_set_data (export "caml_ephe_set_data")
      (param $vx (ref eq)) (param $data (ref eq)) (result (ref eq))
      (local $x (ref $block))
      (local $v (ref eq))
      (local $m (ref any)) (local $m' (ref any))
      (local $i i32)
      (local.set $x (ref.cast (ref $block) (local.get $vx)))
      (local.set $i (array.len (local.get $x)))
      (local.set $m (local.get $data))
      (loop $loop
         (local.set $i (i32.sub (local.get $i) (i32.const 1)))
         (if (i32.ge_u (local.get $i) (global.get $caml_ephe_key_offset))
            (then
               (local.set $v
                  (array.get $block (local.get $x) (local.get $i)))
               (br_if $loop
                  (ref.eq (local.get $v) (global.get $caml_ephe_none)))
               (br_if $loop (ref.test (ref i31) (local.get $v)))
               (block $released
                  (local.set $v
                     (br_on_null $released
                        (call $weak_deref (call $unwrap (local.get $v)))))
                  (local.set $m' (call $weak_map_new))
                  (call $map_set (local.get $m') (local.get $v)
                     (local.get $m))
                  (local.set $m (local.get $m'))
                  (br $loop))
               (array.set $block (local.get $x) (local.get $i)
                  (global.get $caml_ephe_none))
               (br $loop))))
      (array.set $block (local.get $x) (global.get $caml_ephe_data_offset)
         (call $wrap (local.get $m)))
      (ref.i31 (i32.const 0)))

   (func (export "caml_ephe_unset_data")
      (param $vx (ref eq)) (result (ref eq))
      (local $x (ref $block))
      (local.set $x (ref.cast (ref $block) (local.get $vx)))
      (array.set $block (local.get $x) (global.get $caml_ephe_data_offset)
         (global.get $caml_ephe_none))
      (ref.i31 (i32.const 0)))

   (func (export "caml_ephe_check_data")
      (param $x (ref eq)) (result (ref eq))
      (ref.i31
         (i32.eqz
            (ref.eq (call $caml_ephe_get_data (local.get $x))
               (ref.i31 (i32.const 0))))))

   (func $caml_ephe_set_data_opt
      (param $x (ref eq)) (param $opt_data (ref eq))
      (drop (block $no_data (result (ref eq))
         (call $caml_ephe_set_data (local.get $x)
            (array.get $block
               (br_on_cast_fail $no_data (ref eq) (ref $block)
                  (local.get $opt_data))
               (i32.const 1))))))

   (export "caml_weak_get" (func $caml_ephe_get_key))
   (func $caml_ephe_get_key (export "caml_ephe_get_key")
      (param $vx (ref eq)) (param $vi (ref eq)) (result (ref eq))
      (local $x (ref $block))
      (local $i i32)
      (local $v (ref eq))
      (local.set $x (ref.cast (ref $block) (local.get $vx)))
      (local.set $i
         (i32.add (global.get $caml_ephe_key_offset)
            (i31.get_s (ref.cast (ref i31) (local.get $vi)))))
      (local.set $v (array.get $block (local.get $x) (local.get $i)))
      (block $value
         (block $no_value
            (br_if $no_value
               (ref.eq (local.get $v) (global.get $caml_ephe_none)))
            (br_if $value (ref.test (ref i31) (local.get $v)))
            (block $released
               (local.set $v
                  (br_on_null $released
                     (call $weak_deref (call $unwrap (local.get $v)))))
               (br $value))
            (array.set $block (local.get $x) (local.get $i)
               (global.get $caml_ephe_none))
            (array.set $block (local.get $x) (global.get $caml_ephe_data_offset)
               (global.get $caml_ephe_none)))
         (return (ref.i31 (i32.const 0))))
      (array.new_fixed $block 2 (ref.i31 (i32.const 0)) (local.get $v)))

   (export "caml_weak_get_copy" (func $caml_ephe_get_key_copy))
   (func $caml_ephe_get_key_copy (export "caml_ephe_get_key_copy")
      (param $x (ref eq)) (param $i (ref eq)) (result (ref eq))
      (local $r (ref eq))
      (local.set $r (call $caml_ephe_get_key (local.get $x) (local.get $i)))
      (drop (block $no_copy (result (ref eq))
         (return
            (array.new_fixed $block 2 (ref.i31 (i32.const 0))
               (call $caml_obj_dup
                  (br_on_cast_fail $no_copy (ref eq) (ref $block)
                     (array.get $block
                        (br_on_cast_fail $no_copy (ref eq) (ref $block)
                           (local.get $r))
                        (i32.const 1))))))))
      (local.get $r))

   (export "caml_weak_check" (func $caml_ephe_check_key))
   (func $caml_ephe_check_key (export "caml_ephe_check_key")
      (param $vx (ref eq)) (param $vi (ref eq)) (result (ref eq))
      (local $x (ref $block))
      (local $i i32)
      (local $v (ref eq))
      (local.set $x (ref.cast (ref $block) (local.get $vx)))
      (local.set $i
        (i32.add (i31.get_s (ref.cast (ref i31) (local.get $vi)))
           (global.get $caml_ephe_key_offset)))
      (local.set $v (array.get $block (local.get $x) (local.get $i)))
      (block $value
         (block $no_value
            (br_if $no_value
               (ref.eq (local.get $v) (global.get $caml_ephe_none)))
            (br_if $value (ref.test (ref i31) (local.get $v)))
            (br_if $value
               (i32.eqz
                  (ref.is_null
                     (call $weak_deref (call $unwrap (local.get $v))))))
            (array.set $block (local.get $x) (local.get $i)
               (global.get $caml_ephe_none))
            (array.set $block (local.get $x) (global.get $caml_ephe_data_offset)
               (global.get $caml_ephe_none)))
         (return (ref.i31 (i32.const 0))))
      (ref.i31 (i32.const 1)))

   (func $caml_ephe_set_key (export "caml_ephe_set_key")
      (param $vx (ref eq)) (param $vi (ref eq)) (param $v (ref eq))
      (result (ref eq))
      (local $x (ref $block))
      (local $d (ref eq))
      (local $i i32)
      (local.set $x (ref.cast (ref $block) (local.get $vx)))
      (local.set $i
         (i32.add (i31.get_s (ref.cast (ref i31) (local.get $vi)))
            (global.get $caml_ephe_key_offset)))
      (local.set $d (ref.i31 (i32.const 0)))
      (if (ref.test (ref i31) (local.get $v))
         (then
            (if (ref.test (ref $js)
                   (array.get $block (local.get $x) (local.get $i)))
               (then
                  (local.set $d (call $caml_ephe_get_data (local.get $vx)))))
            (array.set $block (local.get $x) (local.get $i) (local.get $v)))
         (else
            (local.set $d (call $caml_ephe_get_data (local.get $vx)))
            (array.set $block (local.get $x) (local.get $i)
               (call $wrap (call $weak_new (local.get $v))))))
      (call $caml_ephe_set_data_opt (local.get $vx) (local.get $d))
      (ref.i31 (i32.const 0)))

   (func $caml_ephe_unset_key (export "caml_ephe_unset_key")
      (param $vx (ref eq)) (param $vi (ref eq)) (result (ref eq))
      (local $x (ref $block))
      (local $d (ref eq))
      (local $i i32)
      (local.set $x (ref.cast (ref $block) (local.get $vx)))
      (local.set $i
         (i32.add (i31.get_s (ref.cast (ref i31) (local.get $vi)))
            (global.get $caml_ephe_key_offset)))
      (local.set $d (ref.i31 (i32.const 0)))
      (if (ref.test (ref $js) (array.get $block (local.get $x) (local.get $i)))
         (then
            (local.set $d (call $caml_ephe_get_data (local.get $vx)))))
      (array.set $block (local.get $x) (local.get $i)
         (global.get $caml_ephe_none))
      (call $caml_ephe_set_data_opt (local.get $vx) (local.get $d))
      (ref.i31 (i32.const 0)))

   (data $Weak_create "Weak.create")

   (export "caml_weak_create" (func $caml_ephe_create))
   (func $caml_ephe_create (export "caml_ephe_create")
      (param $vlen (ref eq)) (result (ref eq))
      (local $len i32)
      (local $res (ref $block))
      (local.set $len (i31.get_s (ref.cast (ref i31) (local.get $vlen))))
      (if (i32.lt_s (local.get $len) (i32.const 0))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $Weak_create
                  (i32.const 0) (i32.const 11)))))
      (local.set $res
         (array.new $block (global.get $caml_ephe_none)
            (i32.add (local.get $len) (global.get $caml_ephe_key_offset))))
      (array.set $block (local.get $res) (i32.const 0)
         (ref.i31 (global.get $abstract_tag)))
      (local.get $res))

   (func (export "caml_ephe_blit_data")
      (param $x (ref eq)) (param $y (ref eq)) (result (ref eq))
      (call $caml_ephe_set_data_opt
         (local.get $y) (call $caml_ephe_get_data (local.get $x)))
      (ref.i31 (i32.const 0)))

   (export "caml_weak_blit" (func $caml_ephe_blit_key))
   (func $caml_ephe_blit_key (export "caml_ephe_blit_key")
      (param $x (ref eq)) (param $i (ref eq))
      (param $y (ref eq)) (param $j (ref eq))
      (param $l (ref eq)) (result (ref eq))
      (local $d (ref eq))
      (local.set $d (call $caml_ephe_get_data (local.get $y)))
      (array.copy $block $block
         (ref.cast (ref $block) (local.get $y))
         (i32.add (i31.get_s (ref.cast (ref i31) (local.get $j)))
            (global.get $caml_ephe_key_offset))
         (ref.cast (ref $block) (local.get $x))
         (i32.add (i31.get_s (ref.cast (ref i31) (local.get $i)))
            (global.get $caml_ephe_key_offset))
         (i31.get_s (ref.cast (ref i31) (local.get $l))))
      (call $caml_ephe_set_data_opt (local.get $y) (local.get $d))
      (ref.i31 (i32.const 0)))

   (func (export "caml_weak_set")
      (param $x (ref eq)) (param $i (ref eq)) (param $v (ref eq))
      (result (ref eq))
      (drop (block $unset (result (ref eq))
         (return_call $caml_ephe_set_key
            (local.get $x) (local.get $i)
            (array.get $block
               (br_on_cast_fail $unset (ref eq) (ref $block) (local.get $v))
               (i32.const 1)))))
      (return_call $caml_ephe_unset_key (local.get $x) (local.get $i)))
)
