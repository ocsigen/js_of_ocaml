(module
   (import "bindings" "equals"
      (func $equals (param anyref) (param anyref) (result i32)))
   (import "obj" "forward_tag" (global $forward_tag i32))
   (import "obj" "object_tag" (global $object_tag i32))
   (import "obj" "double_array_tag" (global $double_array_tag i32))
   (import "obj" "caml_obj_tag"
      (func $caml_obj_tag (param (ref eq)) (result (ref eq))))
   (import "obj" "caml_is_closure"
      (func $caml_is_closure (param (ref eq)) (result i32)))
   (import "fail" "caml_invalid_argument"
      (func $caml_invalid_argument (param (ref eq))))
   (import "effect" "caml_is_continuation"
      (func $caml_is_continuation (param (ref eq)) (result i32)))
   (import "string" "caml_string_compare"
      (func $caml_string_compare
        (param (ref eq)) (param (ref eq)) (result (ref eq))))

   (type $block (array (mut (ref eq))))
   (type $string (array (mut i8)))
   (type $float (struct (field f64)))
   (type $js (struct (field anyref)))

   (type $int_array (array (mut i32)))
   (type $block_array (array (mut (ref $block))))
   (type $compare_stack
      (struct (field (mut i32))          ;; position in stack
              (field (ref $block_array)) ;; first value
              (field (ref $block_array)) ;; second value
              (field (ref $int_array)))) ;; position in value

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

   (global $unordered (export "unordered") i32 (i32.const 0x80000000))

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

   (func $clear_compare_stack
      ;; clear stack (to avoid memory leaks)
      (local $stack (ref $compare_stack)) (local $n i32) (local $res i32)
      (local.set $stack (global.get $default_compare_stack))
      (local.set $n (struct.get $compare_stack 0 (local.get $stack)))
      (if (i32.ge_s (local.get $n) (i32.const 0))
         (then
            (local.set $n (i32.add (local.get $n) (i32.const 1)))
            (array.fill $block_array
               (struct.get $compare_stack 1 (local.get $stack))
               (i32.const 0) (global.get $dummy_block) (local.get $n))
            (array.fill $block_array
               (struct.get $compare_stack 2 (local.get $stack))
               (i32.const 0) (global.get $dummy_block) (local.get $n)))))

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
      (call $clear_compare_stack)
      (local.get $res))

   (data $abstract_value "compare: abstract value")
   (data $functional_value "compare: functional value")
   (data $continuation_value "compare: continuation value")

   (func $do_compare_val
      (param $stack (ref $compare_stack))
      (param $v1 (ref eq)) (param $v2 (ref eq)) (param $total i32) (result i32)
      (local $i i32) (local $i1 (ref i31)) (local $i2 (ref i31))
      (local $b1 (ref $block)) (local $b2 (ref $block))
      (local $t1 i32) (local $t2 i32)
      (local $s1 i32) (local $s2 i32)
      (local $f1 f64) (local $f2 f64)
      (local $str1 (ref $string)) (local $str2 (ref $string))
      (local $c1 (ref $custom)) (local $c2 (ref $custom))
      (local $js1 anyref) (local $js2 anyref)
      (local $tuple ((ref eq) (ref eq)))
      (local $res i32)
      (loop $loop
         (block $next_item
            (if (local.get $total)
               (then
                  (br_if $next_item (ref.eq (local.get $v1) (local.get $v2)))))
            (drop (block $v1_is_not_int (result (ref eq))
               (local.set $i1
                  (br_on_cast_fail $v1_is_not_int (ref eq) (ref i31)
                    (local.get $v1)))
               (br_if $next_item (ref.eq (local.get $v1) (local.get $v2)))
               (drop (block $v2_is_not_int (result (ref eq))
                  (local.set $i2
                     (br_on_cast_fail $v2_is_not_int (ref eq) (ref i31)
                        (local.get $v2)))
                  ;; v1 and v2 are both integers
                  (return (i32.sub (i31.get_s (local.get $i1))
                                   (i31.get_s (local.get $i2))))))
               ;; check for forward tag
               (drop (block $v2_not_forward (result (ref eq))
                  (local.set $b2
                     (br_on_cast_fail $v2_not_forward (ref eq) (ref $block)
                        (local.get $v2)))
                  (local.set $t2
                     (i31.get_u
                        (ref.cast (ref i31)
                           (array.get $block (local.get $b2)
                              (i32.const 0)))))
                  (if (i32.eq (local.get $t2) (global.get $forward_tag))
                     (then
                        (local.set $v2
                           (array.get $block (local.get $b2) (i32.const 1)))
                        (br $loop)))
                  (i31.new (i32.const 1))))
               (block $v2_not_comparable
                  (drop (block $v2_not_custom (result (ref eq))
                     (local.set $c2
                         (br_on_cast_fail $v2_not_custom (ref eq) (ref $custom)
                            (local.get $v2)))
                     (local.set $res
                        (call_ref $compare
                           (local.get $v1) (local.get $v2) (local.get $total)
                           (br_on_null $v2_not_comparable
                              (struct.get $custom_operations $compare_ext
                                 (struct.get $custom 0 (local.get $c2))))))
                     (br_if $next_item (i32.eqz (local.get $res)))
                     (return (local.get $res)))))
               ;; v1 long < v2 block
               (return (i32.const -1))))
            (if (ref.test (ref i31) (local.get $v2))
               (then
                  ;; check for forward tag
                  (drop (block $v1_not_forward (result (ref eq))
                     (local.set $b1
                        (br_on_cast_fail $v1_not_forward (ref eq) (ref $block)
                           (local.get $v1)))
                     (local.set $t1
                        (i31.get_u (ref.cast (ref i31)
                                      (array.get $block (local.get $b1)
                                         (i32.const 0)))))
                     (if (i32.eq (local.get $t1) (global.get $forward_tag))
                        (then
                           (local.set $v1
                              (array.get $block (local.get $b1) (i32.const 1)))
                           (br $loop)))
                     (i31.new (i32.const 1))))
                  (block $v1_not_comparable
                     (drop (block $v1_not_custom (result (ref eq))
                        (local.set $c1
                            (br_on_cast_fail
                               $v1_not_custom (ref eq) (ref $custom)
                               (local.get $v1)))
                        (local.set $res
                           (call_ref $compare
                              (local.get $v1) (local.get $v2) (local.get $total)
                              (br_on_null $v1_not_comparable
                                 (struct.get $custom_operations $compare_ext
                                    (struct.get $custom 0 (local.get $c1))))))
                        (br_if $next_item (i32.eqz (local.get $res)))
                        (return (local.get $res)))))
                  ;; v1 block > v1 long
                  (return (i32.const 1))))
            (drop (block $heterogeneous (result (ref eq))
               (drop (block $v1_not_block (result (ref eq))
                  (local.set $b1
                     (br_on_cast_fail $v1_not_block (ref eq) (ref $block)
                        (local.get $v1)))
                  (local.set $t1
                     (i31.get_u
                        (ref.cast (ref i31)
                           (array.get $block (local.get $b1) (i32.const 0)))))
                  (local.set $b2
                     (br_on_cast_fail $heterogeneous (ref eq) (ref $block)
                        (local.get $v2)))
                  (local.set $t2
                     (i31.get_u
                        (ref.cast (ref i31)
                           (array.get $block (local.get $b2) (i32.const 0)))))
                  (drop (br_if $heterogeneous (i31.new (i32.const 0))
                     (i32.ne (local.get $t1) (local.get $t2))))
                  ;; forward tag
                  (if (i32.eq (local.get $t1) (global.get $forward_tag))
                     (then
                        (local.set $v1
                           (array.get $block (local.get $b1) (i32.const 1)))
                        (local.set $v2
                           (array.get $block (local.get $b2) (i32.const 1)))
                        (br $loop)))
                  (if (i32.eq (local.get $t1) (global.get $object_tag))
                     (then
                        (local.set $v1
                           (array.get $block (local.get $b1) (i32.const 2)))
                        (local.set $v2
                           (array.get $block (local.get $b2) (i32.const 2)))
                        (br_if $next_item
                           (ref.eq (local.get $v1) (local.get $v2)))
                        (return
                           (i32.sub
                              (i31.get_s (ref.cast (ref i31) (local.get $v1)))
                              (i31.get_s
                                 (ref.cast (ref i31) (local.get $v2)))))))
                  (local.set $s1 (array.len (local.get $b1)))
                  (local.set $s2 (array.len (local.get $b2)))
                  ;; compare size first
                  (if (i32.ne (local.get $s1) (local.get $s2))
                     (then
                        (return (i32.sub (local.get $s1) (local.get $s2)))))
                  (if (i32.eq (local.get $t1) (global.get $double_array_tag))
                     (then
                        (local.set $i (i32.const 1))
                        (loop $float_array
                           (if (i32.lt_s (local.get $i) (local.get $s1))
                              (then
                                 (local.set $f1
                                    (struct.get $float 0
                                       (ref.cast (ref $float)
                                          (array.get $block (local.get $b1)
                                             (local.get $i)))))
                                 (local.set $f2
                                    (struct.get $float 0
                                       (ref.cast (ref $float)
                                          (array.get $block (local.get $b2)
                                          (local.get $i)))))
                                 (if (f64.lt (local.get $f1) (local.get $f2))
                                    (then (return (i32.const -1))))
                                 (if (f64.gt (local.get $f1) (local.get $f2))
                                    (then (return (i32.const 1))))
                                 (if (f64.ne (local.get $f1) (local.get $f2))
                                    (then
                                       (if (i32.eqz (local.get $total))
                                          (then
                                             (return (global.get $unordered))))
                                       (if (f64.eq (local.get $f1)
                                                   (local.get $f1))
                                          (then (return (i32.const 1))))
                                       (if (f64.eq (local.get $f2)
                                                   (local.get $f2))
                                          (then (return (i32.const -1))))))
                                 (local.set $i
                                    (i32.add (local.get $i) (i32.const 1)))
                                 (br $float_array))))
                        (br $next_item)))
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
               (drop (block $v1_not_float (result (ref eq))
                  (local.set $f1
                     (struct.get $float 0
                        (br_on_cast_fail $v1_not_float (ref eq) (ref $float)
                           (local.get $v1))))
                  (local.set $f2
                     (struct.get $float 0
                        (br_on_cast_fail $heterogeneous (ref eq) (ref $float)
                           (local.get $v2))))
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
               (drop (block $v1_not_string (result (ref eq))
                  (local.set $str1
                     (br_on_cast_fail $v1_not_string (ref eq) (ref $string)
                        (local.get $v1)))
                  (local.set $str2
                      (br_on_cast_fail $heterogeneous (ref eq) (ref $string)
                         (local.get $v2)))
                  (local.set $res
                     (call $compare_strings (local.get $str1) (local.get $str2)))
                  (br_if $next_item (i32.eqz (local.get $res)))
                  (return (local.get $res))))
               (drop (block $v1_not_custom (result (ref eq))
                  (local.set $c1
                     (br_on_cast_fail $v1_not_custom (ref eq) (ref $custom)
                        (local.get $v1)))
                  (local.set $c2
                      (br_on_cast_fail $heterogeneous (ref eq) (ref $custom)
                         (local.get $v2)))
                  (if (i32.eqz
                         (ref.eq (struct.get $custom 0 (local.get $c1))
                                 (struct.get $custom 0 (local.get $c2))))
                     (then
                        (return
                           (i31.get_s
                              (ref.cast (ref i31)
                                 (call $caml_string_compare
                                    (struct.get $custom_operations $id
                                       (struct.get $custom 0
                                          (local.get $c1)))
                                    (struct.get $custom_operations $id
                                       (struct.get $custom 0
                                          (local.get $c2)))))))))
                  (block $not_comparable
                     (local.set $res
                        (call_ref $compare
                           (local.get $v1) (local.get $v2) (local.get $total)
                           (br_on_null $not_comparable
                              (struct.get $custom_operations $compare
                                 (struct.get $custom 0 (local.get $c1))))))
                     (br_if $next_item (i32.eqz (local.get $res)))
                     (return (local.get $res)))
                  (call $clear_compare_stack)
                  (call $caml_invalid_argument
                     (array.new_data $string $abstract_value
                        (i32.const 0) (i32.const 23)))
                  (i31.new (i32.const 0))))
               ;; ZZZ float array (unboxed)
               (drop (block $v1_not_js (result (ref eq))
                  (local.set $js1
                     (struct.get $js 0
                        (br_on_cast_fail $v1_not_js (ref eq) (ref $js)
                           (local.get $v1))))
                  (local.set $js2
                     (struct.get $js 0
                        (br_on_cast_fail $heterogeneous (ref eq) (ref $js)
                           (local.get $v2))))
                  (drop (block $not_jsstring (result anyref)
                     (local.set $res
                        (string.compare
                           (br_on_cast_fail $not_jsstring anyref (ref string)
                              (local.get $js1))
                           (br_on_cast_fail $not_jsstring anyref (ref string)
                              (local.get $js2))))
                     (br_if $next_item (i32.eqz (local.get $res)))
                     (return (local.get $res))))
                  ;; We cannot order two JavaScript objects,
                  ;; but we can tell whether they are equal or not
                  (if (i32.eqz (local.get $total))
                     (then
                        (br_if $next_item
                           (call $equals (local.get $js1) (local.get $js2)))
                        (return (global.get $unordered))))
                  (br $heterogeneous (i31.new (i32.const 0)))))
               (if (call $caml_is_closure (local.get $v1))
                  (then
                     (drop (br_if $heterogeneous (i31.new (i32.const 0))
                              (i32.eqz (call $caml_is_closure (local.get $v2)))))
                     (call $clear_compare_stack)
                     (call $caml_invalid_argument
                        (array.new_data $string $functional_value
                           (i32.const 0) (i32.const 25)))))
               (if (call $caml_is_continuation (local.get $v1))
                  (then
                     (drop (br_if $heterogeneous(i31.new (i32.const 0))
                              (i32.eqz
                                 (call $caml_is_continuation (local.get $v2)))))
                     (call $clear_compare_stack)
                     (call $caml_invalid_argument
                        (array.new_data $string $continuation_value
                           (i32.const 0) (i32.const 27)))))
               (i31.new (i32.const 0)))) ;; fall through
            ;; heterogeneous comparison
            (local.set $t1
               (i31.get_u
                  (ref.cast (ref i31) (call $caml_obj_tag (local.get $v1)))))
            (local.set $t2
               (i31.get_u
                  (ref.cast (ref i31) (call $caml_obj_tag (local.get $v2)))))
            (if (i32.eq (local.get $t1) (global.get $forward_tag))
               (then
                  (local.set $v1
                     (array.get $block (ref.cast (ref $block) (local.get $v1))
                        (i32.const 1)))
                  (br $loop)))
            (if (i32.eq (local.get $t2) (global.get $forward_tag))
               (then
                  (local.set $v2
                     (array.get $block (ref.cast (ref $block) (local.get $v2))
                        (i32.const 1)))
                  (br $loop)))
            (local.set $res (i32.sub (local.get $t1) (local.get $t2)))
            (if (i32.eqz (local.get $res))
               (then
                  (call $clear_compare_stack)
                  (call $caml_invalid_argument
                     (array.new_data $string $abstract_value
                        (i32.const 0) (i32.const 23)))))
            (return (local.get $res)))
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
)
