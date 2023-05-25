(module
   (import "bindings" "log" (func $log (param i32)))
   (import "obj" "forward_tag" (global $forward_tag i32))
   (import "obj" "double_array_tag" (global $double_array_tag i32))

   (type $block (array (mut (ref eq))))
   (type $string (array (mut i8)))
   (type $float (struct (field f64)))

   (type $int_array (array (mut i32)))
   (type $block_array (array (mut (ref $block))))
   (type $compare_stack
      (struct (field (mut i32))          ;; position in stack
              (field (ref $block_array)) ;; first value
              (field (ref $block_array)) ;; second value
              (field (ref $int_array)))) ;; position in value
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
)
