(module
   (import "bindings" "log" (func $log_js (param anyref)))
   (import "fail" "caml_failwith" (func $caml_failwith (param (ref eq))))

   (type $block (array (mut (ref eq))))
   (type $string (array (mut i8)))
   (type $float (struct (field f64)))
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
   (type $js (struct (field anyref)))
   (type $function_1 (func (param (ref eq) (ref eq)) (result (ref eq))))
   (type $closure (struct (;(field i32);) (field (ref $function_1))))
   (type $closure_last_arg
      (sub $closure (struct (;(field i32);) (field (ref $function_1)))))

   (type $int_array (array (mut i32)))

   (type $dummy_closure_1
      (sub $closure_last_arg
         (struct (field (ref $function_1)) (field (mut (ref null $closure))))))

   (type $function_2
      (func (param (ref eq) (ref eq) (ref eq)) (result (ref eq))))

   (type $closure_2
      (sub $closure
         (struct (field (ref $function_1)) (field (ref $function_2)))))

   (type $dummy_closure_2
      (sub $closure_2
         (struct (field (ref $function_1)) (field (ref $function_2))
            (field (mut (ref null $closure_2))))))

   (type $function_3
      (func (param (ref eq) (ref eq) (ref eq) (ref eq)) (result (ref eq))))

   (type $closure_3
      (sub $closure
         (struct (field (ref $function_1)) (field (ref $function_3)))))

   (type $dummy_closure_3
      (sub $closure_3
         (struct (field (ref $function_1)) (field (ref $function_3))
            (field (mut (ref null $closure_3))))))

   (type $function_4
      (func (param (ref eq) (ref eq) (ref eq) (ref eq)) (result (ref eq))))

   (type $closure_4
      (sub $closure
         (struct (field (ref $function_1)) (field (ref $function_4)))))

   (type $dummy_closure_4
      (sub $closure_4
         (struct (field (ref $function_1)) (field (ref $function_4))
            (field (mut (ref null $closure_4))))))

   (global $forcing_tag i32 (i32.const 244))
   (global $cont_tag i32 (i32.const 245))
   (global $lazy_tag i32 (i32.const 246))
   (global $closure_tag i32 (i32.const 247))
   (global $object_tag (export "object_tag") i32 (i32.const 248))
   (global $forward_tag (export "forward_tag") i32 (i32.const 250))
   (global $abstract_tag (export "abstract_tag") i32 (i32.const 251))
   (global $string_tag i32 (i32.const 252))
   (global $float_tag i32 (i32.const 253))
   (global $double_array_tag (export "double_array_tag") i32 (i32.const 254))
   (global $custom_tag i32 (i32.const 255))

   (func (export "caml_alloc_dummy") (param $size (ref eq)) (result (ref eq))
      (array.new $block (i31.new (i32.const 0))
                 (i32.add (i31.get_u (ref.cast i31 (local.get $size)))
                          (i32.const 1))))

   (func (export "caml_update_dummy")
      (param $dummy (ref eq)) (param $newval (ref eq)) (result (ref eq))
      (local $i i32)
      (local $dst (ref $block)) (local $src (ref $block))
      (drop (block $not_block (result (ref eq))
         (local.set $dst
            (br_on_cast_fail $not_block $block (local.get $dummy)))
         (local.set $src (ref.cast $block (local.get $newval)))
         (array.copy $block $block
            (local.get $dst) (i32.const 0) (local.get $src) (i32.const 0)
            (array.len (local.get $dst)))
         (return (i31.new (i32.const 0)))))
      (drop (block $not_closure_1 (result (ref eq))
         (struct.set $dummy_closure_1 1
            (br_on_cast_fail $not_closure_1 $dummy_closure_1 (local.get $dummy))
            (ref.cast $closure (local.get $newval)))
         (return (i31.new (i32.const 0)))))
      (drop (block $not_closure_2 (result (ref eq))
         (struct.set $dummy_closure_2 2
            (br_on_cast_fail $not_closure_2 $dummy_closure_2 (local.get $dummy))
            (ref.cast $closure_2 (local.get $newval)))
         (return (i31.new (i32.const 0)))))
      (drop (block $not_closure_3 (result (ref eq))
         (struct.set $dummy_closure_3 2
            (br_on_cast_fail $not_closure_3 $dummy_closure_3 (local.get $dummy))
            (ref.cast $closure_3 (local.get $newval)))
         (return (i31.new (i32.const 0)))))
      (drop (block $not_closure_4 (result (ref eq))
         (struct.set $dummy_closure_4 2
            (br_on_cast_fail $not_closure_4 $dummy_closure_4 (local.get $dummy))
            (ref.cast $closure_4 (local.get $newval)))
         (return (i31.new (i32.const 0)))))
      ;; ZZZ float array
      (unreachable))

   (func $caml_obj_dup (export "caml_obj_dup")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ Deal with non-block values?
      (local $orig (ref $block))
      (local $res (ref $block))
      (local $len i32)
      (local.set $orig (ref.cast $block (local.get 0)))
      (local.set $len (array.len (local.get $orig)))
      (local.set $res
         (array.new $block (array.get $block (local.get $orig) (i32.const 0))
            (local.get $len)))
      (array.copy $block $block
         (local.get $res) (i32.const 1) (local.get $orig) (i32.const 1)
         (i32.sub (local.get $len) (i32.const 1)))
      (local.get $res))

   (func (export "caml_obj_with_tag")
      (param $tag (ref eq)) (param (ref eq)) (result (ref eq))
      (local $res (ref eq))
      (local.set $res (call $caml_obj_dup (local.get 1)))
      (array.set $block (ref.cast $block (local.get $res)) (i32.const 0)
         (local.get $tag))
      (local.get $res))

   (func (export "caml_obj_block")
      (param $tag (ref eq)) (param $size (ref eq)) (result (ref eq))
      (local $res (ref $block))
      ;; ZZZ float array / specific types
      (local.set $res
         (array.new $block
            (i31.new (i32.const 0))
            (i32.add (i31.get_s (ref.cast i31 (local.get $size)))
                     (i32.const 1))))
      (array.set $block (local.get $res) (i32.const 0) (local.get $tag))
      (local.get $res))

   (func (export "caml_obj_tag") (param $v (ref eq)) (result (ref eq))
      (if (ref.test i31 (local.get $v))
         (then (return (i31.new (i32.const 1000)))))
      (drop (block $not_block (result (ref eq))
         (return (array.get $block
                    (br_on_cast_fail $not_block $block (local.get $v))
                    (i32.const 0)))))
      (if (ref.test $string (local.get $v))
         (then (return (i31.new (global.get $string_tag)))))
      (if (ref.test $float (local.get $v))
         (then (return (i31.new (global.get $float_tag)))))
      (if (ref.test $custom (local.get $v))
         (then (return (i31.new (global.get $custom_tag)))))
      (if (ref.test $closure (local.get $v))
         (then (return (i31.new (global.get $closure_tag)))))
      ;; ZZZ float array
      (if (ref.test $js (local.get $v))
         (then (return (i31.new (global.get $abstract_tag)))))
      (unreachable))

   (func (export "caml_obj_make_forward")
      (param $b (ref eq)) (param $v (ref eq)) (result (ref eq))
      (local $block (ref $block))
      (local.set $block (ref.cast $block (local.get $b)))
      (array.set $block (local.get $block)
         (i32.const 0) (i31.new (global.get $forward_tag)))
      (array.set $block (local.get $block) (i32.const 1) (local.get $v))
      (i31.new (i32.const 0)))

   (func (export "caml_lazy_make_forward")
      (param (ref eq)) (result (ref eq))
      (array.new_fixed $block (i31.new (global.get $forward_tag))
         (local.get 0)))

   (func $obj_update_tag
      (param (ref eq)) (param $o i32) (param $n i32) (result i32)
      (local $b (ref $block))
      (local.set $b (ref.cast $block (local.get 0)))
      (if (result i32) (ref.eq (array.get $block (local.get $b) (i32.const 0))
                               (i31.new (local.get $o)))
         (then
            (array.set $block (local.get $b) (i32.const 0)
               (i31.new (local.get $n)))
            (i32.const 1))
         (else
            (i32.const 0))))

   (func (export "caml_lazy_reset_to_lazy") (param (ref eq)) (result (ref eq))
      (drop (call $obj_update_tag (local.get 0)
               (global.get $forcing_tag) (global.get $lazy_tag)))
      (i31.new (i32.const 0)))

   (func (export "caml_lazy_update_to_forward") (param (ref eq)) (result (ref eq))
      (drop (call $obj_update_tag (local.get 0)
               (global.get $forcing_tag) (global.get $forward_tag)))
      (i31.new (i32.const 0)))

   (func (export "caml_lazy_update_to_forcing")
      (param (ref eq)) (result (ref eq))
      (if (ref.test $block (local.get 0))
         (then
            (if (call $obj_update_tag (local.get 0)
                   (global.get $lazy_tag) (global.get $forcing_tag))
               (then (return (i31.new (i32.const 0)))))))
      (i31.new (i32.const 1)))

   (func (export "caml_obj_compare_and_swap")
      (param (ref eq)) (param (ref eq))
      (param $old (ref eq)) (param $new (ref eq)) (result (ref eq))
      (local $b (ref $block))
      (local $i i32)
      (local.set $b (ref.cast $block (local.get 0)))
      (local.set $i
         (i32.add (i31.get_u (ref.cast i31 (local.get 1))) (i32.const 1)))
      (if (result (ref eq))
          (ref.eq
            (array.get $block (local.get $b) (local.get $i)) (local.get $old))
         (then
            (array.set $block (local.get $b) (local.get $i) (local.get $new))
            (i31.new (i32.const 1)))
         (else
            (i31.new (i32.const 0)))))

   (func (export "caml_obj_is_shared") (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 1)))

   (func (export "caml_obj_raw_field")
      (param $o (ref eq)) (param $i (ref eq)) (result (ref eq))
      (array.get $block (ref.cast $block (local.get $o))
         (i32.add (i31.get_u (ref.cast i31 (local.get $i))) (i32.const 1))))

   (func (export "caml_obj_set_raw_field")
      (param $o (ref eq)) (param $i (ref eq)) (param $v (ref eq))
      (result (ref eq))
      (array.set $block (ref.cast $block (local.get $o))
         (i32.add (i31.get_u (ref.cast i31 (local.get $i))) (i32.const 1))
         (local.get $v))
      (i31.new (i32.const 0)))

   (data $not_implemented "Obj.add_offset is not supported")

   (func (export "caml_obj_add_offset")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (call $caml_failwith
         (array.new_data $string $not_implemented (i32.const 0) (i32.const 31)))
      (i31.new (i32.const 0)))

   (data $truncate_not_implemented "Obj.truncate is not supported")

   (func (export "caml_obj_truncate")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (call $caml_failwith
         (array.new_data $string $truncate_not_implemented
            (i32.const 0) (i32.const 29)))
      (i31.new (i32.const 0)))

   (global $method_cache (mut (ref $int_array))
      (array.new $int_array (i32.const 0) (i32.const 8)))

   (func (export "caml_get_public_method")
      (param $obj (ref eq) (ref eq) (ref eq)) (result (ref eq))
      (local $meths (ref $block))
      (local $tag i32) (local $cacheid i32) (local $ofs i32)
      (local $li i32) (local $mi i32) (local $hi i32)
      (local $a (ref $int_array)) (local $len i32)
      (local.set $meths
         (ref.cast $block
            (array.get $block (ref.cast $block (local.get $obj)) (i32.const 1))))
      (local.set $tag (i31.get_s (ref.cast i31 (local.get 1))))
      (local.set $cacheid (i31.get_u (ref.cast i31 (local.get 2))))
      (local.set $len (array.len (global.get $method_cache)))
      (if (i32.ge_s (local.get $cacheid) (local.get $len))
         (then
            (loop $size
               (local.set $len (i32.shl (local.get $len) (i32.const 1)))
               (br_if $size (i32.ge_s (local.get $cacheid) (local.get $len))))
            (local.set $a (array.new $int_array (i32.const 0) (local.get $len)))
            (array.copy $int_array $int_array
               (local.get $a) (i32.const 0)
               (global.get $method_cache) (i32.const 0)
               (array.len (global.get $method_cache)))
            (global.set $method_cache (local.get $a))))
      (local.set $ofs
         (array.get $int_array (global.get $method_cache) (local.get $cacheid)))
      (if (i32.eq (local.get $tag)
             (i31.get_s
                (ref.cast i31
                   (array.get $block (local.get $meths) (local.get $ofs)))))
         (then
            (return
               (array.get $block
                  (local.get $meths) (i32.sub (local.get $ofs) (i32.const 1))))))
      (local.set $li (i32.const 3))
      (local.set $hi
         (i32.add
            (i32.shl
               (i31.get_u
                  (ref.cast i31
                     (array.get $block (local.get $meths) (i32.const 1))))
               (i32.const 1))
            (i32.const 1)))
      (loop $loop
         (if (i32.lt_u (local.get $li) (local.get $hi))
            (then
               (local.set $mi
                  (i32.or (i32.shr_u (i32.add (local.get $li) (local.get $hi))
                                     (i32.const 1))
                          (i32.const 1)))
               (if (i32.lt_s
                      (local.get $tag)
                      (i31.get_s
                         (ref.cast i31
                            (array.get $block
                               (local.get $meths)
                               (i32.add (local.get $mi) (i32.const 1))))))
                  (then
                     (local.set $hi (i32.sub (local.get $mi) (i32.const 2))))
                  (else
                     (local.set $li (local.get $mi))))
               (br $loop))))
      (array.set $int_array (global.get $method_cache) (local.get $cacheid)
         (i32.add (local.get $li) (i32.const 1)))
      (if (result (ref eq))
          (i32.eq (local.get $tag)
             (i31.get_s
                (ref.cast i31
                   (array.get $block (local.get $meths)
                      (i32.add (local.get $li) (i32.const 1))))))
         (then
            (array.get $block (local.get $meths) (local.get $li)))
         (else
            (i31.new (i32.const 0)))))

   (global $caml_oo_last_id (mut i32) (i32.const 0))

   (func (export "caml_set_oo_id") (param (ref eq)) (result (ref eq))
      (local $id i32)
      (local.set $id (global.get $caml_oo_last_id))
      (array.set $block (ref.cast $block (local.get 0)) (i32.const 2)
         (i31.new (local.get $id)))
      (global.set $caml_oo_last_id (i32.add (local.get $id) (i32.const 1)))
      (local.get 0))

   (func (export "caml_fresh_oo_id") (param (ref eq)) (result (ref eq))
      (local $id i32)
      (local.set $id (global.get $caml_oo_last_id))
      (global.set $caml_oo_last_id (i32.add (local.get $id) (i32.const 1)))
      (i31.new (local.get $id)))

   (func (export "caml_obj_reachable_words") (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))
)
