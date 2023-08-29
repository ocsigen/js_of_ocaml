(module
   (import "stdlib" "caml_global_data"
      (global $caml_global_data (mut (ref $block))))
   (import "bindings" "jstag" (tag $javascript_exception (param externref)))

   (type $block (array (mut (ref eq))))
   (type $string (array (mut i8)))

   (tag $ocaml_exception (export "ocaml_exception") (param (ref eq)))
   (export "javascript_exception" (tag $javascript_exception))

   (func $caml_raise_constant (export "caml_raise_constant") (param (ref eq))
      (throw $ocaml_exception (local.get 0)))

   (func $caml_raise_with_arg (export "caml_raise_with_arg")
      (param $tag (ref eq)) (param $arg (ref eq))
      (throw $ocaml_exception
         (array.new_fixed $block 3
            (i31.new (i32.const 0)) (local.get $tag) (local.get $arg))))

   (global $OUT_OF_MEMORY_EXN i32 (i32.const 0))

   (func (export "caml_raise_out_of_memory")
      (return_call $caml_raise_constant
         (array.get $block (global.get $caml_global_data)
                    (global.get $OUT_OF_MEMORY_EXN))))

   (global $SYS_ERROR_EXN i32 (i32.const 1))

   (func (export "caml_raise_sys_error") (param $msg (ref $string))
       (return_call $caml_raise_with_arg
           (array.get $block (global.get $caml_global_data)
              (global.get $SYS_ERROR_EXN))
           (local.get 0)))

   (global $FAILURE_EXN i32 (i32.const 2))

   (func (export "caml_failwith_tag") (result (ref eq))
       (array.get $block (global.get $caml_global_data)
          (global.get $FAILURE_EXN)))

   (func (export "caml_failwith") (param $arg (ref eq))
       (return_call $caml_raise_with_arg
           (array.get $block (global.get $caml_global_data)
              (global.get $FAILURE_EXN))
           (local.get 0)))

   (global $INVALID_EXN i32 (i32.const 3))

   (func $caml_invalid_argument (export "caml_invalid_argument")
       (param $arg (ref eq))
       (return_call $caml_raise_with_arg
           (array.get $block (global.get $caml_global_data)
              (global.get $INVALID_EXN))
           (local.get 0)))

   (data $index_out_of_bounds "index out of bounds")

   (func (export "caml_bound_error")
      (return_call $caml_invalid_argument
         (array.new_data $string $index_out_of_bounds
            (i32.const 0) (i32.const 19))))

   (global $ZERO_DIVIDE_EXN i32 (i32.const 5))

   (func (export "caml_raise_zero_divide")
      (return_call $caml_raise_constant
         (array.get $block (global.get $caml_global_data)
                    (global.get $ZERO_DIVIDE_EXN))))

   (global $NOT_FOUND_EXN i32 (i32.const 6))

   (func (export "caml_raise_not_found")
      (return_call $caml_raise_constant
         (array.get $block (global.get $caml_global_data)
                    (global.get $NOT_FOUND_EXN))))

   (global $MATCH_FAILURE_EXN i32 (i32.const 7))
   (global $ASSERT_FAILURE_EXN i32 (i32.const 10))
   (global $UNDEFINED_RECURSIVE_MODULE_EXN i32 (i32.const 11))

   (func (export "caml_is_special_exception") (param (ref eq)) (result i32)
      (i32.or
         (ref.eq (local.get 0)
            (array.get $block (global.get $caml_global_data)
               (global.get $MATCH_FAILURE_EXN)))
         (i32.or
            (ref.eq (local.get 0)
               (array.get $block (global.get $caml_global_data)
                  (global.get $ASSERT_FAILURE_EXN)))
            (ref.eq (local.get 0)
               (array.get $block (global.get $caml_global_data)
                  (global.get $UNDEFINED_RECURSIVE_MODULE_EXN))))))
)
