(module
   (import "hash" "caml_string_hash"
      (func $caml_string_hash
         (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "string" "caml_string_equal"
      (func $caml_string_equal
         (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "jslib" "caml_string_of_jsstring"
      (func $caml_string_of_jsstring (param (ref eq)) (result (ref eq))))
   (import "jslib" "caml_jsstring_of_string"
      (func $caml_jsstring_of_string (param (ref eq)) (result (ref eq))))
   (import "jslib" "wrap" (func $wrap (param anyref) (result (ref eq))))
   (import "jslib" "unwrap" (func $unwrap (param (ref eq)) (result anyref)))
   (import "obj" "caml_callback_1"
      (func $caml_callback_1
         (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "bindings" "write" (func $write (param i32) (param anyref)))
   (import "string" "caml_string_cat"
      (func $caml_string_cat
         (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "printexc" "caml_format_exception"
      (func $caml_format_exception (param (ref eq)) (result (ref eq))))
   (import "sys" "ocaml_exit" (tag $ocaml_exit (param i32)))
   (import "fail" "ocaml_exception" (tag $ocaml_exception (param (ref eq))))
   (import "bindings" "exit" (func $exit (param i32)))

   (type $block (array (mut (ref eq))))
   (type $string (array (mut i8)))

   (type $assoc
      (struct
         (field (ref $string))
         (field (ref eq))
         (field (mut (ref null $assoc)))))

   (type $assoc_array (array (field (mut (ref null $assoc)))))

   (global $Named_value_size i32 (i32.const 13))

   (global $named_value_table (ref $assoc_array)
     (array.new $assoc_array (ref.null $assoc) (global.get $Named_value_size)))

   (func $find_named_value
      (param $s (ref eq)) (param $l (ref null $assoc)) (result (ref null eq))
      (local $a (ref $assoc))
      (block $tail (result (ref null eq))
         (loop $loop
            (local.set $a
               (br_on_cast_fail $tail (ref null eq) (ref $assoc) (local.get $l)))
            (if (i31.get_u
                   (ref.cast (ref i31)
                       (call $caml_string_equal
                          (local.get $s)
                          (struct.get $assoc 0 (local.get $a)))))
               (then
                  (return (struct.get $assoc 1 (local.get $a)))))
            (local.set $l (struct.get $assoc 2 (local.get $a)))
            (br $loop))))

   (func $caml_named_value (export "caml_named_value")
      (param $s (ref $string)) (result (ref null eq))
      (return_call $find_named_value
         (local.get $s)
         (array.get $assoc_array (global.get $named_value_table)
            (i32.rem_u
               (i31.get_s
                  (ref.cast (ref i31)
                     (call $caml_string_hash
                        (ref.i31 (i32.const 0)) (local.get $s))))
               (global.get $Named_value_size)))))

   (func (export "caml_register_named_value")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (local $h i32)
      (local $r (ref null $assoc))
      (local.set $h
         (i32.rem_u
            (i31.get_s
               (ref.cast (ref i31)
                  (call $caml_string_hash
                     (ref.i31 (i32.const 0)) (local.get 0))))
            (global.get $Named_value_size)))
      (local.set $r
         (array.get $assoc_array
            (global.get $named_value_table) (local.get $h)))
      (if (ref.is_null (call $find_named_value (local.get 0) (local.get $r)))
         (then
            (array.set $assoc_array
               (global.get $named_value_table) (local.get $h)
               (struct.new $assoc
                  (ref.cast (ref $string) (local.get 0))
                  (local.get 1) (local.get $r)))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_unregister_named_value")
      (param $name (ref eq)) (result (ref eq))
      (local $h i32)
      (local $r (ref null $assoc)) (local $a (ref $assoc))
      (local.set $h
         (i32.rem_u
            (i31.get_s
               (ref.cast (ref i31)
                  (call $caml_string_hash
                     (ref.i31 (i32.const 0)) (local.get $name))))
            (global.get $Named_value_size)))
      (local.set $r
         (array.get $assoc_array
            (global.get $named_value_table) (local.get $h)))
      (block $done
         (local.set $a (br_on_null $done (local.get $r)))
         (local.set $r (struct.get $assoc 2 (local.get $a)))
         (if (i31.get_u
                (ref.cast (ref i31)
                    (call $caml_string_equal
                       (local.get $name)
                       (struct.get $assoc 0 (local.get $a)))))
            (then
               (array.set $assoc_array
                  (global.get $named_value_table) (local.get $h)
                  (local.get $r))
               (br $done)))
         (loop $loop
            (local.set $a (br_on_null $done (local.get $r)))
            (if (i31.get_u
                   (ref.cast (ref i31)
                       (call $caml_string_equal
                          (local.get $name)
                          (struct.get $assoc 0 (local.get $a)))))
               (then
                  (struct.set $assoc 2 (local.get $r)
                     (struct.get $assoc 2 (local.get $a)))
                  (br $done)))
            (local.set $r (struct.get $assoc 2 (local.get $a)))
            (br $loop)))
      (ref.i31 (i32.const 0)))

   (global $caml_global_data (export "caml_global_data") (mut (ref $block))
      (array.new $block (ref.i31 (i32.const 0)) (i32.const 12)))

   (func (export "caml_register_global")
      (param (ref eq)) (param $v (ref eq)) (param (ref eq)) (result (ref eq))
      (local $i i32)
      (local.set $i (i31.get_u (ref.cast (ref i31) (local.get 0))))
      (if (i32.lt_u (local.get $i) (array.len (global.get $caml_global_data)))
         (then
            (array.set $block (global.get $caml_global_data)
               (local.get $i) (local.get $v))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_get_global_data") (param (ref eq)) (result (ref eq))
      (global.get $caml_global_data))

   (type $func (func (result (ref eq))))

   (data $fatal_error "Fatal error: exception ")
   (data $handle_uncaught_exception "Printexc.handle_uncaught_exception")
   (data $do_at_exit "Pervasives.do_at_exit")

   (global $uncaught_exception (mut externref) (ref.null extern))

   (import "bindings" "throw" (func $throw (param externref)))

   (func $reraise_exception (result (ref eq))
      (call $throw (global.get $uncaught_exception))
      (ref.i31 (i32.const 0)))

   (func (export "caml_handle_uncaught_exception") (param $exn externref)
      (global.set $uncaught_exception (local.get $exn))
      (call $caml_main (ref.func $reraise_exception)))

   (func $caml_main (export "caml_main") (param $start (ref func))
      (local $exn (ref eq))
      (try
         (do
            (drop (call_ref $func (ref.cast (ref $func) (local.get $start)))))
         (catch $ocaml_exit
            (call $exit (pop i32)))
         (catch $ocaml_exception
            (local.set $exn (pop (ref eq)))
            (block $exit
               (block $not_registered
                  (drop
                     (call $caml_callback_1
                        (call $caml_callback_1
                           (br_on_null $not_registered
                              (call $caml_named_value
                                  (array.new_data $string
                                     $handle_uncaught_exception
                                     (i32.const 0) (i32.const 34))))
                           (local.get $exn))
                     (ref.i31 (i32.const 0))))
                  (br $exit))
               (block $null
                  (drop
                     (call $caml_callback_1
                        (br_on_null $null
                           (call $caml_named_value
                              (array.new_data $string $do_at_exit
                                 (i32.const 0) (i32.const 21))))
                        (ref.i31 (i32.const 0)))))
               (call $write (i32.const 2)
                  (call $unwrap
                     (call $caml_jsstring_of_string
                        (call $caml_string_cat
                           (array.new_data $string $fatal_error
                              (i32.const 0) (i32.const 23))
                           (call $caml_string_cat
                              (call $caml_format_exception (local.get $exn))
                              (array.new_fixed $string 1
                                 (i32.const 10)))))))) ;; `\n`
               (call $exit (i32.const 2)))))
)
