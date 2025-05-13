;; Wasm_of_ocaml runtime support
;; http://www.ocsigen.org/js_of_ocaml/
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, with linking exception;
;; either version 2.1 of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

(module
   (import "bindings" "identity" (func $to_float (param anyref) (result f64)))
   (import "bindings" "identity" (func $from_float (param f64) (result anyref)))
   (import "bindings" "identity" (func $to_bool (param anyref) (result i32)))
   (import "bindings" "identity" (func $to_int32 (param anyref) (result i32)))
   (import "bindings" "identity" (func $from_int32 (param i32) (result anyref)))
   (import "bindings" "from_bool" (func $from_bool (param i32) (result anyref)))
   (import "bindings" "get"
      (func $get (param (ref extern)) (param anyref) (result anyref)))
   (import "bindings" "set"
      (func $set (param anyref) (param anyref) (param anyref)))
   (import "bindings" "delete" (func $delete (param anyref) (param anyref)))
   (import "bindings" "instanceof"
      (func $instanceof (param anyref) (param anyref) (result i32)))
   (import "bindings" "typeof" (func $typeof (param anyref) (result anyref)))
   (import "bindings" "equals"
      (func $equals (param anyref) (param anyref) (result i32)))
   (import "bindings" "strict_equals"
      (func $strict_equals (param anyref) (param anyref) (result i32)))
   (import "bindings" "fun_call"
      (func $fun_call
         (param anyref) (param anyref) (param anyref) (result anyref)))
   (import "bindings" "meth_call"
      (func $meth_call
         (param anyref) (param anyref) (param anyref) (result anyref)))
   (import "bindings" "new"
      (func $new (param anyref) (param anyref) (result anyref)))
   (import "bindings" "new_obj" (func $new_obj (result anyref)))
   (import "bindings" "new_array"
      (func $new_array (param i32) (result (ref extern))))
   (import "bindings" "global_this" (global $global_this anyref))
   (import "bindings" "iter_props"
      (func $iter_props (param anyref) (param anyref)))
   (import "bindings" "array_length"
      (func $array_length (param (ref extern)) (result i32)))
   (import "bindings" "array_get"
      (func $array_get (param (ref extern)) (param i32) (result anyref)))
   (import "bindings" "array_set"
      (func $array_set (param (ref extern)) (param i32) (param anyref)))
   (import "bindings" "wrap_callback"
      (func $wrap_callback (param (ref eq)) (result anyref)))
   (import "bindings" "wrap_callback_args"
      (func $wrap_callback_args (param (ref eq)) (result anyref)))
   (import "bindings" "wrap_callback_strict"
      (func $wrap_callback_strict (param i32) (param (ref eq)) (result anyref)))
   (import "bindings" "wrap_callback_unsafe"
      (func $wrap_callback_unsafe (param (ref eq)) (result anyref)))
   (import "bindings" "wrap_meth_callback"
      (func $wrap_meth_callback (param (ref eq)) (result anyref)))
   (import "bindings" "wrap_meth_callback_args"
      (func $wrap_meth_callback_args (param (ref eq)) (result anyref)))
   (import "bindings" "wrap_meth_callback_strict"
      (func $wrap_meth_callback_strict
         (param i32) (param (ref eq)) (result anyref)))
   (import "bindings" "wrap_meth_callback_unsafe"
      (func $wrap_meth_callback_unsafe (param (ref eq)) (result anyref)))
   (import "bindings" "wrap_fun_arguments"
      (func $wrap_fun_arguments (param anyref) (result anyref)))
   (import "fail" "caml_failwith_tag"
      (func $caml_failwith_tag (result (ref eq))))
   (import "fail" "javascript_exception"
      (tag $javascript_exception (param externref)))
   (import "stdlib" "caml_named_value"
      (func $caml_named_value (param (ref eq)) (result (ref null eq))))
   (import "obj" "caml_callback_1"
      (func $caml_callback_1
         (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "obj" "caml_is_closure"
      (func $caml_is_closure (param (ref eq)) (result i32)))
   (import "obj" "caml_is_last_arg"
      (func $caml_is_last_arg (param (ref eq)) (result i32)))
   (import "jsstring" "jsstring_of_bytes"
      (func $jsstring_of_bytes (param (ref $bytes)) (result anyref)))
   (import "jsstring" "bytes_of_jsstring"
      (func $bytes_of_jsstring (param anyref) (result (ref $bytes))))
   (import "jsstring" "jsstring_of_subbytes"
      (func $jsstring_of_subbytes (param (ref $bytes) i32 i32) (result anyref)))
   (import "int32" "caml_copy_int32"
      (func $caml_copy_int32 (param i32) (result (ref eq))))
   (import "int32" "Int32_val"
      (func $Int32_val (param (ref eq)) (result i32)))
   (import "int32" "caml_copy_nativeint"
      (func $caml_copy_nativeint (param i32) (result (ref eq))))
   (import "int32" "Nativeint_val"
      (func $Nativeint_val (param (ref eq)) (result i32)))

   (type $block (array (mut (ref eq))))
   (type $float (struct (field f64)))
   (type $float_array (array (mut f64)))
   (type $bytes (array (mut i8)))
   (type $js (struct (field anyref)))

   (func $wrap (export "wrap") (param anyref) (result (ref eq))
      (block $is_eq (result (ref eq))
         (return
            (struct.new $js (br_on_cast $is_eq anyref (ref eq) (local.get 0))))))

   (func $unwrap (export "unwrap") (param (ref eq)) (result anyref)
      (block $not_js (result anyref)
         (return
            (struct.get $js 0
               (br_on_cast_fail $not_js (ref eq) (ref $js) (local.get 0))))))

   (func (export "caml_js_equals")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (ref.i31 (call $equals
                  (call $unwrap (local.get 0)) (call $unwrap (local.get 1)))))

   (func (export "caml_js_strict_equals")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (ref.i31 (call $strict_equals
                  (call $unwrap (local.get 0)) (call $unwrap (local.get 1)))))

   (func (export "caml_js_global") (param (ref eq)) (result (ref eq))
      (call $wrap (global.get $global_this)))

   (func (export "caml_js_to_float") (param (ref eq)) (result (ref eq))
      (struct.new $float (call $to_float (call $unwrap (local.get 0)))))

   (func (export "caml_js_from_float") (param (ref eq)) (result (ref eq))
      (return_call $wrap
         (call $from_float
            (struct.get $float 0 (ref.cast (ref $float) (local.get 0))))))

   (func (export "caml_js_to_bool") (param (ref eq)) (result (ref eq))
      (ref.i31
         (call $to_bool (call $unwrap (local.get 0)))))

   (func (export "caml_js_from_bool") (param (ref eq)) (result (ref eq))
      (struct.new $js
         (call $from_bool (i31.get_s (ref.cast (ref i31) (local.get 0))))))

   (func (export "caml_js_to_int32") (param (ref eq)) (result (ref eq))
      (return_call $caml_copy_int32
         (call $to_int32 (call $unwrap (local.get 0)))))

   (func (export "caml_js_from_int32") (param (ref eq)) (result (ref eq))
      (return_call $wrap (call $from_int32 (call $Int32_val (local.get 0)))))

   (func (export "caml_js_to_nativeint") (param (ref eq)) (result (ref eq))
      (return_call $caml_copy_nativeint
         (call $to_int32 (call $unwrap (local.get 0)))))

   (func (export "caml_js_from_nativeint") (param (ref eq)) (result (ref eq))
      (return_call $wrap (call $from_int32 (call $Nativeint_val (local.get 0)))))

  (func (export "caml_js_pure_expr")
     (param $f (ref eq)) (result (ref eq))
     (return_call $caml_callback_1 (local.get $f) (ref.i31 (i32.const 0))))

   (func (export "caml_js_fun_call")
      (param $f (ref eq)) (param $args (ref eq)) (result (ref eq))
      (return_call $wrap
         (call $fun_call (call $unwrap (local.get $f)) (ref.null any)
            (call $unwrap (call $caml_js_from_array (local.get $args))))))

   (func (export "caml_js_call")
      (param $f (ref eq)) (param $o (ref eq)) (param $args (ref eq))
      (result (ref eq))
      (return_call $wrap
         (call $fun_call (call $unwrap (local.get $f))
            (call $unwrap (local.get $o))
            (call $unwrap (call $caml_js_from_array (local.get $args))))))

   (func (export "caml_js_meth_call")
      (param $o (ref eq)) (param $f (ref eq)) (param $args (ref eq))
      (result (ref eq))
      (if (ref.test (ref $bytes) (local.get $f))
         (then
            (local.set $f (call $caml_jsbytes_of_bytes (local.get $f)))))
      (return_call $wrap
         (call $meth_call (call $unwrap (local.get $o))
            (call $unwrap (local.get $f))
            (call $unwrap (call $caml_js_from_array (local.get $args))))))

   (func (export "caml_js_get")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (if (ref.test (ref $bytes) (local.get 1))
         (then
            (local.set 1 (call $caml_jsbytes_of_bytes (local.get 1)))))
      (return_call $wrap
         (call $get
            (ref.as_non_null (extern.convert_any (call $unwrap (local.get 0))))
            (call $unwrap (local.get 1)))))

   (func (export "caml_js_set")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
      (if (ref.test (ref $bytes) (local.get 1))
         (then
            (local.set 1 (call $caml_jsbytes_of_bytes (local.get 1)))))
      (call $set (call $unwrap (local.get 0)) (call $unwrap (local.get 1))
         (call $unwrap (local.get 2)))
      (ref.i31 (i32.const 0)))

   (func (export "caml_js_delete")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (if (ref.test (ref $bytes) (local.get 1))
         (then
            (local.set 1 (call $caml_jsbytes_of_bytes (local.get 1)))))
      (call $delete (call $unwrap (local.get 0)) (call $unwrap (local.get 1)))
      (ref.i31 (i32.const 0)))

   (func (export "caml_js_instanceof")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (ref.i31 (call $instanceof
                  (call $unwrap (local.get 0)) (call $unwrap (local.get 1)))))

   (func (export "caml_js_typeof")
      (param (ref eq)) (result (ref eq))
      (struct.new $js (call $typeof (call $unwrap (local.get 0)))))

   (func (export "caml_js_new")
      (param $c (ref eq)) (param $args (ref eq)) (result (ref eq))
      (return_call $wrap
         (call $new (call $unwrap (local.get $c))
            (call $unwrap (call $caml_js_from_array (local.get $args))))))

   (func (export "caml_ojs_new_arr")
      (param $c (ref eq)) (param $args (ref eq)) (result (ref eq))
      (return_call $wrap
         (call $new (call $unwrap (local.get $c))
            (call $unwrap (local.get $args)))))

   (func (export "caml_ojs_iterate_properties")
      (param $o (ref eq)) (param $f (ref eq)) (result (ref eq))
      (call $iter_props
         (call $unwrap (local.get $o)) (call $unwrap (local.get $f)))
      (ref.i31 (i32.const 0)))

   (func (export "caml_js_object")
      (param (ref eq)) (result (ref eq))
      (local $a (ref $block)) (local $p (ref $block))
      (local $i i32) (local $l i32)
      (local $o anyref)
      (local.set $a (ref.cast (ref $block) (local.get 0)))
      (local.set $l (array.len (local.get $a)))
      (local.set $i (i32.const 1))
      (local.set $o (call $new_obj))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $l))
            (then
               (local.set $p
                  (ref.cast (ref $block)
                     (array.get $block (local.get $a) (local.get $i))))
               (call $set (local.get $o)
                  (call $unwrap
                     (call $caml_jsstring_of_bytes
                        (array.get $block (local.get $p) (i32.const 1))))
                  (call $unwrap
                        (array.get $block (local.get $p) (i32.const 2))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (struct.new $js (local.get $o)))

   (func $caml_js_from_array (export "caml_js_from_array")
      (param $va (ref eq)) (result (ref eq))
      (local $a (ref $block))
      (local $fa (ref $float_array))
      (local $a' (ref extern))
      (local $i i32) (local $l i32)
      (drop (block $not_array (result (ref eq))
         (local.set $a
            (br_on_cast_fail $not_array (ref eq) (ref $block) (local.get $va)))
         (local.set $l (i32.sub (array.len (local.get $a)) (i32.const 1)))
         (local.set $a' (call $new_array (local.get $l)))
         (local.set $i (i32.const 0))
         (loop $loop
            (if (i32.lt_u (local.get $i) (local.get $l))
               (then
                  (call $array_set (local.get $a') (local.get $i)
                     (call $unwrap (array.get $block (local.get $a)
                                      (i32.add (local.get $i) (i32.const 1)))))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (br $loop))))
         (return (struct.new $js (any.convert_extern (local.get $a'))))))
     (local.set $fa (ref.cast (ref $float_array) (local.get $va)))
     (local.set $l (array.len (local.get $fa)))
     (local.set $a' (call $new_array (local.get $l)))
     (local.set $i (i32.const 0))
     (loop $loop
        (if (i32.lt_u (local.get $i) (local.get $l))
           (then
              (call $array_set (local.get $a') (local.get $i)
                 (struct.new $float
                    (array.get $float_array (local.get $fa) (local.get $i))))
              (local.set $i (i32.add (local.get $i) (i32.const 1)))
              (br $loop))))
     (struct.new $js (any.convert_extern (local.get $a'))))

   (func (export "caml_js_to_array")
      (param (ref eq)) (result (ref eq))
      (local $a (ref extern))
      (local $a' (ref $block))
      (local $fa (ref $float_array))
      (local $i i32) (local $l i32)
      (local.set $a
         (ref.as_non_null (extern.convert_any (call $unwrap (local.get 0)))))
      (local.set $l (call $array_length (local.get $a)))
      (if (local.get $l)
         (then
            (if (ref.test (ref $float)
                   (call $array_get (local.get $a) (i32.const 0)))
               (then
                  (local.set $fa
                     (array.new $float_array (f64.const 0) (local.get $l)))
                  (local.set $i (i32.const 0))
                  (loop $loop
                     (if (i32.lt_u (local.get $i) (local.get $l))
                        (then
                           (array.set $float_array (local.get $fa)
                              (local.get $i)
                              (struct.get $float 0
                                 (ref.cast (ref $float)
                                    (call $array_get
                                       (local.get $a) (local.get $i)))))
                           (local.set $i (i32.add (local.get $i) (i32.const 1)))
                           (br $loop))))
                  (return (local.get $fa))))))
      (local.set $a'
         (array.new $block (ref.i31 (i32.const 0))
            (i32.add (local.get $l) (i32.const 1))))
      (local.set $i (i32.const 0))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $l))
            (then
               (array.set $block (local.get $a')
                  (i32.add (local.get $i) (i32.const 1))
                  (call $wrap (call $array_get (local.get $a) (local.get $i))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (local.get $a'))

   (func (export "caml_js_to_string_array")
      (param $a (ref extern)) (result (ref eq))
      (local $a' (ref $block)) (local $l i32) (local $i i32)
      (local.set $l (call $array_length (local.get $a)))
      (local.set $a'
         (array.new $block (ref.i31 (i32.const 0))
            (i32.add (local.get $l) (i32.const 1))))
      (local.set $i (i32.const 0))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $l))
            (then
               (array.set $block (local.get $a')
                  (i32.add (local.get $i) (i32.const 1))
                  (call $caml_string_of_jsstring
                     (call $wrap
                        (call $array_get (local.get $a) (local.get $i)))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (local.get $a'))

   (func $caml_js_wrap_callback (export "caml_js_wrap_callback")
      (param (ref eq)) (result (ref eq))
      (return_call $wrap (call $wrap_callback (local.get 0))))

   (func (export "caml_js_wrap_callback_arguments")
      (param (ref eq)) (result (ref eq))
      (return_call $wrap (call $wrap_callback_args (local.get 0))))

   (func (export "caml_js_wrap_callback_strict")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (return_call $wrap
         (call $wrap_callback_strict
            (i31.get_u (ref.cast (ref i31) (local.get 0))) (local.get 1))))

   (func (export "caml_js_wrap_callback_unsafe")
      (param (ref eq)) (result (ref eq))
      (return_call $wrap (call $wrap_callback_unsafe (local.get 0))))

   (func (export "caml_js_wrap_meth_callback")
      (param (ref eq)) (result (ref eq))
      (return_call $wrap (call $wrap_meth_callback (local.get 0))))

   (func (export "caml_js_wrap_meth_callback_arguments")
      (param (ref eq)) (result (ref eq))
      (return_call $wrap (call $wrap_meth_callback_args (local.get 0))))

   (func (export "caml_js_wrap_meth_callback_strict")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (return_call $wrap
         (call $wrap_meth_callback_strict
            (i31.get_u (ref.cast (ref i31) (local.get 0))) (local.get 1))))

   (func (export "caml_js_wrap_meth_callback_unsafe")
      (param (ref eq)) (result (ref eq))
      (return_call $wrap (call $wrap_meth_callback_unsafe (local.get 0))))

   (func (export "caml_ojs_wrap_fun_arguments")
      (param (ref eq)) (result (ref eq))
      (return_call $wrap
         (call $wrap_fun_arguments
            (call $wrap_callback_strict (i32.const 1) (local.get 0)))))

   (func (export "caml_callback")
      (param $f (ref eq)) (param $count i32) (param $args (ref extern))
      (param $kind i32) ;; 0 ==> strict / 2 ==> unsafe
      (result anyref)
      (local $acc (ref eq)) (local $i i32) (local $arg (ref eq))
      (local.set $acc (local.get $f))
      (if (i32.eq (local.get $kind) (i32.const 2))
         (then
            (loop $loop
               (local.set $f (local.get $acc))
               (local.set $acc
                  (call $caml_callback_1 (local.get $acc)
                     (call $wrap
                        (call $get (local.get $args)
                           (ref.i31 (local.get $i))))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br_if $loop
                  (i32.eqz (call $caml_is_last_arg (local.get $f))))))
         (else
            (local.set $i (i32.const 0))
            (block $done
               (loop $loop
                  (if (i32.lt_u (local.get $i) (local.get $count))
                     (then
                        (br_if $done
                           (i32.eqz (call $caml_is_closure (local.get $acc))))
                        (local.set $acc
                           (call $caml_callback_1 (local.get $acc)
                              (call $wrap
                                 (call $get (local.get $args)
                                    (ref.i31 (local.get $i))))))
                        (local.set $i (i32.add (local.get $i) (i32.const 1)))
                        (br $loop)))))
            (if (local.get $kind)
               (then
                  (if (call $caml_is_closure (local.get $acc))
                     (then (local.set $acc
                              (call $caml_js_wrap_callback
                                 (local.get $acc)))))))))
      (return_call $unwrap (local.get $acc)))

   (export "caml_js_from_string" (func $caml_jsstring_of_bytes))
   (func $caml_jsstring_of_bytes (export "caml_jsstring_of_string")
      (param (ref eq)) (result (ref eq))
      (local $s (ref $bytes))
      (local.set $s (ref.cast (ref $bytes) (local.get 0)))
      (return (struct.new $js (call $jsstring_of_bytes (local.get $s)))))

   (func (export "caml_jsstring_of_substring")
      (param $s (ref eq)) (param $i (ref eq)) (param $l (ref eq))
      (result (ref eq))
      (return
         (struct.new $js
            (call $jsstring_of_subbytes
               (ref.cast (ref $bytes) (local.get $s))
               (i31.get_u (ref.cast (ref i31) (local.get $i)))
               (i31.get_u (ref.cast (ref i31) (local.get $l)))))))

   (func $caml_jsbytes_of_bytes (export "caml_jsbytes_of_string")
      (param (ref eq)) (result (ref eq))
      (local $s (ref $bytes))
      (local $s' (ref $bytes))
      (local $l i32) (local $i i32) (local $n i32) (local $c i32)
      (local.set $s (ref.cast (ref $bytes) (local.get 0)))
      (local.set $l (array.len (local.get $s)))
      (local.set $i (i32.const 0))
      (local.set $n (i32.const 0))
      (loop $count
         (if (i32.lt_u (local.get $i) (local.get $l))
            (then
               (if (i32.ge_u (array.get_u $bytes (local.get $s) (local.get $i))
                      (i32.const 128))
                  (then (local.set $n (i32.add (local.get $n) (i32.const 1)))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $count))))
      (if (i32.eqz (local.get $n))
         (then
            (return
               (struct.new $js
                  (call $jsstring_of_bytes (local.get $s))))))
      (local.set $s'
         (array.new $bytes (i32.const 0)
            (i32.add (local.get $i) (local.get $n))))
      (local.set $i (i32.const 0))
      (local.set $n (i32.const 0))
      (loop $fill
         (if (i32.lt_u (local.get $i) (local.get $l))
            (then
               (local.set $c (array.get_u $bytes (local.get $s) (local.get $i)))
               (if (i32.lt_u (local.get $c) (i32.const 128))
                  (then
                     (array.set $bytes
                        (local.get $s') (local.get $n) (local.get $c))
                     (local.set $n (i32.add (local.get $n) (i32.const 1))))
                  (else
                     (array.set $bytes (local.get $s')
                        (local.get $n)
                        (i32.or (i32.shr_u (local.get $c) (i32.const 6))
                           (i32.const 0xC0)))
                     (array.set $bytes (local.get $s')
                        (i32.add (local.get $n) (i32.const 1))
                        (i32.or (i32.const 0x80)
                           (i32.and (local.get $c) (i32.const 0x3F))))
                     (local.set $n (i32.add (local.get $n) (i32.const 2)))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $fill))))
      (return (struct.new $js (call $jsstring_of_bytes (local.get $s')))))

   (export "caml_js_to_string" (func $caml_string_of_jsstring))
   (func $caml_string_of_jsstring (export "caml_string_of_jsstring")
      (param $s (ref eq)) (result (ref eq))
      (return_call $bytes_of_jsstring
         (struct.get $js 0 (ref.cast (ref $js) (local.get $s)))))

   (func (export "caml_string_of_jsbytes")
      (param $s (ref eq)) (result (ref eq))
      (local $l i32) (local $i i32) (local $n i32) (local $c i32)
      (local $s' (ref $bytes)) (local $s'' (ref $bytes))
      (local.set $s'
         (call $bytes_of_jsstring
            (struct.get $js 0 (ref.cast (ref $js) (local.get $s)))))
      (local.set $l (array.len (local.get $s')))
      (local.set $i (i32.const 0))
      (local.set $n (i32.const 0))
      (loop $count
         (if (i32.lt_u (local.get $i) (local.get $l))
            (then
               (if (i32.ge_u (array.get_u $bytes (local.get $s') (local.get $i))
                      (i32.const 0xC0))
                  (then (local.set $n (i32.add (local.get $n) (i32.const 1)))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $count))))
      (if (i32.eqz (local.get $n)) (then (return (local.get $s'))))
      (local.set $s''
         (array.new $bytes (i32.const 0)
            (i32.sub (local.get $i) (local.get $n))))
      (local.set $i (i32.const 0))
      (local.set $n (i32.const 0))
      (loop $fill
         (if (i32.lt_u (local.get $i) (local.get $l))
            (then
               (local.set $c
                  (array.get_u $bytes (local.get $s') (local.get $i)))
               (if (i32.lt_u (local.get $c) (i32.const 0xC0))
                  (then
                     (array.set $bytes
                        (local.get $s'') (local.get $n) (local.get $c))
                     (local.set $i (i32.add (local.get $i) (i32.const 1))))
                  (else
                     (array.set $bytes (local.get $s'')
                        (local.get $n)
                        (i32.sub
                           (i32.add
                              (i32.shl (local.get $c) (i32.const 6))
                              (array.get_u $bytes (local.get $s')
                                 (i32.add (local.get $i) (i32.const 1))))
                           (i32.const 0x3080)))
                     (local.set $i (i32.add (local.get $i) (i32.const 2)))))
               (local.set $n (i32.add (local.get $n) (i32.const 1)))
               (br $fill))))
      (local.get $s''))

   (func (export "caml_list_to_js_array")
      (param (ref eq)) (result (ref eq))
      (local $i i32)
      (local $a (ref extern))
      (local $l (ref eq))
      (local $b (ref $block))
      (local.set $i (i32.const 0))
      (local.set $l (local.get 0))
      (drop (block $done (result (ref eq))
         (loop $compute_length
            (local.set $l
               (array.get $block
                  (br_on_cast_fail $done (ref eq) (ref $block) (local.get $l))
                  (i32.const 2)))
            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (br $compute_length))))
      (local.set $a (call $new_array (local.get $i)))
      (local.set $i (i32.const 0))
      (local.set $l (local.get 0))
      (drop (block $exit (result (ref eq))
         (loop $loop
            (local.set $b
               (br_on_cast_fail $exit (ref eq) (ref $block) (local.get $l)))
            (call $array_set (local.get $a) (local.get $i)
               (call $unwrap (array.get $block (local.get $b) (i32.const 1))))
            (local.set $l (array.get $block (local.get $b) (i32.const 2)))
            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (br $loop))))
      (struct.new $js (any.convert_extern (local.get $a))))

   (func (export "caml_list_of_js_array")
      (param (ref eq)) (result (ref eq))
      (local $l (ref eq))
      (local $i i32)
      (local $len i32)
      (local $a (ref extern))
      (local.set $a
         (ref.as_non_null (extern.convert_any (call $unwrap (local.get 0)))))
      (local.set $len (call $array_length (local.get $a)))
      (local.set $i (i32.const 0))
      (local.set $l (ref.i31 (i32.const 0)))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $len))
            (then
               (local.set $l
                  (array.new_fixed $block 3 (ref.i31 (i32.const 0))
                     (call $wrap
                        (call $array_get (local.get $a) (local.get $i)))
                     (local.get $l)))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (local.get $l))

   (@string $jsError "jsError")

   (@string $toString "toString")

   (func (export "caml_wrap_exception") (param externref) (result (ref eq))
      (local $exn anyref)
      (local.set $exn (any.convert_extern (local.get 0)))
      ;; ZZZ special case for stack overflows?
      (block $undef
         (return
            (array.new_fixed $block 3 (ref.i31 (i32.const 0))
               (br_on_null $undef
                  (call $caml_named_value (global.get $jsError)))
               (call $wrap (local.get $exn)))))
      (array.new_fixed $block 3 (ref.i31 (i32.const 0))
         (call $caml_failwith_tag)
         (call $caml_string_of_jsstring
            (call $wrap
               (call $meth_call
                  (local.get $exn)
                  (call $unwrap
                     (call $caml_jsstring_of_bytes (global.get $toString)))
                  (any.convert_extern (call $new_array (i32.const 0))))))))

   (func (export "caml_js_error_option_of_exception")
      (param (ref eq)) (result (ref eq))
      (local $exn (ref $block))
      (local.set $exn (ref.cast (ref $block) (local.get 0)))
      (if (ref.eq (array.get $block (local.get $exn) (i32.const 0))
                  (ref.i31 (i32.const 0)))
         (then
            (if (ref.eq (array.get $block (local.get $exn) (i32.const 1))
                   (call $caml_named_value (global.get $jsError)))
               (then
                  (return
                     (array.new_fixed $block 2 (ref.i31 (i32.const 0))
                        (array.get $block (local.get $exn) (i32.const 2))))))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_throw_js_exception")
      (param $exn (ref eq)) (result (ref eq))
      (throw $javascript_exception
         (extern.convert_any (call $unwrap (local.get $exn)))))

   (func (export "caml_js_error_of_exception")
      (param (ref eq)) (result (ref eq))
      (local $exn (ref $block))
      (local.set $exn (ref.cast (ref $block) (local.get 0)))
      (if (ref.eq (array.get $block (local.get $exn) (i32.const 0))
                  (ref.i31 (i32.const 0)))
         (then
            (if (ref.eq (array.get $block (local.get $exn) (i32.const 1))
                   (call $caml_named_value (global.get $jsError)))
               (then
                  (return
                     (array.get $block (local.get $exn) (i32.const 2)))))))
      (call $wrap (ref.null any)))

   (func (export "caml_jsoo_flags_use_js_string")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))
)
