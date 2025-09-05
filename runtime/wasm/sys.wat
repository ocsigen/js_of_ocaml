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
   (import "bindings" "ta_length"
      (func $ta_length (param (ref extern)) (result i32)))
   (import "bindings" "ta_get_i32"
      (func $ta_get_i32 (param (ref extern)) (param i32) (result i32)))
   (import "bindings" "random_seed" (func $random_seed (result (ref extern))))
   (import "jslib" "unwrap" (func $unwrap (param (ref eq)) (result anyref)))
   (import "jslib" "wrap" (func $wrap (param anyref) (result (ref eq))))
   (import "jslib" "caml_jsstring_of_string"
      (func $caml_jsstring_of_string (param (ref eq)) (result (ref eq))))
   (import "jslib" "caml_string_of_jsstring"
      (func $caml_string_of_jsstring (param (ref eq)) (result (ref eq))))
   (import "jslib" "caml_js_to_string_array"
      (func $caml_js_to_string_array (param $a (ref extern)) (result (ref eq))))
   (import "jslib" "caml_js_meth_call"
      (func $caml_js_meth_call
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "fail" "caml_raise_sys_error"
      (func $caml_raise_sys_error (param (ref eq))))
   (import "fail" "caml_raise_not_found" (func $caml_raise_not_found))
   (import "bindings" "argv" (func $argv (result (ref extern))))
   (import "bindings" "on_windows" (global $on_windows i32))
   (import "bindings" "isatty"
      (func $isatty (param (ref eq)) (result (ref eq))))
   (import "bindings" "system" (func $system (param anyref) (result (ref eq))))
   (import "bindings" "getenv" (func $getenv (param anyref) (result anyref)))
   (import "bindings" "time" (func $time (result f64)))
   (import "bindings" "array_length"
      (func $array_length (param (ref extern)) (result i32)))
   (import "bindings" "array_get"
      (func $array_get (param (ref extern)) (param i32) (result anyref)))
   (import "fail" "javascript_exception"
      (tag $javascript_exception (param externref)))
   (import "jsstring" "jsstring_test"
      (func $jsstring_test (param anyref) (result i32)))
   (import "bindings" "exit" (func $exit (param (ref eq))))
   (import "io" "caml_channel_descriptor"
      (func $caml_channel_descriptor (param (ref eq)) (result (ref eq))))

   (type $block (array (mut (ref eq))))
   (type $bytes (array (mut i8)))
   (type $float (struct (field f64)))

   (tag $ocaml_exit (export "ocaml_exit"))

   (func (export "caml_sys_exit") (export "unix_exit") (export "caml_unix_exit")
      (param $code (ref eq)) (result (ref eq))
      (call $exit (local.get $code))
      ;; Fallback: try to exit through an exception
      (throw $ocaml_exit))

   (export "caml_sys_unsafe_getenv" (func $caml_sys_getenv))
   (func $caml_sys_getenv (export "caml_sys_getenv")
      (param (ref eq)) (result (ref eq))
      (local $res anyref)
      (local.set $res
         (call $getenv
            (call $unwrap (call $caml_jsstring_of_string (local.get 0)))))
      (if (i32.eqz (call $jsstring_test (local.get $res)))
         (then
            (call $caml_raise_not_found)))
      (return_call $caml_string_of_jsstring (call $wrap (local.get $res))))

   (func (export "caml_sys_argv") (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $caml_js_to_string_array (call $argv)))

   (func (export "caml_sys_executable_name")
      (param (ref eq)) (result (ref eq))
      (array.get $block
         (ref.cast (ref $block) (call $caml_js_to_string_array (call $argv)))
         (i32.const 1)))

   (export "caml_sys_time_include_children" (func $caml_sys_time))
   (func $caml_sys_time (export "caml_sys_time")
      (param (ref eq)) (result (ref eq))
      (struct.new $float (f64.mul (call $time) (f64.const 0.001))))

   (func (export "caml_sys_system_command")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (try
         (do
            (return
               (call $system
                  (call $unwrap (call $caml_jsstring_of_string (local.get 0))))))
         (catch $javascript_exception
            (call $caml_handle_sys_error (pop externref))
            (return (ref.i31 (i32.const 0))))))

   (func (export "caml_sys_random_seed")
      (param (ref eq)) (result (ref eq))
      (local $r (ref extern))
      (local $a (ref $block))
      (local $i i32) (local $n i32)
      (local.set $r (call $random_seed))
      (local.set $n (call $ta_length (local.get $r)))
      (local.set $a
         (array.new $block (ref.i31 (i32.const 0))
            (i32.add (local.get $n) (i32.const 1))))
      (local.set $i (i32.const 0))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $n))
            (then
               (array.set $block
                  (local.get $a) (i32.add (local.get $i) (i32.const 1))
                  (ref.i31 (call $ta_get_i32 (local.get $r) (local.get $i))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (local.get $a))

   (func (export "caml_sys_const_bigendian")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_sys_const_word_size")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 32)))

   (func (export "caml_sys_const_int_size")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 31)))

   (func (export "caml_sys_const_max_wosize")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0xfffffff)))

   (func (export "caml_sys_const_ostype_unix")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.eqz (global.get $on_windows))))

   (func (export "caml_sys_const_ostype_win32")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (global.get $on_windows)))

   (func (export "caml_sys_const_ostype_cygwin")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (@string $Unix "Unix")
   (@string $Win32 "Win32")

   (func (export "caml_sys_get_config")
      (param (ref eq)) (result (ref eq))
      (array.new_fixed $block 4 (ref.i31 (i32.const 0))
         (select (result (ref eq)) (global.get $Win32) (global.get $Unix)
            (global.get $on_windows))
         (ref.i31 (i32.const 32))
         (ref.i31 (i32.const 0))))

   (func (export "caml_sys_isatty")
      (param $ch (ref eq)) (result (ref eq))
      (return_call $isatty (call $caml_channel_descriptor (local.get $ch))))

   (func (export "caml_runtime_variant") (param (ref eq)) (result (ref eq))
      (@string ""))

   (func (export "caml_runtime_parameters") (param (ref eq)) (result (ref eq))
      (@string ""))

   (func (export "caml_install_signal_handler")
      (param (ref eq) (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (global $caml_runtime_warnings (mut i32) (i32.const 0))

   (func (export "caml_ml_enable_runtime_warnings")
      (param (ref eq)) (result (ref eq))
      (global.set $caml_runtime_warnings
         (i31.get_u (ref.cast (ref i31) (local.get 0))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_ml_runtime_warnings_enabled")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (global.get $caml_runtime_warnings)))

   (@string $toString "toString")

   (func $caml_handle_sys_error (export "caml_handle_sys_error")
      (param $exn externref)
      (call $caml_raise_sys_error
         (call $caml_string_of_jsstring
            (call $caml_js_meth_call
               (call $wrap (any.convert_extern (local.get $exn)))
               (global.get $toString)
               (array.new_fixed $block 1 (ref.i31 (i32.const 0)))))))
)
