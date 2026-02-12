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
   (import "fail" "caml_raise_sys_error"
      (func $caml_raise_sys_error (param (ref eq))))
   (import "fail" "caml_raise_not_found" (func $caml_raise_not_found))
(@if wasi
(@then
   (import "wasi_snapshot_preview1" "random_get"
      (func $random_get (param i32 i32) (result i32)))
   (import "wasi_snapshot_preview1" "clock_time_get"
      (func $clock_time_get (param i32 i64 i32) (result i32)))
   (import "wasi_snapshot_preview1" "args_get"
      (func $args_get (param i32 i32) (result i32)))
   (import "wasi_snapshot_preview1" "args_sizes_get"
      (func $args_sizes_get (param i32 i32) (result i32)))
   (import "wasi_snapshot_preview1" "environ_get"
      (func $environ_get (param i32 i32) (result i32)))
   (import "wasi_snapshot_preview1" "environ_sizes_get"
      (func $environ_sizes_get (param i32 i32) (result i32)))
   (import "wasi_snapshot_preview1" "proc_exit" (func $exit (param i32)))
   (import "libc" "memory" (memory 2))
   (import "libc" "free" (func $free (param i32)))
   (import "libc" "strlen" (func $strlen (param i32) (result i32)))
   (import "wasi_memory" "checked_malloc"
      (func $checked_malloc (param i32) (result i32)))
   (import "wasi_memory" "get_buffer" (func $get_buffer (result i32)))
   (import "wasi_memory" "blit_memory_to_string"
      (func $blit_memory_to_string (param i32 i32) (result (ref $bytes))))
   (import "wasi_errors" "error_messages" (global $error_messages (ref $block)))
   (import "string" "caml_string_concat"
      (func $caml_string_concat
         (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "fail" "caml_invalid_argument"
      (func $caml_invalid_argument (param (ref eq))))
)
(@else
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
   (import "bindings" "argv" (func $argv (result (ref extern))))
   (import "bindings" "on_windows" (global $on_windows i32))
   (import "bindings" "isatty"
      (func $isatty (param (ref eq)) (result (ref eq))))
   (import "bindings" "system" (func $system (param anyref) (result (ref eq))))
   (import "bindings" "getenv" (func $getenv (param anyref) (result anyref)))
   (import "bindings" "time" (func $time (result f64)))
   (import "fail" "javascript_exception"
      (tag $javascript_exception (param externref)))
   (import "jsstring" "jsstring_test"
      (func $jsstring_test (param anyref) (result i32)))
   (import "bindings" "exit" (func $exit (param i32)))
))
   (import "io" "caml_channel_descriptor"
      (func $caml_channel_descriptor (param (ref eq)) (result (ref eq))))

   (type $block (array (mut (ref eq))))
   (type $bytes (array (mut i8)))
   (type $float (struct (field f64)))

   (tag $ocaml_exit (export "ocaml_exit"))

   (func (export "caml_sys_exit") (export "unix_exit") (export "caml_unix_exit")
      (param $code (ref eq)) (result (ref eq))
      (call $exit (i31.get_s (ref.cast (ref i31) (local.get $code))))
      ;; Fallback: try to exit through an exception
      (throw $ocaml_exit))

(@if wasi
(@then
   (global $environment (mut i32) (i32.const 0))
   (global $environment_count (mut i32) (i32.const 0))
   (global $environment_data (mut i32) (i32.const 0))

   (func $initialize_env
      (local $buffer i32) (local $res i32) (local $env i32) (local $data i32)
      (if (i32.eqz (global.get $environment))
         (then
            (local.set $buffer (call $get_buffer))
            (local.set $res
               (call $environ_sizes_get
                  (local.get $buffer)
                  (i32.add (local.get $buffer) (i32.const 4))))
            (if (local.get $res)
               (then
                  (call $caml_handle_sys_error
                     (ref.i31 (i32.const 0)) (local.get $res))))
            (local.set $env
               (call $checked_malloc
                  (i32.shl (i32.load (local.get $buffer)) (i32.const 2))))
            (local.set $data
               (call $checked_malloc (i32.load offset=4 (local.get $buffer))))
            (local.set $res
               (call $environ_get (local.get $env) (local.get $data)))
            (if (local.get $res)
               (then
                  (call $caml_handle_sys_error
                     (ref.i31 (i32.const 0)) (local.get $res))))
            (global.set $environment (local.get $env))
            (global.set $environment_data (local.get $data))
            (global.set $environment_count (i32.load (local.get $buffer))))))

   (func $caml_getenv
      (param $name (ref eq)) (result eqref)
      (local $var (ref $bytes)) (local $i i32) (local $j i32)
      (local $len i32) (local $s i32) (local $c i32)
      (call $initialize_env)
      (local.set $var (ref.cast (ref $bytes) (local.get $name)))
      (local.set $len (array.len (local.get $var)))
      (block $not_found
         (loop $loop
            (if (i32.lt_u (local.get $i) (local.get $len))
               (then
                  (br_if $not_found
                     (i32.eq (i32.const 61) ;; '='
                         (array.get_u $bytes (local.get $var) (local.get $i))))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (br $loop))))
         (local.set $i (i32.const 0))
         (loop $loop
            (if (i32.lt_u (local.get $i) (global.get $environment_count))
               (then
                  (local.set $s
                     (i32.load
                        (i32.add (global.get $environment)
                           (i32.shl (local.get $i) (i32.const 2)))))
                  (local.set $j (i32.const 0))
                  (block $next
                     (loop $scan
                        (if (i32.lt_u (local.get $j) (local.get $len))
                           (then
                              (local.set $c
                                 (i32.load8_u
                                    (i32.add (local.get $s) (local.get $j))))
                              (br_if $next (i32.eqz (local.get $c)))
                              (br_if $next
                                  (i32.ne (local.get $c)
                                     (array.get $bytes
                                        (local.get $var) (local.get $j))))
                              (local.set $j
                                 (i32.add (local.get $j) (i32.const 1)))
                              (br $scan))))
                     (br_if $next
                        (i32.ne (i32.const 61) ;; '='
                           (i32.load8_u
                              (i32.add (local.get $s) (local.get $j)))))
                     (local.set $s
                        (i32.add (local.get $s)
                           (i32.add (local.get $j) (i32.const 1))))
                     (return_call $blit_memory_to_string
                        (local.get $s) (call $strlen (local.get $s))))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (br $loop)))))
      (ref.null eq))
)
(@else
   (func $caml_getenv
      (param (ref eq)) (result eqref)
      (local $res anyref)
      (local.set $res
         (call $getenv
            (call $unwrap (call $caml_jsstring_of_string (local.get 0)))))
      (if (i32.eqz (call $jsstring_test (local.get $res)))
         (then (return (ref.null eq))))
      (return_call $caml_string_of_jsstring (call $wrap (local.get $res))))
))

   (func (export "caml_sys_getenv") (export "caml_sys_unsafe_getenv")
      (param $name (ref eq)) (result (ref eq))
      (local $res eqref)
      (local.set $res (call $caml_getenv (local.get $name)))
      (if (ref.is_null (local.get $res))
         (then
            (call $caml_raise_not_found)))
      (ref.as_non_null (local.get $res)))

   (func (export "caml_sys_getenv_opt")
      (param $name (ref eq)) (result (ref eq))
      (local $res eqref)
      (local.set $res (call $caml_getenv (local.get $name)))
      (if (ref.is_null (local.get $res))
         (then
            (return (ref.i31 (i32.const 0)))))
      (array.new_fixed $block 2 (ref.i31 (i32.const 0))
         (ref.as_non_null (local.get $res))))

(@if wasi
(@then
   (global $argv (mut (ref null $block)) (ref.null $block))

   (func $caml_sys_argv (export "caml_sys_argv")
      (param (ref eq)) (result (ref eq))
      (local $buffer i32) (local $res i32)
      (local $argc i32) (local $argv i32) (local $argv_buf i32)
      (local $args (ref $block)) (local $arg i32) (local $i i32)
      (block $init
         (return (br_on_null $init (global.get $argv))))
      (block $error
         (local.set $buffer (call $get_buffer))
         (local.set $res
            (call $args_sizes_get
               (local.get $buffer)
               (i32.add (local.get $buffer) (i32.const 4))))
         (br_if $error (local.get $res))
         (local.set $argc (i32.load (local.get $buffer)))
         (local.set $argv
            (call $checked_malloc (i32.shl (local.get $argc) (i32.const 2))))
         (local.set $argv_buf
            (call $checked_malloc (i32.load offset=4 (local.get $buffer))))
         (local.set $res
            (call $args_get (local.get $argv) (local.get $argv_buf)))
         (br_if $error (local.get $res))
         (local.set $args
            (array.new $block (ref.i31 (i32.const 0))
               (i32.add (local.get $argc) (i32.const 1))))
         (loop $loop
            (if (i32.lt_u (local.get $i) (local.get $argc))
               (then
                  (local.set $arg
                     (i32.load
                        (i32.add (local.get $argv)
                           (i32.shl (local.get $i) (i32.const 2)))))
                  (array.set $block (local.get $args)
                     (i32.add (local.get $i) (i32.const 1))
                     (call $blit_memory_to_string
                        (local.get $arg) (call $strlen (local.get $arg))))
                  (local.set $i
                     (i32.add (local.get $i) (i32.const 1)))
                  (br $loop))))
         (global.set $argv (local.get $args))
         (call $free (local.get $argv))
         (call $free (local.get $argv_buf))
         (return (local.get $args)))
      (call $caml_handle_sys_error (ref.i31 (i32.const 0)) (local.get $res))
      (array.new_fixed $block 0))

   (func (export "caml_sys_executable_name")
      (param (ref eq)) (result (ref eq))
      (array.get $block
         (ref.cast (ref $block) (call $caml_sys_argv (ref.i31 (i32.const 0))))
         (i32.const 1)))
)
(@else
   (func (export "caml_sys_argv") (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $caml_js_to_string_array (call $argv)))

   (func (export "caml_sys_executable_name")
      (param (ref eq)) (result (ref eq))
      (array.get $block
         (ref.cast (ref $block) (call $caml_js_to_string_array (call $argv)))
         (i32.const 1)))
))

(@if wasi
(@then
   (func (export "caml_sys_time") (export "caml_sys_time_include_children")
      (param (ref eq)) (result (ref eq))
      (local $buffer i32) (local $res i32)
      (local.set $buffer (call $get_buffer))
      (local.set $res
         (call $clock_time_get (i32.const 2) (i64.const 1) (local.get $buffer)))
      ;; wasmtime does not support the CPU-time clock, so use the
      ;; monotonic clock instead as a fallback
      (if (i32.eq (local.get $res) (i32.const 8))
         (then
            (local.set $res
               (call $clock_time_get
                  (i32.const 1) (i64.const 1) (local.get $buffer)))))
      (if (local.get $res)
         (then
            (call $caml_handle_sys_error
               (ref.i31 (i32.const 0)) (local.get $res))))
      (struct.new $float
         (f64.mul (f64.convert_i64_u (i64.load (local.get $buffer)))
            (f64.const 1e-9))))
)
(@else
   (func (export "caml_sys_time") (export "caml_sys_time_include_children")
      (param (ref eq)) (result (ref eq))
      (struct.new $float (f64.mul (call $time) (f64.const 0.001))))
))

(@if wasi
(@then
   (func (export "caml_sys_system_command")
      (param (ref eq)) (result (ref eq))
      (call $caml_invalid_argument (@string "Sys.command not implemented"))
      (return (ref.i31 (i32.const 0))))
)
(@else
   (func (export "caml_sys_system_command")
      (param (ref eq)) (result (ref eq))
      (try
         (do
            (return
               (call $system
                  (call $unwrap (call $caml_jsstring_of_string (local.get 0))))))
         (catch $javascript_exception
            (call $caml_handle_sys_error (pop externref))))
      (return (ref.i31 (i32.const 0))))
))

(@if wasi
(@then
   (func (export "caml_sys_random_seed")
      (param (ref eq)) (result (ref eq))
      (local $r (ref extern))
      (local $a (ref $block))
      (local $i i32) (local $n i32)
      (local $buffer i32) (local $res i32)
      (local.set $n (i32.const 12))
      (local.set $buffer (call $get_buffer))
      (local.set $res (call $random_get (local.get $buffer) (i32.const 96)))
      (if (local.get $res)
         (then
            (call $caml_handle_sys_error
               (ref.i31 (i32.const 0)) (local.get $res))))
      (local.set $a
         (array.new $block (ref.i31 (i32.const 0))
            (i32.add (local.get $n) (i32.const 1))))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $n))
            (then
               (array.set $block
                  (local.get $a) (i32.add (local.get $i) (i32.const 1))
                  (ref.i31
                     (i32.load
                        (i32.add
                           (local.get $buffer
                             (i32.shl (local.get $i) (i32.const 2)))))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (local.get $a))
)
(@else
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
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $n))
            (then
               (array.set $block
                  (local.get $a) (i32.add (local.get $i) (i32.const 1))
                  (ref.i31 (call $ta_get_i32 (local.get $r) (local.get $i))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (local.get $a))
))

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

(@if wasi
(@then
   (global $on_windows i32 (i32.const 0))
))

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

(@if wasi
(@then
   (func (export "caml_sys_isatty")
      (param $ch (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))
)
(@else
   (func (export "caml_sys_isatty")
      (param $ch (ref eq)) (result (ref eq))
      (return_call $isatty (call $caml_channel_descriptor (local.get $ch))))
))

   (func (export "caml_runtime_variant") (param (ref eq)) (result (ref eq))
      (@string ""))

   (func (export "caml_runtime_parameters") (param (ref eq)) (result (ref eq))
      (@string ""))

   (func (export "caml_install_signal_handler")
      (param (ref eq) (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_sys_convert_signal_number")
      (param $signo (ref eq)) (result (ref eq))
      (local.get $signo))

   (func (export "caml_sys_rev_convert_signal_number")
      (param $signo (ref eq)) (result (ref eq))
      (local.get $signo))

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

(@if wasi
(@then
   (func $caml_handle_sys_error (export "caml_handle_sys_error")
      (param $arg (ref eq)) (param $errno i32)
      (local $msg (ref eq))
      (local.set $msg
         (if (result (ref eq)) (i32.gt_u (local.get $errno)
                (array.len (global.get $error_messages)))
            (then
               (@string "unknown system error"))
            (else
               (array.get $block (global.get $error_messages)
                  (local.get $errno)))))
      (if (ref.test (ref $bytes) (local.get $arg))
         (then
            (local.set $msg
               (call $caml_string_concat (local.get $arg)
                  (call $caml_string_concat (@string ": ") (local.get $msg))))))
      (call $caml_raise_sys_error (local.get $msg))
   )
)
(@else
   (func $caml_handle_sys_error (export "caml_handle_sys_error")
      (param $exn externref)
      (call $caml_raise_sys_error
         (call $caml_string_of_jsstring
            (call $caml_js_meth_call
               (call $wrap (any.convert_extern (local.get $exn)))
               (global.get $toString)
               (array.new_fixed $block 1 (ref.i31 (i32.const 0)))))))
))
)
