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
   (import "hash" "caml_string_hash"
      (func $caml_string_hash
         (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "string" "caml_string_equal"
      (func $caml_string_equal
         (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "jslib" "caml_jsstring_of_string"
      (func $caml_jsstring_of_string (param (ref eq)) (result (ref eq))))
   (import "jslib" "unwrap" (func $unwrap (param (ref eq)) (result anyref)))
   (import "obj" "caml_callback_1"
      (func $caml_callback_1
         (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "obj" "caml_callback_2"
      (func $caml_callback_2
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "string" "caml_string_concat"
      (func $caml_string_concat
         (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "printexc" "caml_format_exception"
      (func $caml_format_exception (param (ref eq)) (result (ref eq))))
   (import "sys" "ocaml_exit" (tag $ocaml_exit))
   (import "fail" "ocaml_exception" (tag $ocaml_exception (param (ref eq))))
(@if wasi
(@then
   (import "wasi_snapshot_preview1" "proc_exit" (func $exit (param i32)))
   (import "wasi_snapshot_preview1" "fd_write"
      (func $fd_write (param i32 i32 i32 i32) (result i32)))
   (import "wasi_memory" "get_buffer" (func $get_buffer (result i32)))
   (import "wasi_memory" "write_string_to_memory"
      (func $write_string_to_memory (param i32 i32 (ref eq)) (result i32)))
   (import "wasi_memory" "release_memory"
      (func $release_memory (param i32 i32)))
   (import "io" "IO_BUFFER_SIZE" (global $IO_BUFFER_SIZE i32))
   (import "libc" "memory" (memory 2))
)
(@else
   (import "fail" "javascript_exception"
      (tag $javascript_exception (param externref)))
   (import "bindings" "write" (func $write (param i32) (param anyref)))
   (import "bindings" "exit" (func $exit (param i32)))
))

   (type $block (array (mut (ref eq))))
   (type $bytes (array (mut i8)))

   (type $assoc
      (struct
         (field (ref $bytes))
         (field (mut (ref eq)))
         (field (mut (ref null $assoc)))))

   (type $assoc_array (array (mut (ref null $assoc))))

   (global $Named_value_size i32 (i32.const 13))

   (global $named_value_table (ref $assoc_array)
     (array.new $assoc_array (ref.null $assoc) (global.get $Named_value_size)))

   (func $find_named_value
      (param $s (ref eq)) (param $l (ref null $assoc)) (result (ref null $assoc))
      (local $a (ref $assoc))
      (block $tail (result (ref null $assoc))
         (loop $loop
            (local.set $a
               (br_on_cast_fail $tail (ref null eq) (ref $assoc) (local.get $l)))
            (if (i31.get_u
                   (ref.cast (ref i31)
                       (call $caml_string_equal
                          (local.get $s)
                          (struct.get $assoc 0 (local.get $a)))))
               (then
                  (return (local.get $a))))
            (local.set $l (struct.get $assoc 2 (local.get $a)))
            (br $loop))))

   (func $caml_named_value (export "caml_named_value")
      (param $s (ref eq)) (result (ref null eq))
      (block $not_found
         (return
            (struct.get $assoc 1
               (br_on_null $not_found
                  (call $find_named_value
                     (local.get $s)
                     (array.get $assoc_array (global.get $named_value_table)
                        (i32.rem_u
                           (i31.get_s
                              (ref.cast (ref i31)
                                 (call $caml_string_hash
                                    (ref.i31 (i32.const 0)) (local.get $s))))
                           (global.get $Named_value_size))))))))
      (return (ref.null eq)))

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
      (block $not_found
         (struct.set $assoc 1
            (br_on_null $not_found
               (call $find_named_value (local.get 0) (local.get $r)))
            (local.get 1))
         (return (ref.i31 (i32.const 0))))
      (array.set $assoc_array
         (global.get $named_value_table) (local.get $h)
         (struct.new $assoc
            (ref.cast (ref $bytes) (local.get 0))
            (local.get 1) (local.get $r)))
      (ref.i31 (i32.const 0)))

   ;; Used only for testing (tests-jsoo/bin), but inconvenient to pull out
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

   (@string $fatal_error "Fatal error: exception ")
   (@string $handle_uncaught_exception "Printexc.handle_uncaught_exception")
   (@string $do_at_exit "Pervasives.do_at_exit")

   (global $uncaught_exception (mut externref) (ref.null extern))

(@if (not wasi)
(@then
   (func $reraise_exception (result (ref eq))
      (throw $javascript_exception (global.get $uncaught_exception))
      (ref.i31 (i32.const 0)))

   (func (export "caml_handle_uncaught_exception") (param $exn externref)
      (global.set $uncaught_exception (local.get $exn))
      (call $caml_main (ref.func $reraise_exception)))
))

   (func $caml_main (export "caml_main") (param $start (ref func))
      (local $exn (ref eq))
      (local $msg (ref eq))
(@if wasi
(@then
      (local $buffer i32) (local $i i32) (local $len i32)
      (local $buf i32) (local $remaining i32)
      (local $iovs i32) (local $iovs_len i32) (local $nwritten i32)
      (local $res i32)
))
      (try
         (do
            (drop (call_ref $func (ref.cast (ref $func) (local.get $start)))))
         (catch $ocaml_exit)
         (catch $ocaml_exception
            (local.set $exn (pop (ref eq)))
            (block $exit
               (block $not_registered
                  (try
                     (do
                        (drop
                           (call $caml_callback_2
                              (br_on_null $not_registered
                                 (call $caml_named_value
                                     (global.get $handle_uncaught_exception)))
                              (local.get $exn)
                              (ref.i31 (i32.const 0)))))
                     (catch $ocaml_exit
                        (return)))
                  (br $exit))
               (block $null
                  (drop
                     (call $caml_callback_1
                        (br_on_null $null
                           (call $caml_named_value (global.get $do_at_exit)))
                        (ref.i31 (i32.const 0)))))
               (local.set $msg
                  (call $caml_string_concat
                     (global.get $fatal_error)
                     (call $caml_string_concat
                        (call $caml_format_exception (local.get $exn))
                        (@string "\n"))))
(@if wasi
(@then
               (local.set $len
                  (array.len (ref.cast (ref $bytes) (local.get $msg))))
               (local.set $buffer (call $get_buffer))
               (local.set $nwritten (local.get $buffer))
               (local.set $iovs (i32.add (local.get $buffer) (i32.const 4)))
               (local.set $iovs_len (i32.const 1))
               (local.set $buffer (i32.add (local.get $buffer) (i32.const 12)))
               (local.set $buf
                  (call $write_string_to_memory
                     (local.get $buf) (global.get $IO_BUFFER_SIZE)
                     (local.get $msg)))
               (local.set $remaining (local.get $buf))
               (loop $write
                  (i32.store (local.get $iovs) (local.get $remaining))
                  (i32.store offset=4 (local.get $iovs) (local.get $len))
                  (local.set $res
                     (call $fd_write
                        (i32.const 2) (local.get $iovs) (local.get $iovs_len)
                        (local.get $nwritten)))
                  (if (i32.eqz (local.get $res))
                     (then
                        (local.set $len
                           (i32.sub (local.get $len)
                              (i32.load (local.get $nwritten))))
                        (local.set $remaining
                           (i32.add (local.get $remaining)
                              (i32.load (local.get $nwritten))))
                        (br_if $write (local.get $len)))))
               (call $release_memory (local.get $buffer) (local.get $buf))
)
(@else
               (call $write (i32.const 2)
                  (call $unwrap
                     (call $caml_jsstring_of_string (local.get $msg))))
))
            )
            (call $exit (i32.const 2)))))
)
