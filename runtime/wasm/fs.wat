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
   (import "bindings" "getcwd" (func $getcwd (result anyref)))
   (import "bindings" "chdir" (func $chdir (param anyref)))
   (import "bindings" "mkdir" (func $mkdir (param anyref) (param i32)))
   (import "bindings" "rmdir" (func $rmdir (param anyref)))
   (import "bindings" "unlink" (func $unlink (param anyref)))
   (import "bindings" "read_dir"
      (func $read_dir (param anyref) (result (ref extern))))
   (import "bindings" "file_exists"
      (func $file_exists (param anyref) (result (ref eq))))
   (import "bindings" "is_directory"
      (func $is_directory (param anyref) (result (ref eq))))
   (import "bindings" "is_file"
      (func $is_file (param anyref) (result (ref eq))))
   (import "bindings" "rename" (func $rename (param anyref) (param anyref)))
   (import "jslib" "wrap" (func $wrap (param anyref) (result (ref eq))))
   (import "jslib" "unwrap" (func $unwrap (param (ref eq)) (result anyref)))
   (import "jslib" "caml_string_of_jsstring"
      (func $caml_string_of_jsstring (param (ref eq)) (result (ref eq))))
   (import "jslib" "caml_jsstring_of_string"
      (func $caml_jsstring_of_string (param (ref eq)) (result (ref eq))))
   (import "jslib" "caml_js_to_string_array"
      (func $caml_js_to_string_array (param $a (ref extern)) (result (ref eq))))
   (import "fail" "caml_raise_sys_error"
      (func $caml_raise_sys_error (param (ref eq))))
   (import "fail" "javascript_exception"
      (tag $javascript_exception (param externref)))
   (import "sys" "caml_handle_sys_error"
      (func $caml_handle_sys_error (param externref)))
   (import "string" "caml_string_concat"
      (func $caml_string_concat (param (ref eq) (ref eq)) (result (ref eq))))

   (type $bytes (array (mut i8)))

   (func (export "caml_sys_getcwd")
      (param (ref eq)) (result (ref eq))
      (try (result (ref eq))
         (do
            (call $caml_string_of_jsstring (call $wrap (call $getcwd))))
         (catch $javascript_exception
            (call $caml_handle_sys_error (pop externref))
            (ref.i31 (i32.const 0)))))

   (func (export "caml_sys_chdir")
      (param $name (ref eq)) (result (ref eq))
      (try
         (do
            (call $chdir
               (call $unwrap (call $caml_jsstring_of_string (local.get $name)))))
         (catch $javascript_exception
            (call $caml_handle_sys_error (pop externref))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_sys_mkdir")
      (param $name (ref eq)) (param $perm (ref eq)) (result (ref eq))
      (try
         (do
            (call $mkdir
               (call $unwrap (call $caml_jsstring_of_string (local.get $name)))
               (i31.get_u (ref.cast (ref i31) (local.get $perm)))))
         (catch $javascript_exception
            (call $caml_handle_sys_error (pop externref))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_sys_read_directory")
      (param $name (ref eq)) (result (ref eq))
      (try
         (do
            (return
               (call $caml_js_to_string_array
                  (call $read_dir
                     (call $unwrap
                        (call $caml_jsstring_of_string (local.get $name)))))))
         (catch $javascript_exception
            (call $caml_handle_sys_error (pop externref))
            (return (ref.i31 (i32.const 0))))))

   (func (export "caml_sys_rmdir")
      (param $name (ref eq)) (result (ref eq))
      (try
         (do
            (call $rmdir
               (call $unwrap (call $caml_jsstring_of_string (local.get $name)))))
         (catch $javascript_exception
            (call $caml_handle_sys_error (pop externref))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_sys_remove")
      (param $name (ref eq)) (result (ref eq))
      (try
         (do
            (call $unlink
               (call $unwrap (call $caml_jsstring_of_string (local.get $name)))))
         (catch $javascript_exception
            (call $caml_handle_sys_error (pop externref))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_sys_rename")
      (param $o (ref eq)) (param $n (ref eq)) (result (ref eq))
      (try
         (do
            (call $rename
               (call $unwrap (call $caml_jsstring_of_string (local.get $o)))
               (call $unwrap (call $caml_jsstring_of_string (local.get $n)))))
         (catch $javascript_exception
            (call $caml_handle_sys_error (pop externref))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_sys_file_exists")
      (param $name (ref eq)) (result (ref eq))
      (return_call $file_exists
         (call $unwrap (call $caml_jsstring_of_string (local.get $name)))))

   (@string $no_such_file ": No such file or directory")

   (func $caml_raise_no_such_file (param $name (ref eq))
      (call $caml_raise_sys_error
         (call $caml_string_concat (local.get $name)
            (global.get $no_such_file))))

   (func (export "caml_read_file_content")
      (param (ref eq)) (result (ref eq))
      (call $caml_raise_no_such_file (local.get 0))
      (ref.i31 (i32.const 0)))

   (func (export "caml_create_file")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (call $caml_raise_no_such_file (local.get 0))
      (ref.i31 (i32.const 0)))

   (func (export "caml_fs_init") (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_sys_is_directory")
      (param $name (ref eq)) (result (ref eq))
      (try
         (do
            (return
               (call $is_directory
                  (call $unwrap
                     (call $caml_jsstring_of_string (local.get $name))))))
         (catch $javascript_exception
            (call $caml_handle_sys_error (pop externref))
            (return (ref.i31 (i32.const 0))))))

   (func (export "caml_sys_is_regular_file")
      (param $name (ref eq)) (result (ref eq))
      (try
         (do
            (return
               (call $is_file
                  (call $unwrap
                     (call $caml_jsstring_of_string (local.get $name))))))
         (catch $javascript_exception
            (call $caml_handle_sys_error (pop externref))
            (return (ref.i31 (i32.const 0))))))

   (func (export "caml_mount_autoload")
      (param (ref eq) (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))
)
