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
(@if wasi
(@then
   (import "libc" "memory" (memory 2))
   (import "libc" "free" (func $free (param i32)))
   (import "wasi_memory" "checked_malloc"
      (func $checked_malloc (param i32) (result i32)))
   (import "wasi_memory" "get_buffer" (func $get_buffer (result i32)))
   (import "wasi_memory" "write_string_to_memory"
      (func $write_string_to_memory (param i32 i32 (ref eq)) (result i32)))
   (import "wasi_memory" "blit_memory_to_string"
      (func $blit_memory_to_string (param i32 i32) (result (ref $bytes))))
   (import "wasi_snapshot_preview1" "fd_prestat_get"
      (func $fd_prestat_get (param i32 i32) (result i32)))
   (import "wasi_snapshot_preview1" "fd_prestat_dir_name"
      (func $fd_prestat_dir_name (param i32 i32 i32) (result i32)))
   (import "wasi_snapshot_preview1" "path_rename"
      (func $path_rename (param i32 i32 i32 i32 i32 i32) (result i32)))
   (import "wasi_snapshot_preview1" "path_unlink_file"
      (func $path_unlink_file (param i32 i32 i32) (result i32)))
   (import "wasi_snapshot_preview1" "path_create_directory"
      (func $path_create_directory (param i32 i32 i32) (result i32)))
   (import "wasi_snapshot_preview1" "path_remove_directory"
      (func $path_remove_directory (param i32 i32 i32) (result i32)))
   (import "wasi_snapshot_preview1" "path_filestat_get"
      (func $path_filestat_get (param i32 i32 i32 i32 i32) (result i32)))
   (import "wasi_snapshot_preview1" "path_open"
      (func $path_open (param i32 i32 i32 i32 i32 i64 i64 i32 i32) (result i32)))
   (import "wasi_snapshot_preview1" "fd_readdir"
      (func $fd_readddir (param i32 i32 i32 i64 i32) (result i32)))
   (import "wasi_snapshot_preview1" "fd_close"
      (func $fd_close (param i32) (result i32)))
   (import "sys" "caml_handle_sys_error"
      (func $caml_handle_sys_error (param (ref eq) i32)))
)
(@else
   (import "bindings" "on_windows" (global $on_windows i32))
   (import "bindings" "getcwd" (func $getcwd (result anyref)))
   (import "bindings" "chdir" (func $chdir (param anyref)))
   (import "bindings" "mkdir" (func $mkdir (param anyref) (param i32)))
   (import "bindings" "rmdir" (func $rmdir (param anyref)))
   (import "bindings" "unlink" (func $unlink (param anyref)))
   (import "bindings" "tmpdir" (func $tmpdir (result anyref)))
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
   (import "fail" "javascript_exception"
      (tag $javascript_exception (param externref)))
   (import "sys" "caml_handle_sys_error"
      (func $caml_handle_sys_error (param externref)))
))
   (import "string" "caml_string_concat"
      (func $caml_string_concat (param (ref eq) (ref eq)) (result (ref eq))))
   (import "fail" "caml_raise_sys_error"
      (func $caml_raise_sys_error (param (ref eq))))

   (type $bytes (array (mut i8)))
   (type $block (array (mut (ref eq))))

(@if wasi
(@then
   (type $preopen
      (struct
         (field $prefix (ref $bytes))
         (field $fd i32)
         (field $next (ref null $preopen))))

   (global $preopens (mut (ref null $preopen)) (ref.null $preopen))

   (global $preopens_initialized (mut i32) (i32.const 0))

   (func $normalize_prefix (param $prefix (ref $bytes)) (result (ref $bytes))
      (local $i i32) (local $len i32) (local $c i32) (local $res (ref $bytes))
      (local.set $len (array.len (local.get $prefix)))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $len))
            (then
               (local.set $c
                  (array.get $bytes (local.get $prefix) (local.get $i)))
               (if (i32.eq (local.get $c) (i32.const 47)) ;; '/'
                  (then
                     (local.set $i (i32.add (local.get $i) (i32.const 1)))
                     (br $loop)))
               (if (i32.eq (local.get $c) (i32.const 46)) ;; '.'
                  (then
                     (if (i32.eq (local.get $i)
                            (i32.sub (local.get $len) (i32.const 1)))
                        (then
                           (local.set $i (i32.add (local.get $i) (i32.const 1)))
                           (br $loop))
                        (else
                           (local.set $c
                              (array.get $bytes (local.get $prefix)
                                 (i32.add (local.get $i) (i32.const 1))))
                           (if (i32.eq (local.get $c) (i32.const 47)) ;; '/'
                              (then
                                 (local.set $i
                                    (i32.add (local.get $i) (i32.const 2)))
                                 (br $loop))))))))))
      (if (i32.eq (local.get $i) (local.get $len))
         (then (return (@string ""))))
      (local.set $i (i32.sub (local.get $i) (i32.const 1)))
      (if (i32.gt_u (local.get $i) (i32.const 0))
         (then
            (local.set $res
               (array.new $bytes (i32.const 0)
                  (i32.sub (local.get $len) (local.get $i))))
            (array.copy $bytes $bytes
               (local.get $res) (i32.const 0)
               (local.get $prefix) (local.get $i)
               (i32.sub (local.get $len) (local.get $i)))
            (return (local.get $res))))
      (return (local.get $prefix)))

   (func $get_preopens (result (ref null $preopen))
      (local $fd i32) (local $buffer i32) (local $res i32) (local $len i32)
      (if $done (i32.eqz (global.get $preopens_initialized))
         (then
            (local.set $buffer (call $get_buffer))
            (local.set $fd (i32.const 3))
            (loop $loop
               (local.set $res
                  (call $fd_prestat_get (local.get $fd) (local.get $buffer)))
               (br_if $done (i32.eq (local.get $res) (i32.const 8))) ;; EBADF
               (block $skip
                  (br_if $skip
                     (i32.eqz
                        (i32.and (i32.eqz (local.get $res))
                           (i32.eqz (i32.load8_u (local.get $buffer))))))
                  (local.set $len (i32.load offset=4 (local.get $buffer)))
                  (local.set $res
                     (call $fd_prestat_dir_name
                        (local.get $fd) (local.get $buffer) (local.get $len)))
                  (br_if $skip (local.get $res))
                  (global.set $preopens
                     (struct.new $preopen
                        (call $normalize_prefix
                           (call $blit_memory_to_string
                              (local.get $buffer) (local.get $len)))
                        (local.get $fd)
                        (global.get $preopens))))
               (local.set $fd (i32.add (local.get $fd) (i32.const 1)))
               (br $loop))
            (global.set $preopens_initialized (i32.const 1))))
      (global.get $preopens))

   (global $current_dir (mut (ref $bytes)) (@string ""))

   (@string $root_dir "/")

   (func $make_absolute
      (param $path (ref $bytes)) (result (ref $bytes))
      (local $need_slash i32) (local $i i32) (local $abs_path (ref $bytes))
      (if (i32.eqz (array.len (local.get $path)))
         (then ;; empty path
            (return (global.get $current_dir))))
      (if (i32.eq (i32.const 47) ;; '/'
             (array.get_u $bytes (local.get $path) (i32.const 0)))
         (then ;; absolute path
            (return (local.get $path))))
      (if (i32.and
             (i32.eq (i32.const 46) ;; '.'
                (array.get_u $bytes (local.get $path) (i32.const 0)))
             (i32.eq (array.len (local.get $path)) (i32.const 1)))
         (then
            ;; "."
            (return (global.get $current_dir))))
      (if (i32.ge_u (array.len (local.get $path)) (i32.const 2))
         (then
            (if (i32.and
                   (i32.eq (i32.const 46) ;; '.'
                      (array.get_u $bytes (local.get $path) (i32.const 0)))
                   (i32.eq (i32.const 47) ;; '/'
                      (array.get_u $bytes (local.get $path) (i32.const 1))))
               (then ;; starts with "./"
                  (local.set $i (i32.const 2))))))
      (if (i32.eq (local.get $i) (array.len (local.get $path)))
         (then ;; "./"
            (return (global.get $current_dir))))
      (local.set $need_slash
         (if (result i32) (array.len (global.get $current_dir))
            (then
               (i32.ne (i32.const 47) ;; '/'
                  (array.get_u $bytes (global.get $current_dir)
                     (i32.sub (array.len (global.get $current_dir))
                        (i32.const 1)))))
            (else
               (i32.const 1))))
      (local.set $abs_path
         (array.new $bytes (i32.const 0)
            (i32.add (array.len (global.get $current_dir))
               (i32.add (i32.sub (local.get $need_slash) (local.get $i))
                  (array.len (local.get $path))))))
      (array.copy $bytes $bytes
         (local.get $abs_path) (i32.const 0)
         (global.get $current_dir) (i32.const 0)
         (array.len (global.get $current_dir)))
      (array.set $bytes (local.get $abs_path)
         (array.len (global.get $current_dir))
         (i32.const 47)) ;; '/'
      (array.copy $bytes $bytes
         (local.get $abs_path)
         (i32.add (array.len (global.get $current_dir))
            (local.get $need_slash))
         (local.get $path) (local.get $i)
         (i32.sub (array.len (local.get $path)) (local.get $i)))
      (local.get $abs_path))

   (func $wasi_chdir (export "wasi_chdir") (param $name (ref eq))
      (local $abs_path (ref $bytes)) (local $path (ref $bytes)) (local $i i32)
      (local.set $abs_path
         (call $make_absolute (ref.cast (ref $bytes) (local.get $name))))
      (local.set $i (i32.sub (array.len (local.get $abs_path)) (i32.const 1)))
      ;; remove trailing slashes
      (loop $loop
         (if (i32.ge_s (local.get $i) (i32.const 0))
            (then
               (if (i32.eq (i32.const 47) ;; '/'
                      (array.get $bytes (local.get $abs_path) (local.get $i)))
                  (then
                     (local.set $i (i32.sub (local.get $i) (i32.const 1)))
                     (br $loop))))))
      (local.set $i (i32.add (local.get $i) (i32.const 1)))
      (if (i32.lt_u (local.get $i) (array.len (local.get $abs_path)))
         (then
            (local.set $path (array.new $bytes (i32.const 0) (local.get $i)))
            (array.copy $bytes $bytes
               (local.get $path) (i32.const 0)
               (local.get $abs_path) (i32.const 0)
               (local.get $i))
            (local.set $abs_path (local.get $path))))
      (global.set $current_dir (local.get $abs_path)))

   (func $prefix_match
      (param $prefix (ref $bytes)) (param $path (ref $bytes)) (result i32)
      (local $i i32) (local $len i32)
      (local.set $len (array.len (local.get $prefix)))
      (if (i32.lt_u (array.len (local.get $path)) (local.get $len))
         (then (return (i32.const 0))))
      (if (i32.gt_u (array.len (local.get $path)) (local.get $len))
         (then
            (if (i32.ne (array.get_u $bytes (local.get $path) (local.get $i))
                   (i32.const 47))
               (then (return (i32.const 0))))))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $len))
            (then
               (if (i32.ne (array.get_u $bytes (local.get $path) (local.get $i))
                      (array.get_u $bytes (local.get $prefix) (local.get $i)))
                  (then (return (i32.const 0))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (return (i32.const 1)))

   (func $resolve_abs_path
      (param $path (ref $bytes)) (result i32 (ref $bytes))
      (local $fd i32) (local $len i32) (local $i i32)
      (local $preopens (ref null $preopen)) (local $current (ref $preopen))
      (local $prefix (ref $bytes)) (local $rel_path (ref $bytes))
      (local.set $preopens (call $get_preopens))
      (local.set $i (i32.const -1))
      (block $done
         (loop $loop
            (local.set $current (br_on_null $done (local.get $preopens)))
            (local.set $prefix
               (struct.get $preopen $prefix (local.get $current)))
            (if (i32.and
                   (i32.gt_s (array.len (local.get $prefix)) (local.get $i))
                   (call $prefix_match (local.get $prefix) (local.get $path)))
               (then
                  (local.set $fd (struct.get $preopen $fd (local.get $current)))
                  (local.set $i (array.len (local.get $prefix)))))
            (local.set $preopens
               (struct.get $preopen $next (local.get $current)))
            (br $loop)))
      (if (i32.eq (local.get $i) (i32.const -1))
         (then ;; not found
            (return (tuple.make 2 (i32.const -1) (@string "")))))
      ;; skip leading slashes
      (local.set $len (local.get $i))
      (loop $loop
         (if (i32.lt_u (local.get $i) (array.len (local.get $path)))
            (then
               (if (i32.eq (array.get_u $bytes (local.get $path) (local.get $i))
                      (i32.const 47)) ;; 47
                  (then
                     (local.set $i (i32.add (local.get $i) (i32.const 1)))
                     (br $loop)))
               (local.set $rel_path
                  (array.new $bytes (i32.const 0)
                     (i32.sub (array.len (local.get $path)) (local.get $i))))
               (array.copy $bytes $bytes
                  (local.get $rel_path) (i32.const 0)
                  (local.get $path) (local.get $i)
                  (i32.sub (array.len (local.get $path)) (local.get $i)))
               (return
                  (tuple.make 2 (local.get $fd) (local.get $rel_path))))))
      (return (tuple.make 2 (local.get $fd) (@string "."))))

   (func (export "wasi_resolve_path")
      (param $vpath (ref eq))
      (result (;fd;) i32 (;address;) i32 (;length;) i32)
      (local $res (tuple i32 (ref $bytes)))
      (local $p i32)
      (local.set $res
         (call $resolve_abs_path
            (call $make_absolute
               (ref.cast (ref $bytes) (local.get $vpath)))))
      (if (i32.ge_u (tuple.extract 2 0 (local.get $res)) (i32.const 0))
         (then
            (local.set $p
               (call $write_string_to_memory
                  (i32.const 0) (i32.const 0)
                  (tuple.extract 2 1 (local.get $res))))))
      (return
         (tuple.make 3
            (tuple.extract 2 0 (local.get $res))
            (local.get $p)
            (array.len (tuple.extract 2 1 (local.get $res))))))

   (func $caml_sys_resolve_path (export "caml_sys_resolve_path")
      (param $path (ref eq)) (result i32 i32 i32)
      (local $res (tuple i32 i32 i32))
      (local.set $res (call $wasi_resolve_path (local.get $path)))
      (if (i32.lt_s (tuple.extract 3 0 (local.get $res)) (i32.const 0))
         (then ;; ENOENT
            (call $caml_handle_sys_error (local.get $path) (i32.const 44))))
      (local.get $res))
))

(@if wasi
(@then
   (func (export "caml_sys_getcwd")
      (export "unix_getcwd") (export "caml_unix_getcwd")
      (param (ref eq)) (result (ref eq))
      (if (array.len (global.get $current_dir))
         (then (return (global.get $current_dir))))
      (global.get $root_dir))
)
(@else
   (func (export "caml_sys_getcwd")
      (param (ref eq)) (result (ref eq))
      (try (result (ref eq))
         (do
            (call $caml_string_of_jsstring (call $wrap (call $getcwd))))
         (catch $javascript_exception
            (call $caml_handle_sys_error (pop externref))
            (ref.i31 (i32.const 0)))))
))

(@if wasi
(@then
   (func (export "caml_sys_chdir")
      (param $name (ref eq)) (result (ref eq))
      (local $p (tuple i32 i32 i32))
      (local $buffer i32) (local $res i32) (local $kind i32)
      (local.set $p (call $caml_sys_resolve_path (local.get $name)))
      (local.set $buffer (call $get_buffer))
      (local.set $res
         (call $path_filestat_get
            (tuple.extract 3 0 (local.get $p))
            (i32.const 1)
            (tuple.extract 3 1 (local.get $p))
            (tuple.extract 3 2 (local.get $p))
            (local.get $buffer)))
      (if (local.get $res)
         (then (call $caml_handle_sys_error (local.get $name) (local.get $res))))
      (local.set $kind (i32.load8_u offset=16 (local.get $buffer)))
      (if (i32.ne (local.get $kind) (i32.const 3))
         (then
            (call $caml_handle_sys_error
               (local.get $name) (i32.const 54)))) ;; ENOTDIR
      (call $wasi_chdir (local.get $name))
      (ref.i31 (i32.const 0)))
)
(@else
   (func (export "caml_sys_chdir")
      (param $name (ref eq)) (result (ref eq))
      (try
         (do
            (call $chdir
               (call $unwrap (call $caml_jsstring_of_string (local.get $name)))))
         (catch $javascript_exception
            (call $caml_handle_sys_error (pop externref))))
      (ref.i31 (i32.const 0)))
))

(@if wasi
(@then
   (func (export "caml_sys_mkdir")
      (param $name (ref eq)) (param $perm (ref eq)) (result (ref eq))
      (local $p (tuple i32 i32 i32))
      (local $res i32)
      (local.set $p (call $caml_sys_resolve_path (local.get $name)))
      (local.set $res
         (call $path_create_directory
            (tuple.extract 3 0 (local.get $p))
            (tuple.extract 3 1 (local.get $p))
            (tuple.extract 3 2 (local.get $p))))
      (call $free (tuple.extract 3 1 (local.get $p)))
      (if (local.get $res)
         (then (call $caml_handle_sys_error (local.get $name) (local.get $res))))
      (ref.i31 (i32.const 0)))
)
(@else
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
))

(@if wasi
(@then
   (func (export "caml_sys_read_directory")
      (param $name (ref eq)) (result (ref eq))
      (local $p (tuple i32 i32 i32))
      (local $buffer i32) (local $res i32) (local $fd i32)
      (local $buf i32) (local $new_buf i32)
      (local $size i32) (local $pos i32) (local $available i32)
      (local $left i32) (local $namelen i32)
      (local $entry i32) (local $entry_size i32)
      (local $cookie i64) (local $tbl (ref $block)) (local $new_tbl (ref $block))
      (local $i i32) (local $s (ref $bytes))
      (local.set $p (call $caml_sys_resolve_path (local.get $name)))
      (local.set $buffer (call $get_buffer))
      (local.set $res
         (call $path_open
            (tuple.extract 3 0 (local.get $p))
            (i32.const 1) ;; symlink_follow
            (tuple.extract 3 1 (local.get $p))
            (tuple.extract 3 2 (local.get $p))
            (i32.const 2) ;; O_DIRECTORY
            (i64.const 0x4000) ;; allow fd_readdir
            (i64.const 0)
            (i32.const 0)
            (local.get $buffer)))
      (call $free (tuple.extract 3 1 (local.get $p)))
      (if (local.get $res)
         (then
            (call $caml_handle_sys_error (local.get $name) (local.get $res))))
      (local.set $fd (i32.load (local.get $buffer)))
      (local.set $buf (call $checked_malloc (i32.const 512)))
      (local.set $size (i32.const 512))
      (local.set $tbl (array.new $block (ref.i31 (i32.const 0)) (i32.const 50)))
      (local.set $i (i32.const 1))
      (loop $loop
         (block $refill
            (local.set $left (i32.sub (local.get $available) (local.get $pos)))
            (br_if $refill (i32.lt_u (local.get $left) (i32.const 24)))
            (local.set $entry (i32.add (local.get $buf) (local.get $pos)))
            (local.set $namelen (i32.load offset=16 (local.get $entry)))
            (local.set $entry_size (i32.add (local.get $namelen) (i32.const 24)))
            (br_if $refill (i32.lt_u (local.get $left) (local.get $entry_size)))
            (local.set $pos (i32.add (local.get $pos) (local.get $entry_size)))
            (local.set $cookie (i64.load (local.get $entry)))
            (if (i32.eq (local.get $i) (array.len (local.get $tbl)))
               (then
                  (local.set $new_tbl
                     (array.new $block (ref.i31 (i32.const 0))
                        (i32.shl (local.get $i) (i32.const 1))))
                  (array.copy $block $block
                     (local.get $new_tbl) (i32.const 0)
                     (local.get $tbl) (i32.const 0) (local.get $i))
                  (local.set $tbl (local.get $new_tbl))))
            (local.set $s
               (call $blit_memory_to_string
                  (i32.add (local.get $entry) (i32.const 24))
                  (local.get $namelen)))
            ;; skip "." and ".."
            (if (i32.eq (local.get $namelen) (i32.const 2))
               (then
                  (br_if $loop
                     (i32.and
                        (i32.eq (i32.const 46)
                           (array.get_u $bytes (local.get $s) (i32.const 0)))
                        (i32.eq (i32.const 46)
                           (array.get_u $bytes (local.get $s) (i32.const 1))))))
               (else
                  (if (i32.eq (local.get $namelen) (i32.const 2))
                     (then
                        (br_if $loop
                           (i32.eq
                              (array.get_u $bytes (local.get $s) (i32.const 0))
                              (i32.const 46)))))))
            (array.set $block (local.get $tbl) (local.get $i) (local.get $s))
            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (br $loop))
         ;; refill
         (if (i32.lt_u (local.get $size) (local.get $entry_size))
            (then
               ;; the entry does not fit
               (local.set $new_buf
                  (call $checked_malloc (local.get $entry_size)))
               (call $free (local.get $buf))
               (local.set $buf (local.get $new_buf))
               (local.set $size (local.get $entry_size))))
         (block $done
            (br_if $done
               (i32.and
                  (i32.ne (i32.const 0) (local.get $available))
                  (i32.lt_u (local.get $available) (local.get $size))))
            (local.set $res
               (call $fd_readddir
                  (local.get $fd)
                  (local.get $buffer)
                  (local.get $size)
                  (local.get $cookie)
                  (local.get $buffer)))
            (if (local.get $res)
               (then
                  (call $caml_handle_sys_error
                     (local.get $name) (local.get $res))))
            (local.set $available (i32.load (local.get $buffer)))
            (br_if $done (i32.eqz (local.get $available)))
            (local.set $pos (i32.const 0))
            (br $loop)))
      ;; done
      (call $free (local.get $buf))
      (local.set $res (call $fd_close (local.get $fd)))
      (if (local.get $res)
         (then (call $caml_handle_sys_error (local.get $name) (local.get $res))))
      (if (i32.eq (local.get $i) (array.len (local.get $tbl)))
         (then (return (local.get $tbl))))
      (local.set $new_tbl
         (array.new $block (ref.i31 (i32.const 0)) (local.get $i)))
      (array.copy $block $block
         (local.get $new_tbl) (i32.const 0)
         (local.get $tbl) (i32.const 0) (local.get $i))
      (local.get $new_tbl))
)
(@else
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
))

(@if wasi
(@then
   (func (export "caml_sys_rmdir")
      (param $name (ref eq)) (result (ref eq))
      (local $p (tuple i32 i32 i32))
      (local $res i32)
      (local.set $p (call $caml_sys_resolve_path (local.get $name)))
      (local.set $res
         (call $path_remove_directory
            (tuple.extract 3 0 (local.get $p))
            (tuple.extract 3 1 (local.get $p))
            (tuple.extract 3 2 (local.get $p))))
      (call $free (tuple.extract 3 1 (local.get $p)))
      (if (local.get $res)
         (then (call $caml_handle_sys_error (local.get $name) (local.get $res))))
      (ref.i31 (i32.const 0)))
)
(@else
   (func (export "caml_sys_rmdir")
      (param $name (ref eq)) (result (ref eq))
      (try
         (do
            (call $rmdir
               (call $unwrap (call $caml_jsstring_of_string (local.get $name)))))
         (catch $javascript_exception
            (call $caml_handle_sys_error (pop externref))))
      (ref.i31 (i32.const 0)))
))

(@if wasi
(@then
   (func (export "caml_sys_remove")
      (param $name (ref eq)) (result (ref eq))
      (local $p (tuple i32 i32 i32))
      (local $res i32)
      (local.set $p (call $caml_sys_resolve_path (local.get $name)))
      (local.set $res
         (call $path_unlink_file
            (tuple.extract 3 0 (local.get $p))
            (tuple.extract 3 1 (local.get $p))
            (tuple.extract 3 2 (local.get $p))))
      (call $free (tuple.extract 3 1 (local.get $p)))
      (if (local.get $res)
         (then (call $caml_handle_sys_error (local.get $name) (local.get $res))))
      (ref.i31 (i32.const 0)))
)
(@else
   (func (export "caml_sys_remove")
      (param $name (ref eq)) (result (ref eq))
      (try
         (do
            (call $unlink
               (call $unwrap (call $caml_jsstring_of_string (local.get $name)))))
         (catch $javascript_exception
            (call $caml_handle_sys_error (pop externref))))
      (ref.i31 (i32.const 0)))
))

(@if wasi
(@then
   (func (export "caml_sys_rename")
      (param $o (ref eq)) (param $n (ref eq)) (result (ref eq))
      (local $op (tuple i32 i32 i32))
      (local $np (tuple i32 i32 i32))
      (local $res i32)
      (local.set $op (call $caml_sys_resolve_path (local.get $o)))
      (local.set $np (call $caml_sys_resolve_path (local.get $n)))
      (local.set $res
         (call $path_rename
            (tuple.extract 3 0 (local.get $op))
            (tuple.extract 3 1 (local.get $op))
            (tuple.extract 3 2 (local.get $op))
            (tuple.extract 3 0 (local.get $np))
            (tuple.extract 3 1 (local.get $np))
            (tuple.extract 3 2 (local.get $np))))
      (call $free (tuple.extract 3 1 (local.get $op)))
      (call $free (tuple.extract 3 1 (local.get $np)))
      (if (local.get $res)
         (then (call $caml_handle_sys_error (local.get $o) (local.get $res))))
      (ref.i31 (i32.const 0)))
)
(@else
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
))

(@if wasi
(@then
   (func (export "caml_sys_file_exists")
      (param $name (ref eq)) (result (ref eq))
      (local $p (tuple i32 i32 i32))
      (local $res i32) (local $buffer i32)
      (local.set $p (call $caml_sys_resolve_path (local.get $name)))
      (local.set $buffer (call $get_buffer))
      (local.set $res
         (call $path_filestat_get
            (tuple.extract 3 0 (local.get $p))
            (i32.const 1)
            (tuple.extract 3 1 (local.get $p))
            (tuple.extract 3 2 (local.get $p))
            (local.get $buffer)))
      (ref.i31 (i32.eqz (local.get $res))))
)
(@else
   (func (export "caml_sys_file_exists")
      (param $name (ref eq)) (result (ref eq))
      (return_call $file_exists
         (call $unwrap (call $caml_jsstring_of_string (local.get $name)))))
))

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

(@if wasi
(@then
   (func $caml_sys_file_mode (param $name (ref eq)) (result i32)
      (local $p (tuple i32 i32 i32))
      (local $res i32) (local $buffer i32)
      (local.set $p (call $caml_sys_resolve_path (local.get $name)))
      (local.set $buffer (call $get_buffer))
      (local.set $res
         (call $path_filestat_get
            (tuple.extract 3 0 (local.get $p))
            (i32.const 1)
            (tuple.extract 3 1 (local.get $p))
            (tuple.extract 3 2 (local.get $p))
            (local.get $buffer)))
      (if (local.get $res)
         (then (call $caml_handle_sys_error (local.get $name) (local.get $res))))
      (i32.load8_u offset=16 (local.get $buffer)))

   (func (export "caml_sys_is_directory")
      (param $name (ref eq)) (result (ref eq))
      (ref.i31
         (i32.eq (call $caml_sys_file_mode (local.get $name)) (i32.const 3))))
)
(@else
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
))

(@if wasi
(@then
   (func (export "caml_sys_is_regular_file")
      (param $name (ref eq)) (result (ref eq))
      (ref.i31
         (i32.eq (call $caml_sys_file_mode (local.get $name)) (i32.const 4))))
)
(@else
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
))

(@if wasi
(@then
   (func (export "caml_sys_temp_dir_name") (param (ref eq)) (result (ref eq))
      (@string ""))
)
(@else
   (func (export "caml_sys_temp_dir_name") (param (ref eq)) (result (ref eq))
      (if (global.get $on_windows)
         (then
            (return_call $caml_string_of_jsstring (call $wrap (call $tmpdir)))))
      (@string ""))
))

   (func (export "caml_mount_autoload")
      (param (ref eq) (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))
)
