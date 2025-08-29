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
   (import "wasi_snapshot_preview1" "clock_time_get"
      (func $clock_time_get (param i32 i64 i32) (result i32)))
   (import "wasi_snapshot_preview1" "path_filestat_get"
      (func $path_filestat_get (param i32 i32 i32 i32 i32) (result i32)))
   (import "wasi_snapshot_preview1" "path_rename"
      (func $path_rename (param i32 i32 i32 i32 i32 i32) (result i32)))
   (import "wasi_snapshot_preview1" "path_link"
      (func $path_link (param i32 i32 i32 i32 i32 i32 i32) (result i32)))
   (import "wasi_snapshot_preview1" "path_symlink"
      (func $path_symlink (param i32 i32 i32 i32 i32) (result i32)))
   (import "wasi_snapshot_preview1" "path_readlink"
      (func $path_readlink (param i32 i32 i32 i32 i32 i32) (result i32)))
   (import "wasi_snapshot_preview1" "path_create_directory"
      (func $path_create_directory (param i32 i32 i32) (result i32)))
   (import "wasi_snapshot_preview1" "path_unlink_file"
      (func $path_unlink_file (param i32 i32 i32) (result i32)))
   (import "wasi_snapshot_preview1" "path_remove_directory"
      (func $path_remove_directory (param i32 i32 i32) (result i32)))
   (import "wasi_snapshot_preview1" "path_open"
      (func $path_open (param i32 i32 i32 i32 i32 i64 i64 i32 i32) (result i32)))
   (import "wasi_snapshot_preview1" "path_filestat_set_times"
      (func $path_filestat_set_times
         (param i32 i32 i32 i32 i64 i64 i32) (result i32)))
   (import "wasi_snapshot_preview1" "fd_filestat_get"
      (func $fd_filestat_get (param i32 i32) (result i32)))
   (import "wasi_snapshot_preview1" "fd_filestat_set_size"
      (func $fd_filestat_set_size (param i32 i64) (result i32)))
   (import "wasi_snapshot_preview1" "fd_write"
      (func $fd_write (param i32 i32 i32 i32) (result i32)))
   (import "wasi_snapshot_preview1" "fd_read"
      (func $fd_read (param i32 i32 i32 i32) (result i32)))
   (import "wasi_snapshot_preview1" "fd_seek"
      (func $fd_seek (param i32 i64 i32 i32) (result i32)))
   (import "wasi_snapshot_preview1" "fd_sync"
      (func $fd_sync (param i32) (result i32)))
   (import "wasi_snapshot_preview1" "fd_close"
      (func $fd_close (param i32) (result i32)))
   (import "wasi_snapshot_preview1" "fd_readdir"
      (func $fd_readddir (param i32 i32 i32 i64 i32) (result i32)))
   (import "libc" "memory" (memory 2))
   (import "libc" "free" (func $free (param i32)))
   (import "libc" "gmtime" (func $gmtime (param i32) (result i32)))
   (import "libc" "localtime" (func $localtime (param i32) (result i32)))
   (import "libc" "mktime" (func $mktime (param i32) (result i64)))
   (import "wasi_memory" "checked_malloc"
      (func $checked_malloc (param i32) (result i32)))
   (import "wasi_memory" "get_buffer" (func $get_buffer (result i32)))
   (import "wasi_memory" "write_string_to_memory"
      (func $write_string_to_memory (param i32 i32 (ref eq)) (result i32)))
   (import "wasi_memory" "blit_memory_to_string"
      (func $blit_memory_to_string (param i32 i32) (result (ref $bytes))))
   (import "wasi_memory" "blit_memory_to_substring"
      (func $blit_memory_to_substring (param i32 (ref $bytes) i32 i32)))
   (import "wasi_memory" "blit_substring_to_memory"
      (func $blit_substring_to_memory (param i32 (ref $bytes) i32 i32)))
   (import "fs" "wasi_resolve_path"
      (func $wasi_resolve_path (param (ref eq)) (result i32 i32 i32)))
   (import "fs" "wasi_chdir" (func $wasi_chdir (param (ref eq))))
   (import "wasi_errors" "error_messages" (global $error_messages (ref $block)))
   (import "ints" "caml_format_int"
      (func $caml_format_int (param (ref eq) (ref eq)) (result (ref eq))))
   (import "string" "caml_string_concat"
      (func $caml_string_concat (param (ref eq) (ref eq)) (result (ref eq))))
)
(@else
   (import "bindings" "gettimeofday" (func $gettimeofday (result f64)))
   (import "bindings" "times" (func $times (result (ref eq))))
   (import "bindings" "gmtime" (func $gmtime (param f64) (result (ref eq))))
   (import "bindings" "localtime"
      (func $localtime (param f64) (result (ref eq))))
   (import "bindings" "mktime"
      (func $mktime
         (param i32) (param i32) (param i32) (param i32) (param i32) (param i32)
         (result f64)))
   (import "bindings" "utimes" (func $utimes (param anyref f64 f64)))
   (import "bindings" "stat" (func $stat (param anyref i32) (result (ref eq))))
   (import "bindings" "lstat" (func $lstat (param anyref i32) (result (ref eq))))
   (import "bindings" "fstat"
      (func $fstat (param (ref eq) i32) (result (ref eq))))
   (import "bindings" "chmod" (func $chmod (param anyref (ref eq))))
   (import "bindings" "fchmod" (func $fchmod (param (ref eq) (ref eq))))
   (import "bindings" "rename" (func $rename (param anyref) (param anyref)))
   (import "bindings" "getcwd" (func $getcwd (result anyref)))
   (import "bindings" "chdir" (func $chdir (param anyref)))
   (import "bindings" "mkdir" (func $mkdir (param anyref) (param i32)))
   (import "bindings" "opendir" (func $opendir (param anyref) (result anyref)))
   (import "bindings" "readdir" (func $readdir (param anyref) (result anyref)))
   (import "bindings" "closedir" (func $closedir (param anyref)))
   (import "bindings" "rmdir" (func $rmdir (param anyref)))
   (import "bindings" "link" (func $link (param anyref anyref)))
   (import "bindings" "symlink" (func $symlink (param anyref anyref i32)))
   (import "bindings" "readlink" (func $readlink (param anyref) (result anyref)))
   (import "bindings" "unlink" (func $unlink (param anyref)))
   (import "bindings" "truncate" (func $truncate (param anyref (ref eq))))
   (import "bindings" "truncate" (func $truncate_64 (param anyref f64)))
   (import "bindings" "ftruncate" (func $ftruncate (param (ref eq) (ref eq))))
   (import "bindings" "ftruncate" (func $ftruncate_64 (param (ref eq) f64)))
   (import "bindings" "file_size" (func $file_size (param i32) (result i64)))
   (import "bindings" "access" (func $access (param anyref) (param i32)))
   (import "bindings" "open"
      (func $open (param anyref) (param i32) (param i32) (result i32)))
   (import "bindings" "write"
      (func $write (param i32 (ref extern) i32 i32 i64) (result i32)))
   (import "bindings" "write"
      (func $write' (param i32 (ref extern) i32 i32 nullexternref) (result i32)))
   (import "bindings" "read"
      (func $read (param i32 (ref extern) i32 i32 i64) (result i32)))
   (import "bindings" "read"
      (func $read' (param i32 (ref extern) i32 i32 nullexternref) (result i32)))
   (import "bindings" "fsync" (func $fsync (param (ref eq))))
   (import "bindings" "close" (func $close (param (ref eq))))
   (import "bindings" "isatty"
      (func $isatty (param (ref eq)) (result (ref eq))))
   (import "js" "unix_error" (global $unix_error_js (ref any)))
   (import "js" "caml_strerror"
      (func $caml_strerror (param i32) (result (ref any))))
   (import "jslib" "wrap" (func $wrap (param anyref) (result (ref eq))))
   (import "jslib" "unwrap" (func $unwrap (param (ref eq)) (result anyref)))
   (import "jslib" "caml_js_meth_call"
      (func $caml_js_meth_call
         (param (ref eq) (ref eq) (ref eq)) (result (ref eq))))
   (import "jslib" "caml_js_get"
      (func $caml_js_get (param (ref eq) (ref eq)) (result (ref eq))))
   (import "jslib" "caml_string_of_jsstring"
      (func $caml_string_of_jsstring (param (ref eq)) (result (ref eq))))
   (import "jslib" "caml_jsstring_of_string"
      (func $caml_jsstring_of_string (param (ref eq)) (result (ref eq))))
   (import "jsstring" "jsstring_test"
      (func $jsstring_test (param anyref) (result i32)))
))
   (import "stdlib" "caml_named_value"
      (func $caml_named_value (param (ref eq)) (result (ref null eq))))
   (import "fail" "ocaml_exception" (tag $ocaml_exception (param (ref eq))))
   (import "fail" "javascript_exception"
      (tag $javascript_exception (param externref)))
   (import "fail" "caml_raise_end_of_file" (func $caml_raise_end_of_file))
   (import "fail" "caml_invalid_argument"
      (func $caml_invalid_argument (param (ref eq))))
   (import "fail" "caml_raise_not_found" (func $caml_raise_not_found))
   (import "io" "convert_flag_list"
      (func $convert_flag_list (param (ref $flags) (ref eq)) (result i32)))
   (import "io" "IO_BUFFER_SIZE" (global $IO_BUFFER_SIZE i32))
   (import "io" "initialize_fd_offset"
      (func $initialize_fd_offset (param i32 i64)))
   (import "io" "get_fd_offset_unchecked"
      (func $get_fd_offset_unchecked (param i32) (result (ref null $fd_offset))))
   (import "io" "release_fd_offset" (func $release_fd_offset (param i32)))
   (import "io" "caml_ml_open_descriptor_in"
      (func $caml_ml_open_descriptor_in (param (ref eq)) (result (ref eq))))
   (import "io" "caml_ml_open_descriptor_out"
      (func $caml_ml_open_descriptor_out (param (ref eq)) (result (ref eq))))
   (import "bindings" "ta_new" (func $ta_new (param i32) (result (ref extern))))
   (import "bindings" "ta_blit_from_bytes"
      (func $ta_blit_from_bytes
         (param (ref $bytes)) (param i32) (param (ref extern)) (param i32)
         (param i32)))
   (import "bindings" "ta_blit_to_bytes"
      (func $ta_blit_to_bytes
         (param (ref extern)) (param i32) (param (ref $bytes)) (param i32)
         (param i32)))
   (import "bigarray" "caml_ba_get_data"
      (func $caml_ba_get_data (param (ref eq)) (result (ref extern))))
   (import "int64" "caml_copy_int64"
      (func $caml_copy_int64  (param i64) (result (ref eq))))
   (import "int64" "Int64_val"
      (func $Int64_val (param (ref eq)) (result i64)))

   (type $bytes (array (mut i8)))
   (type $block (array (mut (ref eq))))
   (type $float (struct (field f64)))
   (type $float_array (array (mut f64)))
   (type $js (struct (field anyref)))

   (type $fd_offset
      (struct (field $offset (mut i64)) (field $seeked (mut i32))))

   (global $unix_error_exn (mut (ref eq)) (ref.i31 (i32.const 0)))

   (@string $unix_error_str "Unix.Unix_error")

   (@string $unix_error_not_initialized
      "Exception Unix.Unix_error not initialized, please link unix.cma")

   (func $get_unix_error_exn (result (ref eq))
      (local $unix_error_exn eqref)
      (if (ref.test (ref i31) (global.get $unix_error_exn))
         (then
            (local.set $unix_error_exn
               (call $caml_named_value (global.get $unix_error_str)))
            (if (ref.is_null (local.get $unix_error_exn))
               (then
                  (call $caml_invalid_argument
                      (global.get $unix_error_not_initialized))))
            (global.set $unix_error_exn
               (ref.as_non_null (local.get $unix_error_exn)))))
      (global.get $unix_error_exn))

   (@string $no_arg "")

(@if wasi
(@then
   (func $unix_resolve_path (export "unix_resolve_path")
      (param $cmd (ref eq)) (param $path (ref eq)) (result i32 i32 i32)
      (local $res (tuple i32 i32 i32))
      (local.set $res (call $wasi_resolve_path (local.get $path)))
      (if (i32.lt_s (tuple.extract 3 0 (local.get $res)) (i32.const 0))
         (then
            (call $caml_unix_error
               (i32.const 44) ;; ENOENT
               (local.get $cmd) (local.get $path))))
      (local.get $res))

   (type $constr_table (array i8))
   (global $error_codes (ref $constr_table)
      (array.new_fixed $constr_table 77
         (i32.const -1)
         (i32.const  0) (i32.const  1) (i32.const 50) (i32.const 51)
         (i32.const 49) (i32.const  2) (i32.const 39) (i32.const  3)
         (i32.const -1) (i32.const  4) (i32.const -1) (i32.const  5)
         (i32.const 55) (i32.const 63) (i32.const 56) (i32.const  6)
         (i32.const 41) (i32.const  7) (i32.const -1) (i32.const  8)
         (i32.const  9) (i32.const 10) (i32.const 65) (i32.const -1)
         (i32.const -1) (i32.const 38) (i32.const 11) (i32.const 12)
         (i32.const 13) (i32.const 58) (i32.const 14) (i32.const 66)
         (i32.const 15) (i32.const 16) (i32.const 42) (i32.const -1)
         (i32.const 17) (i32.const 52) (i32.const 54) (i32.const 53)
         (i32.const 18) (i32.const 57) (i32.const 19) (i32.const 20)
         (i32.const 21) (i32.const 22) (i32.const -1) (i32.const 23)
         (i32.const -1) (i32.const 44) (i32.const 24) (i32.const 25)
         (i32.const 59) (i32.const 26) (i32.const 27) (i32.const -1)
         (i32.const 40) (i32.const 47) (i32.const 28) (i32.const 29)
         (i32.const 67) (i32.const -1) (i32.const 30) (i32.const 31)
         (i32.const -1) (i32.const 45) (i32.const 43) (i32.const 32)
         (i32.const 33) (i32.const 34) (i32.const 35) (i32.const -1)
         (i32.const 62) (i32.const -1) (i32.const 36) (i32.const -1)))

   (func $caml_unix_error_of_code (param $errcode i32) (result (ref eq))
      (local $err i32)
      (if (i32.le_u (local.get $errcode) (i32.const 76))
         (then
            (local.set $err
               (array.get_s $constr_table (global.get $error_codes)
                  (local.get $errcode)))
            (if (i32.ne (local.get $err) (i32.const -1))
               (then
                  (return (ref.i31 (local.get $err)))))))
      (array.new_fixed $block 2
         (ref.i31 (i32.const 0)) (ref.i31 (local.get $errcode))))

   (func $caml_unix_error
      (param $errcode i32) (param $cmd_name (ref eq)) (param $cmd_arg (ref eq))
      (throw $ocaml_exception
         (array.new_fixed $block 5
            (ref.i31 (i32.const 0))
            (call $get_unix_error_exn)
            (call $caml_unix_error_of_code (local.get $errcode))
            (local.get $cmd_name)
            (local.get $cmd_arg))))

   (func (export "unix_error_message") (export "caml_unix_error_message")
      (param $err (ref eq)) (result (ref eq))
      (local $errcode i32) (local $i i32) (local $n i32)
      (if (ref.test (ref i31) (local.get $err))
         (then
            (local.set $n (i31.get_u (ref.cast (ref i31) (local.get $err))))
            (loop $loop
               (if (i32.lt_u (local.get $errcode)
                      (array.len (global.get $error_codes)))
                  (then
                     (if (i32.ne (local.get $n)
                            (array.get $constr_table (global.get $error_codes)
                               (local.get $errcode)))
                        (then
                           (local.set $errcode
                              (i32.add (local.get $errcode) (i32.const 1)))
                           (br $loop))))
                  (else
                     (local.set $errcode (i32.const -1))))))
         (else
            (local.set $errcode
               (i31.get_u
                  (ref.cast (ref i31)
                     (array.get $block
                        (ref.cast (ref $block) (local.get $err))
                        (i32.const 1)))))))
      (if (i32.gt_u (local.get $errcode)
             (array.len (global.get $error_messages)))
         (then
            (return_call $caml_string_concat
               (@string "Unknown error ")
               (call $caml_format_int (@string "%d")
                  (ref.i31 (local.get $errcode))))))
      (array.get $block (global.get $error_messages) (local.get $errcode)))
)
(@else
   (global $unix_error (ref eq) (struct.new $js (global.get $unix_error_js)))

   (func $ensure_string (param $s (ref eq)) (result (ref eq))
      (local $str anyref)
      (drop (block $not_jsstring (result anyref)
         (local.set $str
            (struct.get $js 0
               (br_on_cast_fail $not_jsstring (ref eq) (ref $js)
                  (local.get $s))))
         (drop (br_if $not_jsstring
            (ref.i31 (i32.const 0))
            (i32.eqz (call $jsstring_test (local.get $str)))))
         (return (call $caml_string_of_jsstring (local.get $s)))))
      (return (global.get $no_arg)))

   (@string $code "code")
   (@string $errno "errno")
   (@string $indexOf "indexOf")
   (@string $syscall "syscall")
   (@string $path "path")

   (func $caml_unix_error (param $exception externref) (param $cmd eqref)
      (local $exn (ref eq))
      (local $code (ref eq))
      (local $errno (ref eq))
      (local $variant (ref eq))
      (local.set $exn (call $wrap (any.convert_extern (local.get $exception))))
      (local.set $code (call $caml_js_get (local.get $exn) (global.get $code)))
      (local.set $variant
         (call $caml_js_meth_call (global.get $unix_error)
            (global.get $indexOf)
            (array.new_fixed $block 2 (ref.i31 (i32.const 0))
               (local.get $code))))
      (if (ref.eq (local.get $variant) (ref.i31 (i32.const -1)))
         (then
            (local.set $errno
               (call $caml_js_get (local.get $exn) (global.get $errno)))
            (local.set $errno
               (ref.i31
                  (if (result i32) (ref.test (ref i31) (local.get $errno))
                     (then
                        (i32.sub (i32.const 0)
                           (i31.get_s (ref.cast (ref i31) (local.get $errno)))))
                     (else
                        (i32.const 9999)))))
            (local.set $variant
               (array.new_fixed $block 2 (ref.i31 (i32.const 0))
               (local.get $errno)))))
      (throw $ocaml_exception
         (array.new_fixed $block 5
            (ref.i31 (i32.const 0))
            (call $get_unix_error_exn)
            (local.get $variant)
            (if (result (ref eq)) (ref.is_null (local.get $cmd))
               (then
                  (call $ensure_string
                     (call $caml_js_get (local.get $exn)
                        (global.get $syscall))))
               (else
                  (ref.as_non_null (local.get $cmd))))
            (call $ensure_string
               (call $caml_js_get (local.get $exn) (global.get $path))))))

   (func (export "unix_error_message") (export "caml_unix_error_message")
      (param $err (ref eq)) (result (ref eq))
      (local $errno i32)
      (local.set $errno
         (if (result i32) (ref.test (ref i31) (local.get $err))
            (then
               (i31.get_u (ref.cast (ref i31) (local.get $err))))
            (else
               (i32.sub (i32.const 0)
                  (i31.get_u
                     (ref.cast (ref i31)
                        (array.get $block
                           (ref.cast (ref $block) (local.get $err))
                           (i32.const 1))))))))
      (return_call $caml_string_of_jsstring
         (call $wrap (call $caml_strerror (local.get $errno)))))
))

(@if wasi
(@then
   (func (export "unix_gettimeofday") (export "caml_unix_gettimeofday")
      (param (ref eq)) (result (ref eq))
      (local $buffer i32) (local $res i32)
      (local.set $buffer (call $get_buffer))
      (local.set $res
         (call $clock_time_get
            (i32.const 0) (i64.const 1000) (local.get $buffer)))
      (if (local.get $res)
         (then
            (call $caml_unix_error
               (local.get $res) (@string "gettimeofday") (global.get $no_arg))))
      (struct.new $float
         (f64.mul (f64.convert_i64_u (i64.load (local.get $buffer)))
            (f64.const 1e-9))))
)
(@else
   (func (export "unix_gettimeofday") (export "caml_unix_gettimeofday")
      (param (ref eq)) (result (ref eq))
      (struct.new $float (call $gettimeofday)))
))

(@if wasi
(@then
   (func (export "unix_times") (export "caml_unix_times")
      (param (ref eq)) (result (ref eq))
      (local $buffer i32) (local $res i32)
      (local.set $buffer (call $get_buffer))
      (local.set $res
         (call $clock_time_get
            (i32.const 2) (i64.const 1) (local.get $buffer)))
      ;; wasmtime does not support the CPU-time clock, so use the
      ;; monotonic clock instead as a fallback
      (if (i32.eq (local.get $res) (i32.const 8))
         (then
            (local.set $res
               (call $clock_time_get
                  (i32.const 1) (i64.const 1) (local.get $buffer)))))
      (if (local.get $res)
         (then
            (call $caml_unix_error
               (local.get $res)
               (@string "time")
               (global.get $no_arg))))
      (array.new_fixed $float_array 4
         (f64.mul (f64.convert_i64_u (i64.load (local.get $buffer)))
            (f64.const 1e-9))
         (f64.const 0) (f64.const 0) (f64.const 0)))
)
(@else
   (func (export "caml_alloc_times")
      (param $u f64) (param $s f64) (result (ref eq))
      (array.new_fixed $float_array 4
         (local.get $u) (local.get $s) (f64.const 0) (f64.const 0)))

   (func (export "unix_times") (export "caml_unix_times")
      (param (ref eq)) (result (ref eq))
      (return_call $times))
))

(@if wasi
(@then
  (func $alloc_tm (param $tm i32) (result (ref eq))
      (array.new_fixed $block 10 (ref.i31 (i32.const 0))
         (ref.i31 (i32.load (local.get $tm)))
         (ref.i31 (i32.load offset=4 (local.get $tm)))
         (ref.i31 (i32.load offset=8 (local.get $tm)))
         (ref.i31 (i32.load offset=12 (local.get $tm)))
         (ref.i31 (i32.load offset=16 (local.get $tm)))
         (ref.i31 (i32.load offset=20 (local.get $tm)))
         (ref.i31 (i32.load offset=24 (local.get $tm)))
         (ref.i31 (i32.load offset=28 (local.get $tm)))
         (ref.i31 (select (i32.const 1) (i32.const 0)
            (i32.load offset=32 (local.get $tm))))))
)
(@else
   (func (export "caml_alloc_tm")
      (param $sec i32) (param $min i32) (param $hour i32) (param $mday i32)
      (param $mon i32) (param $year i32) (param $wday i32) (param $yday i32)
      (param $isdst i32) (result (ref eq))
      (array.new_fixed $block 10 (ref.i31 (i32.const 0))
         (ref.i31 (local.get $sec))
         (ref.i31 (local.get $min))
         (ref.i31 (local.get $hour))
         (ref.i31 (local.get $mday))
         (ref.i31 (local.get $mon))
         (ref.i31 (local.get $year))
         (ref.i31 (local.get $wday))
         (ref.i31 (local.get $yday))
         (ref.i31 (local.get $isdst))))
))

(@if wasi
(@then
   (func (export "caml_unix_gmtime") (export "unix_gmtime")
      (param $t (ref eq)) (result (ref eq))
      (local $buffer i32) (local $tm i32)
      (local.set $buffer (call $get_buffer))
      (i64.store (local.get $buffer)
         (i64.trunc_sat_f64_s
            (struct.get $float 0 (ref.cast (ref $float) (local.get $t)))))
      (local.set $tm (call $gmtime (local.get $buffer)))
      (if (i32.eqz (local.get $tm))
         (then
            (call $caml_unix_error (i32.const 28) (; EINVAL ;)
               (@string "gmtime") (global.get $no_arg))))
      (return_call $alloc_tm (local.get $tm)))
)
(@else
   (func (export "caml_unix_gmtime") (export "unix_gmtime")
      (param (ref eq)) (result (ref eq))
      (call $gmtime
         (struct.get $float 0 (ref.cast (ref $float) (local.get 0)))))
))

(@if wasi
(@then
   (func (export "caml_unix_localtime") (export "unix_localtime")
      (param $t (ref eq)) (result (ref eq))
      (local $buffer i32) (local $tm i32)
      (local.set $buffer (call $get_buffer))
      (i64.store (local.get $buffer)
         (i64.trunc_sat_f64_s
            (struct.get $float 0 (ref.cast (ref $float) (local.get $t)))))
      (local.set $tm (call $localtime (local.get $buffer)))
      (if (i32.eqz (local.get $tm))
         (then
            (call $caml_unix_error (i32.const 28) (; EINVAL ;)
               (@string "localtime") (global.get $no_arg))))
      (return_call $alloc_tm (local.get $tm)))
)
(@else
   (func (export "caml_unix_localtime") (export "unix_localtime")
      (param (ref eq)) (result (ref eq))
      (call $localtime
         (struct.get $float 0 (ref.cast (ref $float) (local.get 0)))))
))

(@if wasi
(@then
   (func (export "caml_unix_time") (export "unix_time") (param (ref eq))
      (result (ref eq))
      (local $buffer i32) (local $res i32)
      (local.set $buffer (call $get_buffer))
      (local.set $res
         (call $clock_time_get
            (i32.const 0) (i64.const 1000) (local.get $buffer)))
      (if (local.get $res)
         (then
            (call $caml_unix_error
               (local.get $res) (@string "time") (global.get $no_arg))))
      (struct.new $float
         (f64.floor
            (f64.mul (f64.convert_i64_u (i64.load (local.get $buffer)))
               (f64.const 1e-9)))))
)
(@else
   (func (export "caml_unix_time") (export "unix_time")
      (param (ref eq)) (result (ref eq))
      (struct.new $float (f64.floor (call $gettimeofday))))
))

(@if wasi
(@then
   (func (export "caml_unix_mktime") (export "unix_mktime")
      (param $v (ref eq)) (result (ref eq))
      (local $t (ref $block)) (local $tm i32) (local $time i64)
      (local.set $t (ref.cast (ref $block) (local.get $v)))
      (local.set $tm (call $get_buffer))
      (i32.store (local.get $tm)
         (i31.get_s
            (ref.cast (ref i31)
               (array.get $block (local.get $t) (i32.const 1)))))
      (i32.store offset=4 (local.get $tm)
         (i31.get_s
            (ref.cast (ref i31)
               (array.get $block (local.get $t) (i32.const 2)))))
      (i32.store offset=8 (local.get $tm)
         (i31.get_s
            (ref.cast (ref i31)
               (array.get $block (local.get $t) (i32.const 3)))))
      (i32.store offset=12 (local.get $tm)
         (i31.get_s
            (ref.cast (ref i31)
               (array.get $block (local.get $t) (i32.const 4)))))
      (i32.store offset=16 (local.get $tm)
         (i31.get_s
            (ref.cast (ref i31)
               (array.get $block (local.get $t) (i32.const 5)))))
      (i32.store offset=20 (local.get $tm)
         (i31.get_s
            (ref.cast (ref i31)
               (array.get $block (local.get $t) (i32.const 6)))))
      (i32.store offset=24 (local.get $tm)
         (i31.get_s
            (ref.cast (ref i31)
               (array.get $block (local.get $t) (i32.const 7)))))
      (i32.store offset=28 (local.get $tm)
         (i31.get_s
            (ref.cast (ref i31)
               (array.get $block (local.get $t) (i32.const 8)))))
      (i32.store offset=32 (local.get $tm)
         (i32.const -1))
      (local.set $time (call $mktime (local.get $tm)))
      (if (i64.eq (local.get $time) (i64.const -1))
         (then
            (call $caml_unix_error
               (i32.const 68) (; ERANGE ;)
               (@string "mktime") (global.get $no_arg))))
      (array.new_fixed $block 3
         (ref.i31 (i32.const 0))
         (struct.new $float (f64.convert_i64_s (local.get $time)))
         (call $alloc_tm (local.get $tm))))
)
(@else
   (func (export "caml_unix_mktime") (export "unix_mktime")
      (param (ref eq)) (result (ref eq))
      (local $tm (ref $block)) (local $t f64)
      (local.set $tm (ref.cast (ref $block) (local.get 0)))
      (local.set $t
         (f64.div
            (call $mktime
               (i32.add
                  (i31.get_s
                     (ref.cast (ref i31)
                       (array.get $block (local.get $tm) (i32.const 6))))
                  (i32.const 1900))
               (i31.get_s
                  (ref.cast (ref i31)
                     (array.get $block (local.get $tm) (i32.const 5))))
               (i31.get_s
                  (ref.cast (ref i31)
                     (array.get $block (local.get $tm) (i32.const 4))))
               (i31.get_s
                  (ref.cast (ref i31)
                     (array.get $block (local.get $tm) (i32.const 3))))
               (i31.get_s
                  (ref.cast (ref i31)
                     (array.get $block (local.get $tm) (i32.const 2))))
               (i31.get_s
                  (ref.cast (ref i31)
                     (array.get $block (local.get $tm) (i32.const 1)))))
            (f64.const 1000)))
      (array.new_fixed $block 3 (ref.i31 (i32.const 0))
         (struct.new $float (local.get $t))
         (call $localtime (local.get $t))))
))

(@if wasi
(@then
   (@string $utimes "utimes")

   (func (export "unix_utimes") (export "caml_unix_utimes")
      (param $path (ref eq)) (param $atime (ref eq)) (param $mtime (ref eq))
      (result (ref eq))
      (local $p (tuple i32 i32 i32))
      (local $atim i64) (local $mtim i64)
      (local $set_to_now i32) (local $res i32)
      (local $at f64) (local $mt f64)
      (local.set $p
         (call $unix_resolve_path (global.get $utimes) (local.get $path)))
      (local.set $at
         (struct.get $float 0 (ref.cast (ref $float) (local.get $atime))))
      (local.set $mt
         (struct.get $float 0 (ref.cast (ref $float) (local.get $mtime))))
      (local.set $set_to_now
         (i32.and (f64.eq (local.get $at) (f64.const 0))
            (f64.eq (local.get $mt) (f64.const 0))))
      (if (i32.eqz (local.get $set_to_now))
         (then
            (local.set $atim
               (i64.trunc_sat_f64_s
                  (f64.mul (local.get $at) (f64.const 1e9))))
            (local.set $mtim
               (i64.trunc_sat_f64_s
                  (f64.mul (local.get $mt) (f64.const 1e9))))))
      (local.set $res
         (call $path_filestat_set_times
            (tuple.extract 3 0 (local.get $p))
            (i32.const 0)
            (tuple.extract 3 1 (local.get $p))
            (tuple.extract 3 2 (local.get $p))
            (local.get $atim)
            (local.get $mtim)
            (i32.shl (i32.const 5) (local.get $set_to_now))))
      (call $free (tuple.extract 3 1 (local.get $p)))
      (if (local.get $res)
         (then
            (call $caml_unix_error
               (local.get $res) (global.get $utimes) (local.get $path))))
      (ref.i31 (i32.const 0)))
)
(@else
   (func (export "unix_utimes") (export "caml_unix_utimes")
      (param $path (ref eq)) (param $atime (ref eq)) (param $mtime (ref eq))
      (result (ref eq))
      (local $at f64) (local $mt f64)
      (local.set $at
         (struct.get $float 0 (ref.cast (ref $float) (local.get $atime))))
      (local.set $mt
         (struct.get $float 0 (ref.cast (ref $float) (local.get $mtime))))
      (if (i32.and (f64.eq (local.get $at) (f64.const 0))
              (f64.eq (local.get $mt) (f64.const 0)))
         (then
            (local.set $at (call $gettimeofday))
            (local.set $mt (local.get $at))))
      (try
         (do
            (call $utimes
               (call $unwrap (call $caml_jsstring_of_string (local.get $path)))
               (local.get $at) (local.get $mt)))
         (catch $javascript_exception
            (call $caml_unix_error (pop externref) (ref.null eq))))
      (ref.i31 (i32.const 0)))
))

(@if wasi
(@then
   (global $file_kinds (ref $constr_table)
      (array.new_fixed $constr_table 8
         (i32.const 3)
         (i32.const 3)
         (i32.const 2)
         (i32.const 1)
         (i32.const 0)
         (i32.const 6)
         (i32.const 6)
         (i32.const 4)))

   (func $alloc_stat (param $large i32) (param $p i32) (result (ref eq))
      (array.new_fixed $block 13 (ref.i31 (i32.const 0))
         (ref.i31 (i32.wrap_i64 (i64.load (local.get $p))))
         (ref.i31 (i32.wrap_i64 (i64.load offset=8 (local.get $p))))
         (ref.i31
            (array.get $constr_table
               (global.get $file_kinds) (i32.load8_u offset=16 (local.get $p))))
         (ref.i31 (i32.const 384 (;0600;)))
         (ref.i31 (i32.wrap_i64 (i64.load offset=24 (local.get $p))))
         (ref.i31 (i32.const 1))
         (ref.i31 (i32.const 1))
         (ref.i31 (i32.wrap_i64 (i64.load (local.get $p))))
         (if (result (ref eq)) (local.get $large)
            (then
               (call $caml_copy_int64 (i64.load offset=32 (local.get $p))))
            (else
               (ref.i31 (i32.wrap_i64 (i64.load offset=32 (local.get $p))))))
         (struct.new $float
            (f64.mul (f64.const 1e-9)
               (f64.convert_i64_s (i64.load offset=40 (local.get $p)))))
         (struct.new $float
            (f64.mul (f64.const 1e-9)
               (f64.convert_i64_s (i64.load offset=48 (local.get $p)))))
         (struct.new $float
            (f64.mul (f64.const 1e-9)
               (f64.convert_i64_s (i64.load offset=56 (local.get $p)))))))
))

   (func (export "caml_alloc_stat")
      (param $large i32)
      (param $dev i32) (param $ino i32) (param $kind i32) (param $perm i32)
      (param $nlink i32) (param $uid i32) (param $gid i32) (param $rdev i32)
      (param $size i64) (param $atime f64) (param $mtime f64) (param $ctime f64)
      (result (ref eq))
      (array.new_fixed $block 13 (ref.i31 (i32.const 0))
         (ref.i31 (local.get $dev))
         (ref.i31 (local.get $ino))
         (ref.i31 (local.get $kind))
         (ref.i31 (local.get $perm))
         (ref.i31 (local.get $nlink))
         (ref.i31 (local.get $uid))
         (ref.i31 (local.get $gid))
         (ref.i31 (local.get $rdev))
         (if (result (ref eq)) (local.get $large)
            (then
               (call $caml_copy_int64 (local.get $size)))
            (else
               (ref.i31 (i32.wrap_i64 (local.get $size)))))
         (struct.new $float (local.get $atime))
         (struct.new $float (local.get $mtime))
         (struct.new $float (local.get $ctime))))

(@if wasi
(@then
   (func $stat
      (param $path (ref eq)) (param $large i32) (param $follow i32)
      (param $name (ref eq)) (result (ref eq))
      (local $p (tuple i32 i32 i32))
      (local $buffer i32) (local $res i32)
      (local.set $p
         (call $unix_resolve_path (local.get $name) (local.get $path)))
      (local.set $buffer (call $get_buffer))
      (local.set $res
         (call $path_filestat_get
            (tuple.extract 3 0 (local.get $p))
            (local.get $follow) ;; symlink_follow
            (tuple.extract 3 1 (local.get $p))
            (tuple.extract 3 2 (local.get $p))
            (local.get $buffer)))
      (call $free (tuple.extract 3 1 (local.get $p)))
      (if (local.get $res)
         (then
            (call $caml_unix_error
               (local.get $res) (local.get $name) (local.get $path))))
      (return_call $alloc_stat (local.get $large) (local.get $buffer)))

   (@string $stat "stat")

   (func (export "unix_stat") (export "caml_unix_stat")
      (param $path (ref eq)) (result (ref eq))
      (return_call $stat
         (local.get $path) (i32.const 0) (i32.const 1) (global.get $stat)))

   (func (export "unix_stat_64") (export "caml_unix_stat_64")
      (param $path (ref eq)) (result (ref eq))
      (return_call $stat
         (local.get $path) (i32.const 1) (i32.const 1) (global.get $stat)))

   (@string $lstat "lstat")

   (func (export "unix_lstat") (export "caml_unix_lstat")
      (param $path (ref eq)) (result (ref eq))
      (return_call $stat
         (local.get $path) (i32.const 0) (i32.const 0) (global.get $lstat)))

   (func (export "unix_lstat_64") (export "caml_unix_lstat_64")
      (param $path (ref eq)) (result (ref eq))
      (return_call $stat
         (local.get $path) (i32.const 1) (i32.const 0) (global.get $lstat)))

   (func $fstat (param $fd (ref eq)) (param $large i32) (result (ref eq))
      (local $buffer i32) (local $res i32)
      (local.set $buffer (call $get_buffer))
      (local.set $res
         (call $fd_filestat_get
            (i31.get_u (ref.cast (ref i31) (local.get $fd)))
            (local.get $buffer)))
      (if (local.get $res)
         (then
            (call $caml_unix_error
               (local.get $res) (@string "fstat") (global.get $no_arg))))
      (return_call $alloc_stat (local.get $large) (local.get $buffer)))

   (func (export "unix_fstat") (export "caml_unix_fstat")
      (param $fd (ref eq)) (result (ref eq))
      (return_call $fstat (local.get $fd) (i32.const 0)))

   (func (export "unix_fstat_64") (export "caml_unix_fstat_64")
      (param $fd (ref eq)) (result (ref eq))
      (return_call $fstat (local.get $fd) (i32.const 1)))
)
(@else
   (func (export "unix_stat") (export "caml_unix_stat")
      (param $path (ref eq)) (result (ref eq))
      (try (result (ref eq))
         (do
            (call $stat
               (call $unwrap (call $caml_jsstring_of_string (local.get $path)))
               (i32.const 0)))
         (catch $javascript_exception
            (call $caml_unix_error (pop externref) (ref.null eq))
            (ref.i31 (i32.const 0)))))

   (func (export "unix_stat_64") (export "caml_unix_stat_64")
      (param $path (ref eq)) (result (ref eq))
      (try (result (ref eq))
         (do
            (call $stat
               (call $unwrap (call $caml_jsstring_of_string (local.get $path)))
               (i32.const 1)))
         (catch $javascript_exception
            (call $caml_unix_error (pop externref) (ref.null eq))
            (ref.i31 (i32.const 0)))))

   (func (export "unix_lstat") (export "caml_unix_lstat")
      (param $path (ref eq)) (result (ref eq))
      (try (result (ref eq))
         (do
            (call $lstat
               (call $unwrap (call $caml_jsstring_of_string (local.get $path)))
               (i32.const 0)))
         (catch $javascript_exception
            (call $caml_unix_error (pop externref) (ref.null eq))
            (ref.i31 (i32.const 0)))))

   (func (export "unix_lstat_64") (export "caml_unix_lstat_64")
      (param $path (ref eq)) (result (ref eq))
      (try (result (ref eq))
         (do
            (call $lstat
               (call $unwrap (call $caml_jsstring_of_string (local.get $path)))
               (i32.const 1)))
         (catch $javascript_exception
            (call $caml_unix_error (pop externref) (ref.null eq))
            (ref.i31 (i32.const 0)))))

   (func (export "unix_fstat") (export "caml_unix_fstat")
      (param $fd (ref eq)) (result (ref eq))
      (try (result (ref eq))
         (do
            (call $fstat (local.get $fd) (i32.const 0)))
         (catch $javascript_exception
            (call $caml_unix_error (pop externref) (ref.null eq))
            (ref.i31 (i32.const 0)))))

   (func (export "unix_fstat_64") (export "caml_unix_fstat_64")
      (param $fd (ref eq)) (result (ref eq))
      (try (result (ref eq))
         (do
            (call $fstat (local.get $fd) (i32.const 0)))
         (catch $javascript_exception
            (call $caml_unix_error (pop externref) (ref.null eq))
            (ref.i31 (i32.const 0)))))
))

(@if wasi
(@then
   (func (export "unix_chmod") (export "caml_unix_chmod")
      (param (ref eq) (ref eq)) (result (ref eq))
      ;; no notion of permissions in WASI
      (ref.i31 (i32.const 0)))
)
(@else
   (func (export "unix_chmod") (export "caml_unix_chmod")
      (param $path (ref eq)) (param $perms (ref eq)) (result (ref eq))
      (try
         (do
            (call $chmod
               (call $unwrap (call $caml_jsstring_of_string (local.get $path)))
               (local.get $perms)))
         (catch $javascript_exception
            (call $caml_unix_error (pop externref) (ref.null eq))))
      (ref.i31 (i32.const 0)))
))

(@if wasi
(@then
   (func (export "unix_fchmod") (export "caml_unix_fchmod")
      (param (ref eq) (ref eq)) (result (ref eq))
      ;; no notion of permissions in WASI
      (ref.i31 (i32.const 0)))
)
(@else
   (func (export "unix_fchmod") (export "caml_unix_fchmod")
      (param $fd (ref eq)) (param $perms (ref eq)) (result (ref eq))
      (try
         (do
            (call $fchmod (local.get $fd) (local.get $perms)))
         (catch $javascript_exception
            (call $caml_unix_error (pop externref) (ref.null eq))))
      (ref.i31 (i32.const 0)))
))

(@if wasi
(@then
   (@string $rename "rename")

   (func (export "unix_rename") (export "caml_unix_rename")
      (param $o (ref eq)) (param $n (ref eq)) (result (ref eq))
      (local $op (tuple i32 i32 i32))
      (local $np (tuple i32 i32 i32))
      (local $res i32)
      (local.set $op
         (call $unix_resolve_path (global.get $rename)  (local.get $o)))
      (local.set $np
         (call $unix_resolve_path (global.get $rename) (local.get $n)))
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
         (then
            (call $caml_unix_error
               (local.get $res) (global.get $rename) (local.get $o))))
      (ref.i31 (i32.const 0)))
)
(@else
   (func (export "unix_rename") (export "caml_unix_rename")
      (param $o (ref eq)) (param $n (ref eq)) (result (ref eq))
      (try
         (do
            (call $rename
               (call $unwrap (call $caml_jsstring_of_string (local.get $o)))
               (call $unwrap (call $caml_jsstring_of_string (local.get $n)))))
         (catch $javascript_exception
            (call $caml_unix_error (pop externref) (ref.null eq))))
      (ref.i31 (i32.const 0)))
))

(@if wasi
(@then
   (@string $chdir "chdir")

   (func (export "unix_chdir") (export "caml_unix_chdir")
      (param $name (ref eq)) (result (ref eq))
      (local $p (tuple i32 i32 i32))
      (local $buffer i32) (local $res i32) (local $kind i32)
      (local.set $p
         (call $unix_resolve_path (global.get $chdir) (local.get $name)))
      (local.set $buffer (call $get_buffer))
      (local.set $res
         (call $path_filestat_get
            (tuple.extract 3 0 (local.get $p))
            (i32.const 1)
            (tuple.extract 3 1 (local.get $p))
            (tuple.extract 3 2 (local.get $p))
            (local.get $buffer)))
      (if (local.get $res)
         (then
            (call $caml_unix_error
               (local.get $res) (global.get $chdir) (local.get $name))))
      (local.set $kind (i32.load8_u offset=16 (local.get $buffer)))
      (if (i32.ne (local.get $kind) (i32.const 3))
         (then
            (call $caml_unix_error (i32.const 54) ;; ENOTDIR
               (global.get $chdir) (local.get $name))))
      (call $wasi_chdir (local.get $name))
      (ref.i31 (i32.const 0)))
)
(@else
   (func (export "unix_getcwd") (export "caml_unix_getcwd")
      (param (ref eq)) (result (ref eq))
      (try (result (ref eq))
         (do
            (call $caml_string_of_jsstring (call $wrap (call $getcwd))))
         (catch $javascript_exception
            (call $caml_unix_error (pop externref) (ref.null eq))
            (ref.i31 (i32.const 0)))))

   (func (export "unix_chdir") (export "caml_unix_chdir")
      (param $name (ref eq)) (result (ref eq))
      (try
         (do
            (call $chdir
               (call $unwrap (call $caml_jsstring_of_string (local.get $name)))))
         (catch $javascript_exception
            (call $caml_unix_error (pop externref) (ref.null eq))))
      (ref.i31 (i32.const 0)))
))

(@if wasi
(@then
   (@string $mkdir "mkdir")

   (func (export "unix_mkdir") (export "caml_unix_mkdir")
      (param $path (ref eq)) (param $perm (ref eq)) (result (ref eq))
      (local $p (tuple i32 i32 i32))
      (local $res i32)
      (local.set $p
         (call $unix_resolve_path (global.get $mkdir) (local.get $path)))
      (local.set $res
         (call $path_create_directory
            (tuple.extract 3 0 (local.get $p))
            (tuple.extract 3 1 (local.get $p))
            (tuple.extract 3 2 (local.get $p))))
      (call $free (tuple.extract 3 1 (local.get $p)))
      (if (local.get $res)
         (then
            (call $caml_unix_error
               (local.get $res) (global.get $mkdir) (local.get $path))))
      (ref.i31 (i32.const 0)))
)
(@else
   (func (export "unix_mkdir") (export "caml_unix_mkdir")
      (param $name (ref eq)) (param $perm (ref eq)) (result (ref eq))
      (try
         (do
            (call $mkdir
               (call $unwrap (call $caml_jsstring_of_string (local.get $name)))
               (i31.get_u (ref.cast (ref i31) (local.get $perm)))))
         (catch $javascript_exception
            (call $caml_unix_error (pop externref) (ref.null eq))))
      (ref.i31 (i32.const 0)))
))

(@if wasi
(@then
   (type $directory
      (struct
         (field $fd i32)
         (field $buffer (mut i32))
         (field $size (mut i32))
         (field $pos (mut i32))
         (field $available (mut i32))
         (field $cookie (mut i64))))

   (@string $opendir "opendir")

   (func $unix_opendir (export "unix_opendir") (export "caml_unix_opendir")
      (param $name (ref eq)) (result (ref eq))
      (local $p (tuple i32 i32 i32))
      (local $buffer i32) (local $res i32)
      (local.set $p
         (call $unix_resolve_path (global.get $opendir) (local.get $name)))
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
            (call $caml_unix_error
               (local.get $res) (global.get $opendir) (local.get $name))))
      (struct.new $directory
         (i32.load (local.get $buffer))
         (call $checked_malloc (i32.const 512))
         (i32.const 512)
         (i32.const 0)
         (i32.const 0)
         (i64.const 0)))

   (func $readdir_helper
      (param $vdir (ref eq)) (result (ref eq))
      (local $dir (ref $directory)) (local $buf i32) (local $res i32)
      (local $buffer i32) (local $available i32) (local $left i32)
      (local $namelen i32) (local $entry i32) (local $entry_size i32)
      (local.set $dir (ref.cast (ref $directory) (local.get $vdir)))
      (loop $loop
         (block $refill
            (local.set $left
               (i32.sub (struct.get $directory $available (local.get $dir))
                  (struct.get $directory $pos (local.get $dir))))
            (br_if $refill (i32.lt_u (local.get $left) (i32.const 24)))
            (local.set $entry
               (i32.add (struct.get $directory $buffer (local.get $dir))
                  (struct.get $directory $pos (local.get $dir))))
            (local.set $namelen (i32.load offset=16 (local.get $entry)))
            (local.set $entry_size (i32.add (local.get $namelen) (i32.const 24)))
            (br_if $refill (i32.lt_u (local.get $left) (local.get $entry_size)))
            (struct.set $directory $pos (local.get $dir)
               (i32.add (struct.get $directory $pos (local.get $dir))
                  (local.get $entry_size)))
            (struct.set $directory $cookie (local.get $dir)
               (i64.load (local.get $entry)))
            (return_call $blit_memory_to_string
                (i32.add (local.get $entry) (i32.const 24))
                (local.get $namelen)))
         ;; refill
         (if (i32.lt_u (struct.get $directory $size (local.get $dir))
                (local.get $entry_size))
            (then
               ;; the entry does not fit
               (local.set $buf (call $checked_malloc (local.get $entry_size)))
               (call $free (struct.get $directory $buffer (local.get $dir)))
               (struct.set $directory $buffer (local.get $dir) (local.get $buf))
               (struct.set $directory $size (local.get $dir)
                  (local.get $entry_size))))
         (block $done
            (br_if $done
               (i32.and
                  (i32.ne (i32.const 0)
                     (struct.get $directory $available (local.get $dir))
                  (i32.lt_u (struct.get $directory $available (local.get $dir))
                     (struct.get $directory $size (local.get $dir))))))
            (local.set $buffer (call $get_buffer))
            (local.set $res
               (call $fd_readddir
                  (struct.get $directory $fd (local.get $dir))
                  (struct.get $directory $buffer (local.get $dir))
                  (struct.get $directory $size (local.get $dir))
                  (struct.get $directory $cookie (local.get $dir))
                  (local.get $buffer)))
            (if (local.get $res)
               (then
                  (call $caml_unix_error
                     (local.get $res) (@string "readdir") (global.get $no_arg))))
            (local.set $available (i32.load (local.get $buffer)))
            (br_if $done (i32.eqz (local.get $available)))
            (struct.set $directory $pos (local.get $dir) (i32.const 0))
            (struct.set $directory $available (local.get $dir)
               (local.get $available))
            (br $loop)))
      ;; done
      (call $caml_raise_end_of_file)
      (ref.i31 (i32.const 0)))

   (func $unix_closedir (export "unix_closedir") (export "caml_unix_closedir")
      (param $vdir (ref eq)) (result (ref eq))
      (local $dir (ref $directory)) (local $buf i32) (local $res i32)
      (local.set $dir (ref.cast (ref $directory) (local.get $vdir)))
      (local.set $buf (struct.get $directory $buffer (local.get $dir)))
      (block $error
         (if (i32.eqz (local.get $buf))
            (then
               (local.set $res (i32.const 8)) ;; EBADF
               (br $error)))
         (call $free (local.get $buf))
         (struct.set $directory $buffer (local.get $dir) (i32.const 0))
         (local.set $res
            (call $fd_close (struct.get $directory $fd (local.get $dir))))
         (br_if $error (local.get $res))
         (return (ref.i31 (i32.const 0))))
      (call $caml_unix_error
         (local.get $res) (@string "closedir") (global.get $no_arg))
      (ref.i31 (i32.const 0)))

   (func (export "unix_rewinddir") (export "caml_unix_rewinddir")
      (param $vdir (ref eq)) (result (ref eq))
      (local $dir (ref $directory))
      (local.set $dir (ref.cast (ref $directory) (local.get $vdir)))
      (struct.set $directory $cookie (local.get $dir) (i64.const 0))
      (struct.set $directory $pos (local.get $dir) (i32.const 0))
      (struct.set $directory $available (local.get $dir) (i32.const 0))
      (ref.i31 (i32.const 0)))
)
(@else
   (func $unix_opendir (export "unix_opendir") (export "caml_unix_opendir")
      (param $name (ref eq)) (result (ref eq))
      (try (result (ref eq))
         (do
            (call $wrap
               (call $opendir
                  (call $unwrap
                     (call $caml_jsstring_of_string (local.get $name))))))
         (catch $javascript_exception
            (call $caml_unix_error (pop externref) (ref.null eq))
            (ref.i31 (i32.const 0)))))

   (func $throw_ebadf (param $cmd (ref eq))
      (throw $ocaml_exception
         (array.new_fixed $block 5
            (ref.i31 (i32.const 0))
            (call $get_unix_error_exn)
            (ref.i31 (i32.const 3)) ;; EBADF
            (local.get $cmd)
            (global.get $no_arg))))

   (func $readdir_helper (param $dir (ref eq)) (result (ref eq))
      (block $end
         (return
            (try (result (ref eq))
               (do
                  (call $caml_string_of_jsstring
                     (call $wrap
                       (br_on_null $end
                          (call $readdir (call $unwrap (local.get $dir)))))))
               (catch $javascript_exception
                  (drop (pop externref))
                  (call $throw_ebadf (@string "readdir"))
                  (ref.i31 (i32.const 0))))))
      (call $caml_raise_end_of_file)
      (ref.i31 (i32.const 0)))

   (func $unix_closedir (export "unix_closedir") (export "caml_unix_closedir")
      (export "win_findclose") (export "caml_unix_findclose")
      (param $dir (ref eq)) (result (ref eq))
      (try
         (do
            (call $closedir (call $unwrap (local.get $dir))))
         (catch $javascript_exception
            (drop (pop externref))
            (call $throw_ebadf (@string "closedir"))))
      (ref.i31 (i32.const 0)))

   (func (export "unix_rewinddir") (export "caml_unix_rewinddir")
      (param (ref eq)) (result (ref eq))
      (call $caml_invalid_argument (@string "rewinddir not implemented"))
      (ref.i31 (i32.const 0)))
))

   (func (export "unix_readdir") (export "caml_unix_readdir")
      (param $dir (ref eq)) (result (ref eq))
      (block $return (result (ref eq))
         (br_on_non_null $return (call $readdir_helper (local.get $dir)))
         (call $caml_raise_end_of_file)
         (ref.i31 (i32.const 0))))

   (func $win_find_next (export "win_findnext") (export "caml_unix_findnext")
      (param $dir (ref eq)) (result (ref eq))
      (block $return (result (ref eq))
         (br_on_non_null $return (call $readdir_helper (local.get $dir)))
         (drop (call $unix_closedir (local.get $dir)))
         (call $caml_raise_end_of_file)
         (ref.i31 (i32.const 0))))

   (func (export "win_findfirst") (export "caml_unix_findfirst")
      (param $vpath (ref eq)) (result (ref eq))
      (local $dir (ref eq)) (local $p (ref $bytes)) (local $p' (ref $bytes))
      (local $len i32)
      (local.set $p (ref.cast (ref $bytes) (local.get $vpath)))
      (local.set $len (i32.sub (array.len (local.get $p)) (i32.const 3)))
      (local.set $p' (array.new $bytes (i32.const 0) (local.get $len)))
      (array.copy $bytes $bytes
         (local.get $p') (i32.const 0)
         (local.get $p) (i32.const 0)
         (local.get $len))
      (local.set $dir (call $unix_opendir (local.get $p')))
      (array.new_fixed $block 3 (ref.i31 (i32.const 0))
         (call $win_find_next (local.get $dir))
         (local.get $dir)))

(@if wasi
(@then
   (@string $unlink "unlink")

   (func (export "unix_unlink") (export "caml_unix_unlink")
      (param $path (ref eq)) (result (ref eq))
      (local $p (tuple i32 i32 i32))
      (local $res i32)
      (local.set $p
         (call $unix_resolve_path (global.get $unlink) (local.get $path)))
      (local.set $res
         (call $path_unlink_file
            (tuple.extract 3 0 (local.get $p))
            (tuple.extract 3 1 (local.get $p))
            (tuple.extract 3 2 (local.get $p))))
      (call $free (tuple.extract 3 1 (local.get $p)))
      (if (local.get $res)
         (then
            (call $caml_unix_error
               (local.get $res) (global.get $unlink) (local.get $path))))
      (ref.i31 (i32.const 0)))
)
(@else
   (func (export "unix_unlink") (export "caml_unix_unlink")
      (param $p (ref eq)) (result (ref eq))
      (try
         (do
            (call $unlink
               (call $unwrap (call $caml_jsstring_of_string (local.get $p)))))
         (catch $javascript_exception
            (call $caml_unix_error (pop externref) (ref.null eq))))
      (ref.i31 (i32.const 0)))
))

(@if wasi
(@then
   (@string $rmdir "rmdir")

   (func (export "unix_rmdir") (export "caml_unix_rmdir")
      (param $path (ref eq)) (result (ref eq))
      (local $p (tuple i32 i32 i32))
      (local $res i32)
      (local.set $p
         (call $unix_resolve_path (global.get $rmdir) (local.get $path)))
      (local.set $res
         (call $path_remove_directory
            (tuple.extract 3 0 (local.get $p))
            (tuple.extract 3 1 (local.get $p))
            (tuple.extract 3 2 (local.get $p))))
      (call $free (tuple.extract 3 1 (local.get $p)))
      (if (local.get $res)
         (then
            (call $caml_unix_error
               (local.get $res) (global.get $rmdir) (local.get $path))))
      (ref.i31 (i32.const 0)))
)
(@else
   (func (export "unix_rmdir") (export "caml_unix_rmdir")
      (param $p (ref eq)) (result (ref eq))
      (try
         (do
            (call $rmdir
               (call $unwrap (call $caml_jsstring_of_string (local.get $p)))))
         (catch $javascript_exception
            (call $caml_unix_error (pop externref) (ref.null eq))))
      (ref.i31 (i32.const 0)))
))

(@if wasi
(@then
   (@string $link "link")

   (func (export "unix_link") (export "caml_unix_link")
      (param $follow (ref eq)) (param $o (ref eq)) (param $n (ref eq))
      (result (ref eq))
      (local $op (tuple i32 i32 i32))
      (local $np (tuple i32 i32 i32))
      (local $flags i32)
      (local $res i32)
      (local.set $op (call $unix_resolve_path (global.get $link) (local.get $o)))
      (local.set $np (call $unix_resolve_path (global.get $link) (local.get $n)))
      (if (ref.test (ref $block) (local.get $follow))
         (then
            (local.set $flags
               (i31.get_u
                  (ref.cast (ref i31)
                     (array.get $block
                        (ref.cast (ref $block) (local.get $follow))
                        (i32.const 1)))))))
      (local.set $res
         (call $path_link
            (tuple.extract 3 0 (local.get $op))
            (local.get $flags)
            (tuple.extract 3 1 (local.get $op))
            (tuple.extract 3 2 (local.get $op))
            (tuple.extract 3 0 (local.get $np))
            (tuple.extract 3 1 (local.get $np))
            (tuple.extract 3 2 (local.get $np))))
      (call $free (tuple.extract 3 1 (local.get $op)))
      (call $free (tuple.extract 3 1 (local.get $np)))
      (if (local.get $res)
         (then
            (call $caml_unix_error
               (local.get $res) (global.get $link) (local.get $o))))
      (ref.i31 (i32.const 0)))
)
(@else
   (func (export "unix_link") (export "caml_unix_link")
      (param $follow (ref eq)) (param $d (ref eq)) (param $s (ref eq))
      (result (ref eq))
      (if (ref.test (ref $block) (local.get $follow))
         (then
            (throw $ocaml_exception
               (array.new_fixed $block 5
                  (ref.i31 (i32.const 0))
                  (call $get_unix_error_exn)
                  (ref.i31 (i32.const 25)) ;; ENOSYS
                  (@string "link")
                  (global.get $no_arg)))))
      (try
         (do
            (call $link
               (call $unwrap (call $caml_jsstring_of_string (local.get $d)))
               (call $unwrap (call $caml_jsstring_of_string (local.get $s)))))
         (catch $javascript_exception
            (call $caml_unix_error (pop externref) (ref.null eq))))
      (ref.i31 (i32.const 0)))
))

   (func (export "unix_has_symlink") (export "caml_unix_has_symlink")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 1)))

(@if wasi
(@then
   (@string $symlink "symlink")

   (func (export "unix_symlink") (export "caml_unix_symlink")
      (param $to_dir (ref eq)) (param $o (ref eq)) (param $n (ref eq))
      (result (ref eq))
      (local $path (ref $bytes))
      (local $len i32)
      (local $op i32)
      (local $np (tuple i32 i32 i32))
      (local $flags i32)
      (local $res i32)
      (local.set $path (ref.cast (ref $bytes) (local.get $o)))
      (local.set $len (array.len (local.get $path)))
      (local.set $op
         (call $write_string_to_memory
            (i32.const 0) (i32.const 0) (local.get $path)))
      (local.set $np
         (call $unix_resolve_path (global.get $symlink) (local.get $n)))
      (local.set $res
         (call $path_symlink
            (local.get $op)
            (local.get $len)
            (tuple.extract 3 0 (local.get $np))
            (tuple.extract 3 1 (local.get $np))
            (tuple.extract 3 2 (local.get $np))))
      (call $free (local.get $op))
      (call $free (tuple.extract 3 1 (local.get $np)))
      (if (local.get $res)
         (then
            (call $caml_unix_error
               (local.get $res) (global.get $symlink) (local.get $o))))
      (ref.i31 (i32.const 0)))
)
(@else
   (func (export "unix_symlink") (export "caml_unix_symlink")
      (param $to_dir (ref eq)) (param $t (ref eq)) (param $p (ref eq))
      (result (ref eq))
      (local $kind i32)
      (if (ref.test (ref $block) (local.get $to_dir))
         (then
            (local.set $kind
               (i32.add (i32.const 1)
                  (i31.get_s
                     (ref.cast (ref i31)
                        (array.get $block
                           (ref.cast (ref $block) (local.get $to_dir))
                           (i32.const 0))))))))
      (try
         (do
            (call $symlink
               (call $unwrap (call $caml_jsstring_of_string (local.get $t)))
               (call $unwrap (call $caml_jsstring_of_string (local.get $p)))
               (local.get $kind)))
         (catch $javascript_exception
            (call $caml_unix_error (pop externref) (ref.null eq))))
      (ref.i31 (i32.const 0)))
))

(@if wasi
(@then
   (@string $readlink "readlink")

   (func (export "unix_readlink") (export "caml_unix_readlink")
      (param $path (ref eq)) (result (ref eq))
      (local $p (tuple i32 i32 i32))
      (local $buffer i32) (local $buf i32) (local $res i32)
      (local.set $p
         (call $unix_resolve_path (global.get $readlink) (local.get $path)))
      (local.set $buffer (call $get_buffer))
      (local.set $buf (i32.add (local.get $buffer) (i32.const 4)))
      (local.set $res
         (call $path_readlink
            (tuple.extract 3 0 (local.get $p))
            (tuple.extract 3 1 (local.get $p))
            (tuple.extract 3 2 (local.get $p))
            (local.get $buf)
            (global.get $IO_BUFFER_SIZE)
            (local.get $buffer)))
      (call $free (tuple.extract 3 1 (local.get $p)))
      (if (local.get $res)
         (then
            (call $caml_unix_error
               (local.get $res) (global.get $readlink) (local.get $path))))
      (return_call $blit_memory_to_string
         (local.get $buf) (i32.load (local.get $buffer))))
)
(@else
   (func (export "unix_readlink") (export "caml_unix_readlink")
      (param $path (ref eq)) (result (ref eq))
      (try
         (do
            (return_call $caml_string_of_jsstring
               (call $wrap
                  (call $readlink
                     (call $unwrap
                        (call $caml_jsstring_of_string (local.get $path)))))))
         (catch $javascript_exception
            (call $caml_unix_error (pop externref) (ref.null eq))))
      (ref.i31 (i32.const 0)))
))

(@if wasi
(@then
   (@string $truncate "truncate")

   (func $truncate (param $path (ref eq)) (param $len i64) (result (ref eq))
      (local $p (tuple i32 i32 i32))
      (local $fd i32) (local $res i32) (local $buffer i32)
      (block $error
         (local.set $p
            (call $unix_resolve_path (global.get $truncate) (local.get $path)))
         (local.set $buffer (call $get_buffer))
         (local.set $res
            (call $path_open
               (tuple.extract 3 0 (local.get $p))
               (i32.const 1) ;; symlink_follow
               (tuple.extract 3 1 (local.get $p))
               (tuple.extract 3 2 (local.get $p))
               (i32.const 0)
               (i64.const 0x400040) ;; allow fd_filestat_set_size and fd_write
               (i64.const 0)
               (i32.const 0)
               (local.get $buffer)))
         (call $free (tuple.extract 3 1 (local.get $p)))
         (br_if $error (local.get $res))
         (local.set $fd (i32.load (local.get $buffer)))
         (local.set $res
            (call $fd_filestat_set_size (local.get $fd) (local.get $len)))
         (if (local.get $res)
            (then
               (drop (call $fd_close (local.get $fd)))
               (br $error)))
         (local.set $res (call $fd_close (local.get $fd)))
         (br_if $error (local.get $res))
         (return (ref.i31 (i32.const 0))))
      (call $caml_unix_error
         (local.get $res) (global.get $truncate) (local.get $path))
      (return (ref.i31 (i32.const 0))))

   (func (export "unix_truncate") (export "caml_unix_truncate")
      (param $path (ref eq)) (param $len (ref eq))
      (result (ref eq))
      (return_call $truncate (local.get $path)
         (i64.extend_i32_s
            (i31.get_s (ref.cast (ref i31) (local.get $len))))))

   (func (export "unix_truncate_64") (export "caml_unix_truncate_64")
      (param $path (ref eq)) (param $len (ref eq))
      (result (ref eq))
      (return_call $truncate (local.get $path)
         (call $Int64_val (local.get $len))))
)
(@else
   (func (export "unix_truncate") (export "caml_unix_truncate")
      (param $path (ref eq)) (param $len (ref eq))
      (result (ref eq))
      (try
         (do
            (call $truncate
               (call $unwrap (call $caml_jsstring_of_string (local.get $path)))
               (local.get $len)))
         (catch $javascript_exception
            (call $caml_unix_error (pop externref) (ref.null eq))))
      (ref.i31 (i32.const 0)))

   (func (export "unix_truncate_64") (export "caml_unix_truncate_64")
      (param $path (ref eq)) (param $vlen (ref eq))
      (result (ref eq))
      (local $len i64)
      (local.set $len (call $Int64_val (local.get $vlen)))
      (try
         (do
            (call $truncate_64
               (call $unwrap (call $caml_jsstring_of_string (local.get $path)))
               (f64.convert_i64_s (local.get $len))))
         (catch $javascript_exception
            (call $caml_unix_error (pop externref) (ref.null eq))))
      (ref.i31 (i32.const 0)))
))

(@if wasi
(@then
   (func $ftruncate (param $vfd (ref eq)) (param $len i64) (result (ref eq))
      (local $fd i32) (local $res i32)
      (local.set $fd (i31.get_u (ref.cast (ref i31) (local.get $vfd))))
      (local.set $res
         (call $fd_filestat_set_size (local.get $fd) (local.get $len)))
      (if (local.get $res)
         (then
            (call $caml_unix_error
               (local.get $res) (@string "ftruncate") (global.get $no_arg))))
      (ref.i31 (i32.const 0)))

   (func (export "unix_ftruncate") (export "caml_unix_ftruncate")
      (param $fd (ref eq)) (param $len (ref eq)) (result (ref eq))
      (return_call $ftruncate (local.get $fd)
         (i64.extend_i32_s
            (i31.get_s (ref.cast (ref i31) (local.get $len))))))

   (func (export "unix_ftruncate_64") (export "caml_unix_ftruncate_64")
      (param $fd (ref eq)) (param $len (ref eq)) (result (ref eq))
      (return_call $ftruncate (local.get $fd)
         (call $Int64_val (local.get $len))))
)
(@else
   (func (export "unix_ftruncate") (export "caml_unix_ftruncate")
      (param $fd (ref eq)) (param $vlen (ref eq))
      (result (ref eq))
      (local $fd_offset (ref $fd_offset))
      (local $len i64)
      (try
         (do
            (call $ftruncate (local.get $fd) (local.get $vlen)))
         (catch $javascript_exception
            (call $caml_unix_error (pop externref) (ref.null eq))))
      (local.set $len
         (i64.extend_i32_s
            (i31.get_s (ref.cast (ref i31) (local.get $vlen)))))
      ;; node truncates to 0 without failure when $len < 0
      (if (i64.lt_s (local.get $len (i64.const 0)))
         (then (local.set $len (i64.const 0))))
      (local.set $fd_offset
         (call $get_fd_offset (i31.get_u (ref.cast (ref i31) (local.get $fd)))))
      (if (i64.gt_s (struct.get $fd_offset $offset (local.get $fd_offset))
             (local.get $len))
          (then
             (struct.set $fd_offset $offset (local.get $fd_offset)
                (local.get $len))))
      (ref.i31 (i32.const 0)))

   (func (export "unix_ftruncate_64") (export "caml_unix_ftruncate_64")
      (param $fd (ref eq)) (param $vlen (ref eq))
      (result (ref eq))
      (local $fd_offset (ref $fd_offset))
      (local $len i64)
      (local.set $len (call $Int64_val (local.get $vlen)))
      (try
         (do
            (call $ftruncate_64
               (local.get $fd) (f64.convert_i64_s (local.get $len))))
         (catch $javascript_exception
            (call $caml_unix_error (pop externref) (ref.null eq))))
      ;; node truncates to 0 without failure when $len < 0
      (if (i64.lt_s (local.get $len (i64.const 0)))
         (then (local.set $len (i64.const 0))))
      (local.set $fd_offset
         (call $get_fd_offset (i31.get_u (ref.cast (ref i31) (local.get $fd)))))
      (if (i64.gt_s (struct.get $fd_offset $offset (local.get $fd_offset))
             (local.get $len))
          (then
             (struct.set $fd_offset $offset (local.get $fd_offset)
                (local.get $len))))
      (ref.i31 (i32.const 0)))
))

(@if wasi
(@then
   (@string $access "access")

   ;; We can only check that the file exists
   (func (export "unix_access") (export "caml_unix_access")
      (param $path (ref eq)) (param $flags (ref eq)) (result (ref eq))
      (local $p (tuple i32 i32 i32))
      (local $res i32) (local $buffer i32)
      (local.set $p
         (call $unix_resolve_path (global.get $access) (local.get $path)))
      (local.set $buffer (call $get_buffer))
      (local.set $res
         (call $path_filestat_get
            (tuple.extract 3 0 (local.get $p))
            (i32.const 1)
            (tuple.extract 3 1 (local.get $p))
            (tuple.extract 3 2 (local.get $p))
            (local.get $buffer)))
      (call $free (tuple.extract 3 1 (local.get $p)))
      (if (local.get $res)
         (then
            (call $caml_unix_error
               (local.get $res) (global.get $access) (local.get $path))))
      (return (ref.i31 (i32.const 0))))
)
(@else
   (global $access_flags (ref $flags)
      (array.new_fixed $flags 4
         (i32.const 1) (i32.const 2) (i32.const 4) (i32.const 8)))

   (func (export "unix_access") (export "caml_unix_access")
      (param $path (ref eq)) (param $vflags (ref eq)) (result (ref eq))
      (local $flags i32)
      (local.set $flags
         (call $convert_flag_list
            (global.get $access_flags) (local.get $vflags)))
      (try
         (do
            (call $access
               (call $unwrap (call $caml_jsstring_of_string (local.get $path)))
               (local.get $flags)))
         (catch $javascript_exception
            (call $caml_unix_error (pop externref) (ref.null eq))))
      (ref.i31 (i32.const 0)))
))

   (type $flags (array i16))

(@if wasi
(@then
   ;;    0x1 O_RDONLY
   ;;    0x2 O_WRONLY
   ;;    0x3 O_RDWR
   ;;  0x400 O_NONBLOCK
   ;;  0x100 O_APPEND
   ;;   0x10 O_CREAT
   ;;   0x80 O_TRUNC
   ;;   0x40 O_EXCL
   ;;      0 O_NOCTTY
   ;;  0x200 O_DSYNC
   ;; 0x1000 O_SYNC
   ;;  0x800 O_RSYNC
   (global $unix_open_flags (ref $flags)
      (array.new_fixed $flags 15
         (i32.const 1) (i32.const 2) (i32.const 3) (i32.const 0x400)
         (i32.const 0x100) (i32.const 0x10) (i32.const 0x80) (i32.const 0x40)
         (i32.const 0) (i32.const 0x200) (i32.const 0x1000) (i32.const 0x800)
         (i32.const 0) (i32.const 0) (i32.const 0)))

   (@string $open "open")

   (func (export "unix_open") (export "caml_unix_open")
      (param $vpath (ref eq)) (param $vflags (ref eq)) (param $perm (ref eq))
      (result (ref eq))
      (local $flags i32) (local $offset i64)
      (local $path (tuple i32 i32 i32))
      (local $res i32) (local $buffer i32)
      (local.set $path
         (call $unix_resolve_path (global.get $open) (local.get $vpath)))
      (local.set $buffer (call $get_buffer))
      (local.set $flags
         (call $convert_flag_list
            (global.get $unix_open_flags) (local.get $vflags)))
      (local.set $res
         (call $path_open
            (tuple.extract 3 0 (local.get $path))
            (i32.const 1) ;; symlink_follow
            (tuple.extract 3 1 (local.get $path))
            (tuple.extract 3 2 (local.get $path))
            (i32.and (i32.shr_u (local.get $flags) (i32.const 4))
              (i32.const 0xF))
            (select
               (i64.const 0x860007e)
               (select (i64.const 0x860007c) (i64.const 0x820003e)
                  (i32.and (local.get $flags) (i32.const 2)))
               (i32.eq (i32.and (local.get $flags) (i32.const 3)) (i32.const 3)))
            (i64.const 0)
            (i32.shr_u (local.get $flags) (i32.const 8))
            (local.get $buffer)))
      (call $free (tuple.extract 3 1 (local.get $path)))
      (if (local.get $res)
         (then
            (call $caml_unix_error
               (local.get $res) (global.get $open) (local.get $vpath))))
      (ref.i31 (i32.load (local.get $buffer))))
)
(@else
   ;;    1 O_RDONLY
   ;;    2 O_WRONLY
   ;;    4 O_RDWR
   ;;    8 O_APPEND
   ;;   16 O_CREAT
   ;;   32 O_TRUNC
   ;;   64 O_EXCL
   ;;  128 O_NONBLOCK
   ;;  256 O_NOCTTY
   ;;  512 O_DSYNC
   ;; 1024 O_SYNC
   (global $unix_open_flags (ref $flags)
      (array.new_fixed $flags 15
         (i32.const 1) (i32.const 2) (i32.const 4) (i32.const 128)
         (i32.const 8) (i32.const 16) (i32.const 32) (i32.const 64)
         (i32.const 256) (i32.const 512) (i32.const 1024) (i32.const 0)
         (i32.const 0) (i32.const 0) (i32.const 0)))

   (func (export "unix_open") (export "caml_unix_open")
      (param $path (ref eq)) (param $vflags (ref eq)) (param $perm (ref eq))
      (result (ref eq))
      (local $fd i32) (local $flags i32) (local $offset i64)
      (local.set $flags
         (call $convert_flag_list
            (global.get $unix_open_flags) (local.get $vflags)))
      (try
         (do
            (local.set $fd
               (call $open
                  (call $unwrap
                     (call $caml_jsstring_of_string (local.get $path)))
                  (local.get $flags)
                  (i31.get_u (ref.cast (ref i31) (local.get $perm)))))
            (if (i32.and (local.get $flags) (i32.const 4)) ;; O_APPEND
               (then (local.set $offset (call $file_size (local.get $fd))))))
         (catch $javascript_exception
            (call $caml_unix_error (pop externref) (ref.null eq))))
      (call $initialize_fd_offset (local.get $fd) (local.get $offset))
      (ref.i31 (local.get $fd)))
))

   (global $io_buffer (mut externref) (ref.null extern))

   (func $get_io_buffer (result (ref extern))
      (if (ref.is_null (global.get $io_buffer))
         (then
            (global.set $io_buffer (call $ta_new (global.get $IO_BUFFER_SIZE)))))
      (ref.as_non_null (global.get $io_buffer)))

   (func $get_fd_offset (param $fd i32) (result (ref $fd_offset))
      (block $null
         (return
            (br_on_null $null (call $get_fd_offset_unchecked (local.get $fd)))))
      (struct.new $fd_offset (i64.const 0) (i32.const 0)))

(@if wasi
(@then
   (func (export "unix_write") (export "caml_unix_write")
      (param $vfd (ref eq)) (param $vbuf (ref eq)) (param $vpos (ref eq))
      (param $vlen (ref eq)) (result (ref eq))
      (local $buffer i32) (local $res i32)
      (local $iovs i32) (local $iovs_len i32) (local $nwritten i32)
      (local $fd i32) (local $s (ref $bytes))
      (local $pos i32) (local $len i32) (local $numbytes i32)
      (local $written i32) (local $n i32)
      (local.set $fd (i31.get_u (ref.cast (ref i31) (local.get $vfd))))
      (local.set $s (ref.cast (ref $bytes) (local.get $vbuf)))
      (local.set $pos (i31.get_u (ref.cast (ref i31) (local.get $vpos))))
      (local.set $len (i31.get_u (ref.cast (ref i31) (local.get $vlen))))
      (local.set $buffer (call $get_buffer))
      (local.set $nwritten (local.get $buffer))
      (local.set $iovs (i32.add (local.get $buffer) (i32.const 4)))
      (local.set $buffer (i32.add (local.get $buffer) (i32.const 12)))
      (i32.store (local.get $iovs) (local.get $buffer))
      (local.set $iovs_len (i32.const 1))
      (loop $loop
         (if (i32.gt_u (local.get $len) (i32.const 0))
            (then
               (local.set $numbytes
                  (select (global.get $IO_BUFFER_SIZE) (local.get $len)
                     (i32.gt_u (local.get $len) (global.get $IO_BUFFER_SIZE))))
               (call $blit_substring_to_memory
                  (local.get $buffer) (local.get $s) (local.get $pos)
                  (local.get $numbytes))
               (i32.store offset=4 (local.get $iovs) (local.get $numbytes))
               (local.set $res
                  (call $fd_write
                      (local.get $fd) (local.get $iovs) (local.get $iovs_len)
                      (local.get $nwritten)))
               (if (local.get $res)
                  (then
                     (call $caml_unix_error (local.get $res) (@string "write")
                        (global.get $no_arg))))
               (local.set $n (i32.load (local.get $nwritten)))
               (local.set $written (i32.add (local.get $written) (local.get $n)))
               (local.set $pos (i32.add (local.get $pos) (local.get $n)))
               (local.set $len (i32.sub (local.get $len) (local.get $n)))
               (br $loop))))
      (ref.i31 (local.get $n)))

   (func (export "unix_single_write") (export "caml_unix_single_write")
      (param $vfd (ref eq)) (param $vbuf (ref eq)) (param $vpos (ref eq))
      (param $vlen (ref eq)) (result (ref eq))
      (local $buffer i32) (local $res i32)
      (local $iovs i32) (local $iovs_len i32) (local $nwritten i32)
      (local $fd i32) (local $s (ref $bytes))
      (local $pos i32) (local $len i32) (local $numbytes i32)
      (local $written i32) (local $n i32)
      (local.set $fd (i31.get_u (ref.cast (ref i31) (local.get $vfd))))
      (local.set $s (ref.cast (ref $bytes) (local.get $vbuf)))
      (local.set $pos (i31.get_u (ref.cast (ref i31) (local.get $vpos))))
      (local.set $len (i31.get_u (ref.cast (ref i31) (local.get $vlen))))
      (if (i32.eqz (local.get $len))
         (then (return (ref.i31 (i32.const 0)))))
      (local.set $buffer (call $get_buffer))
      (local.set $nwritten (local.get $buffer))
      (local.set $iovs (i32.add (local.get $buffer) (i32.const 4)))
      (local.set $buffer (i32.add (local.get $buffer) (i32.const 12)))
      (i32.store (local.get $iovs) (local.get $buffer))
      (local.set $iovs_len (i32.const 1))
      (local.set $numbytes
         (select (global.get $IO_BUFFER_SIZE) (local.get $len)
            (i32.gt_u (local.get $len) (global.get $IO_BUFFER_SIZE))))
      (call $blit_substring_to_memory
         (local.get $buffer) (local.get $s) (local.get $pos)
         (local.get $numbytes))
      (i32.store offset=4 (local.get $iovs) (local.get $numbytes))
      (local.set $res
         (call $fd_write
             (local.get $fd) (local.get $iovs) (local.get $iovs_len)
             (local.get $nwritten)))
      (if (local.get $res)
         (then
            (call $caml_unix_error (local.get $res) (@string "write")
               (global.get $no_arg))))
      (ref.i31 (i32.load (local.get $nwritten))))

   (func (export "unix_read") (export "caml_unix_read")
      (param $vfd (ref eq)) (param $vbuf (ref eq)) (param $vpos (ref eq))
      (param $vlen (ref eq)) (result (ref eq))
      (local $buffer i32) (local $res i32)
      (local $iovs i32) (local $iovs_len i32) (local $nread i32)
      (local $fd i32) (local $pos i32) (local $len i32) (local $n i32)
      (local.set $fd (i31.get_u (ref.cast (ref i31) (local.get $vfd))))
      (local.set $pos (i31.get_u (ref.cast (ref i31) (local.get $vpos))))
      (local.set $len (i31.get_u (ref.cast (ref i31) (local.get $vlen))))
      (if (i32.gt_u (local.get $len) (global.get $IO_BUFFER_SIZE))
         (then
            (local.set $len (global.get $IO_BUFFER_SIZE))))
      (local.set $buffer (call $get_buffer))
      (local.set $nread (local.get $buffer))
      (local.set $iovs (i32.add (local.get $buffer) (i32.const 4)))
      (local.set $buffer (i32.add (local.get $buffer) (i32.const 12)))
      (i32.store (local.get $iovs) (local.get $buffer))
      (i32.store offset=4 (local.get $iovs) (local.get $len))
      (local.set $iovs_len (i32.const 1))
      (local.set $res
         (call $fd_read
             (local.get $fd) (local.get $iovs) (local.get $iovs_len)
             (local.get $nread)))
      (if (local.get $res)
         (then
            (call $caml_unix_error (local.get $res) (@string "read")
               (global.get $no_arg))))
      (local.set $n (i32.load (local.get $nread)))
      (call $blit_memory_to_substring (local.get $buffer)
         (ref.cast (ref $bytes) (local.get $vbuf))
         (local.get $pos) (local.get $n))
      (ref.i31 (local.get $n)))

   (type $data
      (struct
         (field $array (ref array))
         (field $offset i32)
         (field $len i32)))

   (func (export "unix_write_bigarray") (export "caml_unix_write_bigarray")
      (param $vfd (ref eq)) (param $vbuf (ref eq)) (param $vpos (ref eq))
      (param $vlen (ref eq)) (param $vsingle (ref eq)) (result (ref eq))
      (local $fd i32) (local $data (ref $data)) (local $buf (ref $bytes))
      (local $pos i32) (local $len i32) (local $n i32) (local $written i32)
      (local $buffer i32) (local $nwritten i32) (local $iovs i32)
      (local $iovs_len i32) (local $numbytes i32) (local $res i32)
      (local.set $fd (i31.get_u (ref.cast (ref i31) (local.get $vfd))))
      (local.set $data
         (ref.cast (ref $data)
            (any.convert_extern (call $caml_ba_get_data (local.get $vbuf)))))
      (local.set $buf
         (ref.cast (ref $bytes) (struct.get $data $array (local.get $data))))
      (local.set $pos
          (i32.add (i31.get_u (ref.cast (ref i31) (local.get $vpos)))
             (struct.get $data $offset (local.get $data))))
      (local.set $len (i31.get_u (ref.cast (ref i31) (local.get $vlen))))
      (local.set $buffer (call $get_buffer))
      (local.set $nwritten (local.get $buffer))
      (local.set $iovs (i32.add (local.get $buffer) (i32.const 4)))
      (local.set $buffer (i32.add (local.get $buffer) (i32.const 12)))
      (i32.store (local.get $iovs) (local.get $buffer))
      (local.set $iovs_len (i32.const 1))
      (loop $loop
         (if (i32.gt_u (local.get $len) (i32.const 0))
            (then
               (local.set $numbytes
                  (select (global.get $IO_BUFFER_SIZE) (local.get $len)
                     (i32.gt_u (local.get $len) (global.get $IO_BUFFER_SIZE))))
               (call $blit_substring_to_memory
                  (local.get $buffer) (local.get $buf) (local.get $pos)
                  (local.get $numbytes))
               (i32.store offset=4 (local.get $iovs) (local.get $numbytes))
               (local.set $res
                  (call $fd_write
                      (local.get $fd) (local.get $iovs) (local.get $iovs_len)
                      (local.get $nwritten)))
               (if (local.get $res)
                  (then
                     (call $caml_unix_error (local.get $res) (@string "write")
                        (global.get $no_arg))))
               (local.set $n (i32.load (local.get $nwritten)))
               (local.set $written (i32.add (local.get $written) (local.get $n)))
               (local.set $pos (i32.add (local.get $pos) (local.get $n)))
               (local.set $len (i32.sub (local.get $len) (local.get $n)))
               (br_if $loop
                  (ref.eq (local.get $vsingle) (ref.i31 (i32.const 0)))))))
      (ref.i31 (local.get $written)))

   (func (export "unix_read_bigarray") (export "caml_unix_read_bigarray")
      (param $vfd (ref eq)) (param $vbuf (ref eq)) (param $vpos (ref eq))
      (param $vlen (ref eq)) (result (ref eq))
      (local $fd i32) (local $data (ref $data)) (local $buf (ref $bytes))
      (local $pos i32) (local $len i32) (local $n i32)
      (local $buffer i32) (local $nread i32) (local $iovs i32)
      (local $iovs_len i32) (local $res i32)
      (local.set $fd (i31.get_u (ref.cast (ref i31) (local.get $vfd))))
      (local.set $data
         (ref.cast (ref $data)
            (any.convert_extern (call $caml_ba_get_data (local.get $vbuf)))))
      (local.set $buf
         (ref.cast (ref $bytes) (struct.get $data $array (local.get $data))))
      (local.set $pos
          (i32.add (i31.get_u (ref.cast (ref i31) (local.get $vpos)))
             (struct.get $data $offset (local.get $data))))
      (local.set $len (i31.get_u (ref.cast (ref i31) (local.get $vlen))))
      (if (i32.gt_u (local.get $len) (global.get $IO_BUFFER_SIZE))
         (then
            (local.set $len (global.get $IO_BUFFER_SIZE))))
      (local.set $buffer (call $get_buffer))
      (local.set $nread (local.get $buffer))
      (local.set $iovs (i32.add (local.get $buffer) (i32.const 4)))
      (local.set $buffer (i32.add (local.get $buffer) (i32.const 12)))
      (i32.store (local.get $iovs) (local.get $buffer))
      (i32.store offset=4 (local.get $iovs) (local.get $len))
      (local.set $iovs_len (i32.const 1))
      (local.set $res
         (call $fd_read
             (local.get $fd) (local.get $iovs) (local.get $iovs_len)
             (local.get $nread)))
      (if (local.get $res)
         (then
            (call $caml_unix_error (local.get $res) (@string "read")
               (global.get $no_arg))))
      (local.set $n (i32.load (local.get $nread)))
      (call $blit_memory_to_substring (local.get $buffer)
         (local.get $buf) (local.get $pos) (local.get $n))
      (ref.i31 (local.get $n)))
)
(@else
   (func (export "unix_write") (export "caml_unix_write")
      (param $vfd (ref eq)) (param $vbuf (ref eq)) (param $vpos (ref eq))
      (param $vlen (ref eq)) (result (ref eq))
      (local $fd i32) (local $s (ref $bytes)) (local $buf (ref extern))
      (local $pos i32) (local $len i32) (local $numbytes i32)
      (local $written i32) (local $n i32)
      (local $fd_offset (ref $fd_offset))
      (local $offset i64)
      (local.set $fd (i31.get_u (ref.cast (ref i31) (local.get $vfd))))
      (local.set $s (ref.cast (ref $bytes) (local.get $vbuf)))
      (local.set $pos (i31.get_u (ref.cast (ref i31) (local.get $vpos))))
      (local.set $len (i31.get_u (ref.cast (ref i31) (local.get $vlen))))
      (local.set $buf (call $get_io_buffer))
      (local.set $fd_offset (call $get_fd_offset (local.get $fd)))
      (local.set $offset
         (struct.get $fd_offset $offset (local.get $fd_offset)))
      (loop $loop
         (if (i32.gt_u (local.get $len) (i32.const 0))
            (then
               (local.set $numbytes
                  (select (global.get $IO_BUFFER_SIZE) (local.get $len)
                     (i32.gt_u (local.get $len) (global.get $IO_BUFFER_SIZE))))
               (call $ta_blit_from_bytes
                  (local.get $s) (local.get $pos)
                  (local.get $buf) (i32.const 0) (local.get $numbytes))
               (try
                  (do
                     (local.set $n
                        (if (result i32)
                            (struct.get $fd_offset $seeked (local.get $fd_offset))
                           (then
                              (call $write (local.get $fd) (local.get $buf)
                                 (i32.const 0) (local.get $numbytes)
                                 (local.get $offset)))
                           (else
                              (call $write' (local.get $fd) (local.get $buf)
                                 (i32.const 0) (local.get $numbytes)
                                 (ref.null extern))))))
                  (catch $javascript_exception
                     (call $caml_unix_error (pop externref) (ref.null eq))))
               (local.set $offset
                  (i64.add (local.get $offset)
                     (i64.extend_i32_u (local.get $n))))
               (struct.set $fd_offset $offset
                  (local.get $fd_offset) (local.get $offset))
               (local.set $written (i32.add (local.get $written) (local.get $n)))
               (local.set $pos (i32.add (local.get $pos) (local.get $n)))
               (local.set $len (i32.sub (local.get $len) (local.get $n)))
               (br $loop))))
      (ref.i31 (local.get $n)))

   (func (export "unix_single_write") (export "caml_unix_single_write")
      (param $vfd (ref eq)) (param $vbuf (ref eq)) (param $vpos (ref eq))
      (param $vlen (ref eq)) (result (ref eq))
      (local $fd i32) (local $s (ref $bytes)) (local $buf (ref extern))
      (local $pos i32) (local $len i32) (local $numbytes i32)
      (local $written i32) (local $n i32)
      (local $fd_offset (ref $fd_offset))
      (local $offset i64)
      (local.set $fd (i31.get_u (ref.cast (ref i31) (local.get $vfd))))
      (local.set $s (ref.cast (ref $bytes) (local.get $vbuf)))
      (local.set $pos (i31.get_u (ref.cast (ref i31) (local.get $vpos))))
      (local.set $len (i31.get_u (ref.cast (ref i31) (local.get $vlen))))
      (local.set $buf (call $get_io_buffer))
      (local.set $fd_offset (call $get_fd_offset (local.get $fd)))
      (local.set $offset
         (struct.get $fd_offset $offset (local.get $fd_offset)))
      (if (i32.gt_u (local.get $len) (i32.const 0))
         (then
            (local.set $numbytes
               (select (global.get $IO_BUFFER_SIZE) (local.get $len)
                  (i32.gt_u (local.get $len) (global.get $IO_BUFFER_SIZE))))
            (call $ta_blit_from_bytes
               (local.get $s) (local.get $pos)
               (local.get $buf) (i32.const 0) (local.get $numbytes))
            (try
               (do
                  (local.set $n
                     (if (result i32)
                         (struct.get $fd_offset $seeked (local.get $fd_offset))
                        (then
                           (call $write (local.get $fd) (local.get $buf)
                              (i32.const 0) (local.get $numbytes)
                              (local.get $offset)))
                        (else
                           (call $write' (local.get $fd) (local.get $buf)
                              (i32.const 0) (local.get $numbytes)
                              (ref.null extern))))))
               (catch $javascript_exception
                  (call $caml_unix_error (pop externref) (ref.null eq))))
            (local.set $offset
               (i64.add (local.get $offset) (i64.extend_i32_u (local.get $n))))
            (struct.set $fd_offset $offset
               (local.get $fd_offset) (local.get $offset))))
      (ref.i31 (local.get $n)))

   (func (export "unix_read") (export "caml_unix_read")
      (param $vfd (ref eq)) (param $vbuf (ref eq)) (param $vpos (ref eq))
      (param $vlen (ref eq)) (result (ref eq))
      (local $fd i32) (local $buf (ref extern))
      (local $pos i32) (local $len i32) (local $n i32)
      (local $fd_offset (ref $fd_offset)) (local $offset i64)
      (local.set $fd (i31.get_u (ref.cast (ref i31) (local.get $vfd))))
      (local.set $pos (i31.get_u (ref.cast (ref i31) (local.get $vpos))))
      (local.set $len (i31.get_u (ref.cast (ref i31) (local.get $vlen))))
      (if (i32.gt_u (local.get $len) (global.get $IO_BUFFER_SIZE))
         (then
            (local.set $len (global.get $IO_BUFFER_SIZE))))
      (local.set $buf (call $get_io_buffer))
      (local.set $fd_offset (call $get_fd_offset (local.get $fd)))
      (try
         (do
            (local.set $n
               (if (result i32)
                   (struct.get $fd_offset $seeked (local.get $fd_offset))
                  (then
                     (call $read (local.get $fd) (local.get $buf)
                        (i32.const 0) (local.get $len) (local.get $offset)))
                  (else
                     (call $read' (local.get $fd) (local.get $buf)
                        (i32.const 0) (local.get $len) (ref.null extern))))))
         (catch $javascript_exception
            (call $caml_unix_error (pop externref) (ref.null eq))))
      (call $ta_blit_to_bytes
         (local.get $buf) (i32.const 0)
         (ref.cast (ref $bytes) (local.get $vbuf)) (local.get $pos)
         (local.get $n))
      (ref.i31 (local.get $n)))

   (func (export "unix_write_bigarray") (export "caml_unix_write_bigarray")
      (param $vfd (ref eq)) (param $vbuf (ref eq)) (param $vpos (ref eq))
      (param $vlen (ref eq)) (param $vsingle (ref eq)) (result (ref eq))
      (local $fd i32) (local $buf (ref extern))
      (local $pos i32) (local $len i32) (local $n i32) (local $written i32)
      (local $fd_offset (ref $fd_offset))
      (local $offset i64)
      (local.set $fd (i31.get_u (ref.cast (ref i31) (local.get $vfd))))
      (local.set $buf (call $caml_ba_get_data (local.get $vbuf)))
      (local.set $pos (i31.get_u (ref.cast (ref i31) (local.get $vpos))))
      (local.set $len (i31.get_u (ref.cast (ref i31) (local.get $vlen))))
      (local.set $fd_offset (call $get_fd_offset (local.get $fd)))
      (loop $loop
         (if (i32.gt_u (local.get $len) (i32.const 0))
            (then
               (try
                  (do
                     (local.set $n
                        (if (result i32)
                            (struct.get $fd_offset $seeked
                               (local.get $fd_offset))
                           (then
                              (call $write (local.get $fd) (local.get $buf)
                                 (local.get $pos) (local.get $len)
                                 (local.get $offset)))
                           (else
                              (call $write' (local.get $fd) (local.get $buf)
                                 (local.get $pos) (local.get $len)
                                 (ref.null extern))))))
                  (catch $javascript_exception
                     (call $caml_unix_error (pop externref) (ref.null eq))))
               (local.set $offset
                  (i64.add (local.get $offset)
                     (i64.extend_i32_u (local.get $n))))
               (struct.set $fd_offset $offset
                  (local.get $fd_offset) (local.get $offset))
               (local.set $written (i32.add (local.get $written) (local.get $n)))
               (local.set $pos (i32.add (local.get $pos) (local.get $n)))
               (local.set $len (i32.sub (local.get $len) (local.get $n)))
               (br_if $loop
                  (ref.eq (local.get $vsingle) (ref.i31 (i32.const 0)))))))
      (ref.i31 (local.get $written)))

   (func (export "unix_read_bigarray") (export "caml_unix_read_bigarray")
      (param $vfd (ref eq)) (param $vbuf (ref eq)) (param $vpos (ref eq))
      (param $vlen (ref eq)) (result (ref eq))
      (local $fd i32) (local $buf (ref extern))
      (local $pos i32) (local $len i32) (local $n i32)
      (local $fd_offset (ref $fd_offset)) (local $offset i64)
      (local.set $fd (i31.get_u (ref.cast (ref i31) (local.get $vfd))))
      (local.set $buf (call $caml_ba_get_data (local.get $vbuf)))
      (local.set $pos (i31.get_u (ref.cast (ref i31) (local.get $vpos))))
      (local.set $len (i31.get_u (ref.cast (ref i31) (local.get $vlen))))
      (local.set $fd_offset (call $get_fd_offset (local.get $fd)))
      (try
         (do
            (local.set $n
               (if (result i32)
                   (struct.get $fd_offset $seeked (local.get $fd_offset))
                  (then
                     (call $read (local.get $fd) (local.get $buf)
                        (local.get $pos) (local.get $len) (local.get $offset)))
                  (else
                     (call $read' (local.get $fd) (local.get $buf)
                        (local.get $pos) (local.get $len) (ref.null extern))))))
         (catch $javascript_exception
            (call $caml_unix_error (pop externref) (ref.null eq))))
      (struct.set $fd_offset $offset (local.get $fd_offset)
         (i64.add (local.get $offset) (i64.extend_i32_s (local.get $n))))
      (ref.i31 (local.get $n)))
))

(@if wasi
(@then
   (func $lseek
      (param $fd (ref eq)) (param $offset i64) (param $cmd (ref eq))
      (result i64)
      (local $res i32) (local $buffer i32)
      (local.set $buffer (call $get_buffer))
      (local.set $res
         (call $fd_seek
            (i31.get_u (ref.cast (ref i31) (local.get $fd)))
            (local.get $offset)
            (i31.get_u (ref.cast (ref i31) (local.get $cmd)))
            (local.get $buffer)))
      (if (local.get $res)
         (then
            (call $caml_unix_error
               (local.get $res) (@string "lseek") (global.get $no_arg))))
      (i64.load (local.get $buffer)))
)
(@else
   (func $lseek_exn (param $errno i32) (result (ref eq))
      (array.new_fixed $block 5
         (ref.i31 (i32.const 0))
         (call $get_unix_error_exn)
         (ref.i31 (local.get $errno))
         (@string "lseek")
         (global.get $no_arg)))

   (func $lseek
      (param $vfd (ref eq)) (param $offset i64) (param $cmd (ref eq))
      (result i64)
      (local $fd i32) (local $fd_offset (ref $fd_offset))
      (local.set $fd (i31.get_u (ref.cast (ref i31) (local.get $vfd))))
      (local.set $fd_offset
         (block $non_null (result (ref $fd_offset))
            (br_on_non_null $non_null
               (call $get_fd_offset_unchecked (local.get $fd)))
            (throw $ocaml_exception (call $lseek_exn (i32.const 3))))) ;; EBADF
      (if (ref.eq (local.get $cmd) (ref.i31 (i32.const 1)))
         (then
            (local.set $offset
               (i64.add (struct.get $fd_offset $offset (local.get $fd_offset))
                  (local.get $offset))))
      (else (if (ref.eq (local.get $cmd) (ref.i31 (i32.const 2)))
         (then
            (local.set $offset
               (i64.add (call $file_size (local.get $fd))
                  (local.get $offset)))))))
      (if (i64.lt_s (local.get $offset) (i64.const 0))
         (then
            (throw $ocaml_exception
                (call $lseek_exn (i32.const 12))))) ;; EINVAL
      (struct.set $fd_offset $offset (local.get $fd_offset) (local.get $offset))
      (struct.set $fd_offset $seeked (local.get $fd_offset) (i32.const 1))
      (local.get $offset))
))

   (func (export "unix_lseek") (export "caml_unix_lseek")
      (param $fd (ref eq)) (param $ofs (ref eq)) (param $cmd (ref eq))
      (result (ref eq))
      (ref.i31
         (i32.wrap_i64
            (call $lseek
               (local.get $fd)
               (i64.extend_i32_s
                  (i31.get_s (ref.cast (ref i31) (local.get $ofs))))
               (local.get $cmd)))))

   (func (export "unix_lseek_64") (export "caml_unix_lseek_64")
      (param $fd (ref eq)) (param $ofs (ref eq)) (param $cmd (ref eq))
      (result (ref eq))
      (return_call $caml_copy_int64
         (call $lseek
            (local.get $fd)
            (call $Int64_val (local.get $ofs))
            (local.get $cmd))))

(@if wasi
(@then
   (func (export "unix_fsync") (export "caml_unix_fsync")
      (param $fd (ref eq)) (result (ref eq))
      (local $res i32)
      (local.set $res
         (call $fd_sync (i31.get_u (ref.cast (ref i31) (local.get $fd)))))
      (if (local.get $res)
         (then
            (call $caml_unix_error
               (local.get $res) (@string "fsync") (global.get $no_arg))))
      (ref.i31 (i32.const 0)))
)
(@else
   (func (export "unix_fsync") (export "caml_unix_fsync")
      (param $fd (ref eq)) (result (ref eq))
      (try
         (do
            (call $fsync (local.get $fd)))
         (catch $javascript_exception
            (call $caml_unix_error (pop externref) (ref.null eq))))
      (ref.i31 (i32.const 0)))
))

   (@string $out_channel_of_descr "out_channel_of_descr")
   (@string $in_channel_of_descr "in_channel_of_descr")

   (func $channel_of_descr_name (param $out i32) (result (ref eq))
      (select (result (ref eq))
         (global.get $out_channel_of_descr)
         (global.get $in_channel_of_descr)
         (local.get $out)))

(@if wasi
(@then
   (func $caml_unix_check_stream_semantics (param $fd (ref eq)) (param $out i32)
      (local $s (ref $block)) (local $kind i32)
      (local $buffer i32) (local $res i32) (local $file_type i32)
      (local.set $buffer (call $get_buffer))
      (local.set $res
         (call $fd_filestat_get
            (i31.get_u (ref.cast (ref i31) (local.get $fd)))
            (local.get $buffer)))
      (if (local.get $res)
         (then
            (call $caml_unix_error
               (local.get $res)
               (call $channel_of_descr_name (local.get $out))
               (global.get $no_arg))))
      (local.set $kind (i32.load8_u offset=16 (local.get $buffer)))
      (block $ok
         (block $bad
            (br_table $ok $bad $ok $bad $ok $bad $ok $bad (local.get $kind)))
         (call $caml_unix_error
            (i32.const 28) (; EINVAL ;)
            (call $channel_of_descr_name (local.get $out))
            (global.get $no_arg))))
)
(@else
   (func $caml_unix_check_stream_semantics (param $fd (ref eq)) (param $out i32)
      (local $s (ref $block)) (local $kind i32)
      (local.set $s
         (ref.cast (ref $block)
            (try (result (ref eq))
               (do
                  (call $fstat (local.get $fd) (i32.const 0)))
               (catch $javascript_exception
                  (call $caml_unix_error (pop externref)
                     (call $channel_of_descr_name (local.get $out)))
                  (ref.i31 (i32.const 0))))))
      (local.set $kind
         (i31.get_u
            (ref.cast (ref i31)
               (array.get $block (local.get $s) (i32.const 3)))))
      (block $ok
         (block $bad
            (br_table $ok $bad $ok $bad $bad $ok (local.get $kind)))
         (throw $ocaml_exception
            (array.new_fixed $block 5
               (ref.i31 (i32.const 0))
               (call $get_unix_error_exn)
               (ref.i31 (i32.const 12)) ;; EINVAL
               (call $channel_of_descr_name (local.get $out))
               (global.get $no_arg)))))
))

   (func (export "unix_inchannel_of_filedescr")
      (export "win_inchannel_of_filedescr")
      (export "caml_unix_inchannel_of_filedescr")
      (param $fd (ref eq)) (result (ref eq))
      (call $caml_unix_check_stream_semantics (local.get $fd) (i32.const 0))
      (return_call $caml_ml_open_descriptor_in (local.get $fd)))

   (func (export "unix_outchannel_of_filedescr")
      (export "win_outchannel_of_filedescr")
      (export "caml_unix_outchannel_of_filedescr")
      (param $fd (ref eq)) (result (ref eq))
      (call $caml_unix_check_stream_semantics (local.get $fd) (i32.const 1))
      (return_call $caml_ml_open_descriptor_out (local.get $fd)))

(@if wasi
(@then
   (func (export "unix_close") (export "caml_unix_close")
      (param $fd (ref eq)) (result (ref eq))
      (local $res i32)
      (local.set $res
         (call $fd_close (i31.get_u (ref.cast (ref i31) (local.get $fd)))))
      (if (local.get $res)
         (then
            (call $caml_unix_error
               (local.get $res) (@string "close") (global.get $no_arg))))
      (ref.i31 (i32.const 0)))
)
(@else
   (func (export "unix_close") (export "caml_unix_close")
      (param $fd (ref eq)) (result (ref eq))
      (call $release_fd_offset (i31.get_u (ref.cast (ref i31) (local.get $fd))))
      (try
         (do
            (call $close (local.get $fd)))
         (catch $javascript_exception
            (call $caml_unix_error (pop externref) (ref.null eq))))
      (ref.i31 (i32.const 0)))
))

(@if wasi
(@then
   (func (export "unix_isatty") (export "caml_unix_isatty")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))
)
(@else
   (export "unix_isatty" (func $isatty))
   (export "caml_unix_isatty" (func $isatty))
))

   (func (export "unix_getuid") (export "caml_unix_getuid")
      (export "unix_geteuid") (export "caml_unix_geteuid")
      (export "unix_getgid") (export "caml_unix_getgid")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 1)))

   (func (export "unix_getpwnam") (export "caml_unix_getpwnam")
      (export "unix_getpwuid") (export "caml_unix_getpwuid")
      (export "unix_getgrnam") (export "caml_unix_getgrnam")
      (export "unix_getgruid") (export "caml_unix_getgruid")
      (param (ref eq)) (result (ref eq))
      (call $caml_raise_not_found)
      (ref.i31 (i32.const 0)))

   (func (export "unix_inet_addr_of_string")
      (export "caml_unix_inet_addr_of_string")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "win_handle_fd") (export "caml_unix_filedescr_of_fd")
      (param (ref eq)) (result (ref eq))
      (local.get 0))

   (func (export "win_cleanup") (export "caml_unix_cleanup")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))
)
