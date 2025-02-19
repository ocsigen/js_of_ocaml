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
   (import "bindings" "gettimeofday" (func $gettimeofday (result f64)))
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
   (import "bindings" "rename" (func $rename (param anyref) (param anyref)))
   (import "bindings" "truncate" (func $truncate (param anyref (ref eq))))
   (import "bindings" "truncate" (func $truncate_64 (param anyref f64)))
   (import "bindings" "ftruncate" (func $ftruncate (param (ref eq) (ref eq))))
   (import "bindings" "ftruncate" (func $ftruncate_64 (param (ref eq) f64)))
   (import "bindings" "file_size" (func $file_size (param i32) (result i64)))
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
   (import "bindings" "close" (func $close (param (ref eq))))
   (import "bindings" "isatty"
      (func $isatty (param (ref eq)) (result (ref eq))))
   (import "js" "unix_error" (global $unix_error_js (ref any)))
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
   (import "stdlib" "caml_named_value"
      (func $caml_named_value (param (ref eq)) (result (ref null eq))))
   (import "fail" "ocaml_exception" (tag $ocaml_exception (param (ref eq))))
   (import "fail" "javascript_exception"
      (tag $javascript_exception (param externref)))
   (import "fail" "caml_invalid_argument"
      (func $caml_invalid_argument (param (ref eq))))
   (import "io" "convert_flag_list"
      (func $convert_flag_list (param (ref $open_flags) (ref eq)) (result i32)))
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
   (import "bindings" "ta_blit_from_string"
      (func $ta_blit_from_string
         (param (ref $bytes)) (param i32) (param (ref extern)) (param i32)
         (param i32)))
   (import "bindings" "ta_blit_to_string"
      (func $ta_blit_to_string
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
   (type $js (struct (field anyref)))

   (type $fd_offset
      (struct (field $offset (mut i64)) (field $seeked (mut i32))))

   (global $unix_error_exn (mut (ref eq)) (ref.i31 (i32.const 0)))

   (data $unix_error "Unix.Unix_error")

   (data $unix_error_not_initialized
      "Exception Unix.Unix_error not initialized, please link unix.cma")

   (func $get_unix_error_exn (result (ref eq))
      (local $unix_error_exn eqref)
      (if (ref.test (ref i31) (global.get $unix_error_exn))
         (then
            (local.set $unix_error_exn
               (call $caml_named_value
                  (array.new_data $bytes $unix_error
                     (i32.const 0) (i32.const 15))))
            (if (ref.is_null (local.get $unix_error_exn))
               (then
                  (call $caml_invalid_argument
                      (array.new_data $bytes $unix_error_not_initialized
                         (i32.const 0) (i32.const 63)))))
            (global.set $unix_error_exn
               (ref.as_non_null (local.get $unix_error_exn)))))
      (global.get $unix_error_exn))

   (global $no_arg (ref eq) (array.new_fixed $bytes 0))

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

   (data $code "code")
   (data $errno "errno")
   (data $indexOf "indexOf")
   (data $syscall "syscall")
   (data $path "path")

   (func $caml_unix_error (param $exception externref) (param $cmd eqref)
      (local $exn (ref eq))
      (local $code (ref eq))
      (local $errno (ref eq))
      (local $variant (ref eq))
      (local.set $exn (call $wrap (any.convert_extern (local.get $exception))))
      (local.set $code
         (call $caml_js_get (local.get $exn)
            (array.new_data $bytes $code (i32.const 0) (i32.const 4))))
      (local.set $variant
         (call $caml_js_meth_call (global.get $unix_error)
            (array.new_data $bytes $indexOf (i32.const 0) (i32.const 7))
            (array.new_fixed $block 2 (ref.i31 (i32.const 0))
               (local.get $code))))
      (if (ref.eq (local.get $variant) (ref.i31 (i32.const -1)))
         (then
            (local.set $errno
               (call $caml_js_get (local.get $exn)
                  (array.new_data $bytes $errno (i32.const 0) (i32.const 4))))
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
                        (array.new_data $bytes $syscall
                           (i32.const 0) (i32.const 7)))))
               (else
                  (ref.as_non_null (local.get $cmd))))
            (call $ensure_string
               (call $caml_js_get (local.get $exn)
                  (array.new_data $bytes $path (i32.const 0) (i32.const 4)))))))

   (export "caml_unix_gettimeofday" (func $unix_gettimeofday))
   (func $unix_gettimeofday (export "unix_gettimeofday")
      (param (ref eq)) (result (ref eq))
      (struct.new $float (call $gettimeofday)))

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

   (export "caml_unix_gmtime" (func $unix_gmtime))
   (func $unix_gmtime (export "unix_gmtime") (param (ref eq)) (result (ref eq))
      (call $gmtime
         (struct.get $float 0 (ref.cast (ref $float) (local.get 0)))))

   (export "caml_unix_localtime" (func $unix_localtime))
   (func $unix_localtime (export "unix_localtime")
      (param (ref eq)) (result (ref eq))
      (call $localtime
         (struct.get $float 0 (ref.cast (ref $float) (local.get 0)))))

   (export "caml_unix_time" (func $unix_time))
   (func $unix_time (export "unix_time") (param (ref eq)) (result (ref eq))
      (struct.new $float (f64.floor (call $gettimeofday))))

   (export "caml_unix_mktime" (func $unix_mktime))
   (func $unix_mktime (export "unix_mktime") (param (ref eq)) (result (ref eq))
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

   (type $open_flags (array i8))
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
   (global $unix_open_flags (ref $open_flags)
      (array.new_fixed $open_flags 15
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
               (call $ta_blit_from_string
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
      (call $ta_blit_to_string
         (local.get $buf) (i32.const 0)
         (ref.cast (ref $bytes) (local.get $vbuf)) (local.get $pos)
         (local.get $len))
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

   (data $lseek "lseek")

   (func $lseek_exn (param $errno i32) (result (ref eq))
      (array.new_fixed $block 5
         (ref.i31 (i32.const 0))
         (call $get_unix_error_exn)
         (ref.i31 (local.get $errno))
         (array.new_data $bytes $lseek (i32.const 0) (i32.const 5))
         (global.get $no_arg)))

   (func $lseek
      (param $vfd (ref eq)) (param $offset i64) (param $cmd (ref eq))
      (result (ref eq))
      (local $fd i32) (local $fd_offset (ref $fd_offset))
      (local.set $fd (i31.get_u (ref.cast (ref i31) (local.get $vfd))))
      (local.set $fd_offset
         (block $non_null (result (ref $fd_offset))
            (br_on_non_null $non_null
               (call $get_fd_offset_unchecked (local.get $fd)))
            (throw $ocaml_exception (call $lseek_exn (i32.const 9))))) ;; EBADF
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
      (ref.i31 (i32.const 0)))

   (func (export "unix_lseek") (export "caml_unix_lseek")
      (param $fd (ref eq)) (param $ofs (ref eq)) (param $cmd (ref eq))
      (result (ref eq))
      (return_call $lseek
         (local.get $fd)
         (i64.extend_i32_s (i31.get_s (ref.cast (ref i31) (local.get $ofs))))
         (local.get $cmd)))

   (func (export "unix_lseek_64") (export "caml_unix_lseek_64")
      (param $fd (ref eq)) (param $ofs (ref eq)) (param $cmd (ref eq))
      (result (ref eq))
      (return_call $lseek
         (local.get $fd)
         (call $Int64_val (local.get $ofs))
         (local.get $cmd)))

   (data $out_channel_of_descr "out_channel_of_descr")
   (data $in_channel_of_descr "in_channel_of_descr")

   (func $channel_of_descr_name (param $out i32) (result (ref eq))
      (if (result (ref eq)) (local.get $out)
         (then
            (array.new_data $bytes $out_channel_of_descr
               (i32.const 0) (i32.const 20)))
         (else
            (array.new_data $bytes $in_channel_of_descr
               (i32.const 0) (i32.const 19)))))

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

   (func (export "unix_close") (export "caml_unix_close")
      (param $fd (ref eq)) (result (ref eq))
      (call $release_fd_offset (i31.get_u (ref.cast (ref i31) (local.get $fd))))
      (try
         (do
            (call $close (local.get $fd)))
         (catch $javascript_exception
            (call $caml_unix_error (pop externref) (ref.null eq))))
      (ref.i31 (i32.const 0)))

   (export "unix_isatty" (func $isatty))
   (export "caml_unix_isatty" (func $isatty))

   (export "caml_unix_inet_addr_of_string" (func $unix_inet_addr_of_string))
   (func $unix_inet_addr_of_string (export "unix_inet_addr_of_string")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))
   (export "caml_unix_filedescr_of_fd" (func $unix_filedescr_of_fd))
   (func $unix_filedescr_of_fd (export "unix_filedescr_of_fd")
      (param (ref eq)) (result (ref eq))
      (local.get 0))
   (func $unix_cleanup (export "caml_unix_cleanup")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))
)
