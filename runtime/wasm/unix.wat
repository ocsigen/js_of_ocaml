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

   (type $block (array (mut (ref eq))))
   (type $float (struct (field f64)))

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
