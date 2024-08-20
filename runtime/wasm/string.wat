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
   (import "fail" "caml_bound_error" (func $caml_bound_error))
   (import "fail" "caml_invalid_argument"
      (func $caml_invalid_argument (param $arg (ref eq))))

   (type $bytes (array (mut i8)))

   (export "caml_bytes_equal" (func $caml_string_equal))
   (func $caml_string_equal (export "caml_string_equal")
      (param $p1 (ref eq)) (param $p2 (ref eq)) (result (ref eq))
      (local $s1 (ref $bytes)) (local $s2 (ref $bytes))
      (local $len i32) (local $i i32)
      (if (ref.eq (local.get $p1) (local.get $p2))
         (then (return (ref.i31 (i32.const 1)))))
      (local.set $s1 (ref.cast (ref $bytes) (local.get $p1)))
      (local.set $s2 (ref.cast (ref $bytes) (local.get $p2)))
      (local.set $len (array.len (local.get $s1)))
      (if (i32.ne (local.get $len) (array.len (local.get $s2)))
         (then (return (ref.i31 (i32.const 0)))))
      (local.set $i (i32.const 0))
      (loop $loop
         (if (i32.lt_s (local.get $i) (local.get $len))
            (then
               (if (i32.ne (array.get_u $bytes (local.get $s1) (local.get $i))
                           (array.get_u $bytes (local.get $s2) (local.get $i)))
                  (then (return (ref.i31 (i32.const 0)))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (ref.i31 (i32.const 1)))

   (export "caml_bytes_notequal" (func $caml_string_notequal))
   (func $caml_string_notequal (export "caml_string_notequal")
      (param $p1 (ref eq)) (param $p2 (ref eq)) (result (ref eq))
      (return
         (ref.i31 (i32.eqz (i31.get_u (ref.cast (ref i31)
            (call $caml_string_equal (local.get $p1) (local.get $p2))))))))

   (func $string_compare
      (param $p1 (ref eq)) (param $p2 (ref eq)) (result i32)
      (local $s1 (ref $bytes)) (local $s2 (ref $bytes))
      (local $l1 i32) (local $l2 i32) (local $len i32) (local $i i32)
      (local $c1 i32) (local $c2 i32)
      (if (ref.eq (local.get $p1) (local.get $p2))
         (then (return (i32.const 0))))
      (local.set $s1 (ref.cast (ref $bytes) (local.get $p1)))
      (local.set $s2 (ref.cast (ref $bytes) (local.get $p2)))
      (local.set $l1 (array.len (local.get $s1)))
      (local.set $l2 (array.len (local.get $s2)))
      (local.set $len (select (local.get $l1) (local.get $l2)
                          (i32.le_u (local.get $l1) (local.get $l2))))
      (local.set $i (i32.const 0))
      (loop $loop
         (if (i32.lt_s (local.get $i) (local.get $len))
            (then
               (local.set $c1
                  (array.get_u $bytes (local.get $s1) (local.get $i)))
               (local.set $c2
                  (array.get_u $bytes (local.get $s2) (local.get $i)))
               (if (i32.lt_u (local.get $c1) (local.get $c2))
                  (then (return (i32.const -1))))
               (if (i32.gt_u (local.get $c1) (local.get $c2))
                  (then (return (i32.const 1))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (if (i32.lt_u (local.get $l1) (local.get $l2))
         (then (return (i32.const -1))))
      (if (i32.gt_u (local.get $l1) (local.get $l2))
         (then (return (i32.const 1))))
      (i32.const 0))

   (export "caml_bytes_compare" (func $caml_string_compare))
   (func $caml_string_compare (export "caml_string_compare")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (ref.i31 (call $string_compare (local.get 0) (local.get 1))))

   (export "caml_bytes_lessequal" (func $caml_string_lessequal))
   (func $caml_string_lessequal (export "caml_string_lessequal")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.le_s (call $string_compare (local.get 0) (local.get 1))
                         (i32.const 0))))

   (export "caml_bytes_lessthan" (func $caml_string_lessthan))
   (func $caml_string_lessthan (export "caml_string_lessthan")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.lt_s (call $string_compare (local.get 0) (local.get 1))
                         (i32.const 0))))

   (export "caml_bytes_greaterequal" (func $caml_string_greaterequal))
   (func $caml_string_greaterequal (export "caml_string_greaterequal")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.ge_s (call $string_compare (local.get 0) (local.get 1))
                         (i32.const 0))))

   (export "caml_bytes_greaterthan" (func $caml_string_greaterthan))
   (func $caml_string_greaterthan (export "caml_string_greaterthan")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.gt_s (call $string_compare (local.get 0) (local.get 1))
                         (i32.const 0))))

   (export "caml_bytes_of_string" (func $caml_string_of_bytes))
   (func $caml_string_of_bytes (export "caml_string_of_bytes")
      (param $v (ref eq)) (result (ref eq))
      (local.get $v))

   (@string $Bytes_create "Bytes.create")

   (func (export "caml_create_bytes")
      (param $len (ref eq)) (result (ref eq))
      (local $l i32)
      (local.set $l (i31.get_s (ref.cast (ref i31) (local.get $len))))
      (if (i32.lt_s (local.get $l) (i32.const 0))
         (then (call $caml_invalid_argument (global.get $Bytes_create))))
      (array.new $bytes (i32.const 0) (local.get $l)))

   (export "caml_blit_bytes" (func $caml_blit_string))
   (func $caml_blit_string (export "caml_blit_string")
      (param $v1 (ref eq)) (param $i1 (ref eq))
      (param $v2 (ref eq)) (param $i2 (ref eq))
      (param $n (ref eq)) (result (ref eq))
      (array.copy $bytes $bytes
         (ref.cast (ref $bytes) (local.get $v2))
         (i31.get_s (ref.cast (ref i31) (local.get $i2)))
         (ref.cast (ref $bytes) (local.get $v1))
         (i31.get_s (ref.cast (ref i31) (local.get $i1)))
         (i31.get_s (ref.cast (ref i31) (local.get $n))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_fill_bytes")
      (param $v (ref eq)) (param $offset (ref eq))
      (param $len (ref eq)) (param $init (ref eq))
      (result (ref eq))
      (array.fill $bytes (ref.cast (ref $bytes) (local.get $v))
         (i31.get_u (ref.cast (ref i31) (local.get $offset)))
         (i31.get_u (ref.cast (ref i31) (local.get $init)))
         (i31.get_u (ref.cast (ref i31) (local.get $len))))
      (ref.i31 (i32.const 0)))

   (export "caml_string_get16" (func $caml_bytes_get16))
   (func $caml_bytes_get16 (export "caml_bytes_get16")
      (param $v (ref eq)) (param $i (ref eq)) (result (ref eq))
      (local $s (ref $bytes)) (local $p i32)
      (local.set $s (ref.cast (ref $bytes) (local.get $v)))
      (local.set $p (i31.get_s (ref.cast (ref i31) (local.get $i))))
      (if (i32.lt_s (local.get $p) (i32.const 0))
         (then (call $caml_bound_error)))
      (if (i32.ge_u (i32.add (local.get $p) (i32.const 1))
                    (array.len (local.get $s)))
         (then (call $caml_bound_error)))
      (ref.i31 (i32.or
                  (array.get_u $bytes (local.get $s) (local.get $p))
                  (i32.shl (array.get_u $bytes (local.get $s)
                              (i32.add (local.get $p) (i32.const 1)))
                           (i32.const 8)))))

   (export "caml_string_get32" (func $caml_bytes_get32))
   (func $caml_bytes_get32 (export "caml_bytes_get32")
      (param $v (ref eq)) (param $i (ref eq)) (result i32)
      (local $s (ref $bytes)) (local $p i32)
      (local.set $s (ref.cast (ref $bytes) (local.get $v)))
      (local.set $p (i31.get_s (ref.cast (ref i31) (local.get $i))))
      (if (i32.lt_s (local.get $p) (i32.const 0))
         (then (call $caml_bound_error)))
      (if (i32.ge_u (i32.add (local.get $p) (i32.const 3))
                    (array.len (local.get $s)))
         (then (call $caml_bound_error)))
      (i32.or
         (i32.or
            (array.get_u $bytes (local.get $s) (local.get $p))
            (i32.shl (array.get_u $bytes (local.get $s)
                        (i32.add (local.get $p) (i32.const 1)))
                     (i32.const 8)))
         (i32.or
            (i32.shl (array.get_u $bytes (local.get $s)
                        (i32.add (local.get $p) (i32.const 2)))
                     (i32.const 16))
            (i32.shl (array.get_u $bytes (local.get $s)
                        (i32.add (local.get $p) (i32.const 3)))
                     (i32.const 24)))))

   (export "caml_string_get64" (func $caml_bytes_get64))
   (func $caml_bytes_get64 (export "caml_bytes_get64")
      (param $v (ref eq)) (param $i (ref eq)) (result i64)
      (local $s (ref $bytes)) (local $p i32)
      (local.set $s (ref.cast (ref $bytes) (local.get $v)))
      (local.set $p (i31.get_s (ref.cast (ref i31) (local.get $i))))
      (if (i32.lt_s (local.get $p) (i32.const 0))
         (then (call $caml_bound_error)))
      (if (i32.ge_u (i32.add (local.get $p) (i32.const 7))
                    (array.len (local.get $s)))
         (then (call $caml_bound_error)))
      (i64.or
         (i64.or
            (i64.or
               (i64.extend_i32_u
                  (array.get_u $bytes (local.get $s) (local.get $p)))
               (i64.shl (i64.extend_i32_u
                           (array.get_u $bytes (local.get $s)
                              (i32.add (local.get $p) (i32.const 1))))
                        (i64.const 8)))
            (i64.or
               (i64.shl (i64.extend_i32_u
                           (array.get_u $bytes (local.get $s)
                              (i32.add (local.get $p) (i32.const 2))))
                        (i64.const 16))
               (i64.shl (i64.extend_i32_u
                           (array.get_u $bytes (local.get $s)
                              (i32.add (local.get $p) (i32.const 3))))
                        (i64.const 24))))
         (i64.or
            (i64.or
               (i64.shl (i64.extend_i32_u
                           (array.get_u $bytes (local.get $s)
                              (i32.add (local.get $p) (i32.const 4))))
                        (i64.const 32))
               (i64.shl (i64.extend_i32_u
                           (array.get_u $bytes (local.get $s)
                              (i32.add (local.get $p) (i32.const 5))))
                        (i64.const 40)))
            (i64.or
               (i64.shl (i64.extend_i32_u
                           (array.get_u $bytes (local.get $s)
                              (i32.add (local.get $p) (i32.const 6))))
                        (i64.const 48))
               (i64.shl (i64.extend_i32_u
                           (array.get_u $bytes (local.get $s)
                              (i32.add (local.get $p) (i32.const 7))))
                        (i64.const 56))))))

   (func (export "caml_bytes_set16")
      (param (ref eq) (ref eq) (ref eq)) (result (ref eq))
      (local $s (ref $bytes)) (local $p i32) (local $v i32)
      (local.set $s (ref.cast (ref $bytes) (local.get 0)))
      (local.set $p (i31.get_s (ref.cast (ref i31) (local.get 1))))
      (local.set $v (i31.get_s (ref.cast (ref i31) (local.get 2))))
      (if (i32.lt_s (local.get $p) (i32.const 0))
         (then (call $caml_bound_error)))
      (if (i32.ge_u (i32.add (local.get $p) (i32.const 1))
                    (array.len (local.get $s)))
         (then (call $caml_bound_error)))
      (array.set $bytes (local.get $s) (local.get $p) (local.get $v))
      (array.set $bytes (local.get $s)
         (i32.add (local.get $p) (i32.const 1))
         (i32.shr_u (local.get $v) (i32.const 8)))
      (ref.i31 (i32.const 0)))

   (func (export "caml_bytes_set32")
      (param (ref eq)) (param (ref eq)) (param $v i32) (result (ref eq))
      (local $s (ref $bytes)) (local $p i32)
      (local.set $s (ref.cast (ref $bytes) (local.get 0)))
      (local.set $p (i31.get_s (ref.cast (ref i31) (local.get 1))))
      (if (i32.lt_s (local.get $p) (i32.const 0))
         (then (call $caml_bound_error)))
      (if (i32.ge_u (i32.add (local.get $p) (i32.const 3))
                    (array.len (local.get $s)))
         (then (call $caml_bound_error)))
      (array.set $bytes (local.get $s) (local.get $p) (local.get $v))
      (array.set $bytes (local.get $s)
         (i32.add (local.get $p) (i32.const 1))
         (i32.shr_u (local.get $v) (i32.const 8)))
      (array.set $bytes (local.get $s)
         (i32.add (local.get $p) (i32.const 2))
         (i32.shr_u (local.get $v) (i32.const 16)))
      (array.set $bytes (local.get $s)
         (i32.add (local.get $p) (i32.const 3))
         (i32.shr_u (local.get $v) (i32.const 24)))
      (ref.i31 (i32.const 0)))

   (func (export "caml_bytes_set64")
      (param (ref eq)) (param (ref eq)) (param $v i64) (result (ref eq))
      (local $s (ref $bytes)) (local $p i32)
      (local.set $s (ref.cast (ref $bytes) (local.get 0)))
      (local.set $p (i31.get_s (ref.cast (ref i31) (local.get 1))))
      (if (i32.lt_s (local.get $p) (i32.const 0))
         (then (call $caml_bound_error)))
      (if (i32.ge_u (i32.add (local.get $p) (i32.const 7))
                    (array.len (local.get $s)))
         (then (call $caml_bound_error)))
      (array.set $bytes (local.get $s) (local.get $p)
         (i32.wrap_i64 (local.get $v)))
      (array.set $bytes (local.get $s)
         (i32.add (local.get $p) (i32.const 1))
         (i32.wrap_i64 (i64.shr_u (local.get $v) (i64.const 8))))
      (array.set $bytes (local.get $s)
         (i32.add (local.get $p) (i32.const 2))
         (i32.wrap_i64 (i64.shr_u (local.get $v) (i64.const 16))))
      (array.set $bytes (local.get $s)
         (i32.add (local.get $p) (i32.const 3))
         (i32.wrap_i64 (i64.shr_u (local.get $v) (i64.const 24))))
      (array.set $bytes (local.get $s)
         (i32.add (local.get $p) (i32.const 4))
         (i32.wrap_i64 (i64.shr_u (local.get $v) (i64.const 32))))
      (array.set $bytes (local.get $s)
         (i32.add (local.get $p) (i32.const 5))
         (i32.wrap_i64 (i64.shr_u (local.get $v) (i64.const 40))))
      (array.set $bytes (local.get $s)
         (i32.add (local.get $p) (i32.const 6))
         (i32.wrap_i64 (i64.shr_u (local.get $v) (i64.const 48))))
      (array.set $bytes (local.get $s)
         (i32.add (local.get $p) (i32.const 7))
         (i32.wrap_i64 (i64.shr_u (local.get $v) (i64.const 56))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_string_concat")
      (param $vs1 (ref eq)) (param $vs2 (ref eq)) (result (ref eq))
      (local $s1 (ref $bytes)) (local $s2 (ref $bytes))
      (local $s (ref $bytes))
      (local $l1 i32) (local $l2 i32)
      (local.set $s1 (ref.cast (ref $bytes) (local.get $vs1)))
      (local.set $s2 (ref.cast (ref $bytes) (local.get $vs2)))
      (local.set $l1 (array.len (local.get $s1)))
      (local.set $l2 (array.len (local.get $s2)))
      (local.set $s
         (array.new $bytes (i32.const 0)
            (i32.add (local.get $l1) (local.get $l2))))
      (array.copy $bytes $bytes
         (local.get $s) (i32.const 0) (local.get $s1) (i32.const 0)
         (local.get $l1))
      (array.copy $bytes $bytes
         (local.get $s) (local.get $l1) (local.get $s2) (i32.const 0)
         (local.get $l2))
      (local.get $s))
)
