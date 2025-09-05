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
   (import "bindings" "ta_get_i32"
      (func $ta_get_i32 (param (ref extern)) (param i32) (result i32)))
   (import "bindings" "ta_set_i32"
      (func $ta_set_i32 (param (ref extern)) (param i32) (param i32)))
   (import "bigarray" "caml_ba_get_data"
      (func $caml_ba_get_data (param (ref eq)) (result (ref extern))))

   (func (export "caml_lxm_next") (param $v (ref eq)) (result i64)
      (local $data (ref extern))
      (local $a i64) (local $s i64) (local $q0 i64) (local $q1 i64)
      (local $z i64)
      (local.set $data (call $caml_ba_get_data (local.get $v)))
      (local.set $a
         (i64.or
            (i64.extend_i32_u
               (call $ta_get_i32 (local.get $data) (i32.const 0)))
            (i64.shl
               (i64.extend_i32_u
                  (call $ta_get_i32 (local.get $data) (i32.const 1)))
               (i64.const 32))))
      (local.set $s
         (i64.or
            (i64.extend_i32_u
               (call $ta_get_i32 (local.get $data) (i32.const 2)))
            (i64.shl
               (i64.extend_i32_u
                  (call $ta_get_i32 (local.get $data) (i32.const 3)))
               (i64.const 32))))
      (local.set $q0
         (i64.or
            (i64.extend_i32_u
               (call $ta_get_i32 (local.get $data) (i32.const 4)))
            (i64.shl
               (i64.extend_i32_u
                  (call $ta_get_i32 (local.get $data) (i32.const 5)))
               (i64.const 32))))
      (local.set $q1
         (i64.or
            (i64.extend_i32_u
               (call $ta_get_i32 (local.get $data) (i32.const 6)))
            (i64.shl
               (i64.extend_i32_u
                  (call $ta_get_i32 (local.get $data) (i32.const 7)))
               (i64.const 32))))
      (local.set $z (i64.add (local.get $s) (local.get $q0)))
      (local.set $z
         (i64.mul (i64.xor (local.get $z)
                           (i64.shr_u (local.get $z) (i64.const 32)))
                  (i64.const 0xdaba0b6eb09322e3)))
      (local.set $z
         (i64.mul (i64.xor (local.get $z)
                           (i64.shr_u (local.get $z) (i64.const 32)))
                  (i64.const 0xdaba0b6eb09322e3)))
      (local.set $z
         (i64.xor (local.get $z) (i64.shr_u (local.get $z) (i64.const 32))))
      (local.set $s
         (i64.add (i64.mul (local.get $s) (i64.const 0xd1342543de82ef95))
                  (local.get $a)))
      (call $ta_set_i32 (local.get $data) (i32.const 2)
         (i32.wrap_i64 (local.get $s)))
      (call $ta_set_i32 (local.get $data) (i32.const 3)
         (i32.wrap_i64 (i64.shr_u (local.get $s) (i64.const 32))))
      (local.set $q1 (i64.xor (local.get $q1) (local.get $q0)))
      (local.set $q0 (i64.rotl (local.get $q0) (i64.const 24)))
      (local.set $q0 (i64.xor (i64.xor (local.get $q0) (local.get $q1))
                              (i64.shl (local.get $q1) (i64.const 16))))
      (local.set $q1 (i64.rotl (local.get $q1) (i64.const 37)))
      (call $ta_set_i32 (local.get $data) (i32.const 4)
         (i32.wrap_i64 (local.get $q0)))
      (call $ta_set_i32 (local.get $data) (i32.const 5)
         (i32.wrap_i64 (i64.shr_u (local.get $q0) (i64.const 32))))
      (call $ta_set_i32 (local.get $data) (i32.const 6)
         (i32.wrap_i64 (local.get $q1)))
      (call $ta_set_i32 (local.get $data) (i32.const 7)
         (i32.wrap_i64 (i64.shr_u (local.get $q1) (i64.const 32))))
      (return (local.get $z)))
)
