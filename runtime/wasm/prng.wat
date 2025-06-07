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
   (import "bindings" "dv_get_i64"
      (func $dv_get_i64 (param externref i32 i32) (result i64)))
   (import "bindings" "dv_set_i64"
      (func $dv_set_i64 (param externref i32 i64 i32)))
   (import "bigarray" "caml_ba_get_view"
      (func $caml_ba_get_view (param (ref eq)) (result (ref extern))))
   (import "bindings" "littleEndian" (global $littleEndian i32))

   (func (export "caml_lxm_next") (param $v (ref eq)) (result i64)
      (local $view (ref extern))
      (local $a i64) (local $s i64) (local $q0 i64) (local $q1 i64)
      (local $z i64)
      (local.set $view (call $caml_ba_get_view (local.get $v)))
      (local.set $a
         (call $dv_get_i64 (local.get $view) (i32.const 0)
            (global.get $littleEndian)))
      (local.set $s
         (call $dv_get_i64 (local.get $view) (i32.const 8)
            (global.get $littleEndian)))
      (local.set $q0
         (call $dv_get_i64 (local.get $view) (i32.const 16)
            (global.get $littleEndian)))
      (local.set $q1
         (call $dv_get_i64 (local.get $view) (i32.const 24)
            (global.get $littleEndian)))
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
      (call $dv_set_i64 (local.get $view) (i32.const 8) (local.get $s)
         (global.get $littleEndian))
      (local.set $q1 (i64.xor (local.get $q1) (local.get $q0)))
      (local.set $q0 (i64.rotl (local.get $q0) (i64.const 24)))
      (local.set $q0 (i64.xor (i64.xor (local.get $q0) (local.get $q1))
                              (i64.shl (local.get $q1) (i64.const 16))))
      (local.set $q1 (i64.rotl (local.get $q1) (i64.const 37)))
      (call $dv_set_i64 (local.get $view) (i32.const 16) (local.get $q0)
         (global.get $littleEndian))
      (call $dv_set_i64 (local.get $view) (i32.const 24) (local.get $q1)
         (global.get $littleEndian))
      (return (local.get $z)))
)
