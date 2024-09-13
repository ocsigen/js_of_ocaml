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
   (import "obj" "caml_callback_1"
      (func $caml_callback_1
         (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "sync" "caml_ml_mutex_unlock"
      (func $caml_ml_mutex_unlock (param (ref eq)) (result (ref eq))))
   (import "domain" "caml_domain_latest_id"
     (global $caml_domain_latest_id (mut i32)))
   (import "domain" "caml_domain_id"
     (global $caml_domain_id (mut i32)))

   (func (export "caml_runtime_events_user_write")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_domain_spawn")
      (param $f (ref eq)) (param $mutex (ref eq)) (result (ref eq))
      (local $id i32) (local $old i32)
      (local.set $id (global.get $caml_domain_latest_id))
      (global.set $caml_domain_latest_id
         (i32.add (local.get $id) (i32.const 1)))
      (local.set $old (global.get $caml_domain_id))
      (drop (call $caml_callback_1 (local.get $f) (ref.i31 (i32.const 0))))
      (global.set $caml_domain_id (local.get $old))
      (drop (call $caml_ml_mutex_unlock (local.get $mutex)))
      (ref.i31 (local.get $id)))

   (global (export "caml_marshal_header_size") i32 (i32.const 20))
)
