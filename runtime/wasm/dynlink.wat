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
   (import "bigarray" "caml_uint8_array_of_string"
      (func $caml_uint8_array_of_string (param (ref eq)) (result (ref eq))))
   (import "jslib" "unwrap" (func $unwrap (param (ref eq)) (result anyref)))
   (import "bindings" "load_wasmo"
      (func $load_wasmo (param anyref)))

   (type $bytes (array (mut i8)))

   (func (export "caml_wasm_load_wasmo")
      (param $bytes (ref eq)) (result (ref eq))
      (call $load_wasmo
         (call $unwrap
            (call $caml_uint8_array_of_string (local.get $bytes))))
      (ref.i31 (i32.const 0)))
)
