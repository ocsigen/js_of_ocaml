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

;; Helpers for the [Js_of_ocaml.Promise] binding. Implementations live in
;; runtime/js/promise.js — the Wasm side just bridges between OCaml's
;; (ref eq) representation and the JS [anyref] world.

(module

(@if (not $wasi)
(@then
   (import "jslib" "wrap" (func $wrap (param anyref) (result (ref eq))))
   (import "jslib" "unwrap" (func $unwrap (param (ref eq)) (result anyref)))
   (import "js" "caml_jsoo_promise_wrap"
      (func $caml_jsoo_promise_wrap_js (param anyref) (result anyref)))
   (import "js" "caml_jsoo_promise_unwrap"
      (func $caml_jsoo_promise_unwrap_js (param anyref) (result anyref)))

   (func (export "caml_jsoo_promise_wrap") (param $v (ref eq)) (result (ref eq))
      (return_call $wrap
         (call $caml_jsoo_promise_wrap_js (call $unwrap (local.get $v)))))

   (func (export "caml_jsoo_promise_unwrap") (param $v (ref eq)) (result (ref eq))
      (return_call $wrap
         (call $caml_jsoo_promise_unwrap_js (call $unwrap (local.get $v)))))
))

)
