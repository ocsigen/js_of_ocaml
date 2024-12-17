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
   (import "jslib" "wrap" (func $wrap (param anyref) (result (ref eq))))
   (import "jslib" "unwrap" (func $unwrap (param (ref eq)) (result anyref)))
   (import "jslib" "caml_js_global"
      (func $caml_js_global (param (ref eq)) (result (ref eq))))
   (import "jslib" "caml_js_get"
      (func $caml_js_get (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "jslib" "caml_js_new"
      (func $caml_js_new (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "jslib" "caml_js_from_array"
      (func $caml_js_from_array (param (ref eq)) (result (ref eq))))
   (import "js" "caml_js_html_escape"
      (func $caml_js_html_escape (param anyref) (result anyref)))
   (import "js" "caml_js_html_entities"
      (func $caml_js_html_entities (param anyref) (result anyref)))

   (type $block (array (mut (ref eq))))
   (type $string (array (mut i8)))

   (func (export "caml_js_html_escape") (param (ref eq)) (result (ref eq))
      (return_call $wrap
         (call $caml_js_html_escape (call $unwrap (local.get 0)))))

   (func (export "caml_js_html_entities") (param (ref eq)) (result (ref eq))
      (return_call $wrap
         (call $caml_js_html_entities (call $unwrap (local.get 0)))))

   (data $console "console")

   (func (export "caml_js_get_console") (param (ref eq)) (result (ref eq))
      (return_call $caml_js_get (call $caml_js_global (ref.i31 (i32.const 0)))
         (array.new_data $string $console (i32.const 0) (i32.const 7))))

   (data $XMLHttpRequest "XMLHttpRequest")

   (func (export "caml_xmlhttprequest_create") (param (ref eq)) (result (ref eq))
      (return_call $caml_js_new
         (call $caml_js_get
            (call $caml_js_global (ref.i31 (i32.const 0)))
            (array.new_data $string $XMLHttpRequest
               (i32.const 0) (i32.const 14)))
         (array.new_fixed $block 1 (ref.i31 (i32.const 0)))))
)
