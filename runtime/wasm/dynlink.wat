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
   (import "jslib" "log_str" (func $log_str (param (ref $string))))

   (type $string (array (mut i8)))

   (data $caml_dynlink_close_lib "caml_dynlink_close_lib")

   (func (export "caml_dynlink_close_lib")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_str
         (array.new_data $string $caml_dynlink_close_lib
            (i32.const 0) (i32.const 22)))
      (ref.i31 (i32.const 0)))

   (data $caml_dynlink_lookup_symbol "caml_dynlink_lookup_symbol")

   (func (export "caml_dynlink_lookup_symbol")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_str
         (array.new_data $string $caml_dynlink_lookup_symbol
            (i32.const 0) (i32.const 26)))
      (ref.i31 (i32.const 0)))
)
