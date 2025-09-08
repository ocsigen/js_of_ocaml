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
   (import "fail" "caml_failwith" (func $caml_failwith (param (ref eq))))

   (type $bytes (array (mut i8)))

(@if (>= ocaml_version (5 1 0))
(@then
   (func (export "caml_dynlink_open_lib") (param (ref eq)) (result (ref eq))
      (call $caml_failwith (@string "Dll.dll_open is not supported"))
      (ref.i31 (i32.const 0)))
)
(@else
   (func (export "caml_dynlink_open_lib")
      (param (ref eq) (ref eq)) (result (ref eq))
      (call $caml_failwith (@string "Dll.dll_open is not supported"))
      (ref.i31 (i32.const 0)))
))

   (func (export "caml_dynlink_close_lib") (param (ref eq)) (result (ref eq))
      (call $caml_failwith (@string "Dll.dll_close is not supported"))
      (ref.i31 (i32.const 0)))

   (func (export "caml_dynlink_lookup_symbol")
      (param (ref eq) (ref eq)) (result (ref eq))
      (call $caml_failwith (@string "Dll.dll_sym is not supported"))
      (ref.i31 (i32.const 0)))
)
