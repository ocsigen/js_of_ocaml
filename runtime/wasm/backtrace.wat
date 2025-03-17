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
   (import "bindings" "backtrace_status"
      (func $backtrace_status (result (ref eq))))
   (import "bindings" "record_backtrace"
      (func $record_backtrace (param (ref eq))))
   (import "fail" "caml_invalid_argument"
      (func $caml_invalid_argument (param (ref eq))))

   (type $block (array (mut (ref eq))))
   (type $bytes (array (mut i8)))

   (func (export "caml_get_exception_raw_backtrace")
      (param (ref eq)) (result (ref eq))
      (array.new_fixed $block 1 (ref.i31 (i32.const 0))))

   (func (export "caml_backtrace_status")
      (param (ref eq)) (result (ref eq))
      (call $backtrace_status))

   (func (export "caml_convert_raw_backtrace")
      (param (ref eq)) (result (ref eq))
      (array.new_fixed $block 1 (ref.i31 (i32.const 0))))

   (func (export "caml_raw_backtrace_next_slot")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (@string $raw_backtrace_slot_err
      "Printexc.get_raw_backtrace_slot: index out of bounds")

   (func (export "caml_raw_backtrace_slot")
      (param (ref eq) (ref eq)) (result (ref eq))
      (call $caml_invalid_argument (global.get $raw_backtrace_slot_err))
      (ref.i31 (i32.const 0)))

   (func (export "caml_convert_raw_backtrace_slot")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_restore_raw_backtrace")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_get_current_callstack")
      (param (ref eq)) (result (ref eq))
      (array.new_fixed $block 1 (ref.i31 (i32.const 0))))

   (func (export "caml_ml_debug_info_status")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_record_backtrace") (param $b (ref eq)) (result (ref eq))
      (call $record_backtrace (local.get $b))
      (ref.i31 (i32.const 0)))
)
