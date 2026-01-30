// Js_of_ocaml runtime support
// http://www.ocsigen.org/js_of_ocaml/
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, with linking exception;
// either version 2.1 of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

import { caml_failwith, caml_invalid_argument } from './fail.js';
import { jsoo_sys_getenv } from './sys.js';

//Provides: caml_record_backtrace_env_flag
export var caml_record_backtrace_env_flag = FLAG("with-js-error");

(function () {
  var r = jsoo_sys_getenv("OCAMLRUNPARAM");
  if (r !== undefined) {
    var l = r.split(",");
    for (var i = 0; i < l.length; i++) {
      if (l[i] === "b") {
        caml_record_backtrace_env_flag = 1;
        break;
      } else if (l[i].startsWith("b=")) {
        caml_record_backtrace_env_flag = +l[i].slice(2);
      } else continue;
    }
  }
})();

//Provides: caml_record_backtrace_runtime_flag
export var caml_record_backtrace_runtime_flag = caml_record_backtrace_env_flag;

//Provides: caml_ml_debug_info_status const
export function caml_ml_debug_info_status() {
  return 0;
}
//Provides: caml_backtrace_status
export function caml_backtrace_status(_unit) {
  return caml_record_backtrace_runtime_flag ? 1 : 0;
}
//Provides: caml_get_exception_backtrace const
export function caml_get_exception_backtrace() {
  return 0;
}
//Provides: caml_get_exception_raw_backtrace const
export function caml_get_exception_raw_backtrace(_unit) {
  return [0];
}
//Provides: caml_record_backtrace
export function caml_record_backtrace(b) {
  caml_record_backtrace_runtime_flag = b;
  return 0;
}
//Provides: caml_convert_raw_backtrace const
export function caml_convert_raw_backtrace() {
  return [0];
}
//Provides: caml_raw_backtrace_length
export function caml_raw_backtrace_length() {
  return 0;
}
//Provides: caml_raw_backtrace_next_slot
export function caml_raw_backtrace_next_slot(_slot) {
  return 0;
}
//Provides: caml_raw_backtrace_slot
export function caml_raw_backtrace_slot(_bt, _idx) {
  caml_invalid_argument("Printexc.get_raw_backtrace_slot: index out of bounds");
}
//Provides: caml_restore_raw_backtrace
export function caml_restore_raw_backtrace(_exn, _bt) {
  return 0;
}
//Provides: caml_get_current_callstack const
export function caml_get_current_callstack() {
  return [0];
}

//Provides: caml_convert_raw_backtrace_slot
export function caml_convert_raw_backtrace_slot(_rbt) {
  caml_failwith("caml_convert_raw_backtrace_slot");
}
