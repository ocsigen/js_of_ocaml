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

//Raise exception

//Provides: caml_raise_constant (const)
function caml_raise_constant(tag) {
  throw tag;
}

//Provides: caml_raise_with_arg (const, mutable)
//Requires: caml_maybe_attach_backtrace
function caml_raise_with_arg(tag, arg) {
  throw caml_maybe_attach_backtrace([0, tag, arg]);
}

//Provides: caml_raise_with_args (const, mutable)
//Requires: caml_maybe_attach_backtrace
function caml_raise_with_args(tag, args) {
  throw caml_maybe_attach_backtrace([0, tag].concat(args));
}

//Provides: caml_raise_with_string (const, const)
//Requires: caml_raise_with_arg, caml_string_of_jsbytes
function caml_raise_with_string(tag, msg) {
  caml_raise_with_arg(tag, caml_string_of_jsbytes(msg));
}

//Provides: caml_failwith (const)
//Requires: caml_raise_with_string, caml_global_data, caml_string_of_jsbytes
function caml_failwith(msg) {
  if (!caml_global_data.Failure)
    caml_global_data.Failure = [248, caml_string_of_jsbytes("Failure"), -3];
  caml_raise_with_string(caml_global_data.Failure, msg);
}

//Provides: caml_invalid_argument (const)
//Requires: caml_raise_with_string, caml_global_data
function caml_invalid_argument(msg) {
  caml_raise_with_string(caml_global_data.Invalid_argument, msg);
}

//Provides: caml_raise_end_of_file
//Requires: caml_raise_constant, caml_global_data
function caml_raise_end_of_file() {
  caml_raise_constant(caml_global_data.End_of_file);
}

//Provides: caml_raise_zero_divide
//Requires: caml_raise_constant, caml_global_data
function caml_raise_zero_divide() {
  caml_raise_constant(caml_global_data.Division_by_zero);
}

//Provides: caml_raise_not_found
//Requires: caml_raise_constant, caml_global_data
function caml_raise_not_found() {
  caml_raise_constant(caml_global_data.Not_found);
}

//Provides: caml_array_bound_error
//Requires: caml_invalid_argument
function caml_array_bound_error() {
  caml_invalid_argument("index out of bounds");
}
