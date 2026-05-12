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

// Helpers for the [Js_of_ocaml.Promise] binding.
//
// The JS Promise constructor automatically follows any thenable returned
// from a handler or passed to [Promise.resolve]. To preserve type safety
// for ['a Promise.t Promise.t] we wrap thenable values in a dedicated
// container before resolving and unwrap them on the way back. Non-thenable
// values pass through untouched, which avoids per-call allocation in the
// common case and lets foreign promises (handed in via [Promise.of_any])
// flow through transparently.

//Provides: caml_jsoo_promise_wrapper
function caml_jsoo_promise_wrapper(x) {
  this.wrapped = x;
}

//Provides: caml_jsoo_promise_wrap
//Requires: caml_jsoo_promise_wrapper
function caml_jsoo_promise_wrap(x) {
  return x != null && typeof x.then === "function"
    ? new caml_jsoo_promise_wrapper(x)
    : x;
}

//Provides: caml_jsoo_promise_unwrap
//Requires: caml_jsoo_promise_wrapper
function caml_jsoo_promise_unwrap(x) {
  return x instanceof caml_jsoo_promise_wrapper ? x.wrapped : x;
}
