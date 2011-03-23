(* Js_of_ocaml
 * http://www.ocsigen.org
 * Copyright Grégoire Henry 2010.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open Js

class type json = object
    method parse: 'a. js_string t -> 'a meth
    method parse_:
	'a 'b 'c 'd. js_string t ->
	  ('b t, js_string t -> 'c -> 'd) meth_callback -> 'a meth
    method stringify: 'a. 'a -> js_string t meth
    method stringify_:
	'a 'b 'c 'd. 'a ->
	  ('b t, js_string t -> 'c -> 'd) meth_callback -> js_string t meth
end

let json_constr = Unsafe.variable "JSON"
let json : json t = json_constr

external unsafe_equals: 'a -> 'b -> bool = "caml_js_equals"

external to_MlString: js_string t -> 'a t = "caml_js_to_string"
external to_jsstring: 'a t -> js_string t = "caml_js_from_string"

external to_byte_MlString: js_string t -> 'a t = "caml_js_to_byte_string"
external to_byte_jsstring: 'a t -> js_string t = "caml_js_from_byte_string"

let input_reviver to_string =
  let reviver this key value =
    if unsafe_equals (typeof value) (typeof (string "foo")) then
      to_string (Unsafe.coerce value)
    else
      value in
  wrap_meth_callback reviver
let unsafe_input ?(encoding = `Unicode) s =
  let reviver =
    match encoding with
    | `Unicode -> input_reviver to_MlString
    | `Byte -> input_reviver to_byte_MlString in
  json##parse_ (s, reviver)

let mlString_constr = Unsafe.variable "MlString"
let output_reviver to_jsstring =
  let reviver this key value =
    if instanceof value mlString_constr then
      to_jsstring (Unsafe.coerce value)
    else
      value in
  wrap_meth_callback reviver
let output ?(encoding = `Unicode) obj =
  let reviver =
    match encoding with
    | `Unicode -> output_reviver to_jsstring
    | `Byte -> output_reviver to_byte_jsstring in
  json##stringify_ (obj, reviver)
