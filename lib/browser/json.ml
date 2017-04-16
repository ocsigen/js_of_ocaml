(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
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
    (* Beware that this is only works when the function argument
       expects exactly two arguments (no curryfication is performed). *)
    method stringify_:
      'a 'b 'c 'd. 'a ->
      (js_string t -> 'c -> 'd) -> js_string t meth
end

external get_json : unit -> json t = "caml_json"

let json = get_json ()

let input_reviver =
  let reviver _this _key value =
    if Unsafe.equals (typeof value) (typeof (string "foo")) then
      (Obj.magic (to_bytestring (Obj.magic value : js_string t)) : 'a t)
    else
      value in
  wrap_meth_callback reviver
let unsafe_input s = json##parse_ s input_reviver

class type obj = object
  method constructor : 'a. 'a constr Js.readonly_prop
end

let mlString_constr =
  let dummy_string = "" in
  let dummy_obj : obj t = Obj.magic dummy_string in
  dummy_obj##.constructor

let output_reviver _key value =
  if instanceof value mlString_constr then
    Js.bytestring (Obj.magic value : string)
  else
    value
let output obj = json##stringify_ obj output_reviver
