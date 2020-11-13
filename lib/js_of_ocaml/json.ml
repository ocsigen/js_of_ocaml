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
open! Import

class type json =
  object
    method parse : 'a. js_string t -> 'a meth

    method parse_ :
      'a 'b 'c 'd. js_string t -> ('b t, js_string t -> 'c -> 'd) meth_callback -> 'a meth

    method stringify : 'a. 'a -> js_string t meth

    (* Beware that this is only works when the function argument
       expects exactly two arguments (no curryfication is performed). *)
    method stringify_ : 'a 'b 'c 'd. 'a -> (js_string t -> 'c -> 'd) -> js_string t meth
  end

let json : json Js.t = Unsafe.global##._JSON

external unsafe_equals : 'a -> 'b -> bool = "caml_js_equals"

external of_jsbytes : js_string t -> string = "caml_string_of_jsbytes"

external to_jsbytes : 'a t -> js_string t = "caml_jsbytes_of_string"

external create_int64_lo_mi_hi : int -> int -> int -> Int64.t
  = "caml_int64_create_lo_mi_hi"

let input_reviver =
  let reviver _this _key (value : Unsafe.any) : Obj.t =
    if unsafe_equals (typeof value) (typeof (string "foo"))
    then Obj.repr (of_jsbytes (Unsafe.coerce value))
    else if instanceof value Js.array_empty
            && (Unsafe.coerce value)##.length == 4
            && Unsafe.get value 0 == 255
    then
      Obj.repr
        (create_int64_lo_mi_hi
           (Unsafe.get value 1)
           (Unsafe.get value 2)
           (Unsafe.get value 3))
    else Obj.repr value
  in
  wrap_meth_callback reviver

let unsafe_input s = json##parse_ s input_reviver

class type obj =
  object
    method constructor : 'a. 'a constr Js.readonly_prop
  end

let mlString_constr =
  let dummy_string = "" in
  let dummy_obj : obj t = Obj.magic dummy_string in
  dummy_obj##.constructor

let mlInt64_constr =
  let dummy_int64 = 1L in
  let dummy_obj : obj t = Obj.magic dummy_int64 in
  dummy_obj##.constructor

let output_reviver _key (value : Unsafe.any) : Obj.t =
  if instanceof value mlString_constr
  then Obj.repr (to_jsbytes (Unsafe.coerce value))
  else if instanceof value mlInt64_constr
  then
    let value = Unsafe.coerce value in
    Obj.repr (array [| 255; value##.lo; value##.mi; value##.hi |])
  else Obj.repr value

let output obj = json##stringify_ obj output_reviver
