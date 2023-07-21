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

    method stringify_ :
      'a 'b 'c 'd. 'a -> ('b, js_string t -> 'c -> 'd) meth_callback -> js_string t meth
  end

let json : json Js.t = Unsafe.global##._JSON

let input_reviver =
  let reviver _this _key (value : Unsafe.any) : Obj.t =
    if Js.equals (typeof value) (string "string")
    then Obj.repr (to_bytestring (Unsafe.coerce value))
    else if instanceof value Js.array_empty
            && (Unsafe.coerce value)##.length == 4
            && Unsafe.get value 0 == 255
    then
      Obj.repr
        (Jsoo_runtime.Int64.create_int64_lo_mi_hi
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

let mlInt64_constr =
  let dummy_int64 = 1L in
  let dummy_obj : obj t = Obj.magic dummy_int64 in
  dummy_obj##.constructor

let output_reviver _key (value : Unsafe.any) : Obj.t =
  if Obj.tag (Obj.repr value) = Obj.string_tag
  then Obj.repr (bytestring (Obj.magic value : string))
  else if instanceof value mlInt64_constr
  then
    let value = Unsafe.coerce value in
    Obj.repr (array [| 255; value##.lo; value##.mi; value##.hi |])
  else Obj.repr value

let output obj = json##stringify_ obj (Js.wrap_callback output_reviver)
