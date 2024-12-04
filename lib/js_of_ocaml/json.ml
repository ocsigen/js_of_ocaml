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

(****)

(* The writing logic for basic types is copied from [lib/deriving_json]. *)

let write_string buffer s =
  Buffer.add_char buffer '"';
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '"' -> Buffer.add_string buffer {|\"|}
    | '\\' -> Buffer.add_string buffer {|\\|}
    | '\b' -> Buffer.add_string buffer {|\b|}
    | '\x0C' -> Buffer.add_string buffer {|\f|}
    | '\n' -> Buffer.add_string buffer {|\n|}
    | '\r' -> Buffer.add_string buffer {|\r|}
    | '\t' -> Buffer.add_string buffer {|\t|}
    | c when Poly.(c <= '\x1F') ->
        (* Other control characters are escaped. *)
        Printf.bprintf buffer {|\u%04X|} (int_of_char c)
    | c when Poly.(c < '\x80') -> Buffer.add_char buffer s.[i]
    | _c (* >= '\x80' *) ->
        (* Bytes greater than 127 are embedded in a UTF-8 sequence. *)
        Buffer.add_char buffer (Char.chr (0xC2 lor (Char.code s.[i] lsr 6)));
        Buffer.add_char buffer (Char.chr (0x80 lor (Char.code s.[i] land 0x3F)))
  done;
  Buffer.add_char buffer '"'

let write_float buffer f =
  (* "%.15g" can be (much) shorter; "%.17g" is round-trippable *)
  let s = Printf.sprintf "%.15g" f in
  if Poly.(float_of_string s = f)
  then Buffer.add_string buffer s
  else Printf.bprintf buffer "%.17g" f

let write_int64 buffer i =
  let mask16 = Int64.of_int 0xffff in
  let mask24 = Int64.of_int 0xffffff in
  Printf.bprintf
    buffer
    "[255,%Ld,%Ld,%Ld]"
    (Int64.logand i mask24)
    (Int64.logand (Int64.shift_right i 24) mask24)
    (Int64.logand (Int64.shift_right i 48) mask16)

external custom_identifier : Obj.t -> string = "caml_custom_identifier"

let rec write b v =
  if Obj.is_int v
  then Printf.bprintf b "%d" (Obj.obj v : int)
  else
    let t = Obj.tag v in
    if t <= Obj.last_non_constant_constructor_tag
    then (
      Printf.bprintf b "[%d" t;
      for i = 0 to Obj.size v - 1 do
        Buffer.add_char b ',';
        write b (Obj.field v i)
      done;
      Buffer.add_char b ']')
    else if t = Obj.string_tag
    then write_string b (Obj.obj v : string)
    else if t = Obj.double_tag
    then write_float b (Obj.obj v : float)
    else if t = Obj.double_array_tag
    then (
      Printf.bprintf b "[%d" t;
      for i = 0 to Obj.size v - 1 do
        Buffer.add_char b ',';
        write_float b (Obj.double_field v i)
      done;
      Buffer.add_char b ']')
    else if t = Obj.custom_tag
    then
      match custom_identifier v with
      | "_i" -> Printf.bprintf b "%ld" (Obj.obj v : int32)
      | "_j" ->
          let i : int64 = Obj.obj v in
          write_int64 b i
      | id -> failwith (Printf.sprintf "Json.output: unsupported custom value %s " id)
    else failwith (Printf.sprintf "Json.output: unsupported tag %d " t)

let to_json v =
  let buf = Buffer.create 50 in
  write buf v;
  Buffer.contents buf

(****)

class type json = object
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
    else if
      instanceof value Js.array_empty
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

let unsafe_input s =
  match Sys.backend_type with
  | Other "wasm_of_ocaml" ->
      (* https://github.com/ocsigen/js_of_ocaml/pull/1660#discussion_r1731099372
         The encoding of OCaml values is ambiguous since both integers and floats
         are mapped to numbers *)
      failwith "Json.unsafe_input: not implemented in the Wasm backend"
  | _ -> json##parse_ s input_reviver

class type obj = object
  method constructor : 'a. 'a constr Js.readonly_prop
end

let mlInt64_constr =
  Js.Unsafe.pure_expr
  @@ fun () ->
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

let use_native_stringify_ =
  ref
    (match Sys.backend_type with
    | Other "js_of_ocaml" -> true
    | Native | Bytecode | Other _ -> false)

let use_native_stringify () = !use_native_stringify_

let set_use_native_stringify b = use_native_stringify_ := b

let output_ x = to_json (Obj.repr x)

let output obj =
  match Sys.backend_type with
  | Other "js_of_ocaml" when use_native_stringify () ->
      json##stringify_ obj (Js.wrap_callback output_reviver)
  | _ -> Js.string (output_ obj)
