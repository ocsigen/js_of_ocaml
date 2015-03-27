(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2015 Hugo Heuzard
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

type t = 
  [ `Null
  | `Bool of bool
  | `Float of float
  | `String of string
  | `A of t list
  | `O of (string * t) list ]

module PP = Pretty_print
let array_str1 =
  Array.init 256 (fun i -> String.make 1 (Char.chr i))
let array_conv =
  Array.init 16 (fun i -> String.make 1 (("0123456789abcdef").[i]))
let pp_string f s =
  PP.string f "\"";
  let l = String.length s in
  for i = 0 to l - 1 do
    let c = s.[i] in
    match c with
    | '"' -> PP.string f "\\\""
    | '\\' -> PP.string f "\\\\"
    | '\n' -> PP.string f "\\n"
    | '\000' .. '\031' ->
       let c = Char.code c in
       PP.string f "\\u00";
       PP.string f (Array.unsafe_get array_conv (c lsr 4));
       PP.string f (Array.unsafe_get array_conv (c land 0xf))
    | _ -> PP.string f (Array.unsafe_get array_str1 (Char.code c))
  done;
  PP.string f "\""

let rec json f : t -> unit = function
  | `Null -> PP.string f "null";
  | `Bool true -> PP.string f "true";
  | `Bool false -> PP.string f "false";
  | `Float v ->
     let s = Javascript.string_of_number v in
     PP.string f s
  | `String s -> pp_string f s
  | `A l ->
     PP.start_group f 1;
     PP.string f "[";
     element_list f l;
     PP.string f "]";
     PP.end_group f
  | `O l ->
     PP.start_group f 1;
     PP.string f "{";
     property_name_and_value_list f l;
     PP.string f "}";
     PP.end_group f

and property_name_and_value_list f l =
  match l with
    [] ->
    ()
  | [(s, e)] ->
     PP.start_group f 0;
     pp_string f s;
     PP.string f ":";
     PP.break f;
     json f e;
     PP.end_group f
  | (s, e) :: r ->
     PP.start_group f 0;
     pp_string f s;
     PP.string f ":";
     PP.break f;
     json f e;
     PP.end_group f;
     PP.string f ",";
     PP.break f;
     property_name_and_value_list f r
and element_list f el =
  match el with
  | []     -> ()
  | [e]    ->
     PP.start_group f 0; json f e; PP.end_group f
  | e :: r ->
     PP.start_group f 0; json f e; PP.end_group f;
     PP.string f ","; PP.break f; element_list f r
					       
let pp = json
