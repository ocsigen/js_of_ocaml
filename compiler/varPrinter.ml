(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2013 Hugo Heuzard
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

open Util

type t = {
  names : (int,string) Hashtbl.t;
  known : (int,string) Hashtbl.t;
  cache : (int*int,string) Hashtbl.t;
  mutable last : int;
  mutable pretty : bool;
}

let c1 = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$"
let c2 = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_$"

let name_raw t v nm = Hashtbl.add t.names v nm
let propagate_name t v v' =
  try name_raw t v' (Hashtbl.find t.names v) with Not_found -> ()

let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_num c = (c >= '0' && c <= '9')

let name t v nm =
  if String.length nm > 0 then begin
    let nm = String.copy nm in
    if not (is_alpha nm.[0]) then nm.[0] <- '_';
    for i = 1 to String.length nm - 1 do
      if not (is_alpha nm.[i] || is_num nm.[i]) then nm.[i] <- '_';
    done;
    let c = ref 0 in
    for i = 0 to String.length nm - 1 do
      if nm.[i] = '_' then incr c
    done;
    if !c < String.length nm then name_raw t v nm
  end

let rec format_ident x =
  assert (x >= 0);
  let char c x = String.make 1 (c.[x]) in
  if x < 54 then
    char c1 x
  else
    format_ident ((x - 54) / 64) ^ char c2 ((x - 54) mod 64)

let format_var t i x =
  let s = format_ident x in
  if t.pretty
  then Format.sprintf "_%s_" s
  else s

let reserved = ref StringSet.empty

let add_reserved s = reserved := List.fold_left (fun acc x ->
    StringSet.add x acc) !reserved s


let _ = reserved := StringSet.union !reserved Reserved.keyword(* ; *)
(* add_reserved Reserved.provided *)

let get_reserved () = !reserved

let is_reserved s = StringSet.mem s !reserved

let rec to_string t ?origin i =
  let origin = match origin with
    | Some i when t.pretty -> i
    | _ -> i in
  try
    Hashtbl.find t.cache (i,origin)
  with Not_found ->
    let name =
      try
        Hashtbl.find t.known i
      with Not_found ->
        t.last <- t.last + 1;
        let j = t.last in
        let s = format_var t i j in
        if is_reserved s then
          to_string t i
        else begin
          Hashtbl.add t.known i s;
          s
        end in
    let name =
      if t.pretty
      then
        try
          let nm = Hashtbl.find t.names origin in
          nm ^ name
        with Not_found -> name
      else name
    in
    Hashtbl.add t.cache (i,origin) name;
    name


let set_pretty t b = t.pretty <- b


let reset t =
  Hashtbl.clear t.names; Hashtbl.clear t.known; Hashtbl.clear t.cache;
  t.last <- -1

let create ?(pretty=false) () =
  let t = {
    names = Hashtbl.create 107;
    known = Hashtbl.create 1001;
    cache = Hashtbl.create 1001;
    last = -1;
    pretty;
  } in
  t
