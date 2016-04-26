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
  mutable stable : bool;
}

let c1 = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$"
let c2 = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_$"

let name_raw t v nm = Hashtbl.add t.names v nm
let propagate_name t v v' =
  try
    let name = Hashtbl.find t.names v in
    name_raw t v' name(* ;
     * (try
     *    let n = Hashtbl.find t.names v' in
     *    if n <> name
     *    then Printf.eprintf "erasing name %s by %s\n%!" n name
     *  with _ -> ()) *)
  with Not_found -> ()

let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_num c = (c >= '0' && c <= '9')

let name t v nm_orig =
  let len = String.length nm_orig in
  if len > 0 then begin
    let buf = Buffer.create (String.length nm_orig) in
    let idx = ref 0 in
    while (!idx < len && not (is_alpha nm_orig.[!idx])) do incr idx done;
    let pending = ref false in
    if(!idx >= len) then begin
      pending := true;
      idx := 0
    end;
    for i = !idx to len - 1 do
      if is_alpha nm_orig.[i] || is_num nm_orig.[i]
      then begin
        if !pending
        then Buffer.add_char buf '_';
        Buffer.add_char buf nm_orig.[i];
        pending:=false
      end
      else pending := true
    done;
    let str = Buffer.contents buf in
    if String.length str > 0
    then name_raw t v str
  end

let get_name t v = try Some (Hashtbl.find t.names v) with Not_found -> None

let rec format_ident x =
  assert (x >= 0);
  let char c x = String.make 1 (c.[x]) in
  if x < 54 then
    char c1 x
  else
    format_ident ((x - 54) / 64) ^ char c2 ((x - 54) mod 64)

let format_var t i x =
  let s = format_ident x in
  if t.stable
  then Format.sprintf "v%d" i
  else if t.pretty
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
let set_stable t b = t.stable <- b

let reset t =
  Hashtbl.clear t.names; Hashtbl.clear t.known; Hashtbl.clear t.cache;
  t.last <- -1

let create ?(pretty=false) ?(stable=false) () =
  let t = {
    names = Hashtbl.create 107;
    known = Hashtbl.create 1001;
    cache = Hashtbl.create 1001;
    last = -1;
    pretty;
    stable;
  } in
  t
