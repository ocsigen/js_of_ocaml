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

open! Stdlib

module Alphabet = struct
  type t =
    { c1 : string
    ; c1_len : int
    ; cn : string
    ; cn_len : int
    }

  let create ~c1 ~cn = { c1; c1_len = String.length c1; cn; cn_len = String.length cn }

  let javascript =
    create
      ~c1:"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$"
      ~cn:"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_$"

  let rec size t x acc =
    if x < t.c1_len then 1 + acc else size t ((x - t.c1_len) / t.cn_len) (acc + 1)

  let to_string (t : t) (x : int) =
    let size = size t x 0 in
    let buf = Bytes.create size in
    let rec loop i x =
      match i with
      | 0 ->
          assert (x < t.c1_len);
          Bytes.set buf i t.c1.[x];
          Bytes.unsafe_to_string buf
      | i ->
          let x = x - t.c1_len in
          Bytes.set buf i t.cn.[x mod t.cn_len];
          loop (pred i) (x / t.cn_len)
    in
    loop (size - 1) x
end

type t =
  { names : (int, string) Hashtbl.t
  ; known : (int, string) Hashtbl.t
  ; cache : (int * int, string) Hashtbl.t
  ; alphabet : Alphabet.t
  ; mutable last : int
  ; mutable pretty : bool
  ; mutable stable : bool
  }

let name_raw t v nm = Hashtbl.replace t.names v nm

let merge_name n1 n2 =
  match n1, n2 with
  | "", n2 -> n2
  | n1, "" -> n1
  | n1, n2 ->
      if generated_name n1
      then n2
      else if generated_name n2
      then n1
      else if String.length n1 > String.length n2
      then n1
      else n2

let propagate_name t v v' =
  try
    let name = Hashtbl.find t.names v in
    match Hashtbl.find t.names v' with
    | exception Not_found -> name_raw t v' name
    | name' -> name_raw t v' (merge_name name name')
  with Not_found -> ()

let name t v nm_orig =
  let len = String.length nm_orig in
  if len > 0
  then (
    let buf = Buffer.create (String.length nm_orig) in
    let idx = ref 0 in
    while !idx < len && not (Char.is_alpha nm_orig.[!idx]) do
      incr idx
    done;
    let pending = ref false in
    if !idx >= len
    then (
      pending := true;
      idx := 0);
    for i = !idx to len - 1 do
      if Char.is_alpha nm_orig.[i] || Char.is_num nm_orig.[i]
      then (
        if !pending then Buffer.add_char buf '_';
        Buffer.add_char buf nm_orig.[i];
        pending := false)
      else pending := true
    done;
    let str = Buffer.contents buf in
    let str =
      match str, nm_orig with
      | "", ">>=" -> "symbol_bind"
      | "", ">>|" -> "symbol_map"
      | "", _ -> "symbol"
      | str, _ -> str
    in
    (* protect against large names *)
    let max_len = 30 in
    let str =
      if String.length str > max_len then String.sub str ~pos:0 ~len:max_len else str
    in
    name_raw t v str)

let get_name t v = try Some (Hashtbl.find t.names v) with Not_found -> None

let format_var t i x =
  let s = Alphabet.to_string t.alphabet x in
  if t.stable
  then Format.sprintf "v%d" i
  else if t.pretty
  then Format.sprintf "_%s_" s
  else s

let reserved = ref StringSet.empty

let add_reserved s = reserved := StringSet.add s !reserved

let _ = reserved := StringSet.union !reserved Reserved.keyword

let get_reserved () = !reserved

let is_reserved s = StringSet.mem s !reserved

let rec to_string t ?origin i =
  let origin =
    match origin with
    | Some i when t.pretty -> i
    | _ -> i
  in
  try Hashtbl.find t.cache (i, origin)
  with Not_found ->
    let name =
      try Hashtbl.find t.known i
      with Not_found ->
        t.last <- t.last + 1;
        let j = t.last in
        let s = format_var t i j in
        if is_reserved s
        then to_string t i
        else (
          Hashtbl.add t.known i s;
          s)
    in
    let name =
      if t.pretty
      then
        try
          let nm = Hashtbl.find t.names origin in
          nm ^ name
        with Not_found -> name
      else name
    in
    Hashtbl.add t.cache (i, origin) name;
    name

let set_pretty t b = t.pretty <- b

let set_stable t b = t.stable <- b

let reset t =
  Hashtbl.clear t.names;
  Hashtbl.clear t.known;
  Hashtbl.clear t.cache;
  t.last <- -1

let create ?(pretty = false) ?(stable = false) alphabet =
  let t =
    { names = Hashtbl.create 107
    ; known = Hashtbl.create 1001
    ; cache = Hashtbl.create 1001
    ; alphabet
    ; last = -1
    ; pretty
    ; stable
    }
  in
  t
