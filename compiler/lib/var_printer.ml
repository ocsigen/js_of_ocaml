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
  { known : string Int.Hashtbl.t
  ; alphabet : Alphabet.t
  ; mutable last : int
  }

let reserved = String.Hashtbl.create 100

let () = StringSet.iter (fun s -> String.Hashtbl.add reserved s ()) Reserved.keyword

let is_reserved s = String.Hashtbl.mem reserved s

let to_string t i =
  match Int.Hashtbl.find t.known i with
  | name -> name
  | exception Not_found ->
      let rec loop t i j =
        let s = Alphabet.to_string t.alphabet j in
        if is_reserved s
        then loop t i (j + 1)
        else (
          Int.Hashtbl.add t.known i s;
          t.last <- j;
          s)
      in
      loop t i (t.last + 1)

let create alphabet = { known = Int.Hashtbl.create 1001; alphabet; last = -1 }
