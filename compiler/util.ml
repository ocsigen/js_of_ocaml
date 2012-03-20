(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
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

module Int = struct type t = int let compare (x : int) y = compare x y end
module IntSet = Set.Make (Int)
module IntMap = Map.Make (Int)

module StringSet = Set.Make (String)

let opt_map f x = match x with None -> None | Some v -> Some (f v)
let opt_iter f x = match x with None -> () | Some v -> f v
let opt_bind x f = match x with None -> None | Some v -> f v
let opt_filter p x =
  match x with None -> None | Some v -> if p v then Some v else None

(****)

let rec find_in_paths paths name =
  match paths with
    [] ->
      raise Not_found
  | path :: rem ->
      let file = Filename.concat path name in
      if Sys.file_exists file then file else find_in_paths rem name

let read_file f =
  let ch = open_in_bin f in
  let b = Buffer.create 4096 in
  let s = String.create 4096 in
  while
    let n = input ch s 0 4096 in
    Buffer.add_substring b s 0 n;
    n <> 0
  do () done;
  close_in ch;
  Buffer.contents b

(****)

let debugs = ref []

let debug s =
  let state =
    try
      List.assoc s !debugs
    with Not_found ->
      let state = ref false in
      debugs := (s, state) :: !debugs;
      state
  in
  fun () -> !state

let set_debug s =
  try List.assoc s !debugs := true with Not_found -> ()

(****)

let disabled_lst = ref []

let disabled s =
  let state = ref false in
  disabled_lst := (s, state) :: !disabled_lst;
  fun () -> !state

let set_disabled s =
  try List.assoc s !disabled_lst := true with Not_found ->
   Format.eprintf "%s: no disable option named '%s'@." Sys.argv.(0) s; exit 1

(****)

module Timer = struct
  type t = float
  let timer = ref (fun _ -> 0.)
  let init f = timer := f
  let make () = !timer ()
  let get t = !timer () -. t
  let print f t = Format.fprintf f "%.2f" (get t)
end
