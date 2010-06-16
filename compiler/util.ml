(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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

let debugs = ref []

let debug s =
  let state = ref false in
  debugs := (s, state) :: !debugs;
  fun () -> !state

let set_debug s =
  try List.assoc s !debugs := true with Not_found -> ()
