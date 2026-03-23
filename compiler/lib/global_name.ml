(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2022 Hugo Heuzard
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

type compunit = Compunit of string [@@unboxed]

type predef = Predef of string [@@unboxed]

type t =
  | Glob_compunit of compunit
  | Glob_predef of predef

let to_string = function
  | Glob_compunit (Compunit name) | Glob_predef (Predef name) -> name

let is_predef = function
  | Glob_predef _ -> true
  | Glob_compunit _ -> false

let compare a b =
  match a, b with
  | Glob_compunit (Compunit a), Glob_compunit (Compunit b)
  | Glob_predef (Predef a), Glob_predef (Predef b) -> String.compare a b
  | Glob_compunit _, Glob_predef _ -> -1
  | Glob_predef _, Glob_compunit _ -> 1

let equal a b =
  match a, b with
  | Glob_compunit (Compunit a), Glob_compunit (Compunit b)
  | Glob_predef (Predef a), Glob_predef (Predef b) -> String.equal a b
  | Glob_compunit _, Glob_predef _ | Glob_predef _, Glob_compunit _ -> false

let hash = Hashtbl.hash

let compare_compunit (Compunit a) (Compunit b) = String.compare a b

module Compunit_set = Set.Make (struct
  type t = compunit

  let compare = compare_compunit
end)

module Compunit_hashtbl = Hashtbl.Make (struct
  type t = compunit

  let equal (Compunit a) (Compunit b) = String.equal a b

  let hash (Compunit s) = Hashtbl.hash s
end)

module Hashtbl = Hashtbl.Make (struct
  type nonrec t = t

  let equal = equal

  let hash = hash
end)

module Set = Set.Make (struct
  type nonrec t = t

  let compare = compare
end)

module Map = Map.Make (struct
  type nonrec t = t

  let compare = compare
end)
