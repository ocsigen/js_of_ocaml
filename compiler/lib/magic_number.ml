(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
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

type t = string * int

exception Bad_magic_number of string

exception Bad_magic_version of t

let size = 12

let kind_of_string = function
  | "Caml1999X" -> "exe"
  | "Caml1999I" -> "cmi"
  | "Caml1999O" -> "cmo"
  | "Caml1999A" -> "cma"
  | "Caml1999Y" -> "cmx"
  | "Caml1999Z" -> "cmxa"
  | "Caml2007D" -> "cmxs"
  | "Caml2012T" -> "cmt"
  | "Caml1999M" -> "impl"
  | "Caml1999N" -> "intf"
  | _ -> raise Not_found

let of_string s =
  try
    if String.length s <> size then raise Not_found;
    let kind = String.sub s ~pos:0 ~len:9 in
    let v = String.sub s ~pos:9 ~len:3 in
    let _ = kind_of_string kind in
    kind, int_of_string v
  with _ -> raise (Bad_magic_number s)

let kind (s, _) =
  match kind_of_string s with
  | "exe" -> `Exe
  | "cmo" -> `Cmo
  | "cma" -> `Cma
  | other -> `Other other

let to_string (k, v) = Printf.sprintf "%s%03d" k v

let compare (p1, n1) (p2, n2) =
  if not (String.equal p1 p2) then raise Not_found;
  compare n1 n2

let equal a b = compare a b = 0

let current_exe =
  let v =
    match Ocaml_version.v with
    | `V4_02 | `V4_03 | `V4_04 -> 11
    | `V4_06 -> 11
    | `V4_07 -> 23
    | `V4_08 -> 25
    | `V4_09 -> 26
    | `V4_10 -> 27
    | `V4_11 -> 28
    | `V4_12 -> 29
    | `V4_13 -> 30
  in
  "Caml1999X", v

let current_cmo =
  let v =
    match Ocaml_version.v with
    | `V4_02 -> 10
    | `V4_03 | `V4_04 -> 11
    | `V4_06 -> 22
    | `V4_07 -> 23
    | `V4_08 -> 25
    | `V4_09 -> 26
    | `V4_10 -> 27
    | `V4_11 -> 28
    | `V4_12 -> 29
    | `V4_13 -> 30
  in
  "Caml1999O", v

let current_cma =
  let v =
    match Ocaml_version.v with
    | `V4_02 -> 11
    | `V4_03 | `V4_04 -> 12
    | `V4_06 -> 22
    | `V4_07 -> 23
    | `V4_08 -> 25
    | `V4_09 -> 26
    | `V4_10 -> 27
    | `V4_11 -> 28
    | `V4_12 -> 29
    | `V4_13 -> 30
  in
  "Caml1999A", v

let current = function
  | `Exe -> current_exe
  | `Cmo -> current_cmo
  | `Cma -> current_cma
