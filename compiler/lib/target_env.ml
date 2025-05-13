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

open Stdlib

type t =
  | Browser
  | Nodejs
  | Isomorphic

let all = [ Isomorphic; Browser; Nodejs ]

let equal (x : t) y = Poly.equal x y

let to_string = function
  | Browser -> "browser"
  | Nodejs -> "nodejs"
  | Isomorphic -> "isomorphic"

let of_string =
  let t = String.Hashtbl.create 17 in
  List.iter all ~f:(fun x -> String.Hashtbl.add t (to_string x) x);
  fun name -> try Some (String.Hashtbl.find t name) with Not_found -> None
