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

let aliases = Hashtbl.create 17

let alias nm nm' = Hashtbl.add aliases nm nm'
let resolve nm = try Hashtbl.find aliases nm with Not_found -> nm

(****)

type kind = [ `Const | `Mutable | `Mutator ]

let kinds = Hashtbl.create 37

let register p k = Hashtbl.add kinds p k

let kind nm = try Hashtbl.find kinds (resolve nm) with Not_found -> `Mutator

let is_pure nm = kind nm <> `Mutator

(****)

let primitives = ref Util.StringSet.empty

let mark_used nm =
  primitives := Util.StringSet.add nm !primitives

let list_used () =
  Format.eprintf "Primitives:@.";
  Util.StringSet.iter (fun nm -> Format.eprintf "  %s@." nm) !primitives

let get_used () = Util.StringSet.elements !primitives
