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
open! Stdlib

let aliases = Hashtbl.create 17

let rec resolve nm = try resolve (Hashtbl.find aliases nm) with Not_found -> nm

(****)

type kind =
  [ `Pure
  | `Mutable
  | `Mutator
  ]

type kind_arg =
  [ `Shallow_const
  | `Object_literal
  | `Const
  | `Mutable
  ]

type condition =
  [ `If of string
  | `Ifnot of string
  ]

type t =
  [ `Requires of string list
  | `Provides of string * kind * kind_arg list option
  | `Version of ((int -> int -> bool) * string) list
  | `Weakdef
  | `Always
  | `Alias of string
  | condition
  ]

let string_of_kind = function
  | `Pure -> "pure"
  | `Mutable -> "mutable"
  | `Mutator -> "mutator"

let kinds = Hashtbl.create 37

let kind_args_tbl = Hashtbl.create 37

let arities = Hashtbl.create 37

let kind nm = try Hashtbl.find kinds (resolve nm) with Not_found -> `Mutator

let kind_args nm =
  try Some (Hashtbl.find kind_args_tbl (resolve nm)) with Not_found -> None

let arity nm = Hashtbl.find arities (resolve nm)

let has_arity nm a = try Hashtbl.find arities (resolve nm) = a with Not_found -> false

let is_pure nm = Poly.(kind nm <> `Mutator)

let exists p = Hashtbl.mem kinds p

let externals = ref StringSet.empty

let add_external name = externals := StringSet.add name !externals

let get_external () = !externals

let register p k kargs arity =
  (match Hashtbl.find kinds (resolve p) with
  | exception Not_found -> ()
  | k' when Poly.(k = k') -> ()
  | k' ->
      warn
        "Warning: overriding the purity of the primitive %s: %s -> %s@."
        p
        (string_of_kind k')
        (string_of_kind k));
  add_external p;
  (match arity with
  | Some a -> Hashtbl.replace arities p a
  | _ -> ());
  (match kargs with
  | Some k -> Hashtbl.replace kind_args_tbl p k
  | _ -> ());
  Hashtbl.replace kinds p k

let alias nm nm' =
  add_external nm';
  add_external nm;
  Hashtbl.replace aliases nm nm'

let named_values = ref StringSet.empty

let need_named_value s = StringSet.mem s !named_values

let register_named_value s = named_values := StringSet.add s !named_values

let reset () =
  Hashtbl.clear kinds;
  Hashtbl.clear kind_args_tbl;
  Hashtbl.clear arities;
  Hashtbl.clear aliases;
  named_values := StringSet.empty
