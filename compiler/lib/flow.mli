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

(*
type 'a flat = Void | Known of 'a | Unknown

type v =
  | Blk of t list
  | Cst of int

and t = Code.Var.t flat * v flat

val approx_to_string : t -> string

val get_field : t -> int -> t

val get_const : t -> int option

val get_label : t -> Code.Var.t option

*)

module Info : sig
  type t

  val def : t -> Code.Var.t -> Code.expr option

  val update_def : t -> Code.Var.t -> Code.expr -> unit

  val possibly_mutable : t -> Code.Var.t -> bool
end

val get_approx :
  Info.t -> (Code.Var.Set.elt -> 'b) -> 'b -> ('b -> 'b -> 'b) -> Code.Var.Tbl.key -> 'b

val the_def_of : Info.t -> Code.prim_arg -> Code.expr option

val the_const_of :
     eq:(Code.constant -> Code.constant -> bool)
  -> Info.t
  -> Code.prim_arg
  -> Code.constant option

val the_string_of : Info.t -> Code.prim_arg -> string option

val the_native_string_of : Info.t -> Code.prim_arg -> Code.Native_string.t option

val the_block_contents_of : Info.t -> Code.prim_arg -> Code.Var.t array option

val the_int : Info.t -> Code.prim_arg -> Targetint.t option

val f : Code.program -> Code.program * Info.t
