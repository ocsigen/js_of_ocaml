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

type def = Phi of Code.VarSet.t | Expr of Code.expr | Param

type info = {
  info_defs:def array;
  info_known_origins : Code.VarSet.t Code.VarTbl.t;
  info_maybe_unknown : bool Code.VarTbl.t;
  info_possibly_mutable : bool array;
}

val get_approx : info -> (Code.VarSet.elt -> 'b) ->
           'b -> ('b -> 'b -> 'b) -> Code.VarTbl.key -> 'b

val the_def_of : info -> Code.prim_arg -> Code.expr option

val the_const_of : info -> Code.prim_arg -> Code.constant option

val the_string_of : info -> Code.prim_arg -> string option

val the_int : info -> Code.prim_arg -> int32 option

val update_def : info -> Code.Var.t -> Code.expr -> unit

val f : ?skip_param:bool -> Code.program -> Code.program * info
