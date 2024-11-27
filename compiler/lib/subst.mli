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

open Code

(** The operations of this module substitute variable names that appear in
    expressions, except for binders, i.e., names on the right-hand side of a
    {!constructor:Code.Let}. *)
module Excluding_Binders : sig
  val program : (Var.t -> Var.t) -> program -> program

  val expr : (Var.t -> Var.t) -> expr -> expr

  val instr : (Var.t -> Var.t) -> instr -> instr

  val instrs : (Var.t -> Var.t) -> instr list -> instr list

  val block : (Var.t -> Var.t) -> block -> block

  val last : (Var.t -> Var.t) -> last -> last

  val cont : (Var.t -> Var.t) -> int -> program -> program

  val cont' :
       (Var.t -> Var.t)
    -> int
    -> block Addr.Map.t
    -> Addr.Set.t
    -> block Addr.Map.t * Addr.Set.t
end

val from_array : Var.t array -> Var.t -> Var.t

val build_mapping : Var.t list -> Var.t list -> Var.t Var.Map.t

val from_map : Var.t Var.Map.t -> Var.t -> Var.t

(** The operations of this module also substitute the variables names that
    appear on the left-hand-side of a {!constructor:Code.Let}, or as block
    parameters, or as closure parameters, or are bound by an exception handler.
    *)
module Including_Binders : sig
  val instr : (Var.t -> Var.t) -> instr -> instr

  val instrs : (Var.t -> Var.t) -> instr list -> instr list

  val block : (Var.t -> Var.t) -> block -> block

  module And_Continuations : sig
    val block : Addr.t Addr.Map.t -> (Var.t -> Var.t) -> block -> block
    (** Same as [Including_Binders.block], but also substitutes continuation
        addresses. *)
  end
end
