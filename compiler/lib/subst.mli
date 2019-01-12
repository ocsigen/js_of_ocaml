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

val program : (Var.t -> Var.t) -> program -> program
val expr : (Var.t -> Var.t) -> expr -> expr
val instr : (Var.t -> Var.t) -> instr -> instr
val instrs : (Var.t -> Var.t) -> instr list -> instr list
val last : (Var.t -> Var.t) -> last -> last

val cont : (Var.t -> Var.t) -> int -> program -> program
val from_array : Var.t option array -> Var.t -> Var.t

val build_mapping : Var.t list -> Var.t list -> Var.t VarMap.t

val from_map : Var.t VarMap.t -> Var.t -> Var.t
