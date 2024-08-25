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

val iter_block_free_vars : (Code.Var.t -> unit) -> Code.block -> unit

val iter_block_bound_vars : (Code.Var.t -> unit) -> Code.block -> unit

val iter_instr_free_vars : (Code.Var.t -> unit) -> Code.instr -> unit

val iter_last_free_var : (Code.Var.t -> unit) -> Code.last -> unit

val find_loops_in_closure : Code.program -> Code.Addr.t -> Code.Addr.t Code.Addr.Map.t

val f_mutable : Code.program -> Code.Var.Set.t Code.Addr.Map.t

val f : Code.program -> Code.Var.Set.t Code.Addr.Map.t
