(* Wasm_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2026
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

(** Shallow traversals of the Wasm AST.

    These functions apply [expression] to each direct sub-expression of a
    node and [instructions] to each instruction list it contains, in
    evaluation order: operands from left to right, the arguments of
    [call_ref] before the function, the condition of an [if] before its
    branches, the instructions of a [Seq] before its expression. A pass
    handles the constructs it cares about and delegates the others to
    these functions. *)

open Wasm_ast

val iter_expression :
     expression:(expression -> unit)
  -> instructions:(instruction list -> unit)
  -> expression
  -> unit

val iter_instruction :
     expression:(expression -> unit)
  -> instructions:(instruction list -> unit)
  -> instruction
  -> unit

val map_expression :
     expression:(expression -> expression)
  -> instructions:(instruction list -> instruction list)
  -> expression
  -> expression

val map_instruction :
     expression:(expression -> expression)
  -> instructions:(instruction list -> instruction list)
  -> instruction
  -> instruction

val fold_map_expression :
     expression:('a -> expression -> expression * 'a)
  -> instructions:('a -> instruction list -> instruction list * 'a)
  -> 'a
  -> expression
  -> expression * 'a
(** The accumulator is threaded through the children in evaluation
    order, including through both branches of an [if]: a pass for which
    branches must not see each other handles [if] itself. *)

val fold_map_instruction :
     expression:('a -> expression -> expression * 'a)
  -> instructions:('a -> instruction list -> instruction list * 'a)
  -> 'a
  -> instruction
  -> instruction * 'a

val iter_locals_expression :
  read:(var -> unit) -> write:(var -> unit) -> expression -> unit
(** Same as [iter_locals], for an expression. *)

val iter_locals : read:(var -> unit) -> write:(var -> unit) -> instruction list -> unit
(** Deep traversal: call [read] on the variable of each [local.get] and
    [write] on the variable of each [local.set] or [local.tee] in the
    whole instruction list, in evaluation order. *)
