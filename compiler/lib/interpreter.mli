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

(** A reference interpreter for the intermediate code, used to check
    that the optimization passes preserve the behavior of whole
    programs. It is enabled with [--debug interp]. *)

val start : Code.program -> unit
(** Run the program and record its behavior (its result and what it
    writes on the standard output and error), if [--debug interp] is
    set. *)

val check : name:string -> Code.program -> unit
(** Run the program, and fail if its behavior differs from the one
    recorded by {!start}. [name] is the name of the pass that has just
    been applied. This does nothing if [--debug interp] is not set, or
    if the program recorded by {!start} could not be run to completion
    (unsupported primitive, too many steps, ...). *)

val set_cps : trampolined_calls:Code.Var.Set.t -> in_cps:Code.Var.Set.t -> unit
(** Called once the [Effects] pass has transformed the program into
    CPS, with the calls which return a trampoline when the stack is too
    deep, and (with double translation) the calls in CPS. *)
