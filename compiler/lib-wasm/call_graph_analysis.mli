(* Wasm_of_ocaml compiler
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

type t

val direct_calls_only : t -> Code.Var.t -> bool

val wrappable : t -> Code.Var.t -> bool
(** Whether a closure which is not only called directly can be split
    into a function called directly, with its own calling convention,
    and a wrapper called through closures. With CPS, functions are
    called through closures with a different calling convention. With
    double translation, the wrapper is the direct-style code of the
    closure, which can also have a CPS version. *)

val has_unknown_tail_calls : t -> Code.Var.t -> bool
(** Whether the function may perform a tail call which is not a
    direct call, possibly through other functions it tail calls. The
    return value of such a function must not be converted by a
    wrapper: the cycles of tail calls through the wrapper would
    consume stack space. *)

val has_direct_calls : t -> Code.Var.t -> bool
(** Whether the function is called directly at some call site. *)

val known_callee :
     Global_flow.info
  -> t
  -> wrappable:(Code.Var.t -> bool)
  -> bool
  -> Code.Var.t
  -> Code.Var.t option
(** [known_callee info t ~wrappable exact f] is the function called at
    a call site [Apply { f; exact; _ }] when it is the only possible
    callee and the call can be a direct call: the function is only
    called directly, or it is [wrappable]. *)

val raising_functions :
  Code.program -> Global_flow.info -> t -> (Code.Var.t -> bool) -> unit Code.Var.Hashtbl.t
(** Functions which signal exceptions by returning null. The
    functions that are not only called directly ([direct_calls_only]
    is false) must be wrapped: the function called through closures
    converts the null value back into an exception. Only functions
    satisfying [wrappable] are considered for this. *)

val f : Code.program -> Global_flow.info -> t
