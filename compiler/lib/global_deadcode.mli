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

(** Global deadcode elimination pass.

    This module provides a global liveness analysis more powerful than that found in
    [deadcode.ml]. In particular, this analysis annotates blocks with the specific fields
    that are live. It also uses [global_flow.ml] to determine the liveness of function
    return values. It first computes an initial liveness of each variable by traversing
    the program IR. Then it propagates this information to the dependencies of each
    variable using a flow analysis solver. Lastly it replaces dead variables with a
    sentinal zero variable (the JS value `undefined`).

    Although this module does not perform any dead-code elimination itself, it is designed
    to be used to identify and substitute dead variables that are then removed by
    [deadcode.ml]. In particular it allows for the elimination of unused functions defined
    in functors, which the original deadcode elimination cannot. *)

val f : Code.program -> deadcode_sentinal:Code.Var.t -> Global_flow.info -> Code.program
