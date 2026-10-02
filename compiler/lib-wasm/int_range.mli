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

(** Integer range analysis *)

type t

val f :
     global_flow_state:Global_flow.state
  -> global_flow_info:Global_flow.info
  -> Code.program
  -> t
(** Compute a range for each integer variable of the program *)

val cannot_overflow : t -> Code.Var.t -> bool
(** Whether the value of this variable is computed without overflow *)

val checked_object : t -> Code.Var.t -> Code.Var.t
(** The array, string or bigarray a variable refers to, looking through bound
    checks, which return their argument *)

val valid_index :
     t
  -> at:Code.Var.t
  -> obj:Code.Var.t
  -> is_length:(Code.expr -> bool)
  -> Code.Var.t
  -> bool
(** [valid_index st ~at ~obj ~is_length i] tells whether [i] is a valid index
    of [obj] where [at] is defined. [is_length e] tells whether the
    expression [e] computes the length of [obj]. *)
