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
open Code

type def =
  | Expr of Code.expr
  | Phi of
      { known : Var.Set.t (* Known arguments *)
      ; others : bool (* Can there be other arguments *)
      ; unit : bool (* Whether we are propagating unit (used for typing) *)
      }

type approx =
  | Top
  | Values of
      { known : Var.Set.t (* List of possible values *)
      ; others : bool (* Whether other values are possible *)
      }

type escape_status =
  | Escape
  | Escape_constant (* Escapes but we know the value is not modified *)
  | No

type info =
  { info_defs : def array
  ; info_approximation : approx Var.Tbl.t
  ; info_may_escape : Var.ISet.t
  ; info_variable_may_escape : escape_status array
  ; info_return_vals : Var.Set.t Var.Map.t
  }

type mutable_fields =
  | No_field
  | Some_fields of Stdlib.FBitSet.t
  | All_fields

module VarPairTbl : Hashtbl.S with type key = Var.t * Var.t

type state =
  { vars : Var.ISet.t (* Set of all veriables considered *)
  ; deps : Var.t list Var.Tbl.t (* Dependency between variables *)
  ; defs : def array (* Definition of each variable *)
  ; variable_may_escape : escape_status array
        (* Any value bound to this variable may escape *)
  ; variable_mutable_fields : mutable_fields array
        (* Any value bound to this variable may be mutable *)
  ; may_escape : escape_status array (* This value may escape *)
  ; mutable_fields : mutable_fields array (* This value may be mutable *)
  ; return_values : Var.Set.t Var.Map.t
        (* Set of variables holding return values of each function *)
  ; functions_from_returned_value : Var.t list Var.Hashtbl.t
        (* Functions associated to each return value *)
  ; known_cases : int list Var.Hashtbl.t
        (* Possible tags for a block after a [switch]. This is used to
           get a more precise approximation of the effect of a field
           access [Field] *)
  ; applied_functions : unit VarPairTbl.t
        (* Functions that have been already considered at a call site.
           This is to avoid repeated computations *)
  ; function_call_sites : Var.t list Var.Hashtbl.t
        (* Known call sites of each functions *)
  ; fast : bool
  }

val f : fast:bool -> Code.program -> state * info

val update_def : info -> Code.Var.t -> Code.expr -> unit

val exact_call : info -> Var.t -> int -> bool

val get_unique_closure : info -> Var.t -> (Var.t * Var.t list) option

val function_arity : info -> Var.t -> int option
