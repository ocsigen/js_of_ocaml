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

val f : fast:bool -> Code.program -> info

val exact_call : info -> Var.t -> int -> bool

val function_arity : info -> Var.t -> int option
