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

module Integer : sig
  type kind =
    | Ref
    | Normalized
    | Unnormalized
end

type boxed_number =
  | Int32
  | Int64
  | Nativeint
  | Float

type boxed_status =
  | Boxed
  | Unboxed

module Bigarray : sig
  type kind =
    | Float16
    | Float32
    | Float64
    | Int8_signed
    | Int8_unsigned
    | Int16_signed
    | Int16_unsigned
    | Int32
    | Int64
    | Int
    | Nativeint
    | Complex32
    | Complex64

  type layout =
    | C
    | Fortran

  type t =
    { kind : kind
    ; layout : layout
    }
end

type typ =
  | Top
  | Int of Integer.kind
  | Number of boxed_number * boxed_status
  | Tuple of typ array
  | Bigarray of Bigarray.t
  | Null
  | Bot

val constant_type : Code.constant -> typ

val can_unbox_parameters : Call_graph_analysis.t -> Code.Var.t -> bool

val bigarray_element_type : Bigarray.kind -> typ

type t

val var_type : t -> Code.Var.t -> typ

val return_type : t -> Code.Var.t -> typ

val reset : unit -> unit

val register_prim : string -> unbox:bool -> typ -> unit

val f :
     global_flow_state:Global_flow.state
  -> global_flow_info:Global_flow.info
  -> fun_info:Call_graph_analysis.t
  -> deadcode_sentinel:Code.Var.t
  -> Code.program
  -> t
