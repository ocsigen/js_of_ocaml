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

module Var : sig
  type t
  val print : Format.formatter -> t -> unit
  val idx : t -> int
  val to_string : t -> string

  type stream
  val make_stream : unit -> stream
  val next : stream -> t * stream

  val fresh : unit -> t

  val count : unit -> int

  val compare : t -> t -> int

  val name : t -> string -> unit
  val propagate_name : t -> t -> unit
  val set_pretty : unit -> unit

  val reset : unit -> unit
end

val string_of_ident : int -> string

module VarSet : Set.S with type elt = Var.t
module VarMap : Map.S with type key = Var.t
module VarTbl : sig
  type 'a t
  type key = Var.t
  type size = unit
  val get : 'a t -> key -> 'a
  val set : 'a t -> key -> 'a -> unit
  val make : size -> 'a -> 'a t
end
module VarISet : sig
  type t
  type elt = Var.t

  val empty : unit -> t
  val iter : (elt -> unit) -> t -> unit
  val mem : t -> elt -> bool
  val add : t -> elt -> unit
  val remove : t -> elt -> unit
  val copy : t -> t
end

type addr = int

module AddrSet : Set.S with type elt = int and type t = Util.IntSet.t
module AddrMap : Map.S with type key = int and type 'a t = 'a Util.IntMap.t

type cont = addr * Var.t list

type prim =
    Vectlength
  | Array_get
  | Extern of string
  | Not | IsInt
  | Eq | Neq | Lt | Le | Ult
  | WrapInt

type constant =
    String of string
  | Float of float
  | Float_array of float array
  | Int32 of int32
  | Nativeint of nativeint
  | Int64 of int64
  | Tuple of int * constant array
  | Int of int

type prim_arg =
    Pv of Var.t
  | Pc of constant

type expr =
    Const of int
  | Apply of Var.t * Var.t list * int option
  | Block of int * Var.t array
  | Field of Var.t * int
  | Closure of Var.t list * cont
  | Constant of constant          (*XXX REMOVE *)
  | Prim of prim * prim_arg list  (*XXX prim * Var.t list * constant list *)

type instr =
    Let of Var.t * expr
  | Set_field of Var.t * int * Var.t
  | Offset_ref of Var.t * int
  | Array_set of Var.t * Var.t * Var.t

(*XXX REMOVE *)
type cond = IsTrue | CEq of int | CLt of int | CLe of int | CUlt of int

type last =
    Return of Var.t
  | Raise of Var.t
  | Stop
  | Branch of cont
  | Cond of cond * Var.t * cont * cont
  | Switch of Var.t * cont array * cont array
  | Pushtrap of cont * Var.t * cont * addr
  | Poptrap of cont

type block =
  { params : Var.t list;
    handler : (Var.t * cont) option;
    body : instr list;
    branch : last }

type program = addr * block AddrMap.t * addr

type xinstr = Instr of instr | Last of last

val print_var_list : Format.formatter -> Var.t list -> unit
val print_instr : Format.formatter -> instr -> unit
val print_block : (AddrMap.key -> xinstr -> string) -> int -> block -> unit
val print_program : (AddrMap.key -> xinstr -> string) -> program -> unit

val fold_closures :
  program -> (Var.t option -> Var.t list -> cont -> 'd -> 'd) -> 'd -> 'd
val fold_children :
  block AddrMap.t -> addr  -> (addr -> 'c -> 'c) -> 'c -> 'c

val add_reserved_name : string -> unit
