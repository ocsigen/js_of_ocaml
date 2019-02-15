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

module Addr : sig
  type t = int

  val to_string : t -> string

  val zero : t

  val succ : t -> t

  val pred : t -> t

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t
end

module DebugAddr : sig
  type t = private int

  val of_addr : Addr.t -> t

  val to_addr : t -> Addr.t

  val no : t
end

module Var : sig
  type t

  val print : Format.formatter -> t -> unit

  val idx : t -> int

  val of_idx : int -> t

  val to_string : ?origin:t -> t -> string

  val fresh : unit -> t

  val fresh_n : string -> t

  val fork : t -> t

  val count : unit -> int

  val compare : t -> t -> int

  val loc : t -> Parse_info.t -> unit

  val get_loc : t -> Parse_info.t option

  val get_name : t -> string option

  val name : t -> string -> unit

  val propagate_name : t -> t -> unit

  val reset : unit -> unit

  val set_pretty : bool -> unit

  val set_stable : bool -> unit

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t

  module Tbl : sig
    type key = t

    type 'a t

    type size = unit

    val get : 'a t -> key -> 'a

    val set : 'a t -> key -> 'a -> unit

    val make : size -> 'a -> 'a t
  end

  module ISet : sig
    type elt = t

    type t

    val empty : unit -> t

    val iter : (elt -> unit) -> t -> unit

    val mem : t -> elt -> bool

    val add : t -> elt -> unit

    val remove : t -> elt -> unit

    val copy : t -> t
  end
end

type cont = Addr.t * Var.t list

type prim =
  | Vectlength
  | Array_get
  | Extern of string
  | Not
  | IsInt
  | Eq
  | Neq
  | Lt
  | Le
  | Ult

type constant =
  | String of string
  | IString of string
  | Float of float
  | Float_array of float array
  | Int64 of int64
  | Tuple of int * constant array
  | Int of int32

type prim_arg =
  | Pv of Var.t
  | Pc of constant

type expr =
  | Const of int32
  | Apply of Var.t * Var.t list * bool
  (* if true, then # of arguments = # of parameters *)
  | Block of int * Var.t array
  | Field of Var.t * int
  | Closure of Var.t list * cont
  | Constant of constant
  (*XXX REMOVE *)
  | Prim of prim * prim_arg list

(*XXX prim * Var.t list * constant list *)

type instr =
  | Let of Var.t * expr
  | Set_field of Var.t * int * Var.t
  | Offset_ref of Var.t * int
  | Array_set of Var.t * Var.t * Var.t

(*XXX REMOVE *)
type cond =
  | IsTrue
  | CEq of int32
  | CLt of int32
  | CLe of int32
  | CUlt of int32

type last =
  | Return of Var.t
  | Raise of Var.t * [`Normal | `Notrace | `Reraise]
  | Stop
  | Branch of cont
  | Cond of cond * Var.t * cont * cont
  | Switch of Var.t * cont array * cont array
  | Pushtrap of cont * Var.t * cont * Addr.Set.t
  | Poptrap of cont * Addr.t

type block =
  { params : Var.t list
  ; handler : (Var.t * cont) option
  ; body : instr list
  ; branch : last }

type program = Addr.t * block Addr.Map.t * Addr.t

type xinstr =
  | Instr of instr
  | Last of last

val print_var_list : Format.formatter -> Var.t list -> unit

val print_instr : Format.formatter -> instr -> unit

val print_block : (Addr.Map.key -> xinstr -> string) -> int -> block -> unit

val print_program : (Addr.Map.key -> xinstr -> string) -> program -> unit

val print_last : Format.formatter -> last -> unit

val print_cont : Format.formatter -> cont -> unit

val fold_closures :
  program -> (Var.t option -> Var.t list -> cont -> 'd -> 'd) -> 'd -> 'd

val fold_children : block Addr.Map.t -> Addr.t -> (Addr.t -> 'c -> 'c) -> 'c -> 'c

val traverse :
     (   block Addr.Map.t
      -> Addr.t
      -> (Addr.t -> Addr.Set.t * 'c -> Addr.Set.t * 'c)
      -> Addr.Set.t * 'c
      -> Addr.Set.t * 'c)
  -> (Addr.t -> 'c -> 'c)
  -> Addr.t
  -> block Addr.Map.t
  -> 'c
  -> 'c

val prepend : program -> instr list -> program

val empty : program

val eq : program -> program -> bool

val invariant : program -> unit
