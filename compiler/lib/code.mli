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

  module Hashtbl : Hashtbl.S with type key = t
end

module Var : sig
  type t [@@ocaml.immediate]

  val print : Format.formatter -> t -> unit

  val equal : t -> t -> bool

  val idx : t -> int

  val of_idx : int -> t

  val fresh : unit -> t

  val fresh_n : string -> t

  val fork : t -> t

  val count : unit -> int

  val compare : t -> t -> int

  val get_name : t -> string option

  val set_name : t -> string -> unit

  val propagate_name : t -> t -> unit

  val reset : unit -> unit

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t

  module Hashtbl : Hashtbl.S with type key = t

  module Tbl : sig
    type key = t

    type 'a t

    type size = unit

    val get : 'a t -> key -> 'a

    val set : 'a t -> key -> 'a -> unit

    val length : 'a t -> int

    val make : size -> 'a -> 'a t

    val iter : (key -> 'a -> unit) -> 'a t -> unit
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

type array_or_not =
  | Array
  | NotArray
  | Unknown

module Native_string : sig
  type t = private
    | Byte of string
    | Utf of Utf8_string.t

  val of_string : string -> t

  val of_bytestring : string -> t

  val equal : t -> t -> bool
end

type constant =
  | String of string
  | NativeString of Native_string.t
  | Float of Int64.t
  | Float_array of Int64.t array
  | Int of Targetint.t
  | Int32 of Int32.t  (** Only produced when compiling to WebAssembly. *)
  | Int64 of Int64.t
  | NativeInt of Int32.t  (** Only produced when compiling to WebAssembly. *)
  | Tuple of int * constant array * array_or_not

module Constant : sig
  type t = constant

  val ocaml_equal : t -> t -> bool option
  (** Guaranteed equality in terms of OCaml [(=)]: if [constant_equal a b =
    Some v], then [Poly.equal a b = v]. This is used for optimization purposes. *)
end

type loc =
  | No
  | Before of Addr.t
  | After of Addr.t

val noloc : loc

val location_of_pc : int -> loc

type prim_arg =
  | Pv of Var.t
  | Pc of constant

type special = Alias_prim of string

type mutability =
  | Immutable
  | Maybe_mutable

type field_type =
  | Non_float
  | Float

type expr =
  | Apply of
      { f : Var.t
      ; args : Var.t list
      ; exact : bool (* if true, then # of arguments = # of parameters *)
      }
  | Block of int * Var.t array * array_or_not * mutability
  | Field of Var.t * int * field_type
  | Closure of Var.t list * cont * Parse_info.t option
  | Constant of constant
  | Prim of prim * prim_arg list
  | Special of special

type instr =
  | Let of Var.t * expr
  | Assign of Var.t * Var.t
  | Set_field of Var.t * int * field_type * Var.t
  | Offset_ref of Var.t * int
  | Array_set of Var.t * Var.t * Var.t
  | Event of Parse_info.t

type last =
  | Return of Var.t
  | Raise of Var.t * [ `Normal | `Notrace | `Reraise ]
  | Stop
  | Branch of cont
  | Cond of Var.t * cont * cont
  | Switch of Var.t * cont array
  | Pushtrap of cont * Var.t * cont
  | Poptrap of cont

type block =
  { params : Var.t list
  ; body : instr list
  ; branch : last
  }

type program =
  { start : Addr.t
  ; blocks : block Addr.Map.t
  ; free_pc : Addr.t
  }

module Print : sig
  type xinstr =
    | Instr of instr
    | Last of last

  val expr : Format.formatter -> expr -> unit

  val constant : Format.formatter -> constant -> unit

  val var_list : Format.formatter -> Var.t list -> unit

  val instr : Format.formatter -> instr -> unit

  val block :
    Format.formatter -> (Addr.Map.key -> xinstr -> string) -> int -> block -> unit

  val program : Format.formatter -> (Addr.Map.key -> xinstr -> string) -> program -> unit

  val last : Format.formatter -> last -> unit

  val cont : Format.formatter -> cont -> unit
end

type 'c fold_blocs = block Addr.Map.t -> Addr.t -> (Addr.t -> 'c -> 'c) -> 'c -> 'c

type fold_blocs_poly = { fold : 'a. 'a fold_blocs } [@@unboxed]

val fold_closures :
     program
  -> (Var.t option -> Var.t list -> cont -> Parse_info.t option -> 'd -> 'd)
  -> 'd
  -> 'd
(** [fold_closures p f init] folds [f] over all closures in the program [p],
    starting from the initial value [init]. For each closure, [f] is called
    with the following arguments: the closure name (enclosed in
    {!Stdlib.Some}), its parameter list, the address and parameter instantiation
    of its first block, the optional closure location and the current accumulator.
    In addition, [f] is called on the initial block [p.start], with
    [None] as the closure name.  All closures in all blocks of [p] are
    included in the fold, not only the ones reachable from
    [p.start]. *)

val fold_closures_innermost_first :
     program
  -> (Var.t option -> Var.t list -> cont -> Parse_info.t option -> 'd -> 'd)
  -> 'd
  -> 'd
(** Similar to {!fold_closures}, but applies the fold function to the
    innermost closures first. Unlike with {!fold_closures}, only the closures
    reachable from [p.start] are considered. *)

val fold_closures_outermost_first :
     program
  -> (Var.t option -> Var.t list -> cont -> Parse_info.t option -> 'd -> 'd)
  -> 'd
  -> 'd
(** Similar to {!fold_closures}, but applies the fold function to the
    outermost closures first. Unlike with {!fold_closures}, only the closures
    reachable from [p.start] are considered. *)

val fold_children : 'c fold_blocs

val fold_children_skip_try_body : 'c fold_blocs

val poptraps : block Addr.Map.t -> Addr.t -> Addr.Set.t

val traverse :
  fold_blocs_poly -> (Addr.t -> 'c -> 'c) -> Addr.t -> block Addr.Map.t -> 'c -> 'c

val preorder_traverse :
  fold_blocs_poly -> (Addr.t -> 'c -> 'c) -> Addr.t -> block Addr.Map.t -> 'c -> 'c

val last_instr : instr list -> instr option
(** Last instruction of a block body, ignoring events *)

val used_blocks : program -> BitSet.t

val prepend : program -> instr list -> program

val empty : program

val compact : program -> program

val is_empty : program -> bool

val equal : program -> program -> bool

val print_diff : program -> program -> unit

val check_updates : name:string -> program -> program -> updates:int -> unit

val invariant : program -> unit

val cont_equal : cont -> cont -> bool

val cont_compare : cont -> cont -> int
