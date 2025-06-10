(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2024 Hugo Heuzard
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

type t =
  | Top
  | Block of t list
  | Function of
      { arity : int
      ; pure : bool
      ; res : t
      }

val to_string : t -> string

val of_string : string -> t

val equal : t -> t -> bool

val merge : t -> t -> t

module Store : sig
  val set : name:string -> t -> unit

  val get : name:string -> t option

  val load' : string -> unit

  val load : name:string -> t option
end

module State : sig
  val propagate : Code.Var.t -> int -> Code.Var.t -> unit

  val assign : Code.Var.t -> t -> unit

  val get : Code.Var.t -> t option

  val mem : Code.Var.t -> bool

  val is_pure_fun : Code.Var.t -> bool

  val reset : unit -> unit
end
