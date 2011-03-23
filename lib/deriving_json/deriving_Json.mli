(* Js_of_ocaml
 * http://www.ocsigen.org
 * Copyright Grégoire Henry 2010.
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

(** Json **)

type 'a t

val to_string: 'a t -> 'a -> string
val from_string: 'a t -> string -> 'a

(** Deriver *)

module type Json_min = sig
  type a
  val write: Buffer.t -> a -> unit
  val read: Deriving_Json_lexer.lexbuf -> a
end

module type Json_min' = sig
  type a
  val write: Buffer.t -> a -> unit
  val read: Deriving_Json_lexer.lexbuf -> a
  val match_variant: [`Cst of int | `NCst of int] -> bool
  val read_variant: Deriving_Json_lexer.lexbuf -> [`Cst of int | `NCst of int] -> a
end

module type Json_min'' = sig
  type a
  val t: a t
end

module type Json = sig
  type a
  val t: a t
  val write: Buffer.t -> a -> unit
  val read: Deriving_Json_lexer.lexbuf -> a
  val to_string: a -> string
  (* val to_channel: out_channel -> a -> unit *)
  val from_string: string -> a
  (* val from_channel: in_channel -> a *)
  val match_variant: [`Cst of int | `NCst of int] -> bool
  val read_variant: Deriving_Json_lexer.lexbuf -> [`Cst of int | `NCst of int] -> a
end

module Defaults(J:Json_min) : Json with type a = J.a
module Defaults'(J:Json_min') : Json with type a = J.a
module Defaults''(J:Json_min'') : Json with type a = J.a

module Json_char : Json with type a = char
module Json_bool : Json with type a = bool
module Json_unit : Json with type a = unit
module Json_int : Json with type a = int
module Json_int32 : Json with type a = int32
module Json_int64 : Json with type a = int64
module Json_nativeint : Json with type a = nativeint
(* module Json_num       : Json with type a = Num.num *)
module Json_float : Json with type a = float
module Json_string : Json with type a = string
module Json_list(A : Json) : Json with type a = A.a list
module Json_ref(A : Json) : Json with type a = A.a ref
module Json_option(A : Json) : Json with type a = A.a option
module Json_array(A : Json) : Json with type a = A.a array
