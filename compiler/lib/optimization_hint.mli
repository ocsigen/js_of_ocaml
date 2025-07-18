(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2025 Jérôme Vouillon, Hugo Heuzard
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

type boxed_integer =
  | Int32
  | Int64
  | Nativeint

type array_kind =
  | Generic
  | Value
  | Float

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
    { unsafe : bool
    ; kind : kind
    ; layout : layout
    }
end

type repr =
  | Value
  | Float
  | Int32
  | Nativeint
  | Int64
  | Int

type primitive =
  { name : string
  ; args : repr list
  ; res : repr
  }

type t =
  | Hint_immutable
  | Hint_unsafe
  | Hint_int of boxed_integer
  | Hint_array of array_kind
  | Hint_bigarray of Bigarray.t
  | Hint_primitive of primitive

val print : Format.formatter -> t -> unit
