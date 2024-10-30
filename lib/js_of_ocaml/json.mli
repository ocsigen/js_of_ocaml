(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
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

(** Unsafe IO. (See {!Deriving_Json} for typesafe IO) *)

val output : 'a -> Js.js_string Js.t
(** Marshal any OCaml value into this JSON representation. *)

val unsafe_input : Js.js_string Js.t -> 'a
(** Unmarshal a string in JSON format as an OCaml value (unsafe but fast !). *)

(**/**)

val set_use_native_stringify : bool -> unit
(** Only affects js_of_ocaml. Whether to use native Javascript [stringify] to turn a value
    into JSON in {!val:output}. Otherwise, fall back to the slower method used by other
    backends, such as wasm_of_ocaml. *)

val use_native_stringify : unit -> bool
(** Whether js_of_ocaml is using [stringify] in {!val:output}. See
    {!val:set_use_native_stringify}. *)
