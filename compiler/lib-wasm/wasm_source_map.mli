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

type t

val is_empty : Source_map.Standard.t -> bool

type resize_data =
  { mutable i : int
  ; mutable pos : int array
  ; mutable delta : int array
  }

val resize : resize_data -> Source_map.Standard.t -> Source_map.Standard.t

val concatenate : (int * Source_map.Standard.t) list -> Source_map.t

val iter_sources : Source_map.t -> (int option -> int option -> string -> unit) -> unit

val insert_source_contents :
  Source_map.t -> (int option -> int option -> string -> string option) -> Source_map.t

val blackbox_filename : string

val blackbox_contents : string
