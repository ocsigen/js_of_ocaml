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

val constant_of_const : Lambda.structured_constant -> Code.constant

type module_or_not =
  | Module
  | Not_module
  | Unknown

val is_module_in_summary : Ident.t -> Env.summary -> module_or_not

module Symtable : sig
  module GlobalMap : sig
    type t

    val empty : t

    val filter : (Global_name.t -> bool) -> t -> t

    val find : Global_name.t -> t -> int

    val iter : f:(Global_name.t -> int -> unit) -> t -> unit

    val fold : (Global_name.t -> int -> 'a -> 'a) -> t -> 'a -> 'a

    val enter : t ref -> Global_name.t -> int
  end

  val reloc_ident : string -> int

  val current_state : unit -> GlobalMap.t

  val all_primitives : unit -> string list
end

module Cmo_format : sig
  type t = Cmo_format.compilation_unit

  val name : t -> Global_name.compunit

  val requires : t -> Global_name.compunit list

  val provides : t -> Global_name.compunit list

  val primitives : t -> string list

  val force_link : t -> bool

  val imports : t -> (string * string option) list
end
