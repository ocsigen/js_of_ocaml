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

module Import_info : sig
  type t

  type table

  val make : string -> Digest.t option -> t

  val to_list : table -> t list

  val of_list : t list -> table

  val name : t -> string

  val crc : t -> Digest.t option
end

module Compilation_unit : sig
  type t = Cmo_format.compunit

  val full_path_as_string : t -> string
end
[@@if (not oxcaml) && ocaml_version >= (5, 2, 0)]

module Compilation_unit : sig
  type t = Compilation_unit.t

  val full_path_as_string : t -> string
end
[@@if oxcaml]

module Compilation_unit_descr : sig
  type t = Cmo_format.compilation_unit
end
[@@if not oxcaml]

module Compilation_unit_descr : sig
  type t = Cmo_format.compilation_unit_descr
end
[@@if oxcaml]

module Cmo_format : sig
  type t = Compilation_unit_descr.t

  val name : t -> Global_name.compunit

  val requires : t -> Global_name.compunit list

  val provides : t -> Global_name.compunit list

  val primitives : t -> string list

  val force_link : t -> bool

  val imports : t -> Import_info.t list
end
