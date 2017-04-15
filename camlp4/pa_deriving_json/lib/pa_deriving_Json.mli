(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright Hugo Heuzard 2014.
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

(** Deriving_json syntax extension (based on the {i deriving} library).
    @see <https://github.com/ocsigen/deriving> the source code of {i deriving}
    @see <http://code.google.com/p/deriving/> the documentation of the original {i deriving} library by Jeremy Yallop.
*)

(**
[type typ = .. deriving (Json)] with generate
{[module Json_typ : sig
  type a = typ
  val to_string : a -> string
  val from_string : string -> a
  ...
end]}
*)

open Pa_deriving_common

val register_predefs : Type.qname -> Type.qname -> unit
(** [register_predefs typeA typeB] tells the syntax extension
    that [typeB] is an alias for [typeA].
    The extension syntax will then use the deriving_json implementation
    of [typeB] in place of [typeA]'s one.

    Important note: TypeB does NOT HAVE TO actually exists.

    In practive, you may need to use this in the following cases:
     - use deriving_json on types from external libraries that do not use deriving
     - use deriving_json on abstract types
*)
