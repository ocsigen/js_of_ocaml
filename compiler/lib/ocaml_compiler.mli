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

val find_loc_in_summary : Ident.t -> Env.summary -> Location.t option

module Symtable : sig
  module GlobalMap : sig
    type t

    val filter_global_map : (Ident.t -> bool) -> t -> t

    val find : Ident.t -> t -> int

    val iter : (Ident.t -> int -> unit) -> t -> unit

    val fold : (Ident.t -> int -> 'a -> 'a) -> t -> 'a -> 'a
  end

  val reloc_ident : string -> int
end

module Ident : sig
  type 'a tbl = 'a Ident.tbl

  val table_contents : int -> int Ident.tbl -> (int * string * Ident.t) list
end
