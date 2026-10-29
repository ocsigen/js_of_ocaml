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

val link_input :
     ?load_source_map:(string -> Source_map.Standard.t)
  -> Binaryen.link_input
  -> Wasm_link.input
(** Linker input for a module in binary format. Its source map, if any, is
    loaded with [load_source_map]; it is ignored when this function is
    not provided. *)

val link :
     ?remove_dead_code:bool
  -> inputs:Wasm_link.input list
  -> output_file:string
  -> unit
  -> unit
(** Link the input modules (in binary format), keeping all their code unless
    [remove_dead_code] is set: then, all exports are kept but the code
    and imports they do not use are removed. *)

val build :
     allowed_imports:string list option
  -> opt_options:string list
  -> variables:(string * Wax_link.value) list
  -> inputs:Wax_link.input list
  -> output_file:string
  -> unit
