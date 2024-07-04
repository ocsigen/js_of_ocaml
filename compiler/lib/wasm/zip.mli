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

type output

val open_out : string -> output

val add_entry : output -> name:string -> contents:string -> unit

val add_file : output -> name:string -> file:string -> unit

val close_out : output -> unit

type input

val open_in : string -> input

val with_open_in : string -> (input -> 'a) -> 'a

val has_entry : input -> name:string -> bool

val read_entry : input -> name:string -> string

val get_entry :
  input -> name:string -> in_channel * int (* pos *) * int (* len *) * int32 (* crc *)

val extract_file : input -> name:string -> file:string -> unit

val copy_file : input -> output -> src_name:string -> dst_name:string -> unit

val close_in : input -> unit
