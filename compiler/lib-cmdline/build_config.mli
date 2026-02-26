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

val process :
     ?extra_keys:Js_of_ocaml_compiler.Build_info.config_key list
  -> [ `JavaScript | `Wasm ]
  -> apply:string option
  -> print_and_exit:bool
  -> unit
(** [process ?extra_keys target ~apply ~print_and_exit] optionally
    parses and applies a build config string, then optionally prints the
    current config and exits. *)

val parse : Js_of_ocaml_compiler.Build_info.config_key list -> string -> unit
(** Parse "key1=val1+key2=val2", validate against known
    keys/values, and apply callbacks. Raises [Failure] on
    invalid input. *)

val print_and_exit : Js_of_ocaml_compiler.Build_info.config_key list -> 'a
(** Print entries as sorted "key1=val1+key2=val2" to stdout and
    exit. *)
