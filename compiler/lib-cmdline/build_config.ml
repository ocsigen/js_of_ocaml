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

open Js_of_ocaml_compiler
open! Stdlib

let print_and_exit keys =
  Printf.printf "%s\n" (Build_info.to_config_string (Build_info.get_values keys));
  exit 0

let parse keys input = Build_info.set_values keys (Build_info.parse_config_string input)

let process target ~apply ~print_and_exit:do_print =
  let keys = Build_info.config_keys target in
  (match apply with
  | None -> ()
  | Some s -> parse keys s);
  if do_print then print_and_exit keys
