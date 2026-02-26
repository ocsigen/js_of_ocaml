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

let validate_and_set keys (key, value) =
  match
    List.find_opt keys ~f:(fun k -> String.equal (Build_info.config_key_name k) key)
  with
  | None -> failwith (Printf.sprintf "unknown key %S" key)
  | Some (Build_info.Bool_key { set; _ }) -> (
      match value with
      | "true" -> set true
      | "false" -> set false
      | _ -> failwith (Printf.sprintf "key %S expects true or false, got %S" key value))
  | Some (Build_info.Enum_key { set; valid; _ }) ->
      if List.mem ~eq:String.equal value valid
      then set value
      else
        failwith
          (Printf.sprintf
             "key %S expects one of {%s}, got %S"
             key
             (String.concat ~sep:", " valid)
             value)

let parse keys input =
  List.iter (Build_info.parse_config_string input) ~f:(validate_and_set keys)

let process ?(extra_keys = []) target ~apply ~print_and_exit:do_print =
  let keys = extra_keys @ Build_info.config_keys target in
  (match apply with
  | None -> ()
  | Some s -> parse keys s);
  if do_print then print_and_exit keys
