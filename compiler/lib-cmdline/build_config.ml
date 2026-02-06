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

open! Js_of_ocaml_compiler.Stdlib

type value_spec =
  | Bool of (bool -> unit)
  | Enum of string list * (string -> unit)

let to_string entries =
  let entries = List.sort ~cmp:(fun (k1, _) (k2, _) -> String.compare k1 k2) entries in
  String.concat ~sep:"+" (List.map ~f:(fun (k, v) -> k ^ "=" ^ v) entries)

let print_and_exit entries =
  Printf.printf "%s\n" (to_string entries);
  exit 0

let parse specs input =
  if not (String.is_empty input)
  then
    let parts = String.split_on_char ~sep:'+' input in
    List.iter parts ~f:(fun part ->
        match String.lsplit2 ~on:'=' part with
        | None -> failwith (Printf.sprintf "missing '=' in %S" part)
        | Some (key, value) -> (
            match List.string_assoc key specs with
            | None -> failwith (Printf.sprintf "unknown key %S" key)
            | Some (Bool f) -> (
                match value with
                | "true" -> f true
                | "false" -> f false
                | _ ->
                    failwith
                      (Printf.sprintf "key %S expects true or false, got %S" key value))
            | Some (Enum (valid, f)) ->
                if List.mem ~eq:String.equal value valid
                then f value
                else
                  failwith
                    (Printf.sprintf
                       "key %S expects one of {%s}, got %S"
                       key
                       (String.concat ~sep:", " valid)
                       value)))
