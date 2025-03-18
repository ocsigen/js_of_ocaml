(* Js_of_ocaml
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2020 Hugo Heuzard
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

open Js_of_ocaml_compiler.Stdlib

let input_lines_text file =
  let ic = open_in_text file in
  let lines = In_channel.input_lines ic in
  close_in ic;
  lines

let () =
  let file = Sys.argv.(1) in
  let lines =
    input_lines_text file
    |> List.filter_map ~f:(String.drop_prefix ~prefix:"//# sourceMappingURL=")
  in
  let content =
    match lines with
    | [ line ] -> (
        match String.drop_prefix ~prefix:"data:application/json;base64," line with
        | None -> String.concat ~sep:"\n" (input_lines_text line)
        | Some base64 -> Js_of_ocaml_compiler.Base64.decode_exn base64)
    | _ -> failwith "unable to find sourcemap"
  in
  let sm = Js_of_ocaml_compiler.Source_map.of_string content in
  print_endline (Js_of_ocaml_compiler.Source_map.to_string sm)
