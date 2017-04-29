(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2017 Hugo Heuzard
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

let sourceMappingURL = "//# sourceMappingURL="
let sourceMappingURL_base64 = "//# sourceMappingURL=data:application/json;base64,"
let drop_prefix ~prefix s =
  let plen = String.length prefix in
  if plen > String.length s
  then None
  else begin
    try
      for i = 0 to String.length prefix - 1 do
        if String.get s i <> String.get prefix i
        then raise Exit
      done;
      Some (String.sub s plen (String.length s - plen))
    with Exit -> None
  end

let _ = drop_prefix ~prefix:"qwe:" "qwe"

let kind ~resolve_sourcemap_url file line =
  let s =
    match drop_prefix ~prefix:sourceMappingURL_base64 line with
    | Some base64 ->
      `Json_base64 base64
    | None ->
      match drop_prefix ~prefix:sourceMappingURL line with
      | Some url -> `Url url
      | None -> `Other
  in
  match s with
  | `Other -> `Other
  | `Json_base64 base64->
    `Source_map (Source_map_io.of_string (B64.decode base64))
  | `Url _ when not resolve_sourcemap_url ->
    `Drop
  | `Url url ->
    let base = Filename.dirname file in
    let ic = open_in (Filename.concat base url) in
    let l = in_channel_length ic in
    let content = really_input_string ic l in
    close_in ic;
    `Source_map (Source_map_io.of_string content)
;;

let link ~output ~files ~resolve_sourcemap_url ~source_map =
  let sm = ref [] in
  let line_offset = ref 0 in
  let new_line () =
    output_string output "\n";
    incr line_offset
  in
  let source_offset = ref 0 in
  List.iter (fun file ->
    let ic = open_in file in
    (try
       output_string output (Printf.sprintf "//# 1 %S" file);
       new_line ();
       let start_line = !line_offset in
       while true do
         let line = input_line ic in
         match kind ~resolve_sourcemap_url file line, source_map with
         | `Other, _ ->
           output_string output line;
           new_line ()
         | `Drop, _ -> ()
         | `Source_map _, None ->
           ()
         | `Source_map x, Some _ ->
           source_offset := List.length x.Source_map.sources;
           sm := (start_line, file, x) :: !sm
       done;
     with End_of_file -> ());
    close_in ic;
    new_line ();
  ) files;
  match source_map with
  | None -> ()
  | Some (file, init_sm) ->
    match Source_map.merge ((0,"",init_sm) :: List.rev !sm) with
    | None -> ()
    | Some sm ->
      match file with
      | None ->
        let data = Source_map_io.to_string sm in
        let s = sourceMappingURL_base64 ^ (B64.encode data) in
        output_string output s
      | Some file ->
        Source_map_io.to_file sm file;
        let s = sourceMappingURL ^ (Filename.basename file) in
        output_string output s
