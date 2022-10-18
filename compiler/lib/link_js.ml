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
open! Stdlib

let sourceMappingURL = "//# sourceMappingURL="

let sourceMappingURL_base64 = "//# sourceMappingURL=data:application/json;base64,"

type action =
  | Keep
  | Drop
  | Source_map of Source_map.t

let action ~resolve_sourcemap_url ~drop_source_map file line =
  let prefix_kind =
    match String.is_prefix ~prefix:sourceMappingURL line with
    | false -> `Other
    | true -> (
        match String.is_prefix ~prefix:sourceMappingURL_base64 line with
        | true -> `Json_base64 (String.length sourceMappingURL_base64)
        | false -> `Url (String.length sourceMappingURL))
  in
  match prefix_kind, drop_source_map with
  | `Other, (true | false) -> Keep
  | (`Json_base64 _ | `Url _), true -> Drop
  | `Json_base64 offset, false ->
      Source_map (Source_map_io.of_string (Base64.decode_exn ~off:offset line))
  | `Url _, false when not resolve_sourcemap_url -> Drop
  | `Url offset, false ->
      let url = String.sub line ~pos:offset ~len:(String.length line - offset) in
      let base = Filename.dirname file in
      let ic = open_in (Filename.concat base url) in
      let l = in_channel_length ic in
      let content = really_input_string ic l in
      close_in ic;
      Source_map (Source_map_io.of_string content)

let link ~output ~files ~resolve_sourcemap_url ~source_map =
  let sm = ref [] in
  let line_offset = ref 0 in
  let new_line () =
    output_string output "\n";
    incr line_offset
  in
  List.iter
    ~f:(fun file ->
      let ic = open_in file in
      (try
         output_string output (Printf.sprintf "//# 1 %S" file);
         new_line ();
         let start_line = !line_offset in
         while true do
           let line = input_line ic in
           match
             action
               ~resolve_sourcemap_url
               ~drop_source_map:Poly.(source_map = None)
               file
               line
           with
           | Keep ->
               output_string output line;
               new_line ()
           | Drop -> ()
           | Source_map x -> sm := (start_line, x) :: !sm
         done
       with End_of_file -> ());
      close_in ic;
      new_line ())
    files;
  match source_map with
  | None -> ()
  | Some (file, init_sm) -> (
      match Source_map.merge ((0, init_sm) :: List.rev !sm) with
      | None -> ()
      | Some sm -> (
          (* preserve some info from [init_sm] *)
          let sm =
            { sm with
              version = init_sm.version
            ; file = init_sm.file
            ; sourceroot = init_sm.sourceroot
            }
          in
          match file with
          | None ->
              let data = Source_map_io.to_string sm in
              let s = sourceMappingURL_base64 ^ Base64.encode_exn data in
              output_string output s
          | Some file ->
              Source_map_io.to_file sm file;
              let s = sourceMappingURL ^ Filename.basename file in
              output_string output s))
