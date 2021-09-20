(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2019 Ty Overby
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
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
open Util

let%expect_test _ =
  with_temp_dir ~f:(fun () ->
      let name = "test.ml" in
      Filetype.write_file name "let id x = x";
      let file = Filetype.ocaml_file_of_path name in
      file
      |> compile_ocaml_to_cmo
      |> compile_cmo_to_javascript ~sourcemap:true ~pretty:false
      |> extract_sourcemap
      |> function
      | Some (sm : Js_of_ocaml_compiler.Source_map.t) ->
          Printf.printf "file: %s\n" sm.file;
          Printf.printf "sourceRoot: %s\n" (Option.value ~default:"<none>" sm.sourceroot);
          Printf.printf "sources:\n";
          List.iter sm.sources ~f:(fun source ->
              Printf.printf "- %s\n" (normalize_path source))
      | None -> failwith "no sourcemap generated!");
  [%expect
    {|
      file: test.js
      sourceRoot:
      sources:
      - /dune-root/test.ml
    |}]
