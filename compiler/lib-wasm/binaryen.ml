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

open Stdlib

let debug = Debug.find "binaryen"

let command cmdline =
  let cmdline = String.concat ~sep:" " cmdline in
  if debug () then Format.eprintf "+ %s@." cmdline;
  let res = Sys.command cmdline in
  if res <> 0 then failwith ("the following command terminated unsuccessfully: " ^ cmdline)

let common_options () =
  let l =
    [ "--enable-gc"
    ; "--enable-multivalue"
    ; "--enable-exception-handling"
    ; "--enable-reference-types"
    ; "--enable-tail-call"
    ; "--enable-bulk-memory"
    ; "--enable-nontrapping-float-to-int"
    ; "--enable-strings"
    ]
  in
  if Config.Flag.pretty () then "-g" :: l else l

let opt_flag flag v =
  match v with
  | None -> []
  | Some v -> [ flag; Filename.quote v ]

let link ~runtime_files ~input_files ~opt_output_sourcemap ~output_file =
  command
    ("wasm-merge"
    :: (common_options ()
       @ List.flatten
           (List.map
              ~f:(fun runtime_file -> [ Filename.quote runtime_file; "env" ])
              runtime_files)
       @ List.flatten
           (List.map
              ~f:(fun input_file -> [ Filename.quote input_file; "OCaml" ])
              input_files)
       @ [ "-o"; Filename.quote output_file ]
       @ opt_flag "--output-source-map" opt_output_sourcemap))

let generate_dependencies ~dependencies primitives =
  Yojson.Basic.to_string
    (`List
       (StringSet.fold
          (fun nm s ->
            `Assoc
              [ "name", `String ("js:" ^ nm)
              ; "import", `List [ `String "js"; `String nm ]
              ]
            :: s)
          primitives
          (Yojson.Basic.Util.to_list (Yojson.Basic.from_string dependencies))))

let filter_unused_primitives primitives usage_file =
  let ch = open_in usage_file in
  let s = ref primitives in
  (try
     while true do
       let l = input_line ch in
       match String.drop_prefix ~prefix:"unused: js:" l with
       | Some nm -> s := StringSet.remove nm !s
       | None -> ()
     done
   with End_of_file -> ());
  !s

let dead_code_elimination
    ~dependencies
    ~opt_input_sourcemap
    ~input_file
    ~opt_output_sourcemap
    ~output_file =
  Fs.with_intermediate_file (Filename.temp_file "deps" ".json")
  @@ fun deps_file ->
  Fs.with_intermediate_file (Filename.temp_file "usage" ".txt")
  @@ fun usage_file ->
  let primitives = Linker.list_all () in
  Fs.write_file ~name:deps_file ~contents:(generate_dependencies ~dependencies primitives);
  command
    ("wasm-metadce"
    :: (common_options ()
       @ [ "--graph-file"; Filename.quote deps_file; Filename.quote input_file ]
       @ opt_flag "--input-source-map" opt_input_sourcemap
       @ [ "-o"; Filename.quote output_file ]
       @ opt_flag "--output-source-map" opt_output_sourcemap
       @ [ ">"; Filename.quote usage_file ]));
  filter_unused_primitives primitives usage_file

let optimization_options =
  [| [ "-O2"; "--skip-pass=inlining-optimizing"; "--traps-never-happen" ]
   ; [ "-O2"; "--skip-pass=inlining-optimizing"; "--traps-never-happen" ]
   ; [ "-O3"; "--skip-pass=inlining-optimizing"; "--traps-never-happen" ]
  |]

let optimize ~profile ~opt_input_sourcemap ~input_file ~opt_output_sourcemap ~output_file
    =
  let level =
    match profile with
    | None -> 1
    | Some p -> fst (List.find ~f:(fun (_, p') -> Poly.equal p p') Driver.profiles)
  in
  command
    ("wasm-opt"
     :: (common_options ()
        @ optimization_options.(level - 1)
        @ [ Filename.quote input_file; "-o"; Filename.quote output_file ])
    @ opt_flag "--input-source-map" opt_input_sourcemap
    @ opt_flag "--output-source-map" opt_output_sourcemap)
