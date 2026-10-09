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

let times = Debug.find "binaryen-times"

let command cmdline =
  let cmdline = String.concat ~sep:" " cmdline in
  if debug () then Format.eprintf "+ %s@." cmdline;
  let res = Sys.command ((if times () then "BINARYEN_PASS_DEBUG=1 " else "") ^ cmdline) in
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
    ; "--enable-multimemory" (* To keep wasm-merge happy *)
    ]
  in
  let l =
    match Config.effects () with
    | `Native -> "--enable-stack-switching" :: l
    | `Disabled | `Jspi | `Cps | `Double_translation -> l
  in
  let l = if Config.Flag.pretty () then "-g" :: l else l in
  let l = if times () then "--no-validation" :: l else l in
  l

let opt_flag flag v =
  match v with
  | None -> []
  | Some v -> [ flag; Filename.quote v ]

type link_input =
  { module_name : string
  ; file : string
  ; source_map_file : string option
  }

let link ?options ~inputs ~opt_output_sourcemap ~output_file () =
  command
    ("wasm-merge"
    :: (common_options ()
       @ Option.value ~default:[] options
       @ List.flatten
           (List.map
              ~f:(fun { file; module_name; source_map_file } ->
                Filename.quote file
                :: module_name
                ::
                (match source_map_file with
                | None -> []
                | Some file -> [ "--input-source-map"; Filename.quote file ]))
              inputs)
       @ [ "-o"; Filename.quote output_file ]
       @ opt_flag "--output-source-map" opt_output_sourcemap))

let optimization_options : Profile.t -> _ = function
  | O1 -> [ "-O2"; "--skip-pass=inlining-optimizing"; "--traps-never-happen" ]
  | O2 -> [ "-O2"; "--skip-pass=inlining-optimizing"; "--traps-never-happen" ]
  | O3 -> [ "-O3"; "--skip-pass=inlining-optimizing"; "--traps-never-happen" ]

let optimize
    ~profile
    ?options
    ?(reorder_functions = false)
    ~opt_input_sourcemap
    ~input_file
    ~opt_output_sourcemap
    ~output_file
    () =
  command
    (* [--emit-exnref] is needed even though [Wasm_output] and
       [Wat_output] now emit [try_table] directly when targeting WASI:
       the runtime [.wat] files still use the legacy [try]/[catch] syntax,
       and this flag converts them to [try_table] so the whole output is
       uniformly in the new form. *)
    (("wasm-opt" :: (if Config.Flag.wasi () then [ "--emit-exnref" ] else []))
    @ common_options ()
    @ (match options with
      | Some o -> o
      | None -> optimization_options profile)
    @ (if reorder_functions then [ "--reorder-functions" ] else [])
    @ [ Filename.quote input_file; "-o"; Filename.quote output_file ]
    @ opt_flag "--input-source-map" opt_input_sourcemap
    @ opt_flag "--output-source-map" opt_output_sourcemap)
