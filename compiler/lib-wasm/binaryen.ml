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

let available =
  lazy (Sys.command (Printf.sprintf "wasm-opt --version > %s 2>&1" Filename.null) = 0)

let available () = Lazy.force available

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
    ; "--enable-multimemory"
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

let uses_wasm_opt : Profile.t -> bool = function
  | O1 -> false
  | O2 | O3 -> true

let optimizes profile = uses_wasm_opt profile && available ()

let warn_if_unavailable profile =
  if uses_wasm_opt profile && not (available ())
  then
    Warning.warn
      `Missing_wasm_opt
      "Binaryen's wasm-opt was not found: the generated code is not optimized.@."

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
  if not (available ())
  then (
    (* Binaryen is optional: without it, the module is left unoptimized *)
    Fs.write_file ~name:output_file ~contents:(Fs.read_file input_file);
    match opt_input_sourcemap, opt_output_sourcemap with
    | Some input, Some output -> Fs.write_file ~name:output ~contents:(Fs.read_file input)
    | None, Some output ->
        Source_map.to_file
          (Standard (Source_map.Standard.empty ~inline_source_content:false))
          output
    | _, None -> ())
  else
    command
      (* [Wasm_output] and [Wat_output] emit [try_table] directly when
         targeting WASI, and so does Wax when assembling the runtime files;
         [--emit-exnref] converts any remaining legacy [try]/[catch] (from
         runtime modules provided in binary form) so the whole output is
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
