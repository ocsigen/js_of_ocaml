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

(* Keep in sync with runtime/wasm/check_binaryen.ml *)
let min_version = 120

type status =
  | Available
  | Missing
  | Too_old of int
  | Unknown_version of string

let status =
  lazy
    (let tmp = Filename.temp_file "wasm-opt" ".version" in
     let line =
       if
         Sys.command (Printf.sprintf "wasm-opt --version > %s 2>&1" (Filename.quote tmp))
         = 0
       then
         match String.split_on_char ~sep:'\n' (Fs.read_file tmp) with
         | line :: _ -> Some line
         | [] -> None
       else None
     in
     (try Sys.remove tmp with Sys_error _ -> ());
     match line with
     | None -> Missing
     | Some line -> (
         match Scanf.sscanf line "wasm-opt version %d" (fun v -> v) with
         | v when v >= min_version -> Available
         | v -> Too_old v
         | exception (Scanf.Scan_failure _ | Failure _ | End_of_file) ->
             Unknown_version line))

(* An unusable wasm-opt (too old, or whose version cannot be determined) is
   treated as missing: it may not support the features and options we use. *)
let available () =
  match Lazy.force status with
  | Available -> true
  | Missing | Too_old _ | Unknown_version _ -> false

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

let warned = ref false

let warn_if_unavailable profile =
  if uses_wasm_opt profile && not !warned
  then (
    warned := true;
    match Lazy.force status with
    | Available -> ()
    | Missing ->
        Warning.warn
          `Missing_wasm_opt
          "Binaryen's wasm-opt was not found: the generated code is not optimized.@."
    | Too_old v ->
        Warning.warn
          `Missing_wasm_opt
          "Binaryen's wasm-opt version %d is too old (version %d or later is needed): \
           the generated code is not optimized.@."
          v
          min_version
    | Unknown_version line ->
        Warning.warn
          `Missing_wasm_opt
          "Could not determine the version of Binaryen's wasm-opt from %S: the generated \
           code is not optimized.@."
          line)

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
