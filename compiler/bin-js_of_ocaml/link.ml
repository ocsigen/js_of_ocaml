(* Js_of_ocaml compiler
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

open! Js_of_ocaml_compiler.Stdlib
open Js_of_ocaml_compiler
open Cmdliner

type t =
  { source_map : (string option * Source_map.t) option
  ; js_files : string list
  ; output_file : string option
  ; resolve_sourcemap_url : bool
  }

let options =
  let output_file =
    let doc = "Set output file name to [$(docv)]." in
    Arg.(value & opt (some string) None & info [ "o" ] ~docv:"FILE" ~doc)
  in
  let sourcemap =
    let doc = "Generate source map." in
    Arg.(value & flag & info [ "sourcemap"; "source-map" ] ~doc)
  in
  let sourcemap_inline_in_js =
    let doc = "Inline sourcemap in the generated JavaScript." in
    Arg.(value & flag & info [ "source-map-inline" ] ~doc)
  in
  let sourcemap_root =
    let doc = "root dir for source map." in
    Arg.(value & opt (some string) None & info [ "source-map-root" ] ~doc)
  in
  let resolve_sourcemap_url =
    let doc = "Resolve source map url." in
    Arg.(value & opt bool false & info [ "resolve-sourcemap-url" ] ~doc)
  in
  let js_files =
    let doc = "Link JavaScript files [$(docv)]." in
    Arg.(value & pos_all string [] & info [] ~docv:"JS_FILES" ~doc)
  in
  let build_t
      sourcemap
      sourcemap_inline_in_js
      sourcemap_root
      output_file
      resolve_sourcemap_url
      js_files =
    let chop_extension s = try Filename.chop_extension s with Invalid_argument _ -> s in
    let source_map =
      if sourcemap || sourcemap_inline_in_js
      then
        let file, sm_output_file =
          match output_file with
          | Some file when sourcemap_inline_in_js -> file, None
          | Some file -> file, Some (chop_extension file ^ ".map")
          | None -> "STDIN", None
        in
        Some
          ( sm_output_file
          , { Source_map.version = 3
            ; file
            ; sourceroot = sourcemap_root
            ; sources = []
            ; sources_content = Some []
            ; names = []
            ; mappings = []
            } )
      else None
    in
    `Ok { output_file; js_files; source_map; resolve_sourcemap_url }
  in
  let t =
    Term.(
      pure build_t
      $ sourcemap
      $ sourcemap_inline_in_js
      $ sourcemap_root
      $ output_file
      $ resolve_sourcemap_url
      $ js_files)
  in
  Term.ret t

let f { output_file; source_map; resolve_sourcemap_url; js_files } =
  let with_output f =
    match output_file with
    | None -> f stdout
    | Some file -> Filename.gen_file file f
  in
  with_output (fun output ->
      Link_js.link ~output ~files:js_files ~source_map ~resolve_sourcemap_url)

let info =
  Info.make
    ~name:"link"
    ~doc:"Js_of_ocaml linker"
    ~description:
      "js_of_ocaml-link is a JavaScript linker. It can concatenate multiple JavaScript \
       files keeping sourcemap information."

let command = Cmdliner.Term.(pure f $ options), info
