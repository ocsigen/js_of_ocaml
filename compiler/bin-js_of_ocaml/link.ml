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
  { common : Jsoo_cmdline.Arg.t
  ; source_map : Source_map.Encoding_spec.t option
  ; js_files : string list
  ; output_file : string option
  ; resolve_sourcemap_url : bool
  ; linkall : bool
  ; mklib : bool
  ; toplevel : bool
  }

let options =
  let output_file =
    let doc = "Set output file name to [$(docv)]." in
    Arg.(value & opt (some string) None & info [ "o" ] ~docv:"FILE" ~doc)
  in
  let no_sourcemap =
    let doc =
      "Don't generate source map. All other source map related flags will be ignored."
    in
    Arg.(value & flag & info [ "no-sourcemap"; "no-source-map" ] ~doc)
  in
  let sourcemap =
    let doc = "Generate source map." in
    Arg.(value & flag & info [ "sourcemap"; "source-map" ] ~doc)
  in
  let sourcemap_inline_in_js =
    let doc = "Inline sourcemap in the generated JavaScript." in
    Arg.(value & flag & info [ "source-map-inline" ] ~doc)
  in
  let sourcemap_empty =
    let doc = "Always generate empty source maps." in
    Arg.(value & flag & info [ "empty-sourcemap"; "empty-source-map" ] ~doc)
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
  let linkall =
    let doc = "Link all compilation units." in
    Arg.(value & flag & info [ "linkall" ] ~doc)
  in
  let mklib =
    let doc =
      "Build a library (.cma.js file) with the js files (.cmo.js files) given on the \
       command line. Similar to ocamlc -a."
    in
    Arg.(value & flag & info [ "a" ] ~doc)
  in
  let toplevel =
    let doc = "Compile a toplevel." in
    Arg.(value & flag & info [ "toplevel" ] ~doc)
  in
  let build_t
      common
      no_sourcemap
      sourcemap
      sourcemap_inline_in_js
      sourcemap_empty
      sourcemap_root
      output_file
      resolve_sourcemap_url
      js_files
      linkall
      mklib
      toplevel =
    let chop_extension s = try Filename.chop_extension s with Invalid_argument _ -> s in
    let source_map =
      if (not no_sourcemap) && (sourcemap || sourcemap_inline_in_js)
      then
        let file, sm_output_file =
          match output_file with
          | Some file when sourcemap_inline_in_js -> Some file, None
          | Some file -> Some file, Some (chop_extension file ^ ".map")
          | None -> None, None
        in
        let source_map =
          { (Source_map.Standard.empty ~inline_source_content:true) with
            file
          ; sourceroot = sourcemap_root
          }
        in
        let spec =
          { Source_map.Encoding_spec.output_file = sm_output_file
          ; source_map
          ; keep_empty = sourcemap_empty
          }
        in
        Some spec
      else None
    in
    `Ok
      { common
      ; output_file
      ; js_files
      ; source_map
      ; resolve_sourcemap_url
      ; linkall
      ; mklib
      ; toplevel
      }
  in
  let t =
    Term.(
      const build_t
      $ Lazy.force Jsoo_cmdline.Arg.t
      $ no_sourcemap
      $ sourcemap
      $ sourcemap_inline_in_js
      $ sourcemap_empty
      $ sourcemap_root
      $ output_file
      $ resolve_sourcemap_url
      $ js_files
      $ linkall
      $ mklib
      $ toplevel)
  in
  Term.ret t

let f
    { common
    ; output_file
    ; source_map
    ; resolve_sourcemap_url
    ; js_files
    ; linkall
    ; mklib
    ; toplevel
    } =
  Config.set_target `JavaScript;
  Jsoo_cmdline.Arg.eval common;
  Linker.reset ();
  let with_output f =
    match output_file with
    | None -> f stdout
    | Some file -> Filename.gen_file file f
  in
  with_output (fun output ->
      Link_js.link
        ~output
        ~linkall
        ~mklib
        ~toplevel
        ~files:js_files
        ~source_map
        ~resolve_sourcemap_url)

let info =
  Info.make
    ~name:"link"
    ~doc:"Js_of_ocaml linker"
    ~description:
      "js_of_ocaml-link is a JavaScript linker. It can concatenate multiple JavaScript \
       files keeping sourcemap information."

let command =
  let t = Cmdliner.Term.(const f $ options) in
  Cmdliner.Cmd.v info t
