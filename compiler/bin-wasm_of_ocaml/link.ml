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

open! Js_of_ocaml_compiler.Stdlib
open Wasm_of_ocaml_compiler
open Cmdliner

type t =
  { common : Jsoo_cmdline.Arg.t
  ; files : string list
  ; output_file : string
  ; linkall : bool
  ; enable_source_maps : bool
  }

let options =
  let output_file =
    let doc = "Set output file name to [$(docv)]." in
    Arg.(required & opt (some string) None & info [ "o" ] ~docv:"FILE" ~doc)
  in
  let no_sourcemap =
    let doc = "Disable sourcemap output." in
    Arg.(value & flag & info [ "no-sourcemap"; "no-source-map" ] ~doc)
  in
  let sourcemap =
    let doc = "Output source locations." in
    Arg.(value & flag & info [ "sourcemap"; "source-map"; "source-map-inline" ] ~doc)
  in
  let files =
    let doc =
      "Link the archive files [$(docv)]. The first archive must be a runtime produced by \
       $(b,wasm_of_ocaml build-runtime). The other archives can be produced by compiling \
       .cma or .cmo files."
    in
    Arg.(non_empty & pos_all string [] & info [] ~docv:"FILES" ~doc)
  in
  let linkall =
    let doc = "Link all compilation units." in
    Arg.(value & flag & info [ "linkall" ] ~doc)
  in
  let build_t common no_sourcemap sourcemap output_file files linkall =
    let enable_source_maps = (not no_sourcemap) && sourcemap in
    `Ok { common; output_file; files; linkall; enable_source_maps }
  in
  let t =
    Term.(
      const build_t
      $ Jsoo_cmdline.Arg.t
      $ no_sourcemap
      $ sourcemap
      $ output_file
      $ files
      $ linkall)
  in
  Term.ret t

let f { common; output_file; files; linkall; enable_source_maps } =
  Jsoo_cmdline.Arg.eval common;
  Wa_link.link ~output_file ~linkall ~enable_source_maps ~files

let info =
  Info.make
    ~name:"link"
    ~doc:"Wasm_of_ocaml linker"
    ~description:
      "wasm_of_ocaml-link is a JavaScript linker. It can concatenate multiple JavaScript \
       files keeping sourcemap information."

let command =
  let t = Cmdliner.Term.(const f $ options) in
  Cmdliner.Cmd.v info t
