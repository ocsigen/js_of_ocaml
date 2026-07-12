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
  ; dynlink : bool
  ; esm : bool
  ; bundle : bool
  ; no_tree_shake : bool
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
  let dynlink =
    let doc = "Enable dynlink/toplevel support." in
    Arg.(value & flag & info [ "dynlink"; "toplevel" ] ~doc)
  in
  let esm =
    let doc =
      "Experimental: link ECMAScript modules produced by 'js_of_ocaml --esm'. The output \
       is an entry module importing every file in order; with '--bundle' the modules are \
       merged into a single self-contained module instead."
    in
    Arg.(value & flag & info [ "esm" ] ~doc)
  in
  let bundle =
    let doc =
      "Experimental: bundle the modules into a single module (requires '--esm')."
    in
    Arg.(value & flag & info [ "bundle" ] ~doc)
  in
  let no_tree_shake =
    let doc = "Experimental: disable tree shaking when bundling." in
    Arg.(value & flag & info [ "no-tree-shake" ] ~doc)
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
      dynlink
      esm
      bundle
      no_tree_shake =
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
      ; dynlink
      ; esm
      ; bundle
      ; no_tree_shake
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
      $ dynlink
      $ esm
      $ bundle
      $ no_tree_shake)
  in
  Term.ret t

(* ES module linking (experimental). Paths are used as module identifiers:
   normalize them so that resolved import specifiers and command-line paths
   agree on separators and [.]/[..] components. *)
let normalize_path path =
  let path =
    if Sys.win32
    then String.map path ~f:(fun c -> if Char.equal c '\\' then '/' else c)
    else path
  in
  let parts = String.split_on_char ~sep:'/' path in
  let rec normalize acc = function
    | [] -> List.rev acc
    | "." :: rest -> normalize acc rest
    | ".." :: rest -> (
        match acc with
        | (".." | "") :: _ | [] -> normalize (".." :: acc) rest
        | _ :: acc' -> normalize acc' rest)
    | part :: rest -> normalize (part :: acc) rest
  in
  String.concat ~sep:"/" (normalize [] parts)

let absolute_path path =
  normalize_path
    (if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path else path)

(* The specifier importing [path] from a module located in [dir]. *)
let relative_specifier ~dir path =
  let rec strip_common_prefix l1 l2 =
    match l1, l2 with
    | x :: r1, y :: r2 when String.equal x y -> strip_common_prefix r1 r2
    | _ -> l1, l2
  in
  let dir_parts = String.split_on_char ~sep:'/' (absolute_path dir) in
  let path_parts = String.split_on_char ~sep:'/' (absolute_path path) in
  let dir_rest, path_rest = strip_common_prefix dir_parts path_parts in
  let ups = List.map dir_rest ~f:(fun _ -> "..") in
  let parts =
    match ups with
    | [] -> "." :: path_rest
    | _ -> ups @ path_rest
  in
  String.concat ~sep:"/" parts

let link_esm ~output_file ~files ~bundle ~tree_shake =
  let output_dir =
    match output_file with
    | Some file -> Filename.dirname file
    | None -> "."
  in
  let with_output f =
    match output_file with
    | None -> f stdout
    | Some file -> Filename.gen_file file f
  in
  if not bundle
  then
    (* Emit an entry module importing each file in link order. ES semantics
       guarantee the modules are evaluated in declaration order (post-order
       depth-first), so the runtime imported by each unit runs first. *)
    with_output (fun output ->
        List.iter files ~f:(fun file ->
            Printf.fprintf output "import %S;\n" (relative_specifier ~dir:output_dir file)))
  else
    let parse path =
      let lexer = Parse_js.Lexer.of_string ~filename:path (Fs.read_file path) in
      Parse_js.parse `Module lexer
    in
    let resolve ~from specifier =
      if
        Filename.is_relative specifier
        && (String.starts_with ~prefix:"./" specifier
           || String.starts_with ~prefix:"../" specifier)
      then
        let dir = Filename.dirname from in
        let resolved = normalize_path (Filename.concat dir specifier) in
        if Sys.file_exists resolved then Some resolved else None
      else None
    in
    let entry_points = List.map files ~f:absolute_path in
    let program = Esm_bundle.bundle_modules ~parse ~resolve ~entry_points ~tree_shake in
    with_output (fun output ->
        let pp = Pretty_print.to_out_channel output in
        Driver.configure pp;
        let program = Driver.name_variables program in
        let (_ : Source_map.info) = Js_output.program pp program in
        ())

let f
    { common
    ; output_file
    ; source_map
    ; resolve_sourcemap_url
    ; js_files
    ; linkall
    ; mklib
    ; esm
    ; bundle
    ; no_tree_shake
    ; dynlink =
        _
        (* TODO: when [dynlink] (i.e. --dynlink/--toplevel) is false, the linker
         could optimize global references. Currently, [caml_register_global] and
         [caml_get_global] use string-keyed lookups on [caml_global_data]. Since
         the linker already knows the full symbol table, it could replace these
         with index-based accesses ([caml_register_global_by_index]) and skip
         emitting [caml_set_link_info] entirely. *)
    } =
  Config.set_target `JavaScript;
  Jsoo_cmdline.Arg.eval common;
  Linker.reset ();
  if esm
  then link_esm ~output_file ~files:js_files ~bundle ~tree_shake:(not no_tree_shake)
  else
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
