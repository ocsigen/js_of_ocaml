(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2014 Hugo Heuzard
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

(* Helper to generate a javascript file containing
   cmis needed to use findlib packages in the toplevel *)
(* #use "topfind" *)
(* #require "findlib" *)
(* #require "js_of_ocaml.compiler" *)
(* #require "compiler-libs.common" *)

open Js_of_ocaml_compiler.Stdlib

let prefix = ref "/static/cmis"

let output = ref None

let runtime = ref true

let usage () =
  Format.eprintf "Usage: jsoo_mkcmis [options] [find packages] @.";
  Format.eprintf " -verbose@.";
  Format.eprintf " -help\t\t\tDisplay usage@.";
  Format.eprintf " -prefix [dir]\t\tStore cmi files in [dir] (default /static/cmis)@.";
  Format.eprintf " -o [name]\t\tSet output filename@.";
  exit 1

let rec scan_args acc = function
  | ("--verbose" | "-verbose") :: xs ->
      Jsoo_common.verbose := true;
      scan_args acc xs
  | ("--prefix" | "-prefix") :: y :: xs ->
      prefix := y;
      scan_args acc xs
  | "-o" :: name :: xs ->
      output := Some name;
      scan_args acc xs
  | ("--no-runtime" | "--noruntime") :: xs ->
      runtime := false;
      scan_args acc xs
  | ("--help" | "-help" | "-h") :: _ -> usage ()
  | x :: xs -> scan_args (x :: acc) xs
  | [] -> List.rev acc

let () =
  Js_of_ocaml_compiler.Config.set_target `JavaScript;
  Js_of_ocaml_compiler.Config.set_effects_backend `Disabled;
  let args = List.tl (Array.to_list Sys.argv) in
  let args = scan_args [] args in
  let runtime_files, args =
    List.partition ~f:(fun s -> Filename.check_suffix s ".js") args
  in
  let runtime_files, builtin =
    List.partition_map runtime_files ~f:(fun name ->
        match Js_of_ocaml_compiler.Builtins.find name with
        | Some t -> Right t
        | None -> Left name)
  in
  let builtin =
    if !runtime then Js_of_ocaml_compiler_runtime_files.runtime @ builtin else builtin
  in

  List.iter builtin ~f:(fun t ->
      let filename = Js_of_ocaml_compiler.Builtins.File.name t in
      let runtimes = Js_of_ocaml_compiler.Linker.Fragment.parse_builtin t in
      Js_of_ocaml_compiler.Linker.load_fragments ~target_env:Isomorphic ~filename runtimes);
  Js_of_ocaml_compiler.Linker.load_files ~target_env:Isomorphic runtime_files;
  Js_of_ocaml_compiler.Linker.check_deps ();
  let all = Jsoo_common.cmis args in
  let instr =
    List.map all ~f:(fun filename ->
        let name = Filename.(concat !prefix (basename filename)) in
        Js_of_ocaml_compiler.Pseudo_fs.embed_file ~name ~filename)
  in
  let program = Js_of_ocaml_compiler.Code.prepend Js_of_ocaml_compiler.Code.empty instr in
  let oc =
    match !output, args with
    | Some x, _ -> open_out_bin x
    | None, [ x ] -> open_out_bin (x ^ ".cmis.js")
    | None, _ -> failwith "-o <name> needed"
  in
  let pfs_fmt = Js_of_ocaml_compiler.Pretty_print.to_out_channel oc in
  Js_of_ocaml_compiler.Config.Flag.enable "pretty";
  Js_of_ocaml_compiler.Driver.f' pfs_fmt ~link:`Needed program
