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
  | "-prefix" :: y :: xs ->
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

let args =
  let args = List.tl (Array.to_list Sys.argv) in
  let args = scan_args [] args in
  let js, args = List.partition (fun s -> Filename.check_suffix s ".js") args in
  let js = if !runtime then "+runtime.js" :: js else js in
  let all = Jsoo_common.cmis args in
  let all = List.map (fun x -> Filename.(concat !prefix (basename x)), x) all in
  let program = Js_of_ocaml_compiler.PseudoFs.program_of_files all in
  let oc =
    match !output, args with
    | Some x, _ -> open_out x
    | None, [x] -> open_out (x ^ ".cmis.js")
    | None, _ -> failwith "-o <name> needed"
  in
  Js_of_ocaml_compiler.Linker.load_files js;
  let pfs_fmt = Js_of_ocaml_compiler.Pretty_print.to_out_channel oc in
  Js_of_ocaml_compiler.Config.Flag.enable "pretty";
  Js_of_ocaml_compiler.Driver.f
    pfs_fmt
    (Js_of_ocaml_compiler.Parse_bytecode.Debug.create ())
    program
