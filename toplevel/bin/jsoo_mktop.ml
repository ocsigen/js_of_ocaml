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

(* Helper to compile js_of_ocaml toplevel *)

(* #use "topfind" *)
(* #require "findlib" *)

let js_opt = ref []

let add_js_opt x = js_opt := !js_opt @ [ x ]

let pkgs = ref [ "stdlib" ]

let add_pkgs x = pkgs := !pkgs @ [ x ]

let export = ref []

let add_export x = export := !export @ [ x ]

let execute cmd =
  let s = String.concat " " cmd in
  if !Jsoo_common.verbose then Printf.printf "+ %s\n%!" s;
  let ret = Sys.command s in
  if ret <> 0 then failwith (Printf.sprintf "Error: %s" s)

let to_clean = ref []

let clean file = to_clean := file :: !to_clean

let do_clean () = List.iter (fun x -> if Sys.file_exists x then Sys.remove x) !to_clean

let usage () =
  Format.eprintf "Usage: jsoo_mktop [options] [ocamlfind arguments] @.";
  Format.eprintf " -verbose\t\t\tOutput intermediate commands@.";
  Format.eprintf " -help\t\t\t\tDisplay usage@.";
  Format.eprintf " -o [name]\t\t\tSet output filename@.";
  Format.eprintf " -jsopt [opt]\t\t\tPass [opt] option to js_of_ocaml compiler@.";
  Format.eprintf " -export-package [pkg]\t\tCompile toplevel with [pkg] package loaded@.";
  Format.eprintf " -export-unit [unit]\t\tExport [unit]@.";
  exit 1

let output = ref "./a.out"

let rec scan_args acc = function
  | ("--verbose" | "-verbose") :: xs ->
      Jsoo_common.verbose := true;
      scan_args ("-verbose" :: acc) xs
  | ("--help" | "-help" | "-h") :: _ -> usage ()
  | "-jsopt" :: x :: xs ->
      add_js_opt x;
      scan_args acc xs
  | "-o" :: x :: xs ->
      output := x;
      scan_args acc xs
  | "-export-package" :: x :: xs ->
      add_pkgs x;
      scan_args (x :: "-package" :: acc) xs
  | "-export-unit" :: x :: xs ->
      add_export x;
      scan_args acc xs
  | x :: xs -> scan_args (x :: acc) xs
  | [] -> List.rev acc

let _ =
  try
    let jsoo_top = [ "-package"; "js_of_ocaml-toplevel" ] in
    let base_cmd = [ "ocamlfind"; "ocamlc"; "-linkall"; "-linkpkg" ] in
    let args = List.tl (Array.to_list Sys.argv) in
    let args = scan_args [] args in
    List.iter (fun pkg -> execute [ "jsoo_mkcmis"; pkg ]) !pkgs;
    let toplevel_unit =
      let dir = Findlib.package_directory "compiler-libs" in
      List.map
        (fun x -> Filename.concat dir x ^ ".cmi")
        [ "outcometree"; "topdirs"; "toploop" ]
    in
    execute ([ "jsoo_mkcmis"; "-o"; "exported-unit.cmis.js" ] @ !export @ toplevel_unit);
    let export_output = !output ^ ".export" in
    let cmd = base_cmd @ jsoo_top @ args @ [ "-o"; !output ] in
    execute cmd;
    execute ([ "jsoo_listunits"; "-o"; export_output ] @ !pkgs @ !export);
    clean export_output;
    execute
      ([ "js_of_ocaml"; "--toplevel"; "--no-cmis"; "--export"; export_output ]
      @ !js_opt
      @ [ !output ]);
    do_clean ()
  with exn ->
    do_clean ();
    raise exn
