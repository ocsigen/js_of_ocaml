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

(* Helper to compile js_of_ocaml toplevel
   with support for camlp4 syntax extensions *)

(* #use "topfind" *)
(* #require "findlib" *)

let js_opt = ref []
let add_js_opt x = js_opt:=!js_opt@[x]

let pkgs = ref ["stdlib"]
let add_pkgs x = pkgs:=!pkgs@[x]

let export = ref ["outcometree";"topdirs";"toploop"]
let dont_export = ref []
let add_export x = export:=!export@[x]
let add_dont_export x = dont_export:=!dont_export@[x]
let syntaxes = ref []
let syntaxes_mod = ref []
let add_syntax_pkg p =
  syntaxes:=!syntaxes @ [p]
let add_syntax_mod p =
  syntaxes_mod:=!syntaxes_mod @ [p]

let execute cmd =
  let s = String.concat " " cmd in
  if !Jsoo_common.verbose then Printf.printf "+ %s\n%!" s;
  let ret = Sys.command s in
  if ret <> 0
  then failwith (Printf.sprintf "Error: %s" s)

let to_clean = ref []
let clean file = to_clean := file :: !to_clean
let do_clean () =
  List.iter (fun x ->
      if Sys.file_exists x
      then Sys.remove x) !to_clean

module type CAMLP4 = sig
  val build
    :  all_pkgs:string list
    -> syntax_pkgs:string list
    -> syntax_mods:string list
    -> string list
end
module Camlp4 : CAMLP4 = struct

  (* We do the same as Camlp4Bin
     call Camlp4.Register.iter_and_take_callbacks
     after Every load of syntax module to initialize them.
     Camlp4 do it after dynlink;
     Since we statically link modules, we have to
     link dummy modules to trigger the callbacks *)
  let flush_str = Printf.sprintf
      "let _ = JsooTop.register_camlp4_syntax %S Camlp4.Register.iter_and_take_callbacks"

  let make_flush_module =
    let count = ref 0 in
    fun pkg_name ->
      incr count;
      let name = Printf.sprintf "camlp4_flush_%d" !count in
      let name_ml = name ^ ".ml" in
      let oc = open_out name_ml in
      clean name_ml;
      output_string oc (flush_str pkg_name);
      close_out oc;
      execute ["ocamlfind";"ocamlc";"-c";"-I";"+camlp4";"-package";"js_of_ocaml.toplevel";name_ml];
      clean (name ^ ".cmo");
      clean (name ^ ".cmi");
      (name^".cmo")

  let with_flush x archive =
    [ make_flush_module x ; archive]

  let resolve_syntaxes all =
    let preds = ["syntax";"preprocessor"] in
    let all = Findlib.package_deep_ancestors preds all in
    let ll = List.map (fun x ->
        try "-I" :: (Findlib.package_directory x) :: with_flush x (Findlib.package_property preds x "archive") with
        | Not_found -> []) all in
    List.flatten ll

  let camlp4_pre4_02 =
    try
      let v = Sys.ocaml_version in
      if v.[0] >= '4' &&
         v.[1] =  '.' &&
         v.[2] =  '0' &&
         v.[3] >= '2'
      then false
      else true
    with _ -> true
  let build ~all_pkgs ~syntax_pkgs ~syntax_mods =
    if syntax_pkgs = [] && syntax_mods = [] then [] else
      let maybe_add_dynlink =
        if List.mem "dynlink" all_pkgs
        then []
        else ["dynlink.cma"]
      in
      let all = resolve_syntaxes syntax_pkgs @ syntax_mods in
      maybe_add_dynlink
      @ [
        "-I";"+camlp4";
        "camlp4lib.cma";
	      "Camlp4Parsers/Camlp4OCamlRevisedParser.cmo";
	      "Camlp4Parsers/Camlp4OCamlParser.cmo";
      ]
      @ all
      @ [if camlp4_pre4_02 then "Camlp4Top/Top.cmo" else "jsooTopCamlp4.cmo"]
end

let usage () =
  Format.eprintf "Usage: jsoo_mktop [options] [ocamlfind arguments] @.";
  Format.eprintf " -verbose\t\t\tOutput intermediate commands@.";
  Format.eprintf " -help\t\t\t\tDisplay usage@.";
  Format.eprintf " -top-syntax [pkg]\t\tInclude syntax extension provided by [pkg] findlib package@.";
  Format.eprintf " -top-syntax-mod [mod]\t\tInclude syntax extension provided by the module [mod]@.";
  Format.eprintf " -o [name]\t\t\tSet output filename@.";
  Format.eprintf " -jsopt [opt]\t\t\tPass [opt] option to js_of_ocaml compiler@.";
  Format.eprintf " -export-package [pkg]\t\tCompile toplevel with [pkg] package loaded@.";
  Format.eprintf " -export-unit [unit]\t\tExport [unit]@.";
  Format.eprintf " -dont-export-unit [unit]\tDon't export [unit]@.";
  exit 1


let output = ref "./a.out"

let rec scan_args acc = function
  | "-top-syntax" :: x :: xs -> add_syntax_pkg x; scan_args acc xs
  | "-top-syntax-mod" :: x :: xs -> add_syntax_mod x; scan_args acc xs
  | ("--verbose"|"-verbose")::xs -> Jsoo_common.verbose:=true; scan_args ("-verbose"::acc) xs
  | ("--help"|"-help"|"-h")::_ -> usage ()
  | "-jsopt" :: x :: xs -> add_js_opt x; scan_args acc xs
  | "-o" :: x :: xs -> output:=x; scan_args acc xs
  | "-export-package" :: x :: xs -> add_pkgs x; scan_args (x :: "-package" :: acc) xs
  | "-export-unit" :: x :: xs -> add_export x; scan_args acc xs
  | "-dont-export-unit" :: x :: xs -> add_dont_export x; scan_args acc xs
  | x :: xs -> scan_args (x::acc) xs
  | [] -> List.rev acc

let _ =
  try
    let jsoo_top = ["-package";"js_of_ocaml.toplevel"] in
    let base_cmd = ["ocamlfind";"ocamlc";"-linkall";"-linkpkg"] in
    let args = List.tl (Array.to_list (Sys.argv)) in
    let args = scan_args [] args in
    let cmis = Jsoo_common.cmis_of_packages !pkgs in
    List.iter (fun pkg ->
      execute ["jsoo_mkcmis";pkg] ) !pkgs;
    let modules = List.map (fun x -> Filename.(chop_extension (basename x))) cmis in
    let modules = modules @ !export in
    let modules = List.filter (fun u -> not (List.mem u !dont_export)) modules in
    begin match modules with
      | [] -> assert false
      | modules ->
        let tmp_output = !output ^ ".tmp" in
        let cmd =
          base_cmd
          @ Camlp4.build
            ~all_pkgs:!pkgs
            ~syntax_pkgs:!syntaxes
            ~syntax_mods:!syntaxes_mod
          @ jsoo_top @ args @ ["-o"; tmp_output] in
        execute cmd;
        execute (["ocamlfind";"stdlib/expunge";tmp_output;!output] @ modules);
        clean (tmp_output)
    end;
    let extra_include = List.map (fun x -> ["-I";Findlib.package_directory x]) ("compiler-libs" :: !pkgs) in
    let extra_cmis = List.map (fun u -> ["--file";Printf.sprintf "%s.cmi:/cmis/" u]) !export in
    let extra = List.flatten (extra_include @ extra_cmis) in
    execute (["js_of_ocaml";"--toplevel";"--no-cmis";] @ extra @ !js_opt @ [!output]);
    do_clean ()
  with exn -> do_clean (); raise exn
