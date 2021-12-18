(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2019 Hugo Heuzard
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

type options =
  { files : string list
  ; output_file : string
  ; include_dirs : string list
  }

let options =
  let files =
    let doc = "Embed [$(docv)] in the js_of_ocaml pseudo filesystem." in
    Arg.(value & pos_all string [] & info [] ~docv:"FILES" ~doc)
  in
  let output_file =
    let doc = "Set output file name to [$(docv)]." in
    Arg.(required & opt (some string) None & info [ "o" ] ~docv:"FILE" ~doc)
  in
  let include_dirs =
    let doc = "Add [$(docv)] to the list of include directories." in
    Arg.(value & opt_all string [] & info [ "I" ] ~docv:"DIR" ~doc)
  in
  Term.(
    pure (fun files output_file include_dirs -> { files; output_file; include_dirs })
    $ files
    $ output_file
    $ include_dirs)

let info =
  let doc = "Js_of_ocaml pseudo filesystem utility" in
  let man =
    [ `S "DESCRIPTION"
    ; `P "jsoo_fs is a tool for embeding files in a Js_of_ocaml pseudo filesystem."
    ; `S "BUGS"
    ; `P
        "Bugs are tracked on github at \
         $(i,https://github.com/ocsigen/js_of_ocaml/issues)."
    ; `S "AUTHORS"
    ; `P "Jerome Vouillon, Hugo Heuzard."
    ; `S "LICENSE"
    ; `P "Copyright (C) 2010-2019."
    ; `P
        "jsoo_fs is free software, you can redistribute it and/or modify it under the \
         terms of the GNU Lesser General Public License as published by the Free \
         Software Foundation, with linking exception; either version 2.1 of the License, \
         or (at your option) any later version."
    ]
  in
  let version =
    match Compiler_version.git_version with
    | "" -> Compiler_version.s
    | v -> Printf.sprintf "%s+git-%s" Compiler_version.s v
  in
  Term.info "jsoo_fs" ~doc ~man ~version

let f { files; output_file; include_dirs } =
  let code =
    {|
//Provides: jsoo_create_file_extern
function jsoo_create_file_extern(name,content){
  if(joo_global_object.caml_create_file)
    joo_global_object.caml_create_file(name,content);
  else {
    if(!joo_global_object.caml_fs_tmp) joo_global_object.caml_fs_tmp = [];
    joo_global_object.caml_fs_tmp.push({name:name,content:content});
  }
  return 0;
}
|}
  in
  let fragments = Linker.parse_string code in
  Linker.load_fragments ~target_env:Isomorphic ~filename:"<dummy>" fragments;
  let instr =
    Pseudo_fs.f ~prim:`create_file_extern ~cmis:StringSet.empty ~files ~paths:include_dirs
  in
  let code = Code.prepend Code.empty instr in
  Filename.gen_file output_file (fun chan ->
      let pfs_fmt = Pretty_print.to_out_channel chan in
      Driver.f
        ~standalone:true
        ~global:`globalThis
        pfs_fmt
        (Parse_bytecode.Debug.create ~toplevel:false false)
        code)

let main = Cmdliner.Term.(pure f $ options), info

let _ =
  Timer.init Sys.time;
  try
    Cmdliner.Term.eval
      ~catch:false
      ~argv:(Jsoo_cmdline.normalize_argv ~warn:(warn "%s") Sys.argv)
      main
  with
  | (Match_failure _ | Assert_failure _ | Not_found) as exc ->
      let backtrace = Printexc.get_backtrace () in
      Format.eprintf
        "%s: You found a bug. Please report it at \
         https://github.com/ocsigen/js_of_ocaml/issues :@."
        Sys.argv.(0);
      Format.eprintf "Error: %s@." (Printexc.to_string exc);
      prerr_string backtrace;
      exit 1
  | Failure s ->
      Format.eprintf "%s: Error: %s@." Sys.argv.(0) s;
      exit 1
  | exc ->
      let backtrace = Printexc.get_backtrace () in
      Format.eprintf "%s: Error: %s@." Sys.argv.(0) (Printexc.to_string exc);
      prerr_string backtrace;
      exit 1
