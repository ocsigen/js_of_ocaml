(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2013 Hugo Heuzard
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

open Js_of_ocaml_compiler.Stdlib
open Wasm_of_ocaml_compiler

let () = Sys.catch_break true

let read_contents ch =
  let buf = Buffer.create 65536 in
  let b = Bytes.create 65536 in
  let rec read () =
    let n = input ch b 0 (Bytes.length b) in
    if n > 0
    then (
      Buffer.add_subbytes buf b 0 n;
      read ())
  in
  read ();
  Buffer.contents buf

let set_variables { Cmd_arg.enable; disable; set } =
  List.map ~f:(fun nm -> nm, Wat_preprocess.Bool true) enable
  @ List.map ~f:(fun nm -> nm, Wat_preprocess.Bool false) disable
  @ List.map ~f:(fun (nm, v) -> nm, Wat_preprocess.String v) set

let preprocess { Cmd_arg.input_file; output_file; variables } =
  let with_input f =
    match input_file with
    | None -> f stdin
    | Some file ->
        let ch = open_in file in
        let res = f ch in
        close_in ch;
        res
  in
  let with_output f =
    match output_file with
    | Some "-" | None -> f stdout
    | Some file -> Filename.gen_file file f
  in
  let contents = with_input read_contents in
  let res =
    Wat_preprocess.f
      ~filename:(Option.value ~default:"-" input_file)
      ~contents
      ~variables:(set_variables variables)
  in
  with_output (fun ch -> output_string ch res)

let preprocess_term = Cmdliner.Term.(const preprocess $ Cmd_arg.preprocess_options)

let preprocess_command = Cmdliner.Cmd.v Cmd_arg.preprocess_info preprocess_term

let link
    { Cmd_arg.input_modules
    ; output_file
    ; variables
    ; binaryen_options = { common; merge; opt }
    } =
  let inputs =
    List.map
      ~f:(fun (module_name, file) ->
        { Wat_preprocess.module_name
        ; file
        ; source =
            (if Link.Wasm_binary.check_file ~file
             then File
             else Contents (Js_of_ocaml_compiler.Fs.read_file file))
        })
      input_modules
  in
  Runtime.build
    ~link_options:(common @ merge)
    ~opt_options:(common @ opt)
    ~variables:(set_variables variables)
    ~inputs
    ~output_file

let link_term = Cmdliner.Term.(const link $ Cmd_arg.link_options)

let link_command = Cmdliner.Cmd.v Cmd_arg.link_info link_term

let (_ : int) =
  try
    Cmdliner.Cmd.eval
      ~catch:false
      ~argv:(Jsoo_cmdline.normalize_argv ~warn:(warn "%s") Sys.argv)
      (Cmdliner.Cmd.group Cmd_arg.info [ preprocess_command; link_command ])
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
      Format.eprintf "%s: Error: %s@." Sys.argv.(0) (Printexc.to_string exc);
      exit 1
