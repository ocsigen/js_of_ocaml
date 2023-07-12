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

open! Js_of_ocaml_compiler.Stdlib
open Js_of_ocaml_compiler
open Cmdliner

type t =
  { common : Jsoo_cmdline.Arg.t
  ; (* compile option *)
    profile : Driver.profile option
  ; runtime_files : string list
  ; output_file : string * bool
  ; input_file : string
  ; params : (string * string) list
  }

let options =
  let runtime_files =
    let doc = "Link JavaScript and WebAssembly files [$(docv)]. " in
    Arg.(value & pos_left ~rev:true 0 string [] & info [] ~docv:"RUNTIME_FILES" ~doc)
  in
  let output_file =
    let doc = "Set output file name to [$(docv)]." in
    Arg.(value & opt (some string) None & info [ "o" ] ~docv:"FILE" ~doc)
  in
  let input_file =
    let doc = "Compile the bytecode program [$(docv)]. " in
    Arg.(required & pos ~rev:true 0 (some string) None & info [] ~docv:"PROGRAM" ~doc)
  in
  let profile =
    let doc = "Set optimization profile : [$(docv)]." in
    let profile = List.map Driver.profiles ~f:(fun (i, p) -> string_of_int i, p) in
    Arg.(value & opt (some (enum profile)) None & info [ "opt" ] ~docv:"NUM" ~doc)
  in
  let set_param =
    let doc = "Set compiler options." in
    let all = List.map (Config.Param.all ()) ~f:(fun (x, _) -> x, x) in
    Arg.(
      value
      & opt_all (list (pair ~sep:'=' (enum all) string)) []
      & info [ "set" ] ~docv:"PARAM=VALUE" ~doc)
  in
  let build_t common set_param profile output_file input_file runtime_files =
    let chop_extension s = try Filename.chop_extension s with Invalid_argument _ -> s in
    let output_file =
      match output_file with
      | Some s -> s, true
      | None -> chop_extension input_file ^ ".js", false
    in
    let params : (string * string) list = List.flatten set_param in
    `Ok { common; params; profile; output_file; input_file; runtime_files }
  in
  let t =
    Term.(
      const build_t
      $ Jsoo_cmdline.Arg.t
      $ set_param
      $ profile
      $ output_file
      $ input_file
      $ runtime_files)
  in
  Term.ret t
