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

open Cmdliner
open Js_of_ocaml_compiler.Stdlib
open Wasm_of_ocaml_compiler

let () = Sys.catch_break true

type variables =
  { enable : string list
  ; disable : string list
  ; set : (string * string) list
  }

type options =
  { input_file : string option
  ; output_file : string option
  ; variables : variables
  }

let variable_options =
  let enable =
    let doc = "Set preprocessor variable $(docv) to true." in
    let arg =
      Arg.(value & opt_all (list string) [] & info [ "enable" ] ~docv:"VAR" ~doc)
    in
    Term.(const List.flatten $ arg)
  in
  let disable =
    let doc = "Set preprocessor variable $(docv) to false." in
    let arg =
      Arg.(value & opt_all (list string) [] & info [ "disable" ] ~docv:"VAR" ~doc)
    in
    Term.(const List.flatten $ arg)
  in
  let set =
    let doc = "Set preprocessor variable $(i,VAR) to value $(i,VALUE)." in
    let arg =
      Arg.(
        value
        & opt_all (list (pair ~sep:'=' string string)) []
        & info [ "set" ] ~docv:"VAR=VALUE" ~doc)
    in
    Term.(const List.flatten $ arg)
  in
  let build_t enable disable set = { enable; disable; set } in
  Term.(const build_t $ enable $ disable $ set)

let options =
  let input_file =
    let doc =
      "Use the Wasm text file $(docv) as input (default to the standard input)."
    in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"INPUT_FILE" ~doc)
  in
  let output_file =
    let doc = "Specify the output file $(docv) (default to the standard output)." in
    Arg.(value & opt (some string) None & info [ "o" ] ~docv:"OUTPUT_FILE" ~doc)
  in
  let build_t input_file output_file variables =
    `Ok { input_file; output_file; variables }
  in
  let t = Term.(const build_t $ input_file $ output_file $ variable_options) in
  Term.ret t

let set_variables { enable; disable; set } =
  List.map ~f:(fun nm -> nm, Wat_preprocess.Bool true) enable
  @ List.map ~f:(fun nm -> nm, Wat_preprocess.Bool false) disable
  @ List.map ~f:(fun (nm, v) -> nm, Wat_preprocess.String v) set

let preprocess { input_file; output_file; variables } =
  let with_input f =
    match input_file with
    | None -> f stdin
    | Some file ->
        let ch = open_in_text file in
        let res = f ch in
        close_in ch;
        res
  in
  let with_output f =
    match output_file with
    | Some "-" | None -> f stdout
    | Some file -> Filename.gen_file file f
  in
  let contents = with_input In_channel.input_all in
  let res =
    Wat_preprocess.f
      ~filename:(Option.value ~default:"-" input_file)
      ~contents
      ~variables:(set_variables variables)
  in
  with_output (fun ch -> output_string ch res)

let term = Cmdliner.Term.(const preprocess $ options)

let info =
  Info.make
    ~name:"preprocess"
    ~doc:"Wasm text file preprocessor"
    ~description:"$(b,wasmoo_util pp) is a Wasm text file preprocessor."

let command = Cmdliner.Cmd.v info term

(* Adapted from
   https://github.com/ocaml/opam/blob/fbbe93c3f67034da62d28c8666ec6b05e0a9b17c/s
rc/client/opamArg.ml#L759 *)
let alias_command ?orig_name cmd term name =
  let orig =
    match orig_name with
    | Some s -> s
    | None -> Cmd.name cmd
  in
  let doc = Printf.sprintf "An alias for $(b,%s)." orig in
  let man =
    [ `S "DESCRIPTION"
    ; `P (Printf.sprintf "$(mname)$(b, %s) is an alias for $(mname)$(b, %s)." name orig)
    ; `P (Printf.sprintf "See $(mname)$(b, %s --help) for details." orig)
    ]
  in
  Cmd.v (Cmd.info name ~docs:"COMMAND ALIASES" ~doc ~man) term

let command_alias = alias_command ~orig_name:"preprocess" command term "pp"
