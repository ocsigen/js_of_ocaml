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

open Cmdliner

type variables =
  { enable : string list
  ; disable : string list
  ; set : (string * string) list
  }

type preprocess_options =
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

let preprocess_options =
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

type binaryen_options =
  { common : string list
  ; opt : string list
  ; merge : string list
  }

type link_options =
  { input_modules : (string * string) list
  ; output_file : string
  ; variables : variables
  ; binaryen_options : binaryen_options
  }

let link_options =
  let input_modules =
    let doc =
      "Specify an input module with name $(i,NAME) in Wasm text file $(i,FILE)."
    in
    Arg.(
      value
      & pos_right 0 (pair ~sep:':' string string) []
      & info [] ~docv:"NAME:FILE" ~doc)
  in
  let output_file =
    let doc = "Specify the Wasm binary output file $(docv)." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"WASM_FILE" ~doc)
  in
  let binaryen_options =
    let doc = "Pass option $(docv) to binaryen tools" in
    Arg.(value & opt_all string [] & info [ "binaryen" ] ~docv:"OPT" ~doc)
  in
  let opt_options =
    let doc = "Pass option $(docv) to $(b,wasm-opt)" in
    Arg.(value & opt_all string [] & info [ "binaryen-opt" ] ~docv:"OPT" ~doc)
  in
  let merge_options =
    let doc = "Pass option $(docv) to $(b,wasm-merge)" in
    Arg.(value & opt_all string [] & info [ "binaryen-merge" ] ~docv:"OPT" ~doc)
  in
  let build_t input_modules output_file variables common opt merge =
    `Ok
      { input_modules; output_file; variables; binaryen_options = { common; opt; merge } }
  in
  let t =
    Term.(
      const build_t
      $ input_modules
      $ output_file
      $ variable_options
      $ binaryen_options
      $ opt_options
      $ merge_options)
  in
  Term.ret t

let make_info ~name ~doc ~description =
  let man =
    [ `S "DESCRIPTION"
    ; `P description
    ; `S "BUGS"
    ; `P
        "Bugs are tracked on github at \
         $(i,https://github.com/ocsigen/js_of_ocaml/issues)."
    ; `S "SEE ALSO"
    ; `P "wasm_of_ocaml(1)"
    ; `S "AUTHORS"
    ; `P "Jerome Vouillon, Hugo Heuzard."
    ; `S "LICENSE"
    ; `P "Copyright (C) 2010-2025."
    ; `P
        "wasmoo_util is free software, you can redistribute it and/or modify it under \
         the terms of the GNU Lesser General Public License as published by the Free \
         Software Foundation, with linking exception; either version 2.1 of the License, \
         or (at your option) any later version."
    ]
  in
  let version =
    match Js_of_ocaml_compiler.Compiler_version.git_version with
    | "" -> Js_of_ocaml_compiler.Compiler_version.s
    | v -> Printf.sprintf "%s+%s" Js_of_ocaml_compiler.Compiler_version.s v
  in
  Cmd.info name ~version ~doc ~man

let preprocess_info =
  make_info
    ~name:"pp"
    ~doc:"Wasm text file preprocessor"
    ~description:"$(b,wasmoo_util pp) is a Wasm text file preprocessor."

let link_info =
  make_info
    ~name:"link"
    ~doc:"Wasm linker"
    ~description:
      "$(b,wasmoo_util link) is a Wasm linker. It takes as input a list of Wasm text \
       files, preprocesses them, links them together, and outputs a single Wasm binary \
       module"

let info =
  make_info
    ~name:"wasmoo_util"
    ~doc:"Wasm utilities"
    ~description:"wasmoo_util is a collection of utilities for $(b,wasm_of_ocaml)"
