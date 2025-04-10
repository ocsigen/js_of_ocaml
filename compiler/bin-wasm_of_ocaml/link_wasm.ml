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

type binaryen_options =
  { common : string list
  ; opt : string list
  ; merge : string list
  }

type options =
  { input_modules : (string * string) list
  ; output_file : string
  ; variables : Preprocess.variables
  ; allowed_imports : string list option
  ; binaryen_options : binaryen_options
  }

let options =
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
  let allowed_imports =
    let doc = "List of modules we expect to import from." in
    Arg.(
      value
      & opt_all (list ~sep:',' string) []
      & info [ "allowed-imports" ] ~docv:"IMPORT" ~doc)
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
  let build_t input_modules output_file variables allowed_imports common opt merge =
    let allowed_imports =
      if List.is_empty allowed_imports then None else Some (List.concat allowed_imports)
    in
    `Ok
      { input_modules
      ; output_file
      ; variables
      ; allowed_imports
      ; binaryen_options = { common; opt; merge }
      }
  in
  let t =
    Term.(
      const build_t
      $ input_modules
      $ output_file
      $ Preprocess.variable_options
      $ allowed_imports
      $ binaryen_options
      $ opt_options
      $ merge_options)
  in
  Term.ret t

let link
    { input_modules
    ; output_file
    ; variables
    ; allowed_imports
    ; binaryen_options = { common; merge; opt }
    } =
  let inputs =
    List.map
      ~f:(fun (module_name, file) -> { Wat_preprocess.module_name; file; source = File })
      input_modules
  in
  Runtime.build
    ~allowed_imports
    ~link_options:(common @ merge)
    ~opt_options:(common @ opt)
    ~variables:(Preprocess.set_variables variables)
    ~inputs
    ~output_file

let info =
  Info.make
    ~name:"link-wasm"
    ~doc:"Wasm linker"
    ~description:
      "$(b,wasmoo_link_wasm) is a Wasm linker. It takes as input a list of Wasm text \
       files, preprocesses them, links them together, and outputs a single Wasm binary \
       module"

let term = Cmdliner.Term.(const link $ options)

let command = Cmdliner.Cmd.v info term
