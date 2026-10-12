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

let set_variables { enable; disable; set } =
  List.map ~f:(fun nm -> nm, Wax_link.Bool true) enable
  @ List.map ~f:(fun nm -> nm, Wax_link.Bool false) disable
  @ List.map ~f:(fun (nm, v) -> nm, Wax_link.String v) set
