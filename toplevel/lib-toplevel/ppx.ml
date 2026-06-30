(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2015 OCamlPro
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

let ppx_rewriters = ref []

let () = Ast_mapper.register_function := fun _ f -> ppx_rewriters := f :: !ppx_rewriters

let preprocess_structure str =
  let open Ast_mapper in
  List.fold_right !ppx_rewriters ~init:str ~f:(fun ppx_rewriter str ->
      let mapper = ppx_rewriter [] in
      mapper.structure mapper str)

let preprocess_signature str =
  let open Ast_mapper in
  List.fold_right !ppx_rewriters ~init:str ~f:(fun ppx_rewriter str ->
      let mapper = ppx_rewriter [] in
      mapper.signature mapper str)

let preprocess_phrase phrase =
  let open Parsetree in
  match phrase with
  | Ptop_def str -> Ptop_def (preprocess_structure str)
  | Ptop_dir _ as x -> x
