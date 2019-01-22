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

(** One can add a ppx rewriter to a toplevel by registering it
    {[
      open Migrate_parsetree

      let init () =
        let module Converter =
          Migrate_parsetree.Versions.Convert
            (Migrate_parsetree.OCaml_405)
            (Migrate_parsetree.OCaml_current)
        in
        let mapper = Converter.copy_mapper Ppx_js.mapper in
        Compiler_libs.Ast_mapper.register "js_of_ocaml" (fun _ -> mapper)
    ]}
*)

(** Helpers to embed PPX into the toplevel. *)

val preprocess_structure : Parsetree.structure -> Parsetree.structure

val preprocess_signature : Parsetree.signature -> Parsetree.signature

val preprocess_phrase : Parsetree.toplevel_phrase -> Parsetree.toplevel_phrase
