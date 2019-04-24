(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2019 Ty Overby
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
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

open Js_of_ocaml_compiler

module Format : Format_intf.S

val parse_js : Format.js_file -> Javascript.program

val compile_ocaml_to_cmo : Format.ocaml_file -> Format.cmo_file

val compile_ocaml_to_bc : Format.ocaml_file -> Format.bc_file

val compile_cmo_to_javascript :
     ?pretty:bool
  -> ?sourcemap:bool
  -> Format.cmo_file
  -> Format.js_file * Format.sourcemap_file option

val compile_bc_to_javascript :
     ?pretty:bool
  -> ?sourcemap:bool
  -> Format.bc_file
  -> Format.js_file * Format.sourcemap_file option

val run_javascript : Format.js_file -> string

type find_result =
  { expressions : Javascript.expression list
  ; statements : Javascript.statement list
  ; var_decls : Javascript.variable_declaration list }

val find_javascript :
     ?expression:(Javascript.expression -> bool)
  -> ?statement:(Javascript.statement -> bool)
  -> ?var_decl:(Javascript.variable_declaration -> bool)
  -> Javascript.program
  -> find_result

val expression_to_string : ?compact:bool -> Javascript.expression -> string

val print_var_decl : Javascript.program -> string -> unit
