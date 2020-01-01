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

module Filetype : Filetype_intf.S

val parse_js : Filetype.js_file -> Javascript.program

val compile_ocaml_to_cmo : ?debug:bool -> Filetype.ocaml_file -> Filetype.cmo_file

val compile_ocaml_to_bc : Filetype.ocaml_file -> Filetype.bc_file

val compile_lib : Filetype.cmo_file list -> string -> Filetype.cmo_file

val compile_cmo_to_javascript :
     ?flags:string list
  -> ?pretty:bool
  -> ?sourcemap:bool
  -> Filetype.cmo_file
  -> Filetype.js_file * Filetype.sourcemap_file option

val compile_bc_to_javascript :
     ?flags:string list
  -> ?pretty:bool
  -> ?sourcemap:bool
  -> Filetype.bc_file
  -> Filetype.js_file * Filetype.sourcemap_file option

val run_javascript : Filetype.js_file -> string

val expression_to_string : ?compact:bool -> Javascript.expression -> string

val print_var_decl : Javascript.program -> string -> unit

val print_fun_decl : Javascript.program -> string option -> unit

val compile_and_run : ?flags:string list -> string -> unit
