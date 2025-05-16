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

val with_temp_dir : f:(unit -> 'a) -> 'a

val parse_js : Filetype.js_file -> Javascript.program

val compile_ocaml_to_cmo : ?debug:bool -> Filetype.ocaml_file -> Filetype.cmo_file

val compile_ocaml_to_bc :
  ?debug:bool -> ?unix:bool -> Filetype.ocaml_file -> Filetype.bc_file

val compile_lib : Filetype.cmo_file list -> string -> Filetype.cmo_file

val compile_cmo_to_javascript :
     ?flags:string list
  -> ?effects:[ `Disabled | `Cps | `Double_translation ]
  -> ?use_js_string:bool
  -> ?pretty:bool
  -> ?sourcemap:bool
  -> ?werror:bool
  -> Filetype.cmo_file
  -> Filetype.js_file

val compile_bc_to_javascript :
     ?flags:string list
  -> ?effects:[ `Disabled | `Cps | `Double_translation ]
  -> ?use_js_string:bool
  -> ?pretty:bool
  -> ?sourcemap:bool
  -> ?werror:bool
  -> Filetype.bc_file
  -> Filetype.js_file

val jsoo_minify :
  ?flags:string list -> pretty:bool -> Filetype.js_file -> Filetype.js_file

val extract_sourcemap : Filetype.js_file -> Js_of_ocaml_compiler.Source_map.t option

val run_javascript : Filetype.js_file -> string

val check_javascript : Filetype.js_file -> string

val check_javascript_source : string -> string

val expression_to_string : ?compact:bool -> Javascript.expression -> string

val print_file : string -> unit

val print_program : Javascript.program -> unit

val print_var_decl : Javascript.program -> string -> unit

val print_fun_decl : Javascript.program -> string option -> unit

val find_variable : Javascript.program -> string -> Javascript.expression

val find_function : Javascript.program -> string -> Javascript.function_declaration

(* Prints the two versions of a doubly translated function *)
val print_double_fun_decl : Javascript.program -> string -> unit

val compile_and_run :
     ?debug:bool
  -> ?pretty:bool
  -> ?flags:string list
  -> ?effects:[ `Disabled | `Cps | `Double_translation ]
  -> ?use_js_string:bool
  -> ?unix:bool
  -> ?werror:bool
  -> string
  -> unit

val compile_and_run_bytecode : ?unix:bool -> string -> unit

val compile_and_parse :
     ?debug:bool
  -> ?pretty:bool
  -> ?flags:string list
  -> ?effects:[ `Disabled | `Cps | `Double_translation ]
  -> ?use_js_string:bool
  -> ?werror:bool
  -> string
  -> Javascript.program

val compile_and_parse_whole_program :
     ?debug:bool
  -> ?pretty:bool
  -> ?flags:string list
  -> ?effects:[ `Disabled | `Cps | `Double_translation ]
  -> ?use_js_string:bool
  -> ?unix:bool
  -> ?werror:bool
  -> string
  -> Javascript.program

val normalize_path : string -> string
