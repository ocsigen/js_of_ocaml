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

open Js_of_ocaml_compiler

type t =
  { common : Jsoo_cmdline.Arg.t
  ; (* compile option *)
    profile : Driver.profile option
  ; runtime_files : string list
  ; runtime_only : bool
  ; output_file : string * bool
  ; input_file : string option
  ; enable_source_maps : bool
  ; sourcemap_root : string option
  ; sourcemap_don't_inline_content : bool
  ; params : (string * string) list
  ; include_dirs : string list
  ; effects : Config.effects_backend
  }

val options : unit -> t Cmdliner.Term.t

val options_runtime_only : unit -> t Cmdliner.Term.t
