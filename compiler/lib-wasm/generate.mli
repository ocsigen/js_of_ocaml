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

val init : unit -> unit

val start : unit -> Code_generation.context

val f :
     context:Code_generation.context
  -> unit_name:string option
  -> Code.program
  -> live_vars:int array
  -> in_cps:Effects.in_cps
  -> deadcode_sentinal:Code.Var.t
  -> Wasm_ast.var * (string list * (string * Javascript.expression) list)

val add_start_function : context:Code_generation.context -> Wasm_ast.var -> unit

val add_init_function : context:Code_generation.context -> to_link:string list -> unit

val output : out_channel -> context:Code_generation.context -> unit

val wasm_output :
     out_channel
  -> opt_source_map_file:string option
  -> context:Code_generation.context
  -> unit
