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

(* Usage: link_driver OUTPUT NAME:FILE...

   Link the Wasm modules [FILE] (imported under module name [NAME]) into
   [OUTPUT]. If [FILE.map] exists, it is used as the source map of [FILE],
   and the source map of the output is written to [OUTPUT.map]. *)

open Js_of_ocaml_compiler.Stdlib
open Js_of_ocaml_compiler
open Wasm_of_ocaml_compiler

let () =
  match List.tl (Array.to_list Sys.argv) with
  | [] -> failwith "no output file"
  | output_file :: inputs ->
      let inputs =
        List.map
          ~f:(fun s ->
            match String.index_opt s ':' with
            | None -> failwith ("bad input " ^ s)
            | Some i ->
                let file = String.sub s ~pos:(i + 1) ~len:(String.length s - i - 1) in
                { Wasm_link.module_name = String.sub s ~pos:0 ~len:i
                ; file
                ; code = None
                ; opt_source_map =
                    (if Sys.file_exists (file ^ ".map")
                     then Some (Source_map.Standard.of_file (file ^ ".map"))
                     else None)
                })
          inputs
      in
      let source_map = Wasm_link.f inputs ~output_file in
      if List.exists ~f:(fun i -> Option.is_some i.Wasm_link.opt_source_map) inputs
      then Source_map.to_file ~rewrite_paths:false source_map (output_file ^ ".map")
