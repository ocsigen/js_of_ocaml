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

open Stdlib

let ocaml_version =
  Scanf.sscanf Sys.ocaml_version "%d.%d.%d" (fun major minor patchlevel ->
      Wax.Define.Version (major, minor, patchlevel))

let is_oxcaml = false [@@if not oxcaml]

let is_oxcaml = true [@@if oxcaml]

let value_to_define (v : Wat_preprocess.value) : Wax.Define.value =
  match v with
  | Wat_preprocess.Bool b -> Wax.Define.Bool b
  | Wat_preprocess.String s -> Wax.Define.String s
  | Wat_preprocess.Version (a, b, c) -> Wax.Define.Version (a, b, c)

let with_preprocessed_files ~variables ~inputs action =
  (* [name-wasm-functions] (default true) toggles the export-to-id naming pass
     rather than acting as a conditional-compilation variable. *)
  let name_functions =
    match
      List.find_map variables ~f:(fun (k, v) ->
          if String.equal k "name-wasm-functions" then Some v else None)
    with
    | Some (Wat_preprocess.Bool b) -> b
    | Some _ | None -> true
  in
  let defines =
    Wax.Define.of_list
      (("ocaml_version", ocaml_version)
      :: ("oxcaml", Wax.Define.Bool is_oxcaml)
      :: List.filter_map variables ~f:(fun (k, v) ->
          if String.equal k "name-wasm-functions"
          then None
          else Some (k, value_to_define v)))
  in
  let assemble ~filename contents =
    if Filename.check_suffix filename ".wax"
    then
      (* Wax functions are always named, so no naming pass is needed. *)
      Wax.wax_to_binary ~defines ~filename contents
    else Wax.wat_to_binary ~defines ~name_functions ~filename contents
  in
  List.fold_left
    ~f:(fun cont { Wat_preprocess.module_name; file; source } inputs ->
      match
        match source with
        | Wat_preprocess.Binary -> None
        | Wat_preprocess.File ->
            if Link.Wasm_binary.check_file ~file then None else Some (Fs.read_file file)
        | Wat_preprocess.Contents contents -> Some contents
      with
      | None -> cont ({ Binaryen.module_name; file; source_map_file = None } :: inputs)
      | Some contents ->
          let source_file = file in
          Fs.with_intermediate_file (Filename.temp_file module_name ".wasm")
          @@ fun file ->
          (if Link.Wasm_binary.check ~contents
           then Fs.write_file ~name:file ~contents
           else
             let binary = assemble ~filename:source_file contents in
             let oc = open_out_bin file in
             Fun.protect
               ~finally:(fun () -> close_out oc)
               (fun () -> Wax.output_binary ~out_channel:oc binary));
          cont ({ Binaryen.module_name; file; source_map_file = None } :: inputs))
    ~init:action
    inputs
    []
