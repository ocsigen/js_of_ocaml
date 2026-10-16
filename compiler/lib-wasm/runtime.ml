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

(* Our linker only reads binary files: the files in text format are
   converted with [wasm-opt], without running any optimization pass
   ([--quiet] silences the warning about this). With [all], the files
   in binary format are converted as well. *)
let with_binary_files ?(all = false) ~options inputs f =
  let rec loop acc (l : Binaryen.link_input list) =
    match l with
    | [] -> f (List.rev acc)
    | input :: rem when (not all) && Link.Wasm_binary.check_file ~file:input.file ->
        loop (input :: acc) rem
    | input :: rem ->
        Fs.with_intermediate_file (Filename.temp_file input.module_name ".wasm")
        @@ fun file ->
        let with_source_map_file f =
          match input.source_map_file with
          | None -> f None
          | Some _ ->
              Fs.with_intermediate_file
                (Filename.temp_file input.module_name ".wasm.map")
                (fun file -> f (Some file))
        in
        with_source_map_file
        @@ fun source_map_file ->
        Binaryen.optimize
          ~profile:Profile.O1
          ~options:("--quiet" :: options)
          ~opt_input_sourcemap:input.source_map_file
          ~opt_output_sourcemap:source_map_file
          ~input_file:input.file
          ~output_file:file
          ();
        loop ({ input with file; source_map_file } :: acc) rem
  in
  loop [] inputs

let link ~inputs ~output_file =
  let (_ : Wasm_link.output) =
    Wasm_link.f
      (List.map
         ~f:(fun { Binaryen.module_name; file; _ } ->
           { Wasm_link.module_name; file; code = None; opt_source_map = None })
         inputs)
      ~output_file
  in
  ()

let build ~allowed_imports ~convert_options ~opt_options ~variables ~inputs ~output_file =
  Fs.with_intermediate_file (Filename.temp_file "runtime-merged" ".wasm")
  @@ fun merge_file ->
  (Wat_preprocess.with_preprocessed_files ~variables ~inputs
  @@ fun inputs ->
  with_binary_files ~options:convert_options inputs
  @@ fun inputs -> link ~inputs ~output_file:merge_file);
  Binaryen.optimize
    ~profile:Profile.O1
    ~options:opt_options
    ~opt_input_sourcemap:None
    ~input_file:merge_file
    ~opt_output_sourcemap:None
    ~output_file
    ();
  let imports = Link.Wasm_binary.read_imports ~file:output_file in
  Option.iter allowed_imports ~f:(fun allowed_imports ->
      let missing_imports =
        List.filter
          ~f:(fun { Link.Wasm_binary.module_; _ } ->
            not (List.mem ~eq:String.equal module_ allowed_imports))
          imports
      in
      if not (List.is_empty missing_imports)
      then (
        Format.eprintf "The runtime contains unknown imports:@.";
        List.iter
          ~f:(fun { Link.Wasm_binary.module_; name; _ } ->
            Format.eprintf "  %s %s@." module_ name)
          missing_imports;
        exit 2))
