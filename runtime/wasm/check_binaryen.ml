(* Wasm_of_ocaml runtime support
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

(* Keep in sync with compiler/lib-wasm/binaryen.ml *)
let min_version = 120

let () =
  let tmp = Filename.temp_file "binaryen" ".version" in
  let exit_code = Sys.command (Printf.sprintf "wasm-opt --version > %s 2>&1" tmp) in
  let line =
    if exit_code = 0
    then (
      let ic = open_in tmp in
      let l = try Some (input_line ic) with End_of_file -> None in
      close_in ic;
      l)
    else None
  in
  Sys.remove tmp;
  match line with
  | Some line -> (
      (* An unusable wasm-opt is treated as missing, as the compiler does *)
      match Scanf.sscanf line "wasm-opt version %d" Fun.id with
      | version when version >= min_version -> print_endline "optimized"
      | version ->
          Printf.eprintf
            "Warning: wasm-opt version %d is too old (Binaryen >= %d is needed): the \
             runtime is not optimized.\n"
            version
            min_version;
          print_endline "unoptimized"
      | exception (Scanf.Scan_failure _ | Failure _ | End_of_file) ->
          Printf.eprintf
            "Warning: could not parse wasm-opt version from %S (Binaryen >= %d is \
             needed): the runtime is not optimized.\n"
            line
            min_version;
          print_endline "unoptimized")
  | None ->
      (* Binaryen is optional: without it, the runtime is not optimized. We
         do not warn, since this is run at each build; the compiler warns
         when linking a program instead. *)
      print_endline "unoptimized"
