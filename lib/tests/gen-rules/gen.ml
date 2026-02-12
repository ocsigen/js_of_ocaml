(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
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

open StdLabels

let is_implem x =
  if String.equal (Filename.extension x) ".ml"
  then
    let fname = Filename.chop_extension x in
    try
      String.iter fname ~f:(function
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> ()
        | _ -> raise Exit);
      true
    with Exit -> false
  else false

let () = set_binary_mode_out stdout true

let ends_with ~suffix s =
  let open String in
  let len_s = length s and len_suf = length suffix in
  let diff = len_s - len_suf in
  let rec aux i =
    if i = len_suf
    then true
    else if unsafe_get s (diff + i) <> unsafe_get suffix i
    then false
    else aux (i + 1)
  in
  diff >= 0 && aux 0

let prefix : string =
  let rec loop acc rem =
    let basename = Filename.basename rem in
    let dirname = Filename.dirname rem in
    if
      String.equal dirname rem
      || ends_with ~suffix:"_build" dirname
      || Sys.file_exists (Filename.concat rem "dune-project")
    then acc
    else
      let acc = Filename.concat basename acc in
      loop acc dirname
  in
  loop "" (Sys.getcwd ())
  (* normalizatio for windows *)
  |> String.map ~f:(function
    | '\\' -> '/'
    | c -> c)

type enabled_if =
  | GE5
  | Not_wasm
  | No_effects_not_wasm
  | Any

let enabled_if = function
  | "test_sys" -> GE5
  | "test_fun_call" -> No_effects_not_wasm
  | "test_poly_compare" -> Not_wasm
  | _ -> Any

let () =
  Array.to_list (Sys.readdir ".")
  |> List.filter ~f:is_implem
  |> List.sort ~cmp:compare
  |> List.iter ~f:(fun f ->
      let basename = Filename.chop_extension f in
      Printf.printf
        {|
(library
 ;; %s%s.ml
 (name %s_%d)
 (enabled_if %s)
 (modules %s)
 (libraries js_of_ocaml unix)
 (inline_tests (modes js%s))
 (preprocess
  (pps ppx_js_internal ppx_expect)))
|}
        prefix
        basename
        basename
        (Hashtbl.hash prefix mod 100)
        (match enabled_if basename with
        | Any -> "(<> %{profile} wasi)"
        | Not_wasm -> "true"
        | GE5 -> "(>= %{ocaml_version} 5)"
        | No_effects_not_wasm -> "(<> %{profile} with-effects)")
        basename
        (match enabled_if basename with
        | Any -> " wasm"
        | GE5 -> "" (* ZZZ /static not yet implemented *)
        | Not_wasm | No_effects_not_wasm -> ""))
