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

(* Project-relative path to this directory, passed by dune *)
let prefix : string =
  if Array.length Sys.argv < 2
  then failwith "gen.exe: expected source directory as first argument";
  let p = Sys.argv.(1) in
  if String.length p > 0 && not (Char.equal p.[String.length p - 1] '/')
  then p ^ "/"
  else p

type enabled_if =
  | GE5
  | No_effects
  | Any

let enabled_if = function
  | "test_sys" -> GE5
  | "test_fun_call" -> No_effects
  | _ -> Any

let run_wasm = function
  | "test_fun_call" -> false
  | "test_poly_compare" -> false
  | "test_sys" -> false (* ZZZ /static not yet implemented *)
  | _ -> true

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
 (name %s_%d)%s
 (modules %s)
 (libraries js_of_ocaml unix)
 (inline_tests (modes %s))
 (preprocess
  (pps ppx_js_internal ppx_expect)))
|}
        prefix
        basename
        basename
        (Hashtbl.hash prefix mod 100)
        (match enabled_if basename with
        | Any -> ""
        | GE5 -> "\n (enabled_if (>= %{ocaml_version} 5))"
        | No_effects -> "\n (enabled_if (<> %{profile} with-effects))")
        basename
        (match run_wasm basename with
        | true -> "js wasm"
        | false -> "js"))
