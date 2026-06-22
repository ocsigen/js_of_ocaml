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

let prefix : string =
  let rec loop acc rem =
    let basename = Filename.basename rem in
    let dirname = Filename.dirname rem in
    if
      String.equal dirname rem
      || String.ends_with ~suffix:"_build" dirname
      || Sys.file_exists (Filename.concat rem "dune-project")
    then acc
    else
      let acc = basename :: acc in
      loop acc dirname
  in
  loop [ "" ] (Sys.getcwd ()) |> String.concat ~sep:"/"

type lang =
  | NEQ of lang * lang
  | Var of string
  | Atom of string
  | And of lang list

let profile = Var "profile"

let neq_profile v = NEQ (profile, Atom v)

let not_quickjs = neq_profile "quickjs"

let not_with_effects = neq_profile "with-effects"

(* These tests rely on features not yet available when targeting WASI. *)
let not_wasi = [ neq_profile "wasi"; neq_profile "wasi-with-native-effects" ]

let and_ = function
  | [] -> assert false
  | [ x ] -> x
  | l -> And l

let lib_enabled_if = function
  (* [test_sys] needs OCaml >= 5 ([In_channel.input_all]); that gate now lives
     in the source as a floating [@@@if ocaml_version >= (5, 0, 0)]. It is not
     wasi-gated (unlike the [_] default below). *)
  | "test_sys" -> []
  | "test_fun_call" -> [ not_with_effects ]
  (* [test_localtime] mutates process.env.TZ, which is Node-specific. *)
  | "test_localtime" -> not_quickjs :: not_wasi
  | "test_fetch" -> not_quickjs :: not_wasi
  | _ -> not_wasi

(* Some tests cannot run under wasm yet. *)
let run_wasm = function
  | "test_fun_call" -> false
  | "test_poly_compare" -> false
  | "test_sys" -> false (* ZZZ /static not yet implemented *)
  | "test_localtime" -> false (* relies on Node's process.env.TZ *)
  | _ -> true

let rec pp f = function
  | NEQ (a, b) -> Format.fprintf f "(<> %a %a)" pp a pp b
  | Var x -> Format.fprintf f "%%{%s}" x
  | Atom x -> Format.fprintf f "%s" x
  | And [] -> assert false
  | And l ->
      Format.fprintf
        f
        "(and %a)"
        (Format.pp_print_list ~pp_sep:(fun f () -> Format.fprintf f " ") pp)
        l

let enabled_if n fmt x =
  match x with
  | [] -> ()
  | l ->
      let x = and_ l in
      Format.fprintf fmt "\n%s" (String.make n ' ');
      Format.fprintf fmt "(enabled_if %a)" pp x

let modes basename = if run_wasm basename then "js wasm" else "js"

let () =
  Array.to_list (Sys.readdir ".")
  |> List.filter ~f:is_implem
  |> List.sort ~cmp:compare
  |> List.iter ~f:(fun f ->
      let basename = Filename.chop_extension f in
      Format.printf
        {|
(library
 ;; %s%s.ml
 (name %s_%d)%a
 (modules %s)
 (libraries js_of_ocaml unix)
 (inline_tests (modes %s))
 (preprocess
  (pps ppx_js_internal ppx_optcomp_light ppx_expect_light)))
|}
        prefix
        basename
        basename
        (Hashtbl.hash prefix mod 100)
        (enabled_if 1)
        (lib_enabled_if basename)
        basename
        (modes basename))
