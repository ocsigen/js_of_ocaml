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

(* Tests for runtime files written as ES modules (see Esm_runtime): exports
   provide primitives, imports are dependency edges, and dead code
   elimination keeps only what the used primitives need. *)

open Js_of_ocaml_compiler.Stdlib
open Js_of_ocaml_compiler

let test_dir = Filename.concat (Sys.getcwd ()) "_esm_runtime_test_files"

let ensure_dir dir = if not (Sys.file_exists dir) then Unix.mkdir dir 0o755

let write_file path content =
  let oc = open_out_bin path in
  output_string oc content;
  close_out oc

let with_test_dir f =
  ensure_dir test_dir;
  let files = ref [] in
  let write name content =
    let path = Filename.concat test_dir name in
    write_file path content;
    files := path :: !files;
    path
  in
  Fun.protect
    ~finally:(fun () ->
      List.iter ~f:Sys.remove !files;
      Unix.rmdir test_dir)
    (fun () -> f ~write)

let program_to_string program =
  let buffer = Buffer.create 4096 in
  let pp = Pretty_print.to_buffer buffer in
  Pretty_print.set_compact pp false;
  Config.Flag.disable "debuginfo";
  Config.Flag.disable "shortvar";
  let program = Js_assign.program program in
  let _ = Js_output.program pp program in
  Buffer.contents buffer

(* Load runtime files and link the code needed for [used] primitives *)
let link_for ~used files =
  Linker.reset ();
  Linker.load_files ~target_env:Target_env.Isomorphic files;
  let state = Linker.init () in
  let state, missing = Linker.resolve_deps ~check_missing:false state used in
  let output = Linker.link ~check_missing:false [] state in
  if not (StringSet.is_empty missing)
  then
    Printf.printf "missing: %s\n" (String.concat ~sep:", " (StringSet.elements missing));
  List.iter output.Linker.always_required_codes ~f:(fun always ->
      print_string (program_to_string always.Linker.program));
  print_string (program_to_string output.Linker.runtime_code)

let%expect_test "unused exports and helpers are not linked" =
  with_test_dir
  @@ fun ~write ->
  let f =
    write
      "runtime.mjs"
      {|
function sharedHelper(x) { return x + 1; }
function helperForB(x) { return x * 2; }
export function caml_a(x) { return sharedHelper(x); }
export function caml_b(x) { return sharedHelper(helperForB(x)); }
|}
  in
  link_for ~used:(StringSet.of_list [ "caml_a" ]) [ f ];
  [%expect
    {|
    function sharedHelper(x){return x + 1;}
    function caml_a(x){return sharedHelper(x);}
    |}]

let%expect_test "using both exports links the shared helper once" =
  with_test_dir
  @@ fun ~write ->
  let f =
    write
      "runtime.mjs"
      {|
function sharedHelper(x) { return x + 1; }
function helperForB(x) { return x * 2; }
export function caml_a(x) { return sharedHelper(x); }
export function caml_b(x) { return sharedHelper(helperForB(x)); }
|}
  in
  link_for ~used:(StringSet.of_list [ "caml_a"; "caml_b" ]) [ f ];
  [%expect
    {|
    function sharedHelper(x){return x + 1;}
    function caml_a(x){return sharedHelper(x);}
    function helperForB(x){return x * 2;}
    function caml_b(x){return sharedHelper(helperForB(x));}
    |}]

let%expect_test "imports pull code from other modules" =
  with_test_dir
  @@ fun ~write ->
  let b =
    write
      "b.mjs"
      {|
export function caml_b(x) { return x * 2; }
export function caml_b_unused(x) { return x; }
|}
  in
  let a =
    write
      "a.mjs"
      {|
import { caml_b } from './b.mjs';
export function caml_a(x) { return caml_b(x) + 1; }
|}
  in
  link_for ~used:(StringSet.of_list [ "caml_a" ]) [ a; b ];
  [%expect
    {| function caml_b(x){return x * 2;} function caml_a(x){return caml_b(x) + 1;} |}]

let%expect_test "module side effects run when any export is used" =
  with_test_dir
  @@ fun ~write ->
  let f =
    write
      "runtime.mjs"
      {|
const table = {};
table.init = true;
export function caml_get(x) { return table[x]; }
export function caml_unrelated(x) { return x; }
|}
  in
  link_for ~used:(StringSet.of_list [ "caml_unrelated" ]) [ f ];
  [%expect
    {| const table = {}; table.init = true; function caml_unrelated(x){return x;} |}]

let%expect_test "mutually recursive exports are merged" =
  with_test_dir
  @@ fun ~write ->
  let f =
    write
      "runtime.mjs"
      {|
export function caml_even(n) { return n === 0 ? true : caml_odd(n - 1); }
export function caml_odd(n) { return n === 0 ? false : caml_even(n - 1); }
export function caml_unused(x) { return x; }
|}
  in
  link_for ~used:(StringSet.of_list [ "caml_odd" ]) [ f ];
  [%expect
    {|
    function caml_even(n){return n === 0 ? true : caml_odd(n - 1);}
    function caml_odd(n){return n === 0 ? false : caml_even(n - 1);}
    |}]

let%expect_test "export aliases" =
  with_test_dir
  @@ fun ~write ->
  let f =
    write
      "runtime.mjs"
      {|
function impl(x) { return x + 1; }
export { impl as caml_succ, impl as caml_add_one };
|}
  in
  link_for ~used:(StringSet.of_list [ "caml_succ" ]) [ f ];
  [%expect {| function caml_add_one(x){return x + 1;} var caml_succ = caml_add_one; |}]

let%expect_test "module without exports is always included" =
  with_test_dir
  @@ fun ~write ->
  let f = write "effects.mjs" {|
globalThis.jsoo_effect = 42;
|} in
  link_for ~used:StringSet.empty [ f ];
  [%expect {| globalThis.jsoo_effect = 42; |}]

let%expect_test "helpers are renamed to avoid collisions between modules" =
  with_test_dir
  @@ fun ~write ->
  let a =
    write
      "a.mjs"
      {|
function helper(x) { return x + 1; }
export function caml_a(x) { return helper(x); }
|}
  in
  let b =
    write
      "b.mjs"
      {|
function helper(x) { return x - 1; }
export function caml_b(x) { return helper(x); }
|}
  in
  link_for ~used:(StringSet.of_list [ "caml_a"; "caml_b" ]) [ a; b ];
  [%expect
    {|
    function helper(x){return x + 1;}
    function caml_a(x){return helper(x);}
    function helper$0(x){return x - 1;}
    function caml_b(x){return helper$0(x);}
    |}]

let%expect_test "let and const bindings are supported" =
  with_test_dir
  @@ fun ~write ->
  let f =
    write
      "runtime.mjs"
      {|
const zero = 0;
export const caml_zero = zero;
export let caml_counter = 0;
|}
  in
  link_for ~used:(StringSet.of_list [ "caml_zero" ]) [ f ];
  [%expect {| const zero = 0; const caml_zero = zero; |}]

let%expect_test "unsupported constructs are errors" =
  with_test_dir
  @@ fun ~write ->
  let check name content =
    let f = write name content in
    try link_for ~used:StringSet.empty [ f ]
    with Failure msg ->
      (* Normalize separators and keep only the file's basename so the
         message is stable across platforms *)
      let msg =
        String.map msg ~f:(function
          | '\\' -> '/'
          | c -> c)
      in
      let msg =
        match String.rindex_opt msg '/' with
        | Some i -> String.sub msg ~pos:(i + 1) ~len:(String.length msg - i - 1)
        | None -> msg
      in
      Printf.printf "Failure: %s\n" msg
  in
  check "default.mjs" {|export default function caml_x() {}|};
  check "ns.mjs" {|import * as ns from './x.mjs'; export const caml_x = ns;|};
  check "side.mjs" {|import './x.mjs'; export const caml_x = 1;|};
  check "reexp.mjs" {|export { caml_x } from './x.mjs';|};
  [%expect
    {|
    Failure: default.mjs: default exports are not supported in runtime modules
    Failure: ns.mjs: namespace imports are not supported in runtime modules
    Failure: side.mjs: side-effect imports are not supported in runtime modules
    Failure: reexp.mjs: re-exports are not supported in runtime modules
    |}]
