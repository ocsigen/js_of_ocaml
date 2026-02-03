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

open Js_of_ocaml_compiler.Stdlib
open Js_of_ocaml_compiler

let test_dir = Filename.concat (Sys.getcwd ()) "_esm_test_files"

let ensure_dir dir = if not (Sys.file_exists dir) then Unix.mkdir dir 0o755

let write_file path content =
  let oc = open_out_bin path in
  output_string oc content;
  close_out oc

let parse_file path =
  let lexer = Parse_js.Lexer.of_file path in
  Parse_js.parse `Module lexer

let normalize_path path =
  (* Simple path normalization: remove . and .. components *)
  let parts = String.split_on_char ~sep:'/' path in
  let rec normalize acc = function
    | [] -> List.rev acc
    | "." :: rest -> normalize acc rest
    | ".." :: rest -> (
        match acc with
        | _ :: acc' -> normalize acc' rest
        | [] -> normalize [] rest)
    | part :: rest -> normalize (part :: acc) rest
  in
  String.concat ~sep:"/" (normalize [] parts)

let resolve ~from specifier =
  if Filename.is_relative specifier
  then
    let dir = Filename.dirname from in
    let resolved = normalize_path (Filename.concat dir specifier) in
    if Sys.file_exists resolved then Some resolved else None
  else None

let contains_substring haystack needle =
  let needle_len = String.length needle in
  let haystack_len = String.length haystack in
  if needle_len > haystack_len
  then false
  else
    let found = ref false in
    for i = 0 to haystack_len - needle_len do
      if (not !found) && String.equal (String.sub haystack ~pos:i ~len:needle_len) needle
      then found := true
    done;
    !found

let bundle_to_string program =
  let buffer = Buffer.create 4096 in
  let pp = Pretty_print.to_buffer buffer in
  Pretty_print.set_compact pp false;
  Config.Flag.disable "debuginfo";
  Config.Flag.disable "shortvar";
  (* Assign names to unnamed variables before output *)
  let program = Js_assign.program program in
  let _ = Js_output.program pp program in
  Buffer.contents buffer

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

let%expect_test "bundle simple ES modules" =
  with_test_dir
  @@ fun ~write ->
  let entry =
    write
      "single.js"
      {|
export function hello() {
  return "Hello";
}
export const VALUE = 42;
|}
  in
  let graph = Esm.build_graph ~parse:parse_file ~resolve ~entry_points:[ entry ] in
  let module_count = Esm.ModuleId.Map.cardinal graph.modules in
  Printf.printf "Bundled %d modules\n" module_count;
  let entry_id = Esm.ModuleId.of_path entry in
  let bundled = Esm_bundle.bundle graph ~entry_points:[ entry_id ] in
  let output = bundle_to_string bundled in
  print_endline output;
  [%expect
    {|
    Bundled 1 modules
    function hello(){return "Hello";}
    const VALUE = 42;
    export { hello };
    export { VALUE };
    |}]

let%expect_test "bundle with imports" =
  with_test_dir
  @@ fun ~write ->
  let _ =
    write
      "lib.js"
      {|
export const VERSION = "1.0.0";
export function greet(name) {
  return "Hello, " + name;
}
|}
  in
  let entry =
    write
      "app.js"
      {|
import { VERSION, greet } from './lib.js';

export function app() {
  return greet("World") + " v" + VERSION;
}
|}
  in
  let graph = Esm.build_graph ~parse:parse_file ~resolve ~entry_points:[ entry ] in
  let module_count = Esm.ModuleId.Map.cardinal graph.modules in
  Printf.printf "Bundled %d modules\n" module_count;
  let entry_id = Esm.ModuleId.of_path entry in
  let bundled = Esm_bundle.bundle graph ~entry_points:[ entry_id ] in
  let output = bundle_to_string bundled in
  print_endline output;
  [%expect
    {|
    Bundled 2 modules
    const VERSION = "1.0.0";
    function greet(name){return "Hello, " + name;}
    function app(){return greet("World") + " v" + VERSION;}
    export { app };
    |}]

let%expect_test "import * as namespace" =
  with_test_dir
  @@ fun ~write ->
  let _ =
    write
      "math.js"
      {|
export const PI = 3.14159;
export function add(a, b) { return a + b; }
export function multiply(a, b) { return a * b; }
|}
  in
  let entry =
    write
      "app.js"
      {|
import * as Math from './math.js';

export function calculate() {
  return Math.add(1, 2) * Math.PI;
}
|}
  in
  let graph = Esm.build_graph ~parse:parse_file ~resolve ~entry_points:[ entry ] in
  let module_count = Esm.ModuleId.Map.cardinal graph.modules in
  Printf.printf "Bundled %d modules\n" module_count;
  let entry_id = Esm.ModuleId.of_path entry in
  let bundled = Esm_bundle.bundle graph ~entry_points:[ entry_id ] in
  let output = bundle_to_string bundled in
  print_endline output;
  [%expect
    {|
    Bundled 2 modules
    const PI = 3.14159;
    function add(a, b){return a + b;}
    function multiply(a, b){return a * b;}
    function calculate(){return add(1, 2) * PI;}
    export { calculate };
    |}]

let%expect_test "export * from re-exports" =
  with_test_dir
  @@ fun ~write ->
  let _ =
    write
      "utils.js"
      {|
export const UTIL_VALUE = 100;
export function utilFunc() { return "util"; }
|}
  in
  let _ =
    write "index.js" {|
export * from './utils.js';
export const INDEX_VALUE = 200;
|}
  in
  let entry =
    write
      "app.js"
      {|
import { UTIL_VALUE, utilFunc, INDEX_VALUE } from './index.js';

export function main() {
  return utilFunc() + UTIL_VALUE + INDEX_VALUE;
}
|}
  in
  let graph = Esm.build_graph ~parse:parse_file ~resolve ~entry_points:[ entry ] in
  let module_count = Esm.ModuleId.Map.cardinal graph.modules in
  Printf.printf "Bundled %d modules\n" module_count;
  let entry_id = Esm.ModuleId.of_path entry in
  let bundled = Esm_bundle.bundle graph ~entry_points:[ entry_id ] in
  let output = bundle_to_string bundled in
  print_endline output;
  [%expect
    {|
    Bundled 3 modules
    const UTIL_VALUE = 100;
    function utilFunc(){return "util";}
    const INDEX_VALUE = 200;
    function main(){return utilFunc() + UTIL_VALUE + INDEX_VALUE;}
    export { main };
    |}]

let%expect_test "import alias collision" =
  with_test_dir
  @@ fun ~write ->
  let _ = write "lib.js" {|export const foo = 1;|} in
  let _ = write "utils.js" {|export const bar = 2;|} in
  let entry =
    write
      "app.js"
      {|
import { foo as bar } from './lib.js';
import { bar as utilsBar } from './utils.js';

export function use() {
  return bar + utilsBar;
}
|}
  in
  let graph = Esm.build_graph ~parse:parse_file ~resolve ~entry_points:[ entry ] in
  let entry_id = Esm.ModuleId.of_path entry in
  let bundled = Esm_bundle.bundle graph ~entry_points:[ entry_id ] in
  let output = bundle_to_string bundled in
  print_endline output;
  [%expect
    {|
    const bar = 2;
    const foo = 1;
    function use(){return foo + bar;}
    export { use };
    |}]

let%expect_test "tree shaking removes unused exports" =
  with_test_dir
  @@ fun ~write ->
  let _ =
    write
      "lib.js"
      {|
export const USED_CONST = "I am used";
export const UNUSED_CONST = "I am not used";

export function usedFunction() {
  return USED_CONST;
}

export function unusedFunction() {
  return "This function is never called";
}

export function anotherUnused() {
  return unusedFunction();
}
|}
  in
  let entry =
    write
      "app.js"
      {|
import { usedFunction, USED_CONST } from './lib.js';

export function app() {
  return usedFunction() + " - " + USED_CONST;
}
|}
  in
  (* Bundle WITH tree shaking *)
  let output_shaken =
    Esm_bundle.bundle_modules
      ~parse:parse_file
      ~resolve
      ~entry_points:[ entry ]
      ~tree_shake:true
  in
  let output_shaken_str = bundle_to_string output_shaken in
  (* Bundle WITHOUT tree shaking for comparison *)
  let output_full =
    Esm_bundle.bundle_modules
      ~parse:parse_file
      ~resolve
      ~entry_points:[ entry ]
      ~tree_shake:false
  in
  let output_full_str = bundle_to_string output_full in
  Printf.printf
    "=== Without tree shaking (%d bytes) ===\n"
    (String.length output_full_str);
  print_endline output_full_str;
  Printf.printf "=== With tree shaking (%d bytes) ===\n" (String.length output_shaken_str);
  print_endline output_shaken_str;
  (* Verify unused code is removed *)
  let has_unused_const = contains_substring output_shaken_str "UNUSED_CONST" in
  let has_unused_func = contains_substring output_shaken_str "unusedFunction" in
  let has_another_unused = contains_substring output_shaken_str "anotherUnused" in
  Printf.printf "Tree shaking results:\n";
  Printf.printf "  UNUSED_CONST removed: %b\n" (not has_unused_const);
  Printf.printf "  unusedFunction removed: %b\n" (not has_unused_func);
  Printf.printf "  anotherUnused removed: %b\n" (not has_another_unused);
  [%expect
    {|
    === Without tree shaking (308 bytes) ===
    const USED_CONST = "I am used";
    const UNUSED_CONST = "I am not used";
    function usedFunction(){return USED_CONST;}
    function unusedFunction(){return "This function is never called";}
    function anotherUnused(){return unusedFunction();}
    function app(){return usedFunction() + " - " + USED_CONST;}
    export { app };

    === With tree shaking (152 bytes) ===
    const USED_CONST = "I am used";
    function usedFunction(){return USED_CONST;}
    function app(){return usedFunction() + " - " + USED_CONST;}
    export { app };

    Tree shaking results:
      UNUSED_CONST removed: true
      unusedFunction removed: true
      anotherUnused removed: true
    |}]

let%expect_test "nested variable shadowing" =
  with_test_dir
  @@ fun ~write ->
  let entry =
    write
      "shadow.js"
      {|
export const foo = "top-level";

export function test() {
  let foo = "shadowed";  // This should NOT be renamed
  return foo;            // This should refer to shadowed, not top-level
}

export function usesTopLevel() {
  return foo;  // This should refer to top-level foo
}
|}
  in
  let graph = Esm.build_graph ~parse:parse_file ~resolve ~entry_points:[ entry ] in
  let entry_id = Esm.ModuleId.of_path entry in
  let bundled = Esm_bundle.bundle graph ~entry_points:[ entry_id ] in
  let output = bundle_to_string bundled in
  print_endline output;
  [%expect
    {|
    const foo = "top-level";
    function test(){let foo = "shadowed"; return foo;}
    function usesTopLevel(){return foo;}
    export { usesTopLevel };
    export { test };
    export { foo };
    |}]

let%expect_test "bundle writes to file" =
  with_test_dir
  @@ fun ~write ->
  let entry = write "single.js" {|export function hello() { return "Hello"; }|} in
  let graph = Esm.build_graph ~parse:parse_file ~resolve ~entry_points:[ entry ] in
  let entry_id = Esm.ModuleId.of_path entry in
  let bundled = Esm_bundle.bundle graph ~entry_points:[ entry_id ] in
  let output = bundle_to_string bundled in
  let bundle_path = write "bundle.js" output in
  Printf.printf "Bundle written to bundle.js\n";
  Printf.printf "Bundle size: %d bytes\n" (String.length output);
  let ic = open_in_bin bundle_path in
  let file_size = in_channel_length ic in
  close_in ic;
  Printf.printf "File size on disk: %d bytes\n" file_size;
  [%expect
    {|
    Bundle written to bundle.js
    Bundle size: 52 bytes
    File size on disk: 52 bytes
    |}]

let find_runtime_dir () =
  (* Navigate from test working directory to find runtime/js *)
  let rec find dir =
    let runtime = Filename.concat dir "runtime/js" in
    if Sys.file_exists runtime && Sys.is_directory runtime
    then Some runtime
    else
      let parent = Filename.dirname dir in
      if String.equal parent dir then None else find parent
  in
  find (Sys.getcwd ())

let%expect_test "namespace optimization with bracket notation" =
  with_test_dir
  @@ fun ~write ->
  let _ = write "lib.js" {|export const foo = 1; export const bar = 2;|} in
  let entry =
    write
      "app.js"
      {|
import * as Lib from './lib.js';
export function test() { return Lib["foo"] + Lib["bar"]; }
|}
  in
  let graph = Esm.build_graph ~parse:parse_file ~resolve ~entry_points:[ entry ] in
  let entry_id = Esm.ModuleId.of_path entry in
  let bundled = Esm_bundle.bundle graph ~entry_points:[ entry_id ] in
  let output = bundle_to_string bundled in
  print_endline output;
  let has_object =
    contains_substring output "{foo:" || contains_substring output "{ foo:"
  in
  Printf.printf "Namespace object created: %b\n" has_object;
  [%expect
    {|
    const foo = 1;
    const bar = 2;
    function test(){return foo + bar;}
    export { test };

    Namespace object created: false
    |}]

let%expect_test "namespace not optimized when passed as value" =
  with_test_dir
  @@ fun ~write ->
  let _ = write "lib.js" {|export const foo = 1; export const bar = 2;|} in
  let entry =
    write
      "app.js"
      {|
import * as Lib from './lib.js';
function useNamespace(ns) { return ns.foo; }
export function test() { return useNamespace(Lib); }
|}
  in
  let graph = Esm.build_graph ~parse:parse_file ~resolve ~entry_points:[ entry ] in
  let entry_id = Esm.ModuleId.of_path entry in
  let bundled = Esm_bundle.bundle graph ~entry_points:[ entry_id ] in
  let output = bundle_to_string bundled in
  print_endline output;
  let has_object =
    contains_substring output "{bar:" || contains_substring output "{foo:"
  in
  Printf.printf "Namespace object created: %b\n" has_object;
  [%expect
    {|
    const foo = 1;
    const bar = 2;
    const Lib = {foo: foo, bar: bar};
    function useNamespace(ns){return ns.foo;}
    function test(){return useNamespace(Lib);}
    export { test };

    Namespace object created: true
    |}]

let%expect_test "namespace not optimized with dynamic access" =
  with_test_dir
  @@ fun ~write ->
  let _ = write "lib.js" {|export const foo = 1; export const bar = 2;|} in
  let entry =
    write
      "app.js"
      {|
import * as Lib from './lib.js';
export function test(key) { return Lib[key]; }
|}
  in
  let graph = Esm.build_graph ~parse:parse_file ~resolve ~entry_points:[ entry ] in
  let entry_id = Esm.ModuleId.of_path entry in
  let bundled = Esm_bundle.bundle graph ~entry_points:[ entry_id ] in
  let output = bundle_to_string bundled in
  print_endline output;
  let has_object =
    contains_substring output "{bar:" || contains_substring output "{foo:"
  in
  Printf.printf "Namespace object created: %b\n" has_object;
  [%expect
    {|
    const foo = 1;
    const bar = 2;
    const Lib = {foo: foo, bar: bar};
    function test(key){return Lib[key];}
    export { test };

    Namespace object created: true
    |}]

let%expect_test "mixed namespace and named imports" =
  with_test_dir
  @@ fun ~write ->
  let _ =
    write "lib.js" {|export const a = 1; export const b = 2; export const c = 3;|}
  in
  let entry =
    write
      "app.js"
      {|
import * as Lib from './lib.js';
import { c } from './lib.js';
export function test() { return Lib.a + Lib.b + c; }
|}
  in
  let graph = Esm.build_graph ~parse:parse_file ~resolve ~entry_points:[ entry ] in
  let entry_id = Esm.ModuleId.of_path entry in
  let bundled = Esm_bundle.bundle graph ~entry_points:[ entry_id ] in
  let output = bundle_to_string bundled in
  print_endline output;
  let has_namespace_object = contains_substring output "= {" in
  Printf.printf "Namespace object created: %b\n" has_namespace_object;
  [%expect
    {|
    const a = 1;
    const b = 2;
    const c = 3;
    function test(){return a + b + c;}
    export { test };

    Namespace object created: false
    |}]

let%expect_test "simple cyclic dependency" =
  with_test_dir
  @@ fun ~write ->
  let _ =
    write
      "a.js"
      {|import { b } from './b.js'; export const a = 1; export function useB() { return b + a; }|}
  in
  let _ =
    write
      "b.js"
      {|import { a } from './a.js'; export const b = 2; export function useA() { return a + b; }|}
  in
  let entry =
    write
      "main.js"
      {|import { useB } from './a.js'; import { useA } from './b.js'; export function main() { return useA() + useB(); }|}
  in
  let graph = Esm.build_graph ~parse:parse_file ~resolve ~entry_points:[ entry ] in
  let module_count = Esm.ModuleId.Map.cardinal graph.modules in
  Printf.printf "Bundled %d modules (with cycle)\n" module_count;
  let entry_id = Esm.ModuleId.of_path entry in
  let bundled = Esm_bundle.bundle graph ~entry_points:[ entry_id ] in
  let output = bundle_to_string bundled in
  print_endline output;
  [%expect
    {|
    Bundled 3 modules (with cycle)
    const b = 2;
    function useA(){return a + b;}
    const a = 1;
    function useB(){return b + a;}
    function main(){return useA() + useB();}
    export { main };
    |}]

let%expect_test "three-way cyclic dependency" =
  with_test_dir
  @@ fun ~write ->
  let _ =
    write
      "a.js"
      {|import { c } from './c.js'; export const a = "a"; export function fromA() { return a + c; }|}
  in
  let _ =
    write
      "b.js"
      {|import { a } from './a.js'; export const b = "b"; export function fromB() { return b + a; }|}
  in
  let _ =
    write
      "c.js"
      {|import { b } from './b.js'; export const c = "c"; export function fromC() { return c + b; }|}
  in
  let entry =
    write
      "main.js"
      {|import { fromA } from './a.js'; import { fromB } from './b.js'; import { fromC } from './c.js'; export function main() { return fromA() + fromB() + fromC(); }|}
  in
  let graph = Esm.build_graph ~parse:parse_file ~resolve ~entry_points:[ entry ] in
  let module_count = Esm.ModuleId.Map.cardinal graph.modules in
  Printf.printf "Bundled %d modules (with 3-way cycle)\n" module_count;
  let entry_id = Esm.ModuleId.of_path entry in
  let bundled = Esm_bundle.bundle graph ~entry_points:[ entry_id ] in
  let output = bundle_to_string bundled in
  print_endline output;
  [%expect
    {|
    Bundled 4 modules (with 3-way cycle)
    const c = "c";
    function fromC(){return c + b;}
    const b = "b";
    function fromB(){return b + a;}
    const a = "a";
    function fromA(){return a + c;}
    function main(){return fromA() + fromB() + fromC();}
    export { main };
    |}]

let%expect_test "tree shaking with destructuring exports" =
  with_test_dir
  @@ fun ~write ->
  let _ =
    write
      "lib.js"
      {|
const obj = { used: 1, unused: 2 };
export const { used, unused } = obj;
|}
  in
  let entry =
    write
      "app.js"
      {|
import { used } from './lib.js';
export function app() { return used; }
|}
  in
  let output_shaken =
    Esm_bundle.bundle_modules
      ~parse:parse_file
      ~resolve
      ~entry_points:[ entry ]
      ~tree_shake:true
  in
  let output_str = bundle_to_string output_shaken in
  print_endline output_str;
  let has_unused = contains_substring output_str "unused" in
  Printf.printf "unused export removed: %b\n" (not has_unused);
  [%expect
    {|
    const obj = {used: 1, unused: 2};
    const {used: used, unused: unused} = obj;
    function app(){return used;}
    export { app };

    unused export removed: false
    |}]

let%expect_test "tree shaking preserves side-effect imports" =
  with_test_dir
  @@ fun ~write ->
  let _ = write "logger.js" {|
export function log(msg) { console.log(msg); }
|} in
  let _ =
    write
      "lib.js"
      {|
import { log } from './logger.js';
log("side effect");
export const foo = 1;
|}
  in
  let entry =
    write
      "app.js"
      {|
import { foo } from './lib.js';
export function app() { return foo; }
|}
  in
  let output_shaken =
    Esm_bundle.bundle_modules
      ~parse:parse_file
      ~resolve
      ~entry_points:[ entry ]
      ~tree_shake:true
  in
  let output_str = bundle_to_string output_shaken in
  print_endline output_str;
  let has_log = contains_substring output_str "log" in
  Printf.printf "side-effect import preserved: %b\n" has_log;
  [%expect
    {|
    function log(msg){console.log(msg);}
    log("side effect");
    const foo = 1;
    function app(){return foo;}
    export { app };

    side-effect import preserved: true
    |}]

let%expect_test "tree shaking removes unused imports with side-effects" =
  with_test_dir
  @@ fun ~write ->
  let _ = write "used.js" {|export function used() { return 1; }|} in
  let _ = write "unused.js" {|export function unused() { return 2; }|} in
  let _ =
    write
      "lib.js"
      {|
import { used } from './used.js';
import { unused } from './unused.js';
used();
export const foo = 1;
|}
  in
  let entry =
    write
      "app.js"
      {|
import { foo } from './lib.js';
export function app() { return foo; }
|}
  in
  let output_shaken =
    Esm_bundle.bundle_modules
      ~parse:parse_file
      ~resolve
      ~entry_points:[ entry ]
      ~tree_shake:true
  in
  let output_str = bundle_to_string output_shaken in
  print_endline output_str;
  let has_used = contains_substring output_str "used" in
  let has_unused = contains_substring output_str "unused" in
  Printf.printf "used import preserved: %b\n" has_used;
  Printf.printf "unused import removed: %b\n" (not has_unused);
  [%expect
    {|
    function used(){return 1;}
    used();
    const foo = 1;
    function app(){return foo;}
    export { app };

    used import preserved: true
    unused import removed: true
    |}]

let%expect_test "tree shaking preserves side-effect-only imports" =
  (* Test import './module.js' style imports that have no bindings *)
  with_test_dir
  @@ fun ~write ->
  let _ = write "setup.js" {|
console.log("setup module loaded");
export {};
|} in
  let _ = write "lib.js" {|
import './setup.js';
export const foo = 1;
|} in
  let entry =
    write
      "app.js"
      {|
import { foo } from './lib.js';
export function app() { return foo; }
|}
  in
  let output_shaken =
    Esm_bundle.bundle_modules
      ~parse:parse_file
      ~resolve
      ~entry_points:[ entry ]
      ~tree_shake:true
  in
  let output_str = bundle_to_string output_shaken in
  print_endline output_str;
  let has_setup = contains_substring output_str "setup module loaded" in
  Printf.printf "side-effect-only import preserved: %b\n" has_setup;
  [%expect
    {|
    console.log("setup module loaded");
    const foo = 1;
    function app(){return foo;}
    export { app };

    side-effect-only import preserved: true
    |}]

let%expect_test "tree shaking preserves transitive side-effect imports" =
  (* Test that import './a.js' -> import './b.js' chains are preserved *)
  with_test_dir
  @@ fun ~write ->
  let _ = write "deep.js" {|
console.log("deep module");
export {};
|} in
  let _ =
    write "middle.js" {|
import './deep.js';
console.log("middle module");
export {};
|}
  in
  let _ = write "lib.js" {|
import './middle.js';
export const foo = 1;
|} in
  let entry =
    write
      "app.js"
      {|
import { foo } from './lib.js';
export function app() { return foo; }
|}
  in
  let output_shaken =
    Esm_bundle.bundle_modules
      ~parse:parse_file
      ~resolve
      ~entry_points:[ entry ]
      ~tree_shake:true
  in
  let output_str = bundle_to_string output_shaken in
  print_endline output_str;
  let has_deep = contains_substring output_str "deep module" in
  let has_middle = contains_substring output_str "middle module" in
  Printf.printf "deep side-effect preserved: %b\n" has_deep;
  Printf.printf "middle side-effect preserved: %b\n" has_middle;
  [%expect
    {|
    console.log("deep module");
    console.log("middle module");
    const foo = 1;
    function app(){return foo;}
    export { app };

    deep side-effect preserved: true
    middle side-effect preserved: true
    |}]

let%expect_test "intra-module dead code elimination" =
  with_test_dir
  @@ fun ~write ->
  let _ = write "helper.js" {|export function helper() { return 1; }|} in
  let _ =
    write
      "lib.js"
      {|
import { helper } from './helper.js';
function internal() { return helper(); }
export function used() { return 1; }
export function unused() { return internal(); }
|}
  in
  let entry =
    write
      "app.js"
      {|
import { used } from './lib.js';
export function app() { return used(); }
|}
  in
  let output_shaken =
    Esm_bundle.bundle_modules
      ~parse:parse_file
      ~resolve
      ~entry_points:[ entry ]
      ~tree_shake:true
  in
  let output_str = bundle_to_string output_shaken in
  print_endline output_str;
  let has_internal = contains_substring output_str "internal" in
  let has_helper = contains_substring output_str "helper" in
  let has_unused = contains_substring output_str "unused" in
  Printf.printf "internal function removed: %b\n" (not has_internal);
  Printf.printf "helper import removed: %b\n" (not has_helper);
  Printf.printf "unused export removed: %b\n" (not has_unused);
  [%expect
    {|
    function used(){return 1;} function app(){return used();} export { app };

    internal function removed: true
    helper import removed: true
    unused export removed: true
    |}]

let%expect_test "diamond dependency - both paths need different exports" =
  with_test_dir
  @@ fun ~write ->
  (* A exports x and z *)
  let _ =
    write
      "a.js"
      {|
export function x() { return "x"; }
export function z() { return "z"; }
export function unused() { return "unused"; }
|}
  in
  (* B depends on A.z *)
  let _ =
    write "b.js" {|
import { z } from './a.js';
export function y() { return z(); }
|}
  in
  (* root depends on A.x and B.y *)
  let entry =
    write
      "root.js"
      {|
import { x } from './a.js';
import { y } from './b.js';
export function main() { return x() + y(); }
|}
  in
  let output =
    Esm_bundle.bundle_modules
      ~parse:parse_file
      ~resolve
      ~entry_points:[ entry ]
      ~tree_shake:true
  in
  let output_str = bundle_to_string output in
  print_endline output_str;
  let has_x = contains_substring output_str "\"x\"" in
  let has_z = contains_substring output_str "\"z\"" in
  let has_unused = contains_substring output_str "unused" in
  Printf.printf "x preserved (direct): %b\n" has_x;
  Printf.printf "z preserved (via B): %b\n" has_z;
  Printf.printf "unused removed: %b\n" (not has_unused);
  [%expect
    {|
    function x(){return "x";}
    function z(){return "z";}
    function y(){return z();}
    function main(){return x() + y();}
    export { main };

    x preserved (direct): true
    z preserved (via B): true
    unused removed: true
    |}]

(* ========== merge_modules tests ========== *)

let analyze_js ~resolve name content =
  (* Parse and analyze a JS module *)
  let lexer = Parse_js.Lexer.of_string content in
  let program = Parse_js.parse `Module lexer in
  let id = Esm.ModuleId.of_path name in
  Esm.analyze_module ~resolve id program

let%expect_test "merge_modules simple" =
  (* Test merging two independent modules *)
  let resolve _ = failwith "no resolution needed" in
  let m1 =
    analyze_js
      ~resolve
      "a.js"
      {|
export const foo = 1;
export function hello() { return "hello"; }
|}
  in
  let m2 =
    analyze_js
      ~resolve
      "b.js"
      {|
export const bar = 2;
export function world() { return "world"; }
|}
  in
  let merged = Esm_bundle.merge_modules ~dest:"bundle.js" [ m1; m2 ] in
  let output = bundle_to_string merged in
  print_endline output;
  [%expect
    {|
    const foo = 1;
    function hello(){return "hello";}
    const bar = 2;
    function world(){return "world";}
    export { hello };
    export { foo };
    export { world };
    export { bar };
    |}]

let%expect_test "merge_modules removes self-imports" =
  (* Test that imports from the destination file are removed and substituted *)
  let resolve specifier =
    (* Resolve ./a.js to bundle.js to simulate self-import *)
    if String.equal specifier "./a.js"
    then Esm.ModuleId.of_path "bundle.js"
    else Esm.ModuleId.of_path specifier
  in
  let m1 = analyze_js ~resolve "a.js" {|
export const foo = 42;
|} in
  let m2 =
    analyze_js
      ~resolve
      "b.js"
      {|
import { foo } from './a.js';
export function useFoo() { return foo + 1; }
|}
  in
  let merged = Esm_bundle.merge_modules ~dest:"bundle.js" [ m1; m2 ] in
  let output = bundle_to_string merged in
  print_endline output;
  (* Verify no import statement remains *)
  let has_import = contains_substring output "import" in
  Printf.printf "import removed: %b\n" (not has_import);
  [%expect
    {|
    const foo = 42;
    function useFoo(){return foo + 1;}
    export { foo };
    export { useFoo };

    import removed: true
    |}]

let%expect_test "merge_modules with default export" =
  let resolve specifier =
    if String.equal specifier "./a.js"
    then Esm.ModuleId.of_path "bundle.js"
    else Esm.ModuleId.of_path specifier
  in
  let m1 =
    analyze_js ~resolve "a.js" {|
export default function greet() { return "hi"; }
|}
  in
  let m2 =
    analyze_js
      ~resolve
      "b.js"
      {|
import greet from './a.js';
export function useGreet() { return greet() + "!"; }
|}
  in
  let merged = Esm_bundle.merge_modules ~dest:"bundle.js" [ m1; m2 ] in
  let output = bundle_to_string merged in
  print_endline output;
  let has_import = contains_substring output "import" in
  Printf.printf "import removed: %b\n" (not has_import);
  [%expect
    {|
    function greet(){return "hi";}
    function useGreet(){return greet() + "!";}
    export { greet as default };
    export { useGreet };

    import removed: true
    |}]

let%expect_test "merge_modules preserves all exports" =
  let resolve _ = failwith "no resolution needed" in
  let m1 =
    analyze_js
      ~resolve
      "utils.js"
      {|
export const A = 1;
export const B = 2;
export function helper() { return A + B; }
|}
  in
  let m2 =
    analyze_js
      ~resolve
      "main.js"
      {|
export const C = 3;
export function main() { return C; }
|}
  in
  let merged = Esm_bundle.merge_modules ~dest:"bundle.js" [ m1; m2 ] in
  let output = bundle_to_string merged in
  print_endline output;
  (* Count exports *)
  let export_count =
    List.length (List.filter ~f:(fun line -> contains_substring line "export") (String.split_on_char ~sep:'\n' output))
  in
  Printf.printf "Total exports: %d\n" export_count;
  [%expect
    {|
    const A = 1;
    const B = 2;
    function helper(){return A + B;}
    const C = 3;
    function main(){return C;}
    export { helper };
    export { B };
    export { A };
    export { main };
    export { C };

    Total exports: 5
    |}]

let%expect_test "merge_modules preserves external imports" =
  (* Test that imports from external modules (not dest) are preserved *)
  let resolve specifier =
    if String.equal specifier "./internal.js"
    then Esm.ModuleId.of_path "bundle.js"
    else Esm.ModuleId.of_path specifier
  in
  let m1 =
    analyze_js ~resolve "internal.js" {|
export const foo = 42;
|}
  in
  let m2 =
    analyze_js
      ~resolve
      "main.js"
      {|
import { foo } from './internal.js';
import { external } from './external.js';
import defaultExt from './default-ext.js';
export function useBoth() { return foo + external + defaultExt; }
|}
  in
  let merged = Esm_bundle.merge_modules ~dest:"bundle.js" [ m1; m2 ] in
  let output = bundle_to_string merged in
  print_endline output;
  (* Verify external imports are preserved *)
  let has_external_import = contains_substring output "external.js" in
  let has_default_import = contains_substring output "default-ext.js" in
  let has_internal_import = contains_substring output "internal.js" in
  Printf.printf "external.js import preserved: %b\n" has_external_import;
  Printf.printf "default-ext.js import preserved: %b\n" has_default_import;
  Printf.printf "internal.js import removed: %b\n" (not has_internal_import);
  [%expect
    {|
    import { external } from "./external.js";
    import defaultExt from "./default-ext.js";
    const foo = 42;
    function useBoth(){return foo + external + defaultExt;}
    export { foo };
    export { useBoth };

    external.js import preserved: true
    default-ext.js import preserved: true
    internal.js import removed: true
    |}]

let%expect_test "merge_modules with same-named helpers" =
  (* Test that modules with identically named functions don't collide *)
  let resolve _ = failwith "no resolution needed" in
  let m1 =
    analyze_js
      ~resolve
      "a.js"
      {|
function helper() { return "from a"; }
export function useA() { return helper(); }
|}
  in
  let m2 =
    analyze_js
      ~resolve
      "b.js"
      {|
function helper() { return "from b"; }
export function useB() { return helper(); }
|}
  in
  let merged = Esm_bundle.merge_modules ~dest:"bundle.js" [ m1; m2 ] in
  let output = bundle_to_string merged in
  print_endline output;
  (* Count helper functions - should be 2 distinct ones *)
  let helper_count =
    List.length
      (List.filter
         ~f:(fun line -> contains_substring line "function helper")
         (String.split_on_char ~sep:'\n' output))
  in
  Printf.printf "Number of helper functions: %d\n" helper_count;
  [%expect
    {|
    function helper(){return "from a";}
    function useA(){return helper();}
    function helper$0(){return "from b";}
    function useB(){return helper$0();}
    export { useA };
    export { useB };

    Number of helper functions: 2
    |}]
