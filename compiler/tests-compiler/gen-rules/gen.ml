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

type lang =
  | Not of lang
  | GE of lang * lang
  | LT of lang * lang
  | Var of string
  | Atom of string
  | And of lang list

let ocaml_version = Var "ocaml_version"

let arch_sixtyfour = Var "arch_sixtyfour"

let oxcaml = Var "oxcaml_supported"

let ge v = GE (ocaml_version, Atom v)

let lt v = LT (ocaml_version, Atom v)

let not x = Not x

let and_ = function
  | [] -> assert false
  | [ x ] -> x
  | l -> And l

let lib_enabled_if = function
  | "obj" | "effects" -> [ ge "5" ]
  | "gh1051" -> [ arch_sixtyfour ]
  | _ -> []

let test_enabled_if = function
  | "obj" -> [ ge "5"; not oxcaml ] (* Some Obj functions are no longer primitives *)
  | "lazy" -> [ ge "5" ]
  | "gh1051" -> [ arch_sixtyfour ]
  | "rec52" -> [ ge "5.2" ]
  | "rec" -> [ lt "5.2" ]
  | "gh1354"
  | "gh1868"
  | "exceptions"
  | "effects_continuations"
  | "effects_exceptions"
  | "eliminate_exception_handler"
  | "loops"
  | "global_deadcode" -> [ not oxcaml ] (* In OxCaml, raise is always reraise *)
  | "effects" ->
      [ not oxcaml ] (* Call to Printf.printf is somehow compiled differently *)
  | "gh747" -> [ not oxcaml ] (* More debug locations *)
  | _ -> []

let rec pp f = function
  | Not x -> Format.fprintf f "(not %a)" pp x
  | GE (a, b) -> Format.fprintf f "(>= %a %a)" pp a pp b
  | LT (a, b) -> Format.fprintf f "(< %a %a)" pp a pp b
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
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests%a
  (deps
   (file %%{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %%{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))
|}
        prefix
        basename
        basename
        (Hashtbl.hash prefix mod 100)
        (enabled_if 1)
        (lib_enabled_if basename)
        basename
        (enabled_if 2)
        (test_enabled_if basename))
