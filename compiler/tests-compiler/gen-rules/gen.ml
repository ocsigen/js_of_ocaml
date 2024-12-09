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
  | GE52
  | LT52
  | B64
  | Any

let lib_enabled_if = function
  | "obj" | "effects" -> GE5
  | "gh1051" -> B64
  | _ -> Any

let test_enabled_if = function
  | "obj" | "lazy" -> GE5
  | "gh1051" -> B64
  | "rec52" -> GE52
  | "rec" -> LT52
  | _ -> Any

let enabled_if = function
  | Any -> "true"
  | GE5 -> "(>= %{ocaml_version} 5)"
  | GE52 -> "(>= %{ocaml_version} 5.2)"
  | LT52 -> "(< %{ocaml_version} 5.2)"
  | B64 -> "%{arch_sixtyfour}"

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
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (enabled_if %s)
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
           (enabled_if (lib_enabled_if basename))
           basename
           (enabled_if (test_enabled_if basename)))
