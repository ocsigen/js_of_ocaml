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

(* All profile/version/arch gating now lives in the test sources themselves
   (a floating [@@@if ...] from ppx_optcomp_light, or a per-test [@when ...]
   from ppx_expect_light), so the generated stanzas carry no [enabled_if]. *)
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
 (name %s_%d)
 (modules %s)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (deps
   (file %%{project_root}/compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file %%{project_root}/compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_optcomp_light ppx_expect_light)))
|}
        prefix
        basename
        basename
        (Hashtbl.hash prefix mod 100)
        basename)
