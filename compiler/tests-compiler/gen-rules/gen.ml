let is_implem x =
  if String.equal (Filename.extension x) ".ml"
  then
    let fname = Filename.chop_extension x in
    try
      String.iter
        (function
          | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> ()
          | _ -> raise Exit)
        fname;
      true
    with Exit -> false
  else false

let () = set_binary_mode_out stdout true

let () =
  Array.to_list (Sys.readdir ".")
  |> List.filter is_implem
  |> List.sort compare
  |> List.iter (fun f ->
         let basename = Filename.chop_extension f in
         Printf.printf
           {|
(library
 (name jsooexp_%s)
 (modules %s)
 (libraries js_of_ocaml_compiler unix str jsoo_compiler_expect_tests_helper)
 (inline_tests
  (flags -allow-output-patterns)
  (deps
   (file ../../compiler/bin-js_of_ocaml/js_of_ocaml.exe)
   (file ../../compiler/bin-jsoo_minify/jsoo_minify.exe)))
 (flags (:standard -open Jsoo_compiler_expect_tests_helper))
 (preprocess
  (pps ppx_expect)))
|}
           basename
           basename)
