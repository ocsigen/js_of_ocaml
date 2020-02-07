open Js_of_ocaml_compiler
open Stdlib
open Util

let print_mapping (sm : Source_map.t) =
  let sources = Array.of_list sm.sources in
  let _names = Array.of_list sm.names in
  List.iter sm.mappings ~f:(fun (m : Source_map.map) ->
      let file = function
        | -1 -> "null"
        | n -> sources.(n)
      in
      Printf.printf
        "%s:%d:%d -> %d:%d\n"
        (file m.ori_source)
        m.ori_line
        m.ori_col
        m.gen_line
        m.gen_col)

let%expect_test _ =
  with_temp_dir ~f:(fun () ->
      let ocaml_prog = {|let id x = x|} in
      let ocaml_file =
        ocaml_prog
        |> Filetype.ocaml_text_of_string
        |> Filetype.write_ocaml ~name:"test.ml"
      in
      let js_file =
        ocaml_file
        |> compile_ocaml_to_cmo ~debug:true
        |> compile_cmo_to_javascript ?flags:None ~pretty:true ~sourcemap:true
      in
      print_file (Filetype.path_of_ocaml_file ocaml_file);
      print_file (Filetype.path_of_js_file js_file);
      match extract_sourcemap js_file with
      | None -> Printf.printf "No sourcemap found\n"
      | Some sm -> print_mapping sm);
  [%expect
    {|
      $ cat "test.ml"
        1: let id x = x
      $ cat "test.js"
        1: (function(joo_global_object)
        2:    {"use strict";
        3:     var runtime=joo_global_object.jsoo_runtime;
        4:     function id(x){return x}
        5:     var Test=[0,id];
        6:     runtime.caml_register_global(0,Test,"Test");
        7:     return}
        8:   (function(){return this}()));
        9:
       10: //# sourceMappingURL=test.map
      null:-1:-1 -> 2:4
      /dune-root/test.ml:0:4 -> 3:13
      /dune-root/test.ml:0:7 -> 3:16
      /dune-root/test.ml:0:11 -> 3:19
      /dune-root/test.ml:0:7 -> 3:26
      /dune-root/test.ml:0:12 -> 3:27
      /dune-root/test.ml:0:4 -> 4:16
      null:-1:-1 -> 6:10
    |}]
