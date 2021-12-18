(* Js_of_ocaml
 * http://www.ocsigen.org/js_of_ocaml/
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
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

open Js_of_ocaml_compiler
open Stdlib
open Util

let print_mapping (sm : Source_map.t) =
  let sources = Array.of_list sm.sources in
  let _names = Array.of_list sm.names in
  List.iter sm.mappings ~f:(fun (m : Source_map.map) ->
      let file = function
        | -1 -> "null"
        | n -> normalize_path sources.(n)
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
        1: (function(globalThis)
        2:    {"use strict";
        3:     var runtime=globalThis.jsoo_runtime;
        4:     function id(x){return x}
        5:     var Test=[0,id];
        6:     runtime.caml_register_global(0,Test,"Test");
        7:     return}
        8:   (globalThis));
        9:
       10: //# sourceMappingURL=test.map
      null:-1:-1 -> 3:4
      /dune-root/test.ml:1:4 -> 4:13
      /dune-root/test.ml:1:7 -> 4:16
      /dune-root/test.ml:1:11 -> 4:19
      /dune-root/test.ml:1:7 -> 4:26
      /dune-root/test.ml:1:12 -> 4:27
      /dune-root/test.ml:1:4 -> 5:16
      null:-1:-1 -> 7:10
    |}]

let%expect_test _ =
  with_temp_dir ~f:(fun () ->
      let js_prog = {|
function x (a, b) {
  return a + b;
}
|} in
      let js_file =
        js_prog |> Filetype.js_text_of_string |> Filetype.write_js ~name:"test.ml"
      in
      let js_min_file = js_file |> jsoo_minify ~flags:[ "--debug-info" ] ~pretty:true in
      print_file (Filetype.path_of_js_file js_file);
      print_file (Filetype.path_of_js_file js_min_file));
  [%expect
    {|
    $ cat "test.ml"
      1:
      2: function x (a, b) {
      3:   return a + b;
      4: }
    $ cat "test.min.js"
      1:  /*<<test.ml 2 0>>*/ function x(a,b)
      2:  { /*<<test.ml 3 2>>*/ return a + b /*<<test.ml 4 0>>*/ }
 |}]

let%expect_test _ =
  let map_str = ";;;;EAEE,EAAE,EAAC,CAAE;ECQY,UACC" in
  let map = Source_map.mapping_of_string map_str in
  let map_str' = Source_map.string_of_mapping map in
  print_endline map_str;
  print_endline map_str';
  [%expect
    {|
    ;;;;EAEE,EAAE,EAAC,CAAE;ECQY,UACC
    ;;;;EAEE,EAAE,EAAC,CAAE;ECQY,UACC |}]

let%expect_test _ =
  let gen (gen_line, gen_col) (ori_line, ori_col) ori_source : Source_map.map =
    { gen_line; gen_col; ori_source; ori_line; ori_col; ori_name = None }
  in
  let s1 : Source_map.t =
    { Source_map.empty with
      names = [ "na"; "nb"; "nc" ]
    ; sources = [ "sa"; "sb" ]
    ; mappings = [ gen (1, 1) (10, 10) 0; gen (3, 3) (20, 20) 1 ]
    }
  in
  let s2 : Source_map.t =
    { Source_map.empty with
      names = [ "na2"; "nb2" ]
    ; sources = [ "sa2" ]
    ; mappings = [ gen (3, 3) (5, 5) 0 ]
    }
  in
  let m = Source_map.merge [ 0, "", s1; 20, "", s2 ] in
  (match m with
  | None -> ()
  | Some sm ->
      print_endline (Source_map.string_of_mapping sm.mappings);
      print_mapping sm);
  [%expect
    {|
    CASU;;GCUU;;;;;;;;;;;;;;;;;;;;GCff
    sa:10:10 -> 1:1
    sb:20:20 -> 3:3
    sa2:5:5 -> 23:3 |}]
