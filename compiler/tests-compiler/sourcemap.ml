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

let print_mapping ~line_offset ~col_offset (sm : Source_map.Standard.t) =
  let sources = Array.of_list sm.sources in
  let _names = Array.of_list sm.names in
  let mappings = Source_map.Mappings.decode_exn sm.mappings in
  List.iter mappings ~f:(fun (m : Source_map.map) ->
      match m with
      | Gen_Ori { gen_line; gen_col; ori_line; ori_col; ori_source }
      | Gen_Ori_Name { gen_line; gen_col; ori_line; ori_col; ori_source; ori_name = _ } ->
          let file n = normalize_path sources.(n) in
          Printf.printf
            "%s:%d:%d -> %d:%d\n"
            (file ori_source)
            ori_line
            ori_col
            (gen_line + line_offset)
            (gen_col + col_offset)
      | Gen { gen_line; gen_col } ->
          Printf.printf "null -> %d:%d\n" (gen_line + line_offset) (gen_col + col_offset))

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
      | Some (Standard sm) -> print_mapping ~line_offset:0 ~col_offset:0 sm
      | Some (Index i) ->
          List.iter
            i.sections
            ~f:(fun
                { Js_of_ocaml_compiler.Source_map.Index.offset = { gen_line; gen_column }
                ; map
                }
              -> print_mapping ~line_offset:gen_line ~col_offset:gen_column map));
  [%expect
    {|
      $ cat "test.ml"
        1: let id x = x
      $ cat "test.js"
        1:
        2: //# unitInfo: Provides: Test
        3: (function(globalThis){
        4:    "use strict";
        5:    var runtime = globalThis.jsoo_runtime;
        6:    function id(x){return x;}
        7:    var Test = [0, id];
        8:    runtime.caml_register_global(0, Test, "Test");
        9:    return;
       10:   }
       11:   (globalThis));
       12:
       13: //# sourceMappingURL=test.map
      /builtin/blackbox.ml:1:0 -> 5:7
      /builtin/blackbox.ml:1:0 -> 5:17
      /builtin/blackbox.ml:1:0 -> 6:0
      /builtin/blackbox.ml:1:0 -> 6:12
      /builtin/blackbox.ml:1:0 -> 6:15
      /dune-root/test.ml:1:11 -> 6:18
      /dune-root/test.ml:1:12 -> 6:27
      /dune-root/test.ml:1:12 -> 7:0
      /dune-root/test.ml:1:12 -> 7:7
      /builtin/blackbox.ml:1:0 -> 7:14
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
      1: function x(a, b){
      2:   /*<<test.ml:3:2>>*/ return a + b /*<<test.ml:3:14>>*/ ;
      3:  /*<<test.ml:4:0>>*/ }
 |}]

let%expect_test _ =
  let map_str = ";;;;EAEE,EAAE,EAAC,CAAE;ECQY,UACC" in
  let map = Source_map.Mappings.(decode_exn (of_string_unsafe map_str)) in
  let map_str' = Source_map.Mappings.(to_string (encode map)) in
  print_endline map_str;
  print_endline map_str';
  [%expect
    {|
    ;;;;EAEE,EAAE,EAAC,CAAE;ECQY,UACC
    ;;;;EAEE,EAAE,EAAC,CAAE;ECQY,UACC |}]

let%expect_test _ =
  let gen (gen_line, gen_col) (line, col) source : Source_map.map =
    Source_map.Gen_Ori
      { gen_line; gen_col; ori_source = source; ori_line = line; ori_col = col }
  in
  let s1 : Source_map.Standard.t =
    { (Source_map.Standard.empty ~inline_source_content:false) with
      names = [ "na"; "nb"; "nc" ]
    ; sources = [ "sa"; "sb" ]
    ; mappings =
        Source_map.Mappings.encode [ gen (1, 1) (10, 10) 0; gen (3, 3) (20, 20) 1 ]
    }
  in
  let s2 : Source_map.Standard.t =
    { (Source_map.Standard.empty ~inline_source_content:false) with
      names = [ "na2"; "nb2" ]
    ; sources = [ "sa2" ]
    ; mappings = Source_map.Mappings.encode [ gen (3, 3) (5, 5) 0 ]
    }
  in
  let m =
    Source_map.Standard.merge
      [ s1; Source_map.Standard.filter_map s2 ~f:(fun x -> Some (x + 20)) ]
  in
  (match m with
  | None -> ()
  | Some sm ->
      let encoded_mappings = sm.Source_map.Standard.mappings in
      print_endline (Source_map.Mappings.to_string encoded_mappings);
      print_mapping ~line_offset:0 ~col_offset:0 sm);
  [%expect
    {|
    CASU;;GCUU;;;;;;;;;;;;;;;;;;;;GCff
    sa:10:10 -> 1:1
    sb:20:20 -> 3:3
    sa2:5:5 -> 23:3
    |}]
