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
      3: //# shape: Test:[F(1)*]
      4: (function(globalThis){
      5:    "use strict";
      6:    var runtime = globalThis.jsoo_runtime;
      7:    function id(x){return x;}
      8:    runtime.caml_register_global([0, id], "Test");
      9:    return;
     10:   }
     11:   (globalThis));
     12:
     13: //# sourceMappingURL=test.map
    /builtin/blackbox.ml:1:0 -> 6:7
    /builtin/blackbox.ml:1:0 -> 6:17
    /builtin/blackbox.ml:1:0 -> 7:0
    /builtin/blackbox.ml:1:0 -> 7:12
    /builtin/blackbox.ml:1:0 -> 7:15
    /dune-root/test.ml:1:11 -> 7:18
    /dune-root/test.ml:1:12 -> 7:27
    /dune-root/test.ml:1:12 -> 8:0
    /builtin/blackbox.ml:1:0 -> 8:3
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

(* [Index.to_standard] flattens an index map into a single standard map,
   concatenating sources/names and stitching the encoded mappings together. *)
let print_standard (sm : Source_map.Standard.t) =
  let sources = Array.of_list sm.sources in
  let names = Array.of_list sm.names in
  Printf.printf "sources: %s\n" (String.concat ~sep:"," sm.sources);
  Printf.printf "names: %s\n" (String.concat ~sep:"," sm.names);
  Printf.printf "ignore_list: %s\n" (String.concat ~sep:"," sm.ignore_list);
  Printf.printf
    "sources_content: %s\n"
    (match sm.sources_content with
    | None -> "none"
    | Some l -> Printf.sprintf "%d entries" (List.length l));
  print_endline (Source_map.Mappings.to_string sm.mappings);
  List.iter (Source_map.Mappings.decode_exn sm.mappings) ~f:(fun (m : Source_map.map) ->
      match m with
      | Gen { gen_line; gen_col } -> Printf.printf "null -> %d:%d\n" gen_line gen_col
      | Gen_Ori { gen_line; gen_col; ori_source; ori_line; ori_col } ->
          Printf.printf
            "%s:%d:%d -> %d:%d\n"
            sources.(ori_source)
            ori_line
            ori_col
            gen_line
            gen_col
      | Gen_Ori_Name { gen_line; gen_col; ori_source; ori_line; ori_col; ori_name } ->
          Printf.printf
            "%s:%d:%d(%s) -> %d:%d\n"
            sources.(ori_source)
            ori_line
            ori_col
            names.(ori_name)
            gen_line
            gen_col)

let%expect_test "index to standard (line offsets)" =
  let gen_ori (gen_line, gen_col) (ori_line, ori_col) ori_source : Source_map.map =
    Gen_Ori { gen_line; gen_col; ori_source; ori_line; ori_col }
  in
  let gen_ori_name (gen_line, gen_col) (ori_line, ori_col) ori_source ori_name :
      Source_map.map =
    Gen_Ori_Name { gen_line; gen_col; ori_source; ori_line; ori_col; ori_name }
  in
  let section (gen_line, gen_column) sources names mappings : Source_map.Index.section =
    { offset = { gen_line; gen_column }
    ; map =
        { (Source_map.Standard.empty ~inline_source_content:false) with
          sources
        ; names
        ; mappings = Source_map.Mappings.encode mappings
        }
    }
  in
  let idx : Source_map.Index.t =
    { version = 3
    ; file = Some "out.js"
    ; sections =
        [ section
            (0, 0)
            [ "a.ml"; "b.ml" ]
            [ "x" ]
            [ gen_ori (1, 0) (10, 4) 0; gen_ori_name (2, 2) (20, 6) 1 0 ]
        ; section
            (10, 0)
            [ "c.ml" ]
            [ "y"; "z" ]
            [ gen_ori (1, 0) (5, 0) 0; gen_ori_name (1, 8) (6, 1) 0 1 ]
        ]
    }
  in
  print_standard (Source_map.Index.to_standard idx);
  [%expect
    {|
    sources: a.ml,b.ml,c.ml
    names: x,y,z
    ignore_list:
    sources_content: none
    AASI;ECUEA;;;;;;;;;ACfN,QACCE
    a.ml:10:4 -> 1:0
    b.ml:20:6(x) -> 2:2
    c.ml:5:0 -> 11:0
    c.ml:6:1(z) -> 11:8
    |}]

(* A section whose first name appears only after several origin-only segments
   spread over several lines: the blitted tail then crosses line boundaries and
   carries origin deltas, exercising the prefix/blit split. *)
let%expect_test "index to standard (blitted tail)" =
  let gen_ori (gen_line, gen_col) (ori_line, ori_col) ori_source : Source_map.map =
    Gen_Ori { gen_line; gen_col; ori_source; ori_line; ori_col }
  in
  let gen_ori_name (gen_line, gen_col) (ori_line, ori_col) ori_source ori_name :
      Source_map.map =
    Gen_Ori_Name { gen_line; gen_col; ori_source; ori_line; ori_col; ori_name }
  in
  let section (gen_line, gen_column) sources names mappings : Source_map.Index.section =
    { offset = { gen_line; gen_column }
    ; map =
        { (Source_map.Standard.empty ~inline_source_content:false) with
          sources
        ; names
        ; mappings = Source_map.Mappings.encode mappings
        }
    }
  in
  let idx : Source_map.Index.t =
    { version = 3
    ; file = None
    ; sections =
        [ section (0, 0) [ "a.ml" ] [ "n0" ] [ gen_ori (1, 0) (1, 0) 0 ]
        ; section
            (5, 0)
            [ "b.ml"; "c.ml" ]
            [ "n1"; "n2" ]
            [ gen_ori (1, 0) (10, 0) 0
            ; gen_ori (2, 2) (11, 4) 1
            ; gen_ori (3, 0) (12, 0) 0
            ; gen_ori_name (3, 4) (12, 8) 1 0
            ; gen_ori_name (4, 0) (20, 1) 1 1
            ]
        ]
    }
  in
  print_standard (Source_map.Index.to_standard idx);
  [%expect
    {|
    sources: a.ml,b.ml,c.ml
    names: n0,n1,n2
    ignore_list:
    sources_content: none
    AAAA;;;;;ACSA;ECCI;ADCJ,ICAQC;AAQPC
    a.ml:1:0 -> 1:0
    b.ml:10:0 -> 6:0
    c.ml:11:4 -> 7:2
    b.ml:12:0 -> 8:0
    c.ml:12:8(n1) -> 8:4
    c.ml:20:1(n2) -> 9:0
    |}]

(* Sections stacked on a single generated line via column offsets, as produced
   by [Wasm_source_map.concatenate]. *)
let%expect_test "index to standard (column offsets)" =
  let gen_ori (gen_line, gen_col) (ori_line, ori_col) ori_source : Source_map.map =
    Gen_Ori { gen_line; gen_col; ori_source; ori_line; ori_col }
  in
  let section (gen_line, gen_column) sources mappings : Source_map.Index.section =
    { offset = { gen_line; gen_column }
    ; map =
        { (Source_map.Standard.empty ~inline_source_content:false) with
          sources
        ; mappings = Source_map.Mappings.encode mappings
        }
    }
  in
  let idx : Source_map.Index.t =
    { version = 3
    ; file = None
    ; sections =
        [ section (0, 0) [ "a.ml" ] [ gen_ori (1, 0) (1, 0) 0; gen_ori (1, 4) (1, 7) 0 ]
        ; section (0, 100) [ "b.ml" ] [ gen_ori (1, 0) (3, 2) 0; gen_ori (1, 8) (3, 9) 0 ]
        ]
    }
  in
  print_standard (Source_map.Index.to_standard idx);
  [%expect
    {|
    sources: a.ml,b.ml
    names:
    ignore_list:
    sources_content: none
    AAAA,IAAO,gGCEL,QAAO
    a.ml:1:0 -> 1:0
    a.ml:1:7 -> 1:4
    b.ml:3:2 -> 1:100
    b.ml:3:9 -> 1:108
    |}]
