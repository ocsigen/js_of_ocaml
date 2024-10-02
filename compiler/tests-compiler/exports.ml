(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2022 Hugo Heuzard
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

open Util

let%expect_test "static eval of string get" =
  let use_jsoo_exports st =
    let open Js_of_ocaml_compiler in
    let traverse = new Js_traverse.free in
    let _ = traverse#program [ st ] in
    let jsoo_exports =
      Javascript.ident (Stdlib.Utf8_string.of_string_exn "jsoo_exports")
    in
    Javascript.IdentSet.mem jsoo_exports traverse#get_use
    || Javascript.IdentSet.mem jsoo_exports traverse#get_def
  in
  let clean program =
    let rec clean_statement st =
      let open Js_of_ocaml_compiler.Javascript in
      match st with
      | Function_declaration (name, (k, param, body, loc1)), loc2 -> (
          match List.filter use_jsoo_exports body with
          | [] -> None
          | body ->
              let body = List.filter_map clean_statement body in
              Some (Function_declaration (name, (k, param, body, loc1)), loc2))
      | ( Expression_statement (ECall (EFun (name, (k, param, body, loc1)), ANormal, a, l))
        , loc ) -> (
          match List.filter use_jsoo_exports body with
          | [] -> None
          | body ->
              let body = List.filter_map clean_statement body in
              Some
                ( Expression_statement
                    (ECall (EFun (name, (k, param, body, loc1)), ANormal, a, l))
                , loc ))
      | _, _ -> Some st
    in
    List.filter_map clean_statement program
  in
  let program =
    compile_and_parse_whole_program
      ~flags:
        [ "--wrap-with-fun"
        ; "Loader"
        ; "--target-env"
        ; "browser"
        ; "--no-extern-fs"
        ; "--enable"
        ; "vardecl"
        ]
      {|
      external pure_js_expr : string -> 'a = "caml_pure_js_expr"
      external set : 'a -> 'b -> 'c -> unit = "caml_js_set"
      let x = 3
      let () = set (pure_js_expr "jsoo_exports") (pure_js_expr "'x'") x  |}
  in
  print_program (clean program);
  [%expect
    {|
    function Loader(globalThis){
     var jsoo_exports = {};
     jsoo_exports["x"] = 3;
     return jsoo_exports;
    }
    if(typeof module === "object" && module.exports) module["exports"] = Loader;
    //end |}];
  let program =
    compile_and_parse_whole_program
      ~flags:
        [ "--wrap-with-fun"
        ; "Loader"
        ; "--target-env"
        ; "browser"
        ; "--no-extern-fs"
        ; "--enable"
        ; "vardecl"
        ]
      {|
      external pure_js_expr : string -> 'a = "caml_pure_js_expr"
      external set : 'a -> 'b -> 'c -> unit = "caml_js_set"
      let x = 3
      let () = if false then set (pure_js_expr "jsoo_exports") (pure_js_expr "'x'") x  |}
  in
  print_program (clean program);
  [%expect
    {|
    function Loader(globalThis){var jsoo_exports = {}; return jsoo_exports;}
    if(typeof module === "object" && module.exports) module["exports"] = Loader;
    //end |}];
  let program =
    compile_and_parse_whole_program
      ~flags:[ "--target-env"; "browser"; "--no-extern-fs"; "--enable"; "vardecl" ]
      {|
      external pure_js_expr : string -> 'a = "caml_pure_js_expr"
      external set : 'a -> 'b -> 'c -> unit = "caml_js_set"
      let x = 3
      let () = set (pure_js_expr "jsoo_exports") (pure_js_expr "'x'") x  |}
  in
  print_program (clean program);
  [%expect
    {|
    (function(globalThis){
       var
        jsoo_exports = typeof module === "object" && module.exports || globalThis;
       jsoo_exports["x"] = 3;
      }
      (globalThis));
    //end |}];
  let program =
    compile_and_parse_whole_program
      ~flags:[ "--target-env"; "browser"; "--no-extern-fs"; "--enable"; "vardecl" ]
      {|
      external pure_js_expr : string -> 'a = "caml_pure_js_expr"
      external set : 'a -> 'b -> 'c -> unit = "caml_js_set"
      let x = 3
      let () = if false then set (pure_js_expr "jsoo_exports") (pure_js_expr "'x'") x  |}
  in
  print_program (clean program);
  [%expect {|
    //end |}]
