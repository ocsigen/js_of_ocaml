(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2019 Ty Overby
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

let%expect_test _ =
  let program =
    compile_and_parse
      ~use_js_string:true
      {|
    type js_string
    external js_string : string -> js_string = "caml_jsstring_of_string"
    type r = {x: int; y: string; z : string}
    let ex = {x = 5; y = "hello"; z = "the • and › characters"} ;;
    let sx = js_string "hello2", js_string "the • and › characters (2)"
    let ax = [|1;2;3;4|] ;;
    let bx = [|1.0;2.0;3.0;4.0|] ;;
    let cx = [|0./.0.;-0./.0.;1./.0.;-1./.0.;0.;-0.|] ;;

    let (>>=) a b = a * b
    let (>>|) a b = a + b
    let (>>?=) a b = a / b
    let symbol_op = (>>=), (>>|), (>>?=);;
    |}
  in
  print_var_decl program "ex";
  print_var_decl program "sx";
  print_var_decl program "ax";
  print_var_decl program "bx";
  print_var_decl program "cx";
  print_var_decl program "symbol_op";
  [%expect
    {|
    var ex = [0, 5, "hello", "the \xe2\x80\xa2 and \xe2\x80\xba characters"];
    //end
    var sx = [0, "hello2", "the • and › characters (2)"];
    //end
    var ax = [0, 1, 2, 3, 4];
    //end
    var bx = [254, 1., 2., 3., 4.];
    //end
    var cx = [254, NaN, NaN, Infinity, - Infinity, 0., - 0.];
    //end
    var symbol_op = [0, symbol_bind, symbol_map, symbol];
    //end |}]

let%expect_test _ =
  let program =
    compile_and_parse
      ~use_js_string:false
      {|
    type js_string
    external js_string : string -> js_string = "caml_jsstring_of_string"
    type r = {x: int; y: string; z : string}
    let ex = {x = 5; y = "hello"; z = "the • and › characters"} ;;
    let sx = "hello2", js_string "the • and › characters (2)"
    let ax = [|1;2;3;4|] ;;
    let bx = [|1.0;2.0;3.0;4.0|] ;;
    let cx = [|0./.0.;-0./.0.;1./.0.;-1./.0.;0.;-0.|] ;;
    let (>>=) a b = a * b
    let (>>|) a b = a + b
    let (>>?=) a b = a / b
    let symbol_op = (>>=), (>>|), (>>?=);;
    |}
  in
  print_var_decl program "ex";
  print_var_decl program "sx";
  print_var_decl program "ax";
  print_var_decl program "bx";
  print_var_decl program "cx";
  print_var_decl program "symbol_op";
  [%expect
    {|
    var ex = [0,
     5,
     caml_string_of_jsbytes("hello"),
     caml_string_of_jsbytes("the \xe2\x80\xa2 and \xe2\x80\xba characters")];
    //end
    var sx = [0, caml_string_of_jsbytes("hello2"), "the • and › characters (2)"];
    //end
    var ax = [0, 1, 2, 3, 4];
    //end
    var bx = [254, 1., 2., 3., 4.];
    //end
    var cx = [254, NaN, NaN, Infinity, - Infinity, 0., - 0.];
    //end
    var symbol_op = [0, symbol_bind, symbol_map, symbol];
    //end |}]

let%expect_test _ =
  let compile ~enable s =
    let enable_disable = if enable then "--enable" else "--disable" in
    let flags = [ enable_disable; "vardecl" ] in
    compile_and_parse ~flags s
  in
  let program ~enable =
    compile
      ~enable
      {|
    let match_expr = function
      | [] | [None] | _ :: None :: _ -> 1
      | [ Some None ] -> 2
      | [ Some (Some 2) ] -> 3
      | _ -> 4
    |}
  in
  with_temp_dir ~f:(fun () -> print_fun_decl (program ~enable:true) (Some "match_expr"));
  [%expect
    {|
    function match_expr(param){
     var _c_, _b_, _a_;
     a:
     if(param){
      _a_ = param[1];
      if(_a_){
       _b_ = _a_[1];
       if(_b_){
        if(2 === _b_[1] && ! param[2]) return 3;
       }
       else if(! param[2]) return 2;
      }
      else if(! param[2]) break a;
      _c_ = param[2];
      if(_c_ && ! _c_[1]) break a;
      return 4;
     }
     return 1;
    }
    //end
    |}];
  with_temp_dir ~f:(fun () -> print_fun_decl (program ~enable:false) (Some "match_expr"));
  [%expect
    {|
    function match_expr(param){
     a:
     if(param){
      var _a_ = param[1];
      if(_a_){
       var _b_ = _a_[1];
       if(_b_){
        if(2 === _b_[1] && ! param[2]) return 3;
       }
       else if(! param[2]) return 2;
      }
      else if(! param[2]) break a;
      var _c_ = param[2];
      if(_c_ && ! _c_[1]) break a;
      return 4;
     }
     return 1;
    }
    //end
    |}]
