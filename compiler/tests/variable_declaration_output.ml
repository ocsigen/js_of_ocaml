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
  let compile s =
    s
    |> Filetype.ocaml_text_of_string
    |> Filetype.write_ocaml
    |> compile_ocaml_to_cmo
    |> compile_cmo_to_javascript ~pretty:true
    |> fst
    |> parse_js
  in
  let program =
    compile
      {|
    type r = {x: int; y: string}
    let ex = {x = 5; y = "hello"} ;;
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
  print_var_decl program "ax";
  print_var_decl program "bx";
  print_var_decl program "cx";
  print_var_decl program "symbol_op";
  [%expect
    {|
    var ex = [0,5,runtime.caml_new_string("hello")];
    var ax = [0,1,2,3,4];
    var bx = [254,1.,2.,3.,4.];
    var cx = [254,NaN,NaN,Infinity,- Infinity,0.,- 0.];
    var symbol_op = [0,symbol_bind,symbol_map,symbol]; |}]

let%expect_test _ =
  let compile ~enable s =
    let enable_disable = if enable then "--enable" else "--disable" in
    s
    |> Filetype.ocaml_text_of_string
    |> Filetype.write_ocaml
    |> compile_ocaml_to_cmo
    |> compile_cmo_to_javascript ~pretty:true ~flags:[ enable_disable; "vardecl" ]
    |> fst
    |> parse_js
  in
  let program ~enable =
    compile
      ~enable
      {|
    let match_expr = function
      | [] | [None] | _ :: None :: _ -> 1
      | _ -> 2
    |}
  in
  print_fun_decl (program ~enable:true) (Some "match_expr");
  [%expect
    {|
    function match_expr(param)
     {var switch$1,switch$0,_a_;
      if(param)
       {switch$0 = param[1]?0:param[2]?0:1;
        if(! switch$0)
         {_a_ = param[2];switch$1 = _a_?_a_[1]?0:1:0;if(! switch$1)return 2}}
      return 1} |}];
  print_fun_decl (program ~enable:false) (Some "match_expr");
  [%expect
    {|
    function match_expr(param)
     {if(param)
       {var switch$0=param[1]?0:param[2]?0:1;
        if(! switch$0)
         {var _a_=param[2],switch$1=_a_?_a_[1]?0:1:0;if(! switch$1)return 2}}
      return 1} |}]
