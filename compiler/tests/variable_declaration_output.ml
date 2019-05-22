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
    |}
  in
  print_var_decl program "ex";
  print_var_decl program "ax";
  print_var_decl program "bx";
  print_var_decl program "cx";
  [%expect
    {|
    var ex = [0,5,runtime.caml_new_string("hello")];
    var ax = [0,1,2,3,4];
    var bx = [254,1.,2.,3.,4.];
    var cx = [254,- nan,- nan,Infinity,- Infinity,0.,- 0.]; |}]
