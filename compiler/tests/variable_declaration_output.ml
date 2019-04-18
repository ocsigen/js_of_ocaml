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
    |> Format.ocaml_source_of_string
    |> Format.write_ocaml
    |> Util.compile_ocaml_to_cmo
    |> Util.compile_cmo_to_javascript ~pretty:true
    |> Util.parse_js
  in
  let program =
    compile
      {|
    let lr = ref (List.init 2 Obj.repr)
    let black_box v = lr := (Obj.repr v) :: !lr

    type r = {x: int; y: string}

    let ex = {x = 5; y = "hello"} ;;
    black_box ex
    let ax = [|1;2;3;4|] ;;
    black_box ax
    let bx = [|1.0;2.0;3.0;4.0|] ;;
    black_box bx ;;

    (* combined with the black_box function above, this
       will prevent the ocaml compiler from optimizing
       away the constructions. *)
    print_int ((List.length !lr) + (List.length !lr))
  |}
  in
  print_var_decl program "ex";
  print_var_decl program "ax";
  print_var_decl program "bx";
  [%expect
    {|
    var ex = [0,5,caml_new_string("hello")];
    var ax = [0,1,2,3,4];
    var bx = [254,1.,2.,3.,4.]; |}]
