(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2026 Hugo Heuzard
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

open! Util

(* Introcaml only: the descriptor of a block is stored in its header word,
   above the tag ([tag | (desc << 8)]), and tag reads mask it off. *)
[@@@if introspect]

let%expect_test "blocks with a descriptor" =
  let program =
    compile_and_parse
      ~flags:[ "--enable=introspection" ]
      {|
      type point = { x : int; y : int }
      let mk_point x y = { x; y }
      let mk_pair x y = (x, y)
      let mk_option x = Some x
      let mk_ref x = ref x
      let mk_float_array x = [| x; x |]
      let mk_list x = [ x; x ]
      type t = A of int | B of int * int | C of int
      let tag_of x = match x with A n -> n | B (n, _) -> n | C n -> n + 1
      let is_some x = match x with Some _ -> true | None -> false
    |}
  in
  print_fun_decl program (Some "mk_point");
  print_fun_decl program (Some "mk_pair");
  print_fun_decl program (Some "mk_option");
  print_fun_decl program (Some "mk_ref");
  print_fun_decl program (Some "mk_float_array");
  print_fun_decl program (Some "mk_list");
  print_fun_decl program (Some "tag_of");
  print_fun_decl program (Some "is_some");
  [%expect
    {|
    function mk_point(x, y){return [418778624, x, y];}
    //end
    function mk_pair(x, y){return [723772928, x, y];}
    //end
    function mk_option(x){return [202938880, x];}
    //end
    function mk_ref(x){return [494314752, x];}
    //end
    function mk_float_array(x){return [787307776, x, x];}
    //end
    function mk_list(x){return [652883968, x, [652883968, x, 0]];}
    //end
    function tag_of(x){
     if(2 === (x[0] & 255)){var n$0 = x[1]; return n$0 + 1 | 0;}
     var n = x[1];
     return n;
    }
    //end
    function is_some(x){return x ? 1 : 0;}
    //end
    |}]

let%expect_test "blocks without a descriptor" =
  let program =
    compile_and_parse
      {|
      type point = { x : int; y : int }
      let mk_point x y = { x; y }
      let mk_option x = Some x
    |}
  in
  print_fun_decl program (Some "mk_point");
  print_fun_decl program (Some "mk_option");
  [%expect
    {|
           function mk_point(x, y){return [0, x, y];}
           //end
           function mk_option(x){return [0, x];}
           //end
           |}]
