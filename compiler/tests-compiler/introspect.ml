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

(* Introcaml only: blocks carrying a descriptor are allocated as instances of
   an Array subclass holding the descriptor index, one class per descriptor,
   shared across the compilation unit. *)
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
    |}
  in
  print_fun_decl program (Some "mk_point");
  print_fun_decl program (Some "mk_pair");
  print_fun_decl program (Some "mk_option");
  print_fun_decl program (Some "mk_ref");
  print_fun_decl program (Some "mk_float_array");
  print_fun_decl program (Some "mk_list");
  [%expect
    {|
           function mk_point(x, y){return new desc_18f60e(0, x, y);}
           //end
           function mk_pair(x, y){return new desc_2b23e6(0, x, y);}
           //end
           function mk_option(x){return new desc_c189a(0, x);}
           //end
           function mk_ref(x){return new desc_1d76a5(0, x);}
           //end
           function mk_float_array(x){return new desc_2eed5d(0, x, x);}
           //end
           function mk_list(x){return new desc_26ea38(0, x, new desc_26ea38(0, x, 0));}
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
