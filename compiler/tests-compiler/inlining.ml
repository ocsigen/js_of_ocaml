(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2019 Hugo Heuzard
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

let%expect_test "inline recursive function" =
  let program = compile_and_parse {|
    let rec f () = f ()
    and g () = f ()
  |} in
  print_fun_decl program (Some "f");
  print_fun_decl program (Some "g");
  [%expect
    {|
    function f(param){for(;;) ;}
    //end
    function g(param){for(;;) ;}
    //end |}]

let%expect_test "inline small function exposing more tc" =
  let program =
    compile_and_parse
      {|
    let ( >>= ) x f = match x with `Ok v -> f v | `Error _ as e -> e

    let f g x =
      x >>= fun x ->
      g x >>= fun y ->
      y
  |}
  in
  print_fun_decl program (Some "f");
  print_fun_decl program (Some "g");
  [%expect
    {|
    function f(g, x){
     var variant = x[1];
     if(106380200 <= variant) return x;
     var v = x[2], x$0 = caml_call1(g, v), variant$0 = x$0[1];
     if(106380200 <= variant$0) return x$0;
     var v$0 = x$0[2];
     return v$0;
    }
    //end
    not found
    |}]

(* When inline_recursively inlines a function passed as argument,
   the actual argument is still referenced in block arguments.
   Without forced duplication, the closure's params would conflict
   with the intermediate block's params. *)
let%expect_test "inline_recursively must duplicate closure" =
  let program =
    compile_and_parse
      ~flags:[ "--debug"; "invariant" ]
      {|
    let hash_fold_int acc x = 7 * acc + x
    let as_int f s x = hash_fold_int s (f x)
    let hash_fold_char = as_int Char.code
    let hash_char x = hash_fold_char 0 x
  |}
  in
  print_fun_decl program (Some "hash_char");
  [%expect {|
    function hash_char(x){return hash_fold_int(0, x);}
    //end
    |}]
