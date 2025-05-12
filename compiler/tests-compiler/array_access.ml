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

let array_set =
  {|
   let some_name a n =
     let x = a.(n) <- n in
     x = ()
   let a = [|1;2;3|]
   let () = assert (some_name a 2)
   |}

let%expect_test "array_set" =
  let program = compile_and_parse array_set in
  print_fun_decl program (Some "some_name");
  [%expect
    {|
    function some_name(a, n){runtime.caml_check_bound(a, n)[n + 1] = n; return 1;}
    //end
    |}]

let%expect_test "array_set" =
  compile_and_run array_set;
  [%expect {| |}]
