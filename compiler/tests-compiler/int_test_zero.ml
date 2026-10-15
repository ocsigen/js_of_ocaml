(* Js_of_ocaml tests
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
 * along with this program; if not, should be able to obtain it from
 * https://www.gnu.org/licenses/.
 *)

(* An integer comparison with 0 whose result is only used as a condition
   is compiled to a truthiness test: [0 === x] becomes [!x] and
   [0 !== x] becomes [x]. Tests on the tag of a block are such
   comparisons. *)

let prog =
  {|
type t = A of int | B of int
let f = function A x -> x | B x -> - x
let g a b = match a, b with A x, A y -> x + y | B x, B y -> x - y | _ -> 0
let h = function A _ -> true | B _ -> false
|}

let%expect_test "tests on block tags" =
  let program = Util.compile_and_parse prog in
  Util.print_fun_decl program (Some "f");
  Util.print_fun_decl program (Some "g");
  Util.print_fun_decl program (Some "h");
  [%expect
    {|
    function f(param){
     if(! param[0]){var x = param[1]; return x;}
     var x$0 = param[1];
     return - x$0 | 0;
    }
    //end
    function g(a, b){
     if(! a[0]){
      var x = a[1];
      if(! b[0]){var y = b[1]; return x + y | 0;}
     }
     else{var x$0 = a[1]; if(b[0]){var y$0 = b[1]; return x$0 - y$0 | 0;}}
     return 0;
    }
    //end
    function h(param){return ! param[0] ? 1 : 0;}
    //end
    |}]

let%expect_test "semantics" =
  Util.compile_and_run
    (prog
    ^ {|
let () =
  List.iter (fun v -> Printf.printf "%d " (f v)) [A 3; B 4; A 0; B 0];
  List.iter (fun (a, b) -> Printf.printf "%d " (g a b)) [A 1, A 2; B 5, B 2; A 1, B 2; B 1, A 2];
  Printf.printf "%d\n" (List.length (List.filter h [A 1; B 2; A 3]))
|}
    );
  [%expect {| 3 -4 0 0 3 3 0 0 2 |}]
