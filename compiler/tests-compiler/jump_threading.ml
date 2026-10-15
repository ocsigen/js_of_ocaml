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

(* A boolean computed on several paths and then tested: each path with a
   known value branches directly to the corresponding target. *)

let prog =
  {|
type t = A of int | B | C of int | D
let f x y =
  let special = match x with A _ | C _ -> true | B | D -> false in
  if special then y + 1 else y - 1
|}

let%expect_test "jump threading" =
  let program = Util.compile_and_parse prog in
  Util.print_fun_decl program (Some "f");
  [%expect
    {|
    function f(x, y){return typeof x === "number" ? y - 1 | 0 : y + 1 | 0;}
    //end
    |}]

let%expect_test "semantics" =
  Util.compile_and_run
    (prog
    ^ {|
let () =
  List.iter (fun x -> Printf.printf "%d " (f x 10)) [A 1; B; C 2; D];
  print_newline ()
|}
    );
  [%expect {| 11 9 11 9 |}]
