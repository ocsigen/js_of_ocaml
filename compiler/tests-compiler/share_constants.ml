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

(* Structured constants are immutable: identical ones occurring several
   times are bound once to a variable. *)

let prog =
  {|
let f x = Printf.sprintf "<%d>" x
let g x = Printf.sprintf "<%d>" (x + 1)
let l1 () = [1; 2; 3]
let l2 () = [1; 2; 3]
|}

let%expect_test "shared constants" =
  let program = Util.compile_and_parse prog in
  (* The list and the format, each used twice *)
  Util.print_var_decl program "cst_block";
  Util.print_var_decl program "cst_block$0";
  [%expect
    {|
           var cst_block = [0, 1, [0, 2, [0, 3, 0]]];
           //end
           var cst_block$0 = [0, [12, 60, [4, 0, 0, 0, [12, 62, 0]]], caml_string_of_jsbytes("<%d>")];
           //end
           |}]

let%expect_test "semantics" =
  Util.compile_and_run
    (prog
    ^ {|
let () =
  print_endline (f 1 ^ g 1);
  Printf.printf "%b %d\n" (l1 () = l2 ()) (List.length (l1 ()))
|}
    );
  [%expect {|
           <1><2>
           true 3
           |}]
