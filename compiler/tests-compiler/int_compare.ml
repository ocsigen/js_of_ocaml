(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2026 Jérôme Vouillon
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

(* Comparing the result of [compare] on integers with 0 is a direct
   comparison of the integers. *)

let%expect_test "compare with 0" =
  let prog =
    {|
let lt (x : int) y = compare x y < 0
let le (x : int) y = compare x y <= 0
let gt (x : int) y = compare x y > 0
let ge (x : int) y = compare x y >= 0
let eq (x : int) y = compare x y = 0
let ne (x : int) y = compare x y <> 0
let lt' (x : int) y = 0 < compare x y
let le' (x : int) y = 0 <= compare x y
let eq' (x : int) y = 0 = compare x y
let l = [ min_int; -1; 0; 1; max_int ]
let () =
  List.iter
    (fun (name, f) ->
      print_string name;
      List.iter (fun x -> List.iter (fun y -> print_char (if f x y then '1' else '0')) l) l;
      print_newline ())
    [ "lt ", lt; "le ", le; "gt ", gt; "ge ", ge; "eq ", eq; "ne ", ne;
      "lt'", lt'; "le'", le'; "eq'", eq' ]
|}
  in
  Util.compile_and_run prog;
  [%expect
    {|
    lt 0111100111000110000100000
    le 1111101111001110001100001
    gt 0000010000110001110011110
    ge 1000011000111001111011111
    eq 1000001000001000001000001
    ne 0111110111110111110111110
    lt'0000010000110001110011110
    le'1000011000111001111011111
    eq'1000001000001000001000001
    |}];
  let program = Util.compile_and_parse prog in
  List.iter
    (fun f -> Util.print_fun_decl program (Some f))
    [ "lt"; "le"; "gt"; "ge"; "eq"; "ne" ];
  [%expect
    {|
           function lt(x, y){return x < y ? 1 : 0;}
           //end
           function le(x, y){return x <= y ? 1 : 0;}
           //end
           function gt(x, y){return y < x ? 1 : 0;}
           //end
           function ge(x, y){return y <= x ? 1 : 0;}
           //end
           function eq(x, y){return x === y ? 1 : 0;}
           //end
           function ne(x, y){return x !== y ? 1 : 0;}
           //end
           |}]
