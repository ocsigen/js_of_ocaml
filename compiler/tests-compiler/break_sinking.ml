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

(* The redundant [break]s of a labelled block are dropped by sinking the
   statements that follow each conditional exit into its else-branch. *)

let%expect_test "a few exits are sunk" =
  let prog =
    {|
let n = ref 0
let f x y =
  let r =
    if x = 0 && y > 0 then 0 else begin incr n;
    if x = 1 && y > 1 then 7 else begin incr n;
    if x = 2 && y > 2 then 14 else begin incr n;
    0 end end end
  in
  r + 1
let () = Printf.printf "%d %d %d %d\n" (f 0 1) (f 1 2) (f 2 3) (f 3 4)
let () = Printf.printf "%d\n" !n
|}
  in
  Util.compile_and_run prog;
  [%expect {|
    1 8 15 1
    6 |}];
  let program = Util.compile_and_parse prog in
  Util.print_fun_decl program (Some "f");
  [%expect
    {|
    function f(x, y){
     var
      r =
        0 === x && 0 < y
         ? 0
         : (n
            [1]++,
           1 === x && 1 < y ? 7 : (n[1]++, 2 === x && 2 < y ? 14 : (n[1]++, 0)));
     return r + 1 | 0;
    }
    //end
    |}]

(* Each sunk exit nests the rest of the block one level deeper. Above
   [break_sink_depth] exits, the labelled block is kept as is, so that the
   generated code does not nest arbitrarily deep. *)
let%expect_test "many exits keep the labelled block" =
  let prog =
    {|
let n = ref 0
let f x y =
  let r =
    if x = 0 && y > 0 then 0 else begin incr n;
    if x = 1 && y > 1 then 1 else begin incr n;
    if x = 2 && y > 2 then 2 else begin incr n;
    if x = 3 && y > 3 then 3 else begin incr n;
    if x = 4 && y > 4 then 4 else begin incr n;
    if x = 5 && y > 5 then 5 else begin incr n;
    if x = 6 && y > 6 then 6 else begin incr n;
    if x = 7 && y > 7 then 7 else begin incr n;
    if x = 8 && y > 8 then 8 else begin incr n;
    if x = 9 && y > 9 then 9 else begin incr n;
    if x = 10 && y > 10 then 10 else begin incr n;
    if x = 11 && y > 11 then 11 else begin incr n;
    12 end end end end end end end end end end end end
  in
  r + 1
let () = Printf.printf "%d %d %d %d\n" (f 0 1) (f 5 6) (f 11 12) (f 12 13)
let () = Printf.printf "%d\n" !n
|}
  in
  Util.compile_and_run prog;
  [%expect {|
    1 6 12 13
    28 |}];
  let program = Util.compile_and_parse prog in
  Util.print_fun_decl program (Some "f");
  [%expect
    {|
    function f(x, y){
     a:
     {
      if(0 === x && 0 < y){var r = 0; break a;}
      n[1]++;
      if(1 === x && 1 < y){r = 1; break a;}
      n[1]++;
      if(2 === x && 2 < y){r = 2; break a;}
      n[1]++;
      if(3 === x && 3 < y){r = 3; break a;}
      n[1]++;
      if(4 === x && 4 < y){r = 4; break a;}
      n[1]++;
      if(5 === x && 5 < y){r = 5; break a;}
      n[1]++;
      if(6 === x && 6 < y){r = 6; break a;}
      n[1]++;
      if(7 === x && 7 < y){r = 7; break a;}
      n[1]++;
      if(8 === x && 8 < y){r = 8; break a;}
      n[1]++;
      if(9 === x && 9 < y){r = 9; break a;}
      n[1]++;
      if(10 === x && 10 < y){r = 10; break a;}
      n[1]++;
      if(11 === x && 11 < y){r = 11; break a;}
      n[1]++;
      r = 12;
     }
     return r + 1 | 0;
    }
    //end
    |}]
