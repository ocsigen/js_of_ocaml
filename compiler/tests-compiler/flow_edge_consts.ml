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

(* Flow knows the value of a tested variable along some edges: 0 in the
   false branch of a conditional, [i] in branch [i] of a switch. A join
   parameter that receives the same constant on all its edges is then
   replaced by this constant. *)

let%expect_test "join of the tested value and 0" =
  let prog =
    {|
let swap_first a =
  let l = Array.length a in
  if 1 < l then begin
    let e = a.(1) in
    a.(1) <- a.(0);
    a.(0) <- e
  end
|}
  in
  let program = Util.compile_and_parse prog in
  Util.print_fun_decl program (Some "swap_first");
  [%expect
    {|
           function swap_first(a){
            var l = a.length - 1;
            if(1 < l){
             var e = caml_check_bound(a, 1)[2];
             a[2] = caml_check_bound(a, 0)[1];
             a[1] = e;
            }
            return 0;
           }
           //end
           |}]

(* Along branch 0 of the switch, [x] is passed to the join and is known
   to be 0; the other branches pass other constants, so the join is
   kept. *)
let%expect_test "switch edge" =
  let prog =
    {|
let h x = (match x with 0 -> x | 1 -> 7 | 2 -> 9 | 3 -> 11 | 4 -> 13 | 5 -> 1 | _ -> 0) + 1
|}
  in
  let program = Util.compile_and_parse prog in
  Util.print_fun_decl program (Some "h");
  [%expect
    {|
           function h(x){
            if(5 < x >>> 0)
             var _a_ = 0;
            else
             switch(x){
               case 1:
                _a_ = 7; break;
               case 2:
                _a_ = 9; break;
               case 3:
                _a_ = 11; break;
               case 4:
                _a_ = 13; break;
               case 5:
                _a_ = 1; break;
               default: _a_ = x;
             }
            return _a_ + 1 | 0;
           }
           //end
           |}]

let%expect_test "semantics" =
  let prog =
    {|
let swap_first a =
  let l = Array.length a in
  if 1 < l then begin
    let e = a.(1) in a.(1) <- a.(0); a.(0) <- e
  end
let h x = (match x with 0 -> x | 1 -> 7 | 2 -> 9 | 3 -> 11 | 4 -> 13 | 5 -> 1 | _ -> 0) + 1
let () =
  let a = [| 1; 2 |] in
  swap_first a;
  Printf.printf "%d %d\n" a.(0) a.(1);
  List.iter (fun x -> Printf.printf "%d " (h x)) [0; 1; 2; 3; 4; 5; 7; -1];
  print_newline ()
|}
  in
  Util.compile_and_run prog;
  [%expect {|
           2 1
           1 8 10 12 14 2 1 1
           |}]
