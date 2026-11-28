(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2026 Hugo Heuzard
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

(* A pure expression whose only use is an argument of a single branch of
   a conditional or switch is evaluated in that branch.

   The tests are compiled without [--pretty], since named variables are
   otherwise not inlined, but with readable variable names. *)

open! Util

let prog =
  {|
let use : int -> unit = fun x -> print_int x; print_char ' '
let use_pair : int * int -> unit = fun (x, y) -> use x; use y
let side_effect = ref 0

(* The loop counter increment is only needed when the loop continues *)
let for_loop f n =
  for i = 0 to n do
    f i
  done

(* A single instruction, used by one branch of a conditional *)
let single c x =
  let y = x + 1 in
  let r = if c then y else 0 in
  use r; use r

(* A chain of instructions is moved as a whole *)
let chain c x =
  let y = x + 1 in
  let z = y * 2 in
  let t = z - x in
  let r = if c then t else 0 in
  use r; use r

(* A block allocation *)
let block c x q =
  let p = (x, x + 1) in
  let r = if c then p else q in
  use_pair r; use_pair r

(* One arm of a switch *)
type t = A | B | C | D
let switch_arm v x =
  let y = x + 1 in
  let r = match v with A -> y | B -> 0 | C -> 1 | D -> 2 in
  use r; use r

(* Used by both branches: not moved *)
let both c x =
  let y = x + 1 in
  let r = if c then y else y * 2 in
  use r; use r

(* Used after the conditional as well: not moved *)
let used_later c x =
  let y = x + 1 in
  let r = if c then y else 0 in
  use r; use y

(* Reading a mutable field is not moved past the side effect *)
let impure c (x : int ref) =
  let y = !x in
  incr side_effect;
  x := 7;
  let r = if c then y else 0 in
  use r; use r

(* In the body of a [try], with a mutable variable read in the
   handler *)
let mutable_in_try c f n =
  let r = ref 0 in
  (try
    for i = 0 to n do
       let y = !r + 1 in
       r := !r + i;
       let v = if c then y else 0 in
       f v;
       f v
    done
   with Exit -> ());
  use !r

let () =
  for_loop use 3;
  single true 1; single false 1;
  chain true 1; chain false 1;
  block true 1 (8, 9); block false 1 (8, 9);
  switch_arm A 1; switch_arm B 1; switch_arm C 1; switch_arm D 1;
  both true 1; both false 1;
  used_later true 1; used_later false 1;
  impure true (ref 3); impure false (ref 3);
  mutable_in_try true (fun x -> if x > 5 then raise Exit else use x) 9;
  mutable_in_try false use 2;
  print_newline ()
|}

let%expect_test "behaviour" =
  compile_and_run prog;
  [%expect
    {| 0 1 2 3 2 2 0 0 3 3 0 0 1 2 1 2 8 9 8 9 2 2 0 0 1 1 2 2 2 2 4 4 2 2 0 2 3 3 0 0 1 1 1 1 2 2 4 4 10 0 0 0 0 0 0 3 |}]

let program =
  lazy (compile_and_parse ~pretty:false ~flags:[ "--disable"; "shortvar" ] prog)

let print name = print_fun_decl (Stdlib.Lazy.force program) (Some name)

let%expect_test "for loop" =
  print "for_loop";
  [%expect
    {|
    function for_loop(f, n){
     for(var i = 0; i <= n; i++) caml_call1(f, i);
     return 0;
    }
    //end
    |}]

let%expect_test "single instruction" =
  print "single";
  [%expect
    {|
    function single(c, x){c = c ? x + 1 | 0 : 0; use(c); return use(c);}
    //end
    |}]

let%expect_test "chain" =
  print "chain";
  [%expect
    {|
    function chain(c, x){
     c = c ? ((x + 1 | 0) * 2 | 0) - x | 0 : 0;
     use(c);
     return use(c);
    }
    //end
    |}]

let%expect_test "block" =
  print "block";
  [%expect
    {|
    function block(c, x, q){
     q = c ? [0, x, x + 1 | 0] : q;
     use_pair(q);
     return use_pair(q);
    }
    //end
    |}]

let%expect_test "switch" =
  print "switch_arm";
  [%expect
    {|
    function switch_arm(v, x){
     switch(v){
       case 1:
        x = 0; break;
       case 2:
        x = 1; break;
       case 3:
        x = 2; break;
       default: x = x + 1 | 0;
     }
     use(x);
     return use(x);
    }
    //end
    |}]

let%expect_test "used by both branches" =
  print "both";
  [%expect
    {|
    function both(c, x){
     x = x + 1 | 0;
     c = c ? x : x * 2 | 0;
     use(c);
     return use(c);
    }
    //end
    |}]

let%expect_test "used after the conditional" =
  print "used_later";
  [%expect
    {|
    function used_later(c, x){
     x = x + 1 | 0;
     c = c ? x : 0;
     use(c);
     return use(x);
    }
    //end
    |}]

let%expect_test "mutable read" =
  print "impure";
  [%expect
    {|
    function impure(c, x){
     var y = x[1];
     side_effect[1]++;
     x[1] = 7;
     y = c ? y : 0;
     use(y);
     return use(y);
    }
    //end
    |}]

let%expect_test "in a try body" =
  print "mutable_in_try";
  [%expect
    {|
    function mutable_in_try(c, f, n){
     var r = [0, 0];
     try{
      for(var i = 0; i <= n; i++){
       var y = r[1] + 1 | 0;
       r[1] = r[1] + i | 0;
       y = c ? y : 0;
       caml_call1(f, y);
       caml_call1(f, y);
      }
     }
     catch(exn){
      c = caml_wrap_exception(exn);
      if(c !== Stdlib[3]) throw caml_maybe_attach_backtrace(c, 0);
     }
     return use(r[1]);
    }
    //end
    |}]
