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

(* Tests for the [Cond_propagation] pass. *)

open! Util

let prog =
  {|
let g x = print_int x; print_char ' '

(* Boolean [Cond]: [c] is known to be 1 in the true branch and 0 in the false
   branch. *)
let bool_cond (a : int) (b : int) =
  let c = a = b in
  if c then (g 10; c) else (g 20; c)

(* Multi-predecessor target: [c] is only rewritten in the edge arguments. *)
let edge_args (a : int) (b : int) =
  let c = a <= b in
  let r = if c then b <= 10 else c in
  g (if r then 1 else 0)

(* [Switch]: [x] is known to be [i] in the [i]th branch. *)
type t = A | B | C

let switch_prop (x : t) =
  match x with
  | A -> g 10; x
  | B -> g 20; x
  | C -> g 30; x

(* [x] is tested again in the branch where it is zero, and has no other
   use: everything can be rewritten. *)
let retest (x : bool) =
  if x
  then g 1
  else (
    g 0;
    let r = if x then 2 else (Obj.magic x : int) in
    g r;
    g r)

(* Same thing, but [x] is also used in the branch where its value is not
   known. The redundant test is folded, while the value of [x] is not
   replaced by a constant. *)
let retest_other_use (x : bool) h =
  if x
  then h x
  else (
    g 0;
    let r = if x then 2 else (Obj.magic x : int) in
    g r;
    g r)

(* The second test does not pass [x] along. *)
let retest_no_arg (x : bool) =
  if x
  then g 1
  else (
    g (Obj.magic x : int);
    if x then g 2 else g 3)

(* [x] has no value uses, only a redundant test in the false branch. *)
let retest_no_use (x : bool) =
  if x
  then g 1
  else (
    g 0;
    if x then g 2 else g 3)

(* A boolean tested again in the branch where it is one *)
let retest_true (a : int) (b : int) =
  let c = a = b in
  if c
  then (
    g 1;
    if c then g 2 else g 3)
  else g 4

(* An option tested again in the branch where it is not [None]: its value
   is not known there, so the second test is kept. *)
let retest_some (o : int option) =
  match o with
  | None -> g 0
  | Some _ -> (
      g 1;
      match o with
      | None -> g 2
      | Some n -> g n)

(* A [Switch] followed by a [Cond] on the same variable *)
let switch_then_cond (x : t) =
  match x with
  | A ->
      g 0;
      if (Obj.magic x : bool) then g 1 else g 2
  | B ->
      g 3;
      if (Obj.magic x : bool) then g 4 else g 5
  | C -> g 6

(* A [Cond] on a boolean followed by a [Switch] on the same variable *)
let cond_then_switch (a : int) (b : int) =
  let c = a = b in
  if c
  then (
    g 0;
    match (Obj.magic c : t) with
    | A -> g 1
    | B -> g 2
    | C -> g 3)
  else g 4

let () =
  let h b = g (if b then 7 else 8) in
  let pb b = g (if b then 1 else 0) in
  let pt = function A -> g 0 | B -> g 1 | C -> g 2 in
  List.iter (fun (a, b) -> pb (bool_cond a b)) [ 1, 1; 1, 2 ];
  List.iter (fun (a, b) -> edge_args a b) [ 3, 5; 8, 5 ];
  List.iter (fun x -> pt (switch_prop x)) [ A; B; C ];
  List.iter retest [ false; true ];
  List.iter (fun b -> retest_other_use b h) [ false; true ];
  List.iter retest_no_arg [ false; true ];
  List.iter retest_no_use [ false; true ];
  List.iter (fun (a, b) -> retest_true a b) [ 1, 1; 1, 2 ];
  List.iter retest_some [ None; Some 5 ];
  List.iter switch_then_cond [ A; B; C ];
  List.iter (fun (a, b) -> cond_then_switch a b) [ 1, 1; 1, 2 ];
  print_newline ()
|}

let%expect_test "behaviour" =
  compile_and_run ~flags:[ "--debug"; "invariant" ] prog;
  [%expect
    {| 10 1 20 0 1 0 10 0 20 1 30 2 0 0 0 1 0 0 0 7 0 3 1 0 3 1 1 2 4 0 1 5 0 2 3 4 6 0 2 4 |}]

let%expect_test "behaviour (cps)" =
  compile_and_run ~effects:`Cps ~flags:[ "--debug"; "invariant" ] prog;
  [%expect
    {| 10 1 20 0 1 0 10 0 20 1 30 2 0 0 0 1 0 0 0 7 0 3 1 0 3 1 1 2 4 0 1 5 0 2 3 4 6 0 2 4 |}]

let program = lazy (compile_and_parse ~flags:[ "--debug"; "invariant" ] prog)

let print name = print_fun_decl (Stdlib.Lazy.force program) (Some name)

let%expect_test "boolean cond" =
  print "bool_cond";
  [%expect
    {|
    function bool_cond(a, b){var c = a === b; return c ? (g(10), 1) : (g(20), 0);}
    //end
    |}]

let%expect_test "edge args" =
  print "edge_args";
  [%expect
    {|
    function edge_args(a, b){
     var c = a <= b, r = c ? b <= 10 : 0, _c_ = r ? 1 : 0;
     return g(_c_);
    }
    //end
    |}]

let%expect_test "switch" =
  print "switch_prop";
  [%expect
    {|
    function switch_prop(x){
     switch(x){
       case 0:
        g(10); return 0;
       case 1:
        g(20); return 1;
       default: g(30); return 2;
     }
    }
    //end
    |}]

let%expect_test "tested again" =
  print "retest";
  [%expect
    {|
    function retest(x){
     if(x) return g(1);
     g(0);
     var x$0 = 0;
     g(x$0);
     return g(x$0);
    }
    //end
    |}]

let%expect_test "tested again, with another use" =
  print "retest_other_use";
  [%expect
    {|
    function retest_other_use(x, h){
     return x ? caml_call1(h, x) : (g(0), g(x), g(x));
    }
    //end
    |}]

let%expect_test "tested again, not passed along" =
  print "retest_no_arg";
  [%expect
    {|
    function retest_no_arg(x){return x ? g(1) : (g(0), g(3));}
    //end
    |}]

let%expect_test "tested again, no value use" =
  print "retest_no_use";
  [%expect
    {|
    function retest_no_use(x){return x ? g(1) : (g(0), g(3));}
    //end
    |}]

let%expect_test "tested again in the first branch" =
  print "retest_true";
  [%expect
    {|
           function retest_true(a, b){var c = a === b; return c ? (g(1), g(2)) : g(4);}
           //end
           |}]

let%expect_test "option tested again" =
  print "retest_some";
  [%expect
    {|
    function retest_some(o){
     if(! o) return g(0);
     g(1);
     if(! o) return g(2);
     var n = o[1];
     return g(n);
    }
    //end
    |}]

let%expect_test "switch, then cond" =
  print "switch_then_cond";
  [%expect
    {|
           function switch_then_cond(x){
            switch(x){
              case 0:
               g(0); return g(2);
              case 1:
               g(3); return g(4);
              default: return g(6);
            }
           }
           //end
           |}]

let%expect_test "cond, then switch" =
  print "cond_then_switch";
  [%expect
    {|
           function cond_then_switch(a, b){
            var c = a === b;
            return c ? (g(0), g(2)) : g(4);
           }
           //end
           |}]

(* A mutable variable (a reference live in an exception handler), tested
   before and after being assigned. Compiled without [-g]: otherwise, the
   reference is not turned into a mutable variable and there is no
   [Assign]. *)
let assigned_prog =
  {|
let assigned_var (b : bool) (f : unit -> unit) =
  let r = ref b in
  try
    if !r
    then (
      f ();
      2)
    else (
      r := true;
      f ();
      if !r then 3 else 4)
  with _ -> if !r then 5 else 6

let () =
  List.iter
    (fun (b, f) ->
      print_int (assigned_var b f);
      print_char ' ')
    [ true, ignore; false, ignore; false, (fun () -> raise Not_found) ];
  print_newline ()
|}

let%expect_test "assigned variable" =
  compile_and_run
    ~debug:false
    ~flags:[ "--debug"; "invariant"; "-w"; "no-missing-debug-event" ]
    assigned_prog;
  [%expect {| 2 3 5 |}]
