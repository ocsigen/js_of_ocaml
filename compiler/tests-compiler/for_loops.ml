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

(* Counting loops are emitted as [for(var i = lo; i <= hi; i++){...}].

   The tests are compiled without [--pretty], since named variables are
   otherwise not inlined, but with readable variable names. *)

open! Util

let prog =
  {|
let use : int -> unit = fun x -> print_int x; print_char ' '

let up a = for i = 0 to Array.length a - 1 do use a.(i) done

let down a = for i = Array.length a - 1 downto 0 do use a.(i) done

let bounds lo hi = for i = lo to hi do use i done

(* Nested loops *)
let nested n = for i = 1 to n do for j = i to n do use (i * j) done done

(* The loop variable is captured: its final value must be preserved *)
let captured n =
  let l = ref [] in
  for i = 0 to n do l := (fun () -> i) :: !l done;
  List.iter (fun f -> use (f ())) !l

(* The loop has an accumulator, which is used afterwards *)
let sum a =
  let s = ref 0 in
  for i = 0 to Array.length a - 1 do s := !s + a.(i) done;
  !s

(* Another exit *)
exception Found of int

let find a x =
  try
    for i = 0 to Array.length a - 1 do if a.(i) = x then raise (Found i) done;
    -1
  with Found i -> i

let () =
  let a = [| 1; 2; 3 |] in
  up a; up [||]; down a; down [||];
  bounds 3 5; bounds 5 3; bounds 4 4;
  bounds (max_int - 1) max_int; bounds min_int (min_int + 1);
  nested 3; captured 2;
  use (sum a); use (sum [||]);
  use (find a 3); use (find a 4);
  for i = max_int downto max_int - 1 do use i done;
  for i = min_int + 1 downto min_int do use i done
|}

let%expect_test "behaviour" =
  compile_and_run prog;
  [%expect
    {| 1 2 3 3 2 1 3 4 5 4 2147483646 2147483647 -2147483648 -2147483647 1 2 3 4 6 9 2 1 0 6 0 2 -1 2147483647 2147483646 -2147483647 -2147483648 |}]

let program =
  lazy (compile_and_parse ~pretty:false ~flags:[ "--disable"; "shortvar" ] prog)

let print name = print_fun_decl (Stdlib.Lazy.force program) (Some name)

let%expect_test "up" =
  print "up";
  [%expect
    {|
    function up(a){
     var b = a.length - 2 | 0;
     for(var i = 0; i <= b; i++) use(caml_check_bound(a, i)[i + 1]);
     return 0;
    }
    //end
    |}]

let%expect_test "down" =
  print "down";
  [%expect
    {|
    function down(a){
     var b = a.length - 2 | 0;
     for(; b >= 0; b--) use(caml_check_bound(a, b)[b + 1]);
     return 0;
    }
    //end
    |}]

let%expect_test "bounds" =
  print "bounds";
  [%expect
    {|
    function bounds(lo, hi){for(; lo <= hi; lo++) use(lo); return 0;}
    //end
    |}]

let%expect_test "nested" =
  print "nested";
  [%expect
    {|
    function nested(n){
     for(var i = 1; i <= n; i++)
      for(var j = i; j <= n; j++) use(runtime.caml_mul(i, j));
     return 0;
    }
    //end
    |}]

let%expect_test "captured" =
  print "captured";
  [%expect
    {|
    function captured(n){
     var a = 0;
     if(n >= 0){
      var i = 0;
      for(;;){
       let i$0 = i;
       a = [0, function(param){return i$0;}, a];
       if(n === i) break;
       i = i + 1 | 0;
      }
     }
     return caml_call2
             (Stdlib_List[19], function(f){return use(caml_call1(f, 0));}, a);
    }
    //end
    |}]

let%expect_test "accumulator" =
  print "sum";
  [%expect
    {|
    function sum(a){
     var c = a.length - 2 | 0, b = 0;
     if(c >= 0){
      var i = 0;
      for(;;){
       b = b + caml_check_bound(a, i)[i + 1] | 0;
       if(c === i) break;
       i = i + 1 | 0;
      }
     }
     return b;
    }
    //end
    |}]

let%expect_test "other exit" =
  print "find";
  [%expect
    {|
    function find(a, x){
     try{
      var b = a.length - 2 | 0;
      for(var i = 0; i <= b; i++)
       if(runtime.caml_equal(caml_check_bound(a, i)[i + 1], x))
        throw caml_maybe_attach_backtrace([0, Found, i], 1);
      return - 1;
     }
     catch(exn){
      i = caml_wrap_exception(exn);
      if(i[1] === Found) return i[2];
      throw caml_maybe_attach_backtrace(i, 0);
     }
    }
    //end
    |}]
