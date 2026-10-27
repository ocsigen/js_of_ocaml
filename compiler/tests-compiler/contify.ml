(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
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
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open Util

let%expect_test "tail calls" =
  let program =
    compile_and_parse
      {|
    let foo x =
      let r = ref 0. in
      if x < 10 then r := !r +. 1.;
      let ret n = !r *. n in
      if x mod 3 = 0 then ret 2.
      else if x mod 3 = 1 then ret 1.
      else 10.
  |}
  in
  print_fun_decl program (Some "foo");
  [%expect
    {|
           function foo(x){
            var r = x < 10 ? 1. : 0.;
            if(0 === (x % 3 | 0))
             var n = 2.;
            else{if(1 !== (x % 3 | 0)) return 10.; n = 1.;}
            return r * n;
           }
           //end
           |}]

let%expect_test "shared continuation" =
  let program =
    compile_and_parse
      {|
    let bar x y =
      let k z = (z * 7) + (x * y) + (z / 3) + 12 + (x lxor z) in
      let v = match x with 0 -> k 1 | 1 -> k y | 2 -> k (y + 1) | _ -> 0 in
      v + 1
  |}
  in
  print_fun_decl program (Some "bar");
  [%expect
    {|
           function bar(x, y){
            if(2 < x >>> 0)
             var v = 0;
            else{
             switch(x){
               case 0:
                var z = 1; break;
               case 2:
                z = y + 1 | 0; break;
               default: z = y;
             }
             v =
              ((((z * 7 | 0) + runtime.caml_mul(x, y) | 0) + (z / 3 | 0) | 0) + 12 | 0)
              + (x ^ z)
              | 0;
            }
            return v + 1 | 0;
           }
           //end
           |}]

let%expect_test "local loop" =
  let program =
    compile_and_parse
      {|
    let sum a =
      let n = Array.length a in
      let rec loop i acc = if i = n then acc else loop (i + 1) (acc + a.(i)) in
      loop 0 0 + 1
  |}
  in
  print_fun_decl program (Some "sum");
  [%expect
    {|
           function sum(a){
            var n = a.length - 1, i = 0, acc = 0;
            for(;;){
             if(i === n) return acc + 1 | 0;
             var
              acc$0 = acc + runtime.caml_check_bound(a, i)[i + 1] | 0,
              i$0 = i + 1 | 0;
             i = i$0;
             acc = acc$0;
            }
           }
           //end
           |}]

let%expect_test "different continuations" =
  let program =
    compile_and_parse
      {|
    let baz x y =
      let k z = (z * 7) + (x * y) + (z / 3) + 12 + (x lxor z) in
      let v = if x > 0 then k 1 else 2 in
      let w = if y > 0 then k 2 else 3 in
      v + w
  |}
  in
  print_fun_decl program (Some "baz");
  [%expect
    {|
           function baz(x, y){
            function k(z){
             return ((((z * 7 | 0) + runtime.caml_mul(x, y) | 0) + (z / 3 | 0) | 0) + 12
                    | 0)
                    + (x ^ z)
                    | 0;
            }
            var v = 0 < x ? k(1) : 2, w = 0 < y ? k(2) : 3;
            return v + w | 0;
           }
           //end
           |}]

let%expect_test "escaping function" =
  let program =
    compile_and_parse
      {|
    let qux x g =
      let k z = (z * 7) + x + (z / 3) + 12 + (x lxor z) in
      if x > 0 then g k else k 1
  |}
  in
  print_fun_decl program (Some "qux");
  [%expect
    {|
    function qux(x, g){
     function k(z){
      return ((((z * 7 | 0) + x | 0) + (z / 3 | 0) | 0) + 12 | 0) + (x ^ z) | 0;
     }
     return 0 < x ? caml_call1(g, k) : k(1);
    }
    //end
    |}]

let%expect_test "tail calls in a function applied once" =
  (* [g] is tail-called from [h], whose single call is followed by more
     code: the returns of [g] jump to that code, which should not be
     rewritten when processing [h] *)
  compile_and_run
    {|
    let f x l =
      let rec g acc = function [] -> acc | y :: r -> g (acc + y) r in
      let h b = if b then g 0 l else g 1 (List.rev l) in
      let r = h x in
      print_int (r + 1);
      print_newline ()

    let () = f true [ 1; 2 ]; f false [ 3 ]
  |};
  [%expect {|
           4
           5
           |}]

let%expect_test "calls in different exception handlers" =
  (* Both calls end with a [Poptrap] to the same block, but remove
     different handlers *)
  compile_and_run
    {|
    let g c n =
      let f x = print_int x; print_newline (); if x > n then raise Not_found; x + 1 in
      if c then (try f 1 with Not_found -> 0) else (try f 2 with Not_found -> 10)

    let () = print_int (g (Sys.opaque_identity true) (Sys.opaque_identity 0)); print_newline ()
    let () = print_int (g (Sys.opaque_identity false) (Sys.opaque_identity 0)); print_newline ()
  |};
  [%expect {|
           1
           0
           2
           10
           |}]

let%expect_test "calls in the same exception handler" =
  let prog =
    {|
    exception E

    let g pr fail c n =
      try
        let f x = pr x; if x > n then fail (); x + 1 in
        if c then f 1 else f 2
      with E -> 10

    let pr x = print_int x; print_newline ()
    let fail () = raise E
    let () = pr (g pr fail (Sys.opaque_identity true) (Sys.opaque_identity 0))
    let () = pr (g pr fail (Sys.opaque_identity false) (Sys.opaque_identity 5))
  |}
  in
  compile_and_run prog;
  [%expect {|
           1
           10
           2
           3
           |}];
  print_fun_decl (compile_and_parse prog) (Some "g");
  [%expect
    {|
    function g(pr, fail, c, n){
     try{
      var x = c ? 1 : 2;
      caml_call1(pr, x);
      if(n < x) caml_call1(fail, 0);
      return x + 1 | 0;
     }
     catch(exn$0){
      var exn = caml_wrap_exception(exn$0);
      if(exn === E) return 10;
      throw caml_maybe_attach_backtrace(exn, 0);
     }
    }
    //end
    |}]

let%expect_test "jumps to a loop header" =
  (* One call is inside the loop and the other outside: the function
     body would be a second entry point of the loop *)
  compile_and_run
    ~flags:[ "--debug"; "invariant" ]
    {|
    let r = ref 0

    let g c n =
      let f x = r := !r + x; print_int !r; print_newline () in
      if c then f 1;
      while !r < n do f 1 done

    let () = g (Sys.opaque_identity true) 3; g (Sys.opaque_identity false) 5
  |};
  [%expect
    {|
           1
           2
           3
           4
           5
           |}]

let%expect_test "function that never returns" =
  (* The continuation of the call becomes unreachable *)
  compile_and_run
    ~flags:[ "--debug"; "invariant" ]
    {|
    let h p =
      let f a = print_int a; print_newline (); raise Exit in
      let x = f p in
      print_int x; x + 1

    let () = try ignore (h (Sys.opaque_identity 3)) with Exit -> print_endline "E"
  |};
  [%expect {|
           3
           E
           |}]
