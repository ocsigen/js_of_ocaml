(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2019 Ty Overby
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

module M1 = struct
  let code =
    {|
  let f_prime g = g 1 2
  let f g = f_prime g 3 4
  (* [g] will be unknown as long as [f] is not inlined. *)
  let g () = f (fun a b c d -> print_int (a + b + c + d))
  let h () = f (fun a b c d -> print_int (a + b + c + d))
  let () = g (); print_newline ()
  let () = h (); print_newline ()
  (* using [f] multiple time should prevent inlining to trigger. *)

  let k a b c d = a + b + c + d |> print_int
  let l = k 1 2
  let m = k 3 4

  let () = l 5 6; print_newline ()
  let () = m 8 10; print_newline ()
|}

  let%expect_test "executed code" =
    compile_and_run code;
    [%expect {|
    10
    10
    14
    25 |}]

  let%expect_test "generated code" =
    let generated = compile_and_parse code in
    print_fun_decl generated (Some "f");
    print_fun_decl generated (Some "f_prime");
    print_fun_decl generated (Some "g");
    print_fun_decl generated (Some "h");
    print_fun_decl generated (Some "k");
    print_fun_decl generated (Some "l");
    print_fun_decl generated (Some "m");
    print_fun_decl generated (Some "caml_call1");
    print_fun_decl generated (Some "caml_call2");
    [%expect
      {|
    function f(g){return caml_call2(f_prime(g),3,4)}
    function f_prime(g){return caml_call2(g,1,2)}
    function g(param)
     {return f
              (function(a,b,c,d)
                {return caml_call1(Stdlib[44],((a + b | 0) + c | 0) + d | 0)})}
    function h(param)
     {return f
              (function(a,b,c,d)
                {return caml_call1(Stdlib[44],((a + b | 0) + c | 0) + d | 0)})}
    function k(a,b,c,d)
     {return caml_call1(Stdlib[44],((a + b | 0) + c | 0) + d | 0)}
    function l(_g_,_h_){return k(_b_,_a_,_g_,_h_)}
    function m(_e_,_f_){return k(_d_,_c_,_e_,_f_)}
    function caml_call1(f,a0)
     {return f.length == 1?f(a0):runtime.caml_call_gen(f,[a0])}
    function caml_call2(f,a0,a1)
     {return f.length == 2?f(a0,a1):runtime.caml_call_gen(f,[a0,a1])}
    |}]
end

module M2 = struct
  let code =
    {|
  let f _ a b c d e (_f: int -> int -> int -> int -> int -> unit -> unit) =
      print_int (a + b + c + d + e);
      print_newline ();;
  let f_prime f = f true ;;
  let f_prime_prime f = f false;;
  let g _a _b _c _d _e _f = failwith "printed g!" ;;
  let () = f_prime f 1 2 3 4 5 g ;;
  let () = f_prime f 2 3 4 5 6 g ;;
  let () = f_prime_prime f 1 2 3 4 5 g ;;
  let () = f_prime_prime f 2 3 4 5 6 g ;;
  |}

  let%expect_test "generated code" =
    let generated = compile_and_parse code in
    print_fun_decl generated (Some "f");
    print_fun_decl generated (Some "f_prime");
    print_fun_decl generated (Some "f_prime_prime");
    print_fun_decl generated (Some "g");
    [%expect
      {|
      function f(param,a,b,c,d,e,f)
       {caml_call1(Stdlib[44],(((a + b | 0) + c | 0) + d | 0) + e | 0);
        return caml_call1(Stdlib[47],0)}
      function f_prime(f){return caml_call1(f,1)}
      function f_prime_prime(f){return caml_call1(f,0)}
      function g(a,b,c,d,e,f){return caml_call1(Stdlib[2],cst_printed_g)} |}]

  let%expect_test _ =
    compile_and_run code;
    [%expect {|
      15
      20
      15
      20 |}]
end
