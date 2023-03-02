(* Js_of_ocaml compiler
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

let%expect_test "test-compiler/lib-effects/effects_call_opt.ml" =
  let code =
    compile_and_parse
      ~effects:true
      {|
         (* Arity of the argument of a function / direct call *)
         let test1 () =
           let f g x = g x in
           ignore (f (fun x -> x + 1) 7);
           ignore (f (fun x -> x *. 2.) 4.)

         (* Arity of the argument of a function / CPS call *)
         let test2 () =
           let f g x = g x in
           ignore (f (fun x -> x + 1) 7);
           ignore (f (fun x -> x ^ "a") "a")

         (* Arity of functions in a functor / direct call *)
         let test3 x =
           let module F(_ : sig end) = struct let f x = x + 1 end in
           let module M1 = F (struct end) in
           let module M2 = F (struct end) in
           (M1.f 1, M2.f 2)

         (* Arity of functions in a functor / CPS call *)
         let test4 x =
           let module F(_ : sig end) =
             struct let f x = Printf.printf "%d" x end in
           let module M1 = F (struct end) in
           let module M2 = F (struct end) in
           M1.f 1; M2.f 2
|}
  in
  print_fun_decl code (Some "test1");
  print_fun_decl code (Some "test2");
  print_fun_decl code (Some "test3");
  print_fun_decl code (Some "test4");
  [%expect
    {|
    function test1(param, cont){
     function f(g, x){return g(x);}
     var _k_ = 7;
     f(function(x){return x + 1 | 0;}, _k_);
     var _l_ = 4.;
     f(function(x){return x * 2.;}, _l_);
     return cont(0);
    }
    //end
    function test2(param, cont){
     function f(g, x, cont){return caml_cps_exact_call2(g, x, cont);}
     var _f_ = 7;
     function _g_(x, cont){return cont(x + 1 | 0);}
     return caml_cps_exact_call3
             (f,
              _g_,
              _f_,
              function(_h_){
               function _i_(x, cont){
                return caml_cps_call3(Stdlib[28], x, cst_a$0, cont);
               }
               return caml_cps_exact_call3
                       (f, _i_, cst_a, function(_j_){return cont(0);});
              });
    }
    //end
    function test3(x, cont){
     function F(symbol){function f(x){return x + 1 | 0;} return [0, f];}
     var M1 = F([0]), M2 = F([0]), _e_ = M2[1](2);
     return cont([0, M1[1](1), _e_]);
    }
    //end
    function test4(x, cont){
     function F(symbol){
      function f(x, cont){return caml_cps_call3(Stdlib_Printf[2], _a_, x, cont);}
      return [0, f];
     }
     var M1 = F([0]), M2 = F([0]), _b_ = 1, _c_ = M1[1];
     return caml_cps_exact_call2
             (_c_,
              _b_,
              function(_d_){return caml_cps_exact_call2(M2[1], 2, cont);});
    }
    //end |}]
