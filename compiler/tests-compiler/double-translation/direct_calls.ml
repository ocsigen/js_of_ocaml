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

let%expect_test "direct calls with --enable effects,doubletranslate" =
  let code =
    compile_and_parse
      ~effects:true
      ~doubletranslate:true
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
  print_double_fun_decl code "test1";
  print_double_fun_decl code "test2";
  print_double_fun_decl code "test3";
  print_double_fun_decl code "test4";
  [%expect
    {|
    function test1$0(param){
     function f(g, x){return caml_doublecall1(g, x);}
     var _H_ = 7;
     f(function(x){return x + 1 | 0;}, _H_);
     var _I_ = 4.;
     f(function(x){return x * 2.;}, _I_);
     return 0;
    }
    //end
    function test1$1(param, cont){
     function f(g, x){return caml_doublecall1(g, x);}
     var _F_ = 7;
     f(function(x){return x + 1 | 0;}, _F_);
     var _G_ = 4.;
     f(function(x){return x * 2.;}, _G_);
     return cont(0);
    }
    //end
    var test1 = caml_cps_closure(test1$0, test1$1);
    //end
    function test2$0(param){
     var f = f$0();
     f(_h_(), 7);
     f(_j_(), cst_a);
     return 0;
    }
    //end
    function test2$1(param, cont){
     var f = f$0(), _y_ = 7, _z_ = _h_();
     return caml_cps_exact_double_call3
             (f,
              _z_,
              _y_,
              function(_A_){
               var _B_ = _j_();
               return caml_cps_exact_double_call3
                       (f, _B_, cst_a, function(_C_){return cont(0);});
              });
    }
    //end
    var test2 = caml_cps_closure(test2$0, test2$1);
    //end
    function test3$0(x){
     function F(symbol){function f(x){return x + 1 | 0;} return [0, f];}
     var M1 = F([0]), M2 = F([0]), _x_ = caml_doublecall1(M2[1], 2);
     return [0, caml_doublecall1(M1[1], 1), _x_];
    }
    //end
    function test3$1(x, cont){
     function F(symbol){function f(x){return x + 1 | 0;} return [0, f];}
     var M1 = F([0]), M2 = F([0]), _w_ = M2[1].call(null, 2);
     return cont([0, M1[1].call(null, 1), _w_]);
    }
    //end
    var test3 = caml_cps_closure(test3$0, test3$1);
    //end
    function test4$0(x){
     function F(symbol){var f$0 = f(); return [0, f$0];}
     var M1 = F([0]), M2 = F([0]);
     caml_doublecall1(M1[1], 1);
     return caml_doublecall1(M2[1], 2);
    }
    //end
    function test4$1(x, cont){
     function F(symbol){var f$0 = f(); return [0, f$0];}
     var M1 = F([0]), M2 = F([0]), _t_ = 1, _u_ = M1[1];
     return caml_cps_exact_double_call2
             (_u_,
              _t_,
              function(_v_){return caml_cps_exact_double_call2(M2[1], 2, cont);});
    }
    //end
    var test4 = caml_cps_closure(test4$0, test4$1);
    //end |}]
