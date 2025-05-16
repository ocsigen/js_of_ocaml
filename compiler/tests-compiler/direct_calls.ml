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

let%expect_test "direct calls without --effects=cps" =
  let code =
    compile_and_parse
      {|
         (* Arity of the argument of a function / direct call *)
         let test1 () =
           let f g x = try g x with e -> raise e in
           ignore (f (fun x -> x + 1) 7);
           ignore (f (fun x -> x *. 2.) 4.)

         (* Arity of the argument of a function / CPS call *)
         let test2 () =
           let f g x = try g x with e -> raise e in
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
    function test1(param){
     function f(g, x){
      try{caml_call1(g, x); return;}
      catch(e$0){
       var e = caml_wrap_exception(e$0);
       throw caml_maybe_attach_backtrace(e, 0);
      }
     }
     f(function(x){return x + 1 | 0;}, 7);
     f(function(x){return x * 2.;}, 4.);
     return 0;
    }
    //end
    function test2(param){
     function f(g, x){
      try{caml_call1(g, x); return;}
      catch(e$0){
       var e = caml_wrap_exception(e$0);
       throw caml_maybe_attach_backtrace(e, 0);
      }
     }
     f(function(x){return x + 1 | 0;}, 7);
     f(function(x){return caml_call2(Stdlib[28], x, cst_a$0);}, cst_a);
     return 0;
    }
    //end
    function test3(x){
     function F(symbol){function f(x){return x + 1 | 0;} return [0, f];}
     var M1 = F([0]), M2 = F([0]), _a_ = M2[1].call(null, 2);
     return [0, M1[1].call(null, 1), _a_];
    }
    //end
    function test4(x){
     function F(symbol){
      function f(x){return caml_call2(Stdlib_Printf[2], _a_, x);}
      return [0, f];
     }
     var M1 = F([0]), M2 = F([0]);
     M1[1].call(null, 1);
     return M2[1].call(null, 2);
    }
    //end
    |}]

let%expect_test "direct calls with --effects=cps" =
  let code =
    compile_and_parse
      ~effects:`Cps
      {|
         (* Arity of the argument of a function / direct call *)
         let test1 () =
           let f g x = try g x with e -> raise e in
           ignore (f (fun x -> x + 1) 7);
           ignore (f (fun x -> x *. 2.) 4.)

         (* Arity of the argument of a function / CPS call *)
         let test2 () =
           let f g x = try g x with e -> raise e in
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
     function f(g, x){
      try{g(); return;}
      catch(e$0){
       var e = caml_wrap_exception(e$0);
       throw caml_maybe_attach_backtrace(e, 0);
      }
     }
     f(function(x){});
     f(function(x){});
     return cont(0);
    }
    //end
    function test2(param, cont){
     function f(g, x, cont){
      runtime.caml_push_trap
       (function(e){
         var raise = caml_pop_trap(), e$0 = caml_maybe_attach_backtrace(e, 0);
         return raise(e$0);
        });
      return caml_exact_trampoline_cps_call
              (g, x, function(_a_){caml_pop_trap(); return cont();});
     }
     return caml_exact_trampoline_cps_call$0
             (f,
              function(x, cont){return cont();},
              7,
              function(_a_){
               return caml_exact_trampoline_cps_call$0
                       (f,
                        function(x, cont){
                         return caml_trampoline_cps_call3
                                 (Stdlib[28], x, cst_a$0, cont);
                        },
                        cst_a,
                        function(_a_){return cont(0);});
              });
    }
    //end
    function test3(x, cont){
     function F(symbol){function f(x){return x + 1 | 0;} return [0, f];}
     var M1 = F(), M2 = F(), _a_ = M2[1].call(null, 2);
     return cont([0, M1[1].call(null, 1), _a_]);
    }
    //end
    function test4(x, cont){
     function F(symbol){
      function f(x, cont){
       return caml_trampoline_cps_call3(Stdlib_Printf[2], _a_, x, cont);
      }
      return [0, f];
     }
     var M1 = F(), M2 = F();
     return caml_exact_trampoline_cps_call
             (M1[1],
              1,
              function(_a_){
               return caml_exact_trampoline_cps_call(M2[1], 2, cont);
              });
    }
    //end
    |}]
