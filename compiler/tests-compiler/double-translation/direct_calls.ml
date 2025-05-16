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

let%expect_test "direct calls with --effects=double-translation" =
  let code =
    compile_and_parse
      ~effects:`Double_translation
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

         (* Result of double-translating two mutually recursive functions *)
         let test5 () =
           let g x =
             let rec f y = if y = 0 then 1 else x + h (y - 1)
             and h z = if z = 0 then 1 else x + f (z - 1)
             in
             print_int (f 12 + h 100)
           in
           ignore (g 42);
           ignore (g (-5));
|}
  in
  print_program code;
  [%expect
    {|
    (function(globalThis){
       "use strict";
       var
        runtime = globalThis.jsoo_runtime,
        caml_cps_closure = runtime.caml_cps_closure,
        caml_maybe_attach_backtrace = runtime.caml_maybe_attach_backtrace,
        caml_pop_trap = runtime.caml_pop_trap,
        caml_string_of_jsbytes = runtime.caml_string_of_jsbytes,
        caml_wrap_exception = runtime.caml_wrap_exception;
       function caml_call1(f, a0){
        return (f.l >= 0 ? f.l : f.l = f.length) === 1
                ? f(a0)
                : runtime.caml_call_gen(f, [a0]);
       }
       function caml_call2(f, a0, a1){
        return (f.l >= 0 ? f.l : f.l = f.length) === 2
                ? f(a0, a1)
                : runtime.caml_call_gen(f, [a0, a1]);
       }
       function caml_trampoline_cps_call2(f, a0, a1){
        return runtime.caml_stack_check_depth()
                ? f.cps
                  ? (f.cps.l
                      >= 0
                      ? f.cps.l
                      : f.cps.l = f.cps.length)
                    === 2
                    ? f.cps.call(null, a0, a1)
                    : runtime.caml_call_gen_cps(f, [a0, a1])
                  : a1
                    ((f.l >= 0 ? f.l : f.l = f.length) === 1
                      ? f(a0)
                      : runtime.caml_call_gen(f, [a0]))
                : runtime.caml_trampoline_return(f, [a0, a1], 0);
       }
       function caml_exact_trampoline_cps_call(f, a0, a1){
        return runtime.caml_stack_check_depth()
                ? f.cps ? f.cps.call(null, a0, a1) : a1(f(a0))
                : runtime.caml_trampoline_return(f, [a0, a1], 0);
       }
       function caml_trampoline_cps_call3(f, a0, a1, a2){
        return runtime.caml_stack_check_depth()
                ? f.cps
                  ? (f.cps.l
                      >= 0
                      ? f.cps.l
                      : f.cps.l = f.cps.length)
                    === 3
                    ? f.cps.call(null, a0, a1, a2)
                    : runtime.caml_call_gen_cps(f, [a0, a1, a2])
                  : a2
                    ((f.l >= 0 ? f.l : f.l = f.length) === 2
                      ? f(a0, a1)
                      : runtime.caml_call_gen(f, [a0, a1]))
                : runtime.caml_trampoline_return(f, [a0, a1, a2], 0);
       }
       function caml_exact_trampoline_cps_call$0(f, a0, a1, a2){
        return runtime.caml_stack_check_depth()
                ? f.cps ? f.cps.call(null, a0, a1, a2) : a2(f(a0, a1))
                : runtime.caml_trampoline_return(f, [a0, a1, a2], 0);
       }
       var
        dummy = 0,
        global_data = runtime.caml_get_global_data(),
        _a_ = [0, [4, 0, 0, 0, 0], caml_string_of_jsbytes("%d")],
        cst_a$0 = caml_string_of_jsbytes("a"),
        cst_a = caml_string_of_jsbytes("a"),
        Stdlib = global_data.Stdlib,
        Stdlib_Printf = global_data.Stdlib__Printf;
       function test1(param){
        function f(g, x){
         try{caml_call1(g, dummy); return;}
         catch(e$0){
          var e = caml_wrap_exception(e$0);
          throw caml_maybe_attach_backtrace(e, 0);
         }
        }
        f(function(x){});
        f(function(x){});
        return 0;
       }
       function f$0(){
        function f$0(g, x){
         try{caml_call1(g, x); return;}
         catch(e$0){
          var e = caml_wrap_exception(e$0);
          throw caml_maybe_attach_backtrace(e, 0);
         }
        }
        function f$1(g, x, cont){
         runtime.caml_push_trap
          (function(e$0){
            var raise = caml_pop_trap(), e = caml_maybe_attach_backtrace(e$0, 0);
            return raise(e);
           });
         return caml_exact_trampoline_cps_call
                 (g, x, function(_e_){caml_pop_trap(); return cont();});
        }
        var f = caml_cps_closure(f$0, f$1);
        return f;
       }
       function _b_(){return function(x){};}
       function _c_(){
        return caml_cps_closure
                (function(x){return caml_call2(Stdlib[28], x, cst_a$0);},
                 function(x, cont){
                  return caml_trampoline_cps_call3(Stdlib[28], x, cst_a$0, cont);
                 });
       }
       function test2$0(param){
        var f = f$0();
        f(_b_(), 7);
        f(_c_(), cst_a);
        return 0;
       }
       function test2$1(param, cont){
        var f = f$0();
        return caml_exact_trampoline_cps_call$0
                (f,
                 _b_(),
                 7,
                 function(_e_){
                  return caml_exact_trampoline_cps_call$0
                          (f, _c_(), cst_a, function(_e_){return cont(0);});
                 });
       }
       var test2 = caml_cps_closure(test2$0, test2$1);
       function test3(x){
        function F(symbol){function f(x){return x + 1 | 0;} return [0, f];}
        var M1 = F(), M2 = F(), _e_ = caml_call1(M2[1], 2);
        return [0, caml_call1(M1[1], 1), _e_];
       }
       function f(){
        function f$0(x){return caml_call2(Stdlib_Printf[2], _a_, x);}
        function f$1(x, cont){
         return caml_trampoline_cps_call3(Stdlib_Printf[2], _a_, x, cont);
        }
        var f = caml_cps_closure(f$0, f$1);
        return f;
       }
       function F(){function F(symbol){var f$0 = f(); return [0, f$0];} return F;}
       function test4$0(x){
        var F$0 = F(), M1 = F$0(), M2 = F$0();
        caml_call1(M1[1], 1);
        return caml_call1(M2[1], 2);
       }
       function test4$1(x, cont){
        var F$0 = F(), M1 = F$0(), M2 = F$0();
        return caml_exact_trampoline_cps_call
                (M1[1],
                 1,
                 function(_e_){
                  return caml_exact_trampoline_cps_call(M2[1], 2, cont);
                 });
       }
       var test4 = caml_cps_closure(test4$0, test4$1);
       function recfuncs(x){
        function f(y){return 0 === y ? 1 : x + h(y - 1 | 0) | 0;}
        function h(z){return 0 === z ? 1 : x + f(z - 1 | 0) | 0;}
        var tuple = [0, h, f];
        return tuple;
       }
       function g(){
        function g$0(x){
         var
          tuple = recfuncs(x),
          f = tuple[2],
          h = tuple[1],
          _d_ = h(100),
          _e_ = f(12) + _d_ | 0;
         return caml_call1(Stdlib[44], _e_);
        }
        function g$1(x, cont){
         var
          tuple = recfuncs(x),
          f = tuple[2],
          h = tuple[1],
          _c_ = h(100),
          _d_ = f(12) + _c_ | 0;
         return caml_trampoline_cps_call2(Stdlib[44], _d_, cont);
        }
        var g = caml_cps_closure(g$0, g$1);
        return g;
       }
       function test5$0(param){var g$0 = g(); g$0(42); g$0(- 5); return 0;}
       function test5$1(param, cont){
        var g$0 = g();
        return caml_exact_trampoline_cps_call
                (g$0,
                 42,
                 function(_c_){
                  return caml_exact_trampoline_cps_call
                          (g$0, - 5, function(_c_){return cont(0);});
                 });
       }
       var
        test5 = caml_cps_closure(test5$0, test5$1),
        Test = [0, test1, test2, test3, test4, test5];
       runtime.caml_register_global(7, Test, "Test");
       return;
      }
      (globalThis));
    //end
    |}]
