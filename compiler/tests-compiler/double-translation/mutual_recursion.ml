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

(* Mutually tail-recursive functions which do not need CPS only get a
   direct-style version, with the counter-based trampoline used without
   effects. *)
let%expect_test "mutual tail recursion without CPS with --effects=double-translation" =
  let code =
    compile_and_parse
      ~effects:`Double_translation
      {|
        let rec ping n acc = if n = 0 then acc else pong (n - 1) (acc + 1)
        and pong n acc = if n = 0 then acc else ping (n - 1) (acc + 1)
      |}
  in
  print_program code;
  [%expect
    {|
    (function(globalThis){
       "use strict";
       var
        runtime = globalThis.jsoo_runtime,
        caml_trampoline = runtime.caml_trampoline,
        caml_trampoline_return = runtime.caml_trampoline_return;
       function ping$0(counter, n, acc){
        if(0 === n) return acc;
        var _b_ = acc + 1 | 0, _c_ = n - 1 | 0;
        return counter < 50
                ? pong$0(counter + 1 | 0, _c_, _b_)
                : caml_trampoline_return(pong$0, [0, _c_, _b_]);
       }
       function ping(n, acc){return caml_trampoline(ping$0(0, n, acc));}
       function pong$0(counter, n, acc){
        if(0 === n) return acc;
        var _a_ = acc + 1 | 0, _b_ = n - 1 | 0;
        return counter < 50
                ? ping$0(counter + 1 | 0, _b_, _a_)
                : caml_trampoline_return(ping$0, [0, _b_, _a_]);
       }
       function pong(n, acc){return caml_trampoline(pong$0(0, n, acc));}
       runtime.caml_register_global([0, ping, pong], "Test");
       return;
      }
      (globalThis));
    //end
    |}]

[@@@if ocaml_version >= (5, 0, 0)]

(* Mutually tail-recursive cps_needed closures (here, because [ping]
   performs an effect) get both a direct and a CPS version (paired via
   caml_cps_closure). The direct half is a wrapper that drives a
   caml_direct_trampoline loop, and each recursive tail call between the
   siblings is gated on caml_stack_check_depth, bouncing through
   caml_trampoline_return when the JS stack budget is exhausted. This mirrors
   the CPS-side trampoline and keeps deep direct-style mutual recursion from
   overflowing the JS stack. *)
let%expect_test "mutual tail recursion with CPS with --effects=double-translation" =
  let code =
    compile_and_parse
      ~effects:`Double_translation
      {|
        [@@@alert "-unsafe_effects"] (* OxCaml warns about [Effect.perform] *)
        type _ Effect.t += E : unit Effect.t
        let rec ping n acc =
          if n = 0 then (Effect.perform E; acc) else pong (n - 1) (acc + 1)
        and pong n acc = if n = 0 then acc else ping (n - 1) (acc + 1)
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
        caml_direct_trampoline = runtime.caml_direct_trampoline,
        caml_stack_check_depth = runtime.caml_stack_check_depth,
        caml_trampoline_return = runtime.caml_trampoline_return;
       function caml_exact_trampoline_cps_call(f, a0, a1, a2){
        return runtime.caml_stack_check_depth()
                ? f.cps ? f.cps.call(null, a0, a1, a2) : a2(f(a0, a1))
                : runtime.caml_trampoline_return(f, [a0, a1, a2], 0);
       }
       var
        E =
          [248,
           runtime.caml_string_of_jsbytes("Test.E"),
           runtime.caml_fresh_oo_id(0)];
       function ping$1(n, acc){
        if(0 === n){runtime.caml_raise_unhandled(E); return acc;}
        var _b_ = acc + 1 | 0, _c_ = n - 1 | 0;
        return caml_stack_check_depth()
                ? pong$1(_c_, _b_)
                : caml_trampoline_return(pong$1, [_c_, _b_], 1);
       }
       function ping$0(n, acc, cont){
        return 0 === n
                ? runtime.caml_perform_effect(E, function(_b_){return cont(acc);})
                : caml_exact_trampoline_cps_call
                  (pong, n - 1 | 0, acc + 1 | 0, cont);
       }
       var
        ping =
          caml_cps_closure
           (function(n, acc){return caml_direct_trampoline(ping$1, [n, acc]);},
            ping$0);
       function pong$1(n, acc){
        if(0 === n) return acc;
        var _a_ = acc + 1 | 0, _b_ = n - 1 | 0;
        return caml_stack_check_depth()
                ? ping$1(_b_, _a_)
                : caml_trampoline_return(ping$1, [_b_, _a_], 1);
       }
       function pong$0(n, acc, cont){
        return 0 === n
                ? cont(acc)
                : caml_exact_trampoline_cps_call
                  (ping, n - 1 | 0, acc + 1 | 0, cont);
       }
       var
        pong =
          caml_cps_closure
           (function(n, acc){return caml_direct_trampoline(pong$1, [n, acc]);},
            pong$0);
       runtime.caml_register_global([0, E, ping, pong], "Test");
       return;
      }
      (globalThis));
    //end
    |}]
