(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2017 Hugo Heuzard
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

(* https://github.com/ocsigen/js_of_ocaml/issues/177 *)
(* https://github.com/ocsigen/js_of_ocaml/pull/178 *)

let%expect_test _ =
  let prog =
    Util.compile_and_parse
      ~flags:[]
      {|
  let i = ref 0
  let log_success () = print_endline "Success!"
  let log_failure = Printf.printf "Failure! %s"

  let side_effect yes label =
    if yes
    then (
      Printf.printf "Side effect: %s\n%!" label;
      incr i);
    0

  let _ = side_effect false "this is only to avoid inlining"

  let f =
    match side_effect true "Should only see this once" with
    | 0 | 1 | 2 -> Printf.printf "Please don't optimize this away\n%!"
    | _ -> Printf.printf "Or this\n%!"

  let _ = if !i = 1 then log_success () else log_failure "side effect computed twice"
  |}
  in
  Util.print_program prog;
  [%expect
    {|
    (function(globalThis){
       "use strict";
       var
        runtime = globalThis.jsoo_runtime,
        caml_string_of_jsbytes = runtime.caml_string_of_jsbytes;
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
       var
        global_data = runtime.caml_get_global_data(),
        Stdlib_Printf = global_data.Stdlib__Printf,
        Stdlib = global_data.Stdlib,
        i = [0, 0],
        cst_Success = caml_string_of_jsbytes("Success!");
       function log_success(param){return caml_call1(Stdlib[46], cst_Success);}
       var
        log_failure =
          caml_call1
           (Stdlib_Printf[2],
            [0,
             [11, caml_string_of_jsbytes("Failure! "), [2, 0, 0]],
             caml_string_of_jsbytes("Failure! %s")]),
        _a_ =
          [0,
           [11,
            caml_string_of_jsbytes("Side effect: "),
            [2, 0, [12, 10, [10, 0]]]],
           caml_string_of_jsbytes("Side effect: %s\n%!")];
       function side_effect(yes, label){
        if(yes){caml_call2(Stdlib_Printf[2], _a_, label); i[1]++;}
        return 0;
       }
       side_effect(0, caml_string_of_jsbytes("this is only to avoid inlining"));
       var
        _b_ =
          [0,
           [11, caml_string_of_jsbytes("Or this\n"), [10, 0]],
           caml_string_of_jsbytes("Or this\n%!")],
        _c_ =
          [0,
           [11,
            caml_string_of_jsbytes("Please don't optimize this away\n"),
            [10, 0]],
           caml_string_of_jsbytes("Please don't optimize this away\n%!")],
        cst_side_effect_computed_twice =
          caml_string_of_jsbytes("side effect computed twice"),
        f =
          2
           <
            side_effect(1, caml_string_of_jsbytes("Should only see this once"))
            >>> 0
           ? caml_call1(Stdlib_Printf[2], _b_)
           : caml_call1(Stdlib_Printf[2], _c_);
       if(1 === i[1])
        log_success(0);
       else
        caml_call1(log_failure, cst_side_effect_computed_twice);
       var Test = [0, i, log_success, log_failure, side_effect, f];
       runtime.caml_register_global(10, Test, "Test");
       return;
      }
      (globalThis));
    //end
    |}];
  [%expect {| |}];
  Util.compile_and_run
    ~flags:[]
    {|
  let i = ref 0
  let log_success () = print_endline "Success!"
  let log_failure = Printf.printf "Failure! %s"

  let side_effect yes label =
    if yes
    then (
      Printf.printf "Side effect: %s\n%!" label;
      incr i);
    0

  let _ = side_effect false "this is only to avoid inlining"

  let f =
    match side_effect true "Should only see this once" with
    | 0 | 1 | 2 -> Printf.printf "Please don't optimize this away\n%!"
    | _ -> Printf.printf "Or this\n%!"

  let _ = if !i = 1 then log_success () else log_failure "side effect computed twice"
  |};
  [%expect
    {|
    Side effect: Should only see this once
    Please don't optimize this away
    Success! |}]
