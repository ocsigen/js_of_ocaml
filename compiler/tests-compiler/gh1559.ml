(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2024 Hugo Heuzard
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

(* https://github.com/ocsigen/js_of_ocaml/issues/1559 *)

let%expect_test _ =
  let prog =
    {|
let my_ref = ref 1

module _ : sig end = struct
  type 'a thing =
    | Thing of 'a
    | No

  let f2 t =
    match t with
    | Thing 1 -> true
    | Thing _ | No -> false
  ;;

  let length = function
    | Thing i -> i
    | No -> -1
  ;;

  let () =
    let init = Thing 1 in
    let nesting = 1 in
    let rec handle_state t =
      let this_will_be_undefined () = if f2 t then 1 else 2 in
      match length t with
      | 0 -> this_will_be_undefined ()
      | 1 -> if Stdlib.Int.equal nesting 0 then nesting else this_will_be_undefined ()
      | _ -> handle_state (Thing 0)
    in
    print_endline (Int.to_string (handle_state init))
  ;;

  let _ : _ thing = No
end

let () = my_ref := 2
|}
  in
  Util.compile_and_run prog;
  [%expect {|
    1 |}];
  let program = Util.compile_and_parse prog in
  Util.print_program program;
  [%expect
    {|
    (function(globalThis){
       "use strict";
       var runtime = globalThis.jsoo_runtime;
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
        t$0 = [0, 0],
        init = [0, 1],
        Stdlib_Int = global_data.Stdlib__Int,
        Stdlib = global_data.Stdlib,
        my_ref = [0, 1],
        nesting = 1;
       a:
       {
        var t = init;
        for(;;){
         let t$1 = t;
         var
          this_will_be_undefined =
            function(param){var _b_ = 1 === t$1[1] ? 1 : 0; return _b_ ? 1 : 2;},
          i = t[1];
         if(0 === i){var _a_ = this_will_be_undefined(0); break a;}
         if(1 === i) break;
         t = t$0;
        }
        var
         _a_ =
           caml_call2(Stdlib_Int[8], nesting, 0)
            ? nesting
            : this_will_be_undefined(0);
       }
       var _b_ = caml_call1(Stdlib_Int[12], _a_);
       caml_call1(Stdlib[46], _b_);
       my_ref[1] = 2;
       var Test = [0, my_ref];
       runtime.caml_register_global(4, Test, "Test");
       return;
      }
      (globalThis));
    //end
    |}]

let%expect_test _ =
  let prog =
    {|
let my_ref = ref 1

module _ : sig end = struct
  type 'a thing =
    | Thing of 'a
    | No

  let f2 t =
    match t with
    | Thing 1 -> true
    | Thing _ | No -> false
  ;;

  let length = function
    | Thing i -> i
    | No -> -1
  ;;

  let () =
    let init = Thing 1 in
    let nesting = 1 in
    let rec handle_state t =
      let this_will_be_undefined () = if f2 t then 1 else 2 in
      match length t with
      | 0 ->
        let g () = 2 + this_will_be_undefined () in
        g () + g ()
      | 1 ->
        if Stdlib.Int.equal nesting 0
        then nesting
        else
          let g () = if Random.int 3 > 1 then 2 + this_will_be_undefined () else 1 in
          g () + g ()
      | _ -> handle_state (Thing 0)
    in
    print_endline (Int.to_string (handle_state init))
  ;;

  let _ : _ thing = No
end

let () = my_ref := 2
|}
  in
  Util.compile_and_run prog;
  [%expect {|
    2 |}];
  let program = Util.compile_and_parse prog in
  Util.print_program program;
  [%expect
    {|
    (function(globalThis){
       "use strict";
       var runtime = globalThis.jsoo_runtime;
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
        t$0 = [0, 0],
        init = [0, 1],
        Stdlib_Random = global_data.Stdlib__Random,
        Stdlib_Int = global_data.Stdlib__Int,
        Stdlib = global_data.Stdlib,
        my_ref = [0, 1],
        nesting = 1;
       a:
       {
        b:
        {
         var t = init;
         for(;;){
          let t$1 = t;
          var
           this_will_be_undefined =
             function(param){var _d_ = 1 === t$1[1] ? 1 : 0; return _d_ ? 1 : 2;},
           i = t[1];
          if(0 === i) break;
          if(1 === i) break b;
          t = t$0;
         }
         var
          g = function(param){return 2 + this_will_be_undefined(0) | 0;},
          _b_ = g(0),
          _a_ = g(0) + _b_ | 0;
         break a;
        }
        if(caml_call2(Stdlib_Int[8], nesting, 0))
         var _a_ = nesting;
        else
         var
          g$0 =
            function(param){
             return 1 < caml_call1(Stdlib_Random[5], 3)
                     ? 2 + this_will_be_undefined(0) | 0
                     : 1;
            },
          _c_ = g$0(0),
          _a_ = g$0(0) + _c_ | 0;
       }
       var _d_ = caml_call1(Stdlib_Int[12], _a_);
       caml_call1(Stdlib[46], _d_);
       my_ref[1] = 2;
       var Test = [0, my_ref];
       runtime.caml_register_global(5, Test, "Test");
       return;
      }
      (globalThis));
    //end
    |}]
