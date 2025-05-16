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

let%expect_test _ =
  Util.compile_and_run
    {|
  let log_success () = print_endline "Success!"
  let log_failure = Printf.printf "Failure! %s"

  let direct = ref []

  let indirect = ref []

  let () =
    for i = 0 to 3 do
      let rec f = function
        | 0 -> i
        | -1 -> g (-2) (* deadcode or infinite loop *)
        | n -> g (pred n)
      and g = function
        | 0 -> i
        | -1 -> f (-2) (* deadcode or infinite loop *)
        | n -> f (pred n)
      in
      direct := f i :: !direct;
      indirect := (fun () -> f i) :: !indirect
    done;
    let indirect = List.map (fun f -> f ()) !indirect in
    let direct = !direct in
    assert (indirect = direct)

  let () =
    let delayed = ref (fun () -> ()) in
    for i = 1 to 2 do
      let rec f n = function
        | 0 -> assert (i = n)
        | j ->
            delayed :=
              let prev = !delayed in
              fun () ->
                prev ();
                f (succ n + i - i) (pred j)
      in
      f 0 i
    done;
    !delayed ()

  let _ =
    let l_fun = ref [] in
    let l_var = ref [] in
    let l_base = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10] in
    for i = 0 to 10 do
      l_fun := (fun () -> i) :: !l_fun;
      l_var := i :: !l_var
    done;
    let sum l = List.fold_left ( + ) 0 l in
    let sum_base = sum l_base in
    if sum !l_var <> sum_base
    then log_failure "l_var"
    else if sum (List.map (fun f -> f ()) !l_fun) <> sum_base
    then log_failure "l_fun"
    else log_success ()
|};
  [%expect "Success!"]

let%expect_test _ =
  let program =
    Util.compile_and_parse
      {|
  let log_success () = print_endline "Success!"
  let log_failure = Printf.printf "Failure! %s"

  let direct = ref []

  let indirect = ref []

  let list_map = List.map
  (* Avoid to expose the offset of stdlib modules *)
  let () = ignore (list_map (fun f -> f ()) [])

  let fun1 () =
    for i = 0 to 3 do
      let rec f = function
        | 0 -> i
        | -1 -> g (-2) (* deadcode or infinite loop *)
        | n -> g (pred n)
      and g = function
        | 0 -> i
        | -1 -> f (-2) (* deadcode or infinite loop *)
        | n -> f (pred n)
      in
      direct := f i :: !direct;
      indirect := (fun () -> f i) :: !indirect
    done;
    let indirect = list_map (fun f -> f ()) !indirect in
    let direct = !direct in
    assert (indirect = direct)
|}
  in
  Util.print_fun_decl program (Some "fun1");
  [%expect
    {|
    function fun1(param){
     var i = 0;
     for(;;){
      let i$0 = i;
      var
       f$0 =
         function(counter, n){
          if(- 1 === n){
           var _d_ = - 2;
           if(counter >= 50) return caml_trampoline_return(g$0, [0, _d_]);
           var counter$1 = counter + 1 | 0;
           return g$0(counter$1, _d_);
          }
          if(0 === n) return i$0;
          var _e_ = n - 1 | 0;
          if(counter >= 50) return caml_trampoline_return(g$0, [0, _e_]);
          var counter$0 = counter + 1 | 0;
          return g$0(counter$0, _e_);
         },
       f = function(n){return caml_trampoline(f$1(0, n));},
       g =
         function(counter, n){
          if(- 1 === n){
           var _c_ = - 2;
           if(counter >= 50) return caml_trampoline_return(f$1, [0, _c_]);
           var counter$1 = counter + 1 | 0;
           return f$1(counter$1, _c_);
          }
          if(0 === n) return i$0;
          var _d_ = n - 1 | 0;
          if(counter >= 50) return caml_trampoline_return(f$1, [0, _d_]);
          var counter$0 = counter + 1 | 0;
          return f$1(counter$0, _d_);
         };
      let f$1 = f$0, g$0 = g;
      var _b_ = direct[1];
      direct[1] = [0, f(i), _b_];
      let f$2 = f;
      indirect[1] = [0, function(param){return f$2(i$0);}, indirect[1]];
      var _c_ = i + 1 | 0;
      if(3 === i) break;
      i = _c_;
     }
     var
      indirect$0 =
        caml_call2(list_map, function(f){return caml_call1(f, 0);}, indirect[1]),
      direct$0 = direct[1];
     if(runtime.caml_equal(indirect$0, direct$0)) return 0;
     throw caml_maybe_attach_backtrace([0, Assert_failure, _a_], 1);
    }
    //end
    |}]

let%expect_test _ =
  let prog = {|

let f =
  let my_ref = ref 1 in
  fun () -> incr my_ref; !my_ref
|} in
  let program = Util.compile_and_parse prog in
  Util.print_program program;
  [%expect
    {|
    (function(globalThis){
       "use strict";
       var runtime = globalThis.jsoo_runtime, my_ref = [0, 1];
       function f(param){my_ref[1]++; return my_ref[1];}
       var Test = [0, f];
       runtime.caml_register_global(0, Test, "Test");
       return;
      }
      (globalThis));
    //end |}]
