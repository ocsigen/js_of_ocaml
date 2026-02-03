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

(* https://github.com/ocsigen/js_of_ocaml/commit/a1a24b53e3e25af30b30e2e1779991db1055143e *)

let%expect_test _ =
  let prog =
    {|
    let log_success () = print_endline "Success!"
    let log_failure = Printf.printf "Failure! %s"

    let fun1 () =
      let rec odd x = if x = 0 then false else even (x - 1)
      and even x = if x = 0 then true else odd (x - 1) in
      assert (odd 1 <> even 1);
      try
        ignore (odd 5000);
        log_success ()
      with _ -> log_failure "too much recursion"

    let () = fun1 ()
    |}
  in
  Util.compile_and_run prog;
  [%expect {| Success! |}];
  let program = Util.compile_and_parse prog in
  Util.print_fun_decl program (Some "fun1");
  [%expect
    {|
    function fun1(_b_){
     function odd$0(counter, x){
      if(0 === x) return 0;
      var _b_ = x - 1 | 0;
      return counter < 50
              ? even$0(counter + 1 | 0, _b_)
              : caml_trampoline_return(even$0, [0, _b_]);
     }
     function odd(x){return caml_trampoline(odd$0(0, x));}
     function even$0(counter, x){
      if(0 === x) return 1;
      var _b_ = x - 1 | 0;
      return counter < 50
              ? odd$0(counter + 1 | 0, _b_)
              : caml_trampoline_return(odd$0, [0, _b_]);
     }
     function even(x){return caml_trampoline(even$0(0, x));}
     var _b_ = even(1);
     if(odd(1) === _b_)
      throw caml_maybe_attach_backtrace([0, Assert_failure, _a_], 1);
     try{odd(5000); _b_ = log_success(0); return _b_;}
     catch(exn){return caml_call1(log_failure, cst_too_much_recursion);}
    }
    //end
    |}]

let%expect_test _ =
  let prog =
    {|
    let log_success () = print_endline "Success!"
    let log_failure = Printf.printf "Failure! %s"

    let fun1 () =
      let rec odd x = if x = 0 then false else even (x - 1)
      and even x = if x = 0 then true else odd (x - 1) in
      assert (odd 1 <> even 1);
      try
        ignore (odd 5000);
        log_success ()
      with _ -> log_failure "too much recursion"

    let () = fun1 ()
    |}
  in
  Util.compile_and_run prog;
  [%expect {| Success! |}];
  let program = Util.compile_and_parse ~flags:[ "--set"; "tc_depth=0" ] prog in
  Util.print_fun_decl program (Some "fun1");
  [%expect
    {|
    function fun1(_b_){
     function odd$0(x){
      return 0 === x ? 0 : caml_trampoline_return(even$0, [0, x - 1 | 0]);
     }
     function odd(x){return caml_trampoline(odd$0(x));}
     function even$0(x){
      return 0 === x ? 1 : caml_trampoline_return(odd$0, [0, x - 1 | 0]);
     }
     function even(x){return caml_trampoline(even$0(x));}
     var _b_ = even(1);
     if(odd(1) === _b_)
      throw caml_maybe_attach_backtrace([0, Assert_failure, _a_], 1);
     try{odd(5000); _b_ = log_success(0); return _b_;}
     catch(exn){return caml_call1(log_failure, cst_too_much_recursion);}
    }
    //end
    |}]

let%expect_test "global-deadcode-bug" =
  let prog =
    {|
     let log_success () = print_endline "Success!"
     type t = { a : bool; b : bool }
     let fun1 () =
     let g f = if f.b then true else false in
     let f x = print_endline "here"; g { a = true; b = false} in
     let _ = (f 5000) in
     log_success ()
     let () = fun1 ()
    |}
  in
  Util.compile_and_run ~flags:[ "--disable"; "inline" ] prog;
  [%expect {|
    here
    Success!
    |}];
  let program = Util.compile_and_parse ~flags:[ "--disable"; "inline" ] prog in
  Util.print_fun_decl program (Some "fun1");
  [%expect
    {|
    function fun1(param){
     function f(x){caml_call1(Stdlib[46], cst_here);}
     f(5000);
     return log_success(0);
    }
    //end
    |}]

let%expect_test "_" =
  let prog =
    {|
type t =
  | Zero
  | Succ of t

let rules (rules : t -> t) : t -> t =
  function
  | Zero -> Succ Zero
  | Succ n -> Succ (rules n)

let rec step n =
  rules step n

let rec grow (iters : int) (n : t) : t =
  if iters < 0 then n else
    (grow [@tailcall]) (iters - 1) (step n)
|}
  in
  let program = Util.compile_and_parse ~flags:[ "--debug"; "js_assign" ] prog in
  Util.print_fun_decl program (Some "grow");
  [%expect
    {|
    function grow(iters$1, n$1){
     var iters = iters$1, n = n$1;
     for(;;){
      if(0 > iters) return n;
      var n$0 = step(n), iters$0 = iters - 1 | 0;
      iters = iters$0;
      n = n$0;
     }
    }
    //end
    |}]
