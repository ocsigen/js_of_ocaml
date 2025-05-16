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

(* https://github.com/ocsigen/js_of_ocaml/issues/400 *)
(* https://github.com/ocsigen/js_of_ocaml/pull/402 *)

let%expect_test _ =
  Util.compile_and_run
    {|
  exception A
  exception B of int

  let a_exn () = raise A

  (* Make sure that [a] doesn't look constant *)
  let a () = if Random.int 1 + 1 = 0 then 2 else 4

  let b_exn () = raise (B 2)

  (* https://github.com/ocsigen/js_of_ocaml/issues/400
   * match .. with exception is no compiled properly *)
  let () =
    assert (
      try
        match a () with
        | exception (A | B _) -> true
        | _n -> b_exn ()
      with B _ -> true);
  print_endline "Success!"
|};
  [%expect "Success!"]

open Util

let%expect_test "static eval of string get" =
  let program =
    compile_and_parse
      {|
exception A of int

let fun1 () =
  match Random.int 2 with
  | 0 as i | exception A (2 as i) -> i
  | i -> i+1
  | exception A i -> i+2

let fun2 () =
  match Random.int 2 with
  | 0 as i | exception A (2 as i) -> i
  | i -> i+1

  |}
  in
  print_fun_decl program (Some "fun1");
  print_fun_decl program (Some "fun2");
  [%expect
    {|
    function fun1(param){
     a:
     {
      try{var i$1 = caml_call1(Stdlib_Random[5], 2);}
      catch(exn$0){
       var exn = caml_wrap_exception(exn$0), tag = exn[1];
       if(tag !== A) throw caml_maybe_attach_backtrace(exn, 0);
       var i = exn[2];
       if(2 !== i) return i + 2 | 0;
       var i$0 = i;
       break a;
      }
      if(0 !== i$1) return i$1 + 1 | 0;
      var i$0 = i$1;
     }
     return i$0;
    }
    //end
    function fun2(param){
     a:
     {
      try{var i$0 = caml_call1(Stdlib_Random[5], 2);}
      catch(_c_){
       var _a_ = caml_wrap_exception(_c_);
       if(_a_[1] === A){var _b_ = _a_[2]; if(2 === _b_){var i = _b_; break a;}}
       throw caml_maybe_attach_backtrace(_a_, 0);
      }
      if(0 !== i$0) return i$0 + 1 | 0;
      var i = i$0;
     }
     return i;
    }
    //end
    |}]
