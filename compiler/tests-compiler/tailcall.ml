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
    function fun1(param)
     {function odd$0(counter,x)
       {if(0 === x)return 0;
        var _g_=x - 1 | 0;
        if(counter < 50)
         {var counter$0=counter + 1 | 0;return even$0(counter$0,_g_)}
        return caml_trampoline_return(even$0,[0,_g_])}
      function even$0(counter,x)
       {if(0 === x)return 1;
        var _f_=x - 1 | 0;
        if(counter < 50)
         {var counter$0=counter + 1 | 0;return odd$0(counter$0,_f_)}
        return caml_trampoline_return(odd$0,[0,_f_])}
      function odd(x){return caml_trampoline(odd$0(0,x))}
      function even(x){return caml_trampoline(even$0(0,x))}
      var _c_=even(1);
      if(odd(1) !== _c_)
       try
        {odd(5000);var _d_=log_success(0);return _d_}
       catch(_e_){return caml_call1(log_failure,cst_too_much_recursion)}
      throw [0,Assert_failure,_b_]} |}]
