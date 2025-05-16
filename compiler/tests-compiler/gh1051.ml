(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2020 Hugo Heuzard
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

(* https://github.com/ocsigen/js_of_ocaml/issues/1051 *)

let prog = {|let () = Printf.printf "%nx" 0xffffffffn;;|}

let%expect_test _ =
  Util.compile_and_run ~werror:false prog;
  [%expect
    {|
    Warning: integer overflow: native integer 0xffffffff (4294967295) truncated to 0xffffffff (-1); the generated code might be incorrect.
    ffffffff
    |}];
  ()

let%expect_test _ =
  Util.print_fun_decl (Util.compile_and_parse ~werror:false prog) None;
  [%expect
    {|
    Warning: integer overflow: native integer 0xffffffff (4294967295) truncated to 0xffffffff (-1); the generated code might be incorrect.
    function caml_call2(f, a0, a1){
     return (f.l >= 0 ? f.l : f.l = f.length) === 2
             ? f(a0, a1)
             : runtime.caml_call_gen(f, [a0, a1]);
    }
    //end
    |}];
  ()
