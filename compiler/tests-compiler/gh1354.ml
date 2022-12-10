(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2022 Hugo Heuzard
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

(* https://github.com/ocsigen/js_of_ocaml/issues/1354 *)

let%expect_test _ =
  let prog =
    {|
let x = ref 0 in
try
  incr x;
  raise Exit
with Exit ->
  Format.eprintf "%d@." !x|}
  in
  let program =
    Util.compile_and_parse
      ~debug:false
      ~flags:
        (List.concat
           [ [ "--disable"; "inline" ]
           ; [ "--disable"; "deadcode" ]
           ; [ "--disable"; "staticeval" ]
           ])
      prog
  in
  Util.print_program program;
  [%expect
    {|
    (function(globalThis)
       {"use strict";
        var
         runtime=globalThis.jsoo_runtime,
         caml_wrap_exception=runtime.caml_wrap_exception;
        function caml_call2(f,a0,a1)
         {return f.length == 2?f(a0,a1):runtime.caml_call_gen(f,[a0,a1])}
        var
         global_data=runtime.caml_get_global_data(),
         Stdlib=global_data.Stdlib,
         Stdlib_Format=global_data.Stdlib__Format,
         _b_=[0,[4,0,0,0,[17,4,0]],runtime.caml_string_of_jsbytes("%d@.")],
         _a_=0;
        try
         {_a_ + 1 | 0;0;throw Stdlib[3]}
        catch(_c_)
         {_c_ = caml_wrap_exception(_c_);
          if(_c_ !== Stdlib[3])throw _c_;
          caml_call2(Stdlib_Format[131],_b_,_a_);
          var Test=[0];
          runtime.caml_register_global(3,Test,"Test");
          0;
          return}}
      (globalThis));
    //end |}];
  Util.compile_and_run ~debug:false prog;
  [%expect
    {|
    0 |}]
