(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2022 Hugo Heuzard, Jérôme Vouillon
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

(* https://github.com/ocsigen/js_of_ocaml/issues/1320 *)

let%expect_test _ =
  let prog =
    {|
let app f x = f x

let myfun () =
  for i = 1 to 4 do
    let rec f x = if x = 0 then 1 else i * app g (x - 1) and g x = app f x in
    Printf.eprintf "%d\n" (g i)
  done

let () = myfun ()
|}
  in
  Util.compile_and_run prog;
  [%expect {|
    1
    4
    27
    256 |}];
  let program = Util.compile_and_parse prog in
  Util.print_fun_decl program (Some "myfun");
  [%expect
    {|
    function myfun(param){
     var i = 1;
     for(;;){
      var
       closures =
         function(i){
          function g(x){return app(f, x);}
          function f(x){
           return 0 === x ? 1 : runtime.caml_mul(i, app(g, x - 1 | 0));
          }
          var block = [0, g, f];
          return block;
         },
       closures$0 = closures(i),
       g = closures$0[1],
       _b_ = g(i);
      caml_call2(Stdlib_Printf[3], _a_, _b_);
      var _c_ = i + 1 | 0;
      if(4 === i) return 0;
      var i = _c_;
     }
    }
    //end |}]
