(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2019 Hugo Heuzard
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

let%expect_test "test-compiler/lib-effects/test1.ml" =
  let program =
    compile_and_parse
      ~effects:true
      {|

open Effect
open Effect.Deep

type _ t += E : unit t

let fff () =
  Printf.printf "%d\n%!" @@
    try_with (fun x -> x) 10
    { effc = (fun (type a) (e : a t) ->
        match e with
        | E -> Some (fun k -> 11)
        | e -> None) }
|}
  in
  print_fun_decl program (Some "fff");
  [%expect
    {|
    function fff(param, cont){
     var
      _b_ =
        [0,
         function(e, cont){
          return e === E
                  ? cont([0, function(k, cont){return cont(11);}])
                  : cont(0);
         }],
      _c_ = 10;
     function _d_(x, cont){return cont(x);}
     var _e_ = Stdlib_Effect[3][5];
     return caml_cps_call4
             (_e_,
              _d_,
              _c_,
              _b_,
              function(_f_){
               var _g_ = Stdlib_Printf[2];
               return caml_cps_call2
                       (_g_,
                        _a_,
                        function(_h_){return caml_cps_call2(_h_, _f_, cont);});
              });
    }
    //end |}]
