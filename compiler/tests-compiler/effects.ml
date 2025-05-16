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
      ~effects:`Cps
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
     return caml_trampoline_cps_call4
             (Stdlib_Effect[3][5],
              function(x, cont){return cont(x);},
              10,
              [0,
               function(e, cont){
                return e === E
                        ? cont([0, function(k, cont){return cont(11);}])
                        : cont(0);
               }],
              function(_b_){
               return caml_trampoline_cps_call2
                       (Stdlib_Printf[2],
                        _a_,
                        function(_c_){
                         return caml_trampoline_cps_call2(_c_, _b_, cont);
                        });
              });
    }
    //end
    |}]
