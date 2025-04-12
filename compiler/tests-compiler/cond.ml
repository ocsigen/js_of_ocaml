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

open Util

let%expect_test "conditional" =
  let program =
    compile_and_parse
      {|
    let f a b c d e f =
    let x = match a,b,c,d,e,f with
    | true, false, false, false ,false, false -> 1
    | false, false, false, true, false, false -> 4
    | false, true, false, false, false ,false -> 2
    | false, false, true, false, false, false -> 3
    | false, false, false, false, true, false -> 5
    | false, false, false, false, false, true -> 6
    | false, false, false, false, false, false -> 100
    | true, true, _, _, _, _
    | _, true, _, true, _, _
    | _ -> -1
    in x + 2
  |}
  in
  print_fun_decl program (Some "f");
  [%expect
    {|
    function f(a, b, c, d, e, f){
     a:
     {
      if(a){
       if(! b && ! c && ! d && ! e && ! f){var x = 1; break a;}
      }
      else if(b){
       if(! c && ! d && ! e && ! f){var x = 2; break a;}
      }
      else if(c){
       if(! d && ! e && ! f){var x = 3; break a;}
      }
      else if(d){
       if(! e && ! f){var x = 4; break a;}
      }
      else{
       if(! e){if(f){var x = 6; break a;} var x = 100; break a;}
       if(! f){var x = 5; break a;}
      }
      var x = - 1;
     }
     return x + 2 | 0;
    }
    //end
    |}]
