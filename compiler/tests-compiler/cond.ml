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
       if(! c && ! d && ! e && ! f){x = 2; break a;}
      }
      else if(c){
       if(! d && ! e && ! f){x = 3; break a;}
      }
      else if(d){
       if(! e && ! f){x = 4; break a;}
      }
      else{
       if(! e){if(f){x = 6; break a;} x = 100; break a;}
       if(! f){x = 5; break a;}
      }
      x = - 1;
     }
     return x + 2 | 0;
    }
    //end
    |}]

let%expect_test "conditional" =
  let program =
    compile_and_parse
      {|
type rip_relative_kind =
| Explicitly_rip_relative
| Implicitly_rip_relative
| Not_rip_relative

(** val rip_relative_kind_beq :
    rip_relative_kind -> rip_relative_kind -> bool **)

let rip_relative_kind_beq x y =
  match x with
  | Explicitly_rip_relative ->
    (match y with
     | Explicitly_rip_relative -> true
     | Implicitly_rip_relative -> false
     | Not_rip_relative -> false)
  | Implicitly_rip_relative ->
    (match y with
     | Explicitly_rip_relative -> false
     | Implicitly_rip_relative -> true
     | Not_rip_relative -> false)
  | Not_rip_relative ->
    (match y with
     | Explicitly_rip_relative -> false
     | Implicitly_rip_relative -> false
     | Not_rip_relative -> true)
       |}
  in
  print_fun_decl program (Some "rip_relative_kind_beq");
  [%expect
    {|
    function rip_relative_kind_beq(x, y){
     switch(x){
       case 0:
        return 0 === y ? 1 : 0;
       case 1:
        return 1 === y ? 1 : 0;
       default: return 2 === y ? 1 : 0;
     }
    }
    //end
    |}]

let%expect_test "conditional" =
  let program =
    compile_and_parse
      {|
type rip_relative_kind =
| Explicitly_rip_relative
| Implicitly_rip_relative
| Not_rip_relative

(** val rip_relative_kind_beq :
    rip_relative_kind -> rip_relative_kind -> bool **)

let rip_relative_kind_beq x y =
  let i = match x with
  | Explicitly_rip_relative ->
    (match y with
     | Explicitly_rip_relative -> 1
     | Implicitly_rip_relative -> 2
     | Not_rip_relative -> 2)
  | Implicitly_rip_relative ->
    (match y with
     | Explicitly_rip_relative -> 2
     | Implicitly_rip_relative -> 1
     | Not_rip_relative -> 2)
  | Not_rip_relative ->
    (match y with
     | Explicitly_rip_relative -> 2
     | Implicitly_rip_relative -> 2
       | Not_rip_relative -> 1)
   in print_int i
       |}
  in
  print_fun_decl program (Some "rip_relative_kind_beq");
  [%expect
    {|
    function rip_relative_kind_beq(x, y){
     switch(x){
       case 0:
        var i = 0 === y ? 1 : 2; break;
       case 1:
        i = 1 === y ? 1 : 2; break;
       default: i = 2 === y ? 1 : 2;
     }
     return caml_call1(Stdlib[44], i);
    }
    //end
    |}]
