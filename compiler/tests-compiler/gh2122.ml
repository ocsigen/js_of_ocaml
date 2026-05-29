(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2024 Hugo Heuzard
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
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

(* A match with many branches sharing continuations (here, or-patterns over
   strings, as in a menhir/ocamllex-generated lexer) produces many sibling
   merge-node targets dominated by a single block. By default these are
   compiled as a tower of nested labelled blocks, one level per target, so the
   statement-nesting depth grows with the number of distinct continuations.
   For a large lexer this nests hundreds of levels deep, which overflows the
   recursive-descent parser of some JavaScript engines (e.g. Firefox /
   SpiderMonkey: "too much recursion" at load time, before any code runs).
   See #2122.

   With [merge_node_max] lowered, the same block is instead compiled as a flat
   dispatch loop ([for(;;)] around a [switch]) whose statement-nesting depth is
   constant. *)

open Util

let prog =
  {|
let kind = function
  | "if" | "then" | "else" -> 1
  | "let" | "in" -> 2
  | "fun" | "function" | "match" -> 3
  | "type" | "module" -> 4
  | "open" | "include" -> 5
  | _ -> 0

let () =
  List.iter
    (fun s -> Printf.printf "%s=%d " s (kind s))
    [ "if"; "in"; "match"; "module"; "open"; "while"; "then"; "function" ];
  print_newline ()
|}

let%expect_test "match with many shared continuations is a flat dispatch loop" =
  (* Force the flat dispatch loop on the [kind] match. *)
  let flags = [ "--set=merge_node_max=2" ] in
  compile_and_run ~flags prog;
  [%expect {| if=1 in=2 match=3 module=4 open=5 while=0 then=1 function=3 |}];
  let program = compile_and_parse ~flags prog in
  print_fun_decl program (Some "kind");
  [%expect
    {|
    function kind(param){
     var _c_ = runtime.caml_string_compare(param, cst_let), _b_ = 0;
     for(;;){
      switch(_b_){
        case 0:
         if(0 <= _c_){
          if(0 >= _c_){_b_ = 3; continue;}
          if(caml_string_notequal(param, cst_match)){
           if(caml_string_notequal(param, cst_module)){
            if(! caml_string_notequal(param, cst_open)){_b_ = 2; continue;}
            if(! caml_string_notequal(param, cst_then)){_b_ = 4; continue;}
            if(caml_string_notequal(param, cst_type)){_b_ = 1; continue;}
           }
           return 4;
          }
         }
         else{
          if(! caml_string_notequal(param, cst_else)){_b_ = 4; continue;}
          if
           (caml_string_notequal(param, cst_fun)
            && caml_string_notequal(param, cst_function)){
           if(! caml_string_notequal(param, cst_if)){_b_ = 4; continue;}
           if(! caml_string_notequal(param, cst_in)){_b_ = 3; continue;}
           if(caml_string_notequal(param, cst_include)){_b_ = 1; continue;}
           _b_ = 2;
           continue;
          }
         }
        case 5:
         return 3;
        case 4:
         return 1;
        case 3:
         return 2;
        case 2:
         return 5;
        case 1:
         return 0;
      }
      break;
     }
    }
    //end
    |}]

let%expect_test "the default (nested) scheme computes the same result" =
  compile_and_run prog;
  [%expect {| if=1 in=2 match=3 module=4 open=5 while=0 then=1 function=3 |}]

(* Non-independent scopes: the match result is used downstream, so the
   per-value scopes are not self-contained -- each one assigns the result and
   then flows into the shared join scope that computes [n * 100 + length].

   The cases are laid out so that the join sits right after the last result
   scope, which therefore falls through into it ([case 2: n = 0;] below)
   instead of re-entering the loop. The other result scopes are not adjacent to
   the join, so they still route through it ([n = <v>; sel = <join>; continue]),
   just as they would all [break] to the join in the nested scheme. *)
let score_prog =
  {|
let score s =
  let n =
    match s with
    | "if" | "then" | "else" -> 1
    | "let" | "in" -> 2
    | "fun" | "function" | "match" -> 3
    | "type" | "module" -> 4
    | "open" | "include" -> 5
    | _ -> 0
  in
  n * 100 + String.length s

let () =
  List.iter
    (fun s -> Printf.printf "%d " (score s))
    [ "if"; "in"; "match"; "module"; "open"; "while" ];
  print_newline ()
|}

let%expect_test "non-independent scopes fall through to the join where adjacent" =
  let flags = [ "--set=merge_node_max=2" ] in
  compile_and_run ~flags score_prog;
  [%expect {| 102 202 305 406 504 5 |}];
  let program = compile_and_parse ~flags score_prog in
  print_fun_decl program (Some "score");
  [%expect
    {|
    function score(s){
     var _c_ = runtime.caml_string_compare(s, cst_let), _b_ = 0;
     for(;;){
      switch(_b_){
        case 0:
         if(0 <= _c_){
          if(0 >= _c_){_b_ = 4; continue;}
          if(caml_string_notequal(s, cst_match)){
           if(caml_string_notequal(s, cst_module)){
            if(! caml_string_notequal(s, cst_open)){_b_ = 3; continue;}
            if(! caml_string_notequal(s, cst_then)){_b_ = 5; continue;}
            if(caml_string_notequal(s, cst_type)){_b_ = 2; continue;}
           }
           var n = 4;
           _b_ = 1;
           continue;
          }
         }
         else{
          if(! caml_string_notequal(s, cst_else)){_b_ = 5; continue;}
          if
           (caml_string_notequal(s, cst_fun)
            && caml_string_notequal(s, cst_function)){
           if(! caml_string_notequal(s, cst_if)){_b_ = 5; continue;}
           if(! caml_string_notequal(s, cst_in)){_b_ = 4; continue;}
           if(caml_string_notequal(s, cst_include)){_b_ = 2; continue;}
           _b_ = 3;
           continue;
          }
         }
        case 6:
         n = 3; _b_ = 1; continue;
        case 5:
         n = 1; _b_ = 1; continue;
        case 4:
         n = 2; _b_ = 1; continue;
        case 3:
         n = 5; _b_ = 1; continue;
        case 2: n = 0;
        case 1:
         return (n * 100 | 0) + runtime.caml_ml_string_length(s) | 0;
      }
      break;
     }
    }
    //end
    |}]

let%expect_test "non-independent scopes: both schemes compute the same result" =
  compile_and_run score_prog;
  [%expect {| 102 202 305 406 504 5 |}]
