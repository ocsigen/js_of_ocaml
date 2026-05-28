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
   merge-node targets dominated by a single block. These are compiled as a
   tower of nested labelled blocks, one level per target, so the
   statement-nesting depth grows with the number of distinct continuations.
   For a large lexer this nests hundreds of levels deep, which overflows the
   recursive-descent parser of some JavaScript engines (e.g. Firefox /
   SpiderMonkey: "too much recursion" at load time, before any code runs).
   See #2122. *)

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

let%expect_test "match with many shared continuations" =
  compile_and_run prog;
  [%expect {| if=1 in=2 match=3 module=4 open=5 while=0 then=1 function=3 |}];
  let program = compile_and_parse prog in
  print_fun_decl program (Some "kind");
  [%expect
    {|
    function kind(param){
     var _b_ = runtime.caml_string_compare(param, cst_let);
     a:
     {
      b:
      {
       c:
       {
        d:
        {
         if(0 <= _b_){
          if(0 >= _b_) break c;
          if(caml_string_notequal(param, cst_match)){
           if(caml_string_notequal(param, cst_module)){
            if(! caml_string_notequal(param, cst_open)) break b;
            if(! caml_string_notequal(param, cst_then)) break d;
            if(caml_string_notequal(param, cst_type)) break a;
           }
           return 4;
          }
         }
         else{
          if(! caml_string_notequal(param, cst_else)) break d;
          if
           (caml_string_notequal(param, cst_fun)
            && caml_string_notequal(param, cst_function)){
           if(! caml_string_notequal(param, cst_if)) break d;
           if(! caml_string_notequal(param, cst_in)) break c;
           if(caml_string_notequal(param, cst_include)) break a;
           break b;
          }
         }
         return 3;
        }
        return 1;
       }
       return 2;
      }
      return 5;
     }
     return 0;
    }
    //end
    |}]
