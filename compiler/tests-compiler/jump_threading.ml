(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
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

let disabled = [ "--disable"; "jump-threading" ]

(* Without -g, ocamlc shares the second test of [a && b]: the false edge of
   the first test passes the first condition to the block testing the second
   condition. *)
let%expect_test "&& without -g" =
  let prog =
    {|
let get (a : int array) i = if i >= 0 && i < Array.length a then a.(i) else 0
|}
  in
  print_program (compile_and_parse ~debug:false ~flags:disabled prog);
  [%expect
    {|
    (function(globalThis){
       "use strict";
       var
        runtime = globalThis.jsoo_runtime,
        Test =
          [0,
           function(_c_, _b_){
            var
             _a_ = 0 <= _b_ ? 1 : 0,
             _a_ = _a_ && (_b_ < _c_.length - 1 ? 1 : 0),
             _a_ = _a_ && runtime.caml_check_bound(_c_, _b_)[_b_ + 1];
            return _a_;
           }];
       runtime.caml_register_global(Test, "Test");
       return;
      }
      (globalThis));
    //end
    |}];
  print_program (compile_and_parse ~debug:false prog);
  [%expect
    {|
    (function(globalThis){
       "use strict";
       var
        runtime = globalThis.jsoo_runtime,
        Test =
          [0,
           function(_c_, _b_){
            var _a_ = 0 <= _b_ ? 1 : 0;
            if(_a_){
             _a_ = _b_ < _c_.length - 1 ? 1 : 0;
             _a_ = _a_ && runtime.caml_check_bound(_c_, _b_)[_b_ + 1];
            }
            return _a_;
           }];
       runtime.caml_register_global(Test, "Test");
       return;
      }
      (globalThis));
    //end
    |}]

(* A boolean computed by a cascade of tests, and then tested *)
let%expect_test "boolean cascade" =
  let prog =
    {|
let f c =
  if match c with
     | 'a' .. 'z' | 'A' .. 'Z' -> true
     | '0' .. '9' -> false
     | _ -> '_' = c
  then 1
  else 2
|}
  in
  print_fun_decl (compile_and_parse ~flags:disabled prog) (Some "f");
  [%expect
    {|
           function f(c){
            var _a_ = c - 65 | 0;
            a:
            {
             if(57 < _a_ >>> 0){
              if(9 >= _a_ + 17 >>> 0){_a_ = 0; break a;}
             }
             else if(5 < _a_ - 26 >>> 0){_a_ = 1; break a;}
             _a_ = 95 === c;
            }
            return _a_ ? 1 : 2;
           }
           //end
           |}];
  print_fun_decl (compile_and_parse prog) (Some "f");
  [%expect
    {|
           function f(c){
            var _a_ = c - 65 | 0;
            a:
            {
             b:
             {
              if(57 < _a_ >>> 0){
               if(9 >= _a_ + 17 >>> 0) break b;
              }
              else if(5 < _a_ - 26 >>> 0) break a;
              if(95 === c) break a;
             }
             return 2;
            }
            return 1;
           }
           //end
           |}]

(* An option built in each branch, and then matched: the field read is
   resolved and the allocation is removed. *)
let%expect_test "option matched after a join" =
  let prog =
    {|
let f x y =
  let r =
    if x > 0 then Some (y + 1) else None
  in
  match r with
  | None -> 0
  | Some z -> z * 2
|}
  in
  print_fun_decl (compile_and_parse ~flags:disabled prog) (Some "f");
  [%expect
    {|
           function f(x, y){
            var r = 0 < x ? [0, y + 1 | 0] : 0;
            if(! r) return 0;
            var z = r[1];
            return z * 2 | 0;
           }
           //end
           |}];
  print_fun_decl (compile_and_parse prog) (Some "f");
  [%expect
    {|
           function f(x, y){if(0 >= x) return 0; var z = y + 1 | 0; return z * 2 | 0;}
           //end
           |}]

(* The tested variable is used after the test: it is passed as a parameter
   to the target of the threaded edge. *)
let%expect_test "variable used after the test" =
  let prog =
    {|
let g f x y =
  let r = if x > 0 then Some y else None in
  match r with
  | None -> 0
  | Some _ -> f r
|}
  in
  print_fun_decl (compile_and_parse prog) (Some "g");
  [%expect
    {|
           function g(f, x, y){
            if(0 >= x) return 0;
            var r = [0, y];
            return caml_call1(f, r);
           }
           //end
           |}]

(* Constant constructors built in each branch, and then matched *)
let%expect_test "switch on a constant constructor" =
  let prog =
    {|
type t = A | B | C | D

let f x =
  let k = if x = 0 then A else if x = 1 then B else if x = 2 then C else D in
  match k with
  | A -> "a"
  | B -> "b"
  | C -> "c"
  | D -> "d"
|}
  in
  print_fun_decl (compile_and_parse ~flags:disabled prog) (Some "f");
  [%expect
    {|
           function f(x){
            var k = 0 === x ? 0 : 1 === x ? 1 : 2 === x ? 2 : 3;
            switch(k){
              case 0:
               return cst_a;
              case 1:
               return cst_b;
              case 2:
               return cst_c;
              default: return cst_d;
            }
           }
           //end
           |}];
  print_fun_decl (compile_and_parse prog) (Some "f");
  [%expect
    {|
           function f(x){
            return 0 === x ? cst_a : 1 === x ? cst_b : 2 === x ? cst_c : cst_d;
           }
           //end
           |}]
