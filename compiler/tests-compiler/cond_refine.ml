(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2026 Hugo Heuzard
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
 * along with this program; if not, should be able to obtain it from
 * https://www.gnu.org/licenses/.
 *)

(* In the false branch of a conditional, the tested variable is known
   to be 0; in branch [i] of a switch, it is known to be [i]. *)

let%expect_test "the tested value is known in the false branch" =
  let prog =
    {|
let rec for_all p = function [] -> true | a :: l -> p a && for_all p l
let valid i = (0 <= i && i <= 0xD7FF) || (0xE000 <= i && i <= 0x10FFFF)
let sw x = match x with 0 -> x + 10 | 1 -> x * 7 | 2 -> x - 5 | _ -> x
|}
  in
  let program = Util.compile_and_parse prog in
  Util.print_fun_decl program (Some "for_all");
  Util.print_fun_decl program (Some "valid");
  Util.print_fun_decl program (Some "sw");
  [%expect
    {|
    function for_all(p, _a_){
     for(;;){
      if(! _a_) return 1;
      var l = _a_[2], a = _a_[1];
      if(! caml_call1(p, a)) return 0;
      _a_ = l;
     }
    }
    //end
    function valid(i){
     var _a_ = 0 <= i ? i <= 55295 ? 1 : 0 : 0;
     if(! _a_) _a_ = 57344 <= i ? i <= 1114111 ? 1 : 0 : 0;
     return _a_;
    }
    //end
    function sw(x){
     if(2 < x >>> 0) return x;
     switch(x){
       case 0:
        return 10 | 0;
       case 1:
        return 1 * 7 | 0;
       default: return - 3 | 0;
     }
    }
    //end
    |}]

(* The false branch of the conditional loops back to the block that
   performs the test, which is also the entry point of a closure: the
   tested variable must not be replaced there. *)
let%expect_test "loop back to the tested block" =
  let prog =
    {|
let eq : int -> int -> bool = Sys.opaque_identity (fun a b -> a = b)
let rename subst =
  let rec rename x = let y = subst.(x) in if eq x y then y else rename y in
  rename
let () = print_int (rename [| 1; 2; 2 |] 0); print_newline ()
|}
  in
  Util.compile_and_run prog;
  [%expect {| 2 |}]
