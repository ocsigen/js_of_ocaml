(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2020 Hugo Heuzard
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

(* https://github.com/ocsigen/js_of_ocaml/issues/1007 *)

(* Small bug in the global flow analysis in fast mode *)
let%expect_test _ =
  let prog =
    {|
let () =
  let bug () = let g = ref (fun x -> Fun.id) in (fun () -> !g 1), g in
  let h f =
    let (h, g) = f() in g := (fun x y -> y); Printf.printf "%d\n" (h () 7) in
  h bug; h bug
|}
  in
  Util.compile_and_run prog;
  [%expect {|
    7
    7 |}];
  let program = Util.compile_and_parse prog in
  Util.print_fun_decl program (Some "bug");
  [%expect
    {|
    function bug(param){
     var g = [0, function(x){return function(_b_){return _b_;};}];
     return [0, function(param){return caml_call1(g[1], 1);}, g];
    }
    //end
    |}]
