(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2025 Hugo Heuzard
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

let%expect_test _ =
  let prog =
    {|

     let f o = o#a

     let o1 = object
       method a = print_endline "a from o1"
     end

     let o2 = object
       method b = ()
       method a = print_endline "a from o2"
     end

     let () = f o1; f o2
|}
  in
  Util.compile_and_run prog;
  [%expect {|
    a from o1
    a from o2
    |}];
  let program = Util.compile_and_parse prog in
  Util.print_var_decl program "cache_id";
  Util.print_fun_decl program (Some "f");
  [%expect
    {|
    var cache_id = runtime.caml_oo_cache_id();
    //end
    function f(o){
     return caml_call1(runtime.caml_get_public_method(o, 97, cache_id), o);
    }
    //end
    |}]
