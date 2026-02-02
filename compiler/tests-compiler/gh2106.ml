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

open Util

let%expect_test "inlining" =
  let p =
    {|
module X : sig
end = struct
  external fun_to_js : (unit -> unit) -> unit = "foo"
  
  let[@tail_mod_cons] rec aux f = f () :: aux f
  
  let map_to_list f = aux (fun x -> f x)
  
  let embedded_input_file_handler _ =
    fun_to_js (fun _ ->
        let _ = map_to_list (fun _ -> assert false) in
        ())
  
  let _ = embedded_input_file_handler ()
end
   |}
  in
  let p = compile_and_parse ~flags:[ "--debug"; "js_assign" ] p in
  print_program p;
  [%expect
    {|
    (function(globalThis){
       "use strict";
       var
        runtime = globalThis.jsoo_runtime,
        caml_maybe_attach_backtrace = runtime.caml_maybe_attach_backtrace,
        Assert_failure = runtime.caml_get_global_data().Assert_failure,
        _a_ = [0, runtime.caml_string_of_jsbytes("test.ml"), 12, 38];
       runtime.foo
        (function(param){
          function f(param){
           throw caml_maybe_attach_backtrace([0, Assert_failure, _a_], 1);
          }
          var block = [0, f(0), 24029], dst = block, offset = 1;
          for(;;){
           var dst$0 = [0, f(0), 24029];
           dst[offset + 1] = dst$0;
           dst = dst$0;
           offset = 1;
          }
         });
       runtime.caml_register_global(2, [0, [0]], "Test");
       return;
      }
      (globalThis));
    //end
    |}]
