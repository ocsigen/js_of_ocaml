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

(* https://github.com/ocsigen/js_of_ocaml/issues/827 *)

let%expect_test _ =
  let program =
    compile_and_parse
      {|
[@@@ocaml.warning "-26-27"]
let some_name () =
try raise Not_found with
| f ->
      (try fun g -> 0 with h -> fun h -> raise Not_found)
        (let o =
           ( try
               fun a ->
                 try try fun c -> 0 with q -> raise (Not_found) with
                 | f ->
                     raise Not_found
             with
           | h ->
             (raise Not_found) )
             ()
         in
         try
           let m = try [] with j -> [] in
           true
         with
         | s ->
             true)
                   |}
  in
  print_fun_decl program (Some "some_name");
  [%expect
    {|
    function some_name(param){
     try{throw caml_maybe_attach_backtrace(Stdlib[8], 1);}catch(f){return 0;}
    }
    //end
    |}]
