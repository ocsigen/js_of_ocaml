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

(* https://github.com/ocsigen/js_of_ocaml/issues/739 *)

let%expect_test _ =
  compile_and_run
    {|
    let r = ref false
    let f x = match Obj.is_int x with
              | true -> r := true; true
              | false -> r := false; false

    let print_bool b = print_endline (string_of_bool b)
    let () =
      print_string "[not (is_int 1)]: ";
      print_bool (not (f (Obj.repr 1)));
      print_string "[is_int (1,2,3)]: ";
      print_bool (f (Obj.repr (1, 2, 3)))
  |};
  [%expect {|
    [not (is_int 1)]: false
    [is_int (1,2,3)]: false
  |}]
