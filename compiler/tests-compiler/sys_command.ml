(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2020 Seb Mondet
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

let%expect_test _ =
  compile_and_run
    {|
  (match Sys.command "printf hello" with
  | 0 -> ()
  | _ -> print_endline "BUG")|};
  [%expect {|hello|}]

let%expect_test _ =
  compile_and_run
    {|
  (match Sys.command "printf hello >&2" with
  | 0 -> ()
  | _ -> print_endline "BUG")|};
  [%expect {|hello|}]

(* The following tests are disable because they don't play well on Windows.
   They should eventually be fixed *)

(* let%expect_test _ =
 *   compile_and_run
 *     {|
 *   (match Sys.command "ls /does-not-exist 2> /dev/null" with
 *   | 0 -> print_endline "BUG"
 *   | _ -> ())|};
 *   [%expect {||}]
 *
 * let%expect_test _ =
 *   compile_and_run
 *     {|
 *   (match Sys.command "ls / > /dev/null" with
 *   | 0 -> ()
 *   | _ -> print_endline "BUG")|};
 *   [%expect {||}]
 *
 * let%expect_test _ =
 *   compile_and_run
 *     {|
 *   (match Sys.command "{ printf hello ; printf world ; } >&2" with
 *   | 0 -> ()
 *   | _ -> print_endline "BUG")|};
 *   [%expect {|helloworld|}] *)
