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

let%expect_test _ =
  compile_and_run
    ~flags:[ "--setenv"; "A=A" ]
    {|
  (match Sys.getenv "A" with
  | "A" -> ()
  | _ -> print_endline "BUG")|};
  [%expect {||}]

let%expect_test _ =
  compile_and_run
    ~flags:[ "--setenv"; "A=A" ]
    {|
  (match Sys.getenv "B" with
  | exception Not_found -> ()
  | _ -> print_endline "BUG")|};
  [%expect {||}]

let%expect_test _ =
  compile_and_run
    ~flags:[ "--setenv"; "A=A" ]
    {|
  (* fallback for older version of the stdlib *)
  let getenv_opt a = try Some (Sys.getenv a) with Not_found -> None
  open Sys;;
  (match getenv_opt "A" with
  | Some "A" -> ()
  | _ -> print_endline "BUG")|};
  [%expect {||}]

let%expect_test _ =
  compile_and_run
    ~flags:[ "--setenv"; "A=A" ]
    {|
  (* fallback for older version of the stdlib *)

  let getenv_opt a = try Some (Sys.getenv a) with Not_found -> None
  open Sys;;
  (match getenv_opt "B" with
  | None -> ()
  | Some _ -> print_endline "BUG")|};
  [%expect {||}]

let%expect_test _ =
  compile_and_run
    ~flags:[ "--setenv"; "D=±" ]
    {|
  (match Sys.getenv "D" with
    | "\u{00b1}" -> ()
    | _ -> print_endline "BUG")|};
  [%expect {||}]
