(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2021 Hugo Heuzard
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
let () =
  try
    let _files = Sys.readdir "." in
    print_endline "Sys.readdir ok - isomorphic compile"
  with
  | e ->
    print_endline @@ Printexc.to_string e;
    ()
  |};
  [%expect {|Sys.readdir ok - isomorphic compile|}]

let%expect_test _ =
  compile_and_run
    ~flags:[ "--target-env=nodejs" ]
    {|
let () =
  try
    let _files = Sys.readdir "." in
    print_endline "Sys.readdir ok - nodejs compile"
  with
  | e ->
    print_endline @@ Printexc.to_string e;
    ()
  |};
  [%expect {|Sys.readdir ok - nodejs compile|}]

let%expect_test _ =
  compile_and_run
    ~flags:[ "--target-env=browser" ]
    {|
let () =
  try
    let _files = Sys.readdir "." in
    print_endline "Sys.readdir ok - browser compile"
  with
  | e ->
    print_endline @@ Printexc.to_string e;
    ()
  |};
  [%expect {| Sys_error*No such file or directory* (glob) |}]
