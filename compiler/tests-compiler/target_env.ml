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
  let p =
    {|
let () =
  let cwd = Sys.getcwd() in
  if cwd = "/static/"
  then print_endline "without node filesystem"
  else print_endline "with node filesystem";
  try
    let _files = Sys.readdir "." in
    print_endline "Sys.readdir ok";
    let rec get_root s =
      let parent = Filename.dirname s in
      if s = parent then s else get_root parent
    in
    let files = Array.to_list (Sys.readdir (get_root cwd)) in
    (match files with
    | [] -> print_endline "empty root"
    | _ -> ())
  with
  | e ->
    print_endline @@ Printexc.to_string e;
    ()
  |}
  in
  compile_and_run p;
  [%expect {|
    with node filesystem
    Sys.readdir ok|}];
  compile_and_run p ~flags:[ "--target-env=isomorphic" ];
  [%expect {|
    with node filesystem
    Sys.readdir ok|}];
  compile_and_run p ~flags:[ "--target-env=nodejs" ];
  [%expect {|
    with node filesystem
    Sys.readdir ok|}];
  compile_and_run p ~flags:[ "--target-env=browser" ];
  [%expect {|
    without node filesystem
    Sys.readdir ok
    empty root |}]
