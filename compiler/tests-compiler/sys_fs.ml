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
let f () =
  Sys.mkdir "aaa" 0o777;
  let oc = open_out "aaa/bbb" in
  close_out oc;
  (try ignore(Sys.readdir "aaa/bb") with
  | Sys_error _ -> ()
  | e -> print_endline (Printexc.to_string e));
  (try ignore(Sys.readdir "aaa/bbb") with
  | Sys_error _ -> ()
  | e -> print_endline (Printexc.to_string e));
  Sys.remove "aaa/bbb";
  Sys.rmdir "aaa"
in
f (); Sys.chdir "/static"; f ()
  |};
  [%expect {| |}]

let%expect_test _ =
  compile_and_run
    {|
let f () =
  Sys.mkdir "aaa" 0o777;
  Sys.mkdir "aaa/bbb" 0o777;
  let l = Sys.readdir "aaa" |> Array.to_list in
  List.iter print_endline l;
  (match Sys.rmdir "aaa" with
  | exception _ -> ()
  | _ -> print_endline "BUG");
  Sys.rmdir "aaa/bbb";
  Sys.rmdir "aaa"
in
f (); Sys.chdir "/static"; f ()
  |};
  [%expect {|
    bbb
    bbb|}]

let%expect_test _ =
  compile_and_run
    {|
  (match Sys.mkdir "/not/exists" 0o777 with
  | exception Sys_error path -> print_endline "EXPECTED ERROR"
  | exception err -> print_endline (Printexc.to_string err)
  | _ -> print_endline "BUG");
  |};
  [%expect {|EXPECTED ERROR|}]

let%expect_test _ =
  compile_and_run
    {|
  (match Sys.rmdir "/not/exists" with
  | exception Sys_error path -> print_endline "EXPECTED ERROR"
  | exception err -> print_endline (Printexc.to_string err)
  | _ -> print_endline "BUG");
  |};
  [%expect {|EXPECTED ERROR|}]
