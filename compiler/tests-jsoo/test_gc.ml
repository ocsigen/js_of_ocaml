(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2023 Hugo Heuzard
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

let print_bool b = print_endline (string_of_bool b)

let ocaml_version =
  match String.split_on_char '.' Sys.ocaml_version with
  | major :: minor :: _ ->
      let major = int_of_string major and minor = int_of_string minor in
      major, minor
  | _ -> assert false

let ok () = print_endline "OK"

let ko size = Printf.printf "size=%d, ocaml_version=%s" size Sys.ocaml_version

let%expect_test "stat" =
  let s = Gc.stat () in
  let size = Obj.size (Obj.repr s) in
  (match size with
  | 17 when ocaml_version >= (4, 12) -> ok ()
  | 16 when ocaml_version < (4, 12) -> ok ()
  | _ -> ko size);
  [%expect {| OK |}]

let%expect_test "quick_stat" =
  let s = Gc.quick_stat () in
  let size = Obj.size (Obj.repr s) in
  (match size with
  | 17 when ocaml_version >= (4, 12) -> ok ()
  | 16 when ocaml_version < (4, 12) -> ok ()
  | _ -> ko size);
  [%expect {| OK |}]

let%expect_test "control" =
  let s = Gc.get () in
  let size = Obj.size (Obj.repr s) in
  (match size with
  | 11 when ocaml_version >= (4, 8) -> ok ()
  | _ -> ko size);
  [%expect {| OK |}]
