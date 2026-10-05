(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2026 Jérôme Vouillon
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
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

let test name =
  Printf.printf
    "%s: %s\n"
    (if String.length name > 20 then String.sub name 0 20 ^ "..." else name)
    (match Sys.filepath_exists name with
    | b -> string_of_bool b
    | exception Sys_error _ -> "Sys_error")

let%expect_test _ =
  close_out (open_out "fpe.txt");
  test "fpe.txt";
  test "fpe_missing";
  (* ENOTDIR *)
  test "fpe.txt/x";
  test "fpe_missing/x";
  test ".";
  test "";
  (* ENAMETOOLONG is reported, unlike with Sys.file_exists *)
  let long = String.make 300 'a' in
  test long;
  Printf.printf "file_exists: %b\n" (Sys.file_exists long);
  Sys.remove "fpe.txt";
  test "fpe.txt";
  [%expect
    {|
    fpe.txt: true
    fpe_missing: false
    fpe.txt/x: false
    fpe_missing/x: false
    .: true
    : false
    aaaaaaaaaaaaaaaaaaaa...: Sys_error
    file_exists: false
    fpe.txt: false
    |}]
