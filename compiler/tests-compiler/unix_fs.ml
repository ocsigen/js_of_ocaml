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

let%expect_test "Unix.mkdir_Unix.rmdir" =
  compile_and_run
    {|
  Unix.mkdir "aaa" 0o777;
  Unix.mkdir "aaa/bbb" 0o777;
  let l = Sys.readdir "aaa" |> Array.to_list in
  List.iter print_endline l;
  (match Unix.rmdir "aaa" with
  | exception _ -> ()
  | _ -> print_endline "BUG");
  Unix.rmdir "aaa/bbb";
  Unix.rmdir "aaa"
  |};
  [%expect {|bbb|}]

let%expect_test "Unix.mkdir_Unix.rmdir_static" =
  compile_and_run
    {|
  Sys.chdir "/static/";
  Unix.mkdir "aaa" 0o777;
  Unix.mkdir "aaa/bbb" 0o777;
  let l = Sys.readdir "aaa" |> Array.to_list in
  List.iter print_endline l;
  (match Unix.rmdir "aaa" with
  | exception _ -> ()
  | _ -> print_endline "BUG");
  Unix.rmdir "aaa/bbb";
  Unix.rmdir "aaa";
  |};
  [%expect {|
    bbb|}]

let%expect_test "Unix.mkdir_ENOENT" =
  compile_and_run
    {|
  (match Unix.mkdir "/not/exists" 0o777 with
  | exception Unix.Unix_error (Unix.ENOENT, syscall, path) -> print_endline ("ENOENT: " ^ syscall ^ " " ^ path)
  | exception _ -> print_endline "INCORRECT ERROR"
  | _ -> print_endline "BUG");
  |};
  [%expect {|ENOENT: mkdir /not/exists|}]

let%expect_test "Unix.mkdir_ENOTDIR" =
  compile_and_run
    {|
  let oc = open_out "aaa" in
  output_string oc "bbb";
  close_out oc;
  (match Unix.mkdir "aaa/bbb" 0o777 with
  | exception Unix.Unix_error (Unix.ENOTDIR, syscall, path) -> print_endline ("ENOTDIR: " ^ syscall)
  | exception _ -> print_endline "INCORRECT ERROR"
  | _ -> print_endline "BUG");
  Sys.remove "aaa";
  |};
  [%expect {|ENOTDIR: mkdir|}]

let%expect_test "Unix.rmdir_ENOENT" =
  compile_and_run
    {|
  (match Unix.rmdir "/not/exists" with
  | exception Unix.Unix_error (Unix.ENOENT, syscall, path) -> print_endline ("ENOENT: " ^ syscall ^ " " ^ path)
  | exception _ -> print_endline "INCORRECT ERROR"
  | _ -> print_endline "BUG");
  |};
  [%expect {|ENOENT: rmdir /not/exists|}]

let%expect_test "Unix.rmdir_ENOTDIR" =
  compile_and_run
    {|
  Unix.mkdir "aaa" 0o777;
  let oc = open_out "aaa/bbb" in
  output_string oc "ccc";
  close_out oc;
  (match Unix.rmdir "aaa/bbb" with
  | exception Unix.Unix_error (Unix.ENOTDIR, syscall, path) -> print_endline ("ENOTDIR: " ^ syscall)
  | exception _ -> print_endline "INCORRECT ERROR"
  | _ -> print_endline "BUG");
  Sys.remove "aaa/bbb";
  Unix.rmdir "aaa";
  |};

  [%expect {|ENOTDIR: rmdir|}]

let%expect_test "Unix.stat_file" =
  compile_and_run
    {|
  let oc = open_out "aaa" in
  output_string oc "bbb";
  close_out oc;
  (match Unix.stat "aaa" with
  | exception _ -> print_endline "UNEXPECTED ERROR"
  | { st_kind = Unix.S_REG; st_size } ->
      print_string "File size: ";
      print_int st_size;
  | _ -> print_endline "BUG");
  Sys.remove "aaa"
  |};
  [%expect {|File size: 3|}]

let%expect_test "Unix.stat_dir" =
  compile_and_run
    {|
  Unix.mkdir "aaa" 0o777;
  (match Unix.stat "aaa" with
  | exception _ -> print_endline "UNEXPECTED ERROR"
  | { st_kind = Unix.S_DIR } -> print_string "Found dir"
  | _ -> print_endline "BUG");
  Unix.rmdir "aaa"
  |};
  [%expect {|Found dir|}]

let%expect_test "Unix.stat_symlink" =
  compile_and_run
    {|
  let oc = open_out "aaa" in
  output_string oc "bbb";
  close_out oc;
  Unix.symlink "aaa" "ccc";
  (match Unix.stat "ccc" with
  | exception _ -> print_endline "UNEXPECTED ERROR"
  | { st_kind = Unix.S_REG; st_size } ->
    print_string "File size: ";
    print_int st_size;
  | _ -> print_endline "BUG");
  Sys.remove "ccc";
  Sys.remove "aaa";
  |};
  [%expect {|File size: 3|}]

let%expect_test "Unix.symlink_Unix.readlink" =
  compile_and_run
    {|
  let oc = open_out "aaa" in
  output_string oc "bbb";
  close_out oc;
  Unix.symlink "aaa" "ccc";
  (match Unix.readlink "ccc" with
  | exception _ -> print_endline "UNEXPECTED ERROR"
  | path -> (
    let ic = open_in path in
    let contents = input_line ic in
    print_endline contents;
    close_in ic));
  Sys.remove "ccc";
  Sys.remove "aaa";
  |};
  [%expect {|bbb|}]

let%expect_test "Unix.readlink_EINVAL" =
  compile_and_run
    {|
  (match Unix.readlink "." with
  | exception Unix.Unix_error (Unix.EINVAL, syscall, path) -> print_endline ("EINVAL: " ^ syscall)
  | exception _ -> print_endline "INCORRECT ERROR"
  | _ -> print_endline "BUG");
  |};
  [%expect {|EINVAL: readlink|}]

let%expect_test "Unix.lstat_file" =
  compile_and_run
    {|
  let oc = open_out "aaa" in
  output_string oc "bbb";
  close_out oc;
  (match Unix.lstat "aaa" with
  | exception _ -> print_endline "UNEXPECTED ERROR"
  | { st_kind = Unix.S_REG; st_size } ->
    print_string "File size: ";
    print_int st_size;
  | _ -> print_endline "BUG");
  Sys.remove "aaa";
  |};
  [%expect {|File size: 3|}]

let%expect_test "Unix.lstat_symlink" =
  compile_and_run
    {|
  let oc = open_out "aaa" in
  output_string oc "bbb";
  close_out oc;
  Unix.symlink "aaa" "ccc";
  (match Unix.lstat "ccc" with
  | exception _ -> print_endline "UNEXPECTED ERROR"
  | { st_kind = Unix.S_LNK; st_size } -> print_endline "Found link"
  | _ -> print_endline "BUG");
  Sys.remove "ccc";
  Sys.remove "aaa";
  |};
  [%expect {|Found link|}]
