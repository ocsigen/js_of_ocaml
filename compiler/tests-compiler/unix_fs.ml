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
    ~unix:true
    {|
let f () =
  Unix.mkdir "aaa" 0o777;
  Unix.mkdir "aaa/bbb" 0o777;
  let l = Sys.readdir "aaa" |> Array.to_list in
  List.iter print_endline l;
  (match Unix.rmdir "aaa" with
  | exception (Unix.Unix_error (Unix.ENOTEMPTY, _, _)) -> ()
  | exception e -> print_endline (Printexc.to_string e)
  | _ -> print_endline "UNEXPECTED SUCCESS");
  Unix.rmdir "aaa/bbb";
  Unix.rmdir "aaa"
in
f ();Sys.chdir "/static"; f ()|};
  [%expect {|
    bbb
    bbb|}]

let%expect_test "Unix.mkdir_ENOENT" =
  compile_and_run
    ~unix:true
    {|
let f () =
  (match Unix.mkdir "aaa/bbb/ccc" 0o777 with
  | exception Unix.Unix_error (Unix.ENOENT, syscall, path) -> print_endline ("ENOENT: " ^ syscall)
  | exception err -> print_endline (Printexc.to_string err)
  | _ -> print_endline "UNEXPECTED SUCCESS")
in
f (); Sys.chdir "/static"; f ()|};
  [%expect {|
    ENOENT: mkdir
    ENOENT: mkdir|}]

let%expect_test "Unix.mkdir_ENOTDIR" =
  compile_and_run
    ~unix:true
    {|
let f () =
  let oc = open_out "aaa" in
  output_string oc "bbb";
  close_out oc;
  (match Unix.mkdir "aaa/bbb" 0o777 with
  (* Linux & Mac raise Unix.ENOTDIR *)
  | exception Unix.Unix_error (Unix.ENOTDIR, syscall, path) -> print_endline ("EXPECTED ERROR: " ^ syscall)
  (* Windows raises Unix.ENOENT *)
  | exception Unix.Unix_error (Unix.ENOENT, syscall, path) -> print_endline ("EXPECTED ERROR: " ^ syscall)
  | exception err -> print_endline (Printexc.to_string err)
  | _ -> print_endline "UNEXPECTED SUCCESS");
  Sys.remove "aaa"
in
f (); Sys.chdir "/static"; f () |};
  [%expect {|
    EXPECTED ERROR: mkdir
    EXPECTED ERROR: mkdir|}]

let%expect_test "Unix.rmdir_ENOENT" =
  compile_and_run
    ~unix:true
    {|
let f () =
  (match Unix.rmdir "aaa/bbb/ccc" with
  | exception Unix.Unix_error (Unix.ENOENT, syscall, path) -> print_endline ("ENOENT: " ^ syscall)
  | exception err -> print_endline (Printexc.to_string err)
  | _ -> print_endline "UNEXPECTED SUCCESS")
in
f (); Sys.chdir "/static"; f () |};
  [%expect {|
    ENOENT: rmdir
    ENOENT: rmdir|}]

let%expect_test "Unix.rmdir_ENOTDIR" =
  compile_and_run
    ~unix:true
    {|
let f () =
  Unix.mkdir "aaa" 0o777;
  let oc = open_out "aaa/bbb" in
  output_string oc "ccc";
  close_out oc;
  (match Unix.rmdir "aaa/bbb" with
  (* Linux & Mac raise Unix.ENOTDIR *)
  | exception Unix.Unix_error (Unix.ENOTDIR, syscall, path) -> print_endline ("EXPECTED ERROR: " ^ syscall)
  (* Windows raises Unix.ENOENT *)
  | exception Unix.Unix_error (Unix.ENOENT, syscall, path) -> print_endline ("EXPECTED ERROR: " ^ syscall)
  | exception err -> print_endline (Printexc.to_string err)
  | _ -> print_endline "UNEXPECTED SUCCESS");
  Sys.remove "aaa/bbb";
  Unix.rmdir "aaa"
in
f (); Sys.chdir "/static"; f () |};
  [%expect {|
    EXPECTED ERROR: rmdir
    EXPECTED ERROR: rmdir|}]

let%expect_test "Unix.stat_file" =
  compile_and_run
    ~unix:true
    {|
let f () =
  let oc = open_out "aaa" in
  output_string oc "bbb";
  close_out oc;
  (match Unix.stat "aaa" with
  | exception err -> print_endline (Printexc.to_string err)
  | { st_kind = Unix.S_REG; st_size } ->
      print_string "File size: ";
      print_int st_size;
      print_newline ();
  | _ -> print_endline "UNEXPECTED SUCCESS");
  Sys.remove "aaa"
in
f (); Sys.chdir "/static"; f () |};
  [%expect {|
    File size: 3
    Failure("unix_stat: not implemented")|}]

let%expect_test "Unix.stat_dir" =
  compile_and_run
    ~unix:true
    {|
let f () =
  Unix.mkdir "aaa" 0o777;
  (match Unix.stat "aaa" with
  | exception err -> print_endline (Printexc.to_string err)
  | { st_kind = Unix.S_DIR } -> print_endline "Found dir"
  | _ -> print_endline "UNEXPECTED SUCCESS");
  Unix.rmdir "aaa"
in
f (); Sys.chdir "/static"; f () |};
  [%expect {|
    Found dir
    Failure("unix_stat: not implemented")|}]

let%expect_test "Unix.stat_symlink" =
  compile_and_run
    ~unix:true
    {|
let f () =
  let oc = open_out "aaa" in
  output_string oc "bbb";
  close_out oc;
  (try Unix.symlink "aaa" "ccc" with
  | err -> print_endline (Printexc.to_string err));
  (match Unix.stat "ccc" with
  | exception err -> print_endline (Printexc.to_string err)
  | { st_kind = Unix.S_REG; st_size } ->
    print_string "File size: ";
    print_int st_size;
    print_newline ();
  | _ -> print_endline "UNEXPECTED SUCCESS");
  (try Sys.remove "ccc" with | _ -> ());
  Sys.remove "aaa";
in
f (); Sys.chdir "/static"; f () |};
  [%expect
    {|
    File size: 3
    Failure("unix_symlink: not implemented")
    Failure("unix_stat: not implemented")|}]

let%expect_test "Unix.symlink_Unix.readlink" =
  compile_and_run
    ~unix:true
    {|
let f () =
  let oc = open_out "aaa" in
  output_string oc "bbb";
  close_out oc;
  (try Unix.symlink "aaa" "ccc" with
  | err -> print_endline (Printexc.to_string err));
  (match Unix.readlink "ccc" with
  | exception err -> print_endline (Printexc.to_string err)
  | path -> (
    let ic = open_in path in
    let contents = input_line ic in
    print_endline contents;
    close_in ic));
  (try Sys.remove "ccc" with | _ -> ());
  Sys.remove "aaa";
in
f (); Sys.chdir "/static"; f () |};
  [%expect
    {|
    bbb
    Failure("unix_symlink: not implemented")
    Failure("unix_readlink: not implemented")|}]

let%expect_test "Unix.readlink_EINVAL" =
  compile_and_run
    ~unix:true
    {|
let f () =
  (match Unix.readlink "." with
  (* Linux & Mac raise Unix.EINVAL *)
  | exception Unix.Unix_error (Unix.EINVAL, syscall, path) -> print_endline "EXPECTED ERROR"
  (* Windows raises Unix.EUNKNOWNERR *)
  | exception Unix.Unix_error (Unix.EUNKNOWNERR(code), syscall, path) -> print_endline "EXPECTED ERROR"
  | exception err -> print_endline (Printexc.to_string err)
  | _ -> print_endline "UNEXPECTED SUCCESS");
in
f (); Sys.chdir "/static"; f () |};
  [%expect {|
    EXPECTED ERROR
    Failure("unix_readlink: not implemented")|}]

let%expect_test "Unix.lstat_file" =
  compile_and_run
    ~unix:true
    {|
let f () =
  let oc = open_out "aaa" in
  output_string oc "bbb";
  close_out oc;
  (match Unix.lstat "aaa" with
  | exception err -> print_endline (Printexc.to_string err)
  | { st_kind = Unix.S_REG; st_size } ->
    print_string "File size: ";
    print_int st_size;
    print_newline ();
  | _ -> print_endline "UNEXPECTED SUCCESS");
  Sys.remove "aaa";
in
f (); Sys.chdir "/static"; f () |};
  [%expect {|
    File size: 3
    Failure("unix_lstat: not implemented")|}]

let%expect_test "Unix.lstat_symlink" =
  compile_and_run
    ~unix:true
    {|
let f () =
  let oc = open_out "aaa" in
  output_string oc "bbb";
  close_out oc;
  (try Unix.symlink "aaa" "ccc" with
  | err -> print_endline (Printexc.to_string err));
  (match Unix.lstat "ccc" with
  | exception err -> print_endline (Printexc.to_string err)
  | { st_kind = Unix.S_LNK; st_size } -> print_endline "Found link"
  | _ -> print_endline "UNEXPECTED SUCCESS");
  (try Sys.remove "ccc" with | _ -> ());
  Sys.remove "aaa";
in
f (); Sys.chdir "/static"; f () |};
  [%expect
    {|
    Found link
    Failure("unix_symlink: not implemented")
    Failure("unix_lstat: not implemented")|}]
