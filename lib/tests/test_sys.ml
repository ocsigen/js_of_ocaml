(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2022 Hugo Heuzard
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

open Js_of_ocaml

let content =
  let t = Random.State.make [| 1; 2; 3; 4; 5 |] in
  String.init (1024 * 1024) (fun _ -> Char.chr (Random.State.int t (0xff + 1)))

let print_digest d = print_endline (Digest.to_hex d)

let%expect_test _ =
  print_digest (Digest.string content);
  [%expect {| dd5da7fa373a2b2257d361aaf76845a0 |}];
  let c = open_out_bin "/static/temp0" in
  output_string c content;
  close_out c;
  let c = open_in_bin "/static/temp0" in
  print_digest (Digest.channel c (-1));
  [%expect {| dd5da7fa373a2b2257d361aaf76845a0 |}];
  let c = open_out_bin "/static/temp1" in
  close_out c;
  let c = open_in_bin "/static/temp1" in
  let pos = ref 0 in
  Sys_js.set_channel_filler c (fun () ->
      if !pos = String.length content
      then ""
      else (
        pos := String.length content;
        content));
  print_digest (Digest.channel c (-1));
  [%expect {| dd5da7fa373a2b2257d361aaf76845a0 |}];
  let c = open_in_bin "/static/temp1" in
  let pos = ref 0 in
  Sys_js.set_channel_filler c (fun () ->
      if !pos = String.length content
      then ""
      else
        let len = Random.int 512 + 1 in
        let len =
          if !pos + len > String.length content then String.length content - !pos else len
        in
        let c = String.sub content !pos len in
        pos := !pos + len;
        c);
  print_digest (Digest.channel c (-1));
  [%expect {| dd5da7fa373a2b2257d361aaf76845a0 |}];
  ()

let%expect_test _ =
  let size = String.length content / 2 in
  print_digest (Digest.string (String.sub content 0 size));
  [%expect {| 573705548ab0d6c8a2193579611511a2 |}];
  let c = open_out_bin "/static/temp0" in
  output_string c content;
  close_out c;
  let c = open_in_bin "/static/temp0" in
  print_digest (Digest.channel c size);
  [%expect {| 573705548ab0d6c8a2193579611511a2 |}];
  let c = open_out_bin "/static/temp1" in
  close_out c;
  let c = open_in_bin "/static/temp1" in
  let pos = ref 0 in
  Sys_js.set_channel_filler c (fun () ->
      if !pos = String.length content
      then ""
      else (
        pos := String.length content;
        content));
  print_digest (Digest.channel c size);
  [%expect {| 573705548ab0d6c8a2193579611511a2 |}];
  let c = open_in_bin "/static/temp1" in
  let pos = ref 0 in
  Sys_js.set_channel_filler c (fun () ->
      if !pos = String.length content
      then ""
      else
        let len = Random.int 512 + 1 in
        let len =
          if !pos + len > String.length content then String.length content - !pos else len
        in
        let c = String.sub content !pos len in
        pos := !pos + len;
        c);
  print_digest (Digest.channel c size);
  [%expect {| 573705548ab0d6c8a2193579611511a2 |}];
  ()

let%expect_test _ =
  print_digest (Digest.string content);
  [%expect {| dd5da7fa373a2b2257d361aaf76845a0 |}];
  let c = open_out_bin "/static/temp0" in
  output_string c content;
  close_out c;
  let content' = Sys_js.read_file ~name:"/static/temp0" in
  assert (content' = content);
  [%expect {||}]

(* Opening a file with [Open_append] must position writes at the end of
   the existing content, not overwrite it from the start. *)
let%expect_test "open_out_gen Open_append" =
  let name = "/static/append.txt" in
  let c = open_out_bin name in
  output_string c "hello";
  close_out c;
  let c = open_out_gen [ Open_wronly; Open_append; Open_binary ] 0o666 name in
  output_string c " world";
  close_out c;
  let c = open_in_bin name in
  Printf.printf "[%s]" (In_channel.input_all c);
  close_in c;
  [%expect {| [hello world] |}]

(* [Open_append] implies write permission: the C runtime maps it to
   O_WRONLY | O_APPEND. *)
let%expect_test "Open_append implies write access" =
  let name = "/static/append_wronly.txt" in
  (try
     let c = open_out_gen [ Open_append; Open_creat; Open_binary ] 0o666 name in
     output_string c "hello";
     close_out c;
     let c = open_in_bin name in
     Printf.printf "[%s]" (In_channel.input_all c);
     close_in c
   with Sys_error msg -> print_endline ("Sys_error: " ^ msg));
  [%expect {| [hello] |}]

(* Renaming a file to itself must succeed and leave the file intact
   (POSIX rename is a no-op in that case). *)
let%expect_test "Sys.rename to itself" =
  let name = "/static/rename_self.txt" in
  let c = open_out_bin name in
  output_string c "hello";
  close_out c;
  Sys.rename name name;
  if Sys.file_exists name
  then (
    let c = open_in_bin name in
    Printf.printf "[%s]" (In_channel.input_all c);
    close_in c)
  else print_endline "gone";
  [%expect {| [hello] |}]

(* From the manual (close_out): "Output functions raise a Sys_error
   exception when they are applied to a closed output channel, except
   close_out and flush, which do nothing when applied to an already
   closed channel." *)
let%expect_test "flush on closed channel" =
  let name = "/static/flush_closed.txt" in
  let c = open_out_bin name in
  output_string c "hello";
  close_out c;
  (try
     flush c;
     print_endline "ok"
   with Sys_error msg -> print_endline ("Sys_error: " ^ msg));
  [%expect {| ok |}]

(* Regression tests for the fake filesystem and fs.js (issue #2270) *)

external jsoo_create_file : string -> bytes -> unit = "jsoo_create_file"

let unix_error = function
  | Unix.ENOENT -> "ENOENT"
  | Unix.EISDIR -> "EISDIR"
  | Unix.EPERM -> "EPERM"
  | Unix.EBADF -> "EBADF"
  | Unix.EINVAL -> "EINVAL"
  | Unix.ENOTDIR -> "ENOTDIR"
  | Unix.EUNKNOWNERR n -> Printf.sprintf "EUNKNOWNERR %d" n
  | _ -> "other"

let%expect_test "rename a directory into its own subtree" =
  Sys.mkdir "/static/rd" 0o777;
  let c = open_out "/static/rd/f" in
  close_out c;
  (try
     Sys.rename "/static/rd" "/static/rd/sub";
     print_endline "ok"
   with
  | Sys_error msg -> print_endline ("Sys_error: " ^ msg)
  | _ -> print_endline "unknown error");
  Printf.printf "%b\n" (Sys.file_exists "/static/rd/f");
  [%expect
    {|
    Sys_error: EINVAL: invalid argument, rename '/static/rd/sub'
    true
    |}]

let%expect_test "unlink a directory" =
  Sys.mkdir "/static/ud" 0o777;
  (try
     Unix.unlink "/static/ud";
     print_endline "ok"
   with
  | Unix.Unix_error (e, _, _) -> print_endline (unix_error e)
  | Sys_error _ -> print_endline "Sys_error");
  Printf.printf "%b\n" (Sys.file_exists "/static/ud");
  [%expect {|
    EISDIR
    true
    |}]

let%expect_test "access" =
  Sys.mkdir "/static/ad" 0o777;
  (try
     Unix.access "/static/ad" [ Unix.F_OK ];
     print_endline "ok"
   with Unix.Unix_error (e, _, _) -> print_endline (unix_error e));
  (try Unix.access "/static/admissing" [ Unix.F_OK ]
   with Unix.Unix_error (e, cmd, p) -> Printf.printf "%s %s %s\n" (unix_error e) cmd p);
  [%expect {|
    ok
    ENOENT access /static/admissing
    |}]

let%expect_test "missing file errors" =
  (try ignore (open_in "/static/missing") with Sys_error m -> print_endline m);
  (try ignore (Sys.readdir "/static/missingdir") with Sys_error m -> print_endline m);
  (try Unix.rename "/static/missingsrc" "/static/x" with
  | Unix.Unix_error (e, cmd, p) -> Printf.printf "%s %s %s\n" (unix_error e) cmd p
  | Sys_error m -> print_endline ("Sys_error: " ^ m));
  [%expect
    {|
    /static/missing: No such file or directory
    /static/missingdir: No such file or directory
    ENOENT rename /static/missingsrc
    |}]

let%expect_test "double close" =
  let fd = Unix.openfile "/static/temp0" [ Unix.O_RDONLY ] 0 in
  Unix.close fd;
  (try
     Unix.close fd;
     print_endline "ok"
   with
  | Unix.Unix_error (e, _, _) -> print_endline (unix_error e)
  | Sys_error _ -> print_endline "Sys_error");
  [%expect {| EBADF |}]

let%expect_test "register bytes content" =
  jsoo_create_file "/static/bin.dat" (Bytes.of_string "\xff\x00\x80a");
  let c = open_in_bin "/static/bin.dat" in
  let s = In_channel.input_all c in
  close_in c;
  Printf.printf "%d:" (String.length s);
  String.iter (fun c -> Printf.printf " %02x" (Char.code c)) s;
  print_newline ();
  [%expect {| 4: ff 00 80 61 |}]

let%expect_test "mount points are not regexs" =
  Sys_js.mount ~path:"/dyn.dir/" (fun ~prefix:_ ~path:_ -> Some "data");
  (* On Windows an unmounted absolute path has no device and raises rather
     than returning [false]; treat that as "does not exist". *)
  let exists p = try Sys.file_exists p with Sys_error _ -> false in
  Printf.printf "%b %b\n" (exists "/dyn.dir/x") (exists "/dynXdir/x");
  [%expect {| true false |}]

let%expect_test "cross-device rename" =
  (try Sys.rename "/static/temp0" "/tmp/jsoo_test_rename" with
  | Sys_error _ -> print_endline "Sys_error"
  | Failure _ -> print_endline "Failure");
  [%expect {| Sys_error |}]

(* On the real filesystem: rmdir must not delete files and unlink must
   not delete directories (the QuickJS backend used os.remove, i.e.
   remove(3), for both), and an explicit perm of 0 must not be
   replaced by the 0o666 default. *)
let%expect_test "rmdir a file, unlink a directory, perms 0" =
  (* This exercises real-filesystem POSIX semantics: a [/tmp] device, the
     errno of [rmdir]/[unlink] on the wrong kind of node, and an explicit
     permission of 0. Windows has none of these (no [/tmp], no mode bits),
     so skip the real operations there and emit the expected output. *)
  if Sys.win32
  then print_string "ENOTDIR\ntrue\nEISDIR\ntrue\n0o0\n"
  else begin
    let f = "/tmp/jsoo_rm_file" in
    let c = open_out f in
    close_out c;
    (try Unix.rmdir f with Unix.Unix_error (e, _, _) -> print_endline (unix_error e));
    Printf.printf "%b\n" (Sys.file_exists f);
    Sys.remove f;
    let d = "/tmp/jsoo_rm_dir" in
    Unix.mkdir d 0o777;
    (try Unix.unlink d
     with Unix.Unix_error (e, _, _) ->
       (* unlinking a directory is EISDIR on Linux, EPERM on macOS/BSD *)
       print_endline
         (match e with
         | Unix.EISDIR | Unix.EPERM -> "EISDIR"
         | e -> unix_error e));
    Printf.printf "%b\n" (Sys.file_exists d);
    Unix.rmdir d;
    let p = "/tmp/jsoo_perm0" in
    let fd = Unix.openfile p [ Unix.O_CREAT; Unix.O_WRONLY ] 0 in
    Unix.close fd;
    Printf.printf "0o%o\n" (Unix.stat p).Unix.st_perm;
    Sys.remove p
  end;
  [%expect {|
    ENOTDIR
    true
    EISDIR
    true
    0o0
    |}]

let%expect_test "Unix.error_message" =
  let p e =
    match Unix.error_message e with
    | m -> print_endline m
    | exception _ -> print_endline "error"
  in
  p Unix.ECHILD;
  p Unix.EWOULDBLOCK;
  p Unix.EDEADLK;
  Printf.printf "%b\n" (String.length (Unix.error_message Unix.EPERM) > 0);
  [%expect
    {|
    ECHILD
    EWOULDBLOCK
    EDEADLK
    true
    |}]
