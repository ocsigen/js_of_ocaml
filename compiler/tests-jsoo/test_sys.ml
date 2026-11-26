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

open! Js_of_ocaml

(* The four [In_channel.input_all] tests carry a per-test [@@if] (the API is
   5.0+), so they only build on OCaml >= 5; likewise, the [Sys.filepath_exists]
   test only builds on OCaml >= 5.6. The three tests digesting the
   Random-generated [content] run everywhere, but the Random PRNG changed in 5.0,
   so each digest snapshot has a [@when ocaml_version >= (5, 0, 0)] variant (the
   5.x value) and a default (the pre-5 value). Everything else runs unconditionally. *)

let content =
  let t = Random.State.make [| 1; 2; 3; 4; 5 |] in
  String.init (1024 * 1024) (fun _ -> Char.chr (Random.State.int t (0xff + 1)))

let print_digest d = print_endline (Digest.to_hex d)

let%expect_test _ =
  print_digest (Digest.string content);
  [%expect ({| dd5da7fa373a2b2257d361aaf76845a0 |} [@when ocaml_version >= (5, 0, 0)])];
  [%expect {| ed06ea572e62b0e2448c55777aeaca4c |}];
  let c = open_out_bin "/static/temp0" in
  output_string c content;
  close_out c;
  let c = open_in_bin "/static/temp0" in
  print_digest (Digest.channel c (-1));
  [%expect ({| dd5da7fa373a2b2257d361aaf76845a0 |} [@when ocaml_version >= (5, 0, 0)])];
  [%expect {| ed06ea572e62b0e2448c55777aeaca4c |}];
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
  [%expect ({| dd5da7fa373a2b2257d361aaf76845a0 |} [@when ocaml_version >= (5, 0, 0)])];
  [%expect {| ed06ea572e62b0e2448c55777aeaca4c |}];
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
  [%expect ({| dd5da7fa373a2b2257d361aaf76845a0 |} [@when ocaml_version >= (5, 0, 0)])];
  [%expect {| ed06ea572e62b0e2448c55777aeaca4c |}];
  ()

let%expect_test _ =
  let size = String.length content / 2 in
  print_digest (Digest.string (String.sub content 0 size));
  [%expect ({| 573705548ab0d6c8a2193579611511a2 |} [@when ocaml_version >= (5, 0, 0)])];
  [%expect {| e2a6ef4c88593bb6d8e456687407bf04 |}];
  let c = open_out_bin "/static/temp0" in
  output_string c content;
  close_out c;
  let c = open_in_bin "/static/temp0" in
  print_digest (Digest.channel c size);
  [%expect ({| 573705548ab0d6c8a2193579611511a2 |} [@when ocaml_version >= (5, 0, 0)])];
  [%expect {| e2a6ef4c88593bb6d8e456687407bf04 |}];
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
  [%expect ({| 573705548ab0d6c8a2193579611511a2 |} [@when ocaml_version >= (5, 0, 0)])];
  [%expect {| e2a6ef4c88593bb6d8e456687407bf04 |}];
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
  [%expect ({| 573705548ab0d6c8a2193579611511a2 |} [@when ocaml_version >= (5, 0, 0)])];
  [%expect {| e2a6ef4c88593bb6d8e456687407bf04 |}];
  ()

let%expect_test _ =
  print_digest (Digest.string content);
  [%expect ({| dd5da7fa373a2b2257d361aaf76845a0 |} [@when ocaml_version >= (5, 0, 0)])];
  [%expect {| ed06ea572e62b0e2448c55777aeaca4c |}];
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
[@@if ocaml_version >= (5, 0, 0)]

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
[@@if ocaml_version >= (5, 0, 0)]

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
[@@if ocaml_version >= (5, 0, 0)]

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

(* [jsoo_create_file] is a primitive of the JavaScript runtime only *)
let%expect_test ("register bytes content" [@when js]) =
  jsoo_create_file "/static/bin.dat" (Bytes.of_string "\xff\x00\x80a");
  let c = open_in_bin "/static/bin.dat" in
  let s = In_channel.input_all c in
  close_in c;
  Printf.printf "%d:" (String.length s);
  String.iter (fun c -> Printf.printf " %02x" (Char.code c)) s;
  print_newline ();
  [%expect {| 4: ff 00 80 61 |}]
[@@if ocaml_version >= (5, 0, 0)]

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
  [%expect {|
    ECHILD
    EWOULDBLOCK
    EDEADLK
    true
    |}]

let%expect_test "filepath_exists" =
  Sys.mkdir "/static/fpe" 0o777;
  close_out (open_out "/static/fpe/f");
  List.iter
    (fun name -> Printf.printf "%s: %b\n" name (Sys.filepath_exists name))
    [ "/static"; "/static/fpe"; "/static/fpe/f"; "/static/fpe/g"; "/static/fpe/f/x" ];
  [%expect
    {|
    /static: true
    /static/fpe: true
    /static/fpe/f: true
    /static/fpe/g: false
    /static/fpe/f/x: false
    |}]
[@@if ocaml_version >= (5, 6, 0)]

(* The pseudo filesystem: same behavior with js_of_ocaml and wasm_of_ocaml *)

let read_all name =
  let c = open_in_bin name in
  let s = really_input_string c (in_channel_length c) in
  close_in c;
  s

let sorted a =
  let l = Array.to_list a in
  String.concat " " (List.sort compare l)

let%expect_test "mount: files are loaded on demand" =
  Sys_js.mount ~path:"/lazy/" (fun ~prefix ~path ->
      Printf.printf "load %s %s\n" prefix path;
      if String.equal path "missing" then None else Some ("<" ^ path ^ ">"));
  print_endline (read_all "/lazy/a/b.txt");
  [%expect {|
    load /lazy/ a/b.txt
    <a/b.txt>
    |}];
  (* Loaded only once *)
  print_endline (read_all "/lazy/a/b.txt");
  print_endline (Sys_js.read_file ~name:"/lazy/a/b.txt");
  Printf.printf "%b %b\n" (Sys.is_directory "/lazy/a") (Sys.is_directory "/lazy/a/b.txt");
  print_endline (sorted (Sys.readdir "/lazy/a"));
  [%expect {|
    <a/b.txt>
    <a/b.txt>
    true false
    b.txt
    |}];
  Printf.printf "%b\n" (Sys.file_exists "/lazy/missing");
  (try ignore (open_in "/lazy/missing") with Sys_error m -> print_endline m);
  [%expect
    {|
    load /lazy/ missing
    false
    load /lazy/ missing
    /lazy/missing: No such file or directory
    |}];
  Printf.printf "%b\n" (List.mem "/lazy/" (Sys_js.mount_point ()));
  Sys_js.unmount ~path:"/lazy/";
  Printf.printf "%b\n" (List.mem "/lazy/" (Sys_js.mount_point ()));
  [%expect {|
    true
    false
    |}]

let%expect_test "create_file, update_file, read_file" =
  Sys_js.create_file ~name:"/static/reg/x.txt" ~content:"abc";
  print_endline (Sys_js.read_file ~name:"/static/reg/x.txt");
  Sys_js.update_file ~name:"/static/reg/x.txt" ~content:"defgh";
  print_endline (read_all "/static/reg/x.txt");
  print_endline (sorted (Sys.readdir "/static/reg"));
  (try ignore (Sys_js.read_file ~name:"/static/reg/y.txt")
   with Sys_error m -> print_endline m);
  [%expect
    {|
    abc
    defgh
    x.txt
    /static/reg/y.txt: No such file or directory
    |}]

let%expect_test "write, seek, append, truncate" =
  let name = "/static/rw.bin" in
  let c = open_out_bin name in
  output_string c "0123456789";
  seek_out c 2;
  output_string c "ab";
  close_out c;
  print_endline (read_all name);
  let c = open_out_gen [ Open_wronly; Open_append ] 0o666 name in
  output_string c "XY";
  close_out c;
  print_endline (read_all name);
  let c = open_in_bin name in
  seek_in c 8;
  print_endline (really_input_string c 4);
  close_in c;
  Unix.truncate name 3;
  print_endline (read_all name);
  (* A removed file can still be read through an open channel *)
  let c = open_in_bin name in
  Sys.remove name;
  Printf.printf "%b %s\n" (Sys.file_exists name) (really_input_string c 3);
  close_in c;
  [%expect {|
    01ab456789
    01ab456789XY
    89XY
    01a
    false 01a
    |}]

let%expect_test "directories" =
  Sys.mkdir "/static/d1" 0o777;
  Sys.mkdir "/static/d1/d2" 0o777;
  close_out (open_out "/static/d1/f");
  print_endline (sorted (Sys.readdir "/static/d1"));
  (try Sys.rmdir "/static/d1" with Sys_error _ -> print_endline "not empty");
  (try Sys.mkdir "/static/d1/d2" 0o777 with Sys_error _ -> print_endline "exists");
  Sys.rename "/static/d1/f" "/static/d1/d2/g";
  Sys.rename "/static/d1/d2" "/static/d1/d3";
  print_endline (sorted (Sys.readdir "/static/d1"));
  print_endline (sorted (Sys.readdir "/static/d1/d3"));
  let h = Unix.opendir "/static/d1" in
  let rec loop acc =
    match Unix.readdir h with
    | e -> loop (e :: acc)
    | exception End_of_file -> acc
  in
  print_endline (String.concat " " (List.sort compare (loop [])));
  Unix.closedir h;
  Sys.remove "/static/d1/d3/g";
  Sys.rmdir "/static/d1/d3";
  Sys.rmdir "/static/d1";
  Printf.printf "%b\n" (Sys.file_exists "/static/d1");
  [%expect {|
    d2 f
    not empty
    exists
    d3
    g
    . .. d3
    false
    |}]

let%expect_test "relative paths" =
  let cwd = Sys.getcwd () in
  Sys.mkdir "/static/cd" 0o777;
  Sys.chdir "/static/cd";
  close_out (open_out "rel");
  Printf.printf
    "%b %b\n"
    (Sys.file_exists "/static/cd/rel")
    (Sys.file_exists "../cd/./rel");
  Sys.chdir cwd;
  Printf.printf "%b\n" (Sys.file_exists "rel");
  [%expect {|
    true true
    false
    |}]

(* The in-memory file system of the JavaScript runtime does not implement
   [Unix.stat], and creates the missing parent directories when opening a
   file. *)
let%expect_test ("stat, missing parent directory" [@when not js]) =
  let name = "/static/st.txt" in
  let c = open_out name in
  output_string c "hello";
  close_out c;
  let st = Unix.stat name in
  Printf.printf "%d %b\n" st.Unix.st_size (st.Unix.st_kind = Unix.S_REG);
  Unix.chmod name 0o600;
  Printf.printf "0o%o\n" (Unix.stat name).Unix.st_perm;
  Printf.printf "%b\n" ((Unix.lstat "/static").Unix.st_kind = Unix.S_DIR);
  (try close_out (open_out "/static/nodir/f") with Sys_error m -> print_endline m);
  (try ignore (Unix.stat "/static/st.txt/x")
   with Unix.Unix_error (e, cmd, p) -> Printf.printf "%s %s %s\n" (unix_error e) cmd p);
  [%expect
    {|
    5 true
    0o600
    true
    /static/nodir/f: No such file or directory
    ENOTDIR stat /static/st.txt/x
    |}]

(* On Windows, a mount point can be on a drive. The drive letter is not case
   sensitive. *)
let%expect_test ("mount on a Windows drive" [@when not js]) =
  if Sys.win32
  then begin
    Sys_js.mount ~path:"c:\\jsoo_vfs" (fun ~prefix ~path -> Some (prefix ^ path));
    print_endline (read_all "C:/jsoo_vfs/a/b");
    print_endline (read_all "c:\\jsoo_vfs\\c");
    Printf.printf "%b\n" (List.mem "C:/jsoo_vfs/" (Sys_js.mount_point ()));
    let cwd = Sys.getcwd () in
    Sys.chdir "C:/jsoo_vfs/a";
    print_endline (Sys.getcwd ());
    print_endline (read_all "..\\d");
    Sys.chdir cwd;
    Sys_js.unmount ~path:"C:/jsoo_vfs/";
    Printf.printf "%b\n" (List.mem "C:/jsoo_vfs/" (Sys_js.mount_point ()))
  end
  else
    print_string
      "C:/jsoo_vfs/a/b\nC:/jsoo_vfs/c\ntrue\nC:/jsoo_vfs/a\nC:/jsoo_vfs/d\nfalse\n";
  [%expect
    {|
    C:/jsoo_vfs/a/b
    C:/jsoo_vfs/c
    true
    C:/jsoo_vfs/a
    C:/jsoo_vfs/d
    false
    |}]

let%expect_test "Unix operations on in-memory files" =
  Unix.mkdir "/static/ux" 0o755;
  Unix.access "/static/ux" [ Unix.F_OK ];
  let name = "/static/ux/f" in
  let fd = Unix.openfile name [ Unix.O_RDWR; Unix.O_CREAT ] 0o644 in
  ignore (Unix.write_substring fd "0123456789" 0 10);
  ignore (Unix.lseek fd 3 Unix.SEEK_SET);
  let b = Bytes.create 4 in
  let n = Unix.read fd b 0 4 in
  Printf.printf "%d %s\n" n (Bytes.to_string b);
  Unix.ftruncate fd 5;
  Printf.printf "%d\n" (Unix.lseek fd 0 Unix.SEEK_END);
  Unix.close fd;
  Unix.rename name "/static/ux/g";
  (try Unix.access name [ Unix.F_OK ]
   with Unix.Unix_error (e, _, _) -> print_endline (unix_error e));
  (try Unix.rmdir "/static/ux"
   with Unix.Unix_error (_, cmd, _) -> Printf.printf "%s failed\n" cmd);
  Unix.unlink "/static/ux/g";
  Unix.rmdir "/static/ux";
  Printf.printf "%b\n" (Sys.file_exists "/static/ux");
  [%expect {|
    4 3456
    5
    ENOENT
    rmdir failed
    false
    |}]

(* The host can create files from JavaScript *)

let host_create_file name content : unit =
  Js.Unsafe.fun_call
    (Js.Unsafe.get Js.Unsafe.global (Js.string "jsoo_create_file"))
    (Array.append [| Js.Unsafe.inject (Js.string name) |] content)

let show_bytes name =
  String.iter (fun c -> Printf.printf " %02x" (Char.code c)) (read_all name);
  print_newline ()

let%expect_test "globalThis.jsoo_create_file" =
  host_create_file "/static/host/dir/s.bin" [| Js.Unsafe.inject (Js.bytestring "a\xe9") |];
  show_bytes "/static/host/dir/s.bin";
  (try
     host_create_file "/static/host/dir/s.bin" [| Js.Unsafe.inject (Js.bytestring "b") |]
   with _ -> print_endline "exists");
  host_create_file
    "/static/host/b64.txt"
    [| Js.Unsafe.inject (Js.string "aGk="); Js.Unsafe.inject Js._true |];
  print_endline (read_all "/static/host/b64.txt");
  print_endline (sorted (Sys.readdir "/static/host"));
  [%expect {|
     61 e9
    exists
    hi
    b64.txt dir
    |}]

(* With the JavaScript runtime, the content has to be a string *)
let%expect_test ("globalThis.jsoo_create_file: Uint8Array" [@when not js]) =
  let bytes l =
    Js.Unsafe.inject
      (Js.Unsafe.new_obj
         (Js.Unsafe.get Js.Unsafe.global (Js.string "Uint8Array"))
         [| Js.Unsafe.inject (Js.array (Array.of_list l)) |])
  in
  host_create_file "/static/host/f.bin" [| bytes [ 0xff; 0x00; 0x80 ] |];
  show_bytes "/static/host/f.bin";
  [%expect {| ff 00 80 |}]
