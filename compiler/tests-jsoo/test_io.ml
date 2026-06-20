(* Js_of_ocaml tests
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

(* The In_channel/Out_channel APIs exercised here require OCaml 4.14. *)
[@@@if ocaml_version >= (4, 14, 0)]

let%expect_test "unicode" =
  let () = print_endline "the • and › characters" in
  [%expect {| the • and › characters |}]

let%expect_test _ =
  let () =
    let oc = open_out "file.txt" in
    for i = 0 to 32 do
      output_string oc (string_of_int i);
      output_string oc "\n"
    done;
    close_out oc
  in
  let () =
    let ic = open_in "file.txt" in
    let l = ref [] in
    (try
       while true do
         l := input_line ic :: !l
       done
     with End_of_file -> ());
    close_in ic;
    print_int (List.length !l);
    Sys.remove "file.txt"
  in
  [%expect {|
    33 |}]

let%expect_test _ =
  Printf.printf "%b%!" Out_channel.(is_buffered stdout);
  [%expect {| true |}];
  Printf.printf "%b%!" Out_channel.(is_buffered stderr);
  [%expect {| true |}];
  Out_channel.(set_buffered stdout false);
  Printf.printf "%b%!" Out_channel.(is_buffered stdout);
  [%expect {| false |}];
  Out_channel.(set_buffered stderr false);
  Printf.printf "%b%!" Out_channel.(is_buffered stderr);
  [%expect {| false |}];
  Out_channel.(set_buffered stdout true);
  Printf.printf "%b%!" Out_channel.(is_buffered stdout);
  [%expect {| true |}];
  Out_channel.(set_buffered stderr true);
  Printf.printf "%b%!" Out_channel.(is_buffered stderr);
  [%expect {| true |}]

let%expect_test _ =
  let file_contents fname =
    let ic = open_in_bin fname in
    match really_input_string ic (in_channel_length ic) with
    | s ->
        close_in ic;
        s
    | exception e ->
        close_in ic;
        raise e
  in
  let fname = "file2.txt" in
  let oc = open_out fname in
  Printf.printf "%b%!" Out_channel.(is_buffered oc);
  [%expect {| true |}];
  output_string oc "this ";
  print_endline (file_contents fname);
  [%expect {||}];
  flush oc;
  print_endline (file_contents fname);
  [%expect {| this |}];
  output_string oc "is ";
  print_endline (file_contents fname);
  [%expect {| this |}];
  Out_channel.set_buffered oc false;
  print_endline (file_contents fname);
  [%expect {| this is |}];
  output_string oc "a test";
  print_endline (file_contents fname);
  [%expect {| this is a test |}];
  flush oc;
  print_endline (file_contents fname);
  [%expect {| this is a test |}];
  close_out oc;
  print_endline (file_contents fname);
  [%expect {| this is a test |}];
  Sys.remove fname;
  ()

(* Each call to in_channel_of_descr / out_channel_of_descr must
   allocate a fresh channel, like the C runtime. They used to share
   one channel record per fd, so the two channels were physically
   equal and reading after writing looped forever. *)
let%expect_test "channel_of_descr channels are distinct" =
  let f = Filename.temp_file "jsoo_chan" ".txt" in
  let fd = Unix.openfile f [ Unix.O_RDWR; Unix.O_CREAT ] 0o644 in
  let oc = Unix.out_channel_of_descr fd in
  let ic = Unix.in_channel_of_descr fd in
  Printf.printf "%b\n" (Obj.repr ic == Obj.repr oc);
  close_out oc;
  Sys.remove f;
  [%expect {| false |}]

(* flush_all must reach every open output channel, not just stdout/stderr;
   a buffered user channel is flushed to disk by flush_all alone. *)
let%expect_test "flush_all flushes user channels" =
  let f = Filename.temp_file "jsoo_flushall" ".txt" in
  let oc = open_out f in
  output_string oc "hello";
  flush_all ();
  let ic = open_in f in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  close_out oc;
  Sys.remove f;
  print_endline s;
  [%expect {| hello |}]

(* [Out_channel.is_binary_mode] was added in OCaml 5.2. *)
let%expect_test "is_binary_mode" =
  let f = Filename.temp_file "jsoo_bin" ".txt" in
  let oc = open_out f in
  (* On Unix (and the js/wasm runtimes) text channels report binary mode;
     native Windows reports them as non-binary. *)
  Printf.printf
    "%b %b\n"
    (Sys.win32 || Out_channel.is_binary_mode oc)
    (Sys.win32 || Out_channel.is_binary_mode stdout);
  close_out oc;
  Sys.remove f;
  [%expect {| true true |}]
[@@if ocaml_version >= (5, 2, 0)]
