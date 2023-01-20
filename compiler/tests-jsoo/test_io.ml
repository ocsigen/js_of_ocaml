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
