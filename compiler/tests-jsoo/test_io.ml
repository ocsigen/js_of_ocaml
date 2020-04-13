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
