(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2019 Shachar Itzhaky
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
(*
let%expect_test _ =
  let data =
    "\132\149\166\189\r\022\206\021\001\147F\137d(\181/\253\000Xm\000\0000\n\
     \000\000'\016c\001\000\012\135\007E"
  in
  let s =
    if Compression.compression_supported
    then Marshal.from_string data 0
    else String.make 10000 'c'
  in
  Printf.printf "%s ... (%d)\n" (String.sub s 0 20) (String.length s);
  [%expect {| cccccccccccccccccccc ... (10000) |}];
  let tmp = Filename.temp_file "a" "txt" in
  let ch = open_out tmp in
  output_string ch data;
  close_out ch;
  let ch = open_in tmp in
  let s =
    if Compression.compression_supported
    then Marshal.from_channel ch
    else String.make 10000 'c'
  in
  close_in ch;
  Printf.printf "%s ... (%d)\n" (String.sub s 0 20) (String.length s);
  [%expect {| cccccccccccccccccccc ... (10000) |}] *)
