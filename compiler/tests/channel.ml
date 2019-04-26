(* Js_of_ocaml compiler
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

(* https://github.com/ocsigen/js_of_ocaml/issues/777 *)

let%expect_test _ =
  Util.compile_and_run
    {|
    let oc = open_out "a.txt"
    let () = print_int (out_channel_length oc)
    let () = output_string oc "test"
    let () = print_int (out_channel_length oc)
    let () = flush oc
    let () = print_int (out_channel_length oc)
    let () = output_string oc "test"
    let () = print_int (out_channel_length oc)
    let () = seek_out oc 0
    let () = print_int (out_channel_length oc)
|};
  [%expect {| 00448 |}]

let%expect_test _ =
  let oc = open_out "b.txt" in
  let () = print_int (out_channel_length oc) in
  let () = output_string oc "test" in
  let () = print_int (out_channel_length oc) in
  let () = flush oc in
  let () = print_int (out_channel_length oc) in
  let () = output_string oc "test" in
  let () = print_int (out_channel_length oc) in
  let () = seek_out oc 0 in
  let () = print_int (out_channel_length oc) in
  [%expect {| 00448 |}]
