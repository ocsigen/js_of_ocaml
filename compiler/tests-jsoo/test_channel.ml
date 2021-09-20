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

(* https://github.com/ocsigen/js_of_ocaml/issues/777 *)

let%expect_test _ =
  let tmp = Filename.temp_file "b" "txt" in
  let oc = open_out_bin tmp in
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

let%expect_test _ =
  let module M = struct
    let marshal_out ch v =
      Marshal.to_channel ch v [];
      flush ch

    let marshal_out_segment f ch v =
      let start = pos_out ch in
      Format.printf "start=%d\n%!" start;
      output_binary_int ch 0;
      (* dummy value for stop *)
      marshal_out ch v;
      let stop = pos_out ch in
      seek_out ch start;
      output_binary_int ch stop;
      seek_out ch stop;
      Digest.output ch (Digest.file f)

    let _ =
      let tmp = Filename.temp_file "out" "txt" in
      let filename = tmp in
      let chan = open_out_bin filename in
      output_binary_int chan 8900;
      marshal_out_segment filename chan [ "output"; "data" ];
      marshal_out_segment filename chan [ "more"; "stuff" ]
  end in
  let open! M in
  [%expect {|
    start=4
    start=59 |}]
