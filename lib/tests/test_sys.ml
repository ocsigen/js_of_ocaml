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
