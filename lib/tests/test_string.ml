(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2025 Hugo Heuzard
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

let%expect_test "test utf8-utf16 conversions" =
  let min = Uchar.to_int Uchar.min in
  let max = Uchar.to_int Uchar.max in
  let utf8 = Buffer.create (max * 4) in
  let utf16 = Buffer.create (max * 4) in
  let l = ref [] in
  for i = max downto min do
    if Uchar.is_valid i then l := Uchar.of_int i :: !l
  done;
  List.iter
    (fun u ->
      Buffer.add_utf_16be_uchar utf16 u;
      Buffer.add_utf_8_uchar utf8 u)
    !l;
  let utf8 = Buffer.contents utf8 in
  let utf16 = Buffer.contents utf16 in

  let utf16' = Js.string utf8 in
  let rec loop i =
    if i / 2 >= utf16'##.length
    then assert (i = String.length utf16)
    else
      let u_js =
        utf16'##codePointAt (i / 2)
        |> Js.Optdef.to_option
        |> Option.get
        |> Js.to_float
        |> int_of_float
      in
      let u_ml, len =
        let d = String.get_utf_16be_uchar utf16 i in
        assert (Uchar.utf_decode_is_valid d);
        let len = Uchar.utf_decode_length d in
        Uchar.to_int (Uchar.utf_decode_uchar d), len
      in
      if u_js = u_ml
      then loop (i + len)
      else (
        Printf.eprintf "%x <> %x\n" u_js u_ml;
        Printf.eprintf "string differ at %x\n" i;
        assert false)
  in
  loop 0;
  let utf8' = Js.to_string utf16' in
  for i = 0 to String.length utf8 - 1 do
    if Char.equal (String.get utf8 i) (String.get utf8' i)
    then ()
    else (
      Printf.eprintf "%C <> %C\n" (String.get utf8 i) (String.get utf8' i);
      Printf.eprintf "string differ at %d\n" i;
      ())
  done;
  [%expect {||}]
