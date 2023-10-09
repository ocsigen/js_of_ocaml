(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2022 Hugo Heuzard
 * Laboratoire PPS - CNRS Université Paris Diderot
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

let round_trip x =
  let s = Json.output x in
  Printf.printf "%s\n" (Js.to_bytestring s);
  (* Other direction of the round-trip (unmarshalling from JSON) is only
     available with js_of_ocaml *)
  match Sys.backend_type with
  | Other "js_of_ocaml" when Json.use_native_stringify () ->
      let y = Json.unsafe_input s in
      if not (x = y) then Printf.printf "not invariant by round-trip\n"
  | _ -> ()

let%expect_test _ =
  let tests ~use_native_stringify =
    let () = Json.set_use_native_stringify use_native_stringify in
    round_trip 123L;
    [%expect {|
      [255,123,0,0] |}];
    round_trip "asd";
    [%expect {|
      "asd" |}];
    round_trip "\000\255\254";
    [%expect {|
      "\u0000��" |}];
    round_trip (2, 3);
    round_trip (2., 3.);
    round_trip (2.2, 3.3);
    [%expect {|
      [0,2,3]
      [0,2,3]
      [0,2.2,3.3] |}]
  in
  tests ~use_native_stringify:false;
  match Sys.backend_type with
  | Other "js_of_ocaml" -> tests ~use_native_stringify:true
  | _ -> ()
