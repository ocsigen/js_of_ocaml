(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
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

let%expect_test _ =
  let a =
    [ CSS.Angle.Rad 0.1
    ; CSS.Angle.Turns 0.12
    ; CSS.Angle.Deg 5.4
    ; CSS.Angle.Turns 0.
    ; CSS.Angle.Grad 10.0
    ; CSS.Angle.Grad 0.10
    ]
  in
  List.iter
    (fun c ->
      try
        let js = CSS.Angle.js c in
        let ml = CSS.Angle.ml js in
        if c = ml
        then ()
        else
          Printf.printf
            "%s <>  %s\n%!"
            (CSS.Angle.string_of_t c)
            (CSS.Angle.string_of_t ml)
      with exn -> print_endline (Printexc.to_string exn))
    a;
  [%expect {||}]

(* [CSS.Angle.js] always formats with a decimal point, so the round-trip test
   above never exercises parsing an integer-valued angle (e.g. "45deg"), which
   is the common form found in actual CSS values. [js_t] is private, so coerce
   a raw string to feed it to [ml] the way a DOM value would. *)
let%expect_test "parse angle without decimal point" =
  let as_js_t s : CSS.Angle.js_t = Obj.magic (Js.string s) in
  List.iter
    (fun s ->
      match CSS.Angle.ml (as_js_t s) with
      | a -> Printf.printf "%s -> %s\n%!" s (CSS.Angle.string_of_t a)
      | exception exn -> Printf.printf "%s -> %s\n%!" s (Printexc.to_string exn))
    [ "45deg"; "100grad"; "2turns"; "1rad"; "0.5deg" ];
  [%expect
    {|
    45deg -> 45.000000deg
    100grad -> 100.000000grad
    2turns -> 2.000000turns
    1rad -> 1.000000rad
    0.5deg -> 0.500000deg
    |}]
