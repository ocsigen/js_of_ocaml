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
