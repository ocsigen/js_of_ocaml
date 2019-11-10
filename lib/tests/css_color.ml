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
  let cols =
    [ CSS.Color.RGB (120, 3, 56)
    ; CSS.Color.RGBA (120, 3, 56, 1.)
    ; CSS.Color.RGB_percent (10, 3, 60)
    ; CSS.Color.RGBA_percent (100, 53, 60, 0.45)
    ; CSS.Color.HSL (120, 75, 56)
    ; CSS.Color.HSLA (180, 3, 56, 0.2)
    ; CSS.Color.RGB (CSS.Color.rgb_of_name CSS.Color.Dodgerblue)
    ; CSS.Color.RGB (CSS.Color.rgb_of_name CSS.Color.Pink)
    ; CSS.Color.Name CSS.Color.Hotpink
    ; CSS.Color.Name CSS.Color.Cornsilk
    ]
  in
  List.iter
    (fun c ->
      try
        let js = CSS.Color.js c in
        let ml = CSS.Color.ml js in
        if c = ml
        then ()
        else Printf.printf "%s   %s" (CSS.Color.string_of_t c) (CSS.Color.string_of_t ml)
      with exn -> print_endline (Printexc.to_string exn))
    cols;
  [%expect {||}]
