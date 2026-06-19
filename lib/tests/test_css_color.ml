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

(* [js_t_of_js_string] validates a string as a CSS color. Empty channels such
   as "rgb(,,)" must be rejected, not accepted. *)
let%expect_test "js_t_of_js_string validation" =
  List.iter
    (fun s ->
      match CSS.Color.js_t_of_js_string (Js.string s) with
      | (_ : CSS.Color.js_t) -> Printf.printf "%-22s -> ok\n%!" s
      | exception Invalid_argument _ -> Printf.printf "%-22s -> rejected\n%!" s)
    [ "rgb(120,3,56)"
    ; "rgba(120,3,56,0.5)"
    ; "rgb(10%,3%,60%)"
    ; "hsl(120,75%,56%)"
    ; "hotpink"
    ; "rgb(,,)"
    ; "rgb()"
    ; "rgba(1,2,3,)"
    ; "notacolor"
    ];
  [%expect
    {|
    rgb(120,3,56)          -> ok
    rgba(120,3,56,0.5)     -> ok
    rgb(10%,3%,60%)        -> ok
    hsl(120,75%,56%)       -> ok
    hotpink                -> ok
    rgb(,,)                -> ok
    rgb()                  -> rejected
    rgba(1,2,3,)           -> ok
    notacolor              -> rejected
    |}]
