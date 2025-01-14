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

(* CSS.Length testing *)
open Js_of_ocaml

let%expect_test _ =
  let ls =
    [ CSS.Length.Em 0.1
    ; CSS.Length.Ex 0.12
    ; CSS.Length.Px 5.4
    ; CSS.Length.Gd 0.
    ; CSS.Length.Rem 10.0
    ; CSS.Length.Vw 0.10
    ; CSS.Length.Vh 0.1
    ; CSS.Length.Vm 0.5
    ; CSS.Length.Ch 20.6
    ; CSS.Length.Zero
    ; CSS.Length.Mm 100.
    ; CSS.Length.Cm 10.1
    ; CSS.Length.In 0.1
    ; CSS.Length.Pt 10.2
    ; CSS.Length.Pc 103.1
    ]
  in
  List.iter
    (fun c ->
      try
        let js = CSS.Length.js c in
        let ml = CSS.Length.ml js in
        if c = ml
        then ()
        else
          Printf.printf
            "%s   %s\n%!"
            (CSS.Length.string_of_t c)
            (CSS.Length.string_of_t ml)
      with exn -> print_endline (Printexc.to_string exn))
    ls;
  [%expect {||}]
