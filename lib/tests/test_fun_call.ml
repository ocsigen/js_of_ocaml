(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2020 Hugo Heuzard
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
open Js_of_ocaml

let%expect_test _ =
  let f = Js.wrap_callback (fun _ -> print_endline "done") in
  let () = Js.Unsafe.fun_call f [| Js.Unsafe.inject 1; Js.Unsafe.inject 2 |] in
  [%expect{| done |}]

let%expect_test _ =
  let f = Js.wrap_callback (fun _ _ _ -> print_endline "done") in
  let () =
    Js.Unsafe.fun_call
      (Js.Unsafe.fun_call f [| Js.Unsafe.inject 1; Js.Unsafe.inject 2 |])
      [| Js.Unsafe.inject 1; Js.Unsafe.inject 2 |]
  in
  [%expect{| done |}]
