(* Js_of_ocaml compiler
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

(* https://github.com/ocaml/ocaml/pull/9497 *)
(* Original test case from the issue: *)

module rec Id : sig
  type t = { id : int }

  val compare : t -> t -> int
end = (* error here: undefined compare function *)
  Id

module IdSet = Set.Make (Id)

let%expect_test _ =
  (try
     let basic_set = IdSet.singleton { id = 0 } in
     ignore (IdSet.mem { id = 1 } basic_set : bool)
     (* diverge here *)
   with e -> print_endline @@ Printexc.to_string e);
  [%expect
    {| File "[^"]*test_rec_mod.ml", line [0-9]*, characters [0-9-]*: Undefined recursive module (regexp) |}]

(* Looping version *)
module rec M1 : sig
  val f : int -> int

  val g : int -> int

  val h : int -> int
end = struct
  let f = M1.g

  let g i = if i < 0 then 2 else M1.f (i - 1)

  let h = M1.f
end

let%expect_test _ =
  (try print_int (M1.f 3) with e -> print_endline @@ Printexc.to_string e);
  [%expect
    {| 2 |}];
  (try print_int (M1.g 3) with e -> print_endline @@ Printexc.to_string e);
  [%expect
    {| 2 |}];
  (try print_int (M1.h 3) with e -> print_endline @@ Printexc.to_string e);
  [%expect
    {| 2 |}]

module rec Odd : sig
  val odd : int -> bool
end = struct
  let odd x = if x = 0 then false else Even.even (pred x)
end

and Even : sig
  val even : int -> bool
end = struct
  let even x = if x = 0 then true else Odd.odd (pred x)
end

let%expect_test _ =
  Printf.printf "%b" (Even.even 1000);
  [%expect {| true |}];
  Printf.printf "%b" (Odd.odd 1000);
  [%expect {| false |}]
