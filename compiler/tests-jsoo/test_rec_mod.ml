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
  [%expect {| File "compiler/tests-jsoo/test_rec_mod.ml", line 28, characters 2-8: Undefined recursive module |}]
