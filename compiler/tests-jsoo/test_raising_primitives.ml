(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2026 Hugo Heuzard
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

(* Primitives which can raise must not be removed when their result is
   unused. *)

let check f =
  match f () with
  | () -> print_endline "no exception"
  | exception (Invalid_argument _ | Failure _) -> print_endline "raised"

let%expect_test "Marshal.to_string" =
  check (fun () -> ignore (Marshal.to_string (fun x -> x + 1) []));
  [%expect {| raised |}]

let%expect_test "Array.make" =
  check (fun () -> ignore (Array.make (Sys.opaque_identity (-1)) 0));
  [%expect {| raised |}]

let%expect_test "Array.create_float" =
  check (fun () -> ignore (Array.create_float (Sys.opaque_identity (-1))));
  [%expect {| raised |}]

let%expect_test "Bytes.create" =
  check (fun () -> ignore (Bytes.create (Sys.opaque_identity (-1))));
  [%expect {| raised |}]
