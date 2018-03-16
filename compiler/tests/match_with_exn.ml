(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2017 Hugo Heuzard
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

open Common

let log_stop = log_start "match .. with exception"

exception A
exception B of int

let a_exn () =
  raise A

(* Make sure that [a] doesn't look constant *)
let a () = if Random.int 1 + 1 = 0 then 2 else 4

let b_exn () =
  raise (B 2)

(* https://github.com/ocsigen/js_of_ocaml/issues/400
 * match .. with exception is no compiled properly *)
let () =
  assert
    (try
      match a () with
      | exception (A|B _) -> true
      | _n -> b_exn ()
    with B _ -> true)

let _ = log_stop ()
