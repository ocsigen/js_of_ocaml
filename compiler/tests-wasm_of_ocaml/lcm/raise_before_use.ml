(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2026 Ocsigen team
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

(* A conversion of a value of unknown type must not be performed before an
   instruction which may raise and which precedes it: the value is only
   known to have the right representation once the conversion is reached. *)

(* The untagging of [x] follows a primitive which raises when [x] is not
   an integer: it must not be moved before the primitive, although it is
   performed on both branches and used after them. *)
let cell = ref (Obj.repr 0)

let conv s b =
  let x = !cell in
  let y =
    if b
    then
      let q = int_of_string s in
      (Obj.obj x : int) + q
    else (Obj.obj x : int) + 1
  in
  y + (Obj.obj x : int)

let conv x s b =
  cell := x;
  conv s b

let () =
  assert (conv (Obj.repr 5) "1" true = 11);
  assert (conv (Obj.repr 5) "z" false = 11);
  assert (
    match conv (Obj.repr (Sys.opaque_identity "abc")) "z" true with
    | _ -> false
    | exception Failure _ -> true)
