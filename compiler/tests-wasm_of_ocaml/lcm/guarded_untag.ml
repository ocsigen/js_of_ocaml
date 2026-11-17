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

(* The untagging of a loop invariant of unknown type is hoisted out of the
   loop, and must then not fail when the value is not an integer: here, it
   is only used as an integer when [Obj.is_int] holds. *)

let f (x : Obj.t) n =
  let s = ref 0 in
  for i = 1 to n do
    if Obj.is_int x then s := !s + (Obj.obj x : int) + i else s := !s - i
  done;
  !s

let () =
  assert (f (Obj.repr 5) 10 = 105);
  assert (f (Obj.repr (Sys.opaque_identity "str")) 10 = -55)

(* The speculative untagging must not be used to test the value itself: it
   is 0 for a block. *)

let g (x : Obj.t) n =
  let s = ref 0 in
  for i = 1 to n do
    if i land 1 = 0
    then (if Obj.is_int x then s := !s + (Obj.obj x : int))
    else
      match (Obj.obj x : int option) with
      | None -> s := !s + 100
      | Some _ -> s := !s + 1
  done;
  !s

let () =
  assert (g (Obj.repr 0) 10 = 500);
  assert (g (Obj.repr (Sys.opaque_identity (Some 3))) 10 = 5)
