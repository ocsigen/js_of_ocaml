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

(* A value known to be an integer or a float can reach, on a dead path, a
   use expecting a reference, a function or a boolean (with GADTs, for
   instance; [Obj.magic] is used here to keep the test simple). The
   conversion of the value must then be explicit. *)

let[@inline never] ints c n =
  let x = n + 1 in
  if c
  then (
    incr (Obj.magic x : int ref);
    (Obj.magic x : int -> int) 1)
  else x

let[@inline never] floats c y =
  let x = y +. 1. in
  if c
  then (
    incr (Obj.magic x : int ref);
    (Obj.magic x : int -> int) 1)
  else int_of_float x

let[@inline never] negate c y =
  let x = y +. 1. in
  if c then not (Obj.magic x : bool) else x > 0.

let () =
  let n = Array.length Sys.argv in
  let c = n > 100 in
  assert (ints c n = n + 1);
  assert (floats c (float n) = n + 1);
  assert (negate c (float n))
