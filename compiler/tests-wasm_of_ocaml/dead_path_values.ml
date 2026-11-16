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

(* A value known to be a number can reach, on a dead path, a use expecting
   another kind of value (with GADTs, for instance; [Obj.magic] is used
   here to keep the test simple). The generated code must still be valid. *)

let glob = ref (Obj.repr 0)

let[@inline never] takes_float (f : float) = int_of_float (f *. 2.)

(* A bound check on an integer *)
let[@inline never] check_bound c n =
  let x = n + 1 in
  if c
  then (
    (Obj.magic x : int array).(0) <- 1;
    (Obj.magic x : int array).(0))
  else x

(* A conditional on an unboxed float *)
let[@inline never] cond c n =
  let x = float n +. 1. in
  if c then if (Obj.magic x : bool) then 1 else 2 else int_of_float x

(* Raising an integer or an unboxed float *)
let[@inline never] raise_int c n =
  let x = n + 1 in
  if c then raise (Obj.magic x : exn) else x

let[@inline never] raise_float c n =
  let x = float n +. 1. in
  if c then raise (Obj.magic x : exn) else int_of_float x

(* An unboxed number used as a number of another kind *)
let[@inline never] float_as_int c n =
  let x = float n +. 1. in
  if c then if (Obj.magic x : int) < n then 1 else 2 else int_of_float x

let[@inline never] float_as_int32 c n =
  let x = float n +. 1. in
  if c then Int32.to_int (Int32.add (Obj.magic x : int32) 1l) else int_of_float x

let[@inline never] int32_as_float c n =
  let x = Int32.add (Int32.of_int n) 1l in
  if c then int_of_float ((Obj.magic x : float) +. 1.) else Int32.to_int x

let[@inline never] int64_as_float c n =
  let x = Int64.add (Int64.of_int n) 1L in
  if c
  then (
    glob := Obj.repr [| (Obj.magic x : float); 1. |];
    takes_float (Obj.magic x : float))
  else Int64.to_int x

let () =
  let n = Array.length Sys.argv in
  let c = n > 100 in
  assert (takes_float (float n) = 2 * n);
  assert (check_bound c n = n + 1);
  assert (cond c n = n + 1);
  assert (raise_int c n = n + 1);
  assert (raise_float c n = n + 1);
  assert (float_as_int c n = n + 1);
  assert (float_as_int32 c n = n + 1);
  assert (int32_as_float c n = n + 1);
  assert (int64_as_float c n = n + 1)
