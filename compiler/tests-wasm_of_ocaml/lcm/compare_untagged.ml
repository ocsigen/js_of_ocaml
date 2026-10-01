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

(* An untagged copy of a value may be 0 when the value is not an integer
   (a speculative untagging, hoisted out of a loop): it must not be used to
   compare the value with 0. Here, [p] is not an integer when [y] is not. *)

let f (y : Obj.t) c n =
  let s = ref 0 in
  let j = ref 0 in
  while !j < n do
    if Obj.is_int y then s := !s + (Obj.obj y : int);
    let p = if c then y else Obj.repr 0 in
    let i = ref 0 in
    while !i < n do
      if p == Obj.repr 0 then s := !s + 1000;
      if Obj.is_int p then s := !s + (Obj.obj p : int);
      incr i
    done;
    incr j
  done;
  !s

let () =
  assert (f (Obj.repr 5) true 2 = 30);
  assert (f (Obj.repr 5) false 2 = 4010);
  assert (f (Obj.repr (Sys.opaque_identity "str")) true 2 = 0);
  assert (f (Obj.repr (Sys.opaque_identity "str")) false 2 = 4000)
