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

(* A parameter which is untagged on every path from the function entry is
   passed untagged, the callers untagging it: [g] is called on values read
   from an array, whose type is not known. This must not happen when a
   path does not untag the parameter: in [f], [x] is not an integer when
   [b] is false. *)

let[@inline never] f (x : Obj.t) b = if b then (Obj.obj x : int) + 1 else 0

let[@inline never] g (x : Obj.t) = (Obj.obj x : int) + (Obj.obj x : int)

let values = [| Obj.repr 5; Obj.repr 6 |]

let sum () =
  let s = ref 0 in
  for i = 0 to Array.length values - 1 do
    s := !s + g values.(i) + g values.(i)
  done;
  !s

(* Nor when a path loops without untagging it, leaving the loop by an
   exception raised by a primitive *)
type _ ty =
  | I : int ty
  | S : string ty

let[@inline never] h : type a. a ty -> a -> int array -> int =
 fun w x a ->
  match w with
  | I -> x + 1
  | S ->
      let i = ref 0 in
      while true do
        ignore (Sys.opaque_identity a.(!i));
        incr i
      done;
      0

let () =
  let s = Obj.repr (Sys.opaque_identity "str") in
  assert (f (Obj.repr 5) true = 6);
  assert (f s false = 0);
  assert (f (Obj.repr 7) (Sys.opaque_identity true) = 8);
  assert (sum () = 44);
  assert (g (Obj.repr 5) = 10);
  assert (g (Obj.repr (Sys.opaque_identity 6)) = 12);
  assert (h I 5 [||] = 6);
  assert (
    try
      ignore (h S (Sys.opaque_identity "str") [| 1; 2 |]);
      false
    with Invalid_argument _ -> true)
