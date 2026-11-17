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

(* A conversion which may fail (the type of the operand is not known) is
   moved out of a loop when the loop performs it whenever it is entered:
   here, the unboxing of [x], at the beginning of the body of the [for]
   loop. *)

type _ t =
  | F : float t
  | S : string t

let sum (type a) (w : a t) (x : a) n =
  match w with
  | F ->
      let s = ref 0. in
      for i = 1 to n do
        s := !s +. x +. float i
      done;
      int_of_float !s
  | S -> String.length x + n

(* The untagging of [x] is available when the loop is entered, but is not
   kept across the call in the loop: it is performed again after the
   call *)
let count f x n =
  let s = ref (x + 1) in
  for _ = 1 to n do
    s := f (!s + x)
  done;
  !s

let () =
  assert (sum F 1.5 10 = 70);
  assert (sum F 0.5 0 = 0);
  assert (sum S "abc" 10 = 13);
  assert (count succ 3 10 = 44);
  assert (count succ 3 0 = 4)
