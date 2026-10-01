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

(* Normalizations of an integer are shared between its uses: bound checks,
   array accesses, divisions, and comparisons, which use a normalized copy
   when one is available. The sums below overflow when [n] is large, so an
   unnormalized value differs from its normalized copy. *)

let[@inline never] get a i n =
  let j = i + n in
  (* The bound check and the access share the normalization of [j] *)
  a.(j) + a.(j)

let[@inline never] div i n d =
  let j = i + n in
  j / d, j mod d, j < 0, j = -1

let[@inline never] sum a n =
  let s = ref 0 in
  for i = 0 to Array.length a - 1 do
    let j = i + n in
    if j >= 0 && j < Array.length a then s := !s + a.(j) + a.(j)
  done;
  !s

let () =
  let a = Array.init 10 (fun i -> i) in
  assert (get a 3 4 = 14);
  (match get a 3 (Sys.opaque_identity max_int) with
  | _ -> assert false
  | exception Invalid_argument _ -> ());
  assert (
    div 0 (Sys.opaque_identity max_int) 3 = (max_int / 3, max_int mod 3, false, false));
  assert (div 1 (Sys.opaque_identity max_int) 7 = (min_int / 7, min_int mod 7, true, false));
  assert (div 0 (Sys.opaque_identity (-1)) 2 = (0, -1, true, true));
  assert (sum a 5 = 2 * (5 + 6 + 7 + 8 + 9));
  assert (sum a (Sys.opaque_identity max_int) = 0);
  assert (sum a (Sys.opaque_identity min_int) = 0)
