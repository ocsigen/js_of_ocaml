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

(* A number must not be boxed more than once per execution of its
   definition: all its boxed uses then get the same value, as with a
   compiler which boxes numbers where they are defined. *)

exception E

let n = Array.length Sys.argv + 4

let fl = float n

(* Boxed in the body of a [try], and again in the handler *)
let in_handler a =
  let x = a +. 10. in
  let b1 = Some x in
  let b2 =
    try
      if a > 0. then raise E;
      None
    with E -> Some x
  in
  match b1, b2 with
  | Some u, Some v -> u == v
  | _ -> false

(* Boxed on one branch, and again after the branches join *)
let after_join a c1 c2 =
  let x = a *. 4. in
  let l1 = if c1 then [ x ] else [] in
  let l2 = if c2 then [ x ] else [] in
  match l1, l2 with
  | u :: _, v :: _ -> u == v
  | _ -> true

let after_join_int32 a =
  let x = Int32.add a 1l in
  let b1 = Some x in
  let b2 = if Int32.compare a 0l > 0 then Some x else None in
  match b1, b2 with
  | Some u, Some v -> u == v
  | _ -> false

(* Boxed by the function, and by a closure *)
let in_closure a =
  let x = a +. 1. in
  let l1 = [ x ] in
  let h () = [ x ] in
  List.hd l1 == List.hd (h ())

let in_closure_int64 a =
  let x = Int64.add a 3L in
  let l1 = Some x in
  let h () = Some x in
  match l1, h () with
  | Some u, Some v -> u == v
  | _ -> false

(* Boxed by two closures *)
let in_two_closures a =
  let x = a +. 2. in
  let h1 () = [ x ] in
  let k = List.length (h1 ()) in
  let h2 () = [ x ] in
  k = 1 && List.hd (h1 ()) == List.hd (h2 ())

(* Boxed by a closure called twice, and by a loop *)
let in_loop a n =
  let x = a *. 3. in
  let g () = [ x ] in
  let l = ref (g ()) in
  for _ = 1 to n do
    l := x :: !l
  done;
  match !l, g () with
  | y :: z :: _, t :: _ -> y == z && z == t
  | _ -> false

(* Boxed in a loop entered directly from a [Poptrap] *)
let maybe_raise n = if n < 0 then raise E

let in_loop_after_try a n =
  let x = a +. 5. in
  let l = ref [] in
  let i = ref n in
  (try maybe_raise n with E -> ());
  while !i > 0 do
    if Sys.opaque_identity true then l := x :: !l;
    decr i
  done;
  match !l with
  | u :: v :: _ -> u == v
  | _ -> false

(* Boxed only once inside a loop in an exception handler *)
let maybe_raise_pos n = if n > 0 then raise E

let in_handler_loop a n =
  let x = a +. 7. in
  let l = ref [] in
  let i = ref n in
  (try maybe_raise_pos n
   with _ ->
     while !i > 0 do
       if Sys.opaque_identity true then l := x :: !l;
       decr i
     done);
  match !l with
  | u :: v :: _ -> u == v
  | _ -> false

let () =
  assert (in_handler fl);
  assert (after_join fl true true);
  assert (after_join fl true false);
  assert (after_join_int32 (Int32.of_int n));
  assert (in_closure fl);
  assert (in_closure_int64 (Int64.of_int n));
  assert (in_two_closures fl);
  assert (in_loop fl n);
  assert (in_loop_after_try fl n);
  assert (in_handler_loop fl n)
