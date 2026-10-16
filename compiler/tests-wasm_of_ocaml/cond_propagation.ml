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

(* [Cond_propagation] replaces the variable tested by a conditional by a
   constant integer in its branches. On a path which is never executed, the
   constant can then reach a use expecting another kind of value (with GADTs,
   for instance; [Obj.magic] is used here to keep the test simple). The
   generated code must still be valid. *)

(* [B] is only used on the path which is never executed *)
type t =
  | A
  | B
  | C
[@@warning "-37"]

let res = ref 0

let[@inline never] g x = res := !res + x

(* The constant used as a block *)
let[@inline never] as_block (o : int option) =
  match o with
  | None -> g (fst (Obj.magic o : int * int))
  | Some _ -> g 1

(* The constant used as a function *)
let[@inline never] as_function (a : int) (b : int) =
  let c = a = b in
  if c then g 1 else g ((Obj.magic c : int -> int) 3)

(* The constant used as a float *)
let[@inline never] as_float (a : int) (b : int) =
  let c = a = b in
  if c then g 1 else g (int_of_float ((Obj.magic c : float) +. 1.))

(* The constant used as a string, in a [Switch] *)
let[@inline never] as_string (x : t) =
  match x with
  | A -> g 1
  | B -> g (String.length (Obj.magic x : string))
  | C -> g 3

let () =
  List.iter as_block [ Some 1; Some 2 ];
  List.iter (fun (a, b) -> as_function a b) [ 1, 1; 2, 2 ];
  List.iter (fun (a, b) -> as_float a b) [ 1, 1; 2, 2 ];
  List.iter as_string [ A; C ];
  assert (!res = 10)
