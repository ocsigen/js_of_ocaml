(* Wasm_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
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

(* Values known to be bigarrays are given a bigarray type and cast
   where they are defined: check the places this type flows through
   (results and parameters of functions, phis, assignments). *)

open Bigarray

let make n = Array1.create float64 c_layout n [@@inline never]

let fill a v =
  Array1.fill a v;
  a
[@@inline never]

let sum a =
  let s = ref 0. in
  for i = 0 to Array1.dim a - 1 do
    s := !s +. a.{i}
  done;
  !s
[@@inline never]

let () =
  let a = fill (make 3) 1. in
  let b = fill (make 4) 2. in
  let c = if Array.length Sys.argv > 5 then a else b in
  assert (sum c = 8.);
  let x = ref a in
  (try
     x := b;
     if Array.length Sys.argv > 0 then failwith "a";
     x := a
   with _ -> ());
  assert (!x.{0} = 2.);
  let m = Array2.create int32 c_layout 2 3 in
  m.{1, 2} <- 7l;
  assert (m.{1, 2} = 7l && Array2.dim1 m = 2 && Array2.dim2 m = 3)
