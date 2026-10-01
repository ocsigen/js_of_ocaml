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

let slow_path x =
  (* This forces x to be boxed as a Value *)
  ignore (Obj.magic x : 'a)

let test n =
  let rec loop i acc =
    if i = 0
    then acc
    else
      let acc' = acc + i in
      if i = 1 then slow_path i;
      (* Conditional slow path *)
      loop (i - 1) acc'
  in
  loop n 0

let () =
  let r = test 100 in
  Printf.printf "Result: %d\n" r
