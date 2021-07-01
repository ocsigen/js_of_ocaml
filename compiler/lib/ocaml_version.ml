(* Js_of_ocaml compiler
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

open! Stdlib

type t = int list

let split_char ~sep p =
  let len = String.length p in
  let rec split beg cur =
    if cur >= len
    then if cur - beg > 0 then [ String.sub p ~pos:beg ~len:(cur - beg) ] else []
    else if sep p.[cur]
    then String.sub p ~pos:beg ~len:(cur - beg) :: split (cur + 1) (cur + 1)
    else split beg (cur + 1)
  in
  split 0 0

let split v =
  match
    split_char
      ~sep:(function
        | '+' | '-' | '~' -> true
        | _ -> false)
      v
  with
  | [] -> assert false
  | x :: _ ->
      List.map
        (split_char
           ~sep:(function
             | '.' -> true
             | _ -> false)
           x)
        ~f:int_of_string

let current = split Sys.ocaml_version

let compint (a : int) b = compare a b

let rec compare v v' =
  match v, v' with
  | [ x ], [ y ] -> compint x y
  | [], [] -> 0
  | [], y :: _ -> compint 0 y
  | x :: _, [] -> compint x 0
  | x :: xs, y :: ys -> (
      match compint x y with
      | 0 -> compare xs ys
      | n -> n)

let v =
  if compare current [ 4; 3 ] < 0
  then `V4_02
  else if compare current [ 4; 4 ] < 0
  then `V4_03
  else if compare current [ 4; 6 ] < 0
  then `V4_04
  else if compare current [ 4; 7 ] < 0
  then `V4_06
  else if compare current [ 4; 8 ] < 0
  then `V4_07
  else if compare current [ 4; 9 ] < 0
  then `V4_08
  else if compare current [ 4; 10 ] < 0
  then `V4_09
  else if compare current [ 4; 11 ] < 0
  then `V4_10
  else if compare current [ 4; 12 ] < 0
  then `V4_11
  else if compare current [ 4; 13 ] < 0
  then `V4_12
  else `V4_13
