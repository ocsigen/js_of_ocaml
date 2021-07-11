(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
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

type t = string

let of_string_unsafe s = s

let to_string s = s

let to_int32 s =
  if String.is_prefix s ~prefix:"0"
     && String.length s > 1
     && String.for_all s ~f:(function
            | '0' .. '7' -> true
            | _ -> false)
  then (* octal notation *)
    Int32.of_string ("0o" ^ s)
  else Int32.of_string s

let of_int32 = Int32.to_string

let of_float v =
  match Float.classify_float v with
  | FP_nan -> "NaN"
  | FP_zero ->
      (* [1/-0] < 0. seems to be the only way to detect -0 in JavaScript *)
      if Float.(1. /. v < 0.) then "-0." else "0."
  | FP_infinite -> if Float.(v < 0.) then "-Infinity" else "Infinity"
  | FP_normal | FP_subnormal ->
      let vint = int_of_float v in
      if Float.equal (float_of_int vint) v
      then Printf.sprintf "%d." vint
      else
        let s1 = Printf.sprintf "%.12g" v in
        if Float.equal v (float_of_string s1)
        then s1
        else
          let s2 = Printf.sprintf "%.15g" v in
          if Float.equal v (float_of_string s2) then s2 else Printf.sprintf "%.18g" v

let is_zero s = String.equal s "0"

let is_one s = String.equal s "1"

let is_neg s = Char.equal s.[0] '-'

let neg s =
  match String.drop_prefix s ~prefix:"-" with
  | None -> "-" ^ s
  | Some s -> s

let add a b = of_int32 (Int32.add (to_int32 a) (to_int32 b))
