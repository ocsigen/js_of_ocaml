(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2019 Hugo Heuzard
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
module J = Javascript

let zero = J.ENum (J.Num.of_targetint Targetint.zero)

let one = J.ENum (J.Num.of_targetint Targetint.one)

(* JavaScript engines recognize the pattern 'typeof x==="number"'; if the string is
   shared, less efficient code is generated. *)
let type_of_is_number binop e =
  J.EBin (binop, J.EUn (J.Typeof, e), J.EStr (Utf8_string.of_string_exn "number"))

let is_block e = type_of_is_number J.NotEqEq e

let is_immediate e = type_of_is_number J.EqEqEq e

(* Introcaml: a block descriptor is stored in the header word, above the
   tag, so that blocks remain plain arrays: [x[0] = tag | (desc << 8)]. The
   descriptor is at most [caml_obj_reserved_bits] (22) bits wide, so the
   header stays a small integer. *)
let desc_shift = 8

let header ~tag ~desc =
  let n = if Config.Flag.introspection () then tag lor (desc lsl desc_shift) else tag in
  J.ENum (J.Num.of_targetint (Targetint.of_int_exn n))

module Block = struct
  let make ~tag ~desc ~args = J.EArr (J.Element (header ~tag ~desc) :: args)

  let tag e =
    let hd = J.EAccess (e, ANormal, zero) in
    if Config.Flag.introspection ()
    then J.EBin (J.Band, hd, J.ENum (J.Num.of_targetint (Targetint.of_int_exn 255)))
    else hd

  let field e idx =
    let adjusted = J.ENum (J.Num.of_targetint (Targetint.of_int_exn (idx + 1))) in
    J.EAccess (e, ANormal, adjusted)
end

module Array = struct
  let make = Block.make

  let length e =
    let underlying = J.EDot (e, ANormal, Utf8_string.of_string_exn "length") in
    J.EBin (J.Minus, underlying, one)

  let field e i =
    match i with
    | J.ENum n ->
        let idx = J.Num.to_targetint n in
        let adjusted = J.ENum (J.Num.of_targetint (Targetint.succ idx)) in
        J.EAccess (e, ANormal, adjusted)
    | J.EUn (J.Neg, _) -> failwith "Negative field indexes are not allowed"
    | _ ->
        let adjusted = J.EBin (J.Plus, i, one) in
        J.EAccess (e, ANormal, adjusted)
end
