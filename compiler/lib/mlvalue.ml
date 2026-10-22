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

let int n = J.ENum (J.Num.of_targetint (Targetint.of_int_exn n))

(* JavaScript engines recognize the pattern 'typeof x==="number"'; if the string is
   shared, less efficient code is generated. *)
let type_of_is_number binop e =
  J.EBin (binop, J.EUn (J.Typeof, e), J.EStr (Utf8_string.of_string_exn "number"))

let is_block e = type_of_is_number J.NotEqEq e

let is_immediate e = type_of_is_number J.EqEqEq e

(* Introcaml: the reserved header bits of a block (its descriptor) are
   stored in the header word, above the tag, so that blocks remain plain
   arrays: [x[0] = tag | (bits << 8)]. With the 22 bits reserved by
   Introcaml, the header fits in 30 bits: it stays non-negative under
   bitwise operators, and a small integer in JavaScript engines. *)
let tag_bits = 8

let reserved_bits = 22

let tag_mask = (1 lsl tag_bits) - 1

let reserved_mask = (1 lsl reserved_bits) - 1

let header ~tag ~desc =
  int
    (if Config.Flag.introspection ()
     then tag lor ((Code.Block_desc.to_int desc land reserved_mask) lsl tag_bits)
     else tag)

module Block = struct
  let make ~tag ~desc ~args = J.EArr (J.Element (header ~tag ~desc) :: args)

  let reserved_bits = reserved_bits

  let shifted_reserved bits =
    J.EBin (J.Lsl, J.EBin (J.Band, bits, int reserved_mask), int tag_bits)

  let header ~tag ~reserved =
    if Config.Flag.introspection ()
    then J.EBin (J.Bor, tag, shifted_reserved reserved)
    else tag

  let max_header () =
    int
      (if Config.Flag.introspection ()
       then (1 lsl (tag_bits + reserved_bits)) - 1
       else tag_mask)

  let tag e =
    let hd = J.EAccess (e, ANormal, zero) in
    if Config.Flag.introspection () then J.EBin (J.Band, hd, int tag_mask) else hd

  let set_tag e tag =
    let hd = J.EAccess (e, ANormal, zero) in
    if Config.Flag.introspection ()
    then
      let reserved = J.EBin (J.Band, hd, int (lnot tag_mask)) in
      J.EBin (J.Eq, hd, J.EBin (J.Bor, reserved, tag))
    else J.EBin (J.Eq, hd, tag)

  let reserved e = J.EBin (J.Lsr, J.EAccess (e, ANormal, zero), int tag_bits)

  let set_reserved e bits =
    let hd = J.EAccess (e, ANormal, zero) in
    let tag = J.EBin (J.Band, hd, int tag_mask) in
    J.EBin
      ( J.Eq
      , hd
      , match bits with
        | J.ENum n when Targetint.is_zero (J.Num.to_targetint n) -> tag
        | _ -> J.EBin (J.Bor, tag, shifted_reserved bits) )

  let field e idx = J.EAccess (e, ANormal, int (idx + 1))
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
