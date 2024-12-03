(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2019 Ty Overby, Jane Street Group LLC
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

type m =
  | Replace
  | Count of int ref

class macro_mapper ~flags =
  object (m)
    inherit Js_traverse.map as super

    method expression x =
      let module J = Javascript in
      match x with
      | J.ECall (J.EVar (J.S { name = Utf8 name; _ }), (ANormal | ANullish), args, _) -> (
          match name, args with
          | "FLAG", [ J.Arg (J.EStr (Utf8 s)) ] -> (
              match flags with
              | Replace ->
                  let i = if Config.Flag.find s then Targetint.one else Targetint.zero in
                  J.ENum (J.Num.of_targetint i)
              | Count count ->
                  incr count;
                  super#expression x)
          | "BLOCK", J.Arg (J.ENum tag) :: (_ :: _ as args)
            when List.for_all args ~f:(function
                   | J.Arg _ -> true
                   | J.ArgSpread _ -> false) ->
              let tag = Targetint.to_int_exn (J.Num.to_targetint tag) in
              let args =
                List.map args ~f:(function
                  | J.Arg e -> J.Element (m#expression e)
                  | J.ArgSpread _ -> assert false)
              in
              Mlvalue.Block.make ~tag ~args
          | "TAG", [ J.Arg e ] -> Mlvalue.Block.tag (m#expression e)
          | "LENGTH", [ J.Arg e ] -> Mlvalue.Array.length (m#expression e)
          | "FIELD", [ J.Arg e; J.Arg (J.ENum n) ] ->
              let idx = Targetint.to_int_exn (J.Num.to_targetint n) in
              Mlvalue.Block.field (m#expression e) idx
          | "FIELD", [ _; J.Arg (J.EUn (J.Neg, _)) ] ->
              failwith "Negative field indexes are not allowed"
          | "ISBLOCK", [ J.Arg e ] -> Mlvalue.is_block (m#expression e)
          | (("BLOCK" | "TAG" | "LENGTH" | "FIELD" | "ISBLOCK" | "FLAG") as name), _ ->
              failwith
                (Format.sprintf "macro %s called with inappropriate arguments" name)
          | _ -> super#expression x)
      | _ -> super#expression x
  end

let f ~flags js =
  let count = ref 0 in
  let flags =
    match flags with
    | true -> Replace
    | false -> Count count
  in
  let trav = new macro_mapper ~flags in
  let js = trav#program js in
  js, !count > 0
