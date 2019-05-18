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

class macro_mapper =
  object (m)
    inherit Js_traverse.map as super

    method expression x =
      let module J = Javascript in
      match x with
      | J.ECall (J.EVar (J.S {name; _}), args, _) -> (
        match name, args with
        | "BLOCK", J.ENum tag :: (_ :: _ as args) ->
            let tag = Int32.to_int (J.Num.to_int32 tag) in
            let args = List.map args ~f:m#expression in
            Mlvalue.Block.make ~tag ~args
        | "TAG", [e] -> Mlvalue.Block.tag (m#expression e)
        | "LENGTH", [e] -> Mlvalue.Array.length (m#expression e)
        | "FIELD", [e; J.ENum n] ->
            let idx = Int32.to_int (J.Num.to_int32 n) in
            Mlvalue.Block.field (m#expression e) idx
        | "FIELD", [_; J.EUn (J.Neg, _)] ->
            failwith "Negative field indexes are not allowed"
        | "ISBLOCK", [e] -> Mlvalue.is_block (m#expression e)
        | ("BLOCK" | "TAG" | "LENGTH" | "FIELD" | "ISBLOCK"), _ ->
            failwith (Format.sprintf "macro %s called with inappropriate arguments" name)
        | _ -> super#expression x)
      | _ -> super#expression x
  end

let f js =
  let trav = new macro_mapper in
  trav#program js
