(* Wasm_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2026
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

(* Code generation casts a value each time it is used at a more precise
   type, without knowing whether the static type of the expression
   producing it is already precise enough. This is the case for instance
   of a global holding a constant block, of a field of a closure
   environment, or of a local introduced by [Cast_reuse]. We compute the
   static type of the operand of each cast, and remove the cast when this
   type is a subtype of the target type. Only the expressions whose type
   is readily known are considered. *)

open! Stdlib
module W = Wasm_ast
module Var = Code.Var

let f ~context ~params ~locals body =
  let local_types = Var.Hashtbl.create 64 in
  List.iter ~f:(fun (x, typ) -> Var.Hashtbl.replace local_types x typ) (params @ locals);
  let field_type ty i =
    match Var.Hashtbl.find_opt context.Code_generation.types ty with
    | Some { typ = Struct l; _ } -> (
        match (List.nth l i).typ with
        | Value typ -> Some typ
        | Packed _ -> None)
    | Some { typ = Array { typ = Value typ; _ }; _ } when i < 0 -> Some typ
    | _ -> None
  in
  let ref_type typ : W.value_type = Ref { nullable = false; typ } in
  let static_type (e : W.expression) : W.value_type option =
    match e with
    | LocalGet x | LocalTee (x, _) -> Var.Hashtbl.find_opt local_types x
    | GlobalGet g -> Code_generation.global_type context g
    | StructGet (None, ty, i, _) -> field_type ty i
    | ArrayGet (None, ty, _, _) -> field_type ty (-1)
    | RefCast (typ, _) -> Some (Ref typ)
    | StructNew (ty, _)
    | ArrayNew (ty, _, _)
    | ArrayNewFixed (ty, _)
    | ArrayNewData (ty, _, _, _) -> Some (ref_type (Type ty))
    | RefI31 _ -> Some (ref_type I31)
    | _ -> None
  in
  let rec expression e =
    match Wasm_traverse.map_expression ~expression ~instructions e with
    | W.RefCast (typ, e') as e -> (
        match static_type e' with
        | Some (Ref typ')
          when ((not typ'.nullable) || typ.nullable)
               && Code_generation.is_heap_subtype context typ'.typ typ.typ -> e'
        | _ -> e)
    | e -> e
  and instructions l =
    List.map ~f:(Wasm_traverse.map_instruction ~expression ~instructions) l
  in
  instructions body
