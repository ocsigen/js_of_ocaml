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

open! Stdlib

let get_return ~tail i =
  match i with
  | Wasm_ast.Return (Some (LocalGet y)) -> Some y
  | Push (LocalGet y) when tail -> Some y
  | _ -> None

let rewrite_tail_call ~y i =
  match i with
  | Wasm_ast.LocalSet (x, Call (symb, l)) when Code.Var.equal x y ->
      Some (Wasm_ast.Return_call (symb, l))
  | LocalSet (x, Call_ref (ty, e, l)) when Code.Var.equal x y ->
      Some (Return_call_ref (ty, e, l))
  | _ -> None

let rec instruction ~tail i =
  match i with
  | Wasm_ast.Loop (ty, l) -> Wasm_ast.Loop (ty, instructions ~tail l)
  | Block (ty, l) -> Block (ty, instructions ~tail l)
  | If (ty, e, l1, l2) -> If (ty, e, instructions ~tail l1, instructions ~tail l2)
  | Return (Some (Call (symb, l))) -> Return_call (symb, l)
  | Return (Some (Call_ref (ty, e, l))) -> Return_call_ref (ty, e, l)
  | Push (Call (symb, l)) when tail -> Return_call (symb, l)
  | Push (Call_ref (ty, e, l)) when tail -> Return_call_ref (ty, e, l)
  | Push (Call_ref _) -> i
  | Drop (BlockExpr (typ, l)) -> Drop (BlockExpr (typ, instructions ~tail:false l))
  | Drop _
  | LocalSet _
  | GlobalSet _
  | Br_table _
  | Br _
  | Br_if _
  | Return _
  | Throw _
  | Rethrow _
  | CallInstr _
  | Nop
  | Push _
  | ArraySet _
  | StructSet _
  | Return_call _
  | Return_call_ref _
  | Unreachable
  | Event _ -> i

and instructions ~tail l =
  match l with
  | [] -> []
  | [ i ] -> [ instruction ~tail i ]
  | i :: Nop :: rem -> instructions ~tail (i :: rem)
  | i :: i' :: Nop :: rem -> instructions ~tail (i :: i' :: rem)
  | i :: i' :: (([] | [ Event _ ]) as event_opt) -> (
      (* There can be an event at the end of the function, which we
         should keep. *)
      match get_return ~tail i' with
      | None -> instruction ~tail:false i :: instruction ~tail i' :: event_opt
      | Some y -> (
          match rewrite_tail_call ~y i with
          | None -> instruction ~tail:false i :: instruction ~tail i' :: event_opt
          | Some i'' -> i'' :: event_opt))
  | i :: rem -> instruction ~tail:false i :: instructions ~tail rem

let f l = instructions ~tail:true l
