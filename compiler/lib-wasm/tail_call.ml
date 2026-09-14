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

let rewrite_tail_call ~return_exn ~no_tail_call ~y i =
  match i with
  | Wasm_ast.LocalSet (x, Call (symb, l))
    when Code.Var.equal x y && not (Code.Var.Hashtbl.mem no_tail_call symb) ->
      Some (Wasm_ast.Return_call (symb, l))
  | LocalSet (x, Call_ref (ty, e, l)) when Code.Var.equal x y ->
      Some (Return_call_ref (ty, e, l))
  | LocalSet (x, Br_on_null (_, Call (symb, l))) when return_exn && Code.Var.equal x y ->
      Some (Wasm_ast.Return_call (symb, l))
  | LocalSet (x, Br_on_null (_, Call_ref (ty, e, l)))
    when return_exn && Code.Var.equal x y -> Some (Return_call_ref (ty, e, l))
  | _ -> None

let rec instruction ~return_exn ~no_tail_call ~tail i =
  match i with
  | Wasm_ast.Loop (ty, l) ->
      Wasm_ast.Loop (ty, instructions ~return_exn ~no_tail_call ~tail l)
  | Block (ty, l) -> Block (ty, instructions ~return_exn ~no_tail_call ~tail l)
  | If (ty, e, l1, l2) ->
      If
        ( ty
        , e
        , instructions ~return_exn ~no_tail_call ~tail l1
        , instructions ~return_exn ~no_tail_call ~tail l2 )
  | Return (Some (Call (symb, l))) when not (Code.Var.Hashtbl.mem no_tail_call symb) ->
      Return_call (symb, l)
  | Return (Some (Call_ref (ty, e, l))) -> Return_call_ref (ty, e, l)
  | Push (Call (symb, l)) when tail && not (Code.Var.Hashtbl.mem no_tail_call symb) ->
      Return_call (symb, l)
  | Push (Call_ref (ty, e, l)) when tail -> Return_call_ref (ty, e, l)
  | Push (Call_ref _) -> i
  | Return (Some (Br_on_null (_, Call (symb, l)))) when return_exn -> Return_call (symb, l)
  | Return (Some (Br_on_null (_, Call_ref (ty, e, l)))) when return_exn ->
      Return_call_ref (ty, e, l)
  | Push (Br_on_null (_, Call (symb, l))) when return_exn && tail -> Return_call (symb, l)
  | Push (Br_on_null (_, Call_ref (ty, e, l))) when return_exn && tail ->
      Return_call_ref (ty, e, l)
  | Push (Br_on_null (_, Call_ref _)) when return_exn -> i
  | Drop (BlockExpr (typ, l)) ->
      Drop (BlockExpr (typ, instructions ~return_exn ~no_tail_call ~tail:false l))
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

and instructions ~return_exn ~no_tail_call ~tail l =
  match l with
  | [] -> []
  | [ i ] -> [ instruction ~return_exn ~no_tail_call ~tail i ]
  | i :: Nop :: rem -> instructions ~return_exn ~no_tail_call ~tail (i :: rem)
  | i :: i' :: Nop :: rem -> instructions ~return_exn ~no_tail_call ~tail (i :: i' :: rem)
  | i :: i' :: (([] | [ Event _ ]) as event_opt) -> (
      (* There can be an event at the end of the function, which we
         should keep. *)
      match get_return ~tail i' with
      | None ->
          instruction ~return_exn ~no_tail_call ~tail:false i
          :: instruction ~return_exn ~no_tail_call ~tail i'
          :: event_opt
      | Some y -> (
          match rewrite_tail_call ~return_exn ~no_tail_call ~y i with
          | None ->
              instruction ~return_exn ~no_tail_call ~tail:false i
              :: instruction ~return_exn ~no_tail_call ~tail i'
              :: event_opt
          | Some i'' -> i'' :: event_opt))
  | i :: rem ->
      instruction ~return_exn ~no_tail_call ~tail:false i
      :: instructions ~return_exn ~no_tail_call ~tail rem

let f ~return_exn ~no_tail_call l = instructions ~return_exn ~no_tail_call ~tail:true l
