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

open! Stdlib
open Wasm_ast

(* The children are visited with explicit [let]s: OCaml does not specify
   the evaluation order of constructor arguments. *)

let fold_map_list f acc l =
  let l, acc =
    List.fold_left l ~init:([], acc) ~f:(fun (l, acc) x ->
        let x, acc = f acc x in
        x :: l, acc)
  in
  List.rev l, acc

let fold_map_expression ~expression:f ~instructions:g acc e =
  let unary make e' =
    let e', acc = f acc e' in
    make e', acc
  in
  let binary make e1 e2 =
    let e1, acc = f acc e1 in
    let e2, acc = f acc e2 in
    make e1 e2, acc
  in
  match e with
  | Const _ | LocalGet _ | GlobalGet _ | Pop _ | RefFunc _ | RefNull _ -> e, acc
  | UnOp (op, e') -> unary (fun e' -> UnOp (op, e')) e'
  | I32WrapI64 e' -> unary (fun e' -> I32WrapI64 e') e'
  | I64ExtendI32 (s, e') -> unary (fun e' -> I64ExtendI32 (s, e')) e'
  | F32DemoteF64 e' -> unary (fun e' -> F32DemoteF64 e') e'
  | F64PromoteF32 e' -> unary (fun e' -> F64PromoteF32 e') e'
  | LocalTee (x, e') -> unary (fun e' -> LocalTee (x, e')) e'
  | RefI31 e' -> unary (fun e' -> RefI31 e') e'
  | I31Get (s, e') -> unary (fun e' -> I31Get (s, e')) e'
  | ArrayLen e' -> unary (fun e' -> ArrayLen e') e'
  | StructGet (s, ty, i, e') -> unary (fun e' -> StructGet (s, ty, i, e')) e'
  | RefCast (ty, e') -> unary (fun e' -> RefCast (ty, e')) e'
  | RefTest (ty, e') -> unary (fun e' -> RefTest (ty, e')) e'
  | Br_on_cast (i, ty, ty', e') -> unary (fun e' -> Br_on_cast (i, ty, ty', e')) e'
  | Br_on_cast_fail (i, ty, ty', e') ->
      unary (fun e' -> Br_on_cast_fail (i, ty, ty', e')) e'
  | Br_on_null (i, e') -> unary (fun e' -> Br_on_null (i, e')) e'
  | ExternConvertAny e' -> unary (fun e' -> ExternConvertAny e') e'
  | AnyConvertExtern e' -> unary (fun e' -> AnyConvertExtern e') e'
  | BinOp (op, e1, e2) -> binary (fun e1 e2 -> BinOp (op, e1, e2)) e1 e2
  | ArrayNew (ty, e1, e2) -> binary (fun e1 e2 -> ArrayNew (ty, e1, e2)) e1 e2
  | ArrayNewData (ty, d, e1, e2) ->
      binary (fun e1 e2 -> ArrayNewData (ty, d, e1, e2)) e1 e2
  | ArrayGet (s, ty, e1, e2) -> binary (fun e1 e2 -> ArrayGet (s, ty, e1, e2)) e1 e2
  | RefEq (e1, e2) -> binary (fun e1 e2 -> RefEq (e1, e2)) e1 e2
  | Call (fn, l) ->
      let l, acc = fold_map_list f acc l in
      Call (fn, l), acc
  | ArrayNewFixed (ty, l) ->
      let l, acc = fold_map_list f acc l in
      ArrayNewFixed (ty, l), acc
  | StructNew (ty, l) ->
      let l, acc = fold_map_list f acc l in
      StructNew (ty, l), acc
  | Call_ref (ty, fn, l) ->
      (* The arguments are evaluated before the function. *)
      let l, acc = fold_map_list f acc l in
      let fn, acc = f acc fn in
      Call_ref (ty, fn, l), acc
  | IfExpr (ty, cond, e1, e2) ->
      let cond, acc = f acc cond in
      let e1, acc = f acc e1 in
      let e2, acc = f acc e2 in
      IfExpr (ty, cond, e1, e2), acc
  | BlockExpr (ty, l) ->
      let l, acc = g acc l in
      BlockExpr (ty, l), acc
  | Seq (l, e') ->
      let l, acc = g acc l in
      let e', acc = f acc e' in
      Seq (l, e'), acc
  | Try (ty, body, catches) ->
      let body, acc = g acc body in
      Try (ty, body, catches), acc

let fold_map_instruction ~expression:f ~instructions:g acc i =
  let unary make e =
    let e, acc = f acc e in
    make e, acc
  in
  match i with
  | Nop | Event _ | Br (_, None) | Return None | Rethrow _ | Unreachable -> i, acc
  | Drop e -> unary (fun e -> Drop e) e
  | Push e -> unary (fun e -> Push e) e
  | LocalSet (x, e) -> unary (fun e -> LocalSet (x, e)) e
  | GlobalSet (x, e) -> unary (fun e -> GlobalSet (x, e)) e
  | Return (Some e) -> unary (fun e -> Return (Some e)) e
  | Throw (t, e) -> unary (fun e -> Throw (t, e)) e
  | Br (n, Some e) -> unary (fun e -> Br (n, Some e)) e
  | Br_if (n, e) -> unary (fun e -> Br_if (n, e)) e
  | Br_table (e, l, d) -> unary (fun e -> Br_table (e, l, d)) e
  | StructSet (ty, n, e1, e2) ->
      let e1, acc = f acc e1 in
      let e2, acc = f acc e2 in
      StructSet (ty, n, e1, e2), acc
  | ArraySet (ty, e1, e2, e3) ->
      let e1, acc = f acc e1 in
      let e2, acc = f acc e2 in
      let e3, acc = f acc e3 in
      ArraySet (ty, e1, e2, e3), acc
  | CallInstr (fn, l) ->
      let l, acc = fold_map_list f acc l in
      CallInstr (fn, l), acc
  | Return_call (fn, l) ->
      let l, acc = fold_map_list f acc l in
      Return_call (fn, l), acc
  | Return_call_ref (ty, fn, l) ->
      let l, acc = fold_map_list f acc l in
      let fn, acc = f acc fn in
      Return_call_ref (ty, fn, l), acc
  | If (ty, cond, l1, l2) ->
      let cond, acc = f acc cond in
      let l1, acc = g acc l1 in
      let l2, acc = g acc l2 in
      If (ty, cond, l1, l2), acc
  | Block (ty, l) ->
      let l, acc = g acc l in
      Block (ty, l), acc
  | Loop (ty, l) ->
      let l, acc = g acc l in
      Loop (ty, l), acc

let map_list f l = List.map ~f l

let map_expression ~expression:f ~instructions:g e =
  match e with
  | Const _ | LocalGet _ | GlobalGet _ | Pop _ | RefFunc _ | RefNull _ -> e
  | UnOp (op, e') -> UnOp (op, f e')
  | I32WrapI64 e' -> I32WrapI64 (f e')
  | I64ExtendI32 (s, e') -> I64ExtendI32 (s, f e')
  | F32DemoteF64 e' -> F32DemoteF64 (f e')
  | F64PromoteF32 e' -> F64PromoteF32 (f e')
  | LocalTee (x, e') -> LocalTee (x, f e')
  | RefI31 e' -> RefI31 (f e')
  | I31Get (s, e') -> I31Get (s, f e')
  | ArrayLen e' -> ArrayLen (f e')
  | StructGet (s, ty, i, e') -> StructGet (s, ty, i, f e')
  | RefCast (ty, e') -> RefCast (ty, f e')
  | RefTest (ty, e') -> RefTest (ty, f e')
  | Br_on_cast (i, ty, ty', e') -> Br_on_cast (i, ty, ty', f e')
  | Br_on_cast_fail (i, ty, ty', e') -> Br_on_cast_fail (i, ty, ty', f e')
  | Br_on_null (i, e') -> Br_on_null (i, f e')
  | ExternConvertAny e' -> ExternConvertAny (f e')
  | AnyConvertExtern e' -> AnyConvertExtern (f e')
  | BinOp (op, e1, e2) ->
      let e1 = f e1 in
      BinOp (op, e1, f e2)
  | ArrayNew (ty, e1, e2) ->
      let e1 = f e1 in
      ArrayNew (ty, e1, f e2)
  | ArrayNewData (ty, d, e1, e2) ->
      let e1 = f e1 in
      ArrayNewData (ty, d, e1, f e2)
  | ArrayGet (s, ty, e1, e2) ->
      let e1 = f e1 in
      ArrayGet (s, ty, e1, f e2)
  | RefEq (e1, e2) ->
      let e1 = f e1 in
      RefEq (e1, f e2)
  | Call (fn, l) -> Call (fn, map_list f l)
  | ArrayNewFixed (ty, l) -> ArrayNewFixed (ty, map_list f l)
  | StructNew (ty, l) -> StructNew (ty, map_list f l)
  | Call_ref (ty, fn, l) ->
      let l = map_list f l in
      Call_ref (ty, f fn, l)
  | IfExpr (ty, cond, e1, e2) ->
      let cond = f cond in
      let e1 = f e1 in
      IfExpr (ty, cond, e1, f e2)
  | BlockExpr (ty, l) -> BlockExpr (ty, g l)
  | Seq (l, e') ->
      let l = g l in
      Seq (l, f e')
  | Try (ty, body, catches) -> Try (ty, g body, catches)

let map_instruction ~expression:f ~instructions:g i =
  match i with
  | Nop | Event _ | Br (_, None) | Return None | Rethrow _ | Unreachable -> i
  | Drop e -> Drop (f e)
  | Push e -> Push (f e)
  | LocalSet (x, e) -> LocalSet (x, f e)
  | GlobalSet (x, e) -> GlobalSet (x, f e)
  | Return (Some e) -> Return (Some (f e))
  | Throw (t, e) -> Throw (t, f e)
  | Br (n, Some e) -> Br (n, Some (f e))
  | Br_if (n, e) -> Br_if (n, f e)
  | Br_table (e, l, d) -> Br_table (f e, l, d)
  | StructSet (ty, n, e1, e2) ->
      let e1 = f e1 in
      StructSet (ty, n, e1, f e2)
  | ArraySet (ty, e1, e2, e3) ->
      let e1 = f e1 in
      let e2 = f e2 in
      ArraySet (ty, e1, e2, f e3)
  | CallInstr (fn, l) -> CallInstr (fn, map_list f l)
  | Return_call (fn, l) -> Return_call (fn, map_list f l)
  | Return_call_ref (ty, fn, l) ->
      let l = map_list f l in
      Return_call_ref (ty, f fn, l)
  | If (ty, cond, l1, l2) ->
      let cond = f cond in
      let l1 = g l1 in
      If (ty, cond, l1, g l2)
  | Block (ty, l) -> Block (ty, g l)
  | Loop (ty, l) -> Loop (ty, g l)

let iter_expression ~expression:f ~instructions:g e =
  match e with
  | Const _ | LocalGet _ | GlobalGet _ | Pop _ | RefFunc _ | RefNull _ -> ()
  | UnOp (_, e')
  | I32WrapI64 e'
  | I64ExtendI32 (_, e')
  | F32DemoteF64 e'
  | F64PromoteF32 e'
  | LocalTee (_, e')
  | RefI31 e'
  | I31Get (_, e')
  | ArrayLen e'
  | StructGet (_, _, _, e')
  | RefCast (_, e')
  | RefTest (_, e')
  | Br_on_cast (_, _, _, e')
  | Br_on_cast_fail (_, _, _, e')
  | Br_on_null (_, e')
  | ExternConvertAny e'
  | AnyConvertExtern e' -> f e'
  | BinOp (_, e1, e2)
  | ArrayNew (_, e1, e2)
  | ArrayNewData (_, _, e1, e2)
  | ArrayGet (_, _, e1, e2)
  | RefEq (e1, e2) ->
      f e1;
      f e2
  | Call (_, l) | ArrayNewFixed (_, l) | StructNew (_, l) -> List.iter ~f l
  | Call_ref (_, fn, l) ->
      List.iter ~f l;
      f fn
  | IfExpr (_, cond, e1, e2) ->
      f cond;
      f e1;
      f e2
  | BlockExpr (_, l) | Try (_, l, _) -> g l
  | Seq (l, e') ->
      g l;
      f e'

let iter_instruction ~expression:f ~instructions:g i =
  match i with
  | Nop | Event _ | Br (_, None) | Return None | Rethrow _ | Unreachable -> ()
  | Drop e
  | Push e
  | LocalSet (_, e)
  | GlobalSet (_, e)
  | Return (Some e)
  | Throw (_, e)
  | Br (_, Some e)
  | Br_if (_, e)
  | Br_table (e, _, _) -> f e
  | StructSet (_, _, e1, e2) ->
      f e1;
      f e2
  | ArraySet (_, e1, e2, e3) ->
      f e1;
      f e2;
      f e3
  | CallInstr (_, l) | Return_call (_, l) -> List.iter ~f l
  | Return_call_ref (_, fn, l) ->
      List.iter ~f l;
      f fn
  | If (_, cond, l1, l2) ->
      f cond;
      g l1;
      g l2
  | Block (_, l) | Loop (_, l) -> g l

let rec iter_locals_expression ~read ~write e =
  match (e : expression) with
  | LocalGet x -> read x
  | LocalTee (x, e') ->
      iter_locals_expression ~read ~write e';
      write x
  | _ ->
      iter_expression
        ~expression:(iter_locals_expression ~read ~write)
        ~instructions:(iter_locals ~read ~write)
        e

and iter_locals ~read ~write l =
  List.iter l ~f:(fun (i : instruction) ->
      match i with
      | LocalSet (x, e) ->
          iter_locals_expression ~read ~write e;
          write x
      | _ ->
          iter_instruction
            ~expression:(iter_locals_expression ~read ~write)
            ~instructions:(iter_locals ~read ~write)
            i)
