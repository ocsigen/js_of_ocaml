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

open Stdlib

type ctx =
  { mutable initialized : Code.Var.Set.t
  ; uninitialized : Code.Var.Set.t ref
  }

let mark_initialized ctx i = ctx.initialized <- Code.Var.Set.add i ctx.initialized

let fork_context { initialized; uninitialized } = { initialized; uninitialized }

let check_initialized ctx i =
  if not (Code.Var.Set.mem i ctx.initialized)
  then ctx.uninitialized := Code.Var.Set.add i !(ctx.uninitialized)

let rec scan_expression ctx e =
  match e with
  | Wasm_ast.Const _ | GlobalGet _ | Pop _ | RefFunc _ | RefNull _ -> ()
  | UnOp (_, e')
  | I32WrapI64 e'
  | I64ExtendI32 (_, e')
  | F32DemoteF64 e'
  | F64PromoteF32 e'
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
  | AnyConvertExtern e' -> scan_expression ctx e'
  | BinOp (_, e', e'')
  | ArrayNew (_, e', e'')
  | ArrayNewData (_, _, e', e'')
  | ArrayGet (_, _, e', e'')
  | RefEq (e', e'') ->
      scan_expression ctx e';
      scan_expression ctx e''
  | LocalGet i -> check_initialized ctx i
  | LocalTee (i, e') ->
      scan_expression ctx e';
      mark_initialized ctx i
  | Call_ref (_, e', l) ->
      scan_expressions ctx l;
      scan_expression ctx e'
  | Call (_, l) | ArrayNewFixed (_, l) | StructNew (_, l) -> scan_expressions ctx l
  | BlockExpr (_, l) -> scan_instructions ctx l
  | Seq (l, e') -> scan_instructions ctx (l @ [ Push e' ])
  | IfExpr (_, cond, e1, e2) ->
      scan_expression ctx cond;
      scan_expression (fork_context ctx) e1;
      scan_expression (fork_context ctx) e2
  | Try (_, body, _) -> scan_instructions ctx body

and scan_expressions ctx l = List.iter ~f:(fun e -> scan_expression ctx e) l

and scan_instruction ctx i =
  match i with
  | Wasm_ast.Drop e
  | GlobalSet (_, e)
  | Br (_, Some e)
  | Br_if (_, e)
  | Br_table (e, _, _)
  | Throw (_, e)
  | Return (Some e)
  | Push e -> scan_expression ctx e
  | StructSet (_, _, e, e') ->
      scan_expression ctx e;
      scan_expression ctx e'
  | LocalSet (i, e) ->
      scan_expression ctx e;
      mark_initialized ctx i
  | Loop (_, l) | Block (_, l) -> scan_instructions ctx l
  | If (_, e, l, l') ->
      scan_expression ctx e;
      scan_instructions ctx l;
      scan_instructions ctx l'
  | CallInstr (_, l) | Return_call (_, l) -> scan_expressions ctx l
  | Br (_, None) | Return None | Rethrow _ | Nop | Unreachable | Event _ -> ()
  | ArraySet (_, e, e', e'') ->
      scan_expression ctx e;
      scan_expression ctx e';
      scan_expression ctx e''
  | Return_call_ref (_, e', l) ->
      scan_expressions ctx l;
      scan_expression ctx e'

and scan_instructions ctx l =
  let ctx = fork_context ctx in
  List.iter ~f:(fun i -> scan_instruction ctx i) l

let rec rewrite_expression uninitialized (e : Wasm_ast.expression) =
  match e with
  | Const _ | GlobalGet _ | Pop _ | RefFunc _ | RefNull _ -> e
  | UnOp (op, e') -> UnOp (op, rewrite_expression uninitialized e')
  | I32WrapI64 e' -> I32WrapI64 (rewrite_expression uninitialized e')
  | I64ExtendI32 (s, e') -> I64ExtendI32 (s, rewrite_expression uninitialized e')
  | F32DemoteF64 e' -> F32DemoteF64 (rewrite_expression uninitialized e')
  | F64PromoteF32 e' -> F64PromoteF32 (rewrite_expression uninitialized e')
  | RefI31 e' -> RefI31 (rewrite_expression uninitialized e')
  | I31Get (s, e') -> I31Get (s, rewrite_expression uninitialized e')
  | ArrayLen e' -> ArrayLen (rewrite_expression uninitialized e')
  | StructGet (s, ty, i, e') -> StructGet (s, ty, i, rewrite_expression uninitialized e')
  | RefCast (ty, e') -> RefCast (ty, rewrite_expression uninitialized e')
  | RefTest (ty, e') -> RefTest (ty, rewrite_expression uninitialized e')
  | Br_on_cast (i, ty, ty', e') ->
      Br_on_cast (i, ty, ty', rewrite_expression uninitialized e')
  | Br_on_cast_fail (i, ty, ty', e') ->
      Br_on_cast_fail (i, ty, ty', rewrite_expression uninitialized e')
  | Br_on_null (i, e') -> Br_on_null (i, rewrite_expression uninitialized e')
  | BinOp (op, e', e'') ->
      BinOp (op, rewrite_expression uninitialized e', rewrite_expression uninitialized e'')
  | ArrayNew (ty, e', e'') ->
      ArrayNew
        (ty, rewrite_expression uninitialized e', rewrite_expression uninitialized e'')
  | ArrayNewData (ty, i, e', e'') ->
      ArrayNewData
        (ty, i, rewrite_expression uninitialized e', rewrite_expression uninitialized e'')
  | ArrayGet (s, ty, e', e'') ->
      ArrayGet
        (s, ty, rewrite_expression uninitialized e', rewrite_expression uninitialized e'')
  | RefEq (e', e'') ->
      RefEq (rewrite_expression uninitialized e', rewrite_expression uninitialized e'')
  | LocalGet i ->
      if Code.Var.Hashtbl.mem uninitialized i
      then RefCast (Code.Var.Hashtbl.find uninitialized i, e)
      else e
  | LocalTee (i, e') ->
      let e = Wasm_ast.LocalTee (i, rewrite_expression uninitialized e') in
      if Code.Var.Hashtbl.mem uninitialized i
      then RefCast (Code.Var.Hashtbl.find uninitialized i, e)
      else e
  | Call_ref (f, e', l) ->
      Call_ref
        (f, rewrite_expression uninitialized e', rewrite_expressions uninitialized l)
  | Call (f, l) -> Call (f, rewrite_expressions uninitialized l)
  | ArrayNewFixed (ty, l) -> ArrayNewFixed (ty, rewrite_expressions uninitialized l)
  | StructNew (ty, l) -> StructNew (ty, rewrite_expressions uninitialized l)
  | BlockExpr (ty, l) -> BlockExpr (ty, rewrite_instructions uninitialized l)
  | Seq (l, e') ->
      Seq (rewrite_instructions uninitialized l, rewrite_expression uninitialized e')
  | IfExpr (ty, cond, e1, e2) ->
      IfExpr
        ( ty
        , rewrite_expression uninitialized cond
        , rewrite_expression uninitialized e1
        , rewrite_expression uninitialized e2 )
  | Try (ty, body, catches) -> Try (ty, rewrite_instructions uninitialized body, catches)
  | ExternConvertAny e' -> ExternConvertAny (rewrite_expression uninitialized e')
  | AnyConvertExtern e' -> AnyConvertExtern (rewrite_expression uninitialized e')

and rewrite_expressions uninitialized l =
  List.map ~f:(fun e -> rewrite_expression uninitialized e) l

and rewrite_instruction uninitialized i =
  match i with
  | Wasm_ast.Drop e -> Wasm_ast.Drop (rewrite_expression uninitialized e)
  | GlobalSet (x, e) -> GlobalSet (x, rewrite_expression uninitialized e)
  | Br (i, Some e) -> Br (i, Some (rewrite_expression uninitialized e))
  | Br_if (i, e) -> Br_if (i, rewrite_expression uninitialized e)
  | Br_table (e, l, i) -> Br_table (rewrite_expression uninitialized e, l, i)
  | Throw (t, e) -> Throw (t, rewrite_expression uninitialized e)
  | Return (Some e) -> Return (Some (rewrite_expression uninitialized e))
  | Push e -> Push (rewrite_expression uninitialized e)
  | StructSet (ty, i, e, e') ->
      StructSet
        (ty, i, rewrite_expression uninitialized e, rewrite_expression uninitialized e')
  | LocalSet (i, e) -> LocalSet (i, rewrite_expression uninitialized e)
  | Loop (ty, l) -> Loop (ty, rewrite_instructions uninitialized l)
  | Block (ty, l) -> Block (ty, rewrite_instructions uninitialized l)
  | If (ty, e, l, l') ->
      If
        ( ty
        , rewrite_expression uninitialized e
        , rewrite_instructions uninitialized l
        , rewrite_instructions uninitialized l' )
  | CallInstr (f, l) -> CallInstr (f, rewrite_expressions uninitialized l)
  | Return_call (f, l) -> Return_call (f, rewrite_expressions uninitialized l)
  | Br (_, None) | Return None | Rethrow _ | Nop | Unreachable | Event _ -> i
  | ArraySet (ty, e, e', e'') ->
      ArraySet
        ( ty
        , rewrite_expression uninitialized e
        , rewrite_expression uninitialized e'
        , rewrite_expression uninitialized e'' )
  | Return_call_ref (f, e', l) ->
      Return_call_ref
        (f, rewrite_expression uninitialized e', rewrite_expressions uninitialized l)

and rewrite_instructions uninitialized l =
  List.map ~f:(fun i -> rewrite_instruction uninitialized i) l

let has_default (ty : Wasm_ast.heap_type) =
  match ty with
  | Any | Eq | I31 -> true
  | Func | Extern | Array | Struct | None_ | Type _ -> false

let f ~param_names ~locals instrs =
  let ctx =
    { initialized = Code.Var.Set.empty; uninitialized = ref Code.Var.Set.empty }
  in
  List.iter ~f:(fun x -> mark_initialized ctx x) param_names;
  List.iter
    ~f:(fun (var, typ) ->
      match (typ : Wasm_ast.value_type) with
      | I32 | I64 | F32 | F64 | Ref { nullable = true; _ } -> mark_initialized ctx var
      | Ref { nullable = false; _ } -> ())
    locals;
  scan_instructions ctx instrs;
  let local_types = Code.Var.Hashtbl.create 16 in
  let locals =
    List.map
      ~f:(fun ((var, typ) as local) ->
        match typ with
        | Ref ({ nullable = false; typ } as ref_typ) ->
            if Code.Var.Set.mem var !(ctx.uninitialized) && not (has_default typ)
            then (
              Code.Var.Hashtbl.add local_types var ref_typ;
              var, Wasm_ast.Ref { nullable = true; typ })
            else local
        | I32 | I64 | F32 | F64 | Ref { nullable = true; _ } -> local)
      locals
  in
  let initializations =
    List.filter_map
      ~f:(fun i ->
        if Code.Var.Hashtbl.mem local_types i
        then None
        else Some (Wasm_ast.LocalSet (i, RefI31 (Const (I32 0l)))))
      (Code.Var.Set.elements !(ctx.uninitialized))
  in
  let instrs =
    if Code.Var.Hashtbl.length local_types = 0
    then instrs
    else rewrite_instructions local_types instrs
  in
  locals, initializations @ instrs
