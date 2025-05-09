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
  | Br_on_null (_, e') -> scan_expression ctx e'
  | ExternConvertAny e' -> scan_expression ctx e'
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
  List.map
    ~f:(fun i -> Wasm_ast.LocalSet (i, RefI31 (Const (I32 0l))))
    (Code.Var.Set.elements !(ctx.uninitialized))
  @ instrs
