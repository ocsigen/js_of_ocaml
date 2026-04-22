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

(*
   Sink [local.set x e] to the first reachable [local.get x], turning the
   pair into a single [local.tee x e]. We search forward in evaluation
   order, stop at any control-flow boundary or compound construct, and
   bail if we meet another write to [x] (a [local.set x] or [local.tee
   x]) before a read.

   Effect ordering: moving [e] past other code is sound iff

   - [e] itself has no observable side effects (no writes, no calls,
     no traps beyond the ones [effect_free] already tolerates);
   - every sub-expression we cross is [effect_free]; and
   - no instruction we cross writes any local that [e] reads — if it
     did, [e]'s reads would see a different value at the new position.

   The last condition is what makes an "[effect_free] implies safe"
   rule unsound: [effect_free] expressions can still read mutable
   locals, and if the intermediate code writes those locals the move
   would change [e]'s result.
*)

open! Stdlib
module W = Wasm_ast
module Var = Code.Var

(* Same as [Gc_target.effect_free]. Copied here (it is a small and
   self-contained helper) to avoid exposing it as a shared interface. *)
let rec effect_free (e : W.expression) =
  match e with
  | Const _ | LocalGet _ | GlobalGet _ | RefFunc _ | RefNull _ -> true
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
  | ExternConvertAny e'
  | AnyConvertExtern e' -> effect_free e'
  | BinOp (_, e1, e2)
  | ArrayNew (_, e1, e2)
  | ArrayNewData (_, _, e1, e2)
  | ArrayGet (_, _, e1, e2)
  | RefEq (e1, e2) -> effect_free e1 && effect_free e2
  | LocalTee _
  | BlockExpr _
  | Call _
  | Seq _
  | Pop _
  | Call_ref _
  | Br_on_cast _
  | Br_on_cast_fail _
  | Br_on_null _
  | Try _ -> false
  | IfExpr (_, e1, e2, e3) -> effect_free e1 && effect_free e2 && effect_free e3
  | ArrayNewFixed (_, l) | StructNew (_, l) -> List.for_all ~f:effect_free l

(* Set of locals read by [e] (LocalGet occurrences). LocalTee's inner
   expression is also walked, but the tee'd variable itself is a write,
   not a read. *)
let rec reads_of_expr acc (e : W.expression) =
  match e with
  | Const _ | GlobalGet _ | RefFunc _ | RefNull _ | Pop _ -> acc
  | LocalGet x -> Var.Set.add x acc
  | LocalTee (_, e')
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
  | ExternConvertAny e'
  | AnyConvertExtern e'
  | Br_on_cast (_, _, _, e')
  | Br_on_cast_fail (_, _, _, e')
  | Br_on_null (_, e') -> reads_of_expr acc e'
  | BinOp (_, e1, e2)
  | ArrayNew (_, e1, e2)
  | ArrayNewData (_, _, e1, e2)
  | ArrayGet (_, _, e1, e2)
  | RefEq (e1, e2) -> reads_of_expr (reads_of_expr acc e1) e2
  | Call (_, l) | ArrayNewFixed (_, l) | StructNew (_, l) ->
      List.fold_left l ~init:acc ~f:reads_of_expr
  | Call_ref (_, f, l) ->
      let acc = reads_of_expr acc f in
      List.fold_left l ~init:acc ~f:reads_of_expr
  | IfExpr (_, c, t, e) -> reads_of_expr (reads_of_expr (reads_of_expr acc c) t) e
  | BlockExpr (_, l) -> reads_of_instrs acc l
  | Seq (l, e') -> reads_of_expr (reads_of_instrs acc l) e'
  | Try (_, body, _) -> reads_of_instrs acc body

and reads_of_instrs acc l = List.fold_left l ~init:acc ~f:reads_of_instr

and reads_of_instr acc (i : W.instruction) =
  match i with
  | Nop | Event _ | Br (_, None) | Return None | Rethrow _ | Unreachable -> acc
  | Drop e
  | Push e
  | GlobalSet (_, e)
  | LocalSet (_, e)
  | Br (_, Some e)
  | Br_if (_, e)
  | Br_table (e, _, _)
  | Throw (_, e)
  | Return (Some e) -> reads_of_expr acc e
  | StructSet (_, _, e1, e2) -> reads_of_expr (reads_of_expr acc e1) e2
  | ArraySet (_, e1, e2, e3) -> reads_of_expr (reads_of_expr (reads_of_expr acc e1) e2) e3
  | CallInstr (_, l) | Return_call (_, l) -> List.fold_left l ~init:acc ~f:reads_of_expr
  | Return_call_ref (_, f, l) ->
      List.fold_left l ~init:(reads_of_expr acc f) ~f:reads_of_expr
  | Loop (_, l) | Block (_, l) -> reads_of_instrs acc l
  | If (_, c, t, e) -> reads_of_instrs (reads_of_instrs (reads_of_expr acc c) t) e

(* Walker result for a single expression. [Clean] means "no occurrence
   of x in this expression; the caller may continue past, using
   [effect_free] to gate the path-clean state". *)
type walk_result =
  | Found of W.expression
  | Bail
  | Clean

(* [reads] is [reads_of_expr e_to_sink]: the set of locals we must not
   let an intermediate [LocalSet]/[LocalTee] write. *)
(* [ctx] bundles the parameters that don't change during a sink attempt:
   the target variable, the expression to sink, its read set, and whether
   the expression itself is [effect_free]. When [e_effect_free] is
   [false] we may not cross any *evaluated* sub-expression or
   instruction even if it is itself [effect_free] — the path could
   read heap/global state that [e]'s side effects would change. *)
type ctx =
  { x : Var.t
  ; e_to_sink : W.expression
  ; reads : Var.Set.t
  ; e_effect_free : bool
  }

(* Purely-local expression — no heap/global reads, no calls, no traps.
   Stricter than [effect_free]: a [GlobalGet] or [ArrayGet] is
   [effect_free] but not [trivially_pure], because crossing it with an
   effectful [e] would reorder a read against [e]'s writes. *)
let rec trivially_pure (e : W.expression) =
  match e with
  | W.Const _ | RefFunc _ | RefNull _ | LocalGet _ | Pop _ -> true
  | UnOp (_, e')
  | I32WrapI64 e'
  | I64ExtendI32 (_, e')
  | F32DemoteF64 e'
  | F64PromoteF32 e'
  | RefI31 e'
  | I31Get (_, e')
  | ExternConvertAny e'
  | AnyConvertExtern e' -> trivially_pure e'
  | BinOp (_, e1, e2) | RefEq (e1, e2) -> trivially_pure e1 && trivially_pure e2
  | LocalTee _
  | GlobalGet _
  | ArrayLen _
  | StructGet _
  | ArrayGet _
  | ArrayNew _
  | ArrayNewData _
  | ArrayNewFixed _
  | StructNew _
  | RefCast _
  | RefTest _
  | Br_on_cast _
  | Br_on_cast_fail _
  | Br_on_null _
  | Call _
  | Call_ref _
  | IfExpr _
  | BlockExpr _
  | Seq _
  | Try _ -> false

(* A sibling sub-expression was walked [Clean] (no x) and we're about
   to continue to the next sibling. This is the reorder point: [e] will
   evaluate *after* [sibling] in the sunk version, whereas originally
   [e] ran first. When [e] itself is [effect_free] it cannot observe
   or alter anything an [effect_free] sibling does; when [e] may have
   side effects (a Call, say), the sibling must be stricter — no
   heap/global reads either, since those would see different values
   after the move. *)
let may_cross_sibling ctx sibling =
  effect_free sibling && (ctx.e_effect_free || trivially_pure sibling)

let rec walk_expr ctx (e : W.expression) =
  match e with
  | W.Const _ | GlobalGet _ | RefFunc _ | RefNull _ | Pop _ -> Clean
  | LocalGet y ->
      if Var.equal y ctx.x then Found (W.LocalTee (ctx.x, ctx.e_to_sink)) else Clean
  | LocalTee (y, _) when Var.equal y ctx.x ->
      (* Another write to [x] — bail. *)
      Bail
  | LocalTee (y, e') -> (
      (* Reading [e'] first, then this tee writes [y]. If [y] is read by
         our sink expression, we cannot cross this write. *)
      match walk_expr ctx e' with
      | Found e'' -> Found (W.LocalTee (y, e''))
      | Bail -> Bail
      | Clean -> if Var.Set.mem y ctx.reads then Bail else Clean)
  | UnOp (op, e') -> wrap_unary (fun e -> W.UnOp (op, e)) ctx e'
  | I32WrapI64 e' -> wrap_unary (fun e -> W.I32WrapI64 e) ctx e'
  | I64ExtendI32 (s, e') -> wrap_unary (fun e -> W.I64ExtendI32 (s, e)) ctx e'
  | F32DemoteF64 e' -> wrap_unary (fun e -> W.F32DemoteF64 e) ctx e'
  | F64PromoteF32 e' -> wrap_unary (fun e -> W.F64PromoteF32 e) ctx e'
  | RefI31 e' -> wrap_unary (fun e -> W.RefI31 e) ctx e'
  | I31Get (s, e') -> wrap_unary (fun e -> W.I31Get (s, e)) ctx e'
  | ArrayLen e' -> wrap_unary (fun e -> W.ArrayLen e) ctx e'
  | StructGet (s, ty, i, e') -> wrap_unary (fun e -> W.StructGet (s, ty, i, e)) ctx e'
  | RefCast (ty, e') -> wrap_unary (fun e -> W.RefCast (ty, e)) ctx e'
  | RefTest (ty, e') -> wrap_unary (fun e -> W.RefTest (ty, e)) ctx e'
  | ExternConvertAny e' -> wrap_unary (fun e -> W.ExternConvertAny e) ctx e'
  | AnyConvertExtern e' -> wrap_unary (fun e -> W.AnyConvertExtern e) ctx e'
  | BinOp (op, e1, e2) -> wrap_binary (fun a b -> W.BinOp (op, a, b)) ctx e1 e2
  | ArrayNew (ty, e1, e2) -> wrap_binary (fun a b -> W.ArrayNew (ty, a, b)) ctx e1 e2
  | ArrayNewData (ty, d, e1, e2) ->
      wrap_binary (fun a b -> W.ArrayNewData (ty, d, a, b)) ctx e1 e2
  | ArrayGet (s, ty, e1, e2) ->
      wrap_binary (fun a b -> W.ArrayGet (s, ty, a, b)) ctx e1 e2
  | RefEq (e1, e2) -> wrap_binary (fun a b -> W.RefEq (a, b)) ctx e1 e2
  | Call (f, args) -> wrap_list (fun args' -> W.Call (f, args')) ctx args
  | ArrayNewFixed (ty, args) ->
      wrap_list (fun args' -> W.ArrayNewFixed (ty, args')) ctx args
  | StructNew (ty, args) -> wrap_list (fun args' -> W.StructNew (ty, args')) ctx args
  | Call_ref (ty, f, args) -> (
      (* Wasm evaluates args before the funcref. *)
      match wrap_list_intermediate ctx args with
      | `Found args' -> Found (W.Call_ref (ty, f, args'))
      | `Bail -> Bail
      | `Clean -> (
          (* Between args and f, we've crossed every arg — allowed
              only if each is [may_cross_sibling]-safe. That check was
              already made inside [wrap_list_intermediate]. *)
          match walk_expr ctx f with
          | Found f' -> Found (W.Call_ref (ty, f', args))
          | Bail -> Bail
          | Clean -> Clean))
  | IfExpr _
  | BlockExpr _
  | Seq _
  | Try _
  | Br_on_cast _
  | Br_on_cast_fail _
  | Br_on_null _ ->
      (* Compound / control-flow-like expressions; we don't descend past
         these for sinking purposes. *)
      Bail

and wrap_unary make ctx e' =
  match walk_expr ctx e' with
  | Found e'' -> Found (make e'')
  | Bail -> Bail
  | Clean -> Clean

and wrap_binary make ctx e1 e2 =
  match walk_expr ctx e1 with
  | Found e1' -> Found (make e1' e2)
  | Bail -> Bail
  | Clean -> (
      if not (may_cross_sibling ctx e1)
      then Bail
      else
        match walk_expr ctx e2 with
        | Found e2' -> Found (make e1 e2')
        | Bail -> Bail
        | Clean -> Clean)

and wrap_list make ctx args =
  match wrap_list_intermediate ctx args with
  | `Found args' -> Found (make args')
  | `Bail -> Bail
  | `Clean -> Clean

and wrap_list_intermediate ctx args =
  let rec loop acc = function
    | [] -> `Clean
    | a :: rest -> (
        match walk_expr ctx a with
        | Found a' -> `Found (List.rev_append acc (a' :: rest))
        | Bail -> `Bail
        | Clean -> if not (may_cross_sibling ctx a) then `Bail else loop (a :: acc) rest)
  in
  loop [] args

(* Walk a single instruction looking for a sink target for [ctx.x]. *)
type instr_result =
  | IFound of W.instruction
  | IBail
  | IClean

let try_sink_in_instr ctx instr : instr_result =
  let wrap_one make e =
    match walk_expr ctx e with
    | Found e' -> IFound (make e')
    | Bail -> IBail
    | Clean -> IClean
  in
  match (instr : W.instruction) with
  | Nop | Event _ -> IClean
  | Drop e -> wrap_one (fun e -> W.Drop e) e
  | Push e -> wrap_one (fun e -> W.Push e) e
  | LocalSet (y, e) when Var.equal y ctx.x -> (
      (* x may still appear inside [e] (evaluated before the set). If we
         find and rewrite it there, we stop (the [local.set x] after
         would be a shadowing write). If [e] is [Clean], this is a
         shadowing write without a sink target → bail. *)
      match walk_expr ctx e with
      | Found e' -> IFound (W.LocalSet (y, e'))
      | Bail -> IBail
      | Clean -> IBail)
  | LocalSet (y, e) -> wrap_one (fun e -> W.LocalSet (y, e)) e
  | GlobalSet (g, e) -> wrap_one (fun e -> W.GlobalSet (g, e)) e
  | StructSet (ty, i, e1, e2) -> (
      match walk_expr ctx e1 with
      | Found e1' -> IFound (W.StructSet (ty, i, e1', e2))
      | Bail -> IBail
      | Clean -> (
          if not (may_cross_sibling ctx e1)
          then IBail
          else
            match walk_expr ctx e2 with
            | Found e2' -> IFound (W.StructSet (ty, i, e1, e2'))
            | Bail -> IBail
            | Clean -> IClean))
  | ArraySet (ty, e1, e2, e3) -> (
      match walk_expr ctx e1 with
      | Found e1' -> IFound (W.ArraySet (ty, e1', e2, e3))
      | Bail -> IBail
      | Clean -> (
          if not (may_cross_sibling ctx e1)
          then IBail
          else
            match walk_expr ctx e2 with
            | Found e2' -> IFound (W.ArraySet (ty, e1, e2', e3))
            | Bail -> IBail
            | Clean -> (
                if not (may_cross_sibling ctx e2)
                then IBail
                else
                  match walk_expr ctx e3 with
                  | Found e3' -> IFound (W.ArraySet (ty, e1, e2, e3'))
                  | Bail -> IBail
                  | Clean -> IClean)))
  | CallInstr (f, args) -> (
      match wrap_list_intermediate ctx args with
      | `Found args' -> IFound (W.CallInstr (f, args'))
      | `Bail -> IBail
      | `Clean -> IClean)
  (* Control-flow-terminal instructions with sub-expressions: we can still
     rewrite within the expression, but cannot continue past on a Clean. *)
  | Return (Some e) -> (
      match walk_expr ctx e with
      | Found e' -> IFound (W.Return (Some e'))
      | Bail -> IBail
      | Clean -> IBail)
  | Throw (t, e) -> (
      match walk_expr ctx e with
      | Found e' -> IFound (W.Throw (t, e'))
      | Bail -> IBail
      | Clean -> IBail)
  | Br (n, Some e) -> (
      match walk_expr ctx e with
      | Found e' -> IFound (W.Br (n, Some e'))
      | Bail -> IBail
      | Clean -> IBail)
  | Br_if (n, e) -> (
      match walk_expr ctx e with
      | Found e' -> IFound (W.Br_if (n, e'))
      | Bail -> IBail
      | Clean -> IBail)
  | Br_table (e, tl, d) -> (
      match walk_expr ctx e with
      | Found e' -> IFound (W.Br_table (e', tl, d))
      | Bail -> IBail
      | Clean -> IBail)
  | Return_call (f, args) -> (
      match wrap_list_intermediate ctx args with
      | `Found args' -> IFound (W.Return_call (f, args'))
      | `Bail -> IBail
      | `Clean -> IBail)
  | Return_call_ref (ty, f, args) -> (
      match wrap_list_intermediate ctx args with
      | `Found args' -> IFound (W.Return_call_ref (ty, f, args'))
      | `Bail -> IBail
      | `Clean -> (
          match walk_expr ctx f with
          | Found f' -> IFound (W.Return_call_ref (ty, f', args))
          | Bail -> IBail
          | Clean -> IBail))
  | Return None | Br (_, None) | Rethrow _ | Unreachable -> IBail
  | If (ty, cond, l1, l2) -> (
      (* The condition is evaluated unconditionally before branching —
         we can still sink into it. If no [x] is found there, we bail
         because we can't continue past the branch. *)
      match walk_expr ctx cond with
      | Found cond' -> IFound (W.If (ty, cond', l1, l2))
      | Bail -> IBail
      | Clean -> IBail)
  | Loop _ | Block _ ->
      (* No expression to walk at this level; do not sink into the body. *)
      IBail

(* Can we cross this instruction without [x] being found? Must be safe
   with respect to [ctx.e_to_sink]'s potential side effects. *)
let can_cross_instr ctx (instr : W.instruction) =
  match instr with
  | Nop | Event _ -> true
  | Drop e | Push e -> may_cross_sibling ctx e
  | LocalSet (y, e) -> may_cross_sibling ctx e && not (Var.Set.mem y ctx.reads)
  | _ -> false

let try_sink_in_list ctx instrs =
  let rec loop acc = function
    | [] -> None
    | instr :: rest -> (
        match try_sink_in_instr ctx instr with
        | IFound instr' -> Some (List.rev_append acc (instr' :: rest))
        | IBail -> None
        | IClean -> if can_cross_instr ctx instr then loop (instr :: acc) rest else None)
  in
  loop [] instrs

(* Bottom-up transformation: recurse first, then try to sink each
   [local.set] into the (already-transformed) tail. *)
let rec transform_instrs instrs =
  match instrs with
  | [] -> []
  | W.LocalSet (x, e) :: rest -> (
      let e = transform_expr e in
      let rest = transform_instrs rest in
      let ctx =
        { x
        ; e_to_sink = e
        ; reads = reads_of_expr Var.Set.empty e
        ; e_effect_free = effect_free e
        }
      in
      match try_sink_in_list ctx rest with
      | Some new_rest -> new_rest
      | None -> W.LocalSet (x, e) :: rest)
  | instr :: rest ->
      let instr = transform_instr instr in
      let rest = transform_instrs rest in
      instr :: rest

and transform_instr (instr : W.instruction) : W.instruction =
  match instr with
  | Nop | Event _ | Br (_, None) | Return None | Rethrow _ | Unreachable -> instr
  | Drop e -> Drop (transform_expr e)
  | Push e -> Push (transform_expr e)
  | LocalSet (x, e) -> LocalSet (x, transform_expr e)
  | GlobalSet (g, e) -> GlobalSet (g, transform_expr e)
  | StructSet (ty, i, e1, e2) -> StructSet (ty, i, transform_expr e1, transform_expr e2)
  | ArraySet (ty, e1, e2, e3) ->
      ArraySet (ty, transform_expr e1, transform_expr e2, transform_expr e3)
  | CallInstr (f, args) -> CallInstr (f, List.map args ~f:transform_expr)
  | Return (Some e) -> Return (Some (transform_expr e))
  | Throw (t, e) -> Throw (t, transform_expr e)
  | Br (n, Some e) -> Br (n, Some (transform_expr e))
  | Br_if (n, e) -> Br_if (n, transform_expr e)
  | Br_table (e, tl, d) -> Br_table (transform_expr e, tl, d)
  | Return_call (f, args) -> Return_call (f, List.map args ~f:transform_expr)
  | Return_call_ref (ty, f, args) ->
      Return_call_ref (ty, transform_expr f, List.map args ~f:transform_expr)
  | Loop (ty, l) -> Loop (ty, transform_instrs l)
  | Block (ty, l) -> Block (ty, transform_instrs l)
  | If (ty, cond, l1, l2) ->
      If (ty, transform_expr cond, transform_instrs l1, transform_instrs l2)

and transform_expr (e : W.expression) : W.expression =
  match e with
  | Const _ | LocalGet _ | GlobalGet _ | Pop _ | RefFunc _ | RefNull _ -> e
  | UnOp (op, e') -> UnOp (op, transform_expr e')
  | I32WrapI64 e' -> I32WrapI64 (transform_expr e')
  | I64ExtendI32 (s, e') -> I64ExtendI32 (s, transform_expr e')
  | F32DemoteF64 e' -> F32DemoteF64 (transform_expr e')
  | F64PromoteF32 e' -> F64PromoteF32 (transform_expr e')
  | RefI31 e' -> RefI31 (transform_expr e')
  | I31Get (s, e') -> I31Get (s, transform_expr e')
  | ArrayLen e' -> ArrayLen (transform_expr e')
  | StructGet (s, ty, i, e') -> StructGet (s, ty, i, transform_expr e')
  | RefCast (ty, e') -> RefCast (ty, transform_expr e')
  | RefTest (ty, e') -> RefTest (ty, transform_expr e')
  | ExternConvertAny e' -> ExternConvertAny (transform_expr e')
  | AnyConvertExtern e' -> AnyConvertExtern (transform_expr e')
  | Br_on_cast (i, ty, ty', e') -> Br_on_cast (i, ty, ty', transform_expr e')
  | Br_on_cast_fail (i, ty, ty', e') -> Br_on_cast_fail (i, ty, ty', transform_expr e')
  | Br_on_null (i, e') -> Br_on_null (i, transform_expr e')
  | LocalTee (x, e') -> LocalTee (x, transform_expr e')
  | BinOp (op, e1, e2) -> BinOp (op, transform_expr e1, transform_expr e2)
  | ArrayNew (ty, e1, e2) -> ArrayNew (ty, transform_expr e1, transform_expr e2)
  | ArrayNewData (ty, d, e1, e2) ->
      ArrayNewData (ty, d, transform_expr e1, transform_expr e2)
  | ArrayGet (s, ty, e1, e2) -> ArrayGet (s, ty, transform_expr e1, transform_expr e2)
  | RefEq (e1, e2) -> RefEq (transform_expr e1, transform_expr e2)
  | Call (f, args) -> Call (f, List.map args ~f:transform_expr)
  | ArrayNewFixed (ty, args) -> ArrayNewFixed (ty, List.map args ~f:transform_expr)
  | StructNew (ty, args) -> StructNew (ty, List.map args ~f:transform_expr)
  | Call_ref (ty, f, args) ->
      Call_ref (ty, transform_expr f, List.map args ~f:transform_expr)
  | IfExpr (ty, cond, t, el) ->
      IfExpr (ty, transform_expr cond, transform_expr t, transform_expr el)
  | BlockExpr (ty, l) -> BlockExpr (ty, transform_instrs l)
  | Seq (l, e') -> Seq (transform_instrs l, transform_expr e')
  | Try (ty, body, catches) -> Try (ty, transform_instrs body, catches)

let f instrs = transform_instrs instrs
