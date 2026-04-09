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

(* Extract loops from the toplevel function into separate helper
   functions. Wasm engines tier up (compile to optimised native code)
   functions containing loops. Since the toplevel function is large and
   called only once, we want to avoid this: only the small helpers
   should be tiered up.

   A loop is extractable when all its branches stay within the loop
   body (no escaping [Br], [Return], or [Rethrow]).

   Variables used in the loop are split into parameters and locals.
   Parameters are variables whose pre-loop value may be needed:
   either read before being written in the loop body, or with a
   defaultable type (scalars, nullable refs, [ref eq], …).
   The remaining variables — non-defaultable non-nullable refs that
   are always written before their first read — become locals of the
   helper. They have no meaningful value before the loop, so passing
   them as parameters would introduce a read that did not exist in
   the original code. Since [Initialize_locals] has already run on
   the original function, these locals have a set/get pattern that
   the Wasm validator accepts without initialisation.

   Modified parameters are returned to the caller via a struct (or
   directly when there are zero or one). Locals are not returned:
   they are purely internal to the loop. *)

open! Stdlib
module W = Wasm_ast

(* Check that all branches in a loop body target labels within the loop.
   [depth] counts the number of enclosing control flow constructs
   including the loop itself, so it starts at 1 when called on the loop
   body. A [Br n] escapes the loop when [n >= depth]. *)

let rec is_contained_expr ~depth (e : W.expression) =
  match e with
  | Const _ | GlobalGet _ | Pop _ | RefFunc _ | RefNull _ -> true
  | LocalGet _ -> true
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
  | AnyConvertExtern e' -> is_contained_expr ~depth e'
  | LocalTee (_, e') -> is_contained_expr ~depth e'
  | BinOp (_, e1, e2)
  | ArrayNew (_, e1, e2)
  | ArrayNewData (_, _, e1, e2)
  | ArrayGet (_, _, e1, e2)
  | RefEq (e1, e2) -> is_contained_expr ~depth e1 && is_contained_expr ~depth e2
  | Br_on_cast (n, _, _, e') | Br_on_cast_fail (n, _, _, e') ->
      n < depth && is_contained_expr ~depth e'
  | Br_on_null (n, e') -> n < depth && is_contained_expr ~depth e'
  | Call (_, l) | ArrayNewFixed (_, l) | StructNew (_, l) ->
      List.for_all ~f:(is_contained_expr ~depth) l
  | Call_ref (_, e', l) ->
      is_contained_expr ~depth e' && List.for_all ~f:(is_contained_expr ~depth) l
  | BlockExpr (_, body) -> is_contained_instrs ~depth:(depth + 1) body
  | Seq (instrs, e') -> is_contained_instrs ~depth instrs && is_contained_expr ~depth e'
  | IfExpr (_, cond, e1, e2) ->
      is_contained_expr ~depth cond
      && is_contained_expr ~depth:(depth + 1) e1
      && is_contained_expr ~depth:(depth + 1) e2
  | Try (_, body, catches) ->
      is_contained_instrs ~depth:(depth + 1) body
      && List.for_all ~f:(fun (_, l, _) -> l < depth) catches

and is_contained_instr ~depth (i : W.instruction) =
  match i with
  | Drop e | GlobalSet (_, e) | Push e | Throw (_, e) -> is_contained_expr ~depth e
  | LocalSet (_, e) -> is_contained_expr ~depth e
  | Br (n, e_opt) -> (
      n < depth
      &&
      match e_opt with
      | None -> true
      | Some e -> is_contained_expr ~depth e)
  | Br_if (n, e) -> n < depth && is_contained_expr ~depth e
  | Br_table (e, targets, default) ->
      List.for_all ~f:(fun n -> n < depth) targets
      && default < depth
      && is_contained_expr ~depth e
  | Return _ | Return_call _ | Return_call_ref _ -> false
  | Loop (_, body) | Block (_, body) -> is_contained_instrs ~depth:(depth + 1) body
  | If (_, e, l1, l2) ->
      is_contained_expr ~depth e
      && is_contained_instrs ~depth:(depth + 1) l1
      && is_contained_instrs ~depth:(depth + 1) l2
  | CallInstr (_, l) -> List.for_all ~f:(is_contained_expr ~depth) l
  | Rethrow n -> n < depth
  | Nop | Unreachable | Event _ -> true
  | ArraySet (_, e1, e2, e3) ->
      is_contained_expr ~depth e1
      && is_contained_expr ~depth e2
      && is_contained_expr ~depth e3
  | StructSet (_, _, e1, e2) -> is_contained_expr ~depth e1 && is_contained_expr ~depth e2

and is_contained_instrs ~depth l = List.for_all ~f:(is_contained_instr ~depth) l

(* Collect local variables referenced in an instruction list.
   [reads]: variables appearing in [LocalGet].
   [writes]: variables appearing in [LocalSet] or [LocalTee]. *)

type var_sets =
  { reads : Code.Var.Set.t
  ; writes : Code.Var.Set.t
  }

let rec collect_expr acc (e : W.expression) =
  match e with
  | Const _ | GlobalGet _ | Pop _ | RefFunc _ | RefNull _ -> acc
  | LocalGet v -> { acc with reads = Code.Var.Set.add v acc.reads }
  | LocalTee (v, e') ->
      collect_expr { acc with writes = Code.Var.Set.add v acc.writes } e'
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
  | AnyConvertExtern e' -> collect_expr acc e'
  | BinOp (_, e1, e2)
  | ArrayNew (_, e1, e2)
  | ArrayNewData (_, _, e1, e2)
  | ArrayGet (_, _, e1, e2)
  | RefEq (e1, e2) -> collect_expr (collect_expr acc e1) e2
  | Br_on_cast (_, _, _, e') | Br_on_cast_fail (_, _, _, e') | Br_on_null (_, e') ->
      collect_expr acc e'
  | Call (_, l) | ArrayNewFixed (_, l) | StructNew (_, l) -> collect_exprs acc l
  | Call_ref (_, e', l) -> collect_expr (collect_exprs acc l) e'
  | BlockExpr (_, body) -> collect_instrs acc body
  | Seq (instrs, e') -> collect_expr (collect_instrs acc instrs) e'
  | IfExpr (_, cond, e1, e2) -> collect_expr (collect_expr (collect_expr acc cond) e1) e2
  | Try (_, body, _) -> collect_instrs acc body

and collect_exprs acc l = List.fold_left ~f:collect_expr ~init:acc l

and collect_instr acc (i : W.instruction) =
  match i with
  | Drop e | GlobalSet (_, e) | Push e | Throw (_, e) -> collect_expr acc e
  | LocalSet (v, e) -> collect_expr { acc with writes = Code.Var.Set.add v acc.writes } e
  | Br (_, Some e) | Br_if (_, e) | Br_table (e, _, _) -> collect_expr acc e
  | Br (_, None) | Return None | Nop | Unreachable | Event _ | Rethrow _ -> acc
  | Return (Some e) -> collect_expr acc e
  | Loop (_, body) | Block (_, body) -> collect_instrs acc body
  | If (_, e, l1, l2) -> collect_instrs (collect_instrs (collect_expr acc e) l1) l2
  | CallInstr (_, l) | Return_call (_, l) -> collect_exprs acc l
  | Return_call_ref (_, e', l) -> collect_expr (collect_exprs acc l) e'
  | ArraySet (_, e1, e2, e3) -> collect_expr (collect_expr (collect_expr acc e1) e2) e3
  | StructSet (_, _, e1, e2) -> collect_expr (collect_expr acc e1) e2

and collect_instrs acc l = List.fold_left ~f:collect_instr ~init:acc l

(* Transformation context *)

type ctx =
  { var_types : W.value_type Code.Var.Hashtbl.t
  ; mutable new_fields : W.module_field list
  }

let lookup_types ctx vars =
  Code.Var.Set.fold
    (fun v acc ->
      match Code.Var.Hashtbl.find_opt ctx.var_types v with
      | Some t -> (v, t) :: acc
      | None -> acc)
    vars
    []

let extract_loop ctx ~is_initialized _ty body =
  let { reads; writes } =
    collect_instrs { reads = Code.Var.Set.empty; writes = Code.Var.Set.empty } body
  in
  let all_vars = Code.Var.Set.union reads writes in
  (* Non-nullable ref variables that are not yet initialised when
     reaching the loop become locals (passing them as parameters would
     introduce a read that did not exist in the original code).
     Scalars and nullable refs are safe as parameters since they have
     Wasm default values. *)
  let local_vars =
    Code.Var.Set.filter
      (fun v ->
        (not (is_initialized v))
        &&
        match Code.Var.Hashtbl.find_opt ctx.var_types v with
        | Some (Ref { nullable = false; _ }) -> true
        | _ -> false)
      all_vars
  in
  let param_vars = Code.Var.Set.diff all_vars local_vars in
  let param_with_types = lookup_types ctx param_vars in
  let local_with_types = lookup_types ctx local_vars in
  (* Only return modified parameters — locals are loop-internal. *)
  let modified_with_types = lookup_types ctx (Code.Var.Set.inter param_vars writes) in
  let helper_name = Code.Var.fresh_n "loop_helper" in
  let args = List.map ~f:(fun (v, _) -> W.LocalGet v) param_with_types in
  let param_types = List.map ~f:snd param_with_types in
  let param_names = List.map ~f:fst param_with_types in
  let loop_instr = W.Loop ({ W.params = []; result = [] }, body) in
  let make_helper ~signature ~extra_body =
    W.Function
      { name = helper_name
      ; exported_name = None
      ; typ = None
      ; signature
      ; param_names
      ; locals = local_with_types
      ; body = loop_instr :: extra_body
      }
  in
  let result_types = List.map ~f:snd modified_with_types in
  let extra_body = List.map ~f:(fun (v, _) -> W.Push (LocalGet v)) modified_with_types in
  let signature = { W.params = param_types; result = result_types } in
  ctx.new_fields <- make_helper ~signature ~extra_body :: ctx.new_fields;
  match modified_with_types with
  | [] -> [ W.CallInstr (helper_name, args) ]
  | [ (v, _) ] -> [ W.LocalSet (v, Call (helper_name, args)) ]
  | _ ->
      (* Multi-value: call leaves results on the stack, pop in
         reverse order (last pushed = top of stack = first popped). *)
      W.CallInstr (helper_name, args)
      :: List.rev_map ~f:(fun (v, t) -> W.LocalSet (v, Pop t)) modified_with_types

let fork_il_ctx = Initialize_locals.fork_context

let rec transform_instrs ctx il_ctx instrs =
  List.concat_map ~f:(transform_instr ctx il_ctx) instrs

and transform_instr ctx il_ctx (i : W.instruction) =
  match i with
  | Loop (ty, body) when List.is_empty ty.result && is_contained_instrs ~depth:1 body ->
      (* Use the current initialized set — then scan the original
         instruction to update the outer context for what follows. *)
      let result =
        extract_loop ctx ~is_initialized:(Initialize_locals.is_initialized il_ctx) ty body
      in
      Initialize_locals.scan_instruction il_ctx i;
      result
  | Loop (ty, body) ->
      let inner = fork_il_ctx il_ctx in
      let body' = transform_instrs ctx inner body in
      Initialize_locals.scan_instruction il_ctx i;
      [ W.Loop (ty, body') ]
  | Block (ty, body) ->
      let inner = fork_il_ctx il_ctx in
      let body' = transform_instrs ctx inner body in
      Initialize_locals.scan_instruction il_ctx i;
      [ W.Block (ty, body') ]
  | If (ty, e, l1, l2) ->
      let inner1 = fork_il_ctx il_ctx in
      let inner2 = fork_il_ctx il_ctx in
      let l1' = transform_instrs ctx inner1 l1 in
      let l2' = transform_instrs ctx inner2 l2 in
      Initialize_locals.scan_instruction il_ctx i;
      [ W.If (ty, e, l1', l2') ]
  | _ ->
      Initialize_locals.scan_instruction il_ctx i;
      [ i ]

let f ~toplevel fields =
  List.concat_map
    ~f:(fun field ->
      match field with
      | W.Function ({ name; _ } as func) when Code.Var.equal name toplevel ->
          let var_types = Code.Var.Hashtbl.create 16 in
          List.iter2
            ~f:(fun v t -> Code.Var.Hashtbl.add var_types v t)
            func.param_names
            func.signature.params;
          List.iter ~f:(fun (v, t) -> Code.Var.Hashtbl.add var_types v t) func.locals;
          let ctx = { var_types; new_fields = [] } in
          let il_ctx = Initialize_locals.create_context () in
          List.iter ~f:(Initialize_locals.mark_initialized il_ctx) func.param_names;
          List.iter
            ~f:(fun (var, typ) ->
              match (typ : W.value_type) with
              | I32 | I64 | F32 | F64 | Ref { nullable = true; _ } ->
                  Initialize_locals.mark_initialized il_ctx var
              | Ref { nullable = false; _ } -> ())
            func.locals;
          let body = transform_instrs ctx il_ctx func.body in
          let func' = W.Function { func with body } in
          List.rev ctx.new_fields @ [ func' ]
      | _ -> [ field ])
    fields
