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
   A variable becomes a parameter when its pre-loop value is needed:
   either it is a non-nullable [ref] (no default value available), the
   body's first use of it may be a read, or the body writes it and the
   caller reads it after the loop (a conditional in-loop write must not
   discard the caller's value, since the helper writes back what it
   returns). The remaining variables become locals of the helper —
   non-nullable refs that the body sets before getting (the Wasm
   validator accepts this since [Initialize_locals] has already run on
   the original function), or defaultable types whose helper default
   matches the caller's.

   Modified parameters read by the caller are returned via a struct
   (or directly when there are zero or one). Locals are never returned:
   by the parameter rules above, any modified variable read by the
   caller is already a parameter. *)

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

let read_before_written_instrs body =
  let rec expr (reads, writes) (e : W.expression) =
    match e with
    | Const _ | GlobalGet _ | Pop _ | RefFunc _ | RefNull _ -> (reads, writes)
    | LocalGet v ->
        if Code.Var.Set.mem v writes
        then (reads, writes)
        else (Code.Var.Set.add v reads, writes)
    | LocalTee (v, e') ->
        let reads, writes = expr (reads, writes) e' in
        (reads, Code.Var.Set.add v writes)
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
    | AnyConvertExtern e' -> expr (reads, writes) e'
    | BinOp (_, e1, e2)
    | ArrayNew (_, e1, e2)
    | ArrayNewData (_, _, e1, e2)
    | ArrayGet (_, _, e1, e2)
    | RefEq (e1, e2) -> expr (expr (reads, writes) e1) e2
    | Br_on_cast (_, _, _, e') | Br_on_cast_fail (_, _, _, e') | Br_on_null (_, e') ->
        expr (reads, writes) e'
    | Call (_, l) | ArrayNewFixed (_, l) | StructNew (_, l) -> exprs (reads, writes) l
    | Call_ref (_, e', l) -> expr (exprs (reads, writes) l) e'
    | BlockExpr (_, body) -> instrs (reads, writes) body
    | Seq (instrs', e') -> expr (instrs (reads, writes) instrs') e'
    | IfExpr (_, cond, e1, e2) ->
        let reads, writes = expr (reads, writes) cond in
        let reads1, writes1 = expr (reads, writes) e1 in
        let reads2, writes2 = expr (reads, writes) e2 in
        (Code.Var.Set.union reads1 reads2, Code.Var.Set.inter writes1 writes2)
    | Try (_, body, _) -> instrs (reads, writes) body

  and exprs acc l = List.fold_left ~f:expr ~init:acc l

  and instr (reads, writes) (i : W.instruction) =
    match i with
    | Drop e | GlobalSet (_, e) | Push e | Throw (_, e) -> expr (reads, writes) e
    | LocalSet (v, e) ->
        let reads, writes = expr (reads, writes) e in
        (reads, Code.Var.Set.add v writes)
    | Br (_, Some e) | Br_if (_, e) | Br_table (e, _, _) -> expr (reads, writes) e
    | Br (_, None) | Return None | Nop | Unreachable | Event _ | Rethrow _ -> (reads, writes)
    | Return (Some e) -> expr (reads, writes) e
    | Loop (_, body) | Block (_, body) -> instrs (reads, writes) body
    | If (_, e, l1, l2) ->
        let reads, writes = expr (reads, writes) e in
        let reads1, writes1 = instrs (reads, writes) l1 in
        let reads2, writes2 = instrs (reads, writes) l2 in
        (Code.Var.Set.union reads1 reads2, Code.Var.Set.inter writes1 writes2)
    | CallInstr (_, l) | Return_call (_, l) -> exprs (reads, writes) l
    | Return_call_ref (_, e', l) -> expr (exprs (reads, writes) l) e'
    | ArraySet (_, e1, e2, e3) -> expr (expr (expr (reads, writes) e1) e2) e3
    | StructSet (_, _, e1, e2) -> expr (expr (reads, writes) e1) e2

  and instrs acc l = List.fold_left ~f:instr ~init:acc l
  in
  let reads, _ = instrs (Code.Var.Set.empty, Code.Var.Set.empty) body in
  reads

let empty_var_sets = { reads = Code.Var.Set.empty; writes = Code.Var.Set.empty }

let reads_in_expr e = (collect_expr empty_var_sets e).reads

let reads_in_instr i = (collect_instr empty_var_sets i).reads

(* Backward scan over the function body, producing one entry per [Loop]
   encountered in source order: [Some s] for extractable loops, where
   [s] is the set of variables read after the loop on any path through
   the rest of the function; [None] for non-extractable loops. The
   forward pass in [transform_instrs] consumes the list in the same
   order. *)
let scan_right_to_left body =
  let rec instr (loops, acc_reads) i =
    match i with
    | W.Loop (ty, body) when List.is_empty ty.result && is_contained_instrs ~depth:1 body ->
        let loops' = Some acc_reads :: loops in
        let acc_reads' =
          Code.Var.Set.union acc_reads (read_before_written_instrs body)
        in
        (loops', acc_reads')
    | W.Loop (_, body) ->
        let acc_reads' =
          Code.Var.Set.union acc_reads (read_before_written_instrs body)
        in
        let loops', acc_reads'' = instrs (loops, acc_reads') body in
        (None :: loops', acc_reads'')
    | W.Block (_, body) -> instrs (loops, acc_reads) body
    | W.If (_, cond, l1, l2) ->
        let loops', l2_reads = instrs (loops, acc_reads) l2 in
        let loops'', l1_reads = instrs (loops', acc_reads) l1 in
        let acc_reads' =
          Code.Var.Set.union
            (reads_in_expr cond)
            (Code.Var.Set.union l1_reads l2_reads)
        in
        (loops'', acc_reads')
    | W.LocalSet (v, e) ->
        let acc_reads' =
          Code.Var.Set.union (Code.Var.Set.remove v acc_reads) (reads_in_expr e)
        in
        (loops, acc_reads')
    | _ -> (loops, Code.Var.Set.union acc_reads (reads_in_instr i))
  and instrs (loops, acc_reads) l =
    List.fold_right l ~init:(loops, acc_reads) ~f:(fun i acc -> instr acc i)
  in
  let loops, _ = instrs ([], Code.Var.Set.empty) body in
  loops

(* Transformation context *)

type ctx =
  { var_types : W.value_type Code.Var.Hashtbl.t
  ; mutable new_fields : W.module_field list
  ; mutable extra_locals : (Code.Var.t * W.value_type) list
  }

let lookup_types ctx vars =
  Code.Var.Set.fold
    (fun v acc ->
      match Code.Var.Hashtbl.find_opt ctx.var_types v with
      | Some t -> (v, t) :: acc
      | None -> acc)
    vars
    []

let extract_loop ctx ~is_initialized ~after_reads body =
  let { reads; writes } = collect_instrs empty_var_sets body in
  let all_vars = Code.Var.Set.union reads writes in
  let read_before_written = read_before_written_instrs body in
  let param_vars =
    Code.Var.Set.filter
      (fun v ->
        is_initialized v
        && ((match Code.Var.Hashtbl.find_opt ctx.var_types v with
             | Some (Ref { nullable = false; _ }) -> true
             | _ -> false)
            || Code.Var.Set.mem v read_before_written
            || (Code.Var.Set.mem v writes && Code.Var.Set.mem v after_reads)))
      all_vars
  in
  let local_vars = Code.Var.Set.diff all_vars param_vars in
  let param_with_types = lookup_types ctx param_vars in
  let local_with_types = lookup_types ctx local_vars in
  let returned_vars = Code.Var.Set.inter writes after_reads in
  let modified_with_types = lookup_types ctx returned_vars in
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
  match modified_with_types with
  | [] ->
      let signature = { W.params = param_types; result = [] } in
      ctx.new_fields <- make_helper ~signature ~extra_body:[] :: ctx.new_fields;
      [ W.CallInstr (helper_name, args) ]
  | [ (v, vt) ] ->
      let signature = { W.params = param_types; result = [ vt ] } in
      ctx.new_fields <-
        make_helper ~signature ~extra_body:[ Push (LocalGet v) ] :: ctx.new_fields;
      [ W.LocalSet (v, Call (helper_name, args)) ]
  | _ ->
      let ret_type_name = Code.Var.fresh_n "loop_ret" in
      let fields =
        List.map ~f:(fun (_, t) -> { W.mut = false; typ = W.Value t }) modified_with_types
      in
      ctx.new_fields <-
        W.Type
          [ { name = ret_type_name; typ = Struct fields; supertype = None; final = true }
          ]
        :: ctx.new_fields;
      let ret_ref_type = W.Ref { nullable = false; typ = Type ret_type_name } in
      let signature = { W.params = param_types; result = [ ret_ref_type ] } in
      let struct_new =
        W.StructNew
          (ret_type_name, List.map ~f:(fun (v, _) -> W.LocalGet v) modified_with_types)
      in
      ctx.new_fields <-
        make_helper ~signature ~extra_body:[ Push struct_new ] :: ctx.new_fields;
      let tmp = Code.Var.fresh_n "loop_ret" in
      ctx.extra_locals <- (tmp, ret_ref_type) :: ctx.extra_locals;
      W.LocalSet (tmp, Call (helper_name, args))
      :: List.mapi
           ~f:(fun i (v, _) ->
             W.LocalSet (v, StructGet (None, ret_type_name, i, LocalGet tmp)))
           modified_with_types

let fork_il_ctx = Initialize_locals.fork_context

let rec transform_instrs ctx il_ctx pending_loops instrs =
  List.concat_map ~f:(transform_instr ctx il_ctx pending_loops) instrs

and transform_instr ctx il_ctx pending_loops (i : W.instruction) =
  match i with
  | Loop (ty, body) -> (
      match !pending_loops with
      | Some after_reads :: tl ->
          pending_loops := tl;
          let result =
            extract_loop
              ctx
              ~is_initialized:(Initialize_locals.is_initialized il_ctx)
              ~after_reads
              body
          in
          Initialize_locals.scan_instruction il_ctx i;
          result
      | None :: tl ->
          pending_loops := tl;
          let inner = fork_il_ctx il_ctx in
          let body' = transform_instrs ctx inner pending_loops body in
          Initialize_locals.scan_instruction il_ctx i;
          [ W.Loop (ty, body') ]
      | [] -> assert false)
  | Block (ty, body) ->
      let inner = fork_il_ctx il_ctx in
      let body' = transform_instrs ctx inner pending_loops body in
      Initialize_locals.scan_instruction il_ctx i;
      [ W.Block (ty, body') ]
  | If (ty, e, l1, l2) ->
      let inner1 = fork_il_ctx il_ctx in
      let inner2 = fork_il_ctx il_ctx in
      let l1' = transform_instrs ctx inner1 pending_loops l1 in
      let l2' = transform_instrs ctx inner2 pending_loops l2 in
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
          let ctx = { var_types; new_fields = []; extra_locals = [] } in
          let il_ctx = Initialize_locals.create_context () in
          List.iter ~f:(Initialize_locals.mark_initialized il_ctx) func.param_names;
          List.iter
            ~f:(fun (var, typ) ->
              match (typ : W.value_type) with
              | I32 | I64 | F32 | F64 | Ref { nullable = true; _ } ->
                  Initialize_locals.mark_initialized il_ctx var
              | Ref { nullable = false; _ } -> ())
            func.locals;
          let pending_loops = ref (scan_right_to_left func.body) in
          let body = transform_instrs ctx il_ctx pending_loops func.body in
          let func' =
            W.Function { func with body; locals = func.locals @ ctx.extra_locals }
          in
          List.rev ctx.new_fields @ [ func' ]
      | _ -> [ field ])
    fields
