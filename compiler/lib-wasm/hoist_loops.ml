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
   body (no escaping [Br], [Return], or [Rethrow]). Whether it is
   actually hoisted is then decided by a backward liveness analysis with
   both normal and exceptional continuations: if a local written in the
   loop may be observed after an exceptional exit, the loop is left in
   place because the helper call has no write-back path on exceptions.

   Variables used in the loop are split into parameters and locals.
   A variable becomes a parameter when its current value is live at the
   loop head: either the body may read it before rewriting it on some
   reachable path, or the loop may exit to a caller read without first
   rewriting it. The remaining variables become locals of the helper.
   This includes non-nullable refs whose set/get discipline is already
   known to be valid because [Initialize_locals] has run on the original
   function.

   Modified variables that are live after the loop are returned via a
   struct (or directly when there are zero or one), whether they are
   parameters or helper locals. *)

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

let is_extractable_loop_body body = is_contained_instrs ~depth:1 body

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

let empty_var_sets = { reads = Code.Var.Set.empty; writes = Code.Var.Set.empty }

let empty_vars = Code.Var.Set.empty

let label_reads labels depth =
  let rec find labels depth =
    match labels, depth with
    | live :: _, 0 -> live
    | _ :: tl, n -> find tl (n - 1)
    | [], _ -> assert false
  in
  find labels depth

let catches_live_out labels ~exn_live_out catches =
  List.fold_left
    catches
    ~init:exn_live_out
    ~f:(fun acc (_, label, _) -> Code.Var.Set.union acc (label_reads labels label))

let rec live_before_expr ~labels ~live_out ~exn_live_out (e : W.expression) =
  match e with
  | Const _ | GlobalGet _ | Pop _ | RefFunc _ | RefNull _ -> live_out
  | LocalGet v -> Code.Var.Set.add v live_out
  | LocalTee (v, e') ->
      live_before_expr
        ~labels
        ~live_out:(Code.Var.Set.remove v live_out)
        ~exn_live_out
        e'
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
  | AnyConvertExtern e' -> live_before_expr ~labels ~live_out ~exn_live_out e'
  | BinOp (_, e1, e2)
  | ArrayNew (_, e1, e2)
  | ArrayNewData (_, _, e1, e2)
  | ArrayGet (_, _, e1, e2)
  | RefEq (e1, e2) ->
      let live_out = live_before_expr ~labels ~live_out ~exn_live_out e2 in
      live_before_expr ~labels ~live_out ~exn_live_out e1
  | Br_on_cast (n, _, _, e') | Br_on_cast_fail (n, _, _, e') | Br_on_null (n, e') ->
      let live_out = Code.Var.Set.union live_out (label_reads labels n) in
      live_before_expr ~labels ~live_out ~exn_live_out e'
  | Call (_, l) ->
      let live_out = Code.Var.Set.union live_out exn_live_out in
      live_before_exprs ~labels ~live_out ~exn_live_out l
  | ArrayNewFixed (_, l) | StructNew (_, l) ->
      live_before_exprs ~labels ~live_out ~exn_live_out l
  | Call_ref (_, e', l) ->
      let live_out = Code.Var.Set.union live_out exn_live_out in
      live_before_exprs ~labels ~live_out ~exn_live_out (l @ [ e' ])
  | BlockExpr (_, body) ->
      let _, live_in =
        live_before_instrs ~labels:(live_out :: labels) ~live_out ~exn_live_out body
      in
      live_in
  | Seq (instrs, e') ->
      let live_out = live_before_expr ~labels ~live_out ~exn_live_out e' in
      let _, live_in = live_before_instrs ~labels ~live_out ~exn_live_out instrs in
      live_in
  | IfExpr (_, cond, e1, e2) ->
      let branch_labels = live_out :: labels in
      let live1 = live_before_expr ~labels:branch_labels ~live_out ~exn_live_out e1 in
      let live2 = live_before_expr ~labels:branch_labels ~live_out ~exn_live_out e2 in
      let live_out = Code.Var.Set.union live1 live2 in
      live_before_expr ~labels ~live_out ~exn_live_out cond
  | Try (_, body, catches) ->
      let exn_live_out = catches_live_out labels ~exn_live_out catches in
      let _, live_in =
        live_before_instrs ~labels:(live_out :: labels) ~live_out ~exn_live_out body
      in
      live_in

and live_before_exprs ~labels ~live_out ~exn_live_out l =
  List.fold_right
    l
    ~init:live_out
    ~f:(fun e live_out -> live_before_expr ~labels ~live_out ~exn_live_out e)

and loop_live_in ~labels ~live_out ~exn_live_out body =
  let rec fix live_head =
    let _, live_head' =
      live_before_instrs ~labels:(live_head :: labels) ~live_out ~exn_live_out body
    in
    if Code.Var.Set.equal live_head live_head' then live_head else fix live_head'
  in
  fix empty_vars

and live_before_loop_body ~labels ~live_out ~exn_live_out body =
  let live_head = loop_live_in ~labels ~live_out ~exn_live_out body in
  live_before_instrs ~labels:(live_head :: labels) ~live_out ~exn_live_out body

and live_before_instr ~labels ~rest_loops ~live_out ~exn_live_out (i : W.instruction) =
  match i with
  | Drop e | Push e ->
      rest_loops, live_before_expr ~labels ~live_out ~exn_live_out e
  | GlobalSet (_, e) ->
      rest_loops, live_before_expr ~labels ~live_out ~exn_live_out e
  | Throw (_, e) ->
      rest_loops, live_before_expr ~labels ~live_out:exn_live_out ~exn_live_out e
  | LocalSet (v, e) ->
      let live_out = Code.Var.Set.remove v live_out in
      rest_loops, live_before_expr ~labels ~live_out ~exn_live_out e
  | Br (n, None) -> rest_loops, label_reads labels n
  | Br (n, Some e) ->
      let live_out = label_reads labels n in
      rest_loops, live_before_expr ~labels ~live_out ~exn_live_out e
  | Br_if (n, e) ->
      let live_out = Code.Var.Set.union live_out (label_reads labels n) in
      rest_loops, live_before_expr ~labels ~live_out ~exn_live_out e
  | Br_table (e, targets, default) ->
      let live_out =
        List.fold_left
          ~init:(label_reads labels default)
          ~f:(fun acc n -> Code.Var.Set.union acc (label_reads labels n))
          targets
      in
      rest_loops, live_before_expr ~labels ~live_out ~exn_live_out e
  | Return None -> rest_loops, empty_vars
  | Return (Some e) ->
      rest_loops, live_before_expr ~labels ~live_out:empty_vars ~exn_live_out e
  | Loop (ty, body) ->
      let extractable =
        List.is_empty ty.result
        && is_extractable_loop_body body
        &&
        let { writes; _ } = collect_instrs empty_var_sets body in
        Code.Var.Set.is_empty (Code.Var.Set.inter writes exn_live_out)
      in
      if extractable
      then
        let live_in = loop_live_in ~labels ~live_out ~exn_live_out body in
        Some (live_out, live_in) :: rest_loops, live_in
      else
        let body_loops, live_in =
          live_before_loop_body ~labels ~live_out ~exn_live_out body
        in
        (None :: body_loops) @ rest_loops, live_in
  | Block (_, body) ->
      let body_loops, live_in =
        live_before_instrs ~labels:(live_out :: labels) ~live_out ~exn_live_out body
      in
      body_loops @ rest_loops, live_in
  | If (_, e, l1, l2) ->
      let branch_labels = live_out :: labels in
      let loops1, live1 =
        live_before_instrs ~labels:branch_labels ~live_out ~exn_live_out l1
      in
      let loops2, live2 =
        live_before_instrs ~labels:branch_labels ~live_out ~exn_live_out l2
      in
      let live_out = Code.Var.Set.union live1 live2 in
      let live_in = live_before_expr ~labels ~live_out ~exn_live_out e in
      loops1 @ loops2 @ rest_loops, live_in
  | CallInstr (_, l) ->
      let live_out = Code.Var.Set.union live_out exn_live_out in
      rest_loops, live_before_exprs ~labels ~live_out ~exn_live_out l
  | Nop | Event _ -> rest_loops, live_out
  | ArraySet (_, e1, e2, e3) ->
      let live_out = live_before_expr ~labels ~live_out ~exn_live_out e3 in
      let live_out = live_before_expr ~labels ~live_out ~exn_live_out e2 in
      rest_loops, live_before_expr ~labels ~live_out ~exn_live_out e1
  | StructSet (_, _, e1, e2) ->
      let live_out = live_before_expr ~labels ~live_out ~exn_live_out e2 in
      rest_loops, live_before_expr ~labels ~live_out ~exn_live_out e1
  | Return_call (_, l) ->
      rest_loops, live_before_exprs ~labels ~live_out:empty_vars ~exn_live_out l
  | Return_call_ref (_, e', l) ->
      rest_loops, live_before_exprs ~labels ~live_out:empty_vars ~exn_live_out (l @ [ e' ])
  | Rethrow _ -> rest_loops, exn_live_out
  | Unreachable -> rest_loops, empty_vars

and live_before_instrs ~labels ~live_out ~exn_live_out l =
  List.fold_right
    l
    ~init:([], live_out)
    ~f:(fun i (rest_loops, live_out) ->
      live_before_instr ~labels ~rest_loops ~live_out ~exn_live_out i)

(* Backward dataflow over the function body, producing one entry per
   [Loop] encountered in source order: [Some (live_out, live_in)] for
   loops that will be hoisted — [live_out] is the set of variables read
   after the loop on any normal path through the rest of the function,
   and [live_in] is the fixpoint set of variables whose pre-loop value
   the body may need; [None] for loops that are left in place (either
   not contained, or contained but writing a variable that is read on
   an exceptional exit, which the helper has no way to write back).
   The forward pass in [transform_instrs] consumes the list in the same
   order. *)
let loops_after_reads body =
  let loops, _ =
    live_before_instrs ~labels:[] ~live_out:empty_vars ~exn_live_out:empty_vars body
  in
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

let extract_loop ctx ~is_initialized ~after_reads ~live_in body =
  let { reads; writes } = collect_instrs empty_var_sets body in
  let all_vars = Code.Var.Set.union reads writes in
  let param_vars =
    Code.Var.Set.filter
      (fun v -> is_initialized v && Code.Var.Set.mem v live_in)
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
      | Some (after_reads, live_in) :: tl ->
          pending_loops := tl;
          let result =
            extract_loop
              ctx
              ~is_initialized:(Initialize_locals.is_initialized il_ctx)
              ~after_reads
              ~live_in
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
          let pending_loops = ref (loops_after_reads func.body) in
          let body = transform_instrs ctx il_ctx pending_loops func.body in
          let func' =
            W.Function { func with body; locals = func.locals @ ctx.extra_locals }
          in
          List.rev ctx.new_fields @ [ func' ]
      | _ -> [ field ])
    fields
