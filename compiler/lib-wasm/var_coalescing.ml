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
   Linear-scan variable coalescing for the Wasm backend.

   As [Js_variable_coalescing], this relies on [Linear_scan]:
     1. Build a CFG of the function body.
     2. Compute backward liveness to a fixed point.
     3. Derive per-variable live ranges (intervals with possible holes).
     4. Run linear-scan allocation, only merging locals of the same Wasm
        [value_type], with copy hints from [local.set x (local.get y)]
        patterns.
     5. Rewrite [LocalGet]/[LocalSet]/[LocalTee] through the substitution
        and drop [LocalSet(x, LocalGet x)] copies introduced by merging.
        A write whose value is never read (the local is dead after it) is
        removed as well.
     6. Rebuild the [locals] list with only the representatives that are
        still used.

   Parameters participate as candidates — they are all live at entry
   (enforced by a synthetic Def-of-all-params entry node) so two
   parameters never merge, but a non-parameter whose range does not
   interfere with a parameter can be rewritten to reuse the parameter's
   slot.

   [Try] expressions are handled by creating a synthetic [catch_entry]
   node whose successors are the resolved catch targets (branch labels
   in the enclosing block stack). Because an exception can fire from
   anywhere inside the body, every variable live at [catch_entry] is
   considered live across the entire try body — enforced by extending
   active live ranges back to the body entry when [catch_entry] is
   reached during live-range construction.
*)

open! Stdlib
module Var = Code.Var
module W = Wasm_ast

let times = Debug.find "times"

let stats = Debug.find "stats"

(* Aggregated statistics across all calls to [f]. The pass runs once per
   Wasm function; per-function logs are noisy, so we accumulate here and
   emit a single summary via [report_stats]. *)
let total_time = ref 0.

let total_calls = ref 0

let total_candidates = ref 0

let total_hint_count = ref 0

let total_opportunistic_count = ref 0

let report_stats () =
  if !total_calls > 0
  then (
    if times () then Format.eprintf "  wasm var coalescing: %.2f@." !total_time;
    if stats ()
    then
      Format.eprintf
        "Stats - wasm var coalescing: %d functions, %d candidates, %d coalesced (%d \
         hint, %d opportunistic)@."
        !total_calls
        !total_candidates
        (!total_hint_count + !total_opportunistic_count)
        !total_hint_count
        !total_opportunistic_count;
    total_time := 0.;
    total_calls := 0;
    total_candidates := 0;
    total_hint_count := 0;
    total_opportunistic_count := 0)

(* --------------------------------------------------------------------- *)
(*  CFG construction                                                     *)
(* --------------------------------------------------------------------- *)

open Linear_scan

(* Block stack frame: the target node id reached by [br n] with n equal
   to this frame's depth. For a [Block], that target is the node
   following the block (the block's exit). For a [Loop], that target is
   the loop entry (back-edge). *)

(* The CFG is built with each sub-expression producing its own node
   chain, laid out in Wasm evaluation order (left-to-right for
   operands, args-before-funcref for [call_ref]). Each [local.get],
   [local.tee], and [local.set] becomes a single node with a [Use] or
   [Def] action; structural operators contribute no node by themselves.

   [build_expr block_stack exit_node e] appends the CFG for [e] to the
   graph; control flows in forward execution order from the returned
   entry node through to [exit_node]. *)

(* The nodes of the writes to candidate locals are recorded in [defs],
   with whether the write may be removed when dead. The nodes are created
   in the reverse of the evaluation order (this is why the [else] branch
   of a conditional is built before the [then] branch), so that [defs]
   lists the writes in evaluation order. A write inside a [try] body is
   never removed: an exception raised later in the body can reach the
   handler, and this edge is not part of the graph. *)
let build_cfg ~candidates ~param_vars instrs =
  let builder = Builder.create () in
  let defs = ref [] in
  let loops = ref [] in
  let try_depth = ref 0 in
  let reserve_id () = Builder.reserve builder in
  let set_node id action succs = Builder.set builder id action succs in
  let add_node action succs = Builder.add builder action succs in
  let add_hint x y =
    if Var.Set.mem x candidates && Var.Set.mem y candidates then Builder.hint builder x y
  in
  let target_of_br block_stack n =
    match List.nth_opt block_stack n with
    | Some t -> t
    | None ->
        (* [br] out of the function. Conservatively model as a dead end. *)
        add_node Nop []
  in
  let use_node x exit_node =
    if Var.Set.mem x candidates
    then add_node (Use (Var.Set.singleton x)) [ exit_node ]
    else exit_node
  in
  let def_node x exit_node =
    if Var.Set.mem x candidates
    then (
      let node = add_node (Def (Var.Set.singleton x)) [ exit_node ] in
      defs := (node, !try_depth = 0) :: !defs;
      node)
    else exit_node
  in
  let rec build_expr block_stack exit_node (e : W.expression) =
    match e with
    | Const _ | GlobalGet _ | Pop _ | RefFunc _ | RefNull _ -> exit_node
    | LocalGet x -> use_node x exit_node
    | LocalTee (x, e') ->
        let tee = def_node x exit_node in
        build_expr block_stack tee e'
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
    | AnyConvertExtern e' -> build_expr block_stack exit_node e'
    | Br_on_cast (depth, _, _, e')
    | Br_on_cast_fail (depth, _, _, e')
    | Br_on_null (depth, e') ->
        (* Conditional branch: on a successful type test (or null test),
           branch to the label [depth] with the value on the stack;
           otherwise continue with the value on the stack. *)
        let target = target_of_br block_stack depth in
        let branch = add_node Nop [ target; exit_node ] in
        build_expr block_stack branch e'
    | BinOp (_, e1, e2)
    | ArrayNew (_, e1, e2)
    | ArrayNewData (_, _, e1, e2)
    | ArrayGet (_, _, e1, e2)
    | RefEq (e1, e2) ->
        (* [e1] evaluated first, then [e2]. *)
        let after_e1 = build_expr block_stack exit_node e2 in
        build_expr block_stack after_e1 e1
    | Call (_, l) | ArrayNewFixed (_, l) | StructNew (_, l) ->
        build_exprs_left_to_right block_stack exit_node l
    | Call_ref (_, f, l) ->
        (* Wasm [call_ref] evaluates args first, then the funcref. *)
        let after_f = build_expr block_stack exit_node f in
        build_exprs_left_to_right block_stack after_f l
    | IfExpr (_, cond, e1, e2) ->
        (* Evaluate [cond], then branch to [e1] or [e2]. [br 0] inside
           either branch targets the continuation after the IfExpr. *)
        let block_stack' = exit_node :: block_stack in
        let else_entry = build_expr block_stack' exit_node e2 in
        let then_entry = build_expr block_stack' exit_node e1 in
        let branch = add_node Nop [ then_entry; else_entry ] in
        build_expr block_stack branch cond
    | BlockExpr (_, l) ->
        (* [BlockExpr] is a block targetable by [br] at its own depth. *)
        let block_stack' = exit_node :: block_stack in
        build_instrs block_stack' exit_node l
    | Seq (l, e') ->
        let after_l = build_expr block_stack exit_node e' in
        build_instrs block_stack after_l l
    | Try (_, body, catches) ->
        (* [Try] is a block: [br 0] inside the body targets the normal
           continuation after the [Try]. Catch labels refer to the
           enclosing block stack (not counting the [Try] itself), so they
           are resolved against [block_stack]. *)
        let block_stack' = exit_node :: block_stack in
        incr try_depth;
        let inner_body_entry = build_instrs block_stack' exit_node body in
        decr try_depth;
        (* Wrap the body entry in a fresh [Nop] node whose only
           predecessor is the [Try] wrapper we are about to create. This
           ensures that in the DFS used to compute the RPO layout,
           [body_entry] is numbered strictly after [catch_entry], which
           is visited first from the wrapper. Without this shim the body
           entry can be reached via the catch targets (through
           convergent paths in the enclosing code), putting it inside
           [catch_entry]'s DFS subtree and breaking the invariant that
           [body_order < catch_order]. *)
        let body_entry = add_node Nop [ inner_body_entry ] in
        (* Synthetic catch entry whose [live_in] is the union of the
           live_in sets of the catch targets. Making it a successor of
           the Try wrapper ensures it is reachable and thus has its
           liveness computed by the dataflow solver. *)
        let catch_targets =
          List.map catches ~f:(fun (_, depth, _) -> target_of_br block_stack depth)
        in
        let catch_entry = add_node Nop catch_targets in
        Builder.try_block builder ~catch_entry ~body_entry;
        add_node Nop [ body_entry; catch_entry ]
  and build_exprs_left_to_right block_stack exit_node l =
    List.fold_right l ~init:exit_node ~f:(fun e next -> build_expr block_stack next e)
  and build_instrs block_stack exit_node instrs =
    List.fold_right instrs ~init:exit_node ~f:(fun instr next ->
        build_instr block_stack next instr)
  and build_instr block_stack exit_node instr =
    match (instr : W.instruction) with
    | Nop | Event _ -> exit_node
    | Unreachable | Rethrow _ -> add_node Nop []
    | Drop e | Push e -> build_expr block_stack exit_node e
    | LocalSet (x, e) ->
        (match e with
        | LocalGet y -> add_hint x y
        | _ -> ());
        let set = def_node x exit_node in
        build_expr block_stack set e
    | GlobalSet (_, e) -> build_expr block_stack exit_node e
    | StructSet (_, _, e1, e2) ->
        let after_e1 = build_expr block_stack exit_node e2 in
        build_expr block_stack after_e1 e1
    | ArraySet (_, e1, e2, e3) ->
        let after_e2 = build_expr block_stack exit_node e3 in
        let after_e1 = build_expr block_stack after_e2 e2 in
        build_expr block_stack after_e1 e1
    | CallInstr (_, l) -> build_exprs_left_to_right block_stack exit_node l
    | Return None -> add_node Nop []
    | Return (Some e) ->
        let dead = add_node Nop [] in
        build_expr block_stack dead e
    | Throw (_, e) ->
        let dead = add_node Nop [] in
        build_expr block_stack dead e
    | Return_call (_, l) ->
        let dead = add_node Nop [] in
        build_exprs_left_to_right block_stack dead l
    | Return_call_ref (_, f, l) ->
        let dead = add_node Nop [] in
        let after_f = build_expr block_stack dead f in
        build_exprs_left_to_right block_stack after_f l
    | Br (n, None) -> target_of_br block_stack n
    | Br (n, Some e) ->
        let target = target_of_br block_stack n in
        build_expr block_stack target e
    | Br_if (n, cond) ->
        let target = target_of_br block_stack n in
        let branch = add_node Nop [ target; exit_node ] in
        build_expr block_stack branch cond
    | Br_table (e, ns, default) ->
        let all_targets =
          target_of_br block_stack default :: List.map ~f:(target_of_br block_stack) ns
        in
        let branch = add_node Nop all_targets in
        build_expr block_stack branch e
    | If (_, cond, l1, l2) ->
        (* [If] is a Wasm block: [br 0] inside a branch targets the
           continuation after the if. Push [exit_node] onto [block_stack]. *)
        let block_stack' = exit_node :: block_stack in
        let else_entry = build_instrs block_stack' exit_node l2 in
        let then_entry = build_instrs block_stack' exit_node l1 in
        let branch = add_node Nop [ then_entry; else_entry ] in
        build_expr block_stack branch cond
    | Block (_, l) ->
        let block_stack' = exit_node :: block_stack in
        build_instrs block_stack' exit_node l
    | Loop (_, l) ->
        let loop_entry = reserve_id () in
        let block_stack' = loop_entry :: block_stack in
        let body_entry = build_instrs block_stack' exit_node l in
        set_node loop_entry Nop [ body_entry ];
        let written = ref Var.Set.empty in
        Wasm_traverse.iter_locals
          ~read:(fun _ -> ())
          ~write:(fun x ->
            if Var.Set.mem x candidates then written := Var.Set.add x !written)
          l;
        loops := (loop_entry, !written) :: !loops;
        loop_entry
  in
  let end_node = add_node Nop [] in
  let body_entry = build_instrs [] end_node instrs in
  let entry =
    if Var.Set.is_empty param_vars
    then body_entry
    else add_node (Def param_vars) [ body_entry ]
  in
  Builder.finish builder ~entry, Array.of_list !defs, !loops

(* --------------------------------------------------------------------- *)
(*  Body rewriting                                                       *)
(* --------------------------------------------------------------------- *)

let subst_var subst x =
  match Var.Hashtbl.find_opt subst x with
  | None -> x
  | Some y -> y

(* [dead] tells, for each write to a candidate local in evaluation
   order, whether its value is never read. *)
type rewrite =
  { subst : Var.t Var.Hashtbl.t
  ; candidates : Var.Set.t
  ; dead : bool array
  ; mutable next_def : int
  }

let is_dead r x =
  Var.Set.mem x r.candidates
  &&
  let i = r.next_def in
  r.next_def <- i + 1;
  r.dead.(i)

(* Expressions which can be dropped without changing anything *)
let removable (e : W.expression) =
  match e with
  | LocalGet _ | Const _ | GlobalGet _ | RefFunc _ | RefNull _ | RefI31 (Const _) -> true
  | _ -> false

let rec rewrite_expr r (e : W.expression) : W.expression =
  match e with
  | LocalGet x -> LocalGet (subst_var r.subst x)
  | LocalTee (x, e') ->
      let e' = rewrite_expr r e' in
      if is_dead r x then e' else LocalTee (subst_var r.subst x, e')
  | _ ->
      Wasm_traverse.map_expression
        ~expression:(rewrite_expr r)
        ~instructions:(rewrite_instrs r)
        e

(* Also drop the [local.set x (local.get x)] copies introduced by
   merging. *)
and rewrite_instrs r l =
  List.filter_map l ~f:(fun (i : W.instruction) ->
      match i with
      | LocalSet (x, e) -> (
          let e = rewrite_expr r e in
          if is_dead r x
          then if removable e then None else Some (W.Drop e)
          else
            let x = subst_var r.subst x in
            match e with
            | LocalGet y when Var.equal x y -> None
            | e -> Some (W.LocalSet (x, e)))
      | _ ->
          Some
            (Wasm_traverse.map_instruction
               ~expression:(rewrite_expr r)
               ~instructions:(rewrite_instrs r)
               i))

(* --------------------------------------------------------------------- *)
(*  Entry point                                                          *)
(* --------------------------------------------------------------------- *)

let f ~param_names ~param_types ~locals instrs =
  let t = Timer.make () in
  let num_params = List.length param_names in
  assert (List.length param_types = num_params);
  (* In pretty mode, only coalesce compiler-generated variables so
     user-given OCaml names survive in the Wasm name section (and are
     visible in browser devtools). *)
  let eligible v = (not (Config.Flag.pretty ())) || Var.generated_name v in
  let types = Var.Hashtbl.create (num_params + List.length locals) in
  let param_vars =
    List.fold_left2 param_names param_types ~init:Var.Set.empty ~f:(fun acc v t ->
        Var.Hashtbl.replace types v t;
        if eligible v then Var.Set.add v acc else acc)
  in
  let candidates =
    List.fold_left locals ~init:param_vars ~f:(fun acc (v, t) ->
        Var.Hashtbl.replace types v t;
        if eligible v then Var.Set.add v acc else acc)
  in
  let num_candidates = Var.Set.cardinal candidates in
  if num_candidates <= 1
  then locals, instrs
  else (
    incr total_calls;
    total_candidates := !total_candidates + num_candidates;
    let g, defs, loops = build_cfg ~candidates ~param_vars instrs in
    let live_in_map = liveness g in
    (* For each variable, the loops which write it, and the loops in
       which it is live but never written. We do not merge two such
       variables: this would add a write inside the loop to a local
       which has none. V8 creates a loop phi for each local written in
       the loop body, and does not remove right away such a phi whose
       back edge is the phi itself (it takes one optimization phase per
       loop nesting level), so the value would no longer be recognized
       as the same inside the loop. *)
    let written_in = Var.Hashtbl.create 16 in
    let invariant_in = Var.Hashtbl.create 16 in
    let add tbl x i =
      Var.Hashtbl.replace
        tbl
        x
        (IntSet.add i (Option.value ~default:IntSet.empty (Var.Hashtbl.find_opt tbl x)))
    in
    List.iteri loops ~f:(fun i (entry, written) ->
        Var.Set.iter (fun x -> add written_in x i) written;
        Var.Set.iter
          (fun x -> if Var.Set.mem x candidates then add invariant_in x i)
          (Var.Set.diff live_in_map.(entry) written));
    let loop_set tbl x =
      Option.value ~default:IntSet.empty (Var.Hashtbl.find_opt tbl x)
    in
    let loop_compatible r x =
      IntSet.disjoint (loop_set written_in r) (loop_set invariant_in x)
      && IntSet.disjoint (loop_set invariant_in r) (loop_set written_in x)
    in
    let merge_loops r x =
      let union tbl =
        let s = IntSet.union (loop_set tbl r) (loop_set tbl x) in
        if not (IntSet.is_empty s) then Var.Hashtbl.replace tbl r s
      in
      union written_in;
      union invariant_in
    in
    let dead =
      Array.map
        ~f:(fun (node, removable) ->
          removable
          &&
          match g.actions.(node), g.succs.(node) with
          | Def s, [ next ] -> not (Var.Set.mem (Var.Set.choose s) live_in_map.(next))
          | _ -> assert false)
        defs
    in
    let ranges = live_ranges g live_in_map ~candidates ~param_vars in
    let subst = Var.Hashtbl.create num_candidates in
    let type_of v = Var.Hashtbl.find types v in
    let hint_count, opportunistic_count =
      allocate
        ~compatible:(fun a b -> Poly.equal (type_of a) (type_of b) && loop_compatible a b)
        g
        ranges
        ~assign:(fun x r ->
          if not (Var.equal x r)
          then (
            merge_loops r x;
            Var.Hashtbl.replace subst x r))
    in
    total_hint_count := !total_hint_count + hint_count;
    total_opportunistic_count := !total_opportunistic_count + opportunistic_count;
    let res =
      if Var.Hashtbl.length subst = 0 && not (Array.exists ~f:Fun.id dead)
      then locals, instrs
      else
        let r = { subst; candidates; dead; next_def = 0 } in
        let instrs = rewrite_instrs r instrs in
        assert (r.next_def = Array.length dead);
        (* Only keep the representatives which are still accessed *)
        let used = Var.Hashtbl.create 16 in
        Wasm_traverse.iter_locals
          ~read:(fun x -> Var.Hashtbl.replace used x ())
          ~write:(fun x -> Var.Hashtbl.replace used x ())
          instrs;
        ( List.filter locals ~f:(fun (v, _) ->
              (not (Var.Hashtbl.mem subst v)) && Var.Hashtbl.mem used v)
        , instrs )
    in
    total_time := !total_time +. Timer.get t;
    res)
