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

   The algorithm mirrors [Js_variable_coalescing]:
     1. Build a CFG of the function body.
     2. Compute backward liveness to a fixed point via [Dgraph.Solver].
     3. Derive per-variable live ranges (intervals with possible holes).
     4. Run linear-scan allocation, bucketed by Wasm [value_type], with
        copy hints from [local.set x (local.get y)] patterns.
     5. Rewrite [LocalGet]/[LocalSet]/[LocalTee] through the substitution
        and drop [LocalSet(x, LocalGet x)] copies introduced by merging.
     6. Rebuild the [locals] list with only representatives.

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

let debug = Debug.find "wasm-var-coalescing"

let stats = Debug.find "stats"

(* --------------------------------------------------------------------- *)
(*  CFG construction                                                     *)
(* --------------------------------------------------------------------- *)

module Node = struct
  type t = int
end

module NodeSet = struct
  type t = BitSet.t

  type elt = int

  let iter f t = BitSet.iter ~f t

  let mem = BitSet.mem

  let add = BitSet.set

  let remove = BitSet.unset

  let copy = BitSet.copy
end

module NodeTbl = struct
  type 'a t = 'a array

  type key = int

  type size = int

  let get t k = t.(k)

  let set t k v = t.(k) <- v

  let make = Array.make
end

module G = Dgraph.Make_Imperative (Node) (NodeSet) (NodeTbl)

module Domain = struct
  type t = Var.Set.t

  let equal = Var.Set.equal

  let bot = Var.Set.empty
end

module Solver = G.Solver (Domain)

type action =
  | Use of Var.Set.t
  | Def of Var.Set.t
  | Nop

let defs_of_action = function
  | Def d -> d
  | Use _ | Nop -> Var.Set.empty

let uses_of_action = function
  | Use u -> u
  | Def _ | Nop -> Var.Set.empty

type node_id = int

type stmt_graph =
  { entry : node_id
  ; size : int
  ; actions : action array
  ; succs : node_id list array
  ; coalescing_hints : Var.t Var.Hashtbl.t
  ; try_blocks : node_id Int.Hashtbl.t
        (** [catch_entry -> try_body_entry]. When the live-range pass reaches
            [catch_entry], every variable currently live is extended back to
            the start of the try body, modeling the fact that an exception
            can fire anywhere inside the body. *)
  }

type graph_builder =
  { mutable nodes : (node_id * action * node_id list) list
  ; hints : Var.t Var.Hashtbl.t
  ; tries : node_id Int.Hashtbl.t
  }

(* Block stack frame: the target node id reached by [br n] with n equal
   to this frame's depth. For a [Block], that target is the node
   following the block (the block's exit). For a [Loop], that target is
   the loop entry (back-edge). *)

let nth_opt l n =
  let rec loop l n =
    match l, n with
    | [], _ -> None
    | x :: _, 0 -> Some x
    | _ :: r, n -> loop r (n - 1)
  in
  loop l n

(* The CFG is built with each sub-expression producing its own node
   chain, laid out in Wasm evaluation order (left-to-right for
   operands, args-before-funcref for [call_ref]). Each [local.get],
   [local.tee], and [local.set] becomes a single node with a [Use] or
   [Def] action; structural operators contribute no node by themselves.

   [build_expr block_stack exit_node e] appends the CFG for [e] to the
   graph; control flows in forward execution order from the returned
   entry node through to [exit_node]. *)

let build_cfg ~candidates ~param_vars instrs =
  let builder =
    { nodes = []; hints = Var.Hashtbl.create 16; tries = Int.Hashtbl.create 8 }
  in
  let next_id = ref 0 in
  let reserve_id () =
    let id = !next_id in
    incr next_id;
    id
  in
  let set_node id action succs = builder.nodes <- (id, action, succs) :: builder.nodes in
  let add_node action succs =
    let id = reserve_id () in
    set_node id action succs;
    id
  in
  let add_hint x y =
    if Var.Set.mem x candidates && Var.Set.mem y candidates
    then Var.Hashtbl.replace builder.hints x y
  in
  let target_of_br block_stack n =
    match nth_opt block_stack n with
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
    then add_node (Def (Var.Set.singleton x)) [ exit_node ]
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
        let then_entry = build_expr block_stack' exit_node e1 in
        let else_entry = build_expr block_stack' exit_node e2 in
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
        let inner_body_entry = build_instrs block_stack' exit_node body in
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
        Int.Hashtbl.replace builder.tries catch_entry body_entry;
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
        let then_entry = build_instrs block_stack' exit_node l1 in
        let else_entry = build_instrs block_stack' exit_node l2 in
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
        loop_entry
  in
  let end_node = add_node Nop [] in
  let body_entry = build_instrs [] end_node instrs in
  let entry =
    if Var.Set.is_empty param_vars
    then body_entry
    else add_node (Def param_vars) [ body_entry ]
  in
  let size = !next_id in
  let actions = Array.make size Nop in
  let succs = Array.make size [] in
  List.iter builder.nodes ~f:(fun (id, action, s) ->
      actions.(id) <- action;
      succs.(id) <- s);
  { entry
  ; size
  ; actions
  ; succs
  ; coalescing_hints = builder.hints
  ; try_blocks = builder.tries
  }

(* --------------------------------------------------------------------- *)
(*  Liveness (backward dataflow) and live range extraction                *)
(* --------------------------------------------------------------------- *)

let compute_liveness g =
  let domain_set = BitSet.create' g.size in
  for i = 0 to g.size - 1 do
    BitSet.set domain_set i
  done;
  let preds = Array.make (Array.length g.succs) [] in
  Array.iteri g.succs ~f:(fun i l ->
      List.iter l ~f:(fun j -> preds.(j) <- i :: preds.(j)));
  let inv_graph =
    { G.domain = domain_set; iter_children = (fun f i -> List.iter ~f preds.(i)) }
  in
  let transfer state node_id =
    let live_out =
      List.fold_left g.succs.(node_id) ~init:Var.Set.empty ~f:(fun acc id ->
          Var.Set.union acc state.(id))
    in
    let def = defs_of_action g.actions.(node_id) in
    let use = uses_of_action g.actions.(node_id) in
    Var.Set.union use (Var.Set.diff live_out def)
  in
  Solver.f g.size inv_graph transfer

module Live_range = struct
  type interval =
    { start_pos : int
    ; end_pos : int
    }

  type t =
    { id : Var.t
    ; mutable ranges : interval list (* sorted by start_pos *)
    ; mutable assigned : bool
    }

  let create v = { id = v; ranges = []; assigned = false }

  let add_range t start_pos end_pos =
    let rec loop s e acc = function
      | [] -> List.rev ({ start_pos = s; end_pos = e } :: acc)
      | r :: rest ->
          if e < r.start_pos - 1
          then List.rev_append acc ({ start_pos = s; end_pos = e } :: r :: rest)
          else if s > r.end_pos + 1
          then loop s e (r :: acc) rest
          else
            let new_start = min s r.start_pos in
            let new_end = max e r.end_pos in
            loop new_start new_end acc rest
    in
    t.ranges <-
      (match t.ranges with
      | [] -> [ { start_pos; end_pos } ]
      | r :: _ when r.start_pos > end_pos + 1 -> { start_pos; end_pos } :: t.ranges
      | _ -> loop start_pos end_pos [] t.ranges)

  let add_ranges t other_ranges =
    match t.ranges, other_ranges with
    | [], l | l, [] -> t.ranges <- l
    | l1, l2 ->
        let rec loop acc l1 l2 =
          match l1, l2 with
          | [], l | l, [] -> List.rev_append acc l
          | h1 :: t1, h2 :: t2 ->
              if h1.start_pos < h2.start_pos then step acc h1 t1 l2 else step acc h2 t2 l1
        and step acc current rest other =
          match acc with
          | prev :: acc_rest when prev.end_pos + 1 >= current.start_pos ->
              let merged = { prev with end_pos = max prev.end_pos current.end_pos } in
              loop (merged :: acc_rest) rest other
          | _ -> loop (current :: acc) rest other
        in
        t.ranges <- loop [] l1 l2

  let get_start_pos t =
    match t.ranges with
    | [] -> max_int
    | r :: _ -> r.start_pos

  let get_first_hole t =
    match t.ranges with
    | [] -> 0
    | r :: _ -> r.end_pos + 1

  let rec advance t position =
    match t.ranges with
    | [] -> `Dead
    | r :: rem ->
        if r.end_pos < position
        then (
          t.ranges <- rem;
          advance t position)
        else if r.start_pos > position
        then `Inactive
        else `Active

  let intersects t1 t2 =
    let rec loop l1 l2 =
      match l1, l2 with
      | [], _ | _, [] -> false
      | r1 :: rest1, r2 :: rest2 ->
          if r1.end_pos < r2.start_pos
          then loop rest1 l2
          else if r2.end_pos < r1.start_pos
          then loop l1 rest2
          else true
    in
    loop t1.ranges t2.ranges
end

(* Compute live ranges. Uses the same 2x position granularity as
   [Js_variable_coalescing]: each CFG node at linear position i maps to
   positions 2*i (pre, for uses) and 2*i+1 (post, for defs). *)
let compute_live_ranges g live_in_map candidates param_vars =
  let visited = Array.make g.size false in
  let layout = Array.make g.size 0 in
  let i = ref g.size in
  let rec list_rev_iter ~f l =
    match l with
    | [] -> ()
    | x :: r ->
        list_rev_iter ~f r;
        f x
  in
  let rec dfs n =
    if not visited.(n)
    then (
      visited.(n) <- true;
      list_rev_iter g.succs.(n) ~f:dfs;
      decr i;
      layout.(!i) <- n)
  in
  dfs g.entry;
  let num_reachable = g.size - !i in
  let layout = if !i = 0 then layout else Array.sub layout ~pos:!i ~len:num_reachable in
  let node_order = Array.make g.size (-1) in
  Array.iteri layout ~f:(fun i n -> node_order.(n) <- i);
  let ranges = Var.Hashtbl.create (Var.Set.cardinal candidates) in
  Var.Set.iter (fun v -> Var.Hashtbl.add ranges v (Live_range.create v)) candidates;
  let active_ranges = Var.Hashtbl.create 64 in
  let commit_range v start_pos end_pos =
    let r = Var.Hashtbl.find ranges v in
    Live_range.add_range r start_pos end_pos
  in
  for order = num_reachable - 1 downto 0 do
    let node_id = layout.(order) in
    let start_idx = 2 * order in
    let end_idx = (2 * order) + 1 in
    let succs = g.succs.(node_id) in
    let is_fallthrough =
      match succs with
      | [ s ] -> order < num_reachable - 1 && layout.(order + 1) = s
      | _ -> false
    in
    if not is_fallthrough
    then (
      let live_out =
        List.fold_left succs ~init:Var.Set.empty ~f:(fun acc sid ->
            Var.Set.union acc live_in_map.(sid))
      in
      let to_remove = ref [] in
      Var.Hashtbl.iter
        (fun v high ->
          if not (Var.Set.mem v live_out)
          then (
            commit_range v (end_idx + 1) high;
            to_remove := v :: !to_remove))
        active_ranges;
      List.iter !to_remove ~f:(Var.Hashtbl.remove active_ranges);
      Var.Set.iter
        (fun v ->
          if not (Var.Hashtbl.mem active_ranges v)
          then Var.Hashtbl.add active_ranges v end_idx)
        live_out);
    let defs = defs_of_action g.actions.(node_id) in
    Var.Set.iter
      (fun v ->
        match Var.Hashtbl.find_opt active_ranges v with
        | Some high ->
            commit_range v end_idx high;
            Var.Hashtbl.remove active_ranges v
        | None -> commit_range v end_idx end_idx)
      defs;
    let uses = uses_of_action g.actions.(node_id) in
    Var.Set.iter
      (fun v ->
        if not (Var.Hashtbl.mem active_ranges v)
        then Var.Hashtbl.add active_ranges v start_idx)
      uses;
    (* Try-catch range extension: reaching a [catch_entry] in RPO means we
       have processed the catch targets. Whatever is live here must be
       considered live throughout the try body, because an exception can
       fire anywhere in the body and jump to a catch target. Extend every
       currently-active range back to the start of the body, then clear
       the active set so the body's own liveness is computed without
       double-counting the exception path.

       [body_entry] is a dummy [Nop] shim whose only predecessor is the
       Try wrapper, so its RPO order is guaranteed to be strictly less
       than [catch_entry]'s. *)
    match Int.Hashtbl.find_opt g.try_blocks node_id with
    | None -> ()
    | Some body_entry_id ->
        let body_order = node_order.(body_entry_id) in
        assert (body_order < order);
        let body_start_idx = 2 * body_order in
        Var.Hashtbl.iter (fun v high -> commit_range v body_start_idx high) active_ranges;
        Var.Hashtbl.clear active_ranges
  done;
  Var.Hashtbl.iter (fun v high -> commit_range v 0 high) active_ranges;
  Var.Set.iter (fun v -> commit_range v 0 0) param_vars;
  ranges

(* --------------------------------------------------------------------- *)
(*  Type compatibility                                                   *)
(* --------------------------------------------------------------------- *)

let heap_type_equal (a : W.heap_type) (b : W.heap_type) =
  match a, b with
  | W.Func, W.Func
  | Extern, Extern
  | Any, Any
  | Eq, Eq
  | Struct, Struct
  | Array, Array
  | I31, I31
  | None_, None_ -> true
  | Type x, Type y -> Var.equal x y
  | ( (Func | Extern | Any | Eq | Struct | Array | I31 | None_ | Type _)
    , (Func | Extern | Any | Eq | Struct | Array | I31 | None_ | Type _) ) -> false

let ref_type_equal (a : W.ref_type) (b : W.ref_type) =
  Bool.equal a.nullable b.nullable && heap_type_equal a.typ b.typ

let value_type_equal (a : W.value_type) (b : W.value_type) =
  match a, b with
  | W.I32, W.I32 | I64, I64 | F32, F32 | F64, F64 -> true
  | Ref ra, Ref rb -> ref_type_equal ra rb
  | (I32 | I64 | F32 | F64 | Ref _), (I32 | I64 | F32 | F64 | Ref _) -> false

(* --------------------------------------------------------------------- *)
(*  Linear scan register allocation                                      *)
(* --------------------------------------------------------------------- *)

module Active_pqueue = Pqueue.Make (struct
  type t = Live_range.t

  let compare r r' = compare (Live_range.get_first_hole r) (Live_range.get_first_hole r')
end)

module Inactive_pqueue = Pqueue.Make (struct
  type t = int * Var.t

  let compare (p, _) (p', _) = compare (p : int) p'
end)

let allocate_registers subst types ranges hints =
  let hint_count = ref 0 in
  let opportunistic_count = ref 0 in
  let sorted_intervals =
    let intervals = Var.Hashtbl.fold (fun _ r acc -> r :: acc) ranges [] in
    List.sort
      ~cmp:(fun a b ->
        Int.compare (Live_range.get_start_pos a) (Live_range.get_start_pos b))
      intervals
  in
  let active = ref Active_pqueue.empty in
  let inactive_queue = ref Inactive_pqueue.empty in
  let inactive = Var.Hashtbl.create 128 in
  let free_pool = ref [] in
  let rec update_active_queue position =
    match Active_pqueue.find_min !active with
    | exception Not_found -> ()
    | r -> (
        if Live_range.get_first_hole r <= position
        then
          let active' = Active_pqueue.remove_min !active in
          match Live_range.advance r position with
          | `Dead ->
              free_pool := r :: !free_pool;
              active := active';
              update_active_queue position
          | `Inactive ->
              inactive_queue :=
                Inactive_pqueue.add (Live_range.get_start_pos r, r.id) !inactive_queue;
              Var.Hashtbl.replace inactive r.id r;
              active := active';
              update_active_queue position
          | `Active ->
              active := Active_pqueue.add r active';
              update_active_queue position)
  in
  let rec update_inactive_queue position =
    match Inactive_pqueue.find_min !inactive_queue with
    | exception Not_found -> ()
    | p, v -> (
        if p <= position
        then
          let inactive' = Inactive_pqueue.remove_min !inactive_queue in
          match Var.Hashtbl.find_opt inactive v with
          | None ->
              inactive_queue := inactive';
              update_inactive_queue position
          | Some r -> (
              match Live_range.advance r position with
              | `Dead ->
                  free_pool := r :: !free_pool;
                  inactive_queue := inactive';
                  Var.Hashtbl.remove inactive v;
                  update_inactive_queue position
              | `Inactive ->
                  inactive_queue :=
                    Inactive_pqueue.add (Live_range.get_start_pos r, r.id) inactive';
                  update_inactive_queue position
              | `Active ->
                  active := Active_pqueue.add r !active;
                  inactive_queue := inactive';
                  Var.Hashtbl.remove inactive v;
                  update_inactive_queue position))
  in
  let type_of v = Var.Hashtbl.find types v in
  let compatible a b = value_type_equal (type_of a) (type_of b) in
  let get_free_matching wanted =
    let rec loop kept pool =
      match pool with
      | [] ->
          free_pool := List.rev kept;
          None
      | r :: rs ->
          if r.Live_range.assigned
          then loop kept rs
          else if compatible r.Live_range.id wanted
          then (
            free_pool := List.rev_append kept rs;
            Some r)
          else loop (r :: kept) rs
    in
    loop [] !free_pool
  in
  List.iter sorted_intervals ~f:(fun current ->
      let position = Live_range.get_start_pos current in
      update_active_queue position;
      update_inactive_queue position;
      let hint_repr =
        match Var.Hashtbl.find_opt hints current.Live_range.id with
        | None -> None
        | Some src -> (
            match Var.Hashtbl.find_opt subst src with
            | None -> None
            | Some var -> (
                if not (compatible var current.id)
                then None
                else
                  let r = Var.Hashtbl.find ranges var in
                  match Live_range.advance r position with
                  | `Dead ->
                      r.assigned <- true;
                      Some r
                  | `Inactive ->
                      if Live_range.intersects r current
                      then None
                      else (
                        Var.Hashtbl.remove inactive r.id;
                        Some r)
                  | `Active -> None))
      in
      let repr =
        match hint_repr with
        | Some r ->
            incr hint_count;
            r
        | None -> (
            let candidate =
              let rec loop q count =
                if count >= 50 || Inactive_pqueue.is_empty q
                then None
                else
                  let _, v = Inactive_pqueue.find_min q in
                  let q' = Inactive_pqueue.remove_min q in
                  match Var.Hashtbl.find_opt inactive v with
                  | None -> loop q' count
                  | Some iv ->
                      if
                        compatible iv.id current.id
                        && not (Live_range.intersects iv current)
                      then Some iv
                      else loop q' (count + 1)
              in
              loop !inactive_queue 0
            in
            match candidate with
            | Some r ->
                incr opportunistic_count;
                Var.Hashtbl.remove inactive r.id;
                r
            | None -> (
                match get_free_matching current.id with
                | Some r ->
                    incr opportunistic_count;
                    r
                | None -> current))
      in
      if not (Var.equal current.id repr.id)
      then (
        Var.forget_generated_name current.id;
        Var.forget_generated_name repr.id;
        Live_range.add_ranges repr current.Live_range.ranges;
        Var.Hashtbl.replace subst current.id repr.id);
      active := Active_pqueue.add repr !active);
  !hint_count, !opportunistic_count

(* --------------------------------------------------------------------- *)
(*  Body rewriting                                                       *)
(* --------------------------------------------------------------------- *)

let subst_var subst x =
  match Var.Hashtbl.find_opt subst x with
  | None -> x
  | Some y -> y

let rec rewrite_expr subst (e : W.expression) : W.expression =
  match e with
  | Const _ | GlobalGet _ | Pop _ | RefFunc _ | RefNull _ -> e
  | LocalGet x -> LocalGet (subst_var subst x)
  | LocalTee (x, e') -> LocalTee (subst_var subst x, rewrite_expr subst e')
  | UnOp (op, e') -> UnOp (op, rewrite_expr subst e')
  | I32WrapI64 e' -> I32WrapI64 (rewrite_expr subst e')
  | I64ExtendI32 (s, e') -> I64ExtendI32 (s, rewrite_expr subst e')
  | F32DemoteF64 e' -> F32DemoteF64 (rewrite_expr subst e')
  | F64PromoteF32 e' -> F64PromoteF32 (rewrite_expr subst e')
  | RefI31 e' -> RefI31 (rewrite_expr subst e')
  | I31Get (s, e') -> I31Get (s, rewrite_expr subst e')
  | ArrayLen e' -> ArrayLen (rewrite_expr subst e')
  | StructGet (s, ty, i, e') -> StructGet (s, ty, i, rewrite_expr subst e')
  | RefCast (ty, e') -> RefCast (ty, rewrite_expr subst e')
  | RefTest (ty, e') -> RefTest (ty, rewrite_expr subst e')
  | Br_on_cast (i, a, b, e') -> Br_on_cast (i, a, b, rewrite_expr subst e')
  | Br_on_cast_fail (i, a, b, e') -> Br_on_cast_fail (i, a, b, rewrite_expr subst e')
  | Br_on_null (i, e') -> Br_on_null (i, rewrite_expr subst e')
  | ExternConvertAny e' -> ExternConvertAny (rewrite_expr subst e')
  | AnyConvertExtern e' -> AnyConvertExtern (rewrite_expr subst e')
  | BinOp (op, e1, e2) -> BinOp (op, rewrite_expr subst e1, rewrite_expr subst e2)
  | ArrayNew (ty, e1, e2) -> ArrayNew (ty, rewrite_expr subst e1, rewrite_expr subst e2)
  | ArrayNewData (ty, d, e1, e2) ->
      ArrayNewData (ty, d, rewrite_expr subst e1, rewrite_expr subst e2)
  | ArrayGet (s, ty, e1, e2) ->
      ArrayGet (s, ty, rewrite_expr subst e1, rewrite_expr subst e2)
  | RefEq (e1, e2) -> RefEq (rewrite_expr subst e1, rewrite_expr subst e2)
  | Call (f, l) -> Call (f, List.map l ~f:(rewrite_expr subst))
  | ArrayNewFixed (ty, l) -> ArrayNewFixed (ty, List.map l ~f:(rewrite_expr subst))
  | StructNew (ty, l) -> StructNew (ty, List.map l ~f:(rewrite_expr subst))
  | Call_ref (f, g, l) ->
      Call_ref (f, rewrite_expr subst g, List.map l ~f:(rewrite_expr subst))
  | IfExpr (ty, cond, e1, e2) ->
      IfExpr (ty, rewrite_expr subst cond, rewrite_expr subst e1, rewrite_expr subst e2)
  | BlockExpr (ty, l) -> BlockExpr (ty, rewrite_instrs subst l)
  | Seq (l, e') -> Seq (rewrite_instrs subst l, rewrite_expr subst e')
  | Try (ty, body, catches) -> Try (ty, rewrite_instrs subst body, catches)

and rewrite_instr subst (i : W.instruction) : W.instruction option =
  match i with
  | Nop | Event _ | Unreachable | Rethrow _ | Br (_, None) | Return None -> Some i
  | Drop e -> Some (Drop (rewrite_expr subst e))
  | Push e -> Some (Push (rewrite_expr subst e))
  | LocalSet (x, e) -> (
      let x' = subst_var subst x in
      let e' = rewrite_expr subst e in
      match e' with
      | LocalGet y when Var.equal x' y -> None
      | _ -> Some (LocalSet (x', e')))
  | GlobalSet (x, e) -> Some (GlobalSet (x, rewrite_expr subst e))
  | StructSet (ty, i, e1, e2) ->
      Some (StructSet (ty, i, rewrite_expr subst e1, rewrite_expr subst e2))
  | ArraySet (ty, e1, e2, e3) ->
      Some
        (ArraySet (ty, rewrite_expr subst e1, rewrite_expr subst e2, rewrite_expr subst e3))
  | CallInstr (f, l) -> Some (CallInstr (f, List.map l ~f:(rewrite_expr subst)))
  | Return_call (f, l) -> Some (Return_call (f, List.map l ~f:(rewrite_expr subst)))
  | Return_call_ref (f, g, l) ->
      Some (Return_call_ref (f, rewrite_expr subst g, List.map l ~f:(rewrite_expr subst)))
  | Return (Some e) -> Some (Return (Some (rewrite_expr subst e)))
  | Throw (t, e) -> Some (Throw (t, rewrite_expr subst e))
  | Br (i, Some e) -> Some (Br (i, Some (rewrite_expr subst e)))
  | Br_if (i, e) -> Some (Br_if (i, rewrite_expr subst e))
  | Br_table (e, l, d) -> Some (Br_table (rewrite_expr subst e, l, d))
  | Loop (ty, l) -> Some (Loop (ty, rewrite_instrs subst l))
  | Block (ty, l) -> Some (Block (ty, rewrite_instrs subst l))
  | If (ty, cond, l1, l2) ->
      Some
        (If (ty, rewrite_expr subst cond, rewrite_instrs subst l1, rewrite_instrs subst l2))

and rewrite_instrs subst l = List.filter_map l ~f:(rewrite_instr subst)

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
  else
    let g = build_cfg ~candidates ~param_vars instrs in
    let live_in_map = compute_liveness g in
    let ranges = compute_live_ranges g live_in_map candidates param_vars in
    let subst = Var.Hashtbl.create num_candidates in
    let hint_count, opportunistic_count =
      allocate_registers subst types ranges g.coalescing_hints
    in
    if debug ()
    then
      Format.eprintf
        "wasm var-coalescing: %d candidates, %d hint + %d opportunistic@."
        num_candidates
        hint_count
        opportunistic_count;
    (* Only candidates that merged into a different representative are
         recorded in [subst]; its size is therefore the count of changes. *)
    if Var.Hashtbl.length subst = 0
    then (
      if times () then Format.eprintf "  wasm var coalescing: %a@." Timer.print t;
      locals, instrs)
    else
      let instrs = rewrite_instrs subst instrs in
      (* Rebuild locals: keep only those whose representative is themselves,
           i.e. those that weren't merged away. Preserve original ordering to
           keep the Wasm output deterministic. *)
      let kept_locals =
        List.filter locals ~f:(fun (v, _) -> not (Var.Hashtbl.mem subst v))
      in
      if times () then Format.eprintf "  wasm var coalescing: %a@." Timer.print t;
      if stats ()
      then
        Format.eprintf
          "Stats - wasm var coalescing: %d candidates, %d coalesced (%d hint, %d \
           opportunistic)@."
          num_candidates
          (hint_count + opportunistic_count)
          hint_count
          opportunistic_count;
      kept_locals, instrs
