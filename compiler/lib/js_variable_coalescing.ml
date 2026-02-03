(* Js_of_ocaml compiler
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
   This pass merges JavaScript variables with disjoint lifespans into
   a single variable. This reduces both code size and runtime memory
   usage. In particular, V8's bytecode allocates one stack slot per
   local variable not captured by nested functions; for large toplevel
   functions, this can create huge stack frames.

   Algorithm Overview
   ------------------

   The algorithm is based on linear scan register allocation, adapted
   for variable coalescing rather than physical register assignment.
   See: "Linear Scan Register Allocation in the Context of SSA Form and
   Register Constraints" by Hanspeter Mössenböck and Michael Pfeiffer
   (https://dl.acm.org/doi/10.1145/543552.512558).

   1. Build a CFG for each function scope (statements become nodes,
      control flow creates edges).

   2. Compute live variable sets via backward dataflow analysis:
        LiveIn(n) = Use(n) ∪ (LiveOut(n) - Def(n))
        LiveOut(n) = ∪ LiveIn(succ) for all successors

   3. Compute live ranges (intervals) for each variable. We use 2x
      position granularity: CFG node i maps to positions 2i (before, for
      uses) and 2i+1 (after, for defs). Variables may have multiple
      disjoint intervals (holes in their liveness).

   4. Sort variables by their first live position.

   5. Process variables in order using linear scan:
      - Maintain active set (currently live variables) and inactive
        set (variables in a hole that will become live again).
      - For each variable, try to coalesce with an existing
        representative:
        a) Hint-based: If this variable is a copy target (x = y), try
           to reuse y's representative. This eliminates the copy when
           successful.
        b) Opportunistic: Reuse any dead or non-interfering inactive
           variable.
      - If no coalescing possible, the variable becomes its own
        representative.

   Scope Handling
   --------------

   Each function scope is processed independently. Variables captured by
   nested functions are currently excluded from coalescing. While they
   could theoretically be merged with preceding variables, they are
   typically allocated on the heap (e.g., in V8's context objects)
   rather than the stack. Thus, they do not contribute to stack frame
   size, but are likely slower to access.

   Block-scoped variables (let, const) are excluded from this analysis.
   Js_of_ocaml rarely uses them, except occasionally for captured
   variables, so there is little benefit in extending the implementation
   to support them. In pretty mode, only compiler-generated variables
   are coalesced to preserve user-defined names.

   We skip trivial scopes with 0-1 candidate variables.

   Copy Hints
   ----------

   When we see `var x = y` where y is a local variable, we record a
   "hint" that x should use y's representative if possible. Indeed, if
   x and y share the same variable, the assignment becomes a no-op
   that can be eliminated by later passes.

   Exception Handling
   ------------------

   Exception handlers (catch/finally) require special care. Since an
   exception can be thrown from any point in the try block, variables
   live at the handler entry must be considered live throughout the
   entire try block. We extend their live ranges accordingly.

   Implementation Details
   ----------------------

   To avoid quadratic behavior in pathological cases, the search for
   non-interfering inactive variables is limited to a constant number
   of candidates.
*)

open Stdlib
open Javascript
module Var = Code.Var

let times = Debug.find "times"

let debug = Debug.find "var-coalescing"

let stats = Debug.find "stats"

type pass_stats =
  { mutable candidates : int
  ; mutable hint_coalesced : int
  ; mutable opportunistic_coalesced : int
  ; mutable time_collect : float
  ; mutable time_cfg : float
  ; mutable time_solve : float
  ; mutable time_live_range : float
  ; mutable time_allocate : float
  ; mutable time_mark_captured : float
  ; mutable time_rename : float
  }

(* Mark variables used in the current function (not visiting nested
   functions). *)
let mark_captured_variables pass_stats captured_vars f =
  let t = Timer.make () in
  let visitor =
    object
      inherit Js_traverse.iter as super

      method! fun_decl _ = ()

      method! ident i =
        match i with
        | V v -> Var.Tbl.set captured_vars v true
        | _ -> ()

      method visit f = super#fun_decl f
    end
  in
  visitor#visit f;
  pass_stats.time_mark_captured <- pass_stats.time_mark_captured +. Timer.get t

(* Collect local variables in the current function *)
let collect_locals captured_vars params stmts =
  let locals = ref Var.Set.empty in
  let add i =
    match i with
    | V v -> if not (Var.Tbl.get captured_vars v) then locals := Var.Set.add v !locals
    | S _ -> ()
  in
  let add_list ids = List.iter ids ~f:add in
  let visitor =
    object
      inherit Js_traverse.iter as super

      method! fun_decl _ = () (* Do not descend into nested functions *)

      method! statement s =
        (match s with
        | Variable_statement (Var, decls) ->
            List.iter decls ~f:(fun d ->
                (* Don't coalesce functions, so that we can use
                   [function x () { ... }] instead of [var x = ...]. *)
                match d with
                | DeclIdent (_, Some (EFun _, _)) -> ()
                | _ -> add_list (bound_idents_of_variable_declaration d))
        | ForIn_statement (left, _, _)
        | ForOf_statement (left, _, _)
        | ForAwaitOf_statement (left, _, _) -> (
            match left with
            | Right (Var, binding) -> add_list (bound_idents_of_binding binding)
            | Left _ (* Expression *) | Right ((Let | Const | Using | AwaitUsing), _) ->
                (* Block-scoped variables (let/const) are not hoisted
                   to function scope, so we don't collect them here
                   for the whole-function liveness analysis that this
                   module performs for 'var' optimization. *)
                ())
        | For_statement (init, _, _, _) -> (
            match init with
            | Right (Var, decls) ->
                List.iter decls ~f:(fun d ->
                    add_list (bound_idents_of_variable_declaration d))
            | Left _ (* Expression or empty *)
            | Right ((Let | Const | Using | AwaitUsing), _) -> ())
        | _ -> ());
        super#statement s
    end
  in
  add_list (bound_idents_of_params params);
  visitor#statements stmts;
  (* In pretty mode, only coalesce compiler-generated variables to preserve
     user-defined variable names for readability. *)
  if Config.Flag.pretty () then Var.Set.filter Var.generated_name !locals else !locals

(* Liveness Analysis *)

(* Unique ID for nodes in the CFG *)

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

(* CFG Construction *)
type action =
  | Use of Var.Set.t
  | Def of Var.Set.t
  | DefUse of Var.Set.t * Var.Set.t
  | Nop

let defs_of_action = function
  | Def d | DefUse (d, _) -> d
  | Use _ | Nop -> Var.Set.empty

let uses_of_action = function
  | Use u | DefUse (_, u) -> u
  | Def _ | Nop -> Var.Set.empty

type node_id = int

type stmt_graph =
  { entry : node_id
  ; size : int
  ; actions : action array
  ; succs : node_id list array
  ; coalescing_hints : Var.t Var.Hashtbl.t
  ; try_blocks : node_id Int.Hashtbl.t (* end_id -> start_id *)
  }

(* Mutable builder used during CFG construction *)
type graph_builder =
  { mutable nodes : (node_id * action * node_id list) list
  ; hints : Var.t Var.Hashtbl.t
  ; tries : node_id Int.Hashtbl.t (* end_id -> start_id *)
  }

(* Context entry for break/continue statement targeting *)
type context_entry =
  { labels : Label.t list (* Labels for this entry; empty for unlabelled loops *)
  ; break : node_id
  ; continue : node_id option
  }

let add_var candidates v s =
  match v with
  | V x -> if Var.Set.mem x candidates then Var.Set.add x s else s
  | S _ -> s

let pattern_defs candidates p =
  List.fold_left
    ~f:(fun s v -> add_var candidates v s)
    ~init:Var.Set.empty
    (bound_idents_of_pattern p)

let rec find_break label ctx =
  match ctx, label with
  | [], _ -> failwith "Break without loop"
  | { labels; break; _ } :: _, Some l when List.mem ~eq:Label.equal l labels -> break
  | { break; _ } :: _, None -> break
  | _ :: rest, _ -> find_break label rest

let rec find_continue label ctx =
  match ctx, label with
  | [], _ -> failwith "Continue without loop"
  | { labels; continue = Some target; _ } :: _, Some l
    when List.mem ~eq:Label.equal l labels -> target
  (* For unlabeled continue, match any actual loop *)
  | { continue = Some target; _ } :: _, None -> target
  | _ :: rest, _ -> find_continue label rest

(* Checks whether a statement handles its own break/continue context.
     For such statements, Labelled_statement delegates label handling. *)
let has_own_context stmt =
  match stmt with
  | While_statement _
  | Do_while_statement _
  | For_statement _
  | ForIn_statement _
  | ForOf_statement _
  | ForAwaitOf_statement _
  | Switch_statement _ -> true
  | Block _
  | Variable_statement _
  | Function_declaration _
  | Class_declaration _
  | Empty_statement
  | Expression_statement _
  | If_statement _
  | Continue_statement _
  | Break_statement _
  | Return_statement _
  | With_statement _
  | Throw_statement _
  | Try_statement _
  | Debugger_statement
  | Import _
  | Export _ -> false
  | Labelled_statement _ -> assert false

(* Visitor to build the graph *)
let build_cfg stmts candidates param_vars =
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
  (* Record copy relationships (x = y) as coalescing hints. When allocating
     registers, we prefer to assign x and y to the same variable if their
     live ranges don't interfere, enabling copy propagation. *)
  (* Reuse the visitor to avoid allocating a new object for every expression *)
  let expr_use =
    let visitor =
      object
        inherit Js_traverse.iter as super

        val mutable use = Var.Set.empty

        method collect e =
          use <- Var.Set.empty;
          super#expression e;
          use

        method! ident i =
          match i with
          | V v when Var.Set.mem v candidates -> use <- Var.Set.add v use
          | _ -> ()
      end
    in
    fun e -> visitor#collect e
  in
  let rec expr_use_def e =
    match e with
    | EBin (Eq, EVar v, e') ->
        let u, d = expr_use_def e' in
        u, add_var candidates v d
    | _ -> expr_use e, Var.Set.empty
  in
  let decl_use_def decl =
    let u, d =
      match decl with
      | DeclIdent (_, Some (e, _)) | DeclPattern (_, (e, _)) -> expr_use_def e
      | DeclIdent (_, None) -> Var.Set.empty, Var.Set.empty
    in
    match decl with
    | DeclIdent (x, Some _) -> u, add_var candidates x d
    | DeclIdent (V _, None) | DeclIdent (S _, _) -> u, d
    | DeclPattern (p, _) -> u, Var.Set.union (pattern_defs candidates p) d
  in
  let add_hint x y =
    match x, y with
    | V x, V y when Var.Set.mem x candidates && Var.Set.mem y candidates ->
        Var.Hashtbl.replace builder.hints x y
    | _ -> ()
  in
  (* The CFG is built backwards: we visit statements from the last one to the
     first one.
     - [exit] is the node id of the statement following the current one.
     - [context] contains the targets for break and continue statements.

     The function returns the node id of the first statement of the visited block
     (which acts as the entry point for that block). *)
  let rec visit_stmt ?(labels = []) context exit stmt =
    match stmt with
    | Block stmts -> visit_stmts context exit stmts
    | Expression_statement e ->
        let u, d = expr_use_def e in
        let entry = add_node (DefUse (d, u)) [ exit ] in
        (match e with
        | EBin (Eq, EVar x, EVar y) -> add_hint x y
        | _ -> ());
        entry
    | If_statement (cond, (then_s, _), else_s) ->
        let else_entry =
          match else_s with
          | Some (s, _) -> visit_stmt context exit s
          | None -> exit
        in
        let then_entry = visit_stmt context exit then_s in
        let u_cond, _ = expr_use_def cond in
        add_node (Use u_cond) [ then_entry; else_entry ]
    | While_statement (cond, (body, _)) ->
        let loop_check = reserve_id () in
        let context = { labels; break = exit; continue = Some loop_check } :: context in
        let body_entry = visit_stmt context loop_check body in
        let u_cond, _ = expr_use_def cond in
        set_node loop_check (Use u_cond) [ body_entry; exit ];
        loop_check
    | Do_while_statement ((body, _), cond) ->
        let loop_check = reserve_id () in
        let u_cond, _ = expr_use_def cond in
        let context' = { labels; break = exit; continue = Some loop_check } :: context in
        let body_entry = visit_stmt context' loop_check body in
        set_node loop_check (Use u_cond) [ body_entry; exit ];
        body_entry
    | Break_statement label -> find_break label context
    | Continue_statement label -> find_continue label context
    | Variable_statement (_kind, decls) ->
        let entry, _ =
          List.fold_right decls ~init:(exit, exit) ~f:(fun decl (next, _) ->
              let u, d = decl_use_def decl in
              let node = add_node (DefUse (d, u)) [ next ] in
              (match decl with
              | DeclIdent (x, Some (EVar y, _)) -> add_hint x y
              | _ -> ());
              node, node)
        in
        entry
    | Return_statement (eopt, _) ->
        let u =
          match eopt with
          | Some e -> fst (expr_use_def e)
          | None -> Var.Set.empty
        in
        add_node (Use u) []
    | Throw_statement e ->
        let u, _ = expr_use_def e in
        add_node (Use u) []
    | Switch_statement (cond, pre_cases, def_opt, post_cases) ->
        let context = { labels; break = exit; continue = None } :: context in
        let process_body (_, stmts) (next_body, entries) =
          let next_body = visit_stmts context next_body stmts in
          next_body, next_body :: entries
        in
        let next_body, post_bodies =
          List.fold_right ~f:process_body post_cases ~init:(exit, [])
        in
        let next_body, default_body =
          match def_opt with
          | Some stmts ->
              let entry = visit_stmts context next_body stmts in
              entry, entry
          | None -> next_body, exit
        in
        let _, pre_bodies =
          List.fold_right ~f:process_body pre_cases ~init:(next_body, [])
        in
        let process_case (e, _) body_entry next_case =
          let u, _ = expr_use_def e in
          add_node (Use u) [ body_entry; next_case ]
        in
        let next_case =
          List.fold_right2 ~f:process_case post_cases post_bodies ~init:default_body
        in
        let first_case =
          List.fold_right2 ~f:process_case pre_cases pre_bodies ~init:next_case
        in
        let u_cond, _ = expr_use_def cond in
        add_node (Use u_cond) [ first_case ]
    | Try_statement (body, catch, finally) ->
        let finally_entry =
          match finally with
          | Some block -> visit_stmts context exit block
          | None -> exit
        in
        let catch_entry =
          match catch with
          | Some (_, block) -> visit_stmts context finally_entry block
          | None -> finally_entry
        in
        let body_entry = visit_stmts context finally_entry body in
        (* Map handler entry -> try body entry. This is used during live range
           computation to extend the live range of variables that are live at the
           handler entry to cover the entire try body, since any statement in the
           try block might throw and jump to the handler. *)
        if Option.is_some catch
        then Int.Hashtbl.replace builder.tries catch_entry body_entry;
        if Option.is_some finally
        then Int.Hashtbl.replace builder.tries finally_entry body_entry;
        add_node Nop [ body_entry; catch_entry; finally_entry ]
    | ForIn_statement (left, right, (body, _))
    | ForOf_statement (left, right, (body, _))
    | ForAwaitOf_statement (left, right, (body, _)) ->
        let loop_check = reserve_id () in
        let context = { labels; break = exit; continue = Some loop_check } :: context in
        let body_entry = visit_stmt context loop_check body in
        let body_start =
          match left with
          | Left (EVar (V v)) ->
              if Var.Set.mem v candidates
              then add_node (Def (Var.Set.singleton v)) [ body_entry ]
              else body_entry
          | Right (Var, BindingIdent (V v)) ->
              add_node (Def (Var.Set.singleton v)) [ body_entry ]
          | Left e ->
              let u, _ = expr_use_def e in
              add_node (Use u) [ body_entry ]
          | Right (Var, BindingPattern p) ->
              add_node (Def (pattern_defs candidates p)) [ body_entry ]
          | Right (Var, BindingIdent (S _)) | Right ((Let | Const | Using | AwaitUsing), _)
            -> body_entry
        in
        set_node loop_check Nop [ body_start; exit ];
        let u_right, _ = expr_use_def right in
        add_node (Use u_right) [ loop_check ]
    | For_statement (init, cond, update, (body, _)) -> (
        let loop_check = reserve_id () in
        let update_node =
          match update with
          | Some u ->
              let use, def = expr_use_def u in
              add_node (DefUse (def, use)) [ loop_check ]
          | None -> loop_check
        in
        let context = { labels; break = exit; continue = Some update_node } :: context in
        let body_entry = visit_stmt context update_node body in
        (* Condition check *)
        (match cond with
        | Some c ->
            let u_cond, _ = expr_use_def c in
            set_node loop_check (Use u_cond) [ body_entry; exit ]
        | None -> set_node loop_check Nop [ body_entry ]);
        (* Handle init *)
        match init with
        | Left (Some e) ->
            let u, d = expr_use_def e in
            add_node (DefUse (d, u)) [ loop_check ]
        | Right (Var, decls) ->
            List.fold_right decls ~init:loop_check ~f:(fun decl next ->
                let u, d = decl_use_def decl in
                add_node (DefUse (d, u)) [ next ])
        | Right ((Let | Const | Using | AwaitUsing), _) | Left None -> loop_check)
    | Labelled_statement (label, (stmt, _)) ->
        (* Collect all labels for chained labelled statements *)
        let rec collect_labels acc s =
          match s with
          | Labelled_statement (l, (inner, _)) -> collect_labels (l :: acc) inner
          | _ -> List.rev acc, s
        in
        let all_labels, inner_stmt = collect_labels [ label ] stmt in
        if has_own_context inner_stmt
        then visit_stmt ~labels:(labels @ all_labels) context exit inner_stmt
        else
          let context =
            { labels = all_labels; break = exit; continue = None } :: context
          in
          visit_stmt context exit inner_stmt
    | Empty_statement | Debugger_statement -> exit
    | Function_declaration (_, _) | Class_declaration (_, _) -> exit
    | With_statement (e, (body, _)) ->
        let body_entry = visit_stmt context exit body in
        let u, _ = expr_use_def e in
        add_node (Use u) [ body_entry ]
    | Import (_, _) | Export (_, _) -> exit
  and visit_stmts context exit stmts =
    List.fold_left (List.rev stmts) ~init:exit ~f:(fun next (s, _) ->
        visit_stmt context next s)
  in
  let end_node = add_node Nop [] in
  let start_node = visit_stmts [] end_node stmts in
  let entry =
    if Var.Set.is_empty param_vars
    then start_node
    else add_node (Def param_vars) [ start_node ]
  in
  (* Convert to arrays *)
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

(* Fixpoint Computation *)
let compute_liveness pass_stats stmts candidates param_vars =
  let t_cfg = Timer.make () in
  let g = build_cfg stmts candidates param_vars in
  pass_stats.time_cfg <- pass_stats.time_cfg +. Timer.get t_cfg;
  let t_solve = Timer.make () in
  let domain_set = BitSet.create' g.size in
  for i = 0 to g.size - 1 do
    BitSet.set domain_set i
  done;
  let preds = Array.make (Array.length g.succs) [] in
  Array.iteri
    ~f:(fun i l -> List.iter ~f:(fun j -> preds.(j) <- i :: preds.(j)) l)
    g.succs;
  let inv_graph =
    { G.domain = domain_set; iter_children = (fun f i -> List.iter ~f preds.(i)) }
  in
  let transfer_func state node_id =
    let live_in_succs =
      List.fold_left g.succs.(node_id) ~init:Var.Set.empty ~f:(fun acc id ->
          let v = state.(id) in
          Var.Set.union acc v)
    in
    let def = defs_of_action g.actions.(node_id) in
    let use = uses_of_action g.actions.(node_id) in
    (* LiveIn = Use U (LiveOut - Def) *)
    Var.Set.union use (Var.Set.diff live_in_succs def)
  in
  let live_in_map = Solver.f g.size inv_graph transfer_func in
  pass_stats.time_solve <- pass_stats.time_solve +. Timer.get t_solve;
  g, live_in_map

(* Live Range representation: sorted list of disjoint (start, end) intervals.
   We use 2x granularity for positions: each CFG node n at linear position i
   maps to positions 2*i (before the node, for uses) and 2*i+1 (after the node,
   for defs). This allows distinguishing between a variable that is live-in
   before a definition versus live-out after it at the same CFG node. *)
module Live_range = struct
  type interval =
    { start_pos : int
    ; end_pos : int
    }

  type t =
    { id : Var.t (* The variable associated with this range *)
    ; mutable ranges : interval list (* sorted by start_pos *)
    ; mutable assigned : bool
    }

  let create v = { id = v; ranges = []; assigned = false }

  let print_ranges f r =
    Format.fprintf
      f
      "@[%a@]"
      (Format.pp_print_list
         ~pp_sep:(fun f () -> Format.fprintf f ",@,")
         (fun f r -> Format.fprintf f "%d-%d" r.start_pos r.end_pos))
      r

  let print f t = print_ranges f t.ranges

  let add_range t start_pos end_pos =
    let rec loop s e acc = function
      | [] -> List.rev ({ start_pos = s; end_pos = e } :: acc)
      | r :: rest ->
          if e < r.start_pos - 1
          then List.rev_append acc ({ start_pos = s; end_pos = e } :: r :: rest)
          else if s > r.end_pos + 1
          then loop s e (r :: acc) rest
          else
            (* Overlap or adjacent, merge *)
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

  (* This function consumes ranges before the current position *)
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

let compute_live_ranges pass_stats g live_in_map candidates param_vars =
  let t = Timer.make () in
  (* Linearize the CFG (Reverse Post Order) *)
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
      let succs = g.succs.(n) in
      (* Natural order (important for exception handlers):
         if->else, try->catch->finally *)
      list_rev_iter succs ~f:dfs;
      decr i;
      layout.(!i) <- n)
  in
  dfs g.entry;
  let num_reachable = g.size - !i in
  let layout =
    (* Some nodes may be unreachable *)
    if !i = 0 then layout else Array.sub layout ~pos:!i ~len:num_reachable
  in

  (* Map Node -> Linear Index *)
  let node_order = Array.make g.size (-1) in
  Array.iteri ~f:(fun i n -> node_order.(n) <- i) layout;

  let ranges = Var.Hashtbl.create (Var.Set.cardinal candidates) in
  Var.Set.iter (fun v -> Var.Hashtbl.add ranges v (Live_range.create v)) candidates;

  (* Active ranges map: Var -> end_pos of current active range *)
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
    (* 2x granularity:
       - 2*order: state before the node (uses)
       - 2*order+1: state after the node (defs) *)
    (* Optimization: if the only successor is the next node in linear order
       (fallthrough), we can skip recomputing live_out since it equals the
       live_in of the next iteration. This avoids closing and reopening ranges
       for the common case of sequential statements. *)
    let is_fallthrough =
      match succs with
      | [ s ] -> order < num_reachable - 1 && layout.(order + 1) = s
      | _ -> false
    in
    if not is_fallthrough
    then (
      (* Calculate live_out from successors *)
      let live_out =
        List.fold_left succs ~init:Var.Set.empty ~f:(fun acc sid ->
            Var.Set.union acc live_in_map.(sid))
      in
      (* Process variables that are currently active but not in live_out. *)
      let to_remove = ref [] in
      Var.Hashtbl.iter
        (fun v high ->
          if not (Var.Set.mem v live_out)
          then (
            (* Range ends after this node *)
            commit_range v (end_idx + 1) high;
            to_remove := v :: !to_remove))
        active_ranges;
      List.iter ~f:(Var.Hashtbl.remove active_ranges) !to_remove;
      (* Process variables in live_out but are not active. *)
      Var.Set.iter
        (fun v ->
          if not (Var.Hashtbl.mem active_ranges v)
          then Var.Hashtbl.add active_ranges v end_idx)
        live_out);
    (* 2. Process Defs *)
    let defs = defs_of_action g.actions.(node_id) in
    Var.Set.iter
      (fun v ->
        match Var.Hashtbl.find_opt active_ranges v with
        | Some high ->
            commit_range v end_idx high;
            Var.Hashtbl.remove active_ranges v
        | None ->
            (* Defined but not live out: dead assignment. *)
            commit_range v end_idx end_idx)
      defs;
    (* 3. Process Uses *)
    let uses = uses_of_action g.actions.(node_id) in
    Var.Set.iter
      (fun v ->
        if not (Var.Hashtbl.mem active_ranges v)
        then
          (* Becomes live at use. *)
          Var.Hashtbl.add active_ranges v start_idx)
      uses;
    (* Try-catch/finally liveness extension: when we reach a catch or finally
       handler entry, any variable that is live at this point must have been
       live throughout the entire try body. This is because any statement in
       the try block might throw an exception and jump directly to the handler,
       so the variable's value at any point in the try body might be observed.
       We extend all active live ranges back to the start of the try body. *)
    match Int.Hashtbl.find_opt g.try_blocks node_id with
    | None -> ()
    | Some start_id ->
        let start_order = node_order.(start_id) in
        assert (start_order < order);
        let start_idx = 2 * start_order in
        Var.Hashtbl.iter (fun v high -> commit_range v start_idx high) active_ranges;
        Var.Hashtbl.clear active_ranges
  done;
  (* Close all remaining active ranges at 0 *)
  Var.Hashtbl.iter (fun v high -> commit_range v 0 high) active_ranges;
  (* Mark parameters as live at very start
     (they must have distinct names). *)
  Var.Set.iter (fun v -> commit_range v 0 0) param_vars;
  pass_stats.time_live_range <- pass_stats.time_live_range +. Timer.get t;
  ranges

module Active_pqueue = Pqueue.Make (struct
  type t = Live_range.t

  let compare r r' = compare (Live_range.get_first_hole r) (Live_range.get_first_hole r')
end)

module Inactive_pqueue = Pqueue.Make (struct
  type t = int * Var.t

  let compare (p, _) (p', _) = compare (p : int) p'
end)

(* Linear scan register allocation. Assigns each variable to a representative
   variable (possibly itself) such that variables with the same representative
   have disjoint live ranges and can share the same JavaScript variable name.

   The algorithm processes variables in order of their start position, maintaining:
   - active: variables currently live (sorted by next hole position)
   - inactive: variables in a "hole" in their live range (temporarily not live)
   - free_pool: variables whose live ranges have ended completely

   For each new variable, we try to find a representative in this order:
   1. Hint-based: if this variable is the target of a copy (x = y), try to reuse
      y's representative to enable copy propagation
   2. Free pool: reuse a completely dead variable
   3. Inactive: reuse a variable currently in a hole if ranges don't interfere
   4. Self: use the variable itself (no coalescing) *)
let allocate_registers pass_stats subst ranges hints =
  let hint_count = ref 0 in
  let opportunistic_count = ref 0 in
  let t = Timer.make () in
  (* Sort by Start Position *)
  let sorted_intervals =
    let intervals = Var.Hashtbl.fold (fun _ r acc -> r :: acc) ranges [] in
    List.sort
      ~cmp:(fun a b ->
        Int.compare (Live_range.get_start_pos a) (Live_range.get_start_pos b))
      intervals
  in
  (* List of variables that overlap with the current location *)
  let active = ref Active_pqueue.empty in
  (* List of variables that have started but are currently in a hole *)
  let inactive_queue = ref Inactive_pqueue.empty in
  let inactive = Var.Hashtbl.create 128 in
  (* List of variables which are no longer live *)
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
              (* Was actually already removed from the queue *)
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
  let rec get_free () =
    match !free_pool with
    | [] -> None
    | r :: rs ->
        free_pool := rs;
        if r.assigned then get_free () else Some r
  in
  List.iter
    ~f:(fun current ->
      let position = Live_range.get_start_pos current in
      (* Update queues *)
      update_active_queue position;
      update_inactive_queue position;
      (* Try hint-based coalescing first. If current variable is the target of
         a copy (current = src), we try to reuse src's representative. This is
         beneficial because if they share the same variable, the copy becomes
         a no-op. We can only do this if the representative is dead, inactive
         with non-interfering ranges, or not yet started. *)
      let hint_repr =
        match Var.Hashtbl.find_opt hints current.Live_range.id with
        | None -> None
        | Some src -> (
            match Var.Tbl.get subst src with
            | None -> None
            | Some var -> (
                let r = Var.Hashtbl.find ranges var in
                match Live_range.advance r position with
                | `Dead ->
                    (* Source is completely dead, safe to reuse *)
                    r.assigned <- true;
                    Some r
                | `Inactive ->
                    (* Source is in a hole; check if remaining ranges interfere *)
                    let conflicts = Live_range.intersects r current in
                    if conflicts
                    then None
                    else (
                      Var.Hashtbl.remove inactive r.id;
                      Some r)
                | `Active ->
                    (* Source is still live, cannot coalesce *)
                    None))
      in
      let repr =
        match hint_repr with
        | Some r ->
            incr hint_count;
            r
        | None -> (
            (* Try to find a valid reuse from inactive.
               We prioritize the inactive list over the free list to implement a
               "Best Fit" strategy. Reusing a variable from the inactive list
               fills a specific "hole" in a lifetime, which is a more constrained
               resource. By using it first, we save the "universally compatible"
               free variables for later intervals that might not fit into
               available holes.

               We iterate using the priority queue [inactive_queue] to check
               variables with the smallest holes first, further optimizing the
               fit. *)
            let candidate =
              let rec loop q count =
                if count >= 50 || Inactive_pqueue.is_empty q
                then None
                else
                  let _, v = Inactive_pqueue.find_min q in
                  let q' = Inactive_pqueue.remove_min q in
                  match Var.Hashtbl.find_opt inactive v with
                  | None -> loop q' count (* Stale, don't count *)
                  | Some iv ->
                      if not (Live_range.intersects iv current)
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
                match get_free () with
                | Some r ->
                    incr opportunistic_count;
                    r
                | None -> current))
      in
      if not (Var.equal current.id repr.id)
      then (
        (* Remove the names since they can be confusing. *)
        Var.forget_generated_name current.id;
        Var.forget_generated_name repr.id;
        (* Merge current into repr *)
        Live_range.add_ranges repr current.Live_range.ranges);
      Var.Tbl.set subst current.id (Some repr.id);
      active := Active_pqueue.add repr !active)
    sorted_intervals;

  pass_stats.time_allocate <- pass_stats.time_allocate +. Timer.get t;

  !hint_count, !opportunistic_count

(* Per-scope optimization *)
let optimize_scope pass_stats captured_vars subst params stmts =
  let t_collect = Timer.make () in
  let candidates = collect_locals captured_vars params stmts in
  pass_stats.time_collect <- pass_stats.time_collect +. Timer.get t_collect;
  let num_candidates = Var.Set.cardinal candidates in
  pass_stats.candidates <- pass_stats.candidates + num_candidates;
  (* Early exit: no benefit from coalescing with 0-1 candidates *)
  if num_candidates <= 1
  then ()
  else
    let param_vars =
      List.fold_left
        ~f:(fun vars v -> add_var candidates v vars)
        ~init:Var.Set.empty
        (bound_idents_of_params params)
    in
    let g, live_in = compute_liveness pass_stats stmts candidates param_vars in
    if debug ()
    then
      Format.eprintf
        "  candidates: %d, params: %d, stmts: %d, nodes: %d@."
        num_candidates
        (Var.Set.cardinal param_vars)
        (List.length stmts)
        g.size;
    let intervals = compute_live_ranges pass_stats g live_in candidates param_vars in
    if debug ()
    then
      Var.Hashtbl.iter
        (fun _ r ->
          Format.eprintf
            "@[<2>%a:@ %a@]@."
            Code.Var.print
            r.Live_range.id
            Live_range.print
            r)
        intervals;
    if debug ()
    then
      Format.eprintf
        "  hints: %d, intervals computed for %d vars@."
        (Var.Hashtbl.length g.coalescing_hints)
        (Var.Hashtbl.length intervals);
    let hint_count, opportunistic_count =
      allocate_registers pass_stats subst intervals g.coalescing_hints
    in
    pass_stats.hint_coalesced <- pass_stats.hint_coalesced + hint_count;
    pass_stats.opportunistic_coalesced <-
      pass_stats.opportunistic_coalesced + opportunistic_count;
    if debug ()
    then
      Format.eprintf
        "Scope liveness: %d hint + %d opportunistic = %d coalesced@."
        hint_count
        opportunistic_count
        (hint_count + opportunistic_count)

let rename subst program =
  let rename =
    object
      inherit Js_traverse.map

      method! ident i =
        match i with
        | S _ -> i
        | V v -> (
            match Var.Tbl.get subst v with
            | None -> i
            | Some v' -> V v')
    end
  in
  rename#program program

let f program =
  let t = Timer.make () in
  let pass_stats =
    { candidates = 0
    ; hint_coalesced = 0
    ; opportunistic_coalesced = 0
    ; time_collect = 0.
    ; time_cfg = 0.
    ; time_solve = 0.
    ; time_live_range = 0.
    ; time_allocate = 0.
    ; time_mark_captured = 0.
    ; time_rename = 0.
    }
  in
  let captured_vars = Var.Tbl.make () false in
  let subst = Var.Tbl.make () None in
  let visitor =
    object
      inherit Js_traverse.iter as super

      method! fun_decl f =
        (* Optimize inner functions first *)
        super#fun_decl f;
        let _, params, body, _ = f in
        optimize_scope pass_stats captured_vars subst params body;
        mark_captured_variables pass_stats captured_vars f
    end
  in
  visitor#program program;
  (* Also optimize top-level statements *)
  let empty_params = { list = []; rest = None } in
  optimize_scope pass_stats captured_vars subst empty_params program;
  let t_rename = Timer.make () in
  let program = rename subst program in
  pass_stats.time_rename <- Timer.get t_rename;
  if times ()
  then
    Format.eprintf
      "    liveness analysis: %a (collect: %.2f, cfg: %.2f, solve: %.2f, live_range: \
       %.2f, allocate: %.2f, mark_captured: %.2f, rename: %.2f)@."
      Timer.print
      t
      pass_stats.time_collect
      pass_stats.time_cfg
      pass_stats.time_solve
      pass_stats.time_live_range
      pass_stats.time_allocate
      pass_stats.time_mark_captured
      pass_stats.time_rename;
  if stats ()
  then
    Format.eprintf
      "Stats - variable coalescing: %d candidates, %d coalesced (%d hint, %d \
       opportunistic)@."
      pass_stats.candidates
      (pass_stats.hint_coalesced + pass_stats.opportunistic_coalesced)
      pass_stats.hint_coalesced
      pass_stats.opportunistic_coalesced;
  program
