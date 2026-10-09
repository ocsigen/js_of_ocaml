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

(* Class field initialisers and static blocks run in their own function
   scope: the variables they reference are captured. *)
let mark_class_element_captured pass_stats captured_vars el =
  let t = Timer.make () in
  let visitor =
    object
      inherit Js_traverse.iter

      method! ident i =
        match i with
        | V v -> Var.Tbl.set captured_vars v true
        | _ -> ()
    end
  in
  (match el with
  | CEMethod _ -> ()
  | CEField (_, _, _, init) | CEAccessor (_, _, _, init) -> visitor#initialiser_o init
  | CEStaticBLock b -> visitor#block b);
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

type node_id = Linear_scan.node

(* Context entry for break/continue statement targeting *)
type context_entry =
  { labels : Label.t list (* Labels for this entry; empty for unlabelled loops *)
  ; break : node_id
  ; continue : node_id option
  ; iter_or_switch : bool
        (* Whether unlabelled [break] can target this entry. True for
           iteration statements and [switch]; false for labelled
           non-iteration statements (e.g. labelled blocks), which are
           valid targets only for [break label]. *)
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
  | { iter_or_switch = true; break; _ } :: _, None -> break
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
  let builder = Linear_scan.Builder.create () in
  let reserve_id () = Linear_scan.Builder.reserve builder in
  let set_node id action succs = Linear_scan.Builder.set builder id action succs in
  let add_node action succs = Linear_scan.Builder.add builder action succs in
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
        Linear_scan.Builder.hint builder x y
    | _ -> ()
  in
  (* The CFG is built backwards: we visit statements from the last one to the
     first one.
     - [exit] is the node id of the statement following the current one.
     - [context] contains the targets for break and continue statements.

     Statements that own a break/continue context (iteration statements and
     [switch]) are visited through [visit_owning_stmt], which takes a [labels]
     list to associate with the new context entry. All other statements go
     through [visit_stmt], which has no [labels] parameter — so we cannot
     accidentally drop labels for a statement that needs them.

     Both functions return the node id of the first statement of the visited
     block (which acts as the entry point for that block). *)
  let rec visit_stmt context exit stmt =
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
    | While_statement _
    | Do_while_statement _
    | Switch_statement _
    | ForIn_statement _
    | ForOf_statement _
    | ForAwaitOf_statement _
    | For_statement _ -> visit_owning_stmt [] context exit stmt
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
        let inner_body_entry = visit_stmts context finally_entry body in
        (* Wrap [body_entry] in a fresh [Nop] shim whose only predecessor
           is the [Try] node we are about to create. This guarantees that
           in the DFS used to compute the RPO layout, [body_entry] is
           numbered strictly after [catch_entry]/[finally_entry] (which
           are visited first by [list_rev_iter]).

           Without the shim, [body_entry] can coincide with a node
           reachable from [catch_entry] or [finally_entry] — e.g. when
           the body is empty. That would make the body end up inside
           catch's/finally's DFS subtree, violating
           [body_order < catch_order] in [compute_live_ranges]. *)
        let body_entry = add_node Nop [ inner_body_entry ] in
        (* Map handler entry -> try body entry. This is used during live range
           computation to extend the live range of variables that are live at the
           handler entry to cover the entire try body, since any statement in the
           try block might throw and jump to the handler. *)
        if Option.is_some catch
        then Linear_scan.Builder.try_block builder ~catch_entry ~body_entry;
        if Option.is_some finally
        then Linear_scan.Builder.try_block builder ~catch_entry:finally_entry ~body_entry;
        add_node Nop [ body_entry; catch_entry; finally_entry ]
    | Labelled_statement (label, (stmt, _)) ->
        (* Collect all labels for chained labelled statements *)
        let rec collect_labels acc s =
          match s with
          | Labelled_statement (l, (inner, _)) -> collect_labels (l :: acc) inner
          | _ -> List.rev acc, s
        in
        let all_labels, inner_stmt = collect_labels [ label ] stmt in
        if has_own_context inner_stmt
        then visit_owning_stmt all_labels context exit inner_stmt
        else
          let context =
            { labels = all_labels; break = exit; continue = None; iter_or_switch = false }
            :: context
          in
          visit_stmt context exit inner_stmt
    | Empty_statement | Debugger_statement -> exit
    | Function_declaration (_, _) -> exit
    | Class_declaration (_, cl) ->
        (* Heritage, computed keys and decorators are evaluated here. Other
           reads are captured. *)
        add_node (Use (expr_use (EClass (None, cl)))) [ exit ]
    | With_statement (e, (body, _)) ->
        let body_entry = visit_stmt context exit body in
        let u, _ = expr_use_def e in
        add_node (Use u) [ body_entry ]
    | Import (_, _) | Export (_, _) -> exit
  (* Visits a statement that owns its own break/continue context: iteration
     statements ([while]/[do-while]/[for]/[for-in]/[for-of]/[for-await-of])
     and [switch]. [labels] are the labels (possibly empty) under which this
     statement appears, and become part of the new context entry. *)
  and visit_owning_stmt labels context exit stmt =
    match stmt with
    | While_statement (cond, (body, _)) ->
        let loop_check = reserve_id () in
        let context =
          { labels; break = exit; continue = Some loop_check; iter_or_switch = true }
          :: context
        in
        let body_entry = visit_stmt context loop_check body in
        let u_cond, _ = expr_use_def cond in
        set_node loop_check (Use u_cond) [ body_entry; exit ];
        loop_check
    | Do_while_statement ((body, _), cond) ->
        let loop_check = reserve_id () in
        let u_cond, _ = expr_use_def cond in
        let context' =
          { labels; break = exit; continue = Some loop_check; iter_or_switch = true }
          :: context
        in
        let body_entry = visit_stmt context' loop_check body in
        set_node loop_check (Use u_cond) [ body_entry; exit ];
        body_entry
    | Switch_statement (cond, pre_cases, def_opt, post_cases) ->
        let context =
          { labels; break = exit; continue = None; iter_or_switch = true } :: context
        in
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
    | ForIn_statement (left, right, (body, _))
    | ForOf_statement (left, right, (body, _))
    | ForAwaitOf_statement (left, right, (body, _)) ->
        let loop_check = reserve_id () in
        let context =
          { labels; break = exit; continue = Some loop_check; iter_or_switch = true }
          :: context
        in
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
        let context =
          { labels; break = exit; continue = Some update_node; iter_or_switch = true }
          :: context
        in
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
    | Block _
    | Expression_statement _
    | If_statement _
    | Break_statement _
    | Continue_statement _
    | Variable_statement _
    | Return_statement _
    | Throw_statement _
    | Try_statement _
    | Labelled_statement _
    | Empty_statement
    | Debugger_statement
    | Function_declaration _
    | Class_declaration _
    | With_statement _
    | Import _
    | Export _ -> assert false
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
  Linear_scan.Builder.finish builder ~entry

(* Fixpoint Computation *)
let compute_liveness pass_stats stmts candidates param_vars =
  let t_cfg = Timer.make () in
  let g = build_cfg stmts candidates param_vars in
  pass_stats.time_cfg <- pass_stats.time_cfg +. Timer.get t_cfg;
  let t_solve = Timer.make () in
  let live_in_map = Linear_scan.liveness g in
  pass_stats.time_solve <- pass_stats.time_solve +. Timer.get t_solve;
  g, live_in_map

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
    let t = Timer.make () in
    let intervals = Linear_scan.live_ranges g live_in ~candidates ~param_vars in
    pass_stats.time_live_range <- pass_stats.time_live_range +. Timer.get t;
    if debug ()
    then
      Var.Hashtbl.iter
        (fun _ r ->
          Format.eprintf
            "@[<2>%a:@ %a@]@."
            Code.Var.print
            r.Linear_scan.Live_range.id
            Linear_scan.Live_range.print
            r)
        intervals;
    if debug ()
    then
      Format.eprintf
        "  hints: %d, intervals computed for %d vars@."
        (Var.Hashtbl.length g.hints)
        (Var.Hashtbl.length intervals);
    let t = Timer.make () in
    let hint_count, opportunistic_count =
      Linear_scan.allocate g intervals ~assign:(fun x r -> Var.Tbl.set subst x (Some r))
    in
    pass_stats.time_allocate <- pass_stats.time_allocate +. Timer.get t;
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

      method! class_element el =
        super#class_element el;
        mark_class_element_captured pass_stats captured_vars el
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
