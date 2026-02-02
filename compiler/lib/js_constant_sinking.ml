(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2026 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
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

(** Constant sinking optimization pass.

    This optimization moves constant variable declarations closer to their
    usage sites, minimizing live ranges. Shorter live ranges reduce register
    pressure and make a subsequent variable coalescing pass more effective,
    since variables with non-overlapping live ranges can share the same name.

    Algorithm overview

    1. Analysis phase
    Walk the AST to identify constant declarations (primitives like
    numbers/strings, and allocations like arrays/objects). Track all usage
    sites for each constant, recording the scope stack at each use.

    Dependencies between constants are tracked (e.g. when `x` is used in `y`'s
    initializer). These dynamic usage sites depend on where `y` is eventually
    placed.

    2. Planning phase
    Process constants in reverse definition order. This ensures that when
    planning `x`, the target location of its consumer `y` is already resolved.

    For each constant, compute the Lowest Common Ancestor (LCA) of all its
    usage sites in the scope tree. The LCA is the deepest scope that
    dominates all uses. Then find the deepest valid position to sink to:
    - Primitives can sink freely to the LCA.
    - Allocations cannot cross loop/function boundaries (to avoid creating
      multiple allocations where originally there was one).
    - Single-use constants are inlined directly at their usage site, ONLY if
      doing so respects the movement constraints (e.g. we cannot inline an
      allocation into a loop).

    3. Transformation phase
    Remove original declarations and re-insert them at their computed target
    positions. Inline single-use constants.
*)

open Stdlib
open Javascript
module Var = Code.Var

let debug = Debug.find "constant-sinking"

let stats = Debug.find "stats"

let times = Debug.find "times"

(* Configuration and Types *)

type scope_kind =
  | Scope_Block
  | Scope_Loop
  | Scope_Function

type scope_id = int

type scope_info =
  { id : scope_id
  ; kind : scope_kind
  ; index : int
        (** Statement index within this scope; incremented
            after each statement *)
  }

type scope_stack = scope_info list

type constant_kind =
  | Primitive
      (** Cheap, idempotent values (numbers, strings, vars). Can be
          duplicated or moved into loops without semantic change. *)
  | Allocation
      (** Expressions that allocate fresh objects (arrays, objects).
          Must not be moved into loops/functions to avoid creating
          multiple distinct allocations. *)

(* Tracks where a constant is used. In_init creates dependencies
   between constants: when constant A's initializer references
   constant B, B's final target scope affects where A can be placed.
   We process constants in definition order (reversed) so that when
   computing A's target, B's target_scope is already resolved. *)
type usage_site =
  | Direct of scope_stack  (** Used directly at this scope position *)
  | In_init of constant_info  (** Used within another constant's initializer *)

and constant_info =
  { var : Var.t
  ; decl_scope : scope_stack
  ; kind : constant_kind
  ; init : expression * location
  ; mutable usages : usage_site list
  ; mutable target_scope : scope_stack option  (** Computed during sinking *)
  }

let print_usage_site f u =
  match u with
  | Direct (scope :: _) -> Format.fprintf f "@@%d:%d" scope.id scope.index
  | Direct [] -> assert false
  | In_init i -> Format.fprintf f "=> %a" Var.print i.var

(* Utils *)

let is_primitive_expr assignments expr =
  match expr with
  | EStr _ | EBool _ | ENum _ -> true
  | EVar (V x) -> Var.Tbl.get assignments x = 1
  | _ -> false

let rec is_allocation_expr assignments expr =
  match expr with
  | EArr lst ->
      List.for_all
        ~f:(fun elt ->
          match elt with
          | ElementHole -> true
          | Element e | ElementSpread e -> is_constant_expr assignments e)
        lst
  | EObj lst ->
      List.for_all
        ~f:(fun prop ->
          match prop with
          | Property (_, e) | PropertySpread e -> is_constant_expr assignments e
          | PropertyMethod _ | CoverInitializedName _ -> false)
        lst
  | ERegexp _ -> true
  | ECall
      ( EVar
          (S
             { name =
                 Utf8
                   ( "caml_string_of_jsbytes"
                   | "caml_list_of_js_array"
                   | "caml_int64_create_lo_mi_hi" )
             ; _
             })
      , _
      , args
      , _ ) ->
      (* These functions are used to define constants. With separate
         compilation, a different naming scheme is used, but we mostly
         care about optimizing whole program compilation. *)
      has_constant_args assignments args
  | _ -> false

and has_constant_args assignments args =
  List.for_all
    ~f:(fun a ->
      match a with
      | Arg e | ArgSpread e -> is_constant_expr assignments e)
    args

and is_constant_expr assignments e =
  is_primitive_expr assignments e || is_allocation_expr assignments e

let get_constant_kind assignments expr =
  if is_primitive_expr assignments expr
  then Some Primitive
  else if is_allocation_expr assignments expr
  then Some Allocation
  else None

(* Common Logic for Traversals *)

class scope_manager =
  object
    val mutable counter = 0

    val mutable stack : scope_stack = []

    method enter_scope kind =
      let id = counter in
      counter <- counter + 1;
      let info = { id; kind; index = 0 } in
      stack <- info :: stack;
      info

    method leave_scope () =
      match stack with
      | _ :: rest -> stack <- rest
      | [] -> assert false

    method next_index () =
      match stack with
      | head :: rest ->
          let head' = { head with index = head.index + 1 } in
          stack <- head' :: rest
      | [] -> ()

    method current_stack = stack
  end

(* Pass 1: Analysis *)

class analysis =
  object (self)
    inherit Js_traverse.iter as super

    inherit scope_manager as scopes

    val mutable constants = Var.Hashtbl.create 128

    val mutable ordered_constants : constant_info list = []
    (* In reversed order *)

    (* State to track if we are analyzing an init expression *)
    val mutable current_init_constant : constant_info option = None

    val assignments = Var.Tbl.make () 0

    method get_constants = constants

    method get_ordered_constants =
      (* Only return constants that were assigned exactly once *)
      List.filter
        ~f:(fun info ->
          Var.Tbl.get assignments info.var = 1
          && Option.is_some (get_constant_kind assignments (fst info.init)))
        ordered_constants

    method private note_assignment ident =
      match ident with
      | V var -> Var.Tbl.set assignments var (Var.Tbl.get assignments var + 1)
      | S _ -> ()

    method private note_assignments idents =
      List.iter ~f:(fun id -> self#note_assignment id) idents

    method private note_assignments_from_target target =
      match target with
      | ArrayTarget elts ->
          List.iter elts ~f:(function
            | TargetElementHole -> ()
            | TargetElementId (id, _) -> self#note_assignment id
            | TargetElement e -> self#note_assignments_from_expr e
            | TargetElementSpread e -> self#note_assignments_from_expr e)
      | ObjectTarget elts ->
          List.iter elts ~f:(function
            | TargetPropertyId (Prop_and_ident id, _) -> self#note_assignment id
            | TargetProperty (_, e, _) -> self#note_assignments_from_expr e
            | TargetPropertySpread e -> self#note_assignments_from_expr e
            | TargetPropertyMethod _ -> ())

    method private note_assignments_from_expr e =
      match e with
      | EVar id -> self#note_assignment id
      | EAssignTarget target -> self#note_assignments_from_target target
      | _ -> ()

    method private register_decl ident ((e, _) as init) =
      match ident with
      | V var when Var.Tbl.get assignments var = 1 -> (
          match get_constant_kind assignments e with
          | Some k ->
              let info =
                { var
                ; decl_scope = scopes#current_stack
                ; kind = k
                ; init
                ; usages = []
                ; target_scope = None
                }
              in
              Var.Hashtbl.replace constants var info;
              ordered_constants <- info :: ordered_constants;
              let prev_ctx = current_init_constant in
              current_init_constant <- Some info;
              self#expression e;
              current_init_constant <- prev_ctx
          | None -> self#expression e)
      | _ -> self#expression e

    method! program p =
      let info = scopes#enter_scope Scope_Block in
      if debug () then Format.eprintf "Analysis: enter program scope %d@." info.id;
      List.iter
        ~f:(fun (stmt, _) ->
          self#statement stmt;
          scopes#next_index ())
        p;
      scopes#leave_scope ()

    method! fun_decl f =
      let info = scopes#enter_scope Scope_Function in
      if debug () then Format.eprintf "Analysis: enter fun_decl scope %d@." info.id;
      super#fun_decl f;
      scopes#leave_scope ()

    method! block b =
      let info = scopes#enter_scope Scope_Block in
      if debug () then Format.eprintf "Analysis: enter block scope %d@." info.id;
      List.iter
        ~f:(fun (stmt, _) ->
          self#statement stmt;
          scopes#next_index ())
        b;
      scopes#leave_scope ()

    method! variable_declaration kind decl =
      match kind, decl with
      | Var, DeclIdent (id, Some init) ->
          self#note_assignment id;
          self#register_decl id init
      | _, DeclIdent (id, _) ->
          self#note_assignment id;
          super#variable_declaration kind decl
      | _, DeclPattern (pat, init) ->
          self#note_assignments (bound_idents_of_pattern pat);
          self#expression (fst init)

    method! function_body b =
      List.iter
        ~f:(fun (stmt, _) ->
          self#statement stmt;
          scopes#next_index ())
        b

    method! ident id =
      match id with
      | V var -> (
          match Var.Hashtbl.find_opt constants var with
          | Some info ->
              let u =
                match current_init_constant with
                | Some owner -> In_init owner
                | None -> Direct scopes#current_stack
              in
              if debug ()
              then
                Format.eprintf
                  "Analysis: usage of %a %a@."
                  Var.print
                  var
                  print_usage_site
                  u;
              info.usages <- u :: info.usages
          | None -> ())
      | S _ -> ()

    method! for_binding _kind binding =
      self#note_assignments (bound_idents_of_binding binding)

    method! expression expr =
      (match expr with
      | EBin (op, lhs, _rhs) -> (
          (* Track assignments via assignment operators *)
          match op with
          | Eq
          | StarEq
          | SlashEq
          | ModEq
          | PlusEq
          | MinusEq
          | LslEq
          | AsrEq
          | LsrEq
          | BandEq
          | BxorEq
          | BorEq
          | OrEq
          | AndEq
          | ExpEq
          | CoalesceEq -> (
              match lhs with
              | EVar id -> self#note_assignment id
              | EAssignTarget target -> self#note_assignments_from_target target
              | _ -> ())
          | _ -> ())
      | EUn (op, e) -> (
          (* Track increment/decrement operators as assignments *)
          match op with
          | IncrA | DecrA | IncrB | DecrB -> (
              match e with
              | EVar id -> self#note_assignment id
              | _ -> ())
          | _ -> ())
      | _ -> ());
      super#expression expr

    (* Manual Loop Logic *)
    method! statement stmt =
      match stmt with
      | For_statement (init, cond, incr, body) ->
          (match init with
          | Left expr_opt -> self#expression_o expr_opt
          | Right (kind, decls) ->
              List.iter ~f:(fun d -> self#variable_declaration kind d) decls);
          self#expression_o cond;
          self#expression_o incr;
          let info = scopes#enter_scope Scope_Loop in
          if debug () then Format.eprintf "Analysis: enter for loop scope %d@." info.id;
          self#statement (fst body);
          scopes#leave_scope ()
      | ForIn_statement (msg, source, body)
      | ForOf_statement (msg, source, body)
      | ForAwaitOf_statement (msg, source, body) ->
          (match msg with
          | Left expr -> self#expression expr
          | Right (kind, binding) -> self#for_binding kind binding);
          self#expression source;
          let info = scopes#enter_scope Scope_Loop in
          if debug ()
          then Format.eprintf "Analysis: enter loop (for-in/of/await) scope %d@." info.id;
          self#statement (fst body);
          scopes#leave_scope ()
      | While_statement (cond, body) ->
          self#expression cond;
          let info = scopes#enter_scope Scope_Loop in
          if debug () then Format.eprintf "Analysis: enter while loop scope %d@." info.id;
          self#statement (fst body);
          scopes#leave_scope ()
      | Do_while_statement (body, cond) ->
          let info = scopes#enter_scope Scope_Loop in
          if debug ()
          then Format.eprintf "Analysis: enter dowhile loop scope %d@." info.id;
          self#statement (fst body);
          scopes#leave_scope ();
          self#expression cond
      | If_statement (cond, (th, _), el) -> (
          self#expression cond;
          (* Enter scope for then branch *)
          let info = scopes#enter_scope Scope_Block in
          if debug () then Format.eprintf "Analysis: enter if-then scope %d@." info.id;
          self#statement th;
          scopes#leave_scope ();
          (* Enter scope for else branch if present *)
          match el with
          | Some (el_stmt, _) ->
              let info = scopes#enter_scope Scope_Block in
              if debug () then Format.eprintf "Analysis: enter if-else scope %d@." info.id;
              self#statement el_stmt;
              scopes#leave_scope ()
          | None -> ())
      | Switch_statement (disc, cases, default, cases2) ->
          self#expression disc;
          (* Helper to process cases as blocks *)
          let process_case_list clauses =
            List.iter
              ~f:(fun (e, stmts) ->
                self#expression e;
                let info = scopes#enter_scope Scope_Block in
                if debug () then Format.eprintf "Analysis: enter case scope %d@." info.id;
                List.iter
                  ~f:(fun (s, _) ->
                    self#statement s;
                    scopes#next_index ())
                  stmts;
                scopes#leave_scope ())
              clauses
          in
          process_case_list cases;
          (match default with
          | Some stmts ->
              let info = scopes#enter_scope Scope_Block in
              if debug ()
              then Format.eprintf "Analysis: enter default case scope %d@." info.id;
              List.iter
                ~f:(fun (s, _) ->
                  self#statement s;
                  scopes#next_index ())
                stmts;
              scopes#leave_scope ()
          | None -> ());
          process_case_list cases2
      | Block _
      | Variable_statement _
      | Expression_statement _
      | Return_statement _
      | With_statement _
      | Labelled_statement _
      | Throw_statement _
      | Try_statement _
      | Function_declaration _
      | Class_declaration _
      | Empty_statement
      | Continue_statement _
      | Break_statement _
      | Debugger_statement
      | Import _
      | Export _ -> super#statement stmt

    method! class_element e =
      match e with
      | CEStaticBLock _ ->
          let info = scopes#enter_scope Scope_Function in
          if debug ()
          then Format.eprintf "Analysis: enter class static block scope %d@." info.id;
          super#class_element e;
          scopes#leave_scope ()
      | _ -> super#class_element e
  end

(* Analysis Helper Functions *)

(* Compute the LCA (Lowest Common Ancestor) of two scope stacks.
   Stacks are ordered innermost-first, and scope IDs increase monotonically
   as scopes are entered. A smaller ID means the scope was entered earlier
   (closer to the root). The LCA is the deepest scope that is an ancestor of
   both inputs. When two scopes have the same ID, we pick the one with the
   earlier index (the position before either usage within that scope). *)
let rec lca_of_stacks s1 s2 =
  match s1, s2 with
  | h1 :: t1, h2 :: t2 ->
      if h1.id < h2.id
      then lca_of_stacks s1 t2
      else if h1.id > h2.id
      then lca_of_stacks t1 s2
      else if h1.index <= h2.index
      then s1
      else s2
  | [], _ | _, [] -> []

let usage_site_stack u =
  match u with
  | Direct s -> s
  | In_init owner ->
      (* If the owner's target scope is None, this is because the
         owner is not a constant after all, and it is safe to use
         owner.decl_scope in this case. *)
      Option.value ~default:owner.decl_scope owner.target_scope

(* Compute the LCA of all usage sites for a constant *)
let find_lca usages =
  let stacks = List.map ~f:usage_site_stack usages in
  match stacks with
  | [] -> None
  | head :: tail ->
      let lca_stack = List.fold_left ~f:lca_of_stacks ~init:head tail in
      Some lca_stack

(* Check if `lca_stack` is a valid sub-scope (suffix) of `decl_scope`.
   Since stacks are ordered innermost-first, if scope A is inside scope B,
   stack A will have stack B as a suffix (tail). *)
let rec is_suffix list suffix =
  match list, suffix with
  | h1 :: t1, h2 :: _ ->
      if h1.id < h2.id
      then false
      else if h1.id > h2.id
      then is_suffix t1 suffix
      else h2.index <= h1.index
  | _, [] -> assert false
  | [], _ :: _ -> false

(* Validate that sinking to lca_stack is safe:
   1. lca_stack must be within (suffix of) the declaration scope - we can't
      move a declaration to a scope that doesn't contain the original.
      We are sometimes generating this kind of code:
        a:{var x = cst; ...} ... x ...
   2. For allocations (not primitives), the path from lca to decl_scope must
      not cross loop or function boundaries - otherwise we'd create multiple
      allocations where originally there was one. *)
let can_sink_to info lca_stack =
  match info.kind with
  | Primitive -> is_suffix lca_stack info.decl_scope
  | Allocation ->
      let rec check_path stack suffix =
        match stack, suffix with
        | h1 :: t1, h2 :: _ ->
            h1.id >= h2.id
            &&
            if h1.id > h2.id
            then
              (match h1.kind with
                | Scope_Loop | Scope_Function -> false
                | Scope_Block -> true)
              && check_path t1 suffix
            else h2.index <= h1.index
        | _, [] -> assert false
        | [], _ :: _ -> false
      in
      check_path lca_stack info.decl_scope

(* Find the deepest valid scope we can sink to, stopping before
   constraint violations.
   For primitives: return lca_stack directly (no restrictions on movement).
   For allocations: walk from lca toward decl_scope, stopping before entering
   any loop or function scope to avoid creating multiple allocations.

   The walk processes scopes from innermost (lca) to outermost (toward root),
   accumulating valid scopes in `acc`. When we hit a loop/function boundary,
   we discard accumulated scopes (walk [] rem) since we can't cross it.
   Returns the deepest (innermost) valid position. *)
let find_valid_target info lca_stack =
  match info.kind with
  | Primitive -> if is_suffix lca_stack info.decl_scope then Some lca_stack else None
  | Allocation ->
      let root = info.decl_scope in
      let rec walk acc stack =
        match stack, root with
        | [], _ -> None
        | _, [] -> assert false
        | scope :: rem, scope' :: _ -> (
            if scope.id < scope'.id
            then
              (* Not a suffix *)
              None
            else if scope.id = scope'.id
            then
              if scope.index < scope'.index
              then
                (* Not a suffix *)
                None
              else Some (List.rev_append acc stack)
            else
              match scope.kind with
              | Scope_Loop | Scope_Function ->
                  (* Can't cross this boundary; restart
                       accumulation. Any scope deeper than this
                       boundary (currently in acc) is invalid because
                       moving there would cross the boundary. *)
                  walk [] rem
              | Scope_Block -> walk (scope :: acc) rem)
      in
      walk [] lca_stack

(* Pass 2: Transformation *)

module ScopeTable = Int.Hashtbl

type insertion =
  { index : int
  ; var : Var.t
  ; init : expression * location
  }

type insertion_plan = insertion list

class transformation
  (plan : insertion_plan ScopeTable.t)
  (removals : unit Var.Hashtbl.t)
  (inlinings : expression Var.Hashtbl.t) =
  object (self)
    inherit Js_traverse.map as super

    inherit scope_manager as scopes

    (* Inline single-use constants at their usage site *)
    method! expression expr =
      match expr with
      | EVar (V x) -> (
          match Var.Hashtbl.find_opt inlinings x with
          | Some init_expr ->
              if debug () then Format.eprintf "Inlining %a@." Var.print x;
              self#expression init_expr (* Recursively inline *)
          | None -> super#expression expr)
      | _ -> super#expression expr

    (* Helper to apply insertions to a list of statements *)
    method apply_insertions scope_id stmts =
      let insertions = Option.value (ScopeTable.find_opt plan scope_id) ~default:[] in
      (* Fast path: no insertions, just transform statements *)
      match insertions with
      | [] -> List.map ~f:(fun (s, l) -> self#statement s, l) stmts
      | _ ->
          (* Build IntMap: index -> list of (ident, init) *)
          let insertion_map =
            List.fold_left
              ~f:(fun acc ins ->
                let existing = Option.value (IntMap.find_opt ins.index acc) ~default:[] in
                IntMap.add ins.index (ins :: existing) acc)
              ~init:IntMap.empty
              insertions
          in
          (* Process statements *)
          List.mapi
            ~f:(fun i stmt ->
              let to_insert =
                Option.value (IntMap.find_opt i insertion_map) ~default:[]
              in
              let injected_stmts =
                List.rev_map
                  ~f:(fun ins ->
                    let loc = snd stmt in
                    if debug ()
                    then
                      Format.eprintf
                        "Transform: inserting %a at scope %d, index %d@."
                        Var.print
                        ins.var
                        scope_id
                        i;
                    ( Variable_statement
                        ( Var
                        , [ DeclIdent
                              ( V ins.var
                              , Some (self#expression (fst ins.init), snd ins.init) )
                          ] )
                    , loc ))
                  to_insert
              in
              let s, l = stmt in
              let stmt' = self#statement s in
              injected_stmts @ [ stmt', l ])
            stmts
          |> List.flatten

    method! program p =
      let info = scopes#enter_scope Scope_Block in
      if debug () then Format.eprintf "Transform: enter program scope %d@." info.id;
      let new_progs = self#apply_insertions info.id p in
      scopes#leave_scope ();
      new_progs

    method! block b =
      let info = scopes#enter_scope Scope_Block in
      (if debug ()
       then
         let parent_id =
           match scopes#current_stack with
           | _ :: p :: _ -> p.id
           | _ -> -1
         in
         Format.eprintf "Transform: enter block scope %d (parent %d)@." info.id parent_id);
      let new_stmts = self#apply_insertions info.id b in
      scopes#leave_scope ();
      new_stmts

    method! fun_decl f =
      let info = scopes#enter_scope Scope_Function in
      if debug () then Format.eprintf "Transform: enter fun_decl scope %d@." info.id;
      let k, params, body, loc = super#fun_decl f in
      scopes#leave_scope ();
      k, params, body, loc

    (* Override to match analysis traversal order: init first, then pattern *)
    method! variable_declaration kind decl =
      match kind, decl with
      | Var, DeclIdent (id, Some (e, loc)) ->
          let e' = self#expression e in
          DeclIdent (self#ident id, Some (e', self#loc loc))
      | (Let | Const | Using | AwaitUsing), _ | _, (DeclIdent (_, None) | DeclPattern _)
        -> super#variable_declaration kind decl

    method! function_body stmts =
      let current = List.hd scopes#current_stack in
      self#apply_insertions current.id stmts

    method! statement stmt =
      match stmt with
      | Variable_statement (kind, decls) -> (
          let new_decls_processed =
            List.filter_map
              ~f:(fun decl ->
                let keep =
                  match decl with
                  | DeclIdent (V x, _) when Var.Hashtbl.mem removals x ->
                      if debug ()
                      then Format.eprintf "Transform: removing decl %a@." Var.print x;
                      false
                  | _ -> true
                in
                if keep then Some (self#variable_declaration kind decl) else None)
              decls
          in
          match new_decls_processed with
          | [] -> Empty_statement
          | _ -> Variable_statement (kind, new_decls_processed))
      | For_statement (init, cond, incr, body) ->
          let init' =
            match init with
            | Left expr_opt -> Left (self#expression_o expr_opt)
            | Right (kind, decls) ->
                Right (kind, List.map ~f:(fun d -> self#variable_declaration kind d) decls)
          in
          let cond' = self#expression_o cond in
          let incr' = self#expression_o incr in
          let body' = self#handle_scoped_body Scope_Loop body in
          For_statement (init', cond', incr', body')
      | While_statement (cond, body) ->
          let cond' = self#expression cond in
          let body' = self#handle_scoped_body Scope_Loop body in
          While_statement (cond', body')
      | Do_while_statement (body, cond) ->
          let body' = self#handle_scoped_body Scope_Loop body in
          let cond' = self#expression cond in
          Do_while_statement (body', cond')
      | ForIn_statement (msg, source, body) ->
          let msg' =
            match msg with
            | Left expr -> Left (self#expression expr)
            | Right (kind, binding) -> Right (kind, self#for_binding kind binding)
          in
          let source' = self#expression source in
          let body' = self#handle_scoped_body Scope_Loop body in
          ForIn_statement (msg', source', body')
      | ForOf_statement (msg, source, body) ->
          let msg' =
            match msg with
            | Left expr -> Left (self#expression expr)
            | Right (kind, binding) -> Right (kind, self#for_binding kind binding)
          in
          let source' = self#expression source in
          let body' = self#handle_scoped_body Scope_Loop body in
          ForOf_statement (msg', source', body')
      | If_statement (e, (th, th_loc), el) ->
          let e' = self#expression e in
          let th' = self#handle_scoped_body Scope_Block (th, th_loc) in
          let el' =
            match el with
            | Some el_branch -> Some (self#handle_scoped_body Scope_Block el_branch)
            | None -> None
          in
          If_statement (e', th', el')
      | Switch_statement (disc, cases, default, cases2) ->
          let disc' = self#expression disc in
          let process_case_list clauses =
            List.map
              ~f:(fun (e, stmts) ->
                let e' = self#expression e in
                let info = scopes#enter_scope Scope_Block in
                let stmts' = self#apply_insertions info.id stmts in
                scopes#leave_scope ();
                e', stmts')
              clauses
          in
          let cases' = process_case_list cases in
          let default' =
            Option.map
              ~f:(fun stmts ->
                let info = scopes#enter_scope Scope_Block in
                let stmts' = self#apply_insertions info.id stmts in
                scopes#leave_scope ();
                stmts')
              default
          in
          let cases2' = process_case_list cases2 in
          Switch_statement (disc', cases', default', cases2')
      | ForAwaitOf_statement (msg, source, body) ->
          let msg' =
            match msg with
            | Left expr -> Left (self#expression expr)
            | Right (kind, binding) -> Right (kind, self#for_binding kind binding)
          in
          let source' = self#expression source in
          let body' = self#handle_scoped_body Scope_Loop body in
          ForAwaitOf_statement (msg', source', body')
      | Block _
      | Return_statement _
      | With_statement _
      | Labelled_statement _
      | Throw_statement _
      | Expression_statement _
      | Try_statement _
      | Function_declaration _
      | Class_declaration _
      | Empty_statement
      | Continue_statement _
      | Break_statement _
      | Debugger_statement
      | Import _
      | Export _ -> super#statement stmt

    method! class_element e =
      match e with
      | CEStaticBLock _ ->
          let info = scopes#enter_scope Scope_Function in
          if debug ()
          then Format.eprintf "Transform: enter class static block scope %d@." info.id;
          let e = super#class_element e in
          scopes#leave_scope ();
          e
      | _ -> super#class_element e

    (* Shared helper for loop/if bodies. When we need to insert declarations
       into a body that might be a single statement (e.g., `while (x) foo();`),
       we must wrap it in a block to have somewhere to place the var decls. *)
    method private handle_scoped_body scope_kind body =
      let info = scopes#enter_scope scope_kind in
      let s, l = body in
      let insertions = Option.value (ScopeTable.find_opt plan info.id) ~default:[] in

      let body_stmt' = self#statement s in
      let body' = body_stmt', l in

      let res =
        match insertions with
        | [] -> body'
        | _ -> (
            (* Create variables *)
            let vars =
              List.map
                ~f:(fun ins ->
                  let e, loc = ins.init in
                  ( Variable_statement
                      (Var, [ DeclIdent (V ins.var, Some (self#expression e, loc)) ])
                  , l ))
                insertions
            in
            (* Wrap in Block *)
            match body_stmt' with
            | Block b -> Block (vars @ b), l
            | _ -> Block (vars @ [ body' ]), l)
      in
      scopes#leave_scope ();
      res
  end

let f program =
  let t = Timer.make () in
  let t1 = Timer.make () in
  let analyzer = new analysis in
  analyzer#program program;
  if times () then Format.eprintf "      constant sinking: analyze: %a@." Timer.print t1;

  let t1 = Timer.make () in
  let plan = ScopeTable.create 16 in
  let removals = Var.Hashtbl.create 128 in
  let inlinings = Var.Hashtbl.create 128 in
  let moved_count = ref 0 in
  let inlined_count = ref 0 in
  (* Check if inlining is safe: single usage and constraints pass *)
  let can_inline info =
    match info.usages with
    | [ u ] ->
        (* Single direct usage - check constraints *)
        let stack = usage_site_stack u in
        if can_sink_to info stack then Some stack else None
    | [] | _ :: _ -> None
  in
  List.iter
    ~f:(fun info ->
      match can_inline info with
      | Some target_stack ->
          (* Single use with safe constraints: inline at usage site *)
          if debug () then Format.eprintf "Inline: %a (single use)@." Var.print info.var;
          info.target_scope <- Some target_stack;
          incr inlined_count;
          Var.Hashtbl.add inlinings info.var (fst info.init);
          Var.Hashtbl.add removals info.var ()
      | None -> (
          match find_lca info.usages with
          | Some lca -> (
              match find_valid_target info lca with
              | Some target_stack -> (
                  (* Invariants: LCA and target are within decl scope, constraints hold *)
                  assert (is_suffix lca info.decl_scope);
                  assert (is_suffix target_stack info.decl_scope);
                  assert (can_sink_to info target_stack);
                  info.target_scope <- Some target_stack;

                  assert (not (List.is_empty target_stack));
                  match target_stack with
                  | target_scope :: _ ->
                      let target_id = target_scope.id in
                      let decl_id = (List.hd info.decl_scope).id in
                      let definition_index = (List.hd info.decl_scope).index in
                      let is_same_pos =
                        target_id = decl_id && target_scope.index = definition_index
                      in

                      if not is_same_pos
                      then (
                        if debug ()
                        then
                          Format.eprintf
                            "Plan: %a -> scope %d, index %d (decl was scope %d, index \
                             %d), %d usages@."
                            Var.print
                            info.var
                            target_id
                            target_scope.index
                            decl_id
                            definition_index
                            (List.length info.usages);
                        let current_list =
                          Option.value (ScopeTable.find_opt plan target_id) ~default:[]
                        in
                        ScopeTable.replace
                          plan
                          target_id
                          ({ index = target_scope.index
                           ; var = info.var
                           ; init = info.init
                           }
                          :: current_list);
                        incr moved_count;
                        Var.Hashtbl.add removals info.var ())
                  | [] -> ())
              | None -> info.target_scope <- Some info.decl_scope)
          | None -> info.target_scope <- Some info.decl_scope))
    analyzer#get_ordered_constants;
  if times () then Format.eprintf "      constant sinking: planning: %a@." Timer.print t1;

  let t1 = Timer.make () in
  let program =
    if Var.Hashtbl.length removals = 0
    then program
    else
      let transformer = new transformation plan removals inlinings in
      transformer#program program
  in
  if times () then Format.eprintf "      constant sinking: transform: %a@." Timer.print t1;
  if times () then Format.eprintf "    constant sinking: %a@." Timer.print t;
  if stats ()
  then
    Format.eprintf
      "Stats - constant sinking: %d moved, %d inlined@."
      !moved_count
      !inlined_count;
  program
