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

   When this [local.get x] is the only read of [x] in the function, the
   tee would store a value that is never read: we put [e] in place of
   the [local.get x] instead, and drop the declaration of [x] if it has
   no other write.

   Effect ordering: moving [e] past other code is sound iff

   - [e] itself has no observable side effects (no writes, no calls,
     no traps beyond the ones effect-free expressions are allowed);
   - every sub-expression we cross is effect-free (see [purity]); and
   - no instruction we cross writes any local that [e] reads — if it
     did, [e]'s reads would see a different value at the new position.

   The last condition is what makes an "effect-free implies safe"
   rule unsound: effect-free expressions can still read mutable
   locals, and if the intermediate code writes those locals the move
   would change [e]'s result.

   Traps: we deliberately treat potentially-trapping reads
   (e.g. [StructGet]/[ArrayGet]/[RefCast]/[I31Get] on a null or
   out-of-bounds operand) as having no observable effect. We assume
   traps never happen — the same assumption Binaryen runs under
   (--traps-never-happen) — so reordering such a read relative to other
   code, or across an [Event], cannot change observable behaviour: there
   is no trap whose occurrence or source-map attribution we could move.

   Abnormal exits: an [e] that is not effect-free may leave its
   position without falling through, by raising or by branching to an
   enclosing label. Once sunk past a [local.set y], the code reached
   this way sees the new value of [y] instead of the old one. This
   matters when that code reads [y]: parse_bytecode emits a [Code.Assign]
   for a mutable variable read by an exception handler, which becomes a
   [local.set] inside the [try] body. Without liveness information at
   the exit targets, we only let such an [e] cross a [local.set] when it
   cannot reach any of them: it contains no branch ([may_branch]), and
   we are not inside a [try] body, so that an exception it raises leaves
   the function.
*)

open! Stdlib
module W = Wasm_ast
module Var = Code.Var

(* Hard upper bound on how far a single sink attempt may search forward.
   The walker is O(N) per candidate with O(N) candidates per function, so
   without a cap the pass is O(N²). Most profitable sinks have the target
   within a few steps; this bound just clips the long tail. *)
let max_walk_distance = 32

let times = Debug.find "times"

let stats = Debug.find "stats"

(* Aggregated statistics across all calls to [f]. The pass runs once per
   Wasm function; per-function logs are noisy, so we accumulate here and
   emit a single summary via [report_stats]. *)
let total_time = ref 0.

let total_calls = ref 0

let total_candidates = ref 0

let total_sunk = ref 0

let total_budget_exhausted = ref 0

let total_sink_distance = ref 0

let report_stats () =
  if !total_calls > 0
  then (
    if times () then Format.eprintf "  wasm local sink: %.2f@." !total_time;
    (if stats ()
     then
       let avg_sink_distance =
         if !total_sunk = 0
         then 0.
         else float_of_int !total_sink_distance /. float_of_int !total_sunk
       in
       Format.eprintf
         "Stats - wasm local sink: %d functions, %d candidates, %d sunk, %d \
          budget-limited, avg distance %.1f@."
         !total_calls
         !total_candidates
         !total_sunk
         !total_budget_exhausted
         avg_sink_distance);
    total_time := 0.;
    total_calls := 0;
    total_candidates := 0;
    total_sunk := 0;
    total_budget_exhausted := 0;
    total_sink_distance := 0)

(* Can [e] branch to a label of the enclosing function? We do not
   track which labels are targeted, so any construct holding
   instructions counts as a possible branch. *)
let rec may_branch (e : W.expression) =
  match e with
  | Br_on_cast _ | Br_on_cast_fail _ | Br_on_null _ | BlockExpr _ | Seq _ | Try _ -> true
  | _ -> (
      let exception Found in
      try
        Wasm_traverse.iter_expression
          ~expression:(fun e -> if may_branch e then raise Found)
          ~instructions:(fun _ -> raise Found)
          e;
        false
      with Found -> true)

(* Locals read and written by an expression. A [LocalTee y e'] both reads
   [e']'s locals and writes [y].

   [Pop] reads the implicit Wasm value stack, not a local, so it
   contributes nothing here. This means the stack-ordering dependency
   between a [Push] and a later [Pop] is not modelled: sinking a
   [Pop]-bearing [e] across a [Push] would let the [Pop] consume the
   just-pushed value instead of the original stack top. This is safe only
   because codegen consumes every [Pop] immediately (e.g. try/catch
   results are [Push]ed and popped right away), so a
   [local.set]/[local.get] pair never straddles a [Push] of an unrelated
   value. *)
let rw_of_expr e =
  let reads = ref Var.Set.empty in
  let writes = ref Var.Set.empty in
  Wasm_traverse.iter_locals_expression
    ~read:(fun x -> reads := Var.Set.add x !reads)
    ~write:(fun x -> writes := Var.Set.add x !writes)
    e;
  !reads, !writes

(* What code moved past an expression may observe, from the least to the
   most constrained:
   - [Pure]: the expression only reads locals — no heap or global reads,
     no calls, no side effects;
   - [Effect_free]: no side effect, and it always falls through (we
     assume traps never happen), but it may read the heap or globals,
     so an effectful [e] cannot be moved past it;
   - [Effectful]: anything else. *)
type purity =
  | Pure
  | Effect_free
  | Effectful

let join p p' =
  match p, p' with
  | Effectful, _ | _, Effectful -> Effectful
  | Effect_free, _ | _, Effect_free -> Effect_free
  | Pure, Pure -> Pure

(* Walker result for a single expression. [Clean p] means "no occurrence
   of x in this expression", with [p] the purity of the expression, as
   seen by the [e] we are sinking: a read of a local that [e] writes
   makes an expression [Effectful], since [e] cannot be moved past it. *)
type walk_result =
  | Found of W.expression
  | Bail
  | Clean of purity

(* [ctx] bundles the parameters that don't change during a sink attempt:
   the target variable, the expression to sink, the locals it reads and
   writes, and whether the expression itself is effect-free.
   - [x]: the target variable; [None] when we only compute the purity of
     an expression (see [purity]).
   - [reads]: crossing a write to any of these would change [e]'s result.
   - [writes]: crossing a read of any of these would make the reader see
     the pre-sink value instead of the one [e] would have stored.
   - [e_effect_free]: when [false], we may not cross any *evaluated*
     sub-expression or instruction that is not [Pure] — the path could
     read heap/global state that [e]'s side effects would change.
   - [e_may_exit]: [e] may leave its position abnormally to code in
     this function (see "Abnormal exits" in the header), so it must not
     cross a [local.set].
   - [substitute]: the target [local.get x] is the only read of [x], so
     we replace it by [e] rather than by [local.tee x e]. *)
type ctx =
  { x : Var.t option
  ; e_to_sink : W.expression
  ; substitute : bool
  ; reads : Var.Set.t
  ; writes : Var.Set.t
  ; e_effect_free : bool
  ; e_may_exit : bool
  ; mutable budget : int
  }

(* Spend one unit of walk budget. Returns [false] when the budget is
   exhausted, in which case callers must Bail rather than continue. We
   only tick at the points where the walker steps forward in evaluation
   order (instruction-to-instruction or sibling-to-sibling); descending
   into a unary sub-expression is free. Every caller bails immediately on
   a [false] result, so the budget-exhausted branch is reached at most
   once per sink attempt and the counter is incremented exactly once. *)
let tick ctx =
  if ctx.budget <= 0
  then (
    if Option.is_some ctx.x then incr total_budget_exhausted;
    false)
  else (
    ctx.budget <- ctx.budget - 1;
    true)

(* A sibling sub-expression was walked [Clean] (no x) and we're about
   to continue to the next sibling. This is the reorder point: [e] will
   evaluate *after* [sibling] in the sunk version, whereas originally
   [e] ran first. The sibling must have no observable side effects,
   must not read a local that [e] writes (both are covered by
   [Effectful]), and must not read the heap or globals if [e] has side
   effects. (The converse — [sibling] writing something [e] reads — is
   caught by [LocalTee] being [Effectful] and by the instruction-level
   checks in [try_sink_in_instr].) *)
let may_cross ctx p =
  match p with
  | Pure -> true
  | Effect_free -> ctx.e_effect_free
  | Effectful -> false

let rec walk_expr ctx (e : W.expression) =
  match e with
  | W.Const _ | RefFunc _ | RefNull _ -> Clean Pure
  | GlobalGet _ -> Clean Effect_free
  | Pop _ -> Clean Effectful
  | LocalGet y -> (
      match ctx.x with
      | Some x when Var.equal y x ->
          Found (if ctx.substitute then ctx.e_to_sink else W.LocalTee (x, ctx.e_to_sink))
      | _ -> Clean (if Var.Set.mem y ctx.writes then Effectful else Pure))
  | LocalTee (y, e') -> (
      match ctx.x with
      | Some x when Var.equal y x ->
          (* Another write to [x] — bail. *)
          Bail
      | _ -> (
          match walk_expr ctx e' with
          | Found e'' -> Found (W.LocalTee (y, e''))
          | Bail -> Bail
          | Clean _ -> Clean Effectful))
  | UnOp (op, e') -> wrap_unary (fun e -> W.UnOp (op, e)) Pure ctx e'
  | I32WrapI64 e' -> wrap_unary (fun e -> W.I32WrapI64 e) Pure ctx e'
  | I64ExtendI32 (s, e') -> wrap_unary (fun e -> W.I64ExtendI32 (s, e)) Pure ctx e'
  | F32DemoteF64 e' -> wrap_unary (fun e -> W.F32DemoteF64 e) Pure ctx e'
  | F64PromoteF32 e' -> wrap_unary (fun e -> W.F64PromoteF32 e) Pure ctx e'
  | RefI31 e' -> wrap_unary (fun e -> W.RefI31 e) Pure ctx e'
  | I31Get (s, e') -> wrap_unary (fun e -> W.I31Get (s, e)) Pure ctx e'
  | ExternConvertAny e' -> wrap_unary (fun e -> W.ExternConvertAny e) Pure ctx e'
  | AnyConvertExtern e' -> wrap_unary (fun e -> W.AnyConvertExtern e) Pure ctx e'
  | ArrayLen e' -> wrap_unary (fun e -> W.ArrayLen e) Effect_free ctx e'
  | StructGet (s, ty, i, e') ->
      wrap_unary (fun e -> W.StructGet (s, ty, i, e)) Effect_free ctx e'
  | RefCast (ty, e') -> wrap_unary (fun e -> W.RefCast (ty, e)) Effect_free ctx e'
  | RefTest (ty, e') -> wrap_unary (fun e -> W.RefTest (ty, e)) Effect_free ctx e'
  | BinOp (op, e1, e2) -> wrap_binary (fun a b -> W.BinOp (op, a, b)) Pure ctx e1 e2
  | RefEq (e1, e2) -> wrap_binary (fun a b -> W.RefEq (a, b)) Pure ctx e1 e2
  | ArrayNew (ty, e1, e2) ->
      wrap_binary (fun a b -> W.ArrayNew (ty, a, b)) Effect_free ctx e1 e2
  | ArrayNewData (ty, d, e1, e2) ->
      wrap_binary (fun a b -> W.ArrayNewData (ty, d, a, b)) Effect_free ctx e1 e2
  | ArrayGet (s, ty, e1, e2) ->
      wrap_binary (fun a b -> W.ArrayGet (s, ty, a, b)) Effect_free ctx e1 e2
  | Call (f, args) -> wrap_list (fun args' -> W.Call (f, args')) Effectful ctx args
  | ArrayNewFixed (ty, args) ->
      wrap_list (fun args' -> W.ArrayNewFixed (ty, args')) Effect_free ctx args
  | StructNew (ty, args) ->
      wrap_list (fun args' -> W.StructNew (ty, args')) Effect_free ctx args
  | Call_ref (ty, f, args) -> (
      (* Wasm evaluates args before the funcref. *)
      match wrap_list_intermediate ctx args with
      | `Found args' -> Found (W.Call_ref (ty, f, args'))
      | `Bail -> Bail
      | `Clean _ -> (
          (* Between args and f, we've crossed every arg — allowed
             only if each is [may_cross]-safe. That check was already
             made inside [wrap_list_intermediate]. *)
          match walk_expr ctx f with
          | Found f' -> Found (W.Call_ref (ty, f', args))
          | Bail -> Bail
          | Clean _ -> Clean Effectful))
  | IfExpr (ty, cond, e1, e2) -> (
      match walk_expr ctx cond with
      | Found cond' -> Found (W.IfExpr (ty, cond', e1, e2))
      | Bail -> Bail
      | Clean p -> (
          (* We do not sink into a branch, which is evaluated
             conditionally. *)
          match walk_expr ctx e1 with
          | Found _ | Bail -> Bail
          | Clean p1 -> (
              match walk_expr ctx e2 with
              | Found _ | Bail -> Bail
              | Clean p2 -> Clean (join Effect_free (join p (join p1 p2))))))
  | BlockExpr _ | Seq _ | Try _ | Br_on_cast _ | Br_on_cast_fail _ | Br_on_null _ ->
      (* Compound / control-flow-like expressions; we don't descend past
         these for sinking purposes. *)
      Bail

and wrap_unary make p ctx e' =
  match walk_expr ctx e' with
  | Found e'' -> Found (make e'')
  | Bail -> Bail
  | Clean p' -> Clean (join p p')

and wrap_binary make p ctx e1 e2 =
  match walk_expr ctx e1 with
  | Found e1' -> Found (make e1' e2)
  | Bail -> Bail
  | Clean p1 -> (
      if not (may_cross ctx p1 && tick ctx)
      then Bail
      else
        match walk_expr ctx e2 with
        | Found e2' -> Found (make e1 e2')
        | Bail -> Bail
        | Clean p2 -> Clean (join p (join p1 p2)))

and wrap_list make p ctx args =
  match wrap_list_intermediate ctx args with
  | `Found args' -> Found (make args')
  | `Bail -> Bail
  | `Clean p' -> Clean (join p p')

and wrap_list_intermediate ctx args =
  let rec loop acc p = function
    | [] -> `Clean p
    | a :: rest -> (
        match walk_expr ctx a with
        | Found a' -> `Found (List.rev_append acc (a' :: rest))
        | Bail -> `Bail
        | Clean p' ->
            if not (may_cross ctx p' && tick ctx)
            then `Bail
            else loop (a :: acc) (join p p') rest)
  in
  loop [] Pure args

(* Purity of an expression, as seen by code that does not write any
   local. Compound expressions (blocks, branches, ...) are [Effectful]. *)
let purity e =
  let ctx =
    { x = None
    ; e_to_sink = e
    ; substitute = false
    ; reads = Var.Set.empty
    ; writes = Var.Set.empty
    ; e_effect_free = true
    ; e_may_exit = false
    ; budget = max_int
    }
  in
  match walk_expr ctx e with
  | Clean p -> p
  | Bail | Found _ -> Effectful

(* Walk a single instruction looking for a sink target for [ctx.x].
   [IClean] means that [x] does not occur in the instruction, and that
   we may move [e] past it. *)
type instr_result =
  | IFound of W.instruction
  | IBail
  | IClean

let try_sink_in_instr ctx instr : instr_result =
  (* Look for [x] in an expression, which must be crossed when [x] is
     not found: [crossable p] tells whether this is allowed. *)
  let wrap_one ?(crossable = fun _ -> false) make e =
    match walk_expr ctx e with
    | Found e' -> IFound (make e')
    | Bail -> IBail
    | Clean p -> if crossable p then IClean else IBail
  in
  match (instr : W.instruction) with
  | Nop -> IClean
  | Event _ ->
      (* An [Event] carries a source-map location for everything that
         follows it: moving an effectful [e] past an event would
         re-attribute its call to the wrong source position, so we only
         cross events when [e] is itself effect-free. (A
         potentially-trapping but otherwise effect-free [e] may still
         cross an event: see the traps assumption in the header — we
         assume the trap never fires, so its attribution does not
         matter.) *)
      if ctx.e_effect_free then IClean else IBail
  | Drop e -> wrap_one ~crossable:(may_cross ctx) (fun e -> W.Drop e) e
  | Push e -> wrap_one ~crossable:(may_cross ctx) (fun e -> W.Push e) e
  | LocalSet (y, e) when Option.equal Var.equal (Some y) ctx.x ->
      (* x may still appear inside [e] (evaluated before the set). If we
         find and rewrite it there, we stop (the [local.set x] after
         would be a shadowing write). If [e] is [Clean], this is a
         shadowing write without a sink target → bail. *)
      wrap_one (fun e -> W.LocalSet (y, e)) e
  | LocalSet (y, e) ->
      wrap_one
        ~crossable:(fun p ->
          (not ctx.e_may_exit)
          && may_cross ctx p
          && (not (Var.Set.mem y ctx.reads))
          && not (Var.Set.mem y ctx.writes))
        (fun e -> W.LocalSet (y, e))
        e
  | GlobalSet (g, e) -> wrap_one (fun e -> W.GlobalSet (g, e)) e
  | StructSet (ty, i, e1, e2) -> (
      match walk_expr ctx e1 with
      | Found e1' -> IFound (W.StructSet (ty, i, e1', e2))
      | Bail -> IBail
      | Clean p1 ->
          if not (may_cross ctx p1 && tick ctx)
          then IBail
          else wrap_one (fun e2' -> W.StructSet (ty, i, e1, e2')) e2)
  | ArraySet (ty, e1, e2, e3) -> (
      match walk_expr ctx e1 with
      | Found e1' -> IFound (W.ArraySet (ty, e1', e2, e3))
      | Bail -> IBail
      | Clean p1 -> (
          if not (may_cross ctx p1 && tick ctx)
          then IBail
          else
            match walk_expr ctx e2 with
            | Found e2' -> IFound (W.ArraySet (ty, e1, e2', e3))
            | Bail -> IBail
            | Clean p2 ->
                if not (may_cross ctx p2 && tick ctx)
                then IBail
                else wrap_one (fun e3' -> W.ArraySet (ty, e1, e2, e3')) e3))
  | CallInstr (f, args) -> (
      match wrap_list_intermediate ctx args with
      | `Found args' -> IFound (W.CallInstr (f, args'))
      | `Bail | `Clean _ -> IBail)
  (* Control-flow-terminal instructions with sub-expressions: we can still
     rewrite within the expression, but cannot continue past on a Clean. *)
  | Return (Some e) -> wrap_one (fun e -> W.Return (Some e)) e
  | Throw (t, e) -> wrap_one (fun e -> W.Throw (t, e)) e
  | Br (n, Some e) -> wrap_one (fun e -> W.Br (n, Some e)) e
  | Br_if (n, e) -> wrap_one (fun e -> W.Br_if (n, e)) e
  | Br_table (e, tl, d) -> wrap_one (fun e -> W.Br_table (e, tl, d)) e
  | Return_call (f, args) -> (
      match wrap_list_intermediate ctx args with
      | `Found args' -> IFound (W.Return_call (f, args'))
      | `Bail | `Clean _ -> IBail)
  | Return_call_ref (ty, f, args) -> (
      match wrap_list_intermediate ctx args with
      | `Found args' -> IFound (W.Return_call_ref (ty, f, args'))
      | `Bail -> IBail
      | `Clean _ -> wrap_one (fun f' -> W.Return_call_ref (ty, f', args)) f)
  | Return None | Br (_, None) | Rethrow _ | Unreachable -> IBail
  | If (ty, cond, l1, l2) ->
      (* The condition is evaluated unconditionally before branching —
         we can still sink into it. If no [x] is found there, we bail
         because we can't continue past the branch. *)
      wrap_one (fun cond' -> W.If (ty, cond', l1, l2)) cond
  | Loop _ | Block _ ->
      (* No expression to walk at this level; do not sink into the body. *)
      IBail

let try_sink_in_list ctx instrs =
  let rec loop acc = function
    | [] -> None
    | instr :: rest -> (
        match try_sink_in_instr ctx instr with
        | IFound instr' -> Some (List.rev_append acc (instr' :: rest))
        | IBail -> None
        | IClean -> if tick ctx then loop (instr :: acc) rest else None)
  in
  loop [] instrs

(* [in_try]: we are inside the body of a [Try], so an exception raised
   here may reach a handler of this function. [reads] and [writes]
   count the accesses to each local; [removed] collects the locals we
   no longer access at all. *)
type env =
  { in_try : bool
  ; reads : int Var.Hashtbl.t
  ; writes : int Var.Hashtbl.t
  ; removed : unit Var.Hashtbl.t
  }

let count tbl x = Option.value ~default:0 (Var.Hashtbl.find_opt tbl x)

(* In pretty mode, keep the variables with a user-given name, so that
   they remain visible in a debugger. *)
let may_remove x = (not (Config.Flag.pretty ())) || Var.generated_name x

(* Bottom-up transformation: recurse first, then try to sink each
   [local.set] into the (already-transformed) tail. *)
let rec transform_instrs env instrs =
  match instrs with
  | [] -> []
  | W.LocalSet (x, e) :: rest -> (
      let e = transform_expr env e in
      let rest = transform_instrs env rest in
      let reads, writes = rw_of_expr e in
      let e_effect_free =
        match purity e with
        | Pure | Effect_free -> true
        | Effectful -> false
      in
      let substitute = count env.reads x = 1 && may_remove x in
      let ctx =
        { x = Some x
        ; e_to_sink = e
        ; substitute
        ; reads
        ; writes
        ; e_effect_free
        ; e_may_exit = (not e_effect_free) && (env.in_try || may_branch e)
        ; budget = max_walk_distance
        }
      in
      incr total_candidates;
      match try_sink_in_list ctx rest with
      | Some new_rest ->
          incr total_sunk;
          if substitute
          then (
            Var.Hashtbl.replace env.reads x 0;
            let w = count env.writes x - 1 in
            Var.Hashtbl.replace env.writes x w;
            if w = 0 then Var.Hashtbl.replace env.removed x ());
          total_sink_distance := !total_sink_distance + (max_walk_distance - ctx.budget);
          new_rest
      | None -> W.LocalSet (x, e) :: rest)
  | (W.Event _ as ev) :: rest -> (
      (* Sinking a [local.set] that previously separated two events
         leaves them adjacent. Drop the earlier one so the closer
         (later) event wins, matching the policy used in
         [parse_bytecode], [code_generation], [deadcode], etc. *)
      let rest = transform_instrs env rest in
      match rest with
      | W.Event _ :: _ -> rest
      | _ -> ev :: rest)
  | instr :: rest ->
      let instr = transform_instr env instr in
      let rest = transform_instrs env rest in
      instr :: rest

and transform_instr env instr =
  Wasm_traverse.map_instruction
    ~expression:(transform_expr env)
    ~instructions:(transform_instrs env)
    instr

and transform_expr env (e : W.expression) =
  match e with
  | Try (ty, body, catches) ->
      Try (ty, transform_instrs { env with in_try = true } body, catches)
  | _ ->
      Wasm_traverse.map_expression
        ~expression:(transform_expr env)
        ~instructions:(transform_instrs env)
        e

let f ~locals instrs =
  let t = Timer.make () in
  incr total_calls;
  let reads = Var.Hashtbl.create 64 in
  let writes = Var.Hashtbl.create 64 in
  let incr tbl x = Var.Hashtbl.replace tbl x (count tbl x + 1) in
  Wasm_traverse.iter_locals ~read:(incr reads) ~write:(incr writes) instrs;
  let env = { in_try = false; reads; writes; removed = Var.Hashtbl.create 16 } in
  let instrs =
    match instrs with
    | (W.Event _ as ev) :: rest ->
        (* This event gives the location of the function: it must remain
           first, even if the code it covered gets sunk past the next
           event (see [Wasm_output]). *)
        ev :: transform_instrs env rest
    | _ -> transform_instrs env instrs
  in
  let locals =
    if Var.Hashtbl.length env.removed = 0
    then locals
    else List.filter locals ~f:(fun (x, _) -> not (Var.Hashtbl.mem env.removed x))
  in
  total_time := !total_time +. Timer.get t;
  locals, instrs
