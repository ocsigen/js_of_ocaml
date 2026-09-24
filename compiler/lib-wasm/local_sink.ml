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
     no traps beyond the ones [effect_free] already tolerates);
   - every sub-expression we cross is [effect_free]; and
   - no instruction we cross writes any local that [e] reads — if it
     did, [e]'s reads would see a different value at the new position.

   The last condition is what makes an "[effect_free] implies safe"
   rule unsound: [effect_free] expressions can still read mutable
   locals, and if the intermediate code writes those locals the move
   would change [e]'s result.

   Traps: [effect_free] deliberately treats potentially-trapping reads
   (e.g. [StructGet]/[ArrayGet]/[RefCast]/[I31Get] on a null or
   out-of-bounds operand) as having no observable effect. We assume
   traps never happen — the same assumption Binaryen runs under
   (--traps-never-happen) — so reordering such a read relative to other
   code, or across an [Event], cannot change observable behaviour: there
   is no trap whose occurrence or source-map attribution we could move.

   Abnormal exits: an [e] that is not [effect_free] may leave its
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

(* Reads of a sub-expression, used at [may_cross_sibling] check points. *)
let reads_of_expr e = fst (rw_of_expr e)

(* Walker result for a single expression. [Clean] means "no occurrence
   of x in this expression; the caller may continue past, using
   [effect_free] to gate the path-clean state". *)
type walk_result =
  | Found of W.expression
  | Bail
  | Clean

(* [ctx] bundles the parameters that don't change during a sink attempt:
   the target variable, the expression to sink, the locals it reads and
   writes, and whether the expression itself is [effect_free].
   - [effect_free]: the [effect_free] predicate of the target, passed
     to [f].
   - [reads]: crossing a write to any of these would change [e]'s result.
   - [writes]: crossing a read of any of these would make the reader see
     the pre-sink value instead of the one [e] would have stored.
   When [e_effect_free] is [false] we may not cross any *evaluated*
   sub-expression or instruction even if it is itself [effect_free] —
   the path could read heap/global state that [e]'s side effects would
   change.
   - [e_may_exit]: [e] may leave its position abnormally to code in
     this function (see "Abnormal exits" in the header), so it must not
     cross a [local.set].
   - [substitute]: the target [local.get x] is the only read of [x], so
     we replace it by [e] rather than by [local.tee x e]. *)
type ctx =
  { x : Var.t
  ; e_to_sink : W.expression
  ; substitute : bool
  ; reads : Var.Set.t
  ; writes : Var.Set.t
  ; effect_free : W.expression -> bool
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
    incr total_budget_exhausted;
    false)
  else (
    ctx.budget <- ctx.budget - 1;
    true)

(* True iff [vs] (a set of *reads* by intermediate code) does not
   intersect [ctx.writes] — i.e. no variable that [e] writes is
   observed by the intermediate code. *)
let reads_disjoint_from_e_writes ctx vs =
  let exception Found in
  try
    Var.Set.iter (fun x -> if Var.Set.mem x ctx.writes then raise Found) vs;
    true
  with Found -> false

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
   [e] ran first. Three conditions must all hold:
   - [sibling] has no observable side effects;
   - either [e] is effect-free or [sibling] is trivially pure (no
     heap/global reads, no calls);
   - [sibling]'s reads are disjoint from [e]'s writes — otherwise
     [sibling] would read the pre-sink value of a local that [e]
     overwrites.  (The converse — [sibling] writing something [e]
     reads — is caught by the walker returning [Bail] at intermediate
     [LocalSet]/[LocalTee] and by the instruction-level continue
     checks in [try_sink_in_instr].) *)
let may_cross_sibling ctx sibling =
  ctx.effect_free sibling
  && (ctx.e_effect_free || trivially_pure sibling)
  && reads_disjoint_from_e_writes ctx (reads_of_expr sibling)

let rec walk_expr ctx (e : W.expression) =
  match e with
  | W.Const _ | GlobalGet _ | RefFunc _ | RefNull _ | Pop _ -> Clean
  | LocalGet y ->
      if Var.equal y ctx.x
      then
        Found
          (if ctx.substitute then ctx.e_to_sink else W.LocalTee (ctx.x, ctx.e_to_sink))
      else Clean
  | LocalTee (y, _) when Var.equal y ctx.x ->
      (* Another write to [x] — bail. *)
      Bail
  | LocalTee (y, e') -> (
      (* Reading [e'] first, then this tee writes [y]. Crossing this
         point means [e] would run after the tee. Bail if:
         - [y] is read by [e] (we'd read the tee's value instead of the
           pre-tee one), or
         - [y] is written by [e] (the final value of [y] would change).

         This [y] check is only a partial guard; it does NOT account for
         [e']'s own effects or for [e'] reading a local that [e] writes.
         The full safety comes from elsewhere: a [Clean] result here is
         only ever crossed by a caller that gates continuation on
         [may_cross_sibling], i.e. on [effect_free] of the enclosing
         expression — and [effect_free] is [false] for anything
         containing a [LocalTee]. So an expression with a tee can never
         actually be stepped over; the [y] check is belt-and-braces. *)
      match walk_expr ctx e' with
      | Found e'' -> Found (W.LocalTee (y, e''))
      | Bail -> Bail
      | Clean ->
          if Var.Set.mem y ctx.reads || Var.Set.mem y ctx.writes then Bail else Clean)
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
      if not (may_cross_sibling ctx e1 && tick ctx)
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
        | Clean ->
            if not (may_cross_sibling ctx a && tick ctx)
            then `Bail
            else loop (a :: acc) rest)
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
          if not (may_cross_sibling ctx e1 && tick ctx)
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
          if not (may_cross_sibling ctx e1 && tick ctx)
          then IBail
          else
            match walk_expr ctx e2 with
            | Found e2' -> IFound (W.ArraySet (ty, e1, e2', e3))
            | Bail -> IBail
            | Clean -> (
                if not (may_cross_sibling ctx e2 && tick ctx)
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
   with respect to [ctx.e_to_sink]'s potential side effects. An [Event]
   carries a source-map location for everything that follows it: moving
   an effectful [e_to_sink] past an event would re-attribute its call to
   the wrong source position, so we only cross events when [e] is itself
   effect-free. (A potentially-trapping but otherwise [effect_free] [e]
   may still cross an event: see the traps assumption in the header — we
   assume the trap never fires, so its attribution does not matter.) *)
let can_cross_instr ctx (instr : W.instruction) =
  match instr with
  | Nop -> true
  | Event _ -> ctx.e_effect_free
  | Drop e | Push e -> may_cross_sibling ctx e
  | LocalSet (y, e) ->
      (not ctx.e_may_exit)
      && may_cross_sibling ctx e
      && (not (Var.Set.mem y ctx.reads))
      && not (Var.Set.mem y ctx.writes)
  | _ -> false

let try_sink_in_list ctx instrs =
  let rec loop acc = function
    | [] -> None
    | instr :: rest -> (
        match try_sink_in_instr ctx instr with
        | IFound instr' -> Some (List.rev_append acc (instr' :: rest))
        | IBail -> None
        | IClean ->
            if can_cross_instr ctx instr && tick ctx
            then loop (instr :: acc) rest
            else None)
  in
  loop [] instrs

(* [in_try]: we are inside the body of a [Try], so an exception raised
   here may reach a handler of this function. [reads] and [writes]
   count the accesses to each local; [removed] collects the locals we
   no longer access at all. *)
type env =
  { effect_free : W.expression -> bool
  ; in_try : bool
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
      let e_effect_free = env.effect_free e in
      let substitute = count env.reads x = 1 && may_remove x in
      let ctx =
        { x
        ; e_to_sink = e
        ; substitute
        ; reads
        ; writes
        ; effect_free = env.effect_free
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

let f ~effect_free ~locals instrs =
  let t = Timer.make () in
  incr total_calls;
  let reads = Var.Hashtbl.create 64 in
  let writes = Var.Hashtbl.create 64 in
  let incr tbl x = Var.Hashtbl.replace tbl x (count tbl x + 1) in
  Wasm_traverse.iter_locals ~read:(incr reads) ~write:(incr writes) instrs;
  let env =
    { effect_free; in_try = false; reads; writes; removed = Var.Hashtbl.create 16 }
  in
  let instrs = transform_instrs env instrs in
  let locals =
    if Var.Hashtbl.length env.removed = 0
    then locals
    else List.filter locals ~f:(fun (x, _) -> not (Var.Hashtbl.mem env.removed x))
  in
  total_time := !total_time +. Timer.get t;
  locals, instrs
