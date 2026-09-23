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
   Reuse the result of a cast of a local instead of casting it again.

   Code generation casts a variable each time it is used at a more
   precise type: [ref.cast (ref $block) (local.get x)] for each access
   to an array, for instance. V8's optimizing compiler removes the
   redundant casts, but its baseline compiler (Liftoff) does not, and
   code that runs once (such as the toplevel function of a module) is
   never optimized: V8 has no on-stack replacement for Wasm.

   We walk the function in evaluation order, keeping track of the casts
   available at each point. The first [ref.cast T (local.get x)] becomes
   [local.tee t (ref.cast T (local.get x))], with [t] a fresh local of
   type [T]; a later cast of [x] to [T] becomes [local.get t], as long as
   [x] has not been written in between.

   Availability:
   - [x] is written by a [local.set x] or a [local.tee x]. Each write
     bumps a version number of [x]; a cast is available only if [x]
     still has the version it had when the cast was performed.
   - A cast performed inside a block, a branch of an [if] or a [try]
     body is forgotten at the end of this construct: it does not
     dominate the code that follows. This also keeps each use of [t]
     inside the block where [t] is set, as required by the validation
     rules for non-nullable locals.
   - A loop body starts with no available cast: the loop entry is also
     reached from the end of the body, where [x] may have been written.
     A cast is thus reused within one iteration only.
   - Code generation often copies a variable before using it: [x] is
     cast as [ref.cast T (local.tee z (local.get x))], then used as
     [z]. After such a copy, and as long as neither [z] nor [x] is
     written, the casts of [z] are the casts of [x]. A cast of the copy
     whose result is available becomes
     [(local.set z (local.get x)) (local.get t)], or just [local.get t]
     when nothing else reads [z].
   - Nothing special is needed for branches out of a straight-line
     sequence ([br_if], [br_on_cast], ...): the code that follows is
     still dominated by what came before, and their targets are the end
     of an enclosing construct.

   A cast whose result is never reused should stay as it is, but we only
   know this once we have seen the rest of the function. So we walk the
   function twice: the first walk only finds which casts get reused, the
   second one rewrites the code. Both walks see the same available casts,
   since a cast that is not reused has no effect on the rest of the walk.
*)

open! Stdlib
module W = Wasm_ast
module Var = Code.Var

let times = Debug.find "times"

let stats = Debug.find "stats"

(* Aggregated statistics across all calls to [f], reported by
   [report_stats]. *)
let total_time = ref 0.

let total_calls = ref 0

let total_temps = ref 0

let total_reused = ref 0

let report_stats () =
  if !total_calls > 0
  then (
    if times () then Format.eprintf "  wasm cast reuse: %.2f@." !total_time;
    if stats ()
    then
      Format.eprintf
        "Stats - wasm cast reuse: %d functions, %d casts reused through %d locals@."
        !total_calls
        !total_reused
        !total_temps);
  total_time := 0.;
  total_calls := 0;
  total_temps := 0;
  total_reused := 0

(* A cast of a local [x] available at the current point. *)
type cast =
  { typ : W.ref_type
  ; version : int (* version of [x] when the cast was performed *)
  ; id : int (* rank of the cast in traversal order *)
  ; temp : Var.t option (* local holding the result, if it is reused *)
  }

(* [z] was set to [x] by a copy, when they had these versions. *)
type copy =
  { z_version : int
  ; x : Var.t
  ; x_version : int
  }

(* What is known at the current point: the casts of each local, and the
   copies. *)
type env =
  { casts : cast list Var.Map.t
  ; copies : copy Var.Map.t
  }

let empty_env = { casts = Var.Map.empty; copies = Var.Map.empty }

type state =
  { versions : int Var.Hashtbl.t
  ; mutable next_id : int
  ; reused : unit Int.Hashtbl.t
        (** First walk: filled with the casts that get reused. Second walk:
            the casts whose result we keep in a local. *)
  ; rewrite : bool (* second walk *)
  ; reads : int Var.Hashtbl.t
        (** Number of reads of each local that remain after the rewrite,
            counted during the first walk. *)
  ; mutable temps : (Var.t * W.value_type) list
  ; mutable reuse_count : int
  }

let read st x =
  if not st.rewrite
  then
    Var.Hashtbl.replace
      st.reads
      x
      (1 + Option.value ~default:0 (Var.Hashtbl.find_opt st.reads x))

let version st x = Option.value ~default:0 (Var.Hashtbl.find_opt st.versions x)

let write st x = Var.Hashtbl.replace st.versions x (version st x + 1)

(* The local whose casts are the casts of [x]: follow the copies that are
   still valid. *)
let rec source st env x =
  match Var.Map.find_opt x env.copies with
  | Some c when c.z_version = version st x && c.x_version = version st c.x ->
      source st env c.x
  | _ -> x

let copy st env z x =
  write st z;
  if Var.equal z x
  then env
  else
    { env with
      copies =
        Var.Map.add z { z_version = version st z; x; x_version = version st x } env.copies
    }

let find_cast st env x typ =
  let x = source st env x in
  match Var.Map.find_opt x env.casts with
  | None -> None
  | Some l ->
      List.find_opt l ~f:(fun c -> c.version = version st x && Poly.equal c.typ typ)

(* Cast [e] (which is [ref.cast typ (local.get x)], or [ref.cast typ
   (local.tee x _)] with the tee already performed) for the first time. *)
let first_cast st env x typ e =
  let x = source st env x in
  let id = st.next_id in
  st.next_id <- id + 1;
  let temp =
    if st.rewrite && Int.Hashtbl.mem st.reused id
    then (
      let t = Var.fresh () in
      st.temps <- (t, W.Ref typ) :: st.temps;
      Some t)
    else None
  in
  let c = { typ; version = version st x; id; temp } in
  let env =
    { env with
      casts =
        Var.Map.add
          x
          (c :: Option.value ~default:[] (Var.Map.find_opt x env.casts))
          env.casts
    }
  in
  ( (match temp with
    | Some t -> W.LocalTee (t, e)
    | None -> e)
  , env )

(* Reuse the cast [c]. [e] is the original expression, [e'] its
   replacement. *)
let reuse st c e e' =
  if not st.rewrite then Int.Hashtbl.replace st.reused c.id ();
  match c.temp with
  | Some t ->
      st.reuse_count <- st.reuse_count + 1;
      e' t
  | None -> e

let rec expr st env (e : W.expression) : W.expression * _ =
  match e with
  | Const _ | GlobalGet _ | Pop _ | RefFunc _ | RefNull _ -> e, env
  | LocalGet x ->
      read st x;
      e, env
  | RefCast (typ, LocalGet x) -> (
      match find_cast st env x typ with
      | Some c -> reuse st c e (fun t -> W.LocalGet t), env
      | None ->
          read st x;
          first_cast st env x typ e)
  | RefCast (typ, LocalTee (z, LocalGet x)) -> (
      read st x;
      let env = copy st env z x in
      match find_cast st env z typ with
      | Some c ->
          ( reuse st c e (fun t ->
                if Var.Hashtbl.mem st.reads z
                then W.Seq ([ LocalSet (z, LocalGet x) ], LocalGet t)
                else W.LocalGet t)
          , env )
      | None -> first_cast st env z typ e)
  | LocalTee (z, LocalGet x) ->
      read st x;
      e, copy st env z x
  | RefCast (typ, LocalTee (x, e')) ->
      let e', env = expr st env e' in
      write st x;
      first_cast st env x typ (RefCast (typ, LocalTee (x, e')))
  | LocalTee (x, e') ->
      let e', env = expr st env e' in
      write st x;
      LocalTee (x, e'), env
  (* A cast made in a branch or a block is forgotten at its end. *)
  | IfExpr (ty, cond, e1, e2) ->
      let cond, env = expr st env cond in
      let e1, _ = expr st env e1 in
      let e2, _ = expr st env e2 in
      IfExpr (ty, cond, e1, e2), env
  | BlockExpr (ty, l) ->
      let l, _ = instrs st env l in
      BlockExpr (ty, l), env
  | Try (ty, body, catches) ->
      let body, _ = instrs st env body in
      Try (ty, body, catches), env
  | _ ->
      Wasm_traverse.fold_map_expression
        ~expression:(expr st)
        ~instructions:(instrs st)
        env
        e

and instr st env (i : W.instruction) : W.instruction * _ =
  match i with
  | LocalSet (z, LocalGet x) ->
      read st x;
      i, copy st env z x
  | LocalSet (x, e) ->
      let e, env = expr st env e in
      write st x;
      LocalSet (x, e), env
  | If (ty, cond, l1, l2) ->
      let cond, env = expr st env cond in
      let l1, _ = instrs st env l1 in
      let l2, _ = instrs st env l2 in
      If (ty, cond, l1, l2), env
  | Block (ty, l) ->
      let l, _ = instrs st env l in
      Block (ty, l), env
  | Loop (ty, l) ->
      let l, _ = instrs st empty_env l in
      Loop (ty, l), env
  | _ ->
      Wasm_traverse.fold_map_instruction
        ~expression:(expr st)
        ~instructions:(instrs st)
        env
        i

and instrs st env l =
  let l, env =
    List.fold_left l ~init:([], env) ~f:(fun (acc, env) i ->
        let i, env = instr st env i in
        i :: acc, env)
  in
  List.rev l, env

let f ~locals body =
  let t = Timer.make () in
  incr total_calls;
  let walk ~rewrite reused reads =
    let st =
      { versions = Var.Hashtbl.create 16
      ; next_id = 0
      ; reused
      ; rewrite
      ; reads
      ; temps = []
      ; reuse_count = 0
      }
    in
    let body, _ = instrs st empty_env body in
    st, body
  in
  let st, _ = walk ~rewrite:false (Int.Hashtbl.create 16) (Var.Hashtbl.create 16) in
  let res =
    if Int.Hashtbl.length st.reused = 0
    then locals, body
    else
      let st, body = walk ~rewrite:true st.reused st.reads in
      total_temps := !total_temps + List.length st.temps;
      total_reused := !total_reused + st.reuse_count;
      locals @ List.rev st.temps, body
  in
  total_time := !total_time +. Timer.get t;
  res
