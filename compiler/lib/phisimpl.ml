(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
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
open! Stdlib

let times = Debug.find "times"

let stats = Debug.find "stats"

let debug_stats = Debug.find "stats-debug"

open Code

(****)

(* Only block parameters and assigned variables can be replaced, so
   they are the only variables we need to consider. We collect the
   variables each of them may receive. *)

let add_def vars defs x y =
  Var.ISet.add vars x;
  let idx = Var.idx x in
  defs.(idx) <- Var.Set.add y defs.(idx)

let rec arg_defs vars defs params args =
  match params, args with
  | x :: params, y :: args ->
      add_def vars defs x y;
      arg_defs vars defs params args
  | [], [] -> ()
  | _ -> assert false

let cont_defs blocks vars defs (pc, args) =
  let block = Addr.Map.find pc blocks in
  arg_defs vars defs block.params args

let program_defs { blocks; _ } =
  let vars = Var.ISet.empty () in
  let defs = Array.make (Var.count ()) Var.Set.empty in
  Addr.Map.iter
    (fun _pc block ->
      List.iter block.body ~f:(fun i ->
          match i with
          | Let (_, Closure (_, cont, _)) -> cont_defs blocks vars defs cont
          | Let (_, (Constant _ | Apply _ | Prim _ | Special _ | Block _ | Field _)) -> ()
          | Assign (x, y) -> add_def vars defs x y
          | Event _ | Set_field _ | Array_set _ | Offset_ref _ -> ());
      match block.branch with
      | Return _ | Raise _ | Stop -> ()
      | Branch cont | Poptrap cont -> cont_defs blocks vars defs cont
      | Cond (_, cont1, cont2) ->
          cont_defs blocks vars defs cont1;
          cont_defs blocks vars defs cont2
      | Switch (_, a1) -> Array.iter a1 ~f:(fun cont -> cont_defs blocks vars defs cont)
      | Pushtrap (cont, _, cont_h) ->
          cont_defs blocks vars defs cont_h;
          cont_defs blocks vars defs cont)
    blocks;
  vars, defs

(* [deps.(y)] contains the variables which may receive [y]. Only the
   variables which may be replaced matter. *)
let program_deps vars defs =
  let deps = Array.make (Var.count ()) Var.Set.empty in
  Var.ISet.iter
    (fun x ->
      Var.Set.iter
        (fun y ->
          if Var.ISet.mem vars y
          then
            let idx = Var.idx y in
            deps.(idx) <- Var.Set.add x deps.(idx))
        defs.(Var.idx x))
    vars;
  deps

let rec repr' reprs x acc =
  let idx = Var.idx x in
  match reprs.(idx) with
  | None -> x, acc
  | Some y -> repr' reprs y (x :: acc)

let repr reprs x =
  let last, l = repr' reprs x [] in
  List.iter l ~f:(fun v -> reprs.(Var.idx v) <- Some last);
  last

(* Replace [x] by [y] if [y] is the only variable [x] may receive,
   besides [x] itself (a loop). If [x] only receives itself, it is in
   unreachable code: replacing it by itself would make [repr] loop. *)
let propagate vars deps defs reprs x =
  let idx = Var.idx x in
  let s =
    Var.Set.fold
      (fun y s ->
        let y = repr reprs y in
        if Var.equal y x then s else Var.Set.add y s)
      defs.(idx)
      Var.Set.empty
  in
  defs.(idx) <- s;
  match Var.Set.to_list_bounded 1 s with
  | Some [ y ] ->
      (if Var.ISet.mem vars y
       then
         let yidx = Var.idx y in
         deps.(yidx) <- Var.Set.union deps.(yidx) deps.(idx));
      reprs.(idx) <- Some y;
      true
  | _ -> false

(* Each variable is replaced at most once: we check every variable,
   then only revisit the variables which may receive a variable that
   has just been replaced. *)
let solve vars deps defs =
  let reprs = Array.make (Var.count ()) None in
  let stack = ref [] in
  let check x =
    if Option.is_none reprs.(Var.idx x) && propagate vars deps defs reprs x
    then stack := x :: !stack
  in
  Var.ISet.iter check vars;
  let rec loop () =
    match !stack with
    | [] -> ()
    | x :: rem ->
        stack := rem;
        Var.Set.iter check deps.(Var.idx x);
        loop ()
  in
  loop ();
  reprs

let f p =
  let previous_p = p in
  Code.invariant p;
  let t = Timer.make () in
  let t' = Timer.make () in
  let vars, defs = program_defs p in
  let deps = program_deps vars defs in
  if times () then Format.eprintf "    phi-simpl. 1: %a@." Timer.print t';
  let t' = Timer.make () in
  let reprs = solve vars deps defs in
  if times () then Format.eprintf "    phi-simpl. 2: %a@." Timer.print t';
  let changed = ref false in
  Var.ISet.iter
    (fun x ->
      if Option.is_some reprs.(Var.idx x)
      then (
        changed := true;
        Code.Var.propagate_name x (repr reprs x)))
    vars;
  let need_stats = stats () || debug_stats () in
  let count_uniq = ref 0 in
  let count_seen = BitSet.create' (if need_stats then Var.count () else 0) in
  let subst v1 =
    let idx1 = Code.Var.idx v1 in
    let v2 =
      match reprs.(idx1) with
      | None -> v1
      | Some _ -> repr reprs v1
    in
    if Code.Var.equal v1 v2
    then v1
    else (
      if need_stats && not (BitSet.mem count_seen idx1)
      then (
        incr count_uniq;
        BitSet.set count_seen idx1);
      v2)
  in
  let p = if !changed then Subst.Excluding_Binders.program subst p else p in
  if times () then Format.eprintf "  phi-simpl.: %a@." Timer.print t;
  if stats () then Format.eprintf "Stats - phi updates: %d@." !count_uniq;
  if debug_stats () then Code.check_updates ~name:"phi" previous_p p ~updates:!count_uniq;
  Code.invariant p;
  p
