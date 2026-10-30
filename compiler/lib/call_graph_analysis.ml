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

open! Stdlib
open Code

let debug = Debug.find "call-graph"

let times = Debug.find "times"

let get_approx info x =
  (* Specialization can add some variables *)
  if Var.idx x < Var.Tbl.length info.Global_flow.info_approximation
  then Var.Tbl.get info.Global_flow.info_approximation x
  else Top

let block_deps ~info ~non_escaping ~ambiguous ~blocks pc =
  let block = Addr.Map.find pc blocks in
  List.iter block.body ~f:(fun i ->
      match i with
      | Let (_, Apply { f; exact; _ }) -> (
          match get_approx info f with
          | Top -> ()
          | Values { known; others } ->
              if (not exact) || others || Var.Set.compare_cardinal_with known 1 > 0
              then
                Var.Set.iter
                  (fun x ->
                    Var.Hashtbl.replace ambiguous x ();
                    (* Double translation: the CPS version may be called as well *)
                    Option.iter
                      ~f:(fun c -> Var.Hashtbl.replace ambiguous c ())
                      (Var.Hashtbl.find_opt info.Global_flow.info_cps_versions x))
                  known;
              if debug ()
              then
                Format.eprintf "CALL others:%b known:%d@." others (Var.Set.cardinal known)
          )
      | Let (x, Closure _) -> (
          match get_approx info x with
          | Top -> ()
          | Values { known; others } ->
              if
                Var.Set.compare_cardinal_with known 1 = 0
                && (not others)
                && Var.Set.mem x known
              then (
                let may_escape = Var.ISet.mem info.Global_flow.info_may_escape x in
                if debug () then Format.eprintf "CLOSURE may-escape:%b@." may_escape;
                if not may_escape then Var.Hashtbl.replace non_escaping x ()))
      | Let (_, (Prim _ | Block _ | Constant _ | Field _ | Special _))
      | Event _ | Assign _ | Set_field _ | Offset_ref _ | Array_set _ -> ())

type t = { unambiguous_non_escaping : unit Var.Hashtbl.t }

let direct_calls_only info f =
  Config.Flag.optcall () && Var.Hashtbl.mem info.unambiguous_non_escaping f

(* The function called at a call site, when it is known and either
   only called directly, or [wrappable]: then, the call site is a
   direct call, while other uses of the function go through a wrapper.
   A function only called directly is never one of several possible
   callees: such call sites are ambiguous (see [block_deps]). As in
   [Global_flow.get_unique_closure], blocks are ignored, since they
   cannot be called. *)
let known_callee info call_info ~wrappable exact f =
  match get_approx info f with
  | Top -> None
  | Values { known; others } ->
      if exact && not others
      then
        let closures =
          Var.Set.filter
            (fun g ->
              match info.Global_flow.info_defs.(Var.idx g) with
              | Expr (Closure _) -> true
              | Expr _ | Phi _ -> false)
            known
        in
        if Var.Set.compare_cardinal_with closures 1 = 0
        then
          let g = Var.Set.choose closures in
          if direct_calls_only call_info g || wrappable g then Some g else None
        else None
      else None

let propagate nodes edges eligible =
  let rec propagate n =
    List.iter
      ~f:(fun n' ->
        if (not (Var.Hashtbl.mem nodes n')) && eligible n'
        then (
          Var.Hashtbl.add nodes n' ();
          propagate n'))
      (Var.Hashtbl.find_all edges n)
  in
  (* [nodes] is extended during the propagation: iterate over a copy *)
  List.iter ~f:propagate (Var.Hashtbl.fold (fun n () l -> n :: l) nodes [])

let call_graph p info call_info ~wrappable ~tail_calls eligible =
  let under_handler = Var.Hashtbl.create 16 in
  let callees = Var.Hashtbl.create 16 in
  let callers = Var.Hashtbl.create 16 in
  let has_tail_calls = Var.Hashtbl.create 16 in
  let tail_callers = Var.Hashtbl.create 16 in
  let rec traverse name_opt pc visited nesting =
    if not (Addr.Set.mem pc visited)
    then (
      let visited = Addr.Set.add pc visited in
      let block = Addr.Map.find pc p.blocks in
      List.iter block.body ~f:(fun i ->
          match i with
          | Let (_, Apply { f; exact; _ }) -> (
              match known_callee info call_info ~wrappable exact f with
              | None -> ()
              | Some g ->
                  if nesting > 0
                  then Var.Hashtbl.replace under_handler g ()
                  else
                    Option.iter
                      ~f:(fun f ->
                        Var.Hashtbl.add callees f g;
                        Var.Hashtbl.add callers g f)
                      name_opt)
          | Let (_, (Closure _ | Prim _ | Block _ | Constant _ | Field _ | Special _))
          | Event _ | Assign _ | Set_field _ | Offset_ref _ | Array_set _ -> ());
      Code.fold_children
        p.blocks
        pc
        (fun pc' visited ->
          let nesting =
            match block.branch with
            | Pushtrap ((body_pc, _), _, _) when pc' = body_pc -> nesting + 1
            | Poptrap _ -> nesting - 1
            | _ -> nesting
          in
          traverse name_opt pc' visited nesting)
        visited)
    else visited
  in
  let find_tail_calls f pc =
    Code.traverse
      { fold = Code.fold_children }
      (fun pc () ->
        let block = Addr.Map.find pc p.blocks in
        match block.branch with
        | Return x -> (
            match last_instr block.body with
            | Some (Let (x', Apply { f = g; exact; _ })) when Code.Var.equal x x' -> (
                match known_callee info call_info ~wrappable exact g with
                | None -> Var.Hashtbl.replace has_tail_calls f ()
                | Some g -> Var.Hashtbl.add tail_callers g f)
            | _ -> ())
        | _ -> ())
      pc
      p.blocks
      ()
  in
  fold_closures
    p
    (fun name_opt _ (pc, _) _ () ->
      if tail_calls then Option.iter ~f:(fun f -> find_tail_calls f pc) name_opt;
      ignore (traverse name_opt pc Addr.Set.empty 0))
    ();
  propagate has_tail_calls tail_callers eligible;
  under_handler, callers, callees, has_tail_calls

(* Raising functions return null instead of throwing an exception.
   A tail call from a non-raising function to a raising function is
   thus not a tail call anymore, since the null value needs to be
   turned into an exception. This only adds a bounded overhead, as
   such a call is never part of a cycle of tail calls: along a cycle,
   all functions have the same return type (hence are all eligible or
   not), [has_tail_calls] is propagated to all functions (through
   [tail_callers]), [under_handler] as well (through [callees]) if no
   function has unknown tail calls, and finally the raising property
   (through [callers]).

   A raising function which can also be called through a closure is
   wrapped: the wrapper converts the null value into an exception, so
   the call from the wrapper to the function is not a tail call
   either. But a cycle of tail calls going through the wrapper
   contains an unknown tail call, so [has_tail_calls] is propagated to
   all functions of the cycle, which are thus not raising.

   Without tail calls ([tail_calls] is false), none of this matters:
   a call to a raising function is never a tail call to begin with. *)
let raising_functions p info call_info ~wrappable ~tail_calls eligible =
  (* Only closures can be called directly *)
  let wrappable f =
    wrappable f
    && (not (direct_calls_only call_info f))
    && Var.idx f < Array.length info.Global_flow.info_defs
    &&
    match info.Global_flow.info_defs.(Var.idx f) with
    | Expr (Closure _) -> true
    | Expr _ | Phi _ -> false
  in
  let under_handler, callers, callees, has_tail_calls =
    call_graph p info call_info ~wrappable ~tail_calls eligible
  in
  propagate under_handler callees (fun f ->
      eligible f && not (Var.Hashtbl.mem has_tail_calls f));
  let h = Var.Hashtbl.create 16 in
  let eligible f =
    eligible f
    && Var.Hashtbl.mem under_handler f
    && not (Var.Hashtbl.mem has_tail_calls f)
  in
  Code.fold_closures
    p
    (fun name_opt _params (pc, _) _ () ->
      match name_opt with
      | None -> ()
      | Some name ->
          if
            (direct_calls_only call_info name || wrappable name)
            && eligible name
            && Code.raises_outside_handlers p.blocks pc
          then Var.Hashtbl.add h name ())
    ();
  propagate h callers eligible;
  if debug () then Format.eprintf " raising functions:%d@." (Var.Hashtbl.length h);
  h

let f p info =
  let t = Timer.make () in
  let non_escaping = Var.Hashtbl.create 128 in
  let ambiguous = Var.Hashtbl.create 128 in
  fold_closures
    p
    (fun _ _ (pc, _) _ () ->
      traverse
        { fold = Code.fold_children }
        (fun pc () -> block_deps ~info ~non_escaping ~ambiguous ~blocks:p.blocks pc)
        pc
        p.blocks
        ())
    ();
  if debug ()
  then Format.eprintf "SUMMARY non-escaping:%d" (Var.Hashtbl.length non_escaping);
  Var.Hashtbl.iter (fun x () -> Var.Hashtbl.remove non_escaping x) ambiguous;
  (* Double translation: both versions of a function share their closure,
     which holds the code pointers of both versions or none of them. This
     holds since the CPS version is only reachable through the direct-style
     one: it is ambiguous (see above) or escapes (see [Global_flow]) when
     the direct-style version does. *)
  Var.Hashtbl.iter
    (fun d c ->
      assert (Bool.equal (Var.Hashtbl.mem non_escaping d) (Var.Hashtbl.mem non_escaping c)))
    info.Global_flow.info_cps_versions;
  if debug ()
  then Format.eprintf " unambiguous-non-escaping:%d@." (Var.Hashtbl.length non_escaping);
  if times () then Format.eprintf "  call graph analysis: %a@." Timer.print t;
  { unambiguous_non_escaping = non_escaping }
