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
              then Var.Set.iter (fun x -> Var.Hashtbl.replace ambiguous x ()) known;
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

let callee_if_known info call_info exact f =
  match get_approx info f with
  | Top -> None
  | Values { known; others } ->
      if
        exact
        && (not others)
        && Var.Set.for_all (fun f -> direct_calls_only call_info f) known
      then Some (Var.Set.choose known)
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
  Var.Hashtbl.iter (fun n () -> propagate n) nodes

let call_graph p info call_info eligible =
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
              match get_approx info f with
              | Top -> ()
              | Values { known; others } ->
                  if
                    exact
                    && (not others)
                    && Var.Set.for_all (fun f -> direct_calls_only call_info f) known
                  then
                    if nesting > 0
                    then
                      Var.Set.iter
                        (fun f ->
                          (*                        Format.eprintf "BBB %a@." Code.Var.print f; *)
                          Var.Hashtbl.replace under_handler f ())
                        known
                    else
                      Option.iter
                        ~f:(fun f ->
                          Var.Set.iter
                            (fun g ->
                              Var.Hashtbl.add callees f g;
                              Var.Hashtbl.add callers g f)
                            known)
                        name_opt)
          | Let (_, (Closure _ | Prim _ | Block _ | Constant _ | Field _ | Special _))
          | Event _ | Assign _ | Set_field _ | Offset_ref _ | Array_set _ -> ());
      if nesting = 0
      then
        Option.iter
          ~f:(fun f ->
            Code.traverse
              { fold = Code.fold_children }
              (fun pc () ->
                let block = Addr.Map.find pc p.blocks in
                match block.branch with
                | Return x -> (
                    match last_instr block.body with
                    | Some (Let (x', Apply { f = g; exact; _ })) when Code.Var.equal x x'
                      -> (
                        match callee_if_known info call_info exact g with
                        | None -> Var.Hashtbl.replace has_tail_calls f ()
                        | Some g -> Var.Hashtbl.add tail_callers g f)
                    | _ -> ())
                | _ -> ())
              pc
              p.blocks
              ())
          name_opt;
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
  fold_closures
    p
    (fun name_opt _ (pc, _) _ () -> ignore (traverse name_opt pc Addr.Set.empty 0))
    ();
  propagate has_tail_calls tail_callers eligible;
  under_handler, callers, callees, has_tail_calls

let function_do_raise p pc =
  Code.traverse
    { fold = Code.fold_children_skip_try_body }
    (fun pc do_raise ->
      let block = Addr.Map.find pc p.blocks in
      do_raise
      ||
      match block.branch with
      | Raise _ -> true
      | _ -> false)
    pc
    p.blocks
    false

let raising_functions p info call_info eligible =
  let under_handler, callers, callees, has_tail_calls =
    call_graph p info call_info eligible
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
          if direct_calls_only call_info name && eligible name && function_do_raise p pc
          then Var.Hashtbl.add h name ())
    ();
  propagate h callers eligible;
  if false
  then
    Var.Hashtbl.iter
      (fun name () ->
        Format.eprintf "ZZZ %a %b@." Var.print name (Var.Hashtbl.mem under_handler name))
      h;
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
  if debug ()
  then Format.eprintf " unambiguous-non-escaping:%d@." (Var.Hashtbl.length non_escaping);
  if times () then Format.eprintf "  call graph analysis: %a@." Timer.print t;
  (*
  Var.Hashtbl.iter (fun f _ -> Format.eprintf "AAA %a@." Code.Var.print f) non_escaping;
*)
  let call_info = { unambiguous_non_escaping = non_escaping } in
  call_info

(*
- Optimize tail-calls
*)
