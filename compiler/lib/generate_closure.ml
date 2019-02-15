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
open Stdlib
open Code

let debug_tc = Debug.find "gen_tc"

type closure_info =
  { f_name : Code.Var.t
  ; args : Code.Var.t list
  ; cont : Code.cont
  ; tc : Code.Addr.Set.t Code.Var.Map.t }

type 'a int_ext =
  { int : 'a
  ; ext : 'a }

module SCC = Strongly_connected_components.Make (Var)

let add_multi k v map =
  let set = try Var.Map.find k map with Not_found -> Addr.Set.empty in
  Var.Map.add k (Addr.Set.add v set) map

let rec tailcall pc blocks visited tc =
  if Addr.Set.mem pc visited
  then visited, tc
  else
    let visited = Addr.Set.add pc visited in
    let block = Addr.Map.find pc blocks in
    let tc_opt =
      match block.branch with
      | Return x -> (
        match List.last block.body with
        | Some (Let (y, Apply (z, _, true))) when Code.Var.compare x y = 0 ->
            Some (add_multi z pc tc)
        | None -> None
        | Some _ -> None )
      | _ -> None
    in
    match tc_opt with
    | Some tc -> visited, tc
    | None ->
        Code.fold_children
          blocks
          pc
          (fun pc (visited, tc) -> tailcall pc blocks visited tc)
          (visited, tc)

let rec collect_closures blocks l =
  match l with
  | Let (f_name, Closure (args, ((pc, _) as cont))) :: rem ->
      let tc = snd (tailcall pc blocks Addr.Set.empty Var.Map.empty) in
      let l, rem = collect_closures blocks rem in
      {f_name; args; cont; tc} :: l, rem
  | rem -> [], rem

let group_closures closures =
  let names =
    List.fold_left closures ~init:Var.Set.empty ~f:(fun names x ->
        Var.Set.add x.f_name names )
  in
  let closures_map =
    List.fold_left closures ~init:Var.Map.empty ~f:(fun closures_map x ->
        Var.Map.add x.f_name x closures_map )
  in
  let graph =
    List.fold_left closures ~init:Var.Map.empty ~f:(fun graph x ->
        let tc = Var.Map.fold (fun x _ tc -> Var.Set.add x tc) x.tc Var.Set.empty in
        let tc = Var.Set.inter names tc in
        Var.Map.add x.f_name tc graph )
  in
  closures_map, SCC.connected_components_sorted_from_roots_to_leaf graph

module Trampoline = struct
  let direct_call_block block ~counter ~x ~f ~args =
    let counter_plus_1 = Code.Var.fork counter in
    let return = Code.Var.fork x in
    { block with
      params = []
    ; body =
        [ Let (counter_plus_1, Prim (Extern "%int_add", [Pv counter; Pc (Int 1l)]))
        ; Let (return, Apply (f, counter_plus_1 :: args, true)) ]
    ; branch = Return return }

  let bounce_call_block block ~x ~f ~args =
    let return = Code.Var.fork x in
    let new_args = Code.Var.fresh () in
    { block with
      params = []
    ; body =
        [ Let
            ( new_args
            , Prim (Extern "%js_array", Pc (Int 0l) :: List.map args ~f:(fun x -> Pv x))
            )
        ; Let (return, Prim (Extern "caml_trampoline_return", [Pv f; Pv new_args])) ]
    ; branch = Return return }

  let wrapper_block f ~args ~counter =
    let result1 = Code.Var.fresh () in
    let result2 = Code.Var.fresh () in
    let block =
      { params = []
      ; handler = None
      ; body =
          [ Let (counter, Constant (Int 0l))
          ; Let (result1, Apply (f, counter :: args, true))
          ; Let (result2, Prim (Extern "caml_trampoline", [Pv result1])) ]
      ; branch = Return result2 }
    in
    block

  let wrapper_closure pc args = Closure (args, (pc, []))

  let f free_pc blocks closures_map component =
    match component with
    | SCC.No_loop id ->
        let ci = Var.Map.find id closures_map in
        let instr = Let (ci.f_name, Closure (ci.args, ci.cont)) in
        free_pc, blocks, {int = []; ext = [instr]}
    | SCC.Has_loop all ->
        if debug_tc ()
        then (
          Format.eprintf "Detect cycles of size (%d).\n%!" (List.length all);
          Format.eprintf
            "%s\n%!"
            (String.concat ~sep:", " (List.map all ~f:(fun x -> Var.to_string x))) );
        let all =
          List.map all ~f:(fun id ->
              Code.Var.fresh_n "counter", Var.Map.find id closures_map )
        in
        let blocks, free_pc, instrs, instrs_wrapper =
          List.fold_left
            all
            ~init:(blocks, free_pc, [], [])
            ~f:(fun (blocks, free_pc, instrs, instrs_wrapper) (counter, ci) ->
              if debug_tc ()
              then Format.eprintf "Rewriting for %s\n%!" (Var.to_string ci.f_name);
              let new_f = Code.Var.fork ci.f_name in
              let new_args = List.map ci.args ~f:Code.Var.fork in
              let wrapper_pc = free_pc in
              let free_pc = free_pc + 1 in
              let new_counter = Code.Var.fork counter in
              let wrapper_block =
                wrapper_block new_f ~args:new_args ~counter:new_counter
              in
              let blocks = Addr.Map.add wrapper_pc wrapper_block blocks in
              let instr_wrapper = Let (ci.f_name, wrapper_closure wrapper_pc new_args) in
              let instr_real = Let (new_f, Closure (counter :: ci.args, ci.cont)) in
              let counter_and_pc =
                List.fold_left all ~init:[] ~f:(fun acc (counter, ci2) ->
                    try
                      let pcs = Addr.Set.elements (Var.Map.find ci.f_name ci2.tc) in
                      List.map pcs ~f:(fun x -> counter, x) @ acc
                    with Not_found -> acc )
              in
              let blocks, free_pc =
                List.fold_left
                  counter_and_pc
                  ~init:(blocks, free_pc)
                  ~f:(fun (blocks, free_pc) (counter, pc) ->
                    if debug_tc () then Format.eprintf "Rewriting tc in %d\n%!" pc;
                    let block = Addr.Map.find pc blocks in
                    let direct_call_pc = free_pc in
                    let bounce_call_pc = free_pc + 1 in
                    let free_pc = free_pc + 2 in
                    match List.rev block.body with
                    | Let (x, Apply (f, args, true)) :: rem_rev ->
                        assert (f = ci.f_name);
                        let blocks =
                          Addr.Map.add
                            direct_call_pc
                            (direct_call_block block ~counter ~x ~f:new_f ~args)
                            blocks
                        in
                        let blocks =
                          Addr.Map.add
                            bounce_call_pc
                            (bounce_call_block block ~x ~f:new_f ~args)
                            blocks
                        in
                        let direct = Code.Var.fresh () in
                        let branch =
                          Cond
                            (IsTrue, direct, (direct_call_pc, []), (bounce_call_pc, []))
                        in
                        let last =
                          Let
                            ( direct
                            , Prim
                                ( Lt
                                , [ Pv counter
                                  ; Pc
                                      (Int
                                         (Int32.of_int
                                            (Config.Param.tailcall_max_depth ()))) ] ) )
                        in
                        let block =
                          {block with body = List.rev (last :: rem_rev); branch}
                        in
                        let blocks = Addr.Map.remove pc blocks in
                        Addr.Map.add pc block blocks, free_pc
                    | _ -> assert false )
              in
              blocks, free_pc, instr_real :: instrs, instr_wrapper :: instrs_wrapper )
        in
        free_pc, blocks, {int = instrs; ext = instrs_wrapper}
end

module Ident = struct
  let f free_pc blocks closures_map component =
    match component with
    | SCC.No_loop id ->
        let ci = Var.Map.find id closures_map in
        let instr = Let (ci.f_name, Closure (ci.args, ci.cont)) in
        free_pc, blocks, {int = []; ext = [instr]}
    | SCC.Has_loop ids ->
        let instrs =
          List.map ids ~f:(fun id ->
              let ci = Var.Map.find id closures_map in
              let instr = Let (ci.f_name, Closure (ci.args, ci.cont)) in
              instr )
        in
        free_pc, blocks, {int = []; ext = instrs}
end

let rewrite_tc free_pc blocks closures_map component =
  let open Config.Param in
  match tailcall_optim () with
  | TcNone -> Ident.f free_pc blocks closures_map component
  | TcTrampoline -> Trampoline.f free_pc blocks closures_map component

let rewrite_mutable
    free_pc
    blocks
    mutated_vars
    rewrite_list
    {int = closures_intern; ext = closures_extern} =
  let internal_and_external = closures_intern @ closures_extern in
  assert (closures_extern <> []);
  let all_mut, names =
    List.fold_left
      internal_and_external
      ~init:(Var.Set.empty, Var.Set.empty)
      ~f:(fun (all_mut, names) i ->
        match i with
        | Let (x, Closure (_, (pc, _))) ->
            let all_mut =
              try Var.Set.union all_mut (Addr.Map.find pc mutated_vars)
              with Not_found -> all_mut
            in
            let names = Var.Set.add x names in
            all_mut, names
        | _ -> assert false )
  in
  let vars = Var.Set.elements (Var.Set.diff all_mut names) in
  if vars = []
  then free_pc, blocks, internal_and_external
  else
    match internal_and_external with
    | [Let (x, Closure (params, (pc, pc_args)))] ->
        let new_pc = free_pc in
        let free_pc = free_pc + 1 in
        let closure = Code.Var.fork x in
        let args = List.map vars ~f:Code.Var.fork in
        let new_x = Code.Var.fork x in
        let mapping = Subst.from_map (Subst.build_mapping (x :: vars) (new_x :: args)) in
        rewrite_list := (mapping, pc) :: !rewrite_list;
        let new_block =
          { params = []
          ; handler = None
          ; body = [Let (new_x, Closure (params, (pc, List.map pc_args ~f:mapping)))]
          ; branch = Return new_x }
        in
        let blocks = Addr.Map.add new_pc new_block blocks in
        let body =
          [ Let (closure, Closure (args, (new_pc, [])))
          ; Let (x, Apply (closure, vars, true)) ]
        in
        free_pc, blocks, body
    | _ ->
        let new_pc = free_pc in
        let free_pc = free_pc + 1 in
        let closure = Code.Var.fresh_n "closures" in
        let closure' = Code.Var.fresh_n "closures" in
        let b = Code.Var.fresh_n "block" in
        let args = List.map vars ~f:Code.Var.fork in
        let pcs =
          List.map internal_and_external ~f:(function
              | Let (_, Closure (_, (pc, _))) -> pc
              | _ -> assert false )
        in
        let old_xs =
          List.map closures_extern ~f:(function
              | Let (x, Closure _) -> x
              | _ -> assert false )
        in
        let new_xs = List.map old_xs ~f:Code.Var.fork in
        let mapping =
          Subst.from_map (Subst.build_mapping (old_xs @ vars) (new_xs @ args))
        in
        rewrite_list := List.map pcs ~f:(fun pc -> mapping, pc) @ !rewrite_list;
        let new_block =
          let proj =
            List.map2 closures_extern new_xs ~f:(fun cl new_x ->
                match cl with
                | Let (_, Closure (params, (pc, pc_args))) ->
                    Let (new_x, Closure (params, (pc, List.map pc_args ~f:mapping)))
                | _ -> assert false )
          in
          { params = []
          ; handler = None
          ; body = closures_intern @ proj @ [Let (b, Block (0, Array.of_list new_xs))]
          ; branch = Return b }
        in
        let blocks = Addr.Map.add new_pc new_block blocks in
        let body =
          [ Let (closure, Closure (args, (new_pc, [])))
          ; Let (closure', Apply (closure, vars, true)) ]
          @ List.mapi closures_extern ~f:(fun i x ->
                match x with
                | Let (x, Closure _) -> Let (x, Field (closure', i))
                | _ -> assert false )
        in
        free_pc, blocks, body

let rec rewrite_closures mutated_vars rewrite_list free_pc blocks body : int * _ * _ list
    =
  match body with
  | Let (_, Closure _) :: _ ->
      let closures, rem = collect_closures blocks body in
      let closures_map, components = group_closures closures in
      let free_pc, blocks, closures =
        List.fold_left
          (Array.to_list components)
          ~init:(free_pc, blocks, [])
          ~f:(fun (free_pc, blocks, acc) component ->
            let free_pc, blocks, closures =
              rewrite_tc free_pc blocks closures_map component
            in
            let free_pc, blocks, intrs =
              rewrite_mutable free_pc blocks mutated_vars rewrite_list closures
            in
            free_pc, blocks, intrs :: acc )
      in
      let free_pc, blocks, rem =
        rewrite_closures mutated_vars rewrite_list free_pc blocks rem
      in
      free_pc, blocks, List.flatten closures @ rem
  | i :: rem ->
      let free_pc, blocks, rem =
        rewrite_closures mutated_vars rewrite_list free_pc blocks rem
      in
      free_pc, blocks, i :: rem
  | [] -> free_pc, blocks, []

let f ((pc, blocks, free_pc) as p) : Code.program =
  Code.invariant p;
  let mutated_vars = Freevars.f p in
  let rewrite_list = ref [] in
  let blocks, free_pc =
    Addr.Map.fold
      (fun pc _ (blocks, free_pc) ->
        (* make sure we have the latest version *)
        let block = Addr.Map.find pc blocks in
        let free_pc, blocks, body =
          rewrite_closures mutated_vars rewrite_list free_pc blocks block.body
        in
        Addr.Map.add pc {block with body} blocks, free_pc )
      blocks
      (blocks, free_pc)
  in
  (* Code.invariant (pc, blocks, free_pc); *)
  let p =
    List.fold_left
      !rewrite_list
      ~init:(pc, blocks, free_pc)
      ~f:(fun program (mapping, pc) -> Subst.cont mapping pc program )
  in
  Code.invariant p; p
