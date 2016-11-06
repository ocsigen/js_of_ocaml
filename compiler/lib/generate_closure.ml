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

open Code

let debug_tc = Option.Debug.find "gen_tc"

type closure_info = {
  f_name  : Code.Var.t;
  args    : Code.Var.t list;
  cont    : Code.cont;
  tc      : Code.AddrSet.t Code.VarMap.t
}

type 'a int_ext = {
  int : 'a;
  ext : 'a;
}

module SCC = Jsoo_strongly_connected_components.Make(struct
    include Var
    module Map = VarMap
    module Set = VarSet
  end)

let add_multi k v map =
  let set =
    try VarMap.find k map
    with Not_found -> AddrSet.empty
  in
  VarMap.add k (AddrSet.add v set) map

let rec tailcall pc blocks visited tc =
  if AddrSet.mem pc visited
  then visited, tc
  else
    let visited = AddrSet.add pc visited in
    let block = AddrMap.find pc blocks in
    let tc_opt = match block.branch with
      | Return x ->
        begin match Util.last block.body with
        | Some (Let (y, Apply (z, _, true))) when Code.Var.compare x y = 0 ->
          Some (add_multi z pc tc)
        | None -> None
        | Some _ -> None
        end
      | _ -> None
    in
    match tc_opt with
    | Some tc -> visited, tc
    | None  ->
      Code.fold_children blocks pc
        (fun pc (visited,tc) -> tailcall pc blocks visited tc)
        (visited, tc)

let rec collect_closures blocks l =
  match l with
  | Let (f_name, Closure (args, ((pc,_) as cont))) :: rem ->
    let tc = snd (tailcall pc blocks AddrSet.empty VarMap.empty) in
    let l,rem = collect_closures blocks rem in
    {f_name; args; cont; tc} :: l, rem
  | rem -> [], rem

let group_closures closures =
  let names =
    List.fold_left (fun names x -> VarSet.add x.f_name names) VarSet.empty closures
  in
  let closures_map =
    List.fold_left (fun closures_map x -> VarMap.add x.f_name x closures_map) VarMap.empty closures
  in
  let graph =
    List.fold_left (fun graph x ->
      let tc = VarMap.fold (fun x _ tc -> VarSet.add x tc) x.tc VarSet.empty in
      let tc = VarSet.inter names tc in
      VarMap.add x.f_name tc graph) VarMap.empty closures
  in
  closures_map, SCC.connected_components_sorted_from_roots_to_leaf graph

module Trampoline = struct

  let direct_call_block block ~counter ~x ~f ~args =
    let counter_plus_1 = Code.Var.fork counter in
    let return = Code.Var.fork x in
    { block with
      params = [];
      body =
        [
          Let (counter_plus_1,
               Prim (Extern "%int_add", [Pv counter; Pc (Int 1l)]));
          Let (return,
               Apply (f,counter_plus_1::args,true))
        ] ;
      branch = Return return;
    }

  let bounce_call_block block ~x ~f ~args =
    let return = Code.Var.fork x in
    let new_args = Code.Var.fresh () in
    { block with
      params = [];
      body =
        [
          Let (new_args,
               (Prim (Extern "%js_array",
                      (Pc (Int 0l) :: List.map (fun x -> Pv x) args))));
          Let (return,
               Prim (Extern "caml_trampoline_return", [Pv f; Pv new_args]))
        ] ;
      branch = Return return;
    }

  let wrapper_block f ~args ~counter =
    let result1 = Code.Var.fresh () in
    let result2 = Code.Var.fresh () in
    let block =
      { params = [];
        handler = None;
        body = [
          Let (counter, Constant (Int 0l));
          Let (result1, Apply (f, counter :: args, true));
          Let (result2, Prim (Extern "caml_trampoline", [Pv result1]))
        ];
        branch = Return result2
      }
    in
    block

  let wrapper_closure pc args = Closure (args, (pc, []))

  let f free_pc blocks closures_map component =
    match component with
    | SCC.No_loop id ->
      let ci    = VarMap.find id closures_map in
      let instr = Let (ci.f_name, Closure (ci.args, ci.cont)) in
      free_pc, blocks, {int = []; ext = [instr]}
    | SCC.Has_loop all ->
      if debug_tc ()
      then begin
        Format.eprintf "Detect cycles of size (%d).\n%!" (List.length all);
        Format.eprintf "%s\n%!"
          (String.concat ", "
             (List.map (fun x -> Var.to_string x) all));
      end;
      let all = List.map (fun id ->
        Code.Var.fresh_n "counter",
        VarMap.find id closures_map
      ) all in
      let blocks, free_pc, instrs, instrs_wrapper =
        List.fold_left (fun (blocks, free_pc, instrs, instrs_wrapper) (counter, ci) ->
          if debug_tc ()
          then Format.eprintf "Rewriting for %s\n%!" (Var.to_string ci.f_name);
          let new_f      = Code.Var.fork ci.f_name in
          let new_args   = List.map Code.Var.fork ci.args in
          let wrapper_pc = free_pc in
          let free_pc    = free_pc + 1 in

          let new_counter = Code.Var.fork counter in
          let wrapper_block = wrapper_block new_f ~args:new_args ~counter:new_counter in
          let blocks = AddrMap.add wrapper_pc wrapper_block blocks in

          let instr_wrapper = Let (ci.f_name, wrapper_closure wrapper_pc new_args) in
          let instr_real = Let (new_f, Closure (counter :: ci.args,  ci.cont)) in

          let counter_and_pc =
            List.fold_left (fun acc (counter,ci2) ->
              try
                let pcs = AddrSet.elements (VarMap.find ci.f_name ci2.tc) in
                List.map (fun x -> counter, x) pcs @ acc
              with Not_found -> acc
            ) [] all
          in
          let blocks, free_pc =
            List.fold_left (fun (blocks, free_pc) (counter, pc) ->
              if debug_tc ()
              then Format.eprintf "Rewriting tc in %d\n%!" pc;
              let block = AddrMap.find pc blocks in
              let direct_call_pc = free_pc in
              let bounce_call_pc = free_pc + 1 in
              let free_pc = free_pc + 2 in
              match List.rev block.body with
              | Let (x, Apply (f,args,true)) :: rem_rev ->
                assert (f = ci.f_name);
                let blocks =
                  AddrMap.add direct_call_pc
                    (direct_call_block block ~counter ~x ~f:new_f ~args) blocks
                in
                let blocks =
                  AddrMap.add bounce_call_pc
                    (bounce_call_block block ~x ~f:new_f ~args) blocks
                in
                let direct = Code.Var.fresh () in
                let branch = Cond (IsTrue, direct, (direct_call_pc, []), (bounce_call_pc, [])) in
                let last =
                  Let (direct, Prim (Lt,
                                     [Pv counter;
                                      Pc (Int (Int32.of_int (Option.Param.tailcall_max_depth ())))]))
                in
                let block = { block with body = List.rev (last :: rem_rev); branch = branch } in
                let blocks = AddrMap.remove pc blocks in
                AddrMap.add pc block blocks, free_pc
              | _ -> assert false
            ) (blocks, free_pc) counter_and_pc
          in
          blocks, free_pc, instr_real :: instrs, instr_wrapper :: instrs_wrapper
        ) (blocks, free_pc, [], []) all
      in
      free_pc, blocks, {int = instrs; ext = instrs_wrapper}
end

module Ident = struct
  let f free_pc blocks closures_map component =
    match component with
    | SCC.No_loop id ->
      let ci    = VarMap.find id closures_map in
      let instr = Let (ci.f_name, Closure (ci.args, ci.cont)) in
      free_pc, blocks, { int = []; ext = [instr] }
    | SCC.Has_loop ids ->
      let instrs =
        List.map (fun id ->
          let ci    = VarMap.find id closures_map in
          let instr = Let (ci.f_name, Closure (ci.args, ci.cont)) in
          instr
        ) ids
      in
      free_pc, blocks, {int = []; ext = instrs}
end

let rewrite_tc free_pc blocks closures_map component =
  let open Option.Param in
  match tailcall_optim () with
  | TcNone       -> Ident.f      free_pc blocks closures_map component
  | TcTrampoline -> Trampoline.f free_pc blocks closures_map component

let rewrite_mutable free_pc blocks mutated_vars rewrite_list
      { int = closures_intern
      ; ext = closures_extern } =
  let internal_and_external = closures_intern @ closures_extern in
  assert (closures_extern <> []);
  let all_mut, names = List.fold_left (fun (all_mut, names) i ->
    match i with
    | Let (x, Closure (_, (pc, _))) ->
      let all_mut =
        try VarSet.union all_mut (AddrMap.find pc mutated_vars)
        with Not_found -> all_mut
      in
      let names = VarSet.add x names in
      all_mut, names
    | _ -> assert false) (VarSet.empty, VarSet.empty) (internal_and_external)
  in
  let vars = VarSet.elements (VarSet.diff all_mut names) in
  if vars = []
  then free_pc, blocks, internal_and_external
  else
    match internal_and_external with
    | [Let (x, Closure (params, (pc, pc_args )))] ->
      let new_pc = free_pc in
      let free_pc = free_pc + 1 in
      let closure = Code.Var.fork x in
      let args = List.map Code.Var.fork vars in

      let new_x = Code.Var.fork x in
      let mapping =
        Jsoo_subst.from_map
          (Jsoo_subst.build_mapping (x :: vars) (new_x :: args))
      in
      rewrite_list := (mapping, pc) :: !rewrite_list;
      let new_block =
        { params = [];
          handler = None;
          body = [Let (new_x, Closure (params, (pc, List.map mapping pc_args)))];
          branch = Return new_x }
      in
      let blocks = AddrMap.add new_pc new_block blocks in
      let body =
        [ Let (closure, Closure (args, (new_pc, [])))
        ; Let (x, Apply (closure, vars, true))
        ]
      in
      free_pc, blocks, body
    | _ ->
      let new_pc = free_pc  in
      let free_pc = free_pc + 1 in
      let closure = Code.Var.fresh_n "closures" in
      let closure' = Code.Var.fresh_n "closures" in
      let b = Code.Var.fresh_n "block" in
      let args = List.map Code.Var.fork vars in

      let pcs = List.map (function
        | Let (_, Closure (_,(pc,_))) -> pc
        | _ -> assert false) (internal_and_external) in
      let old_xs = List.map (function
        | Let (x, Closure _) -> x
        | _ -> assert false) closures_extern in
      let new_xs = List.map (Code.Var.fork) old_xs in
      let mapping =
        Jsoo_subst.from_map
          (Jsoo_subst.build_mapping (old_xs @ vars) (new_xs @ args))
      in
      rewrite_list := (List.map (fun pc -> (mapping, pc)) pcs) @ !rewrite_list;

      let new_block =
        let proj =
          List.map2 (fun cl new_x ->
            match cl with
            | Let (_, Closure (params, (pc, pc_args ))) ->
              Let (new_x, Closure (params, (pc, List.map mapping pc_args)))
            | _ -> assert false
          ) closures_extern new_xs
        in
        { params = [];
          handler = None;
          body =
            closures_intern
            @ proj
            @ [
              Let (b, Block (0, Array.of_list new_xs))
            ];
          branch = Return b }
      in

      let blocks = AddrMap.add new_pc new_block blocks in
      let body =
        [ Let (closure, Closure (args, (new_pc, [])))
        ; Let (closure', Apply (closure, vars, true))
        ] @
        List.mapi (fun i x ->
          match x with
          | Let (x, Closure _) -> Let (x, Field (closure', i))
          | _ -> assert false
        ) closures_extern
      in
      free_pc, blocks, body


let rec rewrite_closures mutated_vars rewrite_list free_pc blocks body : (int * _ * _ list) =
  match body with
  | Let (_, Closure _) :: _ ->
    let closures,rem = collect_closures blocks body in
    let closures_map, components = group_closures closures in
    let free_pc, blocks, closures =
      List.fold_left (fun (free_pc, blocks, acc) component ->
        let free_pc, blocks, closures = rewrite_tc free_pc blocks closures_map component in
        let free_pc, blocks, intrs = rewrite_mutable free_pc blocks mutated_vars rewrite_list closures in
        free_pc, blocks, (intrs :: acc)) (free_pc, blocks, []) (Array.to_list components)
    in
    let free_pc, blocks, rem = rewrite_closures mutated_vars rewrite_list free_pc blocks rem in
    free_pc, blocks, List.flatten closures @ rem
  | i :: rem ->
    let free_pc, blocks, rem = rewrite_closures mutated_vars rewrite_list free_pc blocks rem in
    free_pc, blocks, i :: rem
  | [] -> free_pc, blocks, []

let f ((pc, blocks, free_pc) as p) : Code.program =
  Code.invariant p;
  let mutated_vars = Freevars.f p in
  let rewrite_list = ref [] in
  let blocks,free_pc =
    AddrMap.fold
      (fun pc _ (blocks,free_pc) ->
         (* make sure we have the latest version *)
         let block = AddrMap.find pc blocks in
         let free_pc, blocks, body = rewrite_closures mutated_vars rewrite_list free_pc blocks block.body in
         AddrMap.add pc { block with body } blocks, free_pc
      )
      blocks (blocks,free_pc)
  in
  (* Code.invariant (pc, blocks, free_pc); *)
  let p = List.fold_left (fun program (mapping,pc) ->
    Jsoo_subst.cont mapping pc program
  ) (pc, blocks, free_pc) !rewrite_list
  in
  Code.invariant p;
  p
