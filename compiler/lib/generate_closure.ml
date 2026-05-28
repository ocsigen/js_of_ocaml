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
open Code

let debug_tc = Debug.find "gen_tc"

type cps_pair =
  { direct_c : Code.Var.t
  ; cps_c : Code.Var.t
  ; (* The [Let (cps_c, Closure …)] instruction, re-emitted verbatim. *)
    cps_code : Code.instr
  }

type closure_info =
  { f_name : Code.Var.t
  ; args : Code.Var.t list
  ; cont : Code.cont
  ; tc : Code.Addr.Set.t Code.Var.Map.t
  ; pos : int
  ; cloc : Parse_info.t option
  ; (* Under --effects=double-translation, the [Closure] instruction that
       binds [f_name]'s direct version is followed by a sibling [Closure] for
       the CPS version and a [caml_cps_closure] primitive pairing them. In
       that case, [f_name] is the public paired closure (the [x] of
       [Let x = caml_cps_closure(direct_c, cps_c)]), and this field records
       the names and body of the CPS half so trampolines can rewrite the
       triple. [None] in every other mode and for non-cps_needed closures. *)
    cps_pair : cps_pair option
  }

module SCC = Strongly_connected_components.Make (Var)

let add_multi k v map =
  Var.Map.update
    k
    (fun set -> Some (Addr.Set.add v (Option.value ~default:Addr.Set.empty set)))
    map

let rec collect_apply pc blocks visited tc =
  if Addr.Set.mem pc visited
  then visited, tc
  else
    let visited = Addr.Set.add pc visited in
    let block = Addr.Map.find pc blocks in
    let tc_opt =
      match block.branch with
      | Return x -> (
          match List.last block.body with
          | Some (Let (y, Apply { f; exact = true; _ })) when Code.Var.compare x y = 0 ->
              Some (add_multi f pc tc)
          | None -> None
          | Some _ -> None)
      | _ -> None
    in
    match tc_opt with
    | Some tc -> visited, tc
    | None ->
        Code.fold_children
          blocks
          pc
          (fun pc (visited, tc) -> collect_apply pc blocks visited tc)
          (visited, tc)

let rec collect_closures blocks l pos =
  match l with
  | Let (direct_c, Closure (args, ((pc, _) as cont), cloc))
    :: (Let (cps_c, Closure (_, _, _)) as cps_code)
    :: Let (x, Prim (Extern "caml_cps_closure", [ Pv d; Pv c ]))
    :: rem
    when Var.equal d direct_c && Var.equal c cps_c ->
      let _, tc = collect_apply pc blocks Addr.Set.empty Var.Map.empty in
      let l, rem = collect_closures blocks rem (succ pos) in
      { f_name = x
      ; args
      ; cont
      ; tc
      ; pos
      ; cloc
      ; cps_pair = Some { direct_c; cps_c; cps_code }
      }
      :: l
      , rem
  | Let (f_name, Closure (args, ((pc, _) as cont), cloc)) :: rem ->
      let _, tc = collect_apply pc blocks Addr.Set.empty Var.Map.empty in
      let l, rem = collect_closures blocks rem (succ pos) in
      { f_name; args; cont; tc; pos; cloc; cps_pair = None } :: l, rem
  | rem -> [], rem

let group_closures closures_map =
  let names =
    Var.Map.fold (fun _ x names -> Var.Set.add x.f_name names) closures_map Var.Set.empty
  in
  let graph =
    Var.Map.fold
      (fun _ x graph ->
        let calls = Var.Map.fold (fun x _ tc -> Var.Set.add x tc) x.tc Var.Set.empty in
        Var.Map.add x.f_name (Var.Set.inter names calls) graph)
      closures_map
      Var.Map.empty
  in
  SCC.connected_components_sorted_from_roots_to_leaf graph |> Array.to_list

type w =
  | One of
      { name : Code.Var.t
      ; code : Code.instr
      }
  | Wrapper of
      { name : Code.Var.t
      ; code : Code.instr
      ; wrapper : Code.instr
      }
  | Paired_one of
      { name : Code.Var.t
      ; direct_code : Code.instr
      ; cps_code : Code.instr
      ; pair_code : Code.instr
      }
  | Paired_wrapper of
      { name : Code.Var.t
      ; inner_direct_code : Code.instr
      ; wrapper_code : Code.instr
      ; cps_code : Code.instr
      ; pair_code : Code.instr
      }

let wrapper_closure pc args cloc = Closure (args, (pc, []), cloc)

module Trampoline = struct
  let direct_call_block ~counter ~x ~f ~args =
    let return = Code.Var.fork x in
    match counter with
    | None ->
        { params = []
        ; body = [ Let (return, Apply { f; args; exact = true }) ]
        ; branch = Return return
        }
    | Some counter ->
        let counter_plus_1 = Code.Var.fork counter in
        { params = []
        ; body =
            [ Let
                ( counter_plus_1
                , Prim (Extern "%int_add", [ Pv counter; Pc (Int Targetint.one) ]) )
            ; Let (return, Apply { f; args = counter_plus_1 :: args; exact = true })
            ]
        ; branch = Return return
        }

  let bounce_call_block ~x ~f ~args =
    let return = Code.Var.fork x in
    let new_args = Code.Var.fresh () in
    { params = []
    ; body =
        [ Let
            ( new_args
            , Prim
                ( Extern "%js_array"
                , Pc (Int Targetint.zero) :: List.map args ~f:(fun x -> Pv x) ) )
        ; Let (return, Prim (Extern "caml_trampoline_return", [ Pv f; Pv new_args ]))
        ]
    ; branch = Return return
    }

  let wrapper_block f ~args ~counter loc =
    let result1 = Code.Var.fresh () in
    let result2 = Code.Var.fresh () in
    let block =
      { params = []
      ; body =
          (match counter with
          | None ->
              [ Event loc
              ; Let (result1, Apply { f; args; exact = true })
              ; Event Parse_info.zero
              ; Let (result2, Prim (Extern "caml_trampoline", [ Pv result1 ]))
              ]
          | Some counter ->
              [ Event loc
              ; Let (counter, Constant (Int Targetint.zero))
              ; Let (result1, Apply { f; args = counter :: args; exact = true })
              ; Event Parse_info.zero
              ; Let (result2, Prim (Extern "caml_trampoline", [ Pv result1 ]))
              ])
      ; branch = Return result2
      }
    in
    block

  let has_loop free_pc blocks closures_map all =
    if debug_tc ()
    then (
      Format.eprintf "Detect cycles of size (%d).\n%!" (List.length all);
      Format.eprintf
        "%a\n%!"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
           Var.print)
        all);
    let tailcall_max_depth = Config.Param.tailcall_max_depth () in
    let all =
      List.map all ~f:(fun id ->
          ( (if tailcall_max_depth = 0 then None else Some (Code.Var.fresh_n "counter"))
          , Var.Map.find id closures_map ))
    in
    let blocks, free_pc, closures =
      List.fold_left
        all
        ~init:(blocks, free_pc, [])
        ~f:(fun (blocks, free_pc, closures) (counter, ci) ->
          if debug_tc ()
          then Format.eprintf "Rewriting for %a\n%!" Var.print ci.f_name;
          let new_f = Code.Var.fork ci.f_name in
          let new_args = List.map ci.args ~f:Code.Var.fork in
          let wrapper_pc = free_pc in
          let free_pc = free_pc + 1 in
          let new_counter = Option.map counter ~f:Code.Var.fork in
          let start_loc =
            let block = Addr.Map.find (fst ci.cont) blocks in
            match block.body with
            | Event loc :: _ -> loc
            | _ -> Parse_info.zero
          in
          let wrapper_block =
            wrapper_block new_f ~args:new_args ~counter:new_counter start_loc
          in
          let blocks = Addr.Map.add wrapper_pc wrapper_block blocks in
          let instr_wrapper =
            Let (ci.f_name, wrapper_closure wrapper_pc new_args ci.cloc)
          in
          let instr_real =
            match counter with
            | None -> Let (new_f, Closure (ci.args, ci.cont, ci.cloc))
            | Some counter ->
                Let (new_f, Closure (counter :: ci.args, ci.cont, ci.cloc))
          in
          let counter_and_pc =
            List.fold_left all ~init:[] ~f:(fun acc (counter, ci2) ->
                try
                  let pcs = Addr.Set.elements (Var.Map.find ci.f_name ci2.tc) in
                  List.map pcs ~f:(fun x -> counter, x) @ acc
                with Not_found -> acc)
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
                | Let (x, Apply { f; args; exact = true }) :: rem_rev ->
                    assert (Var.equal f ci.f_name);
                    let blocks =
                      Addr.Map.add
                        direct_call_pc
                        (direct_call_block ~counter ~x ~f:new_f ~args)
                        blocks
                    in
                    let blocks =
                      Addr.Map.add
                        bounce_call_pc
                        (bounce_call_block ~x ~f:new_f ~args)
                        blocks
                    in
                    let block =
                      match counter with
                      | None ->
                          let branch = Branch (bounce_call_pc, []) in
                          { block with body = List.rev rem_rev; branch }
                      | Some counter ->
                          let direct = Code.Var.fresh () in
                          let branch =
                            Cond (direct, (direct_call_pc, []), (bounce_call_pc, []))
                          in
                          let last =
                            Let
                              ( direct
                              , Prim
                                  ( Lt
                                  , [ Pv counter
                                    ; Pc
                                        (Int (Targetint.of_int_exn tailcall_max_depth))
                                    ] ) )
                          in
                          { block with body = List.rev (last :: rem_rev); branch }
                    in
                    let blocks = Addr.Map.remove pc blocks in
                    Addr.Map.add pc block blocks, free_pc
                | _ -> assert false)
          in
          ( blocks
          , free_pc
          , Wrapper { name = ci.f_name; code = instr_real; wrapper = instr_wrapper }
            :: closures ))
    in
    free_pc, blocks, closures
end

(* Trampoline variant for --effects=double-translation. The SCC consists of
   [caml_cps_closure]-paired closures. We apply the same depth-guarded
   trampoline strategy that [--effects=cps] uses for ordinary CPS calls
   (cf. effects.ml emit of [caml_stack_check_depth ? f(args) :
   caml_trampoline_return(f, args, 0)]), only here the call we are guarding
   is a plain direct-style call between mutually recursive functions.

   For each member of the SCC:
   - The original direct closure is renamed [new_direct_c], and a small
     wrapper that drives a [caml_direct_trampoline] loop takes the direct
     slot of [caml_cps_closure]. External direct callers go through the
     wrapper; sibling tail calls within the SCC bypass it.
   - Every recursive tail call in the inner-direct body is split into a
     direct branch ([Apply new_direct_c_i]) and a bounce branch that returns
     [caml_trampoline_return(new_direct_c_i, args, 1)]. The bounce object
     bubbles up to the wrapper's trampoline loop, which reapplies the
     callee with a fresh stack budget. [caml_stack_check_depth] gates
     between the two, exactly like the CPS-side check. *)
module Trampoline_dt = struct
  let direct_call_block ~x ~f ~args =
    let return = Code.Var.fork x in
    { params = []
    ; body = [ Let (return, Apply { f; args; exact = true }) ]
    ; branch = Return return
    }

  let bounce_call_block ~x ~f ~args =
    let return = Code.Var.fork x in
    let new_args = Code.Var.fresh () in
    { params = []
    ; body =
        [ Let
            (new_args, Prim (Extern "%js_array", List.map args ~f:(fun x -> Pv x)))
        ; Let
            ( return
            , Prim
                ( Extern "caml_trampoline_return"
                , [ Pv f; Pv new_args; Pc (Int Targetint.one) ] ) )
        ]
    ; branch = Return return
    }

  let wrapper_block inner ~args loc =
    let args_arr = Code.Var.fresh () in
    let result = Code.Var.fresh () in
    { params = []
    ; body =
        [ Event loc
        ; Let
            (args_arr, Prim (Extern "%js_array", List.map args ~f:(fun x -> Pv x)))
        ; Let
            ( result
            , Prim
                (Extern "caml_direct_trampoline", [ Pv inner; Pv args_arr ]) )
        ]
    ; branch = Return result
    }

  let has_loop free_pc blocks closures_map all =
    if debug_tc ()
    then (
      Format.eprintf
        "Detect cycles (paired, double-translation) of size (%d).\n%!"
        (List.length all);
      Format.eprintf
        "%a\n%!"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
           Var.print)
        all);
    let all = List.map all ~f:(fun id -> Var.Map.find id closures_map) in
    let blocks, free_pc, closures =
      List.fold_left
        all
        ~init:(blocks, free_pc, [])
        ~f:(fun (blocks, free_pc, closures) ci ->
          let { direct_c; cps_c; cps_code } =
            match ci.cps_pair with
            | Some p -> p
            | None -> assert false
          in
          if debug_tc ()
          then Format.eprintf "Rewriting (paired) for %a\n%!" Var.print ci.f_name;
          let new_direct_c = Code.Var.fork direct_c in
          let new_args = List.map ci.args ~f:Code.Var.fork in
          let wrapper_pc = free_pc in
          let free_pc = free_pc + 1 in
          let start_loc =
            let block = Addr.Map.find (fst ci.cont) blocks in
            match block.body with
            | Event loc :: _ -> loc
            | _ -> Parse_info.zero
          in
          let wrapper_b = wrapper_block new_direct_c ~args:new_args start_loc in
          let blocks = Addr.Map.add wrapper_pc wrapper_b blocks in
          let wrapper_c = Code.Var.fresh_n "wrapper" in
          let wrapper_code =
            Let (wrapper_c, wrapper_closure wrapper_pc new_args ci.cloc)
          in
          let inner_direct_code =
            Let (new_direct_c, Closure (ci.args, ci.cont, ci.cloc))
          in
          let pair_code =
            Let
              ( ci.f_name
              , Prim (Extern "caml_cps_closure", [ Pv wrapper_c; Pv cps_c ]) )
          in
          let scc_callees =
            List.fold_left all ~init:[] ~f:(fun acc ci2 ->
                try
                  let pcs = Addr.Set.elements (Var.Map.find ci.f_name ci2.tc) in
                  pcs @ acc
                with Not_found -> acc)
          in
          let blocks, free_pc =
            List.fold_left
              scc_callees
              ~init:(blocks, free_pc)
              ~f:(fun (blocks, free_pc) pc ->
                if debug_tc ()
                then Format.eprintf "Rewriting tc (paired) in %d\n%!" pc;
                let block = Addr.Map.find pc blocks in
                let direct_call_pc = free_pc in
                let bounce_call_pc = free_pc + 1 in
                let free_pc = free_pc + 2 in
                match List.rev block.body with
                | Let (x, Apply { f; args; exact = true }) :: rem_rev ->
                    assert (Var.equal f ci.f_name);
                    let blocks =
                      Addr.Map.add
                        direct_call_pc
                        (direct_call_block ~x ~f:new_direct_c ~args)
                        blocks
                    in
                    let blocks =
                      Addr.Map.add
                        bounce_call_pc
                        (bounce_call_block ~x ~f:new_direct_c ~args)
                        blocks
                    in
                    let direct = Code.Var.fresh () in
                    let branch =
                      Cond (direct, (direct_call_pc, []), (bounce_call_pc, []))
                    in
                    let last =
                      Let (direct, Prim (Extern "caml_stack_check_depth", []))
                    in
                    let block =
                      { block with body = List.rev (last :: rem_rev); branch }
                    in
                    let blocks = Addr.Map.remove pc blocks in
                    Addr.Map.add pc block blocks, free_pc
                | _ -> assert false)
          in
          ( blocks
          , free_pc
          , Paired_wrapper
              { name = ci.f_name
              ; inner_direct_code
              ; wrapper_code
              ; cps_code
              ; pair_code
              }
            :: closures ))
    in
    free_pc, blocks, closures
end

let dispatch_component free_pc blocks closures_map component =
  match component with
  | SCC.No_loop id ->
      let ci = Var.Map.find id closures_map in
      (match ci.cps_pair with
       | None ->
           let instr = Let (ci.f_name, Closure (ci.args, ci.cont, ci.cloc)) in
           free_pc, blocks, [ One { name = ci.f_name; code = instr } ]
       | Some { direct_c; cps_c; cps_code } ->
           let direct_code = Let (direct_c, Closure (ci.args, ci.cont, ci.cloc)) in
           let pair_code =
             Let
               ( ci.f_name
               , Prim (Extern "caml_cps_closure", [ Pv direct_c; Pv cps_c ]) )
           in
           ( free_pc
           , blocks
           , [ Paired_one { name = ci.f_name; direct_code; cps_code; pair_code } ]
           ))
  | SCC.Has_loop all ->
      let all_paired =
        List.for_all all ~f:(fun id ->
            Option.is_some (Var.Map.find id closures_map).cps_pair)
      in
      let any_paired =
        List.exists all ~f:(fun id ->
            Option.is_some (Var.Map.find id closures_map).cps_pair)
      in
      assert (Bool.equal any_paired all_paired);
      if all_paired
      then Trampoline_dt.has_loop free_pc blocks closures_map all
      else if not (Poly.equal (Config.effects ()) `Disabled)
      then
        (* Under --effects=cps/double-translation/jspi, unpaired SCCs are
           rare and the classic [Trampoline] transformation is not safe to
           run on them (its bounce mechanism doesn't compose with the CPS
           call-gen). Emit the closures back unchanged; deep recursion in
           these will still risk overflow, but they should generally not
           occur because partial_cps_analysis promotes mutually recursive
           functions to cps_needed. *)
        let closures =
          List.map all ~f:(fun id ->
              let ci = Var.Map.find id closures_map in
              One
                { name = ci.f_name
                ; code = Let (ci.f_name, Closure (ci.args, ci.cont, ci.cloc))
                })
        in
        free_pc, blocks, closures
      else Trampoline.has_loop free_pc blocks closures_map all

let rec rewrite_closures free_pc blocks body : int * _ * _ list =
  match body with
  | Let (_, Closure _) :: _ ->
      let closures, rem = collect_closures blocks body 0 in
      let closures_map =
        List.fold_left closures ~init:Var.Map.empty ~f:(fun closures_map x ->
            Var.Map.add x.f_name x closures_map)
      in
      let components = group_closures closures_map in
      let free_pc, blocks, closures =
        List.fold_left
          components
          ~init:(free_pc, blocks, [])
          ~f:(fun (free_pc, blocks, acc) component ->
            let free_pc, blocks, closures =
              dispatch_component free_pc blocks closures_map component
            in
            let intrs = closures :: acc in
            free_pc, blocks, intrs)
      in
      let closures =
        let pos_of_var x = (Var.Map.find x closures_map).pos in
        let pos = function
          | One { name; _ } -> pos_of_var name
          | Wrapper { name; _ } -> pos_of_var name
          | Paired_one { name; _ } -> pos_of_var name
          | Paired_wrapper { name; _ } -> pos_of_var name
        in
        List.flatten closures
        |> List.sort ~cmp:(fun a b -> compare (pos a) (pos b))
        |> List.concat_map ~f:(function
          | One { code; _ } -> [ code ]
          | Wrapper { code; wrapper; _ } -> [ code; wrapper ]
          | Paired_one { direct_code; cps_code; pair_code; _ } ->
              [ direct_code; cps_code; pair_code ]
          | Paired_wrapper
              { inner_direct_code; wrapper_code; cps_code; pair_code; _ } ->
              [ inner_direct_code; wrapper_code; cps_code; pair_code ])
      in
      let free_pc, blocks, rem = rewrite_closures free_pc blocks rem in
      free_pc, blocks, closures @ rem
  | i :: rem ->
      let free_pc, blocks, rem = rewrite_closures free_pc blocks rem in
      free_pc, blocks, i :: rem
  | [] -> free_pc, blocks, []

let f p : Code.program =
  Code.invariant p;
  let blocks, free_pc =
    Addr.Map.fold
      (fun pc _ (blocks, free_pc) ->
        (* make sure we have the latest version *)
        let block = Addr.Map.find pc blocks in
        let free_pc, blocks, body = rewrite_closures free_pc blocks block.body in
        Addr.Map.add pc { block with body } blocks, free_pc)
      p.blocks
      (p.blocks, p.free_pc)
  in
  let p = { p with blocks; free_pc } in
  Code.invariant p;
  p

let f p =
  assert (
    match Config.effects () with
    | `Disabled | `Jspi | `Native | `Double_translation -> true
    | `Cps -> false);
  let open Config.Param in
  match tailcall_optim () with
  | TcNone -> p
  | TcTrampoline ->
      let t = Timer.make () in
      let p' = f p in
      if Debug.find "times" ()
      then Format.eprintf "  generate closures: %a@." Timer.print t;
      p'
