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

type closure_info =
  { f_name : Code.Var.t
  ; args : Code.Var.t list
  ; cont : Code.cont
  ; tc : Code.Addr.Set.t Code.Var.Map.t
  ; pos : int
  ; cloc : Parse_info.t option
  }

module SCC = Strongly_connected_components.Make (Var)

let add_multi k v map =
  let set = try Var.Map.find k map with Not_found -> Addr.Set.empty in
  Var.Map.add k (Addr.Set.add v set) map

let rec collect_apply pc p visited tc =
  if Addr.Set.mem pc visited
  then visited, tc
  else
    let visited = Addr.Set.add pc visited in
    let block = Code.block pc p in
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
          p
          pc
          (fun pc (visited, tc) -> collect_apply pc p visited tc)
          (visited, tc)

let rec collect_closures p l pos =
  match l with
  | Let (f_name, Closure (args, ((pc, _) as cont), cloc)) :: rem ->
      let _, tc = collect_apply pc p Addr.Set.empty Var.Map.empty in
      let l, rem = collect_closures p rem (succ pos) in
      { f_name; args; cont; tc; pos; cloc } :: l, rem
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

  let wrapper_closure pc args cloc = Closure (args, (pc, []), cloc)

  let f p closures_map component =
    match component with
    | SCC.No_loop id ->
        let ci = Var.Map.find id closures_map in
        let instr = Let (ci.f_name, Closure (ci.args, ci.cont, ci.cloc)) in
        p, [ One { name = ci.f_name; code = instr } ]
    | SCC.Has_loop all ->
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
        let p, closures =
          List.fold_left all ~init:(p, []) ~f:(fun (p, closures) (counter, ci) ->
              if debug_tc ()
              then Format.eprintf "Rewriting for %a\n%!" Var.print ci.f_name;
              let new_f = Code.Var.fork ci.f_name in
              let new_args = List.map ci.args ~f:Code.Var.fork in
              let wrapper_pc = Code.free_pc p in
              let new_counter = Option.map counter ~f:Code.Var.fork in
              let start_loc =
                let block = Code.block (fst ci.cont) p in
                match block.body with
                | Event loc :: _ -> loc
                | _ -> Parse_info.zero
              in
              let wrapper_block =
                wrapper_block new_f ~args:new_args ~counter:new_counter start_loc
              in
              let p = Code.add_block wrapper_pc wrapper_block p in
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
              let p =
                List.fold_left counter_and_pc ~init:p ~f:(fun p (counter, pc) ->
                    if debug_tc () then Format.eprintf "Rewriting tc in %d\n%!" pc;
                    let block = Code.block pc p in
                    let direct_call_pc = Code.free_pc p in
                    let bounce_call_pc = direct_call_pc + 1 in
                    match List.rev block.body with
                    | Let (x, Apply { f; args; exact = true }) :: rem_rev ->
                        assert (Var.equal f ci.f_name);
                        let p =
                          Code.add_block
                            direct_call_pc
                            (direct_call_block ~counter ~x ~f:new_f ~args)
                            p
                        in
                        let p =
                          Code.add_block
                            bounce_call_pc
                            (bounce_call_block ~x ~f:new_f ~args)
                            p
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
                        Code.add_block pc block p
                    | _ -> assert false)
              in
              ( p
              , Wrapper { name = ci.f_name; code = instr_real; wrapper = instr_wrapper }
                :: closures ))
        in
        p, closures
end

let rec rewrite_closures p body : _ * _ list =
  match body with
  | Let (_, Closure _) :: _ ->
      let closures, rem = collect_closures p body 0 in
      let closures_map =
        List.fold_left closures ~init:Var.Map.empty ~f:(fun closures_map x ->
            Var.Map.add x.f_name x closures_map)
      in
      let components = group_closures closures_map in
      let p, closures =
        List.fold_left components ~init:(p, []) ~f:(fun (p, acc) component ->
            let p, closures = Trampoline.f p closures_map component in
            let intrs = closures :: acc in
            p, intrs)
      in
      let closures =
        let pos_of_var x = (Var.Map.find x closures_map).pos in
        let pos = function
          | One { name; _ } -> pos_of_var name
          | Wrapper { name; _ } -> pos_of_var name
        in
        List.flatten closures
        |> List.sort ~cmp:(fun a b -> compare (pos a) (pos b))
        |> List.concat_map ~f:(function
             | One { code; _ } -> [ code ]
             | Wrapper { code; wrapper; _ } -> [ code; wrapper ])
      in
      let p, rem = rewrite_closures p rem in
      p, closures @ rem
  | i :: rem ->
      let p, rem = rewrite_closures p rem in
      p, i :: rem
  | [] -> p, []

let f p : Code.program =
  Code.invariant p;
  let p =
    Addr.Map.fold
      (fun pc _ p ->
        (* make sure we have the latest version *)
        let block = Code.block pc p in
        let p, body = rewrite_closures p block.body in
        Code.add_block pc { block with body } p)
      (Code.blocks p)
      p
  in
  Code.invariant p;
  p

let f p =
  assert (
    match Config.effects () with
    | `Disabled | `Jspi -> true
    | `Cps | `Double_translation -> false);
  let open Config.Param in
  match tailcall_optim () with
  | TcNone -> p
  | TcTrampoline ->
      let t = Timer.make () in
      let p' = f p in
      if Debug.find "times" ()
      then Format.eprintf "  generate closures: %a@." Timer.print t;
      p'
