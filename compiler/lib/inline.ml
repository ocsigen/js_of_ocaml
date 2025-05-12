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

type prop =
  { size : int
  ; optimizable : bool
  }

type closure_info =
  { cl_params : Var.t list
  ; cl_cont : int * Var.t list
  ; cl_prop : prop
  ; cl_simpl : (int Var.Map.t * Var.Set.t) option
  }

let block_size { branch; body; _ } =
  List.fold_left
    ~f:(fun n i ->
      match i with
      | Event _ -> n
      | _ -> n + 1)
    ~init:0
    body
  +
  match branch with
  | Cond _ -> 2
  | Switch (_, a1) -> Array.length a1
  | _ -> 0

let simple_function blocks size name params pc =
  let bound_vars =
    ref (List.fold_left ~f:(fun s x -> Var.Set.add x s) ~init:Var.Set.empty params)
  in
  let free_vars = ref Var.Map.empty in
  let tc = ref Var.Set.empty in
  try
    (* Ignore large functions *)
    if size > 10 then raise Exit;
    Code.preorder_traverse
      { fold = Code.fold_children }
      (fun pc () ->
        let block = Addr.Map.find pc blocks in
        (match block.branch with
        (* We currenly disable inlining when raising and catching exception *)
        | Poptrap _ | Pushtrap _ -> raise Exit
        | Raise _ -> raise Exit
        | Stop -> raise Exit
        | Return x -> (
            match List.last block.body with
            | None -> ()
            | Some (Let (y, Apply { f; _ })) ->
                (* track if some params are called in tail position *)
                if Code.Var.equal x y && List.mem ~eq:Var.equal f params
                then tc := Var.Set.add f !tc
            | Some _ -> ())
        | Branch _ | Cond _ | Switch _ -> ());
        List.iter block.body ~f:(fun i ->
            match i with
            (* We currenly don't want to duplicate Closure *)
            | Let (_, Closure _) -> raise Exit
            | _ -> ());
        Freevars.iter_block_bound_vars
          (fun x -> bound_vars := Var.Set.add x !bound_vars)
          block;
        Freevars.iter_block_free_vars
          (fun x ->
            if not (Var.Set.mem x !bound_vars)
            then
              free_vars :=
                Var.Map.update
                  x
                  (function
                    | None -> Some 1
                    | Some n -> Some (succ n))
                  !free_vars)
          block)
      pc
      blocks
      ();
    if Var.Map.mem name !free_vars then raise Exit;
    Some (!free_vars, !tc)
  with Exit -> None

(****)

let optimizable blocks pc =
  Code.traverse
    { fold = Code.fold_children }
    (fun pc { size; optimizable } ->
      let b = Addr.Map.find pc blocks in
      let this_size = block_size b in
      let optimizable =
        optimizable
        && List.for_all b.body ~f:(function
             | Let (_, Prim (Extern "caml_js_eval_string", _)) -> false
             | Let (_, Prim (Extern "debugger", _)) -> false
             | Let
                 ( _
                 , Prim (Extern ("caml_js_var" | "caml_js_expr" | "caml_pure_js_expr"), _)
                 ) ->
                 (* TODO: we should be smarter here and look the generated js *)
                 (* let's consider it this opmiziable *)
                 true
             | _ -> true)
      in
      { optimizable; size = size + this_size })
    pc
    blocks
    { optimizable = true; size = 0 }

let get_closures { blocks; _ } =
  Addr.Map.fold
    (fun _ block closures ->
      List.fold_left block.body ~init:closures ~f:(fun closures i ->
          match i with
          | Let (x, Closure (cl_params, cl_cont, _)) ->
              (* we can compute this once during the pass
                 as the property won't change with inlining *)
              let cl_prop = optimizable blocks (fst cl_cont) in
              let cl_simpl =
                simple_function blocks cl_prop.size x cl_params (fst cl_cont)
              in
              Var.Map.add x { cl_params; cl_cont; cl_prop; cl_simpl } closures
          | _ -> closures))
    blocks
    Var.Map.empty

(****)

let rewrite_block pc' pc blocks =
  let block = Addr.Map.find pc blocks in
  let block =
    match block.branch, pc' with
    | Return y, Some pc' -> { block with branch = Branch (pc', [ y ]) }
    | _ -> block
  in
  Addr.Map.add pc block blocks

let rewrite_closure blocks cont_pc clos_pc =
  Code.traverse
    { fold = Code.fold_children_skip_try_body }
    (rewrite_block cont_pc)
    clos_pc
    blocks
    blocks

(****)

let inline inline_count live_vars closures pc (outer, p) =
  let block = Addr.Map.find pc p.blocks in
  let body, (outer, branch, p) =
    List.fold_right
      block.body
      ~init:([], (outer, block.branch, p))
      ~f:(fun i (rem, state) ->
        match i with
        | Let (x, Apply { f; args; exact = true; _ }) when Var.Map.mem f closures -> (
            let outer, branch, p = state in
            let { cl_params = params
                ; cl_cont = clos_cont
                ; cl_prop = { size = f_size; optimizable = f_optimizable }
                ; cl_simpl
                } =
              Var.Map.find f closures
            in
            let map_param_to_arg =
              List.fold_left2
                ~f:(fun map a b -> Var.Map.add a b map)
                ~init:Var.Map.empty
                params
                args
            in
            if
              live_vars.(Var.idx f) = 1
              && Bool.equal outer.optimizable f_optimizable
                 (* Inlining the code of an optimizable function could
                     make this code unoptimized. (wrt to Jit compilers) *)
              && f_size < Config.Param.inlining_limit ()
            then (
              live_vars.(Var.idx f) <- 0;
              let blocks, cont_pc, free_pc =
                match rem, branch with
                | [], Return y when Var.compare x y = 0 ->
                    (* We do not need a continuation block for tail calls *)
                    p.blocks, None, p.free_pc
                | _ ->
                    let fresh_addr = p.free_pc in
                    let free_pc = fresh_addr + 1 in
                    ( Addr.Map.add
                        fresh_addr
                        { params = [ x ]; body = rem; branch }
                        p.blocks
                    , Some fresh_addr
                    , free_pc )
              in
              let blocks = rewrite_closure blocks cont_pc (fst clos_cont) in
              (* We do not really need this intermediate block.
                 It just avoids the need to find which function
                 parameters are used in the function body. *)
              let fresh_addr = free_pc in
              let free_pc = fresh_addr + 1 in
              let blocks =
                Addr.Map.add
                  fresh_addr
                  { params; body = []; branch = Branch clos_cont }
                  blocks
              in
              let outer = { outer with size = outer.size + f_size } in
              incr inline_count;
              [], (outer, Branch (fresh_addr, args), { p with blocks; free_pc }))
            else
              match cl_simpl with
              | Some (free_vars, tc_params)
              (* We inline/duplicate
                 - single instruction functions (f_size = 1)
                 - small funtions that call one of their arguments in
                   tail position when the argument is a direct closure
                   used only once. *)
                when Code.Var.Set.exists
                       (fun x ->
                         let farg_tc = Var.Map.find x map_param_to_arg in
                         Var.Map.mem farg_tc closures && live_vars.(Var.idx farg_tc) = 1)
                       tc_params
                     || f_size <= 1 ->
                  let () =
                    (* Update live_vars *)
                    Var.Map.iter
                      (fun fv c ->
                        if not (Var.equal fv f)
                        then
                          let idx = Var.idx fv in
                          live_vars.(idx) <- live_vars.(idx) + c)
                      free_vars;
                    live_vars.(Var.idx f) <- live_vars.(Var.idx f) - 1
                  in
                  let p, _f, params, clos_cont =
                    Duplicate.closure p ~f ~params ~cont:clos_cont
                  in
                  let blocks, cont_pc, free_pc =
                    match rem, branch with
                    | [], Return y when Var.compare x y = 0 ->
                        (* We do not need a continuation block for tail calls *)
                        p.blocks, None, p.free_pc
                    | _ ->
                        let fresh_addr = p.free_pc in
                        let free_pc = fresh_addr + 1 in
                        ( Addr.Map.add
                            fresh_addr
                            { params = [ x ]; body = rem; branch }
                            p.blocks
                        , Some fresh_addr
                        , free_pc )
                  in
                  let blocks = rewrite_closure blocks cont_pc (fst clos_cont) in
                  (* We do not really need this intermediate block.
                       It just avoids the need to find which function
                       parameters are used in the function body. *)
                  let fresh_addr = free_pc in
                  let free_pc = fresh_addr + 1 in
                  let blocks =
                    Addr.Map.add
                      fresh_addr
                      { params; body = []; branch = Branch clos_cont }
                      blocks
                  in
                  let outer = { outer with size = outer.size + f_size } in
                  incr inline_count;
                  [], (outer, Branch (fresh_addr, args), { p with blocks; free_pc })
              | _ -> i :: rem, state)
        | _ -> i :: rem, state)
  in
  outer, { p with blocks = Addr.Map.add pc { block with body; branch } p.blocks }

(****)

let times = Debug.find "times"

let stats = Debug.find "stats"

let debug_stats = Debug.find "stats-debug"

(* Inlining a function used only once will leave an unused closure
   with an initial continuation pointing to a block belonging to
   another function. This removes these closures. *)

let remove_dead_closures_from_block ~live_vars p pc block =
  let is_dead_closure i =
    match i with
    | Let (f, Closure _) ->
        let f = Var.idx f in
        f < Array.length live_vars && live_vars.(f) = 0
    | _ -> false
  in
  if List.exists ~f:is_dead_closure block.body
  then
    { p with
      blocks =
        Addr.Map.add
          pc
          { block with
            body =
              List.fold_left block.body ~init:[] ~f:(fun acc i ->
                  match i, acc with
                  | Event _, Event _ :: prev ->
                      (* Avoid consecutive events (keep just the last one) *)
                      i :: prev
                  | _ -> if is_dead_closure i then acc else i :: acc)
              |> List.rev
          }
          p.blocks
    }
  else p

let remove_dead_closures ~live_vars p pc =
  Code.traverse
    { fold = fold_children }
    (fun pc p ->
      let block = Addr.Map.find pc p.blocks in
      remove_dead_closures_from_block ~live_vars p pc block)
    pc
    p.blocks
    p

let f p live_vars =
  let previous_p = p in
  let inline_count = ref 0 in
  Code.invariant p;
  let t = Timer.make () in
  let closures = get_closures p in
  let _closures, p =
    Code.fold_closures_innermost_first
      p
      (fun name cl_params (pc, _) _ (closures, p) ->
        let traverse outer =
          let outer, p =
            Code.traverse
              { fold = Code.fold_children }
              (inline inline_count live_vars closures)
              pc
              p.blocks
              (outer, p)
          in
          let p = remove_dead_closures ~live_vars p pc in
          outer, p
        in
        match name with
        | None ->
            let _, p = traverse (optimizable p.blocks pc) in
            closures, p
        | Some x ->
            let info = Var.Map.find x closures in
            let outer, p = traverse info.cl_prop in
            let cl_simpl = simple_function p.blocks outer.size x cl_params pc in
            let closures =
              Var.Map.add x { info with cl_prop = outer; cl_simpl } closures
            in
            closures, p)
      (closures, p)
  in
  (* Inlining a raising function can result in empty blocks *)
  if times () then Format.eprintf "  inlining: %a@." Timer.print t;
  if stats () then Format.eprintf "Stats - inline: %d optimizations@." !inline_count;
  let p = Deadcode.remove_unused_blocks p in
  if debug_stats ()
  then Code.check_updates ~name:"inline" previous_p p ~updates:!inline_count;
  Code.invariant p;
  p
