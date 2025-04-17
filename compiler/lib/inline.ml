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

(*
Other ideas:

- specialize recursive function when one parameter is invariant
  (and is a function invoked exactly once)
     let rec f g x1 ... xn = ... f g e1 ... en ...
           ==>
     let g = ...
     let ref f x1 ... xn = ...

- inline short functions that return constants when followed by a match
  (also, move match above branch when argument is known)

- inline function applied to a constant value which performs a match
  on the value (and the corresponding branch of the match is short)
  (can we inline printf this way? could avoid a lot of dependencies)
*)

open! Stdlib
open Code

let debug = Debug.find "inlining"

let times = Debug.find "times"

let stats = Debug.find "stats"

(****)

module SCC = Strongly_connected_components.Make (Addr)

let blocks_in_loop p pc =
  let g =
    Code.traverse
      { fold = Code.fold_children }
      (fun pc g ->
        Addr.Map.add pc (Code.fold_children p.blocks pc Addr.Set.add Addr.Set.empty) g)
      pc
      p.blocks
      Addr.Map.empty
  in
  let scc = SCC.component_graph g in
  Array.fold_left
    ~f:(fun s (c, _) ->
      match c with
      | SCC.No_loop _ -> s
      | Has_loop l -> List.fold_left ~f:(fun s x -> Addr.Set.add x s) l ~init:s)
    ~init:Addr.Set.empty
    scc

(****)

type 'a cache = 'a option ref

type info =
  { f : Var.t
  ; params : Var.t list
  ; cont : Code.cont
  ; enclosing_function : Var.t option
  ; recursive : bool
  ; loops : bool cache
  ; body_size : int cache
  ; full_size : int cache
  ; closure_count : int cache
  ; init_code : int cache
  ; return_block : bool cache
  ; interesting_params : (Var.t * int) list cache
  ; mutable dead : bool
  }

let cache (pc, _) ref f p =
  match !ref with
  | Some v -> v
  | None ->
      let v = f p pc in
      ref := Some v;
      v

let contains_loop info =
  cache info.cont info.loops (fun { blocks; _ } pc ->
      let rec traverse pc ((visited, loop) as accu) =
        if loop
        then accu
        else if Addr.Map.mem pc visited
        then visited, Addr.Map.find pc visited
        else
          let visited, loop =
            Code.fold_children blocks pc traverse (Addr.Map.add pc true visited, false)
          in
          Addr.Map.add pc false visited, loop
      in
      snd (traverse pc (Addr.Map.empty, false)))

let sum f { blocks; _ } pc =
  Code.traverse
    { fold = fold_children }
    (fun pc acc -> f (Addr.Map.find pc blocks) + acc)
    pc
    blocks
    0

let rec block_size ~recurse ~env p { branch; body; _ } =
  List.fold_left
    ~f:(fun n i ->
      match i with
      | Event _ -> n
      | Let (f, Closure (_, (pc, _))) ->
          if Var.Map.mem f env && (Var.Map.find f env).dead
          then n
          else if recurse
          then size ~recurse ~env p pc + n + 1
          else n + 1
      | _ -> n + 1)
    ~init:0
    body
  +
  match branch with
  | Cond _ | Raise _ -> 2
  | Switch (_, a1) -> Array.length a1
  | _ -> 0

and size ~recurse ~env p pc = sum (block_size ~recurse ~env p) p pc

let body_size info ~env =
  cache info.cont info.body_size (fun p pc -> size ~recurse:false ~env p pc)

let full_size info ~env =
  cache info.cont info.full_size (fun p pc -> size ~recurse:true ~env p pc)

let closure_count_uncached ~env =
  sum (fun { body; _ } ->
      List.fold_left
        ~f:(fun n i ->
          match i with
          | Let (f, Closure _) when not (Var.Map.mem f env && (Var.Map.find f env).dead)
            -> n + 1
          | _ -> n)
        ~init:0
        body)

let closure_count info ~env =
  cache info.cont info.closure_count (closure_count_uncached ~env)

let count_init_code info =
  cache
    info.cont
    info.init_code
    (sum
    @@ fun { body; _ } ->
    List.fold_left
      ~f:(fun n i ->
        match i with
        | Let (_, (Closure _ | Field _ | Constant _ | Block _)) -> n + 1
        | Let (_, (Apply _ | Prim _ | Special _))
        | Assign _ | Set_field _ | Offset_ref _ | Array_set _ | Event _ -> n)
      ~init:0
      body)

let return_block info =
  cache info.cont info.return_block (fun { blocks; _ } pc ->
      Code.traverse
        { fold = fold_children }
        (fun pc acc ->
          acc
          &&
          let block = Addr.Map.find pc blocks in
          match block.branch with
          | Return x -> (
              match Code.last_instr block.body with
              | Some (Let (x', Block _)) -> Var.equal x x'
              | _ -> false)
          | Raise _ | Stop | Branch _ | Cond _ | Switch _ | Pushtrap _ | Poptrap _ -> true)
        pc
        blocks
        true)

let interesting_parameters info ~live_vars =
  let params = info.params in
  cache info.cont info.interesting_params (fun { blocks; _ } pc ->
      let params = List.filter ~f:(fun x -> live_vars.(Var.idx x) = 1) params in
      if List.is_empty params
      then []
      else
        Code.traverse
          { fold = fold_children }
          (fun pc lst ->
            let block = Addr.Map.find pc blocks in
            List.fold_left
              ~f:(fun lst i ->
                match i with
                | Let (_, Apply { f; args; _ }) when List.mem f ~set:params ->
                    (f, List.length args) :: lst
                | _ -> lst)
              ~init:lst
              block.body)
          pc
          blocks
          [])

let functor_like ~env p info =
  (not info.recursive)
  && (not (contains_loop info p))
  && return_block info p
  && count_init_code info p * 2 > body_size info ~env p
  && full_size info ~env p - body_size info ~env p <= 20 * closure_count info ~env p

let rec small_function
    ~live_vars
    ~env
    p
    ~current_function
    ~enclosing_function
    ~in_loop
    ~has_closures
    info
    args =
  (not info.recursive)
  && body_size info ~env p <= 15
  && closure_count info ~env p = 0
  && (not (List.is_empty args))
  &&
  let relevant_params = interesting_parameters info ~live_vars p in
  try
    List.iter2
      ~f:(fun arg param ->
        if Var.Map.mem arg env && List.mem_assoc param ~map:relevant_params
        then
          let info' = Var.Map.find arg env in
          let arity = List.assoc param relevant_params in
          if
            List.length info'.params = arity
            && should_inline
                 ~live_vars
                 ~env
                 p
                 ~current_function
                 ~enclosing_function
                 ~in_loop:(in_loop || contains_loop info p)
                 ~has_closures
                 info'
                 []
          then raise Exit)
      args
      info.params;
    false
  with Exit -> true

and should_inline
    ~live_vars
    ~env
    p
    ~current_function
    ~enclosing_function
    ~has_closures
    ~in_loop
    info
    args =
  (* A closure contains a pointer to (recursively) the contexts of its
     enclosing functions. The context of a function contains the
     variables bound in this function which are referred to from one
     of the enclosed function. To avoid memory leaks, we don't inline
     functions containing closure if this makes these closures capture
     additional contexts shared with other closures. *)
  (match Config.target () with
  | `Wasm -> true
  | `JavaScript ->
      closure_count info ~env p = 0
      || Option.equal Var.equal info.enclosing_function current_function
      || (not !has_closures)
         && Option.equal Var.equal info.enclosing_function enclosing_function)
  && (functor_like ~env p info
     || (live_vars.(Var.idx info.f) = 1
        &&
        match Config.target () with
        | `Wasm when in_loop ->
            (* Avoid inlining in loop since, if the loop is not hot, the
               code might never get optimized *)
            body_size info ~env p < 30 && not (contains_loop info p)
        | `JavaScript when Option.is_none current_function && contains_loop info p ->
            (* Avoid inlining loops at toplevel since the toplevel
               code is less likely to get optimized *)
            false
        | _ -> body_size info ~env p < 150)
     || (body_size info ~env p <= 1 && closure_count info ~env p = 0)
     || small_function
          ~live_vars
          ~env
          p
          ~current_function
          ~enclosing_function
          ~in_loop
          ~has_closures
          info
          args)

(****)

let collect_closures p =
  let rec traverse p current pc closures =
    Code.traverse
      { fold = Code.fold_children }
      (fun pc closures ->
        let block = Addr.Map.find pc p.blocks in
        List.fold_left
          ~f:(fun closures i ->
            match i with
            | Let (f, Closure (params, ((pc', _) as cont))) ->
                let closures = Var.Map.add f (params, cont, current) closures in
                traverse p (Some f) pc' closures
            | _ -> closures)
          ~init:closures
          block.body)
      pc
      p.blocks
      closures
  in
  traverse p None p.start Var.Map.empty

let add_dep deps current f =
  Option.iter
    ~f:(fun g -> deps := Var.Map.add f (Var.Set.add g (Var.Map.find f !deps)) !deps)
    current

let collect_deps p closures =
  let deps = ref (Var.Map.map (fun _ -> Var.Set.empty) closures) in
  let traverse p current pc =
    Code.traverse
      { fold = Code.fold_children }
      (fun pc () ->
        let block = Addr.Map.find pc p.blocks in
        Freevars.iter_block_free_vars
          (fun f -> if Var.Map.mem f closures then add_dep deps current f)
          block;
        List.iter
          ~f:(fun i ->
            match i with
            | Let (f, Closure _) -> add_dep deps current f
            | _ -> ())
          block.body)
      pc
      p.blocks
      ()
  in
  traverse p None p.start;
  Var.Map.iter (fun f (_, (pc, _), _) -> traverse p (Some f) pc) closures;
  !deps

module Var_SCC = Strongly_connected_components.Make (Var)

let visit_closures p f acc =
  let closures = collect_closures p in
  let deps = collect_deps p closures in
  let scc = Var_SCC.connected_components_sorted_from_roots_to_leaf deps in
  let f' recursive acc g =
    let params, cont, parent = Var.Map.find g closures in
    f recursive parent (Some g) params cont acc
  in
  let acc =
    Array.fold_left
      scc
      ~f:(fun acc group ->
        match group with
        | Var_SCC.No_loop g -> f' false acc g
        | Has_loop l -> List.fold_left ~f:(fun acc g -> f' true acc g) ~init:acc l)
      ~init:acc
  in
  f false None None [] (p.start, []) acc

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

let inline_function p rem branch x params cont args =
  let blocks, cont_pc, free_pc =
    match rem, branch with
    | [], Return y when Var.compare x y = 0 ->
        (* We do not need a continuation block for tail calls *)
        p.blocks, None, p.free_pc
    | _ ->
        let fresh_addr = p.free_pc in
        let free_pc = fresh_addr + 1 in
        ( Addr.Map.add fresh_addr { params = [ x ]; body = rem; branch } p.blocks
        , Some fresh_addr
        , free_pc )
  in
  let blocks = rewrite_closure blocks cont_pc (fst cont) in
  (* We do not really need this intermediate block.
     It just avoids the need to find which function
     parameters are used in the function body. *)
  let fresh_addr = free_pc in
  let free_pc = fresh_addr + 1 in
  assert (List.length args = List.length params);
  let blocks =
    Addr.Map.add fresh_addr { params; body = []; branch = Branch cont } blocks
  in
  [], (Branch (fresh_addr, args), { p with blocks; free_pc })

let remove_dead_closures_from_block ~env p pc block =
  let is_dead_closure i =
    match i with
    | Let (f, Closure _) -> Var.Map.mem f env && (Var.Map.find f env).dead
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

let rec remove_dead_closures ~env p pc =
  Code.traverse
    { fold = fold_children }
    (fun pc p ->
      let block = Addr.Map.find pc p.blocks in
      let p = remove_dead_closures_from_block ~env p pc block in
      let block = Addr.Map.find pc p.blocks in
      List.fold_left
        ~f:(fun p i ->
          match i with
          | Let (_, Closure (_, (pc', _))) -> remove_dead_closures ~env p pc'
          | _ -> p)
        ~init:p
        block.body)
    pc
    p.blocks
    p

let trace_inlining
    info
    ~live_vars
    ~env
    p
    ~current_function
    ~enclosing_function
    ~in_loop
    ~has_closures
    x
    args =
  if debug ()
  then
    let sz = body_size info ~env p in
    let sz' = full_size info ~env p in
    Format.eprintf
      "%a <- %a%s: %b uses:%d size:%d/%d loop:%b rec:%b closures:%d init:%d \
       return_block:%b functor:%b small:%b@."
      Var.print
      x
      Var.print
      info.f
      (match Var.get_name info.f with
      | Some s -> "(" ^ s ^ ")"
      | None -> "")
      (should_inline
         ~live_vars
         ~env
         ~current_function
         ~enclosing_function
         ~in_loop
         ~has_closures
         p
         info
         args)
      live_vars.(Var.idx info.f)
      sz
      sz'
      (contains_loop info p)
      info.recursive
      (closure_count info ~env p)
      (count_init_code info p)
      (return_block info p)
      (functor_like ~env p info)
      (small_function
         ~live_vars
         ~env
         p
         ~current_function
         ~enclosing_function
         ~in_loop
         ~has_closures
         info
         args)

let inline_in_block
    ~inline_count
    ~live_vars
    ~in_loop
    ~has_closures
    env
    p
    ~current_function
    ~enclosing_function
    pc
    block =
  let body, (branch, p) =
    List.fold_right
      ~f:(fun i (rem, state) ->
        match i with
        | Let (x, Apply { f; args; exact = true; _ }) when Var.Map.mem f env ->
            let info = Var.Map.find f env in
            let { params; cont = (pc, _) as cont; _ } = info in
            trace_inlining
              info
              ~live_vars
              ~env
              p
              ~current_function
              ~enclosing_function
              ~in_loop
              ~has_closures
              x
              args;
            if
              should_inline
                ~live_vars
                ~env
                ~current_function
                ~enclosing_function
                ~in_loop
                ~has_closures
                p
                info
                args
            then (
              incr inline_count;
              let branch, p = state in
              if closure_count ~env info p > 0 then has_closures := true;
              if live_vars.(Var.idx f) > 1
              then (
                let p = remove_dead_closures ~env p pc in
                let p, _, params, cont = Duplicate.closure p ~f ~params ~cont in
                live_vars.(Var.idx f) <- live_vars.(Var.idx f) - 1;
                inline_function p rem branch x params cont args)
              else (
                info.dead <- true;
                inline_function p rem branch x params cont args))
            else i :: rem, state
        | _ -> i :: rem, state)
      ~init:([], (block.branch, p))
      block.body
  in
  { p with blocks = Addr.Map.add pc { block with body; branch } p.blocks }

let inline ~inline_count p ~live_vars =
  if debug () then Format.eprintf "====== inlining ======@.";
  fst
    (visit_closures
       p
       (fun recursive enclosing_function name_opt params ((pc, _) as cont) (p, env) ->
         let has_closures = ref (closure_count_uncached ~env p pc > 0) in
         let in_loop = blocks_in_loop p pc in
         let p =
           Code.traverse
             { fold = Code.fold_children }
             (fun pc p ->
               let block = Addr.Map.find pc p.blocks in
               if
                 (* Skip blocks with no call of known function *)
                 List.for_all
                   ~f:(fun i ->
                     match i with
                     | Let (_, Apply { f; _ }) -> not (Var.Map.mem f env)
                     | _ -> true)
                   block.body
               then p
               else
                 inline_in_block
                   ~inline_count
                   ~live_vars
                   ~in_loop:(Addr.Set.mem pc in_loop)
                   ~has_closures
                   env
                   p
                   ~current_function:name_opt
                   ~enclosing_function
                   pc
                   block)
             pc
             p.blocks
             p
         in
         let env =
           match name_opt with
           | Some f ->
               Var.Map.add
                 f
                 { f
                 ; params
                 ; cont
                 ; enclosing_function
                 ; recursive
                 ; loops = ref None
                 ; body_size = ref None
                 ; full_size = ref None
                 ; closure_count = ref None
                 ; init_code = ref None
                 ; return_block = ref None
                 ; interesting_params = ref None
                 ; dead = false
                 }
                 env
           | None -> env
         in
         p, env)
       (p, Var.Map.empty))

(****)

let f p live_vars =
  Code.invariant p;
  let inline_count = ref 0 in
  let t = Timer.make () in
  let p = inline ~inline_count p ~live_vars in
  if times () then Format.eprintf "  inlining: %a@." Timer.print t;
  if stats () then Format.eprintf "Stats - inlining: %d inlined functions@." !inline_count;
  Code.invariant p;
  p
