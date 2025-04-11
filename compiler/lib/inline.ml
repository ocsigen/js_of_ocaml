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

let debug = Debug.find "inlining"

let times = Debug.find "times"

type 'a cache = 'a option ref

type info =
  { params : Code.Var.t list
  ; cont : Code.cont
  ; loops : bool cache
  ; body_size : int cache
  ; full_size : int cache
  ; closure_count : int cache
  ; init_code : int cache
  ; recursive : bool cache
  ; return_block : bool cache
  ; interesting_params : Code.Var.t list cache
  ; mutable dead : bool
  }

let cache ref f p pc =
  match !ref with
  | Some v -> v
  | None ->
      let v = f p pc in
      ref := Some v;
      v

let contains_loop info =
  cache info.loops (fun { blocks; _ } pc ->
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
          let info = Var.Map.find f env in
          if info.dead
          then n
          else if recurse
          then cache info.full_size (size ~recurse ~env) p pc + n + 1
          else n + 1
      | _ -> n + 1)
    ~init:0
    body
  +
  match branch with
  | Cond _ -> 2
  | Switch (_, a1) -> Array.length a1
  | _ -> 0

and size ~recurse ~env p pc = sum (block_size ~recurse ~env p) p pc

let body_size info ~env = cache info.body_size (fun p pc -> size ~recurse:false ~env p pc)

let full_size info ~env = cache info.full_size (fun p pc -> size ~recurse:true ~env p pc)

let closure_count info =
  cache
    info.closure_count
    (sum (fun { body; _ } ->
         List.fold_left
           ~f:(fun n i ->
             match i with
             | Let (_, Closure _) -> n + 1
             | _ -> n)
           ~init:0
           body))

let count_init_code info =
  cache
    info.init_code
    (sum
    @@ fun { body; _ } ->
    List.fold_left
      ~f:(fun n i ->
        match i with
        | Let (_, (Closure _ | Field _ | Constant _ | Block _)) -> n + 1
        | _ -> n)
      ~init:0
      body)

let is_recursive info ~env p f pc =
  cache
    info.recursive
    (fun { blocks; _ } pc ->
      let rec traverse blocks f pc =
        Code.traverse
          { fold = fold_children }
          (fun pc _ ->
            let block = Addr.Map.find pc blocks in
            Freevars.iter_block_free_vars
              (fun f' -> if Code.Var.equal f f' then raise Exit)
              block;
            List.iter
              ~f:(fun i ->
                match i with
                | Let (f', Closure (_, (pc', _))) ->
                    if not (Var.Map.find f' env).dead then traverse blocks f pc'
                | _ -> ())
              block.body)
          pc
          blocks
          ()
      in
      try
        traverse blocks f pc;
        false
      with Exit -> true)
    p
    pc

let return_block info =
  cache info.return_block (fun { blocks; _ } pc ->
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

let interesting_parameters info ~live_vars p params pc =
  cache
    info.interesting_params
    (fun { blocks; _ } pc ->
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
                | Let (_, Apply { f; _ }) when List.mem f ~set:params -> f :: lst
                | _ -> lst)
              ~init:lst
              block.body)
          pc
          blocks
          [])
    p
    pc

let small_function ~live_vars ~env p info args f pc params =
  body_size info ~env p pc <= 15
  && closure_count info p pc = 0
  && (not (is_recursive info ~env p f pc))
  &&
  let relevant_params = interesting_parameters info ~live_vars p params pc in
  try
    List.iter2
      ~f:(fun arg param ->
        if live_vars.(Var.idx arg) = 1 && Var.Map.mem arg env
        then
          let pc, _ = (Var.Map.find arg env).cont in
          if List.mem param ~set:relevant_params && closure_count info p pc = 0
          then raise Exit)
      args
      params;
    false
  with Exit -> true

let functor_like ~env p info f pc =
  (not (contains_loop info p pc))
  && return_block info p pc
  && count_init_code info p pc * 2 > body_size info ~env p pc
  && (not (is_recursive info ~env p f pc))
  && full_size info ~env p pc < 20 * closure_count info p pc

let should_inline ~live_vars ~env p ~at_toplevel info args f pc params =
  (* Don't inline loops at toplevel *)
  (not (at_toplevel && contains_loop info p pc))
  (* Don't inline closures in JavaScript, except at toplevel, since
     this can results in memory leaks *)
  && (at_toplevel
     ||
     match Config.target () with
     | `JavaScript -> closure_count info p pc = 0
     | `Wasm -> true)
  && (live_vars.(Var.idx f) = 1
     || functor_like ~env p info f pc
     || (body_size info ~env p pc = 1 && closure_count info p pc = 0)
     || small_function ~live_vars ~env p info args f pc params)

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
    | Let (f, Closure _) -> not (Var.Map.find f env).dead
    | _ -> true
  in
  if List.exists ~f:is_dead_closure block.body
  then
    { p with
      blocks =
        Addr.Map.add
          pc
          { block with body = List.filter ~f:is_dead_closure block.body }
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

let trace_inlining info ~live_vars ~env p at_toplevel x args f params pc =
  if debug ()
  then
    let sz = body_size info ~env p pc in
    let sz' = full_size info ~env p pc in
    Format.eprintf
      "%a <- %a%s: %b uses:%d size:%d/%d loop:%b rec:%b closures:%d init:%d \
       return_block:%b functor:%b small:%b@."
      Code.Var.print
      x
      Code.Var.print
      f
      (match Code.Var.get_name f with
      | Some s -> "(" ^ s ^ ")"
      | None -> "")
      (should_inline ~live_vars ~env ~at_toplevel p info args f pc params)
      live_vars.(Var.idx f)
      sz
      sz'
      (contains_loop info p pc)
      (is_recursive info ~env p f pc)
      (closure_count info p pc)
      (count_init_code info p pc)
      (return_block info p pc)
      (functor_like ~env p info f pc)
      (small_function ~live_vars ~env p info args f pc params)

let inline_in_block ~live_vars env p at_toplevel pc block =
  let body, (branch, p) =
    List.fold_right
      ~f:(fun i (rem, state) ->
        match i with
        | Let (x, Apply { f; args; exact = true; _ }) when Var.Map.mem f env ->
            let info = Var.Map.find f env in
            let { params; cont = (pc, _) as cont; _ } = info in
            trace_inlining info ~live_vars ~env p at_toplevel x args f params pc;
            if should_inline ~live_vars ~env ~at_toplevel p info args f pc params
            then
              let branch, p = state in
              if live_vars.(Var.idx f) > 1
              then (
                let p = remove_dead_closures ~env p pc in
                let p, _, params, cont = Duplicate.closure p ~f ~params ~cont in
                live_vars.(Var.idx f) <- live_vars.(Var.idx f) - 1;
                inline_function p rem branch x params cont args)
              else (
                info.dead <- true;
                inline_function p rem branch x params cont args)
            else i :: rem, state
        | _ -> i :: rem, state)
      ~init:([], (block.branch, p))
      block.body
  in
  { p with blocks = Addr.Map.add pc { block with body; branch } p.blocks }

let inline p ~live_vars =
  if debug () then Format.eprintf "====== inlining ======@.";
  fst
    (Code.fold_closures_in_reverse_postorder
       p
       (fun name_opt params ((pc, _) as cont) (p, env) ->
         let at_toplevel = Option.is_none name_opt in
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
               else inline_in_block ~live_vars env p at_toplevel pc block)
             pc
             p.blocks
             p
         in
         let env =
           match name_opt with
           | Some f ->
               Var.Map.add
                 f
                 { params
                 ; cont
                 ; loops = ref None
                 ; body_size = ref None
                 ; full_size = ref None
                 ; closure_count = ref None
                 ; init_code = ref None
                 ; recursive = ref None
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
  let t = Timer.make () in
  Print.program (fun _ _ -> "") p;
  let p = inline p ~live_vars in
  if times () then Format.eprintf "  inlining: %a@." Timer.print t;
  Print.program (fun _ _ -> "") p;
  Code.invariant p;
  p
