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

(*
JavaScript:
- Don't inline function containing closures (except at toplevel)

Always:
- Don't inline loops at toplevel

Wasm:
- Don't inline loops (no tear-up)

Small functions:
- there is a parameter which is a function used once, containing no closure
  and called once in the body
- not recursive
- small enough
- no closures

Don't attempt to inline a not yet visited function

Functor
- no loop
- returns an array
- many closures (but not tool large) / constants / blocks / array accesses
===> inline aggressively
*)

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
  | Some v ->
      prerr_endline "found";
      v
  | None ->
      prerr_endline "not found";
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
    (fun pc acc ->
      Format.eprintf ">> %d@." pc;
      f (Addr.Map.find pc blocks) + acc)
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

let body_size info ~env =
  prerr_endline "body_size";
  cache info.body_size (fun p pc -> size ~recurse:false ~env p pc)

let full_size info ~env =
  prerr_endline "full_size";
  cache info.full_size (fun p pc -> size ~recurse:true ~env p pc)

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
            Format.eprintf "REC %d@." pc;
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

let return_block info ~defs =
  cache info.return_block (fun { blocks; _ } pc ->
      Code.traverse
        { fold = fold_children }
        (fun pc acc ->
          Format.eprintf "RET %d@." pc;
          acc
          &&
          let block = Addr.Map.find pc blocks in
          match block.branch with
          | Return x -> (
              let idx = Var.idx x in
              idx < Array.length defs
              &&
              match defs.(idx) with
              | [ Deadcode.Expr (Block _) ] -> true
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
            Format.eprintf "INT %d@." pc;
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
  Format.eprintf "sf %d@." pc;
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

let functor_like ~defs ~env p info f pc =
  (not (contains_loop info p pc))
  && return_block info ~defs p pc
  && count_init_code info p pc * 2 > body_size info ~env p pc
  && (not (is_recursive info ~env p f pc))
  && full_size info ~env p pc < 20 * closure_count info p pc

let should_inline ~live_vars ~defs ~env p ~at_toplevel info args f pc params =
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
     || functor_like ~defs ~env p info f pc
     || body_size info ~env p pc = 1
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
  Format.eprintf "CONV %d@." fresh_addr;
  [], (Branch (fresh_addr, args), { p with blocks; free_pc })

let trace_inlining info ~live_vars ~defs ~env p at_toplevel x args f params pc =
  if debug ()
  then
    let sz = body_size info ~env p pc in
    let sz' = full_size info ~env p pc in
    Format.eprintf
      "%a <- %a%s: %b uses:%d size:%d/%d loop:%b rec:%b clos:%d init:%d bl:%b functor:%b \
       small:%b@."
      Code.Var.print
      x
      Code.Var.print
      f
      (match Code.Var.get_name f with
      | Some s -> "(" ^ s ^ ")"
      | None -> "")
      (should_inline ~live_vars ~defs ~env ~at_toplevel p info args f pc params)
      live_vars.(Var.idx f)
      sz
      sz'
      (contains_loop info p pc)
      (is_recursive info ~env p f pc)
      (closure_count info p pc)
      (count_init_code info p pc)
      (return_block info ~defs p pc)
      (functor_like ~defs ~env p info f pc)
      (small_function ~live_vars ~env p info args f pc params)

let inline_in_block ~live_vars ~defs env p at_toplevel pc block =
  Format.eprintf "ZZZ %d@." pc;
  let body, (branch, p) =
    List.fold_right
      ~f:(fun i (rem, state) ->
        match i with
        | Let (x, Apply { f; args; exact = true; _ }) when Var.Map.mem f env ->
            Format.eprintf "CONSIDERING %a@." Code.Var.print f;
            let info = Var.Map.find f env in
            let { params; cont = (pc, _) as cont; _ } = info in
            trace_inlining info ~live_vars ~defs ~env p at_toplevel x args f params pc;
            if should_inline ~live_vars ~defs ~env ~at_toplevel p info args f pc params
            then
              let branch, p = state in
              if live_vars.(Var.idx f) > 1
              then
                let p, _, params, cont = Duplicate.cl p ~f ~params ~cont in
                inline_function p rem branch x params cont args
              else (
                info.dead <- true;
                inline_function p rem branch x params cont args)
            else i :: rem, state
        | _ -> i :: rem, state)
      ~init:([], (block.branch, p))
      block.body
  in
  { p with blocks = Addr.Map.add pc { block with body; branch } p.blocks }

let stats p ~live_vars ~defs =
  if debug () then Format.eprintf "====== inlining ======@.";
  fst
    (Code.fold_closures_in_reverse_postorder
       p
       (fun name_opt params ((pc, _) as cont) (p, env) ->
         Option.iter
           ~f:(fun nm -> Format.eprintf "ENTERING %a@." Code.Var.print nm)
           name_opt;
         let at_toplevel = Option.is_none name_opt in
         let p =
           Code.traverse
             { fold = Code.fold_children }
             (fun pc p ->
               let block = Addr.Map.find pc p.blocks in
               prerr_endline "AAA";
               if
                 (* Skip blocks with no call of known function *)
                 List.for_all
                   ~f:(fun i ->
                     match i with
                     | Let (_, Apply { f; exact = true; _ }) -> not (Var.Map.mem f env)
                     | _ -> true)
                   block.body
               then p
               else inline_in_block ~live_vars ~defs env p at_toplevel pc block)
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

type prop =
  { size : int
  ; optimizable : bool
  }

type closure_info =
  { cl_params : Var.t list
  ; cl_cont : int * Var.t list
  ; cl_prop : prop
  ; cl_simpl : (Var.Set.t * int Var.Map.t * bool * Var.Set.t) option
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
                if Code.Var.equal x y && List.mem f ~set:params
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
    Some (!bound_vars, !free_vars, Var.Map.mem name !free_vars, !tc)
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
          | Let (x, Closure (cl_params, cl_cont)) ->
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

let inline live_vars closures name pc (outer, p) =
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
            then
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
              [], (outer, Branch (fresh_addr, args), { p with blocks; free_pc })
            else
              match cl_simpl with
              | Some (bound_vars, free_vars, recursive, tc_params)
              (* We inline/duplicate
                 - single instruction functions (f_size = 1)
                 - small funtions that call one of their arguments in
                   tail position when the argument is a direct closure
                   used only once. *)
                when (Code.Var.Set.exists
                        (fun x ->
                          let farg_tc = Var.Map.find x map_param_to_arg in
                          Var.Map.mem farg_tc closures && live_vars.(Var.idx farg_tc) = 1)
                        tc_params
                     || f_size <= 1)
                     && ((not recursive)
                        ||
                        match name with
                        | None -> true
                        | Some f' -> not (Var.equal f f')) ->
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
                  let p, f, params, clos_cont =
                    let bound_vars = Var.Set.add f bound_vars in
                    Duplicate.closure p ~bound_vars ~f ~params ~cont:clos_cont
                  in
                  if recursive
                  then
                    ( Let (f, Closure (params, clos_cont))
                      :: Let (x, Apply { f; args; exact = true })
                      :: rem
                    , (outer, branch, p) )
                  else
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
                    [], (outer, Branch (fresh_addr, args), { p with blocks; free_pc })
              | _ -> i :: rem, state)
        | _ -> i :: rem, state)
  in
  outer, { p with blocks = Addr.Map.add pc { block with body; branch } p.blocks }

(****)

let f p live_vars defs =
  Code.invariant p;
  let t = Timer.make () in
  Print.program (fun _ _ -> "") p;
  let p =
    if true
    then stats p ~live_vars ~defs
    else
      let closures = get_closures p in
      let _closures, p =
        Code.fold_closures_innermost_first
          p
          (fun name cl_params (pc, _) (closures, p) ->
            let traverse outer =
              Code.traverse
                { fold = Code.fold_children }
                (inline live_vars closures name)
                pc
                p.blocks
                (outer, p)
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
      p
  in
  if times () then Format.eprintf "  inlining: %a@." Timer.print t;
  Print.program (fun _ _ -> "") p;
  Code.invariant p;
  p
