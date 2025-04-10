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

(*
JavaScript:
- Don't inline function containing closures (except at toplevel)

Always:
- Don't inline if Config.Flag.debugger and contains a debugger statement
  (either inlinee or inliner)
- Don't inline loops into large functions

Wasm:
- Don't inline loops (no tear-up)

Try to detect functor applications and inline them aggressively
==> straight line body, returning a block
==> body is mostly field accesses / closures (but not too large) / constants

Small functions:
- there is a parameter which is a function used once and called once
  in the body
- known array as input, used once, accessed in function (???)

Don't attempt to inline a not yet visited function
*)

let straightline_code { blocks; _ } pc =
  let rec follow visited pc =
    if Addr.Set.mem pc visited
    then None
    else
      let block = Addr.Map.find pc blocks in
      match block.branch with
      | Return x -> Some x
      | Branch (pc', _) -> follow (Addr.Set.add pc visited) pc'
      | Raise _ | Stop | Cond _ | Switch _ | Pushtrap _ | Poptrap _ -> None
  in
  follow Addr.Set.empty pc

let contains_loop { blocks; _ } pc =
  let rec traverse pc ((visited, loop) as accu) =
    if loop
    then accu
    else if Addr.Map.mem pc visited
    then visited, Addr.Map.find pc visited
    else
      let visited, loop =
        Code.fold_children blocks pc traverse (Addr.Map.add pc false visited, false)
      in
      Addr.Map.add pc true visited, loop
  in
  snd (traverse pc (Addr.Map.empty, false))

let sum f { blocks; _ } pc =
  Code.traverse
    { fold = fold_children }
    (fun pc acc -> f (Addr.Map.find pc blocks) + acc)
    pc
    blocks
    0

let rec block_size ~recurse blocks { branch; body; _ } =
  List.fold_left
    ~f:(fun n i ->
      match i with
      | Event _ -> n
      | Let (_, Closure (_, (pc, _))) ->
          n + 1 + if recurse then size ~recurse blocks pc else 0
      | _ -> n + 1)
    ~init:0
    body
  +
  match branch with
  | Cond _ -> 2
  | Switch (_, a1) -> Array.length a1
  | _ -> 0

and size ?(recurse = true) blocks pc = sum (block_size ~recurse blocks) blocks pc

let field_accesses =
  sum
  @@ fun { body; _ } ->
  List.fold_left
    ~f:(fun n i ->
      match i with
      | Let (_, Field _) -> n + 1
      | _ -> n)
    ~init:0
    body

let count_closures =
  sum
  @@ fun { body; _ } ->
  List.fold_left
    ~f:(fun n i ->
      match i with
      | Let (_, Closure _) -> n + 1
      | _ -> n)
    ~init:0
    body

let contains_closure { blocks; _ } pc =
  Code.traverse
    { fold = fold_children }
    (fun pc acc ->
      acc
      ||
      let block = Addr.Map.find pc blocks in
      List.exists
        ~f:(fun i ->
          match i with
          | Let (_, Closure _) -> true
          | _ -> false)
        block.body)
    pc
    blocks
    false

let stats p live_vars defs =
  Format.eprintf "==================@.";
  ignore
    (Code.fold_closures_in_reverse_postorder
       p
       (fun _name_opt _ (pc, _) env ->
         Code.traverse
           { fold = Code.fold_children }
           (fun pc env ->
             let block = Addr.Map.find pc p.blocks in
             List.fold_left
               ~f:(fun env i ->
                 match i with
                 | Let (_, Apply { f; _ }) when not (Var.Set.mem f env) ->
                     (match defs.(Var.idx f) with
                     | [ Deadcode.Expr (Closure (_, (pc, _))) ] ->
                         let sz = size p pc in
                         let ret = straightline_code p pc in
                         let return_block =
                           match ret with
                           | None -> false
                           | Some x -> (
                               match defs.(Var.idx x) with
                               | [ Expr (Block _) ] -> true
                               | _ -> false)
                         in
                         let sz' = size ~recurse:false p pc in
                         Format.eprintf
                           "%a%s: uses:%d straight:%b block:%b loop:%b clos:%b \
                            size:%d/%d closures:%d fields:%.0f(%.0f)@."
                           Code.Var.print
                           f
                           (match Code.Var.get_name f with
                           | Some s -> "_" ^ s
                           | None -> "")
                           live_vars.(Var.idx f)
                           (Option.is_some ret)
                           return_block
                           (contains_loop p pc)
                           (contains_closure p pc)
                           sz
                           sz'
                           (count_closures p pc)
                           (float (field_accesses p pc) /. float sz *. 100.)
                           (float (field_accesses p pc + count_closures p pc)
                           /. float sz'
                           *. 100.)
                     | _ -> ());
                     Var.Set.add f env
                 | _ -> env)
               ~init:env
               block.body)
           pc
           p.blocks
           env)
       Var.Set.empty)

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

let rec args_equal xs ys =
  match xs, ys with
  | [], [] -> true
  | x :: xs, Pv y :: ys -> Code.Var.compare x y = 0 && args_equal xs ys
  | _ -> false

let inline ~first_class_primitives live_vars closures name pc (outer, p) =
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
        | Let (x, Closure (l, (pc, []))) when first_class_primitives -> (
            let block = Addr.Map.find pc p.blocks in
            match block with
            | { body =
                  ( [ Let (y, Prim (Extern prim, args)) ]
                  | [ Event _; Let (y, Prim (Extern prim, args)) ]
                  | [ Event _; Let (y, Prim (Extern prim, args)); Event _ ] )
              ; branch = Return y'
              ; params = []
              } ->
                let len = List.length l in
                if
                  Code.Var.compare y y' = 0
                  && Primitive.has_arity prim len
                  && args_equal l args
                then Let (x, Special (Alias_prim prim)) :: rem, state
                else i :: rem, state
            | _ -> i :: rem, state)
        | _ -> i :: rem, state)
  in
  outer, { p with blocks = Addr.Map.add pc { block with body; branch } p.blocks }

(****)

let times = Debug.find "times"

let f p live_vars defs =
  stats p live_vars defs;
  let first_class_primitives =
    match Config.target (), Config.effects () with
    | `JavaScript, `Disabled -> true
    | `JavaScript, (`Cps | `Double_translation) | `Wasm, _ -> false
    | `JavaScript, `Jspi -> assert false
  in
  Code.invariant p;
  let t = Timer.make () in
  let closures = get_closures p in
  let _closures, p =
    Code.fold_closures_innermost_first
      p
      (fun name cl_params (pc, _) (closures, p) ->
        let traverse outer =
          Code.traverse
            { fold = Code.fold_children }
            (inline ~first_class_primitives live_vars closures name)
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
  if times () then Format.eprintf "  inlining: %a@." Timer.print t;
  Code.invariant p;
  p
