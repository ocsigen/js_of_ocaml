(* Js_of_ocaml compiler
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

(* The following CPS transform is based on the one proposed in D.
   Hillerström, S. Lindley, R. Atkey, and K. C. Sivaramakrishnan,
   “Continuation Passing Style for Effect Handlers” (FSCD 2017), with
   adaptations to account for exception handlers (which are not
   considered in detail in the paper) and for the fact that the
   language is an SSA form rather than a classical lambda calculus.

   Rather than using a stack of continuations, and effect and
   exception handlers, only the current continuation is passed between
   functions, while exception handlers and effect handlers are stored
   in global variables. This avoid having to manipulate the stack each
   time the current continuation changes. This also allows us to deal
   with exceptions from the runtime or from JavaScript code (a [try
   ... with] at the top of stack can have access to the current
   exception handler and resume the execution from there; see the
   definition of runtime function [caml_callback]).
*)
open! Stdlib
open Code

let debug = Debug.find "effects"

type control_flow_graph =
  { succs : (Addr.t, Addr.Set.t) Hashtbl.t
  ; predecessor_count : (Addr.t, int) Hashtbl.t
  ; loop_headers : (Addr.t, unit) Hashtbl.t
  ; reverse_post_order : Addr.t list
  ; block_order : (Addr.t, int) Hashtbl.t
  ; matching_exn_handler : (Addr.t, Addr.t) Hashtbl.t
  }

let add_edge g src dst =
  Hashtbl.replace
    g
    src
    (Addr.Set.add dst (try Hashtbl.find g src with Not_found -> Addr.Set.empty))

let build_graph blocks pc =
  let succs = Hashtbl.create 16 in
  let predecessor_count = Hashtbl.create 16 in
  let loop_headers = Hashtbl.create 16 in
  let matching_exn_handler = Hashtbl.create 16 in
  let l = ref [] in
  let visited = Hashtbl.create 16 in
  let update_pred_count pc =
    Hashtbl.replace
      predecessor_count
      pc
      (1 + try Hashtbl.find predecessor_count pc with Not_found -> 0)
  in
  let rec traverse ancestors exn_handler_stack pc =
    if not (Hashtbl.mem visited pc)
    then (
      Hashtbl.add visited pc ();
      let exn_handler_stack =
        let block = Addr.Map.find pc blocks in
        match block.branch with
        | Pushtrap (_, _, (pc, _), _) -> pc :: exn_handler_stack
        | Poptrap _ -> (
            match exn_handler_stack with
            | pc' :: rem ->
                Hashtbl.add matching_exn_handler pc pc';
                rem
            | [] -> assert false)
        | Raise _ -> (
            match exn_handler_stack with
            | pc' :: rem ->
                Hashtbl.add matching_exn_handler pc pc';
                rem
            | [] -> exn_handler_stack)
        | _ -> exn_handler_stack
      in
      let successors = Code.fold_children blocks pc Addr.Set.add Addr.Set.empty in
      Hashtbl.add succs pc successors;
      let ancestors = Addr.Set.add pc ancestors in
      Addr.Set.iter
        (fun pc' ->
          if Addr.Set.mem pc' ancestors
          then Hashtbl.replace loop_headers pc' ()
          else update_pred_count pc')
        successors;
      Addr.Set.iter (fun pc -> traverse ancestors exn_handler_stack pc) successors;
      l := pc :: !l)
  in
  traverse Addr.Set.empty [] pc;
  let block_order = Hashtbl.create 16 in
  List.iteri !l ~f:(fun i pc -> Hashtbl.add block_order pc i);
  { succs
  ; predecessor_count
  ; loop_headers
  ; reverse_post_order = !l
  ; block_order
  ; matching_exn_handler
  }

let dominator_tree g =
  (* A Simple, Fast Dominance Algorithm
     Keith D. Cooper, Timothy J. Harvey, and Ken Kennedy *)
  let dom = Hashtbl.create 16 in
  let rec inter pc pc' =
    (* Compute closest common ancestor *)
    if pc = pc'
    then pc
    else if Hashtbl.find g.block_order pc < Hashtbl.find g.block_order pc'
    then inter pc (Hashtbl.find dom pc')
    else inter (Hashtbl.find dom pc) pc'
  in
  List.iter g.reverse_post_order ~f:(fun pc ->
      let l = Hashtbl.find g.succs pc in
      Addr.Set.iter
        (fun pc' ->
          let d = try inter pc (Hashtbl.find dom pc') with Not_found -> pc in
          Hashtbl.replace dom pc' d)
        l);
  (* Check we have reached a fixed point (reducible graph) *)
  List.iter g.reverse_post_order ~f:(fun pc ->
      let l = Hashtbl.find g.succs pc in
      Addr.Set.iter
        (fun pc' ->
          let d = Hashtbl.find dom pc' in
          assert (inter pc d = d))
        l);
  dom

let reverse_tree g =
  let g' = Hashtbl.create 16 in
  Hashtbl.iter (fun child parent -> add_edge g' parent child) g;
  g'

let is_merge_node cfg pc = Hashtbl.find cfg.predecessor_count pc > 1

(*
   The continuation of an application potentially with effect should
   be transformed.
   If we transform a block, we need to transform the merge nodes
   below.
   If we transform a block in a [try ... with ...] body or in a loop,
   we need to transform the initial block of the body / the loop.
*)
let compute_transformed_blocks ~cfg ~idom ~cps_needed ~blocks ~start =
  let dom_tree = reverse_tree idom in
  let transformation_needed = ref Addr.Set.empty in
  let is_continuation = Hashtbl.create 16 in
  let mark_needed pc = transformation_needed := Addr.Set.add pc !transformation_needed in
  let mark_continuation pc x =
    Hashtbl.add
      is_continuation
      pc
      (if Hashtbl.mem is_continuation pc then `Multiple else `Single x)
  in
  let rec traverse pc ~try_blocks ~frontier ~transform_on_visit =
    (* [try_blocks] is the set of initial blocks of try blocks we are in.
       [frontier] is a superset of the dominance frontier. *)
    let try_header =
      let block = Addr.Map.find pc blocks in
      match block.branch with
      | Branch (dst, _) -> (
          match List.last block.body with
          | Some
              (Let
                (x, (Apply _ | Prim (Extern ("%resume" | "%perform" | "%reperform"), _))))
            when Var.Set.mem x cps_needed ->
              (* If the blocks ends with a CPS call, its
                 continuation needs to be transformed *)
              mark_needed dst;
              (* We need to transform the head of try blocks as well *)
              List.iter ~f:mark_needed try_blocks;
              mark_continuation dst x;
              None
          | _ -> None)
      | Return _ ->
          (*ZZZ Can this actually happen?*)
          List.iter ~f:mark_needed try_blocks;
          None
      | Pushtrap (_, x, (pc2, _), _) ->
          mark_continuation pc2 x;
          Some pc2
      | _ -> None
    in
    let merge_node_children =
      let children = try Hashtbl.find dom_tree pc with Not_found -> Addr.Set.empty in
      Addr.Set.filter (fun pc -> is_merge_node cfg pc) children
    in
    let frontier =
      let frontier = Addr.Set.union merge_node_children frontier in
      if Hashtbl.mem cfg.loop_headers pc then Addr.Set.add pc frontier else frontier
    in
    let successors = Hashtbl.find cfg.succs pc in
    let transform_on_visit =
      if Addr.Set.mem pc !transformation_needed then frontier else transform_on_visit
    in
    Addr.Set.iter
      (fun pc' ->
        traverse_branch pc' ~try_header ~try_blocks ~frontier ~transform_on_visit)
      successors;
    (* Traverse merge node children *)
    let l = Addr.Set.elements merge_node_children in
    let l =
      List.sort
        ~cmp:(fun pc pc' ->
          compare (Hashtbl.find cfg.block_order pc) (Hashtbl.find cfg.block_order pc'))
        l
    in
    ignore
      (List.fold_left l ~init:frontier ~f:(fun frontier pc' ->
           let frontier = Addr.Set.remove pc' frontier in
           traverse pc' ~try_blocks ~frontier ~transform_on_visit;
           frontier))
  and traverse_branch pc ~try_header ~try_blocks ~frontier ~transform_on_visit =
    if Addr.Set.mem pc frontier
    then (if Addr.Set.mem pc transform_on_visit then mark_needed pc)
    else
      let try_blocks =
        match try_header with
        | Some pc' when pc' <> pc -> pc' :: try_blocks
        | _ -> try_blocks
      in
      traverse pc ~try_blocks ~frontier ~transform_on_visit
  in
  traverse
    start
    ~try_blocks:[]
    ~frontier:Addr.Set.empty
    ~transform_on_visit:Addr.Set.empty;
  !transformation_needed, is_continuation

(* Each block is turned into a function which is defined in the
   dominator of the block. [closure_of_jump] provides the name of the
   function correspoding to each block. [closures_of_alloc_site]
   provides the list of functions which should be defined in a given
   block. Exception handlers are dealt with separately.
*)
type jump_closures =
  { closure_of_jump : Var.t Addr.Map.t
  ; closures_of_alloc_site : (Var.t * Addr.t) list Addr.Map.t
  }

let jump_closures blocks_to_transform idom : jump_closures =
  Hashtbl.fold
    (fun node idom_node jc ->
      if not (Addr.Set.mem node blocks_to_transform)
      then jc
      else
        let cname = Var.fresh () in
        { closure_of_jump = Addr.Map.add node cname jc.closure_of_jump
        ; closures_of_alloc_site =
            Addr.Map.add
              idom_node
              ((cname, node)
              ::
              (try Addr.Map.find idom_node jc.closures_of_alloc_site
               with Not_found -> []))
              jc.closures_of_alloc_site
        })
    idom
    { closure_of_jump = Addr.Map.empty; closures_of_alloc_site = Addr.Map.empty }

type st =
  { mutable new_blocks : Code.block Addr.Map.t * Code.Addr.t
  ; blocks : Code.block Addr.Map.t
  ; jc : jump_closures
  ; closure_info : (Addr.t, Var.t * Code.cont) Hashtbl.t
  ; cps_needed : Var.Set.t
  ; blocks_to_transform : Addr.Set.t
  ; is_continuation : (Addr.t, [ `Single of Var.t | `Multiple ]) Hashtbl.t
  ; tail_calls : Var.Set.t ref
  ; matching_exn_handler : (Addr.t, Addr.t) Hashtbl.t
  ; loop_headers : (Addr.t, unit) Hashtbl.t
  ; live_vars : int array
  }

let add_block st block =
  let blocks, free_pc = st.new_blocks in
  st.new_blocks <- Addr.Map.add free_pc block blocks, free_pc + 1;
  free_pc

let closure_of_pc ~st pc =
  try Addr.Map.find pc st.jc.closure_of_jump with Not_found -> assert false

let allocate_closure ~st ~params ~body:(body, branch) =
  let block = { params = []; body; branch } in
  let pc = add_block st block in
  let name = Var.fresh () in
  [ Let (name, Closure (params, (pc, []))) ], name

let tail_call ~st ?(instrs = []) ~f ?(check = true) ?(exact = true) args =
  let ret = Var.fresh () in
  if check then st.tail_calls := Var.Set.add ret !(st.tail_calls);
  instrs @ [ Let (ret, Apply { f; args; exact }) ], Return ret

let cps_branch ~st (pc, args) =
  if Addr.Set.mem pc st.blocks_to_transform
  then
    let args, instrs =
      if List.is_empty args && Hashtbl.mem st.is_continuation pc
      then
        let x = Var.fresh () in
        [ x ], [ Let (x, Constant (Int 0l)) ]
      else args, []
    in
    tail_call
      ~st
      ~instrs
      ~check:(Hashtbl.mem st.loop_headers pc)
      ~f:(closure_of_pc ~st pc)
      args
  else [], Branch (pc, args)

let cps_jump_cont ~st ((pc, _) as cont) =
  if Addr.Set.mem pc st.blocks_to_transform
  then
    let call_block =
      let body, branch = cps_branch ~st cont in
      add_block st { params = []; body; branch }
    in
    call_block, []
  else cont

let cps_last ~st pc (last : last) ~k : instr list * last =
  match last with
  | Return x -> (
      match k with
      | None -> [], last
      | Some k -> tail_call ~st ~f:k [ x ])
  | Raise (x, _) -> (
      match k with
      | None -> [], last
      | Some _ -> (
          match Hashtbl.find_opt st.matching_exn_handler pc with
          | Some pc when not (Addr.Set.mem pc st.blocks_to_transform) ->
              (* We are within a try ... with which is not
                 transformed. We must raise an exception normally *)
              [], last
          | _ ->
              let exn_handler = Var.fresh_n "raise" in
              tail_call
                ~st
                ~instrs:[ Let (exn_handler, Prim (Extern "caml_pop_trap", [])) ]
                ~f:exn_handler
                [ x ]))
  | Stop -> [], Stop
  | Branch cont -> cps_branch ~st cont
  | Cond (x, cont1, cont2) ->
      [], Cond (x, cps_jump_cont ~st cont1, cps_jump_cont ~st cont2)
  | Switch (x, c1, c2) ->
      (* To avoid code duplication during JavaScript generation, we need
         to create a single block per continuation *)
      let cps_jump_cont = Fun.memoize (cps_jump_cont ~st) in
      [], Switch (x, Array.map c1 ~f:cps_jump_cont, Array.map c2 ~f:cps_jump_cont)
  | Pushtrap ((pc, args), _x, (handler_pc, _), _) ->
      if Addr.Set.mem handler_pc st.blocks_to_transform
      then
        let exn_handler = closure_of_pc ~st handler_pc in
        let push_trap =
          Let (Var.fresh (), Prim (Extern "caml_push_trap", [ Pv exn_handler ]))
        in
        if Addr.Set.mem pc st.blocks_to_transform
        then
          let body, branch = cps_branch ~st (pc, args) in
          push_trap :: body, branch
        else [ push_trap ], Branch (pc, args)
      else [], last
  | Poptrap (pc', args) ->
      if Addr.Set.mem (Hashtbl.find st.matching_exn_handler pc) st.blocks_to_transform
      then
        let instrs =
          let exn_handler = Var.fresh () in
          [ Let (exn_handler, Prim (Extern "caml_pop_trap", [])) ]
        in
        if Addr.Set.mem pc' st.blocks_to_transform
        then
          let body, branch = cps_branch ~st (pc', args) in
          instrs @ body, branch
        else instrs, Branch (pc', args)
      else if Addr.Set.mem pc' st.blocks_to_transform
      then
        let pc'' =
          let body, branch = cps_branch ~st (pc', args) in
          add_block st { params = []; body; branch }
        in
        (*ZZZ pop trap + cps call*)
        [], Poptrap (pc'', [])
      else [], last

let cps_instr ~st (instr : instr) : instr =
  match instr with
  | Let (x, Closure (params, (pc, _))) when Var.Set.mem x st.cps_needed ->
      let k, cont = Hashtbl.find st.closure_info pc in
      Let (x, Closure (params @ [ k ], cont))
  | Let (x, Prim (Extern "caml_alloc_dummy_function", [ size; arity ])) -> (
      match arity with
      | Pc (Int a) ->
          Let
            ( x
            , Prim (Extern "caml_alloc_dummy_function", [ size; Pc (Int (Int32.succ a)) ])
            )
      | _ -> assert false)
  | Let (x, (Apply _ | Prim (Extern ("%resume" | "%perform" | "%reperform"), _)))
    when Var.Set.mem x st.cps_needed -> assert false
  | _ -> instr

let cps_block ~st ~k pc block =
  let alloc_jump_closures =
    match Addr.Map.find pc st.jc.closures_of_alloc_site with
    | to_allocate ->
        List.map to_allocate ~f:(fun (cname, jump_pc) ->
            let params =
              let jump_block = Addr.Map.find jump_pc st.blocks in
              if List.is_empty jump_block.params && Hashtbl.mem st.is_continuation jump_pc
              then
                let x =
                  match Hashtbl.find st.is_continuation jump_pc with
                  | `Single x -> x
                  | _ -> Var.fresh ()
                in
                [ x ]
              else jump_block.params
            in
            Let (cname, Closure (params, (jump_pc, []))))
    | exception Not_found -> []
  in
  let rewrite_instr x e =
    match e with
    | Apply { f; args; exact } when Var.Set.mem x st.cps_needed ->
        Some (fun ~k -> tail_call ~st ~f ~exact (args @ [ k ]))
    | Prim (Extern "%resume", [ Pv stack; Pv f; Pv arg ]) ->
        Some
          (fun ~k ->
            let k' = Var.fresh_n "cont" in
            tail_call
              ~st
              ~instrs:[ Let (k', Prim (Extern "caml_resume_stack", [ Pv stack; Pv k ])) ]
              ~f
              ~exact:false
              [ arg; k' ])
    | Prim (Extern "%perform", [ Pv effect ]) ->
        Some
          (fun ~k ->
            let x = Var.fresh () in
            ( [ Let
                  ( x
                  , Prim (Extern "caml_perform_effect", [ Pv effect; Pc (Int 0l); Pv k ])
                  )
              ]
            , Return x ))
    | Prim (Extern "%reperform", [ Pv eff; Pv continuation ]) ->
        Some
          (fun ~k ->
            let x = Var.fresh () in
            ( [ Let
                  ( x
                  , Prim (Extern "caml_perform_effect", [ Pv eff; Pv continuation; Pv k ])
                  )
              ]
            , Return x ))
    | _ -> None
  in
  let rewritten_block =
    match List.split_last block.body, block.branch with
    | Some (body_prefix, Let (x, e)), Return ret ->
        Option.map (rewrite_instr x e) ~f:(fun f ->
            assert (List.is_empty alloc_jump_closures);
            assert (Var.equal x ret);
            let instrs, branch = f ~k in
            body_prefix, instrs, branch)
    | Some (body_prefix, Let (x, e)), Branch cont ->
        let allocate_continuation f =
          let constr_cont, k' =
            (* Construct continuation: it binds the return value [x],
               allocates closures for dominated blocks and jumps to the
               next block. *)
            let pc, args = cont in
            let f' = closure_of_pc ~st pc in
            assert (Hashtbl.mem st.is_continuation pc);
            match args with
            | []
              when match Hashtbl.find st.is_continuation pc with
                   | `Single _ -> true
                   | `Multiple -> st.live_vars.(Var.idx x) = 0 -> alloc_jump_closures, f'
            | [ x' ] when Var.equal x x' -> alloc_jump_closures, f'
            | _ ->
                let args, instrs =
                  if List.is_empty args
                  then
                    let x = Var.fresh () in
                    [ x ], alloc_jump_closures @ [ Let (x, Constant (Int 0l)) ]
                  else args, alloc_jump_closures
                in
                allocate_closure
                  ~st
                  ~params:[ x ]
                  ~body:(tail_call ~st ~instrs ~f:f' args)
          in
          let instrs, branch = f ~k:k' in
          body_prefix, constr_cont @ instrs, branch
        in
        Option.map (rewrite_instr x e) ~f:allocate_continuation
    | Some (_, (Set_field _ | Offset_ref _ | Array_set _ | Assign _)), _
    | Some _, (Raise _ | Stop | Cond _ | Switch _ | Pushtrap _ | Poptrap _)
    | None, _ -> None
  in
  let body, last =
    match rewritten_block with
    | Some (body_prefix, last_instrs, last) ->
        List.map body_prefix ~f:(fun i -> cps_instr ~st i) @ last_instrs, last
    | None ->
        let last_instrs, last = cps_last ~st pc block.branch ~k:(Some k) in
        let body =
          List.map block.body ~f:(fun i -> cps_instr ~st i)
          @ alloc_jump_closures
          @ last_instrs
        in
        body, last
  in
  { params = (if Addr.Set.mem pc st.blocks_to_transform then [] else block.params)
  ; body
  ; branch = last
  }

let transform_block ~st ~k pc block =
  match k with
  | Some k -> cps_block ~st ~k pc block
  | _ ->
      let last_instrs, last = cps_last ~st pc block.branch ~k in
      { params = block.params
      ; body = List.map block.body ~f:(fun i -> cps_instr ~st i) @ last_instrs
      ; branch = last
      }

let split_blocks ~cps_needed (p : Code.program) =
  (* Ensure that function applications and effect primitives are in
     tail position *)
  let split_block pc block p =
    let is_split_point i r branch =
      match i with
      | Let (x, (Apply _ | Prim (Extern ("%resume" | "%perform" | "%reperform"), _))) ->
          ((not (List.is_empty r))
          ||
          match branch with
          | Branch _ -> false
          | Return x' -> not (Var.equal x x')
          | _ -> true)
          && Var.Set.mem x cps_needed
      | _ -> false
    in
    let rec split (p : Code.program) pc block accu l branch =
      match l with
      | [] ->
          let block = { block with body = List.rev accu } in
          { p with blocks = Addr.Map.add pc block p.blocks }
      | (Let (x, e) as i) :: r when is_split_point i r branch ->
          let pc' = p.free_pc in
          let block' = { params = []; body = []; branch = block.branch } in
          let block =
            { block with body = List.rev (Let (x, e) :: accu); branch = Branch (pc', []) }
          in
          let p = { p with blocks = Addr.Map.add pc block p.blocks; free_pc = pc' + 1 } in
          split p pc' block' [] r branch
      | i :: r -> split p pc block (i :: accu) r branch
    in
    let rec should_split l branch =
      match l with
      | [] -> false
      | i :: r -> is_split_point i r branch || should_split r branch
    in
    if should_split block.body block.branch
    then split p pc block [] block.body block.branch
    else p
  in
  Addr.Map.fold split_block p.blocks p

let skip_empty_blocks ~live_vars (p : Code.program) : Code.program =
  let shortcuts = Hashtbl.create 16 in
  let rec resolve_rec l ((pc, args) as cont) =
    if List.mem pc ~set:l
    then cont
    else
      match Hashtbl.find_opt shortcuts pc with
      | Some (params, cont) ->
          let pc', args' = resolve_rec (pc :: l) cont in
          let s = Subst.from_map (Subst.build_mapping params args) in
          pc', List.map ~f:s args'
      | None -> cont
  in
  let resolve cont = resolve_rec [] cont in
  Addr.Map.iter
    (fun pc block ->
      match block with
      | { params; body = []; branch = Branch cont; _ } ->
          (*ZZZ quadratic *)
          if List.for_all
               ~f:(fun x ->
                 live_vars.(Var.idx x) = 1
                 && List.exists ~f:(fun y -> Var.equal x y) (snd cont))
               params
          then Hashtbl.add shortcuts pc (params, cont)
      | _ -> ())
    p.blocks;
  let blocks =
    Addr.Map.map
      (fun block ->
        { block with
          branch =
            (match block.branch with
            | Branch cont -> Branch (resolve cont)
            | Cond (x, cont1, cont2) -> Cond (x, resolve cont1, resolve cont2)
            | Switch (x, a1, a2) ->
                Switch (x, Array.map ~f:resolve a1, Array.map ~f:resolve a2)
            | Pushtrap (cont1, x, cont2, s) ->
                Pushtrap (resolve cont1, x, resolve cont2, s)
            | Poptrap cont -> Poptrap (resolve cont)
            | Return _ | Raise _ | Stop -> block.branch)
        })
      p.blocks
  in
  { p with blocks }

(*
No argument:
 - merge node ==> dummy parameter (variable is not live)
 - loop header, forward edge ==> dummy parameter, allocate closure (if variable is live)
 - loop header, backward edge ==> dummy parameter (variable is not live)
 - otherwise ==> use variable name
*)

let fold_closures_innermost_first { start; blocks; _ } f accu =
  let rec traverse blocks pc f accu =
    Code.traverse
      { fold = Code.fold_children }
      (fun pc accu ->
        let block = Addr.Map.find pc blocks in
        List.fold_left block.body ~init:accu ~f:(fun accu i ->
            match i with
            | Let (x, Closure (params, cont)) ->
                let accu = traverse blocks (fst cont) f accu in
                f (Some x) params cont accu
            | _ -> accu))
      pc
      blocks
      accu
  in
  let accu = traverse blocks start f accu in
  f None [] (start, []) accu

let f (p : Code.program) =
  if debug () then Code.Print.program (fun _ _ -> "") p;
  let p, live_vars = Deadcode.f p in
  let p = skip_empty_blocks ~live_vars p in
  let p, info = Flow.f ~pessimistic:true p in
  let cps_needed = Fun_style_analysis.f (p, info) in
  let p = split_blocks ~cps_needed p in
  if debug () then Code.Print.program (fun _ _ -> "") p;
  let closure_info = Hashtbl.create 16 in
  let tail_calls = ref Var.Set.empty in
  let p =
    fold_closures_innermost_first
      p
      (fun name_opt _ (start, args) ({ blocks; free_pc; _ } as p) ->
        let start' = free_pc in
        let blocks' =
          Addr.Map.add
            free_pc
            { params = []; body = []; branch = Branch (start, args) }
            blocks
        in
        let cfg = build_graph blocks' start' in
        let idom = dominator_tree cfg in
        let function_need_cps =
          match name_opt with
          | None -> true
          | Some name -> Var.Set.mem name cps_needed
        in
        let blocks_to_transform, is_continuation =
          if function_need_cps
          then
            compute_transformed_blocks
              ~cfg
              ~idom
              ~cps_needed
              ~blocks:blocks'
              ~start:start'
          else Addr.Set.empty, Hashtbl.create 1
        in
        if debug ()
        then (
          Format.eprintf "=============== %b@." function_need_cps;
          Code.preorder_traverse
            { fold = Code.fold_children }
            (fun pc _ ->
              if Addr.Set.mem pc blocks_to_transform then Format.eprintf "CPS@.";
              let block = Addr.Map.find pc blocks in
              Code.Print.block
                (fun _ xi -> Fun_style_analysis.annot cps_needed xi)
                pc
                block)
            start
            blocks
            ());
        let closure_jc = jump_closures blocks_to_transform idom in
        let function_cont, blocks, free_pc =
          if not (Addr.Map.mem start' closure_jc.closures_of_alloc_site)
          then (start, args), blocks, free_pc
          else
            let block = { params = []; body = []; branch = Branch (start, args) } in
            (start', []), Addr.Map.add start' block blocks, free_pc + 1
        in
        let st =
          { new_blocks = Addr.Map.empty, free_pc
          ; blocks
          ; jc = closure_jc
          ; closure_info
          ; cps_needed
          ; blocks_to_transform
          ; is_continuation
          ; tail_calls
          ; matching_exn_handler = cfg.matching_exn_handler
          ; loop_headers = cfg.loop_headers
          ; live_vars
          }
        in

        let k = Var.fresh_n "cont" in
        Hashtbl.add closure_info start (k, function_cont);
        let start = fst function_cont in

        let k_opt = if function_need_cps then Some k else None in
        let blocks =
          Code.traverse
            { fold = Code.fold_children }
            (fun pc blocks ->
              Addr.Map.add
                pc
                (transform_block ~st ~k:k_opt pc (Addr.Map.find pc blocks))
                blocks)
            start
            st.blocks
            st.blocks
        in
        let new_blocks, free_pc = st.new_blocks in
        let blocks = Addr.Map.fold Addr.Map.add new_blocks blocks in
        { p with blocks; free_pc })
      p
  in

  (* Call [caml_callback] to set up the execution context. *)
  let new_start = p.free_pc in
  let blocks =
    let main = Var.fresh () in
    let args = Var.fresh () in
    let res = Var.fresh () in
    Addr.Map.add
      new_start
      { params = []
      ; body =
          [ Let
              (main, Closure ([ fst (Hashtbl.find closure_info p.start) ], (p.start, [])))
          ; Let (args, Prim (Extern "%js_array", []))
          ; Let (res, Prim (Extern "caml_callback", [ Pv main; Pv args ]))
          ]
      ; branch = Return res
      }
      p.blocks
  in
  { start = new_start; blocks; free_pc = new_start + 1 }, !tail_calls

let f p =
  let t = Timer.make () in
  let r = f p in
  if Debug.find "times" () then Format.eprintf "  effects: %a@." Timer.print t;
  r
