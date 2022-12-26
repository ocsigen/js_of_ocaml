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

let get_edges g src = try Hashtbl.find g src with Not_found -> Addr.Set.empty

let add_edge g src dst = Hashtbl.replace g src (Addr.Set.add dst (get_edges g src))

let reverse_graph g =
  let g' = Hashtbl.create 16 in
  Hashtbl.iter
    (fun child parents -> Addr.Set.iter (fun parent -> add_edge g' parent child) parents)
    g;
  g'

type control_flow_graph =
  { succs : (Addr.t, Addr.Set.t) Hashtbl.t
  ; loop_headers : (Addr.t, unit) Hashtbl.t
  ; reverse_post_order : Addr.t list
  }

let build_graph blocks pc =
  let succs = Hashtbl.create 16 in
  let loop_headers = Hashtbl.create 16 in
  let l = ref [] in
  let visited = Hashtbl.create 16 in
  let rec traverse ancestors pc =
    if not (Hashtbl.mem visited pc)
    then (
      Hashtbl.add visited pc ();
      let successors = Code.fold_children blocks pc Addr.Set.add Addr.Set.empty in
      Hashtbl.add succs pc successors;
      let ancestors = Addr.Set.add pc ancestors in
      Addr.Set.iter
        (fun pc' ->
          if Addr.Set.mem pc' ancestors then Hashtbl.replace loop_headers pc' ())
        successors;
      Addr.Set.iter (fun pc -> traverse ancestors pc) successors;
      l := pc :: !l)
  in
  traverse Addr.Set.empty pc;
  { succs; loop_headers; reverse_post_order = !l }

let dominator_tree g =
  (* A Simple, Fast Dominance Algorithm
     Keith D. Cooper, Timothy J. Harvey, and Ken Kennedy *)
  let dom = Hashtbl.create 16 in
  let order = Hashtbl.create 16 in
  List.iteri g.reverse_post_order ~f:(fun i pc -> Hashtbl.add order pc i);
  let rec inter pc pc' =
    (* Compute closest common ancestor *)
    if pc = pc'
    then pc
    else if Hashtbl.find order pc < Hashtbl.find order pc'
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

let dominance_frontier g idom =
  let preds = reverse_graph g.succs in
  let frontiers = Hashtbl.create 16 in
  Hashtbl.iter
    (fun pc preds ->
      if Addr.Set.cardinal preds > 1
      then
        let dom = Hashtbl.find idom pc in
        let rec loop runner =
          if runner <> dom
          then (
            add_edge frontiers runner pc;
            loop (Hashtbl.find idom runner))
        in
        Addr.Set.iter loop preds)
    preds;
  frontiers

(****)

(*
We establish the list of blocks that needs to be CPS-transformed. We
also mark blocks that correspond to function continuations or
exception handlers. And we keep track of the exception handler
associated to each Poptrap, and possibly Raise.
*)
let compute_needed_transformations ~cfg ~idom ~cps_needed ~blocks ~start =
  let frontiers = dominance_frontier cfg idom in
  let transformation_needed = ref Addr.Set.empty in
  let matching_exn_handler = Hashtbl.create 16 in
  let is_continuation = Hashtbl.create 16 in
  let rec mark_needed pc =
    (* If a block is transformed, all the blocks in its dominance
       frontier needs to be transformed as well. *)
    if not (Addr.Set.mem pc !transformation_needed)
    then (
      transformation_needed := Addr.Set.add pc !transformation_needed;
      Addr.Set.iter mark_needed (get_edges frontiers pc))
  in
  let mark_continuation pc x =
    if not (Hashtbl.mem is_continuation pc)
    then
      Hashtbl.add
        is_continuation
        pc
        (if Addr.Set.mem pc (get_edges frontiers pc) then `Loop else `Param x)
  in
  let rec traverse visited ~englobing_exn_handlers pc =
    if Addr.Set.mem pc visited
    then visited
    else
      let visited = Addr.Set.add pc visited in
      let block = Addr.Map.find pc blocks in
      (match block.branch with
      | Branch (dst, _) -> (
          match List.last block.body with
          | Some
              (Let
                (x, (Apply _ | Prim (Extern ("%resume" | "%perform" | "%reperform"), _))))
            when Var.Set.mem x cps_needed ->
              (* The block after a function application that needs to
                 be turned to CPS or an effect primitive needs to be
                 transformed. *)
              mark_needed dst;
              (* We need to transform the englobing exception handlers
                 as well *)
              List.iter ~f:mark_needed englobing_exn_handlers;
              mark_continuation dst x
          | _ -> ())
      | Pushtrap (_, x, (handler_pc, _), _) -> mark_continuation handler_pc x
      | Poptrap _ | Raise _ -> (
          match englobing_exn_handlers with
          | handler_pc :: _ -> Hashtbl.add matching_exn_handler pc handler_pc
          | _ -> ())
      | _ -> ());
      let englobing_exn_handlers =
        match block.branch with
        | Pushtrap (_, _, (handler_pc, _), _) -> handler_pc :: englobing_exn_handlers
        | Poptrap _ -> List.tl englobing_exn_handlers
        | _ -> englobing_exn_handlers
      in
      Code.fold_children
        blocks
        pc
        (fun pc visited -> traverse visited ~englobing_exn_handlers pc)
        visited
  in
  ignore @@ traverse Addr.Set.empty ~englobing_exn_handlers:[] start;
  !transformation_needed, matching_exn_handler, is_continuation

(****)

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
  ; is_continuation : (Addr.t, [ `Param of Var.t | `Loop ]) Hashtbl.t
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
        (* We are jumping to a block that is also used as a continuation.
           We pass it a dummy argument. *)
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
  | Return x -> tail_call ~st ~f:k [ x ]
  | Raise (x, _) -> (
      match Hashtbl.find_opt st.matching_exn_handler pc with
      | Some pc when not (Addr.Set.mem pc st.blocks_to_transform) ->
          (* We are within a try ... with which is not
             transformed. We should raise an exception normally *)
          [], last
      | _ ->
          let exn_handler = Var.fresh_n "raise" in
          tail_call
            ~st
            ~instrs:[ Let (exn_handler, Prim (Extern "caml_pop_trap", [])) ]
            ~f:exn_handler
            [ x ])
  | Stop -> [], Stop
  | Branch cont -> cps_branch ~st cont
  | Cond (x, cont1, cont2) ->
      [], Cond (x, cps_jump_cont ~st cont1, cps_jump_cont ~st cont2)
  | Switch (x, c1, c2) ->
      (* To avoid code duplication during JavaScript generation, we need
         to create a single block per continuation *)
      let cps_jump_cont = Fun.memoize (cps_jump_cont ~st) in
      [], Switch (x, Array.map c1 ~f:cps_jump_cont, Array.map c2 ~f:cps_jump_cont)
  | Pushtrap ((pc, args), _, (handler_pc, _), _) ->
      assert (Hashtbl.mem st.is_continuation handler_pc);
      if Addr.Set.mem handler_pc st.blocks_to_transform
      then
        let exn_handler = closure_of_pc ~st handler_pc in
        let push_trap =
          Let (Var.fresh (), Prim (Extern "caml_push_trap", [ Pv exn_handler ]))
        in
        let body, branch = cps_branch ~st (pc, args) in
        push_trap :: body, branch
      else [], last
  | Poptrap (pc', args) ->
      if try Addr.Set.mem (Hashtbl.find st.matching_exn_handler pc) st.blocks_to_transform
         with Not_found -> false
      then
        let exn_handler = Var.fresh () in
        let body, branch = cps_branch ~st (pc', args) in
        [ Let (exn_handler, Prim (Extern "caml_pop_trap", [])) ] @ body, branch
      else [], Poptrap (cps_jump_cont ~st (pc', args))

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
                (* For loops, we may need to allocate a closure to bind [x],
                   so we have to use a fresh variable here *)
                let x =
                  match Hashtbl.find st.is_continuation jump_pc with
                  | `Param x -> x
                  | `Loop -> Var.fresh ()
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
                   | `Param _ -> true
                   | `Loop -> st.live_vars.(Var.idx x) = 0 ->
                (* When entering a loop, we have to allocate a closure
                   to bind [x] if it is used in the loop body. In
                   other cases, we can just call the continuation. *)
                alloc_jump_closures, f'
            | [ x' ] when Var.equal x x' -> alloc_jump_closures, f'
            | _ ->
                let args, instrs =
                  if List.is_empty args
                  then
                    (* We use a dummy argument since the continuation
                       expects at least one argument. *)
                    let x = Var.fresh () in
                    [ x ], alloc_jump_closures @ [ Let (x, Constant (Int 0l)) ]
                  else args, alloc_jump_closures
                in
                allocate_closure
                  ~st
                  ~params:[ x ]
                  ~body:(tail_call ~st ~instrs ~check:false ~f:f' args)
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
        let last_instrs, last = cps_last ~st pc block.branch ~k in
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

let cps_transform ~live_vars ~cps_needed p =
  let closure_info = Hashtbl.create 16 in
  let tail_calls = ref Var.Set.empty in
  let p =
    Code.fold_closures_innermost_first
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
        let function_needs_cps =
          match name_opt with
          | None -> true
          | Some name -> Var.Set.mem name cps_needed
        in
        let blocks_to_transform, matching_exn_handler, is_continuation =
          if function_needs_cps
          then
            compute_needed_transformations
              ~cfg
              ~idom
              ~cps_needed
              ~blocks:blocks'
              ~start:start'
          else Addr.Set.empty, Hashtbl.create 1, Hashtbl.create 1
        in
        let function_needs_cps =
          if Option.is_none name_opt
          then Addr.Set.cardinal blocks_to_transform > 0
          else function_needs_cps
        in
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
          ; matching_exn_handler
          ; loop_headers = cfg.loop_headers
          ; live_vars
          }
        in
        let k = Var.fresh_n "cont" in
        if function_needs_cps then Hashtbl.add closure_info start (k, function_cont);
        if debug ()
        then (
          Format.eprintf "======== %b@." function_needs_cps;
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
        let blocks =
          let transform_block =
            if function_needs_cps
            then fun pc block -> cps_block ~st ~k pc block
            else
              fun _ block ->
              { block with body = List.map block.body ~f:(fun i -> cps_instr ~st i) }
          in
          let start = fst function_cont in
          Code.traverse
            { fold = Code.fold_children }
            (fun pc blocks ->
              Addr.Map.add pc (transform_block pc (Addr.Map.find pc blocks)) blocks)
            start
            st.blocks
            st.blocks
        in
        let new_blocks, free_pc = st.new_blocks in
        let blocks = Addr.Map.fold Addr.Map.add new_blocks blocks in
        { p with blocks; free_pc })
      p
  in
  match Hashtbl.find_opt closure_info p.start with
  | None -> p, !tail_calls
  | Some (k, _) ->
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
              [ Let (main, Closure ([ k ], (p.start, [])))
              ; Let (args, Prim (Extern "%js_array", []))
              ; Let (res, Prim (Extern "caml_callback", [ Pv main; Pv args ]))
              ]
          ; branch = Return res
          }
          p.blocks
      in
      { start = new_start; blocks; free_pc = new_start + 1 }, !tail_calls

(****)

let current_loop_header frontiers in_loop pc =
  (* We remain in a loop while the loop header is in the dominance frontier.
     We enter a loop when the block is in its dominance frontier. *)
  let frontier = get_edges frontiers pc in
  match in_loop with
  | Some header when Addr.Set.mem header frontier -> in_loop
  | _ -> if Addr.Set.mem pc frontier then Some pc else None

let wrap_call p cps_needed x f args accu =
  let arg_array = Var.fresh () in
  ( p
  , Var.Set.remove x cps_needed
  , [ Let (arg_array, Prim (Extern "%js_array", List.map ~f:(fun y -> Pv y) args))
    ; Let (x, Prim (Extern "caml_callback", [ Pv f; Pv arg_array ]))
    ]
    :: accu )

let wrap_primitive (p : Code.program) cps_needed x e accu =
  let f = Var.fresh () in
  let closure_pc = p.free_pc in
  ( { p with
      free_pc = p.free_pc + 1
    ; blocks =
        Addr.Map.add
          closure_pc
          (let y = Var.fresh () in
           { params = []; body = [ Let (y, e) ]; branch = Return y })
          p.blocks
    }
  , Var.Set.remove x (Var.Set.add f cps_needed)
  , let args = Var.fresh () in
    [ Let (f, Closure ([], (closure_pc, [])))
    ; Let (args, Prim (Extern "%js_array", []))
    ; Let (x, Prim (Extern "caml_callback", [ Pv f; Pv args ]))
    ]
    :: accu )

let rewrite_toplevel_instr (p, cps_needed, accu) instr =
  match instr with
  | Let (x, Apply { f; args; _ }) when Var.Set.mem x cps_needed ->
      wrap_call p cps_needed x f args accu
  | Let (x, (Prim (Extern ("%resume" | "%perform" | "%reperform"), _) as e)) ->
      wrap_primitive p cps_needed x e accu
  | _ -> p, cps_needed, [ instr ] :: accu

(* Wrap function calls inside [call_back] at toplevel to avoid
   unncessary function nestings. This is not done inside loops since
   using repeadtedly [call_back] can be costly. *)
let rewrite_toplevel p cps_needed =
  let { start; blocks; _ } = p in
  let cfg = build_graph blocks start in
  let idom = dominator_tree cfg in
  let frontiers = dominance_frontier cfg idom in
  let rec traverse visited (p : Code.program) cps_needed in_loop pc =
    if Addr.Set.mem pc visited
    then visited, p, cps_needed
    else
      let visited = Addr.Set.add pc visited in
      let in_loop = current_loop_header frontiers in_loop pc in
      let p, cps_needed =
        if Option.is_none in_loop
        then
          let block = Addr.Map.find pc p.blocks in
          let p, cps_needed, body_rev =
            List.fold_left ~f:rewrite_toplevel_instr ~init:(p, cps_needed, []) block.body
          in
          let body = List.concat @@ List.rev body_rev in
          { p with blocks = Addr.Map.add pc { block with body } p.blocks }, cps_needed
        else p, cps_needed
      in
      Code.fold_children
        blocks
        pc
        (fun pc (visited, p, cps_needed) -> traverse visited p cps_needed in_loop pc)
        (visited, p, cps_needed)
  in
  let _, p, cps_needed = traverse Addr.Set.empty p cps_needed None start in
  p, cps_needed

(****)

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

(****)

let remove_empty_blocks ~live_vars (p : Code.program) : Code.program =
  let shortcuts = Hashtbl.create 16 in
  let rec resolve_rec visited ((pc, args) as cont) =
    if Addr.Set.mem pc visited
    then cont
    else
      match Hashtbl.find_opt shortcuts pc with
      | Some (params, cont) ->
          let pc', args' = resolve_rec (Addr.Set.add pc visited) cont in
          let s = Subst.from_map (Subst.build_mapping params args) in
          pc', List.map ~f:s args'
      | None -> cont
  in
  let resolve cont = resolve_rec Addr.Set.empty cont in
  Addr.Map.iter
    (fun pc block ->
      match block with
      | { params; body = []; branch = Branch cont; _ } ->
          let args =
            List.fold_left
              ~f:(fun args x -> Var.Set.add x args)
              ~init:Var.Set.empty
              (snd cont)
          in
          (* We can skip a block if its parameters are only used as
             argument to the continuation *)
          if List.for_all
               ~f:(fun x -> live_vars.(Var.idx x) = 1 && Var.Set.mem x args)
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

(****)

let f (p : Code.program) =
  if debug () then Code.Print.program (fun _ _ -> "") p;
  let p, live_vars = Deadcode.f p in
  let p = remove_empty_blocks ~live_vars p in
  let _ = Flow2.f ~pessimistic:true p in
  let p, info = Flow.f ~pessimistic:true p in
  let cps_needed = Fun_style_analysis.f (p, info) in
  let p, cps_needed = rewrite_toplevel p cps_needed in
  let p = split_blocks ~cps_needed p in
  if debug () then Code.Print.program (fun _ _ -> "") p;
  cps_transform ~live_vars ~cps_needed p

let f p =
  let t = Timer.make () in
  let r = f p in
  if Debug.find "times" () then Format.eprintf "  effects: %a@." Timer.print t;
  r
