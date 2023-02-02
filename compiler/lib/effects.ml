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
  ; reverse_post_order : Addr.t list
  ; block_order : (Addr.t, int) Hashtbl.t
  }

let build_graph blocks pc =
  let succs = Hashtbl.create 16 in
  let l = ref [] in
  let visited = Hashtbl.create 16 in
  let rec traverse pc =
    if not (Hashtbl.mem visited pc)
    then (
      Hashtbl.add visited pc ();
      let successors = Code.fold_children blocks pc Addr.Set.add Addr.Set.empty in
      Hashtbl.add succs pc successors;
      Addr.Set.iter traverse successors;
      l := pc :: !l)
  in
  traverse pc;
  let block_order = Hashtbl.create 16 in
  List.iteri !l ~f:(fun i pc -> Hashtbl.add block_order pc i);
  { succs; reverse_post_order = !l; block_order }

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
let compute_needed_transformations ~cfg ~idom ~blocks ~start =
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
            ->
              (* The block after a function application or an effect
                 primitive needs to be transformed. *)
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
      Code.fold_children
        blocks
        pc
        (fun pc visited ->
          let englobing_exn_handlers =
            match block.branch with
            | Pushtrap (_, _, (handler_pc, _), _) when pc <> handler_pc ->
                handler_pc :: englobing_exn_handlers
            | Poptrap _ -> List.tl englobing_exn_handlers
            | _ -> englobing_exn_handlers
          in
          traverse visited ~englobing_exn_handlers pc)
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
      match Addr.Set.mem node blocks_to_transform with
      | false -> jc
      | true ->
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

type cps_calls = Var.Set.t

type st =
  { mutable new_blocks : Code.block Addr.Map.t * Code.Addr.t
  ; blocks : Code.block Addr.Map.t
  ; jc : jump_closures
  ; closure_continuation : Addr.t -> Var.t
  ; blocks_to_transform : Addr.Set.t
  ; is_continuation : (Addr.t, [ `Param of Var.t | `Loop ]) Hashtbl.t
  ; matching_exn_handler : (Addr.t, Addr.t) Hashtbl.t
  ; block_order : (Addr.t, int) Hashtbl.t
  ; live_vars : Deadcode.variable_uses
  ; cps_calls : cps_calls ref
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

let tail_call ~st ?(instrs = []) ~exact ~check ~f args =
  assert (exact || check);
  let ret = Var.fresh () in
  if check then st.cps_calls := Var.Set.add ret !(st.cps_calls);
  instrs @ [ Let (ret, Apply { f; args; exact }) ], Return ret

let cps_branch ~st ~src (pc, args) =
  match Addr.Set.mem pc st.blocks_to_transform with
  | false -> [], Branch (pc, args)
  | true ->
      let args, instrs =
        if List.is_empty args && Hashtbl.mem st.is_continuation pc
        then
          (* We are jumping to a block that is also used as a continuation.
             We pass it a dummy argument. *)
          let x = Var.fresh () in
          [ x ], [ Let (x, Constant (Int 0l)) ]
        else args, []
      in
      (* We check the stack depth only for backward edges (so, at
         least once per loop iteration) *)
      let check = Hashtbl.find st.block_order src >= Hashtbl.find st.block_order pc in
      tail_call ~st ~instrs ~exact:true ~check ~f:(closure_of_pc ~st pc) args

let cps_jump_cont ~st ~src ((pc, _) as cont) =
  match Addr.Set.mem pc st.blocks_to_transform with
  | false -> cont
  | true ->
      let call_block =
        let body, branch = cps_branch ~st ~src cont in
        add_block st { params = []; body; branch }
      in
      call_block, []

let cps_last ~st pc (last : last) ~k : instr list * last =
  match last with
  | Return x ->
      (* Is the number of successive 'returns' is unbounded is CPS, it
         means that we have an unbounded of calls in direct style
         (even with tail call optimization) *)
      tail_call ~st ~exact:true ~check:false ~f:k [ x ]
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
            ~exact:true
            ~check:false
            ~f:exn_handler
            [ x ])
  | Stop -> [], Stop
  | Branch cont -> cps_branch ~st ~src:pc cont
  | Cond (x, cont1, cont2) ->
      [], Cond (x, cps_jump_cont ~st ~src:pc cont1, cps_jump_cont ~st ~src:pc cont2)
  | Switch (x, c1, c2) ->
      (* To avoid code duplication during JavaScript generation, we need
         to create a single block per continuation *)
      let cps_jump_cont = Fun.memoize (cps_jump_cont ~st ~src:pc) in
      [], Switch (x, Array.map c1 ~f:cps_jump_cont, Array.map c2 ~f:cps_jump_cont)
  | Pushtrap (body_cont, _, (handler_pc, _), _) -> (
      assert (Hashtbl.mem st.is_continuation handler_pc);
      match Addr.Set.mem handler_pc st.blocks_to_transform with
      | false -> [], last
      | true ->
          let exn_handler = closure_of_pc ~st handler_pc in
          let push_trap =
            Let (Var.fresh (), Prim (Extern "caml_push_trap", [ Pv exn_handler ]))
          in
          let body, branch = cps_branch ~st ~src:pc body_cont in
          push_trap :: body, branch)
  | Poptrap cont -> (
      match
        Addr.Set.mem (Hashtbl.find st.matching_exn_handler pc) st.blocks_to_transform
      with
      | false -> [], Poptrap (cps_jump_cont ~st ~src:pc cont)
      | true ->
          let exn_handler = Var.fresh () in
          let body, branch = cps_branch ~st ~src:pc cont in
          Let (exn_handler, Prim (Extern "caml_pop_trap", [])) :: body, branch)

let cps_instr ~st (instr : instr) : instr =
  match instr with
  | Let (x, Closure (params, (pc, args))) ->
      Let (x, Closure (params @ [ st.closure_continuation pc ], (pc, args)))
  | Let (x, Prim (Extern "caml_alloc_dummy_function", [ size; arity ])) -> (
      match arity with
      | Pc (Int a) ->
          Let
            ( x
            , Prim (Extern "caml_alloc_dummy_function", [ size; Pc (Int (Int32.succ a)) ])
            )
      | _ -> assert false)
  | Let (_, (Apply _ | Prim (Extern ("%resume" | "%perform" | "%reperform"), _))) ->
      assert false
  | _ -> instr

let cps_block ~st ~k pc block =
  let alloc_jump_closures =
    match Addr.Map.find pc st.jc.closures_of_alloc_site with
    | to_allocate ->
        List.map to_allocate ~f:(fun (cname, jump_pc) ->
            let params =
              let jump_block = Addr.Map.find jump_pc st.blocks in
              (* For a function to be used as a continuation, it needs
                 exactly one parameter. So, we add a parameter if
                 needed. *)
              if List.is_empty jump_block.params && Hashtbl.mem st.is_continuation jump_pc
              then
                (* We reuse the name of the value of the tail call of
                   one a the previous blocks. When there is a single
                   previous block, this is exactly what we want. For a
                   merge node, the variable is not used so we can just
                   as well use it. For a loop, we don't want the
                   return value of a call right before entering the
                   loop to be overriden by the value returned by the
                   last call in the loop. So, we may need to use an
                   additional closure to bind it, and we have to use a
                   fresh variable here *)
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

  let rewrite_instr e =
    let perform_effect ~effect ~continuation =
      Some
        (fun ~k ->
          let e =
            Prim (Extern "caml_perform_effect", [ Pv effect; continuation; Pv k ])
          in
          let x = Var.fresh () in
          [ Let (x, e) ], Return x)
    in
    match e with
    | Apply { f; args; exact } ->
        Some (fun ~k -> tail_call ~st ~exact ~check:true ~f (args @ [ k ]))
    | Prim (Extern "%resume", [ Pv stack; Pv f; Pv arg ]) ->
        Some
          (fun ~k ->
            let k' = Var.fresh_n "cont" in
            tail_call
              ~st
              ~instrs:[ Let (k', Prim (Extern "caml_resume_stack", [ Pv stack; Pv k ])) ]
              ~exact:false
              ~check:true
              ~f
              [ arg; k' ])
    | Prim (Extern "%perform", [ Pv effect ]) ->
        perform_effect ~effect ~continuation:(Pc (Int 0l))
    | Prim (Extern "%reperform", [ Pv effect; continuation ]) ->
        perform_effect ~effect ~continuation
    | _ -> None
  in

  let rewritten_block =
    match List.split_last block.body, block.branch with
    | Some (body_prefix, Let (x, e)), Return ret ->
        Option.map (rewrite_instr e) ~f:(fun f ->
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
                  ~body:(tail_call ~st ~instrs ~exact:true ~check:false ~f:f' args)
          in
          let instrs, branch = f ~k:k' in
          body_prefix, constr_cont @ instrs, branch
        in
        Option.map (rewrite_instr e) ~f:allocate_continuation
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

let cps_transform ~live_vars p =
  let closure_continuation =
    (* Provide a name for the continuation of a closure (before CPS
       transform), which can be referred from all the blocks it contains *)
    let tbl = Hashtbl.create 4 in
    fun pc ->
      try Hashtbl.find tbl pc
      with Not_found ->
        let k = Var.fresh_n "cont" in
        Hashtbl.add tbl pc k;
        k
  in
  let cps_calls = ref Var.Set.empty in
  let wrap_toplevel = ref true in
  let p =
    Code.fold_closures
      p
      (fun name_opt _ (start, _) ({ blocks; free_pc; _ } as p) ->
        let cfg = build_graph blocks start in
        let idom = dominator_tree cfg in
        let blocks_to_transform, matching_exn_handler, is_continuation =
          compute_needed_transformations ~cfg ~idom ~blocks ~start
        in
        let closure_jc = jump_closures blocks_to_transform idom in
        let st =
          { new_blocks = Addr.Map.empty, free_pc
          ; blocks
          ; jc = closure_jc
          ; closure_continuation
          ; blocks_to_transform
          ; is_continuation
          ; matching_exn_handler
          ; block_order = cfg.block_order
          ; live_vars
          ; cps_calls
          }
        in
        let function_needs_cps =
          match name_opt with
          | Some _ -> true
          | None ->
              (* We are handling the toplevel code. If it performs no
                 CPS call, we can leave it in direct style and we
                 don't need to wrap it within a [caml_callback]. *)
              let need_cps = not (Addr.Set.is_empty blocks_to_transform) in
              wrap_toplevel := need_cps;
              need_cps
        in
        if debug ()
        then (
          Format.eprintf "======== %b@." function_needs_cps;
          Code.preorder_traverse
            { fold = Code.fold_children }
            (fun pc _ ->
              if Addr.Set.mem pc blocks_to_transform then Format.eprintf "CPS@.";
              let block = Addr.Map.find pc blocks in
              Code.Print.block (fun _ _ -> "") pc block)
            start
            blocks
            ());
        let blocks =
          let transform_block =
            if function_needs_cps
            then
              let k = closure_continuation start in
              fun pc block -> cps_block ~st ~k pc block
            else
              fun _ block ->
              { block with body = List.map block.body ~f:(fun i -> cps_instr ~st i) }
          in
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
  let p =
    if not !wrap_toplevel
    then p
    else
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
              [ Let (main, Closure ([ closure_continuation p.start ], (p.start, [])))
              ; Let (args, Prim (Extern "%js_array", []))
              ; Let (res, Prim (Extern "caml_callback", [ Pv main; Pv args ]))
              ]
          ; branch = Return res
          }
          p.blocks
      in
      { start = new_start; blocks; free_pc = new_start + 1 }
  in
  p, !cps_calls

(****)

let current_loop_header frontiers in_loop pc =
  (* We remain in a loop while the loop header is in the dominance frontier.
     We enter a loop when the block is in its dominance frontier. *)
  let frontier = get_edges frontiers pc in
  match in_loop with
  | Some header when Addr.Set.mem header frontier -> in_loop
  | _ -> if Addr.Set.mem pc frontier then Some pc else None

let wrap_call p x f args accu =
  let arg_array = Var.fresh () in
  ( p
  , [ Let (arg_array, Prim (Extern "%js_array", List.map ~f:(fun y -> Pv y) args))
    ; Let (x, Prim (Extern "caml_callback", [ Pv f; Pv arg_array ]))
    ]
    :: accu )

let wrap_primitive (p : Code.program) x e accu =
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
  , let args = Var.fresh () in
    [ Let (f, Closure ([], (closure_pc, [])))
    ; Let (args, Prim (Extern "%js_array", []))
    ; Let (x, Prim (Extern "caml_callback", [ Pv f; Pv args ]))
    ]
    :: accu )

let rewrite_toplevel_instr (p, accu) instr =
  match instr with
  | Let (x, Apply { f; args; _ }) -> wrap_call p x f args accu
  | Let (x, (Prim (Extern ("%resume" | "%perform" | "%reperform"), _) as e)) ->
      wrap_primitive p x e accu
  | _ -> p, [ instr ] :: accu

(* Wrap function calls inside [caml_callback] at toplevel to avoid
   unncessary function nestings. This is not done inside loops since
   using repeatedly [caml_callback] can be costly. *)
let rewrite_toplevel p =
  let { start; blocks; _ } = p in
  let cfg = build_graph blocks start in
  let idom = dominator_tree cfg in
  let frontiers = dominance_frontier cfg idom in
  let rec traverse visited (p : Code.program) in_loop pc =
    if Addr.Set.mem pc visited
    then visited, p
    else
      let visited = Addr.Set.add pc visited in
      let in_loop = current_loop_header frontiers in_loop pc in
      let p =
        if Option.is_none in_loop
        then
          let block = Addr.Map.find pc p.blocks in
          let p, body_rev =
            List.fold_left ~f:rewrite_toplevel_instr ~init:(p, []) block.body
          in
          let body = List.concat @@ List.rev body_rev in
          { p with blocks = Addr.Map.add pc { block with body } p.blocks }
        else p
      in
      Code.fold_children
        blocks
        pc
        (fun pc (visited, p) -> traverse visited p in_loop pc)
        (visited, p)
  in
  let _, p = traverse Addr.Set.empty p None start in
  p

(****)

let split_blocks (p : Code.program) =
  (* Ensure that function applications and effect primitives are in
     tail position *)
  let split_block pc block p =
    let is_split_point i r branch =
      match i with
      | Let (x, (Apply _ | Prim (Extern ("%resume" | "%perform" | "%reperform"), _))) -> (
          (not (List.is_empty r))
          ||
          match branch with
          | Branch _ -> false
          | Return x' -> not (Var.equal x x')
          | _ -> true)
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
          (* We can skip an empty block if its parameters are only
             used as argument to the continuation *)
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

let f (p, live_vars) =
  let t = Timer.make () in
  let p = remove_empty_blocks ~live_vars p in
  let p = split_blocks p in
  let p = rewrite_toplevel p in
  let p, cps_calls = cps_transform ~live_vars p in
  if Debug.find "times" () then Format.eprintf "  effects: %a@." Timer.print t;
  p, cps_calls
