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

let double_translate () =
  match Config.effects () with
  | `Disabled | `Jspi -> assert false
  | `Cps -> false
  | `Double_translation -> true

let debug_print fmt =
  if debug () then Format.(eprintf (fmt ^^ "%!")) else Format.(ifprintf err_formatter fmt)

let get_edges g src = try Addr.Hashtbl.find g src with Not_found -> Addr.Set.empty

let add_edge g src dst = Addr.Hashtbl.replace g src (Addr.Set.add dst (get_edges g src))

let reverse_graph g =
  let g' = Addr.Hashtbl.create 16 in
  Addr.Hashtbl.iter
    (fun child parents -> Addr.Set.iter (fun parent -> add_edge g' parent child) parents)
    g;
  g'

type control_flow_graph =
  { succs : Addr.Set.t Addr.Hashtbl.t
  ; preds : Addr.Set.t Addr.Hashtbl.t
  ; reverse_post_order : Addr.t list
  ; block_order : int Addr.Hashtbl.t
  }

let build_graph blocks pc =
  let succs = Addr.Hashtbl.create 16 in
  let l = ref [] in
  let visited = Addr.Hashtbl.create 16 in
  let rec traverse pc =
    if not (Addr.Hashtbl.mem visited pc)
    then (
      Addr.Hashtbl.add visited pc ();
      let successors = Code.fold_children blocks pc Addr.Set.add Addr.Set.empty in
      Addr.Hashtbl.add succs pc successors;
      Addr.Set.iter traverse successors;
      l := pc :: !l)
  in
  traverse pc;
  let block_order = Addr.Hashtbl.create 16 in
  List.iteri !l ~f:(fun i pc -> Addr.Hashtbl.add block_order pc i);
  let preds = reverse_graph succs in
  { succs; preds; reverse_post_order = !l; block_order }

let dominator_tree g =
  (* A Simple, Fast Dominance Algorithm
     Keith D. Cooper, Timothy J. Harvey, and Ken Kennedy *)
  let dom = Addr.Hashtbl.create 16 in
  let rec inter pc pc' =
    (* Compute closest common ancestor *)
    if pc = pc'
    then pc
    else if Addr.Hashtbl.find g.block_order pc < Addr.Hashtbl.find g.block_order pc'
    then inter pc (Addr.Hashtbl.find dom pc')
    else inter (Addr.Hashtbl.find dom pc) pc'
  in
  List.iter g.reverse_post_order ~f:(fun pc ->
      let l = Addr.Hashtbl.find g.succs pc in
      Addr.Set.iter
        (fun pc' ->
          let d = try inter pc (Addr.Hashtbl.find dom pc') with Not_found -> pc in
          Addr.Hashtbl.replace dom pc' d)
        l);
  (* Check we have reached a fixed point (reducible graph) *)
  List.iter g.reverse_post_order ~f:(fun pc ->
      let l = Addr.Hashtbl.find g.succs pc in
      Addr.Set.iter
        (fun pc' ->
          let d = Addr.Hashtbl.find dom pc' in
          assert (inter pc d = d))
        l);
  dom

(* pc has at least two forward edges moving into it *)
let is_merge_node g pc =
  let s = try Addr.Hashtbl.find g.preds pc with Not_found -> assert false in
  let o = Addr.Hashtbl.find g.block_order pc in
  let n =
    Addr.Set.fold
      (fun pc' n -> if Addr.Hashtbl.find g.block_order pc' < o then n + 1 else n)
      s
      0
  in
  n > 1

let dominance_frontier g idom =
  let frontiers = Addr.Hashtbl.create 16 in
  Addr.Hashtbl.iter
    (fun pc preds ->
      if Addr.Set.cardinal preds > 1
      then
        let dom = Addr.Hashtbl.find idom pc in
        let rec loop runner =
          if runner <> dom
          then (
            add_edge frontiers runner pc;
            loop (Addr.Hashtbl.find idom runner))
        in
        Addr.Set.iter loop preds)
    g.preds;
  frontiers

(* Split a block, separating the last instruction from the preceeding
   ones, ignoring events *)
let block_split_last xs =
  let rec aux acc = function
    | [] -> None
    | [ x ] | [ x; Event _ ] -> Some (List.rev acc, x)
    | x :: xs -> aux (x :: acc) xs
  in
  aux [] xs

let empty_body b =
  match b with
  | [] | [ Event _ ] -> true
  | _ -> false

(****)

let effect_primitive_or_application = function
  | Prim (Extern ("%resume" | "%perform" | "%reperform"), _) | Apply _ -> true
  | Block (_, _, _, _)
  | Field (_, _, _)
  | Closure (_, _, _)
  | Constant _
  | Prim (_, _)
  | Special _ -> false

(*
We establish the list of blocks that needs to be CPS-transformed. We
also mark blocks that correspond to function continuations or
exception handlers. And we keep track of the exception handler
associated to each Poptrap, and possibly Raise.
*)
let compute_needed_transformations ~cfg ~idom ~cps_needed ~blocks ~start =
  let frontiers = dominance_frontier cfg idom in
  let transformation_needed = ref Addr.Set.empty in
  let matching_exn_handler = Addr.Hashtbl.create 16 in
  let is_continuation = Addr.Hashtbl.create 16 in
  let rec mark_needed pc =
    (* If a block is transformed, all the blocks in its dominance
       frontier needs to be transformed as well. *)
    if not (Addr.Set.mem pc !transformation_needed)
    then (
      transformation_needed := Addr.Set.add pc !transformation_needed;
      Addr.Set.iter mark_needed (get_edges frontiers pc))
  in
  let mark_continuation pc x =
    if not (Addr.Hashtbl.mem is_continuation pc)
    then
      Addr.Hashtbl.add
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
          match Code.last_instr block.body with
          | Some (Let (x, e))
            when effect_primitive_or_application e && Var.Set.mem x cps_needed ->
              (* The block after a function application that needs to
                 be turned to CPS or an effect primitive needs to be
                 transformed. *)
              mark_needed dst;
              (* We need to transform the englobing exception handlers
                 as well *)
              List.iter ~f:mark_needed englobing_exn_handlers;
              mark_continuation dst x
          | _ -> ())
      | Pushtrap (_, x, (handler_pc, _)) -> mark_continuation handler_pc x
      | Poptrap _ | Raise _ -> (
          match englobing_exn_handlers with
          | handler_pc :: _ -> Addr.Hashtbl.add matching_exn_handler pc handler_pc
          | _ -> ())
      | _ -> ());
      Code.fold_children
        blocks
        pc
        (fun pc visited ->
          let englobing_exn_handlers =
            match block.branch with
            | Pushtrap (_, _, (handler_pc, _)) when pc <> handler_pc ->
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
   block. In case of double translation, the keys are the addresses of the
   original (direct-style) blocks. Exception handlers are dealt with
   separately.
*)
type jump_closures =
  { closure_of_jump : Var.t Addr.Map.t
  ; closures_of_alloc_site : (Var.t * Addr.t) list Addr.Map.t
  }

let jump_closures blocks_to_transform idom : jump_closures =
  Addr.Hashtbl.fold
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

type trampolined_calls = Var.Set.t

type in_cps = Var.Set.t

type st =
  { mutable new_blocks : Code.block Addr.Map.t
  ; mutable free_pc : Code.Addr.t
  ; blocks : Code.block Addr.Map.t
  ; cfg : control_flow_graph
  ; jc : jump_closures
  ; closure_info : (Var.t list * (Addr.t * Var.t list)) Addr.Hashtbl.t
        (* Associates a function's address with its CPS parameters and CPS continuation *)
  ; cps_needed : Var.Set.t
  ; blocks_to_transform : Addr.Set.t
  ; is_continuation : [ `Param of Var.t | `Loop ] Addr.Hashtbl.t
  ; matching_exn_handler : Addr.t Addr.Hashtbl.t
  ; block_order : int Addr.Hashtbl.t
  ; live_vars : Deadcode.variable_uses
  ; flow_info : Global_flow.info
  ; trampolined_calls : trampolined_calls ref (* Call sites that require trampolining *)
  ; in_cps : in_cps ref (* Call sites whose callee must have a CPS component *)
  ; cps_pc_of_direct : int Addr.Hashtbl.t
        (* Mapping from direct-style to CPS addresses of functions (used when
           double translation is enabled) *)
  }

let add_block st block =
  let free_pc = st.free_pc in
  st.new_blocks <- Addr.Map.add free_pc block st.new_blocks;
  st.free_pc <- free_pc + 1;
  free_pc

(* Provide the address of the CPS translation of a block *)
let mk_cps_pc_of_direct ~st pc =
  if double_translate ()
  then (
    try Addr.Hashtbl.find st.cps_pc_of_direct pc
    with Not_found ->
      let free_pc = st.free_pc in
      st.free_pc <- free_pc + 1;
      Addr.Hashtbl.add st.cps_pc_of_direct pc free_pc;
      free_pc)
  else pc

let cps_cont_of_direct ~st (pc, args) = mk_cps_pc_of_direct ~st pc, args

let closure_of_pc ~st pc =
  try Addr.Map.find pc st.jc.closure_of_jump with Not_found -> assert false

let allocate_closure ~st ~params ~body ~branch =
  debug_print "@[<v>allocate_closure ~branch:(%a)@,@]" Code.Print.last branch;
  let block = { params = []; body; branch } in
  let pc = add_block st block in
  let name = Var.fresh () in
  [ Let (name, Closure (params, (pc, []), None)) ], name

let tail_call ~st ?(instrs = []) ~exact ~in_cps ~check ~f args =
  assert (exact || check);
  let ret = Var.fresh () in
  if check then st.trampolined_calls := Var.Set.add ret !(st.trampolined_calls);
  if in_cps then st.in_cps := Var.Set.add ret !(st.in_cps);
  instrs @ [ Let (ret, Apply { f; args; exact }) ], Return ret

let cps_branch ~st ~src (pc, args) =
  match Addr.Set.mem pc st.blocks_to_transform with
  | false -> [], Branch (mk_cps_pc_of_direct ~st pc, args)
  | true ->
      let args, instrs =
        if List.is_empty args && Addr.Hashtbl.mem st.is_continuation pc
        then
          (* We are jumping to a block that is also used as a continuation.
             We pass it a dummy argument. *)
          let x = Var.fresh () in
          [ x ], [ Let (x, Constant (Int Targetint.zero)) ]
        else args, []
      in
      (* We check the stack depth only for backward edges (so, at
         least once per loop iteration) *)
      let check =
        Addr.Hashtbl.find st.block_order src >= Addr.Hashtbl.find st.block_order pc
      in
      tail_call
        ~st
        ~instrs
        ~exact:true
        ~in_cps:false
        ~check
        ~f:(closure_of_pc ~st pc)
        args

let cps_jump_cont ~st ~src ((pc, _) as cont) =
  match Addr.Set.mem pc st.blocks_to_transform with
  | false -> cps_cont_of_direct ~st cont
  | true ->
      let call_block =
        let body, branch = cps_branch ~st ~src cont in
        add_block st { params = []; body; branch }
      in
      call_block, []

let allocate_continuation ~st ~alloc_jump_closures ~split_closures src_pc x direct_cont =
  debug_print
    "@[<v>allocate_continuation ~src_pc:%d ~cont:(%d,@ _)@,@]"
    src_pc
    (fst direct_cont);
  (* We need to allocate an additional closure if [cont]
     does not correspond to a continuation that binds [x].
     This closure binds the return value [x], allocates
     closures for dominated blocks and jumps to the next
     block. When entering a loop, we also have to allocate a
     closure to bind [x] if it is used in the loop body. In
     other cases, we can just pass the closure corresponding
     to the next block. *)
  let direct_pc, args = direct_cont in
  if
    (match args with
    | [] -> true
    | [ x' ] -> Var.equal x x'
    | _ -> false)
    &&
    match Addr.Hashtbl.find st.is_continuation direct_pc with
    | `Param _ -> true
    | `Loop -> List.compare_length_with args ~len:st.live_vars.(Var.idx x) = 0
  then alloc_jump_closures, closure_of_pc ~st direct_pc
  else
    let body, branch = cps_branch ~st ~src:src_pc direct_cont in
    let inner_closures, outer_closures =
      (* For [Pushtrap], we need to separate the closures
         corresponding to the exception handler body (that may make
         use of [x]) from the other closures that may be used outside
         of the exception handler. *)
      if not split_closures
      then alloc_jump_closures, []
      else if is_merge_node st.cfg direct_pc
      then [], alloc_jump_closures
      else
        List.partition
          ~f:(fun i ->
            match i with
            | Let (_, Closure (_, (pc'', []), _)) ->
                pc'' = mk_cps_pc_of_direct ~st direct_pc
            | _ -> assert false)
          alloc_jump_closures
    in
    let body, branch =
      allocate_closure ~st ~params:[ x ] ~body:(inner_closures @ body) ~branch
    in
    outer_closures @ body, branch

let cps_last ~st ~alloc_jump_closures pc (last : last) ~k : instr list * last =
  match last with
  | Return x ->
      assert (List.is_empty alloc_jump_closures);
      (* If the number of successive 'returns' is unbounded in CPS, it
         means that we have an unbounded of calls in direct style
         (even with tail call optimization) *)
      tail_call ~st ~exact:true ~in_cps:false ~check:false ~f:k [ x ]
  | Raise (x, rmode) -> (
      assert (List.is_empty alloc_jump_closures);
      match Addr.Hashtbl.find_opt st.matching_exn_handler pc with
      | Some pc when not (Addr.Set.mem pc st.blocks_to_transform) ->
          (* We are within a try ... with which is not
             transformed. We should raise an exception normally *)
          [], last
      | _ ->
          let exn_handler = Var.fresh_n "raise" in
          let x, instrs =
            match rmode with
            | `Notrace -> x, []
            | (`Normal | `Reraise) as m ->
                let x' = Var.fork x in
                let force =
                  match m with
                  | `Normal -> true
                  | `Reraise -> false
                in
                let i =
                  [ Let
                      ( x'
                      , Prim
                          ( Extern "caml_maybe_attach_backtrace"
                          , [ Pv x
                            ; Pc (Int (if force then Targetint.one else Targetint.zero))
                            ] ) )
                  ]
                in
                x', i
          in
          tail_call
            ~st
            ~instrs:(Let (exn_handler, Prim (Extern "caml_pop_trap", [])) :: instrs)
            ~exact:true
            ~in_cps:false
            ~check:false
            ~f:exn_handler
            [ x ])
  | Stop ->
      assert (List.is_empty alloc_jump_closures);
      [], Stop
  | Branch cont ->
      let body, branch = cps_branch ~st ~src:pc cont in
      alloc_jump_closures @ body, branch
  | Cond (x, cont1, cont2) ->
      ( alloc_jump_closures
      , Cond (x, cps_jump_cont ~st ~src:pc cont1, cps_jump_cont ~st ~src:pc cont2) )
  | Switch (x, c1) ->
      (* To avoid code duplication during JavaScript generation, we need
         to create a single block per continuation *)
      let cps_jump_cont = Fun.memoize (fun x -> cps_jump_cont ~st ~src:pc x) in
      alloc_jump_closures, Switch (x, Array.map c1 ~f:cps_jump_cont)
  | Pushtrap (body_cont, exn, ((handler_pc, _) as handler_cont)) -> (
      assert (Addr.Hashtbl.mem st.is_continuation handler_pc);
      match Addr.Set.mem handler_pc st.blocks_to_transform with
      | false ->
          let body_cont = cps_cont_of_direct ~st body_cont in
          let handler_cont = cps_cont_of_direct ~st handler_cont in
          let last = Pushtrap (body_cont, exn, handler_cont) in
          alloc_jump_closures, last
      | true ->
          let constr_cont, exn_handler =
            allocate_continuation
              ~st
              ~alloc_jump_closures
              ~split_closures:true
              pc
              exn
              handler_cont
          in
          let push_trap =
            Let (Var.fresh (), Prim (Extern "caml_push_trap", [ Pv exn_handler ]))
          in
          let body, branch = cps_branch ~st ~src:pc body_cont in
          constr_cont @ (push_trap :: body), branch)
  | Poptrap cont -> (
      match
        Addr.Set.mem (Addr.Hashtbl.find st.matching_exn_handler pc) st.blocks_to_transform
      with
      | false -> alloc_jump_closures, Poptrap (cps_jump_cont ~st ~src:pc cont)
      | true ->
          let exn_handler = Var.fresh () in
          let body, branch = cps_branch ~st ~src:pc cont in
          ( alloc_jump_closures
            @ (Let (exn_handler, Prim (Extern "caml_pop_trap", [])) :: body)
          , branch ))

let rewrite_instr ~st (instr : instr) : instr =
  match instr with
  | Let (x, Closure (_, (pc, _), _)) when Var.Set.mem x st.cps_needed ->
      (* When CPS-transforming with double translation enabled, there are no closures in
         code that requires transforming, due to lambda lifiting. *)
      assert (not (double_translate ()));
      (* Add the continuation parameter, and change the initial block if
         needed *)
      let cps_params, cps_cont = Addr.Hashtbl.find st.closure_info pc in
      st.in_cps := Var.Set.add x !(st.in_cps);
      Let (x, Closure (cps_params, cps_cont, None))
  | Let (x, Prim (Extern "caml_alloc_dummy_function", [ size; arity ])) -> (
      match arity with
      | Pc (Int a) ->
          Let
            ( x
            , Prim
                (Extern "caml_alloc_dummy_function", [ size; Pc (Int (Targetint.succ a)) ])
            )
      | _ -> assert false)
  | Let (x, Apply { f; args; exact }) when not (Var.Set.mem x st.cps_needed) ->
      if double_translate ()
      then
        let exact =
          (* If this function is unknown to the global flow analysis, then it was
           introduced by the lambda lifting and we don't have exactness info any more. *)
          exact
          || Var.idx f < Var.Tbl.length st.flow_info.info_approximation
             && Global_flow.exact_call st.flow_info f (List.length args)
        in
        Let (x, Apply { f; args; exact })
      else (
        (* At the moment, we turn into CPS any function not called with
         the right number of parameter *)
        assert (Global_flow.exact_call st.flow_info f (List.length args));
        Let (x, Apply { f; args; exact = true }))
  | Let (_, e) when effect_primitive_or_application e ->
      (* For the CPS target, applications of CPS functions and effect primitives require
         more work (allocating a continuation and/or modifying end-of-block branches) and
         are handled in a specialized function. *)
      assert false
  | _ -> instr

let call_exact flow_info (f : Var.t) nargs : bool =
  (* If [f] is unknown to the global flow analysis, then it was introduced by
     the lambda lifting and we don't have exactness about it. *)
  Var.idx f < Var.Tbl.length flow_info.Global_flow.info_approximation
  && Global_flow.exact_call flow_info f nargs

let cps_instr ~st (instr : instr) : instr list =
  match instr with
  | Let (x, Prim (Extern "caml_assume_no_perform", [ Pv f ])) when double_translate () ->
      (* When double translation is enabled, we just call [f] in direct style.
         Otherwise, the runtime primitive is used. *)
      let unit = Var.fresh_n "unit" in
      [ Let (unit, Constant (Int Targetint.zero))
      ; Let (x, Apply { exact = call_exact st.flow_info f 1; f; args = [ unit ] })
      ]
  | _ -> [ rewrite_instr ~st instr ]

let cps_block ~st ~k ~orig_pc block =
  debug_print "cps_block %d\n" orig_pc;
  debug_print "cps pc evaluates to %d\n" (mk_cps_pc_of_direct ~st orig_pc);
  let alloc_jump_closures =
    match Addr.Map.find orig_pc st.jc.closures_of_alloc_site with
    | to_allocate ->
        List.map to_allocate ~f:(fun (cname, jump_pc) ->
            let params =
              let jump_block = Addr.Map.find jump_pc st.blocks in
              (* For a function to be used as a continuation, it needs
                 exactly one parameter. So, we add a parameter if
                 needed. *)
              if
                List.is_empty jump_block.params
                && Addr.Hashtbl.mem st.is_continuation jump_pc
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
                  match Addr.Hashtbl.find st.is_continuation jump_pc with
                  | `Param x -> x
                  | `Loop -> Var.fresh ()
                in
                [ x ]
              else jump_block.params
            in
            let cps_jump_pc = mk_cps_pc_of_direct ~st jump_pc in
            Let (cname, Closure (params, (cps_jump_pc, []), None)))
    | exception Not_found -> []
  in

  let rewrite_last_instr (x : Var.t) (e : expr) : (k:Var.t -> instr list * last) option =
    let perform_effect ~effect_ continuation_and_tail =
      Some
        (fun ~k ->
          let e =
            match continuation_and_tail with
            | None -> Prim (Extern "caml_perform_effect", [ Pv effect_; Pv k ])
            | Some (continuation, tail) ->
                Prim
                  ( Extern "caml_reperform_effect"
                  , [ Pv effect_; continuation; tail; Pv k ] )
          in
          let x = Var.fresh () in
          [ Let (x, e) ], Return x)
    in
    match e with
    | Apply { f; args; exact } when Var.Set.mem x st.cps_needed ->
        Some
          (fun ~k ->
            let exact = exact || call_exact st.flow_info f (List.length args) in
            tail_call ~st ~exact ~in_cps:true ~check:true ~f (args @ [ k ]))
    | Prim (Extern "%resume", [ Pv stack; Pv f; Pv arg; tail ]) ->
        Some
          (fun ~k ->
            let k' = Var.fresh_n "cont" in
            tail_call
              ~st
              ~instrs:
                [ Let (k', Prim (Extern "caml_resume_stack", [ Pv stack; tail; Pv k ])) ]
              ~exact:(call_exact st.flow_info f 1)
              ~in_cps:true
              ~check:true
              ~f
              [ arg; k' ])
    | Prim (Extern "%perform", [ Pv effect_ ]) -> perform_effect ~effect_ None
    | Prim (Extern "%reperform", [ Pv effect_; continuation; tail ]) ->
        perform_effect ~effect_ (Some (continuation, tail))
    | _ -> None
  in

  let rewritten_block =
    match block_split_last block.body, block.branch with
    | Some (body_prefix, Let (x, e)), Return ret ->
        Option.map (rewrite_last_instr x e) ~f:(fun f ->
            assert (List.is_empty alloc_jump_closures);
            assert (Var.equal x ret);
            let instrs, branch = f ~k in
            body_prefix, instrs, branch)
    | Some (body_prefix, Let (x, e)), Branch cont ->
        Option.map (rewrite_last_instr x e) ~f:(fun f ->
            let constr_cont, k' =
              allocate_continuation
                ~st
                ~alloc_jump_closures
                ~split_closures:false
                orig_pc
                x
                cont
            in
            let instrs, branch = f ~k:k' in
            body_prefix, constr_cont @ instrs, branch)
    | Some (_, (Event _ | Set_field _ | Offset_ref _ | Array_set _ | Assign _)), _
    | Some _, (Raise _ | Stop | Cond _ | Switch _ | Pushtrap _ | Poptrap _)
    | None, _ -> None
  in

  let body, last =
    match rewritten_block with
    | Some (body_prefix, last_instrs, last) ->
        let body_prefix =
          List.map body_prefix ~f:(fun i -> cps_instr ~st i) |> List.concat
        in
        body_prefix @ last_instrs, last
    | None ->
        let last_instrs, last =
          cps_last ~st ~alloc_jump_closures orig_pc block.branch ~k
        in
        let body = List.map block.body ~f:(fun i -> cps_instr ~st i) |> List.concat in
        body @ last_instrs, last
  in

  { params = (if Addr.Set.mem orig_pc st.blocks_to_transform then [] else block.params)
  ; body
  ; branch = last
  }

(* If double-translating, modify all function applications and closure
   creations to take into account the fact that some closures must now have a
   CPS version. Also rewrite the effect primitives to switch to the CPS version
   of functions (for resume) or fail (for perform).
   If not double-translating, then just add continuation arguments to function
   definitions, and mark as exact all non-CPS calls. *)
let rewrite_direct_block ~st ~cps_needed ~closure_info ~pc block =
  debug_print "@[<v>rewrite_direct_block %d@,@]" pc;
  if double_translate ()
  then
    let rewrite_instr = function
      | Let (x, Closure (params, ((pc, _) as cont), cloc)) when Var.Set.mem x cps_needed
        ->
          let direct_c = Var.fork x in
          let cps_c = Var.fork x in
          let cps_params, cps_cont = Addr.Hashtbl.find closure_info pc in
          [ Let (direct_c, Closure (params, cont, cloc))
          ; Let (cps_c, Closure (cps_params, cps_cont, None))
          ; Let (x, Prim (Extern "caml_cps_closure", [ Pv direct_c; Pv cps_c ]))
          ]
      | Let (x, Prim (Extern "%resume", [ stack; f; arg; tail ])) ->
          [ Let (x, Prim (Extern "caml_resume", [ f; arg; stack; tail ])) ]
      | Let (x, Prim (Extern "%perform", [ effect_ ])) ->
          (* In direct-style code, we just raise [Effect.Unhandled]. *)
          [ Let (x, Prim (Extern "caml_raise_unhandled", [ effect_ ])) ]
      | Let (x, Prim (Extern "%reperform", [ effect_; _continuation; _tail ])) ->
          (* Similar to previous case *)
          [ Let (x, Prim (Extern "caml_raise_unhandled", [ effect_ ])) ]
      | Let (x, Prim (Extern "caml_assume_no_perform", [ Pv f ])) ->
          (* We just need to call [f] in direct style. *)
          let unit = Var.fresh_n "unit" in
          let unit_val = Int Targetint.zero in
          let exact = call_exact st.flow_info f 1 in
          [ Let (unit, Constant unit_val); Let (x, Apply { exact; f; args = [ unit ] }) ]
      | (Let _ | Assign _ | Set_field _ | Offset_ref _ | Array_set _ | Event _) as instr
        -> [ instr ]
    in
    let body = List.concat_map block.body ~f:(fun i -> rewrite_instr i) in
    { block with body }
  else { block with body = List.map ~f:(rewrite_instr ~st) block.body }

(* Apply a substitution in a set of blocks, including to bound variables *)
let subst_bound_in_blocks blocks s =
  Addr.Map.mapi
    (fun pc block ->
      if debug ()
      then (
        debug_print "@[<v>block before first subst: @,";
        Code.Print.block Format.err_formatter (fun _ _ -> "") pc block;
        debug_print "@]");
      let res = Subst.Including_Binders.block s block in
      if debug ()
      then (
        debug_print "@[<v>block after first subst: @,";
        Code.Print.block Format.err_formatter (fun _ _ -> "") pc res;
        debug_print "@]");
      res)
    blocks

let subst_add_fresh array v = array.(Var.idx v) <- Var.fork v

let cps_transform ~live_vars ~flow_info ~cps_needed p =
  let closure_info = Addr.Hashtbl.create 16 in
  let trampolined_calls = ref Var.Set.empty in
  let in_cps = ref Var.Set.empty in
  let cps_pc_of_direct = Addr.Hashtbl.create 512 in
  let cloned_vars = Array.init (Var.count ()) ~f:Var.of_idx in
  let cloned_subst = Subst.from_array cloned_vars in
  let p =
    Code.fold_closures_innermost_first
      p
      (fun name_opt params (start, args) _cloc ({ Code.blocks; free_pc; _ } as p) ->
        Option.iter name_opt ~f:(fun v -> debug_print "@[<v>cname = %a@,@]" Var.print v);
        (* We speculatively add a block at the beginning of the
           function. In case of tail-recursion optimization, the
           function implementing the loop body may have to be placed
           there. *)
        let initial_start = start in
        let start', blocks' =
          ( free_pc
          , Addr.Map.add
              free_pc
              { params = []; body = []; branch = Branch (start, args) }
              blocks )
        in
        let cfg = build_graph blocks' start' in
        let idom = dominator_tree cfg in
        let should_compute_needed_transformations =
          match name_opt with
          | Some name -> Var.Set.mem name cps_needed
          | None ->
              (* We need to handle the CPS calls that are at toplevel, except
                 if we double-translate (in which case they are like all other
                 CPS calls from direct code). *)
              not (double_translate ())
        in
        let blocks_to_transform, matching_exn_handler, is_continuation =
          if should_compute_needed_transformations
          then
            compute_needed_transformations
              ~cfg
              ~idom
              ~cps_needed
              ~blocks:blocks'
              ~start:start'
          else Addr.Set.empty, Addr.Hashtbl.create 1, Addr.Hashtbl.create 1
        in
        let closure_jc = jump_closures blocks_to_transform idom in
        let start, args, blocks, free_pc =
          (* Insert an initial block if needed. *)
          if
            should_compute_needed_transformations
            && Addr.Map.mem start' closure_jc.closures_of_alloc_site
          then start', [], blocks', free_pc + 1
          else start, args, blocks, free_pc
        in
        let st =
          { new_blocks = Addr.Map.empty
          ; free_pc
          ; blocks
          ; cfg
          ; jc = closure_jc
          ; closure_info
          ; cps_needed
          ; blocks_to_transform
          ; is_continuation
          ; matching_exn_handler
          ; block_order = cfg.block_order
          ; flow_info
          ; live_vars
          ; trampolined_calls
          ; in_cps
          ; cps_pc_of_direct
          }
        in
        let function_needs_cps =
          match name_opt with
          | Some _ -> should_compute_needed_transformations
          | None ->
              (* Toplevel code: if we double-translate, no need to handle it
                 specially: CPS calls in it are like all other CPS calls from
                 direct code. Otherwise, it needs to wrapped within a
                 [caml_cps_trampoline], but only if it performs CPS calls. *)
              not (double_translate () || Addr.Set.is_empty blocks_to_transform)
        in
        if debug ()
        then (
          Format.eprintf "======== %b@." function_needs_cps;
          Code.preorder_traverse
            { fold = Code.fold_children }
            (fun pc _ ->
              if Addr.Set.mem pc blocks_to_transform then Format.eprintf "CPS@.";
              let block = Addr.Map.find pc blocks in
              Code.Print.block
                Format.err_formatter
                (fun _ xi -> Partial_cps_analysis.annot cps_needed xi)
                pc
                block)
            start
            blocks
            ());
        let blocks =
          (* For every block in the closure,
             1. CPS-translate it if needed. If we double-translate, add its CPS
                translation to the block map at a fresh address. Otherwise,
                just replace the original block.
             2. If we double-translate, keep the direct-style block but modify function
                definitions to add the CPS version where needed, and turn uses of %resume
                and %perform into switchings to CPS. *)
          let transform_block =
            if function_needs_cps && double_translate ()
            then (
              let k = Var.fresh_n "cont" in
              let cps_start = mk_cps_pc_of_direct ~st start in
              List.iter ~f:(subst_add_fresh cloned_vars) params;
              let params' = List.map ~f:cloned_subst params in
              let cps_args = List.map ~f:cloned_subst args in
              Addr.Hashtbl.add
                st.closure_info
                initial_start
                (params' @ [ k ], (cps_start, cps_args));
              fun pc block ->
                let cps_block = cps_block ~st ~k ~orig_pc:pc block in
                ( rewrite_direct_block
                    ~st
                    ~cps_needed
                    ~closure_info:st.closure_info
                    ~pc
                    block
                , Some cps_block ))
            else if function_needs_cps && not (double_translate ())
            then (
              let k = Var.fresh_n "cont" in
              Addr.Hashtbl.add
                st.closure_info
                initial_start
                (params @ [ k ], (start, args));
              fun pc block -> cps_block ~st ~k ~orig_pc:pc block, None)
            else
              fun pc block ->
                ( rewrite_direct_block
                    ~st
                    ~cps_needed
                    ~closure_info:st.closure_info
                    ~pc
                    block
                , None )
          in
          Code.traverse
            { fold = Code.fold_children }
            (fun pc blocks ->
              let block, cps_block_opt = transform_block pc (Addr.Map.find pc blocks) in
              let blocks = Addr.Map.add pc block blocks in
              match cps_block_opt with
              | None -> blocks
              | Some b ->
                  let cps_pc = mk_cps_pc_of_direct ~st pc in
                  st.new_blocks <- Addr.Map.add cps_pc b st.new_blocks;
                  Addr.Map.add cps_pc b blocks)
            start
            st.blocks
            st.blocks
        in
        (* If double-translating, all variables bound in the CPS version will have to be
             subst with fresh ones to avoid clashing with the definitions in the original
             blocks (the actual substitution is done later). *)
        let new_blocks =
          if function_needs_cps && double_translate ()
          then (
            Code.traverse
              Code.{ fold = fold_children }
              (fun pc () ->
                let block = Addr.Map.find pc p.blocks in
                Freevars.iter_block_bound_vars
                  (fun v -> subst_add_fresh cloned_vars v)
                  block)
              initial_start
              p.blocks
              ();
            subst_bound_in_blocks st.new_blocks cloned_subst)
          else st.new_blocks
        in
        let blocks = Addr.Map.fold Addr.Map.add new_blocks blocks in
        { p with blocks; free_pc = st.free_pc })
      p
  in
  (* Also apply our substitution to the sets of trampolined calls, and cps call sites *)
  trampolined_calls := Var.Set.map cloned_subst !trampolined_calls;
  in_cps := Var.Set.map cloned_subst !in_cps;
  let p =
    if double_translate ()
    then p
    else
      match Addr.Hashtbl.find_opt closure_info p.start with
      | None -> p
      | Some (cps_params, cps_cont) ->
          (* Call [caml_cps_trampoline] to set up the execution context. *)
          let new_start = p.free_pc in
          let blocks =
            let main = Var.fresh () in
            let args = Var.fresh () in
            let res = Var.fresh () in
            Addr.Map.add
              new_start
              { params = []
              ; body =
                  [ Let (main, Closure (cps_params, cps_cont, None))
                  ; Let (args, Prim (Extern "%js_array", []))
                  ; Let (res, Prim (Extern "caml_cps_trampoline", [ Pv main; Pv args ]))
                  ]
              ; branch = Return res
              }
              p.blocks
          in
          { start = new_start; blocks; free_pc = new_start + 1 }
  in
  p, !trampolined_calls, !in_cps

(****)

let current_loop_header frontiers in_loop pc =
  (* We remain in a loop while the loop header is in the dominance frontier.
     We enter a loop when the block is in its dominance frontier. *)
  let frontier = get_edges frontiers pc in
  match in_loop with
  | Some header when Addr.Set.mem header frontier -> in_loop
  | _ -> if Addr.Set.mem pc frontier then Some pc else None

let wrap_call ~cps_needed p x f args accu =
  let arg_array = Var.fresh () in
  ( p
  , Var.Set.remove x cps_needed
  , [ Let (arg_array, Prim (Extern "%js_array", List.map ~f:(fun y -> Pv y) args))
    ; Let (x, Prim (Extern "caml_cps_trampoline", [ Pv f; Pv arg_array ]))
    ]
    :: accu )

let wrap_primitive ~cps_needed (p : program) x e accu =
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
    [ Let (f, Closure ([], (closure_pc, []), None))
    ; Let (args, Prim (Extern "%js_array", []))
    ; Let (x, Prim (Extern "caml_cps_trampoline", [ Pv f; Pv args ]))
    ]
    :: accu )

let rewrite_toplevel_instr (p, cps_needed, accu) instr =
  match instr with
  | Let (x, Apply { f; args; _ }) when Var.Set.mem x cps_needed ->
      wrap_call ~cps_needed p x f args accu
  | Let (x, (Prim (Extern ("%resume" | "%perform" | "%reperform"), _) as e)) ->
      wrap_primitive ~cps_needed p x e accu
  | _ -> p, cps_needed, [ instr ] :: accu

(* Wrap function calls inside [caml_cps_trampoline] at toplevel to avoid
   unncessary function nestings. This is not done inside loops since
   using repeatedly [caml_cps_trampoline] can be costly. *)
let rewrite_toplevel ~cps_needed p =
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
      | Let (x, e) when effect_primitive_or_application e ->
          ((not (empty_body r))
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

(****)

let f ~flow_info ~live_vars p =
  Code.invariant p;
  let t = Timer.make () in
  let cps_needed = Partial_cps_analysis.f p flow_info in
  let p, cps_needed =
    if double_translate ()
    then (
      let p, liftings = Lambda_lifting_simple.f ~to_lift:cps_needed p in
      let cps_needed =
        Var.Set.map
          (fun f -> try Subst.from_map liftings f with Not_found -> f)
          cps_needed
      in
      if debug ()
      then (
        debug_print "@]";
        debug_print "@[<v>cps_needed (after lifting) = @[<hov 2>";
        Var.Set.iter (fun v -> debug_print "%a,@ " Var.print v) cps_needed;
        debug_print "@]@,@]";
        debug_print "@[<v>After lambda lifting...@,";
        Code.Print.program Format.err_formatter (fun _ _ -> "") p;
        debug_print "@]");
      p, cps_needed)
    else
      let p, cps_needed = rewrite_toplevel ~cps_needed p in
      p, cps_needed
  in
  let p = split_blocks ~cps_needed p in
  let p, trampolined_calls, in_cps = cps_transform ~live_vars ~flow_info ~cps_needed p in
  if Debug.find "times" () then Format.eprintf "  effects: %a@." Timer.print t;
  Code.invariant p;
  if debug ()
  then (
    debug_print "@[<v>After CPS transform:@,";
    Code.Print.program Format.err_formatter (fun _ _ -> "") p;
    debug_print "@]");
  p, trampolined_calls, in_cps
