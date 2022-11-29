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

   We rely on inlining to eliminate some administrative redexes.
*)
open! Stdlib
open Code

type graph =
  { succs : (Addr.t, Addr.Set.t) Hashtbl.t
  ; exn_handlers : (Addr.t, unit) Hashtbl.t
  ; reverse_post_order : Addr.t list
  }

let build_graph blocks pc =
  let succs = Hashtbl.create 16 in
  let exn_handlers = Hashtbl.create 16 in
  let l = ref [] in
  let visited = Hashtbl.create 16 in
  let rec traverse pc =
    if not (Hashtbl.mem visited pc)
    then (
      Hashtbl.add visited pc ();
      let successors = Code.fold_children blocks pc Addr.Set.add Addr.Set.empty in
      (match (Addr.Map.find pc blocks).branch with
      | Pushtrap (_, _, (pc', _), _) -> Hashtbl.add exn_handlers pc' ()
      | _ -> ());
      Hashtbl.add succs pc successors;
      Addr.Set.iter traverse successors;
      l := pc :: !l)
  in
  traverse pc;
  { succs; exn_handlers; reverse_post_order = !l }

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

let jump_closures g idom : jump_closures =
  Hashtbl.fold
    (fun node idom_node jc ->
      if Hashtbl.mem g.exn_handlers node
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
  ; closure_continuation : Addr.t -> Var.t
  }

let add_block st block =
  let blocks, free_pc = st.new_blocks in
  st.new_blocks <- Addr.Map.add free_pc block blocks, free_pc + 1;
  free_pc

let closure_of_pc ~st pc =
  try Addr.Map.find pc st.jc.closure_of_jump with Not_found -> assert false

let allocate_closure ~st ~params ~body ~branch =
  let block = { params = []; body; branch } in
  let pc = add_block st block in
  let name = Var.fresh () in
  [ Let (name, Closure (params, (pc, []))) ], name

module Runtime : sig
  (* Manipulation of the execution context and of reified
     continuations. See [runtime/effect.js] for details. *)

  type cont = Var.t (* Low-level continuation (one-argument function) *)

  type continuation = Var.t (* Reified continuation *)

  type stack = Var.t (* Reified stack of fibers *)

  val pop_fiber : cont -> instr list * stack * cont
  (** [pop_fiber k] returns [(instrs, stack, k')], where [instrs] is
      the list of instructions necessary to pop the topmost fiber,
      binding the corresponding one-level stack to [stack] and the
      low-level continuation of the next fiber to [k']. *)

  val resume_stack : stack -> cont -> instr list * cont
  (** [resume_stack stack k] returns [(instr, k')], where [instrs] is
      the list of instructions necessary to updates the execution
      context with the stack of fibers in [stack] (so as to resume a
      continuation), binding the low-level continuation of the
      innermost fiber to [k'] *)

  val cons_fiber : stack -> continuation -> instr list
  (** [cons_fiber stack cont] returns the list of instructions
      necessary to append a one-level stack [stack] to the stack of
      fibers of the continuation. *)

  val get_effect_handler : stack -> instr list * Var.t
  (** [get_effect_handler stack] returns [(instrs, handler)], where
      [instrs] is the list of instructions necessary to bind the
      topmost effect handler in [stack] to [handler]. *)

  val continuation_of_stack : stack -> instr list * continuation
  (** [continuation_of_stack stack] returns [(instrs, handler)], where
      [instrs] is the list of instructions necessary to allocate a
      continuation corresponding to stack [stack] and bind it to
      [continuation] *)

  val push_trap : cont -> instr list
  (** [push_trap exn_handler] returns the list of instructions
      necessary to push the exception handler [exn_handler] to the
      stack of exception handlers. *)

  val pop_trap : unit -> instr list * cont
  (** [pop_trap exn_handler] returns [(instrs, exn_handler)], where
      [instrs] is the list of instructions necessary to remove and
      return the topmost element of the stack of exception handlers,
      and bind it to [exn_handler]. *)
end = struct
  type cont = Var.t

  type continuation = Var.t

  type stack = Var.t

  let pop_fiber k =
    let res = Var.fresh () in
    let stack = Var.fresh_n "stack" in
    let k' = Var.fresh_n "cont" in
    ( [ Let (res, Prim (Extern "caml_pop_fiber", [ Pv k ]))
      ; Let (stack, Field (res, 0))
      ; Let (k', Field (res, 1))
      ]
    , stack
    , k' )

  let resume_stack stack k =
    let k' = Var.fresh_n "cont" in
    [ Let (k', Prim (Extern "caml_resume_stack", [ Pv stack; Pv k ])) ], k'

  let cons_fiber fiber continuation =
    (* Assumes a one-level stack (the last component of [fiber] is empty). *)
    (* let Cons (k, exn_stack, handlers, Empty) = fiber in
       continuation := Cons (k, exn_stack, handlers, !continuation) *)
    let stack = Var.fresh () in
    let k = Var.fresh () and exn_stack = Var.fresh () and b = Var.fresh () in
    let handlers = Var.fresh () in
    [ Let (stack, Field (continuation, 0))
    ; Let (k, Field (fiber, 0))
    ; Let (exn_stack, Field (fiber, 1))
    ; Let (handlers, Field (fiber, 2))
    ; Let (b, Block (0, [| k; exn_stack; handlers; stack |], NotArray))
    ; Set_field (continuation, 0, b)
    ]

  let get_effect_handler stack =
    (* let Cons (_, _, h, _) = stack in h.effc *)
    let a = Var.fresh () in
    let handler = Var.fresh_n "effect_handler" in
    [ Let (a, Field (stack, 2)); Let (handler, Field (a, 2)) ], handler

  let continuation_of_stack stack =
    let continuation = Var.fresh_n "continuation" in
    [ Let (continuation, Block (245, [| stack |], NotArray)) ], continuation

  let push_trap exn_handler =
    [ Let (Var.fresh (), Prim (Extern "caml_push_trap", [ Pv exn_handler ])) ]

  let pop_trap () =
    let exn_handler = Var.fresh_n "raise" in
    [ Let (exn_handler, Prim (Extern "caml_pop_trap", [])) ], exn_handler
end

let cps_branch ~st (pc, args) =
  let ret = Var.fresh () in
  [ Let (ret, Apply { f = closure_of_pc ~st pc; args; exact = true }) ], Return ret

let cps_jump_cont ~st cont =
  let call_block =
    let body, branch = cps_branch ~st cont in
    add_block st { params = []; body; branch }
  in
  call_block, []

let cps_last ~st (last : last) ~(k : Runtime.cont) : instr list * last =
  match last with
  | Return x ->
      let ret = Var.fresh () in
      [ Let (ret, Apply { f = k; args = [ x ]; exact = true }) ], Return ret
  | Raise (x, _) ->
      let pop_instrs, exn_handler = Runtime.pop_trap () in
      let ret = Var.fresh () in
      ( pop_instrs @ [ Let (ret, Apply { f = exn_handler; args = [ x ]; exact = true }) ]
      , Return ret )
  | Stop -> [], Stop
  | Branch cont -> cps_branch ~st cont
  | Cond (x, cont1, cont2) ->
      [], Cond (x, cps_jump_cont ~st cont1, cps_jump_cont ~st cont2)
  | Switch (x, c1, c2) ->
      (* To avoid code duplication during JavaScript generation, we need
         to create a single block per continuation *)
      let cps_jump_cont = Fun.memoize (cps_jump_cont ~st) in
      [], Switch (x, Array.map c1 ~f:cps_jump_cont, Array.map c2 ~f:cps_jump_cont)
  | Pushtrap ((pc, args), x, handler_cont, _) ->
      let constr_handler, exn_handler =
        (* Construct handler closure *)
        allocate_closure ~st ~params:[ x ] ~body:[] ~branch:(Branch handler_cont)
      in
      let push = Runtime.push_trap exn_handler in
      let ret = Var.fresh () in
      ( constr_handler
        @ push
        @ [ Let (ret, Apply { f = closure_of_pc ~st pc; args; exact = true }) ]
      , Return ret )
  | Poptrap (pc, args) ->
      let pop, _ = Runtime.pop_trap () in
      let ret = Var.fresh () in
      ( pop @ [ Let (ret, Apply { f = closure_of_pc ~st pc; args; exact = true }) ]
      , Return ret )

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
  | Let (_, Apply _) -> assert false
  | _ -> instr

let cps_block ~st ~(k : Runtime.cont) pc block =
  let alloc_jump_closures =
    match Addr.Map.find pc st.jc.closures_of_alloc_site with
    | to_allocate ->
        List.map to_allocate ~f:(fun (cname, jump_pc) ->
            let jump_block = Addr.Map.find jump_pc st.blocks in
            let fresh_params = List.map jump_block.params ~f:(fun _ -> Var.fresh ()) in
            Let (cname, Closure (fresh_params, (jump_pc, fresh_params))))
    | exception Not_found -> []
  in

  let rewritten_block =
    match List.split_last block.body, block.branch with
    | Some (body_prefix, Let (x, e)), Return ret ->
        let rewritten_instrs =
          match e with
          | Apply { f; args; exact } ->
              Some [ Let (x, Apply { f; args = args @ [ k ]; exact }) ]
          | Prim (Extern "%resume", [ Pv stack; Pv f; Pv arg ]) ->
              let resume, k' = Runtime.resume_stack stack k in
              Some (resume @ [ Let (x, Apply { f; args = [ arg; k' ]; exact = false }) ])
          | Prim (Extern "%reperform", [ Pv eff; Pv continuation ]) ->
              (* We execute the effect handler of the current fiber in
                 the parent fiber. *)
              let pop, fiber, k' = Runtime.pop_fiber k in
              let cons_fiber = Runtime.cons_fiber fiber continuation in
              let read_handler, handler = Runtime.get_effect_handler fiber in
              Some
                (pop
                @ cons_fiber
                @ read_handler
                @ [ Let
                      ( x
                      , Apply
                          { f = handler
                          ; args = [ eff; continuation; k'; k' ]
                          ; exact = false
                          } )
                  ])
          | _ -> None
        in
        Option.map rewritten_instrs ~f:(fun instrs ->
            assert (List.is_empty alloc_jump_closures);
            assert (Var.equal x ret);
            body_prefix, instrs, block.branch)
    | Some (body_prefix, Let (x, e)), Branch cont -> (
        let allocate_continuation f =
          let constr_cont, k' =
            (* Construct continuation: it binds the return value [x],
               allocates closures for dominated blocks and jumps to the
               next block. *)
            let pc, args = cont in
            let ret = Var.fresh () in
            let f' = closure_of_pc ~st pc in
            allocate_closure
              ~st
              ~params:[ x ]
              ~body:
                (alloc_jump_closures @ [ Let (ret, Apply { f = f'; args; exact = true }) ])
              ~branch:(Return ret)
          in
          let ret = Var.fresh () in
          Some (body_prefix, constr_cont @ f ret k', Return ret)
        in
        match e with
        | Apply { f; args; exact; _ } ->
            allocate_continuation
            @@ fun ret k' -> [ Let (ret, Apply { f; args = args @ [ k' ]; exact }) ]
        | Prim (Extern "%resume", [ Pv stack; Pv f; Pv arg ]) ->
            allocate_continuation
            @@ fun ret k' ->
            (* Resume the continuation [stack] and invoke function [f]
               in this context *)
            let resume, k'' = Runtime.resume_stack stack k' in
            resume @ [ Let (ret, Apply { f; args = [ arg; k'' ]; exact = false }) ]
        | Prim (Extern "%perform", [ Pv effect ]) ->
            allocate_continuation
            @@ fun ret k' ->
            (* Create a continuation from the current fiber and
               execute the effect handler in the parent fiber. *)
            let pop, fiber, k'' = Runtime.pop_fiber k' in
            let allocate_continuation, continuation =
              Runtime.continuation_of_stack fiber
            in
            let read_handler, handler = Runtime.get_effect_handler fiber in
            pop
            @ allocate_continuation
            @ read_handler
            @ [ Let
                  ( ret
                  , Apply
                      { f = handler
                      ; args = [ effect; continuation; k''; k'' ]
                      ; exact = false
                      } )
              ]
        | _ -> None)
    | None, _
    | Some (_, (Set_field _ | Offset_ref _ | Array_set _ | Assign _)), _
    | _, (Raise _ | Stop | Cond _ | Switch _ | Pushtrap _ | Poptrap _) -> None
  in

  let body, last =
    match rewritten_block with
    | Some (body_prefix, last_instrs, last) ->
        List.map body_prefix ~f:(fun i -> cps_instr ~st i) @ last_instrs, last
    | None ->
        let last_instrs, last = cps_last ~st block.branch ~k in
        let body =
          List.map block.body ~f:(fun i -> cps_instr ~st i)
          @ alloc_jump_closures
          @ last_instrs
        in
        body, last
  in

  { params = block.params; body; branch = last }

let f (p : Code.program) =
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
  let p =
    Code.fold_closures
      p
      (fun _ _ (start, _) ({ blocks; free_pc; _ } as p) ->
        let cfg = build_graph blocks start in
        let idom = dominator_tree cfg in
        let closure_jc = jump_closures cfg idom in
        let st =
          { new_blocks = Addr.Map.empty, free_pc
          ; blocks
          ; jc = closure_jc
          ; closure_continuation
          }
        in
        let k = closure_continuation start in
        let blocks =
          Code.traverse
            { fold = Code.fold_children }
            (fun pc blocks ->
              Addr.Map.add pc (cps_block ~st ~k pc (Addr.Map.find pc blocks)) blocks)
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
          [ Let (main, Closure ([ closure_continuation p.start ], (p.start, [])))
          ; Let (args, Prim (Extern "%js_array", []))
          ; Let (res, Prim (Extern "caml_callback", [ Pv main; Pv args ]))
          ]
      ; branch = Return res
      }
      p.blocks
  in
  { start = new_start; blocks; free_pc = new_start + 1 }

let f p =
  let t = Timer.make () in
  let r = f p in
  if Debug.find "times" () then Format.eprintf "  effects: %a@." Timer.print t;
  r
