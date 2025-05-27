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

let stats = Debug.find "stats"

let debug_stats = Debug.find "stats-debug"

(****)

(*
We try to find a good order to traverse the code:
- when a function calls another function or contains another function,
  we process it after the other function
- in case of recursive cycles, we process functions called only once
  first
*)

let collect_closures p =
  let closures = Var.Hashtbl.create 128 in
  let rec traverse p enclosing pc =
    Code.traverse
      { fold = Code.fold_children }
      (fun pc () ->
        let block = Addr.Map.find pc p.blocks in
        List.iter
          ~f:(fun i ->
            match i with
            | Let (f, Closure (params, ((pc', _) as cont), _)) ->
                Var.Hashtbl.add closures f (params, cont, enclosing);
                traverse p (Some f) pc'
            | _ -> ())
          block.body)
      pc
      p.blocks
      ()
  in
  traverse p None p.start;
  closures

let collect_deps p closures =
  let deps = Var.Hashtbl.create (Var.Hashtbl.length closures) in
  Var.Hashtbl.iter (fun f _ -> Var.Hashtbl.add deps f (ref Var.Set.empty)) closures;
  let traverse p g pc =
    let add_dep f =
      if Var.Hashtbl.mem closures f
      then
        let s = Var.Hashtbl.find deps f in
        s := Var.Set.add g !s
    in
    Code.traverse
      { fold = Code.fold_children }
      (fun pc () ->
        let block = Addr.Map.find pc p.blocks in
        Freevars.iter_block_free_vars add_dep block;
        List.iter
          ~f:(fun i ->
            match i with
            | Let (f, Closure _) -> add_dep f
            | _ -> ())
          block.body)
      pc
      p.blocks
      ()
  in
  Var.Hashtbl.iter (fun f (_, (pc, _), _) -> traverse p f pc) closures;
  Var.Hashtbl.fold (fun f s m -> Var.Map.add f !s m) deps Var.Map.empty

module Var_SCC = Strongly_connected_components.Make (Var)

let visit_closures p ~live_vars f acc =
  let closures = collect_closures p in
  let deps = collect_deps p closures in
  let f' ~recursive acc g =
    let params, cont, enclosing_function = Var.Hashtbl.find closures g in
    f ~recursive ~enclosing_function ~current_function:(Some g) ~params ~cont acc
  in
  let rec visit ~recursive deps acc =
    let scc = Var_SCC.connected_components_sorted_from_roots_to_leaf deps in
    Array.fold_left
      scc
      ~f:(fun acc group ->
        match group with
        | Var_SCC.No_loop g -> f' ~recursive acc g
        | Has_loop l ->
            let set = Var.Set.of_list l in
            let deps' =
              List.fold_left
                ~f:(fun deps' g ->
                  Var.Map.add
                    g
                    (Var.Set.inter
                       (if recursive || live_vars.(Var.idx g) > 1
                        then
                          (* Make sure that inner closures are
                             processed before their enclosing
                             closure *)
                          let _, _, enclosing = Var.Hashtbl.find closures g in
                          match enclosing with
                          | None -> Var.Set.empty
                          | Some enclosing -> Var.Set.singleton enclosing
                        else Var.Map.find g deps)
                       set)
                    deps')
                ~init:Var.Map.empty
                l
            in
            visit ~recursive:true deps' acc)
      ~init:acc
  in
  let acc = visit ~recursive:false deps acc in
  f
    ~recursive:false
    ~enclosing_function:None
    ~current_function:None
    ~params:[]
    ~cont:(p.start, [])
    acc

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

(* Information about a function candidate for inlining. Some
   information / statistics about this function are computed lazily
   and stored there. *)

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
  ; returns_a_block : bool cache
  ; interesting_params : (Var.t * int) list cache
  }

type context =
  { profile : Profile.t  (** Aggressive inlining? *)
  ; p : program
  ; live_vars : int array  (** Occurence count of all variables *)
  ; inline_count : int ref  (** Inlining statistics *)
  ; env : info Var.Map.t  (** Functions that are candidate for inlining *)
  ; in_loop : bool  (** Whether the current block is in a loop *)
  ; has_closures : bool ref  (** Whether the current function contains closures *)
  ; current_function : Var.t option  (** Name of the current function *)
  ; enclosing_function : Var.t option
        (** Name of the function enclosing the current function *)
  }
(** Current context into which we consider inlining some functions. *)

let cache ~info:{ cont = pc, _; _ } ref f =
  match !ref with
  | Some v -> v
  | None ->
      let v = f pc in
      ref := Some v;
      v

(** Does the function contain a loop? *)
let contains_loop ~context info =
  cache ~info info.loops (fun pc ->
      let rec traverse pc ((visited, loop) as accu) : _ * bool =
        if loop
        then accu
        else if Addr.Map.mem pc visited
        then visited, Addr.Map.find pc visited
        else
          let visited, loop =
            Code.fold_children
              context.p.blocks
              pc
              traverse
              (Addr.Map.add pc true visited, false)
          in
          Addr.Map.add pc false visited, loop
      in
      snd (traverse pc (Addr.Map.empty, false)))

let sum ~context f pc =
  let blocks = context.p.blocks in
  Code.traverse
    { fold = fold_children }
    (fun pc acc -> f (Addr.Map.find pc blocks) + acc)
    pc
    blocks
    0

let rec block_size ~recurse ~context { branch; body; _ } =
  List.fold_left
    ~f:(fun n i ->
      match i with
      | Event _ -> n
      | Let (_, Closure (_, (pc, _), _)) ->
          if recurse then size ~recurse ~context pc + n + 1 else n + 1
      | _ -> n + 1)
    ~init:
      (match branch with
      | Cond _ | Raise _ -> 2
      | Switch (_, a1) -> Array.length a1
      | _ -> 0)
    body

and size ~recurse ~context = sum ~context (block_size ~recurse ~context)

(** Size of the function body *)
let body_size ~context info = cache ~info info.body_size (size ~recurse:false ~context)

(** Size of the function, including the size of the closures it contains *)
let full_size ~context info = cache ~info info.full_size (size ~recurse:true ~context)

let closure_count_uncached ~context =
  sum ~context (fun { body; _ } ->
      List.fold_left
        ~f:(fun n i ->
          match i with
          | Let (_, Closure _) -> n + 1
          | _ -> n)
        ~init:0
        body)

(** Number of closures contained in the function *)
let closure_count ~context info =
  cache ~info info.closure_count (closure_count_uncached ~context)

(** Number of instructions in the function which look like
    initialization code. *)
let count_init_code ~context info =
  cache
    ~info
    info.init_code
    (sum ~context
    @@ fun { body; _ } ->
    List.fold_left
      ~f:(fun n i ->
        match i with
        | Let (_, (Closure _ | Field _ | Constant _ | Block _)) -> n + 1
        | Let (_, (Apply _ | Prim _ | Special _))
        | Assign _ | Set_field _ | Offset_ref _ | Array_set _ | Event _ -> n)
      ~init:0
      body)

(** Whether the function returns a block. *)
let returns_a_block ~context info =
  cache ~info info.returns_a_block (fun pc ->
      let blocks = context.p.blocks in
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

(** List of parameters that corresponds to functions called once in
    the function body. *)
let interesting_parameters ~context info =
  let params = info.params in
  cache ~info info.interesting_params (fun pc ->
      let params = List.filter ~f:(fun x -> context.live_vars.(Var.idx x) = 1) params in
      if List.is_empty params
      then []
      else
        let blocks = context.p.blocks in
        Code.traverse
          { fold = fold_children }
          (fun pc lst ->
            let block = Addr.Map.find pc blocks in
            List.fold_left
              ~f:(fun lst i ->
                match i with
                | Let (_, Apply { f; args; _ }) when List.mem ~eq:Var.equal f params ->
                    (f, List.length args) :: lst
                | _ -> lst)
              ~init:lst
              block.body)
          pc
          blocks
          [])

(*
   We are very aggressive at optimizing functor-like code, even if
   this might duplicate quite a lot of code, since this is likely to
   allow other optimizations: direct function calls, more precise dead
   code elimination, ...
*)
let functor_like ~context info =
  (match Config.target (), context.profile with
  | `Wasm, (O2 | O3) -> true
  | `Wasm, O1 -> body_size ~context info <= 15
  | `JavaScript, (O1 | O2) -> false
  | `JavaScript, O3 -> body_size ~context info <= 15)
  && (not info.recursive)
  && (not (contains_loop ~context info))
  && returns_a_block ~context info
  && count_init_code ~context info * 2 > body_size ~context info
  (* A large portion of the body is initialization code *)
  &&
  (* The closures defined in this function are small on average *)
  full_size ~context info - body_size ~context info <= 20 * closure_count ~context info

let trivial_function ~context info =
  body_size ~context info <= 1 && closure_count ~context info = 0

(*
  We inline small functions which are simple (no closure, no
  recursive) when one of the argument is a function that would get
  inlined afterwards.
*)
let rec small_function ~context info args =
  (not info.recursive)
  && body_size ~context info <= 15
  && closure_count ~context info = 0
  && (not (List.is_empty args))
  &&
  let relevant_params = interesting_parameters ~context info in
  try
    List.iter2 args info.params ~f:(fun arg param ->
        if
          Var.Map.mem arg context.env
          && List.exists ~f:(fun (p, _) -> Var.equal p param) relevant_params
        then
          let info' = Var.Map.find arg context.env in
          let _, arity = List.find ~f:(fun (p, _) -> Var.equal p param) relevant_params in
          if
            List.compare_length_with info'.params ~len:arity = 0
            && should_inline
                 ~context:
                   { context with
                     in_loop = context.in_loop || contains_loop ~context info
                   }
                 info'
                 []
          then raise Exit);
    false
  with Exit -> true

and should_inline ~context info args =
  (* Typically, in JavaScript implementations, a closure contains a
     pointer to (recursively) the contexts of its enclosing functions.
     The context of a function contains the variables bound in this
     function which are referred to from one of the enclosed function.
     To limit the risk of memory leaks, we try to avoid inlining functions
     containing closures if this makes these closures capture
     additional contexts shared with other closures.
     We still inline into toplevel functions ([Option.is_none
     context.enclosing_function]) since this results in significant
     performance improvements. *)
  (match Config.target (), Config.effects () with
  | `JavaScript, (`Disabled | `Cps) ->
      closure_count ~context info = 0
      || Option.is_none context.enclosing_function
      || Option.equal Var.equal info.enclosing_function context.current_function
      || (not !(context.has_closures))
         && Option.equal Var.equal info.enclosing_function context.enclosing_function
  | `Wasm, _ | `JavaScript, `Double_translation -> true
  | `JavaScript, `Jspi -> assert false)
  && (functor_like ~context info
     || (context.live_vars.(Var.idx info.f) = 1
        &&
        match Config.target () with
        | `Wasm when context.in_loop ->
            (* Avoid inlining in a loop since, if the loop is not hot,
               the code might never get optimized *)
            body_size ~context info < 30 && not (contains_loop ~context info)
        | `JavaScript
          when Option.is_none context.current_function && contains_loop ~context info ->
            (* Avoid inlining loops at toplevel since the toplevel
               code is less likely to get optimized *)
            false
        | _ -> body_size ~context info < Config.Param.inlining_limit ())
     || trivial_function ~context info
     || small_function ~context info args)

let trace_inlining ~context info x args =
  if debug ()
  then
    let sz = body_size ~context info in
    let sz' = full_size ~context info in
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
      (should_inline ~context info args)
      context.live_vars.(Var.idx info.f)
      sz
      sz'
      (contains_loop ~context info)
      info.recursive
      (closure_count ~context info)
      (count_init_code ~context info)
      (returns_a_block ~context info)
      (functor_like ~context info)
      (small_function ~context info args)

(****)

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
    | [], Return y when Var.equal x y ->
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
  assert (List.compare_lengths args params = 0);
  let blocks =
    Addr.Map.add fresh_addr { params; body = []; branch = Branch cont } blocks
  in
  [], (Branch (fresh_addr, args), { p with blocks; free_pc })

let inline_in_block ~context pc block p =
  let body, (branch, p) =
    List.fold_right
      ~f:(fun i (rem, state) ->
        match i with
        | Let (x, Apply { f; args; exact = true; _ }) when Var.Map.mem f context.env ->
            let info = Var.Map.find f context.env in
            let { params; cont; _ } = info in
            trace_inlining ~context info x args;
            if should_inline ~context info args
            then (
              let branch, p = state in
              incr context.inline_count;
              if closure_count ~context info > 0 then context.has_closures := true;
              context.live_vars.(Var.idx f) <- context.live_vars.(Var.idx f) - 1;
              if context.live_vars.(Var.idx f) > 0
              then
                let p, _, params, cont = Duplicate.closure p ~f ~params ~cont in
                inline_function p rem branch x params cont args
              else inline_function p rem branch x params cont args)
            else i :: rem, state
        | _ -> i :: rem, state)
      ~init:([], (block.branch, p))
      block.body
  in
  { p with blocks = Addr.Map.add pc { block with body; branch } p.blocks }

let inline ~profile ~inline_count p ~live_vars =
  if debug () then Format.eprintf "====== inlining ======@.";
  (visit_closures
     p
     ~live_vars
     (fun ~recursive
          ~enclosing_function
          ~current_function
          ~params
          ~cont:((pc, _) as cont)
          (context : context)
        ->
       let p = context.p in
       let has_closures = ref (closure_count_uncached ~context pc > 0) in
       let in_loop = blocks_in_loop p pc in
       let context =
         { context with has_closures; enclosing_function; current_function }
       in
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
                   | Let (_, Apply { f; _ }) -> not (Var.Map.mem f context.env)
                   | _ -> true)
                 block.body
             then p
             else
               inline_in_block
                 ~context:{ context with in_loop = Addr.Set.mem pc in_loop }
                 pc
                 block
                 p)
           pc
           p.blocks
           p
       in
       let p = remove_dead_closures ~live_vars p pc in
       let env =
         match current_function with
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
               ; returns_a_block = ref None
               ; interesting_params = ref None
               }
               context.env
         | None -> context.env
       in
       { context with p; env })
     { profile
     ; p
     ; live_vars
     ; inline_count
     ; env = Var.Map.empty
     ; in_loop = false
     ; has_closures = ref false
     ; current_function = None
     ; enclosing_function = None
     })
    .p

(****)

let f ~profile p live_vars =
  let previous_p = p in
  let inline_count = ref 0 in
  Code.invariant p;
  let t = Timer.make () in
  let p = inline ~profile ~inline_count p ~live_vars in
  if times () then Format.eprintf "  inlining: %a@." Timer.print t;
  if stats () then Format.eprintf "Stats - inlining: %d inlined functions@." !inline_count;
  if debug_stats ()
  then Code.check_updates ~name:"inline" previous_p p ~updates:!inline_count;
  let p = Deadcode.remove_unused_blocks p in
  Code.invariant p;
  p
