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

(* Extract outermost loops of the entry function into separate helper
   closures, so engines tier up the small helpers instead of the huge
   toplevel. See [Hoist_loops.f] for the entry point. *)

open! Stdlib
open Code

let debug = Debug.find "hoist-loops"

let times = Debug.find "times"

let stats = Debug.find "stats"

(* {1 Small utilities} *)

(* Build a substitution that forks each variable in [vs]. *)
let fork_set vs = Var.Set.fold (fun v m -> Var.Map.add v (Var.fork v) m) vs Var.Map.empty

(* Pack a list of values into a single result value to be returned from the
   helper. Always uses a [Block] when there is at least one value to make
   the call site's unpacking uniform. *)
let pack_for_return values =
  match values with
  | [] ->
      let u = Var.fresh () in
      [ Let (u, Constant (Int Targetint.zero)) ], u
  | _ ->
      let t = Var.fresh () in
      let arr = Array.of_list values in
      [ Let (t, Block (0, arr, NotArray, Immutable)) ], t

(* {1 Loop discovery} *)

(* Walk the dominator tree from [root] and collect the headers of outermost
   loops (those not nested inside another loop). When we hit a loop header,
   we record it and stop descending. *)
let outermost_loop_headers ~g ~dom ~root =
  let rec walk pc acc =
    if Structure.is_loop_header g pc
    then pc :: acc
    else Addr.Set.fold walk (Structure.get_edges dom pc) acc
  in
  walk root []

(* Blocks of [body] from which [h] can be reached without leaving [body]:
   the natural loop. *)
let natural_loop blocks ~h body =
  let preds = Addr.Hashtbl.create 16 in
  Addr.Set.iter
    (fun pc ->
      fold_children
        blocks
        pc
        (fun pc' () ->
          if Addr.Set.mem pc' body
          then
            let s =
              Addr.Hashtbl.find_opt preds pc' |> Option.value ~default:Addr.Set.empty
            in
            Addr.Hashtbl.replace preds pc' (Addr.Set.add pc s))
        ())
    body;
  let rec mark pc acc =
    if Addr.Set.mem pc acc
    then acc
    else
      Addr.Set.fold
        mark
        (Addr.Hashtbl.find_opt preds pc |> Option.value ~default:Addr.Set.empty)
        (Addr.Set.add pc acc)
  in
  mark h Addr.Set.empty

(* Loop body of [h]: every block in the dominator-tree subtree rooted at
   [h]. [Structure.build_graph] runs [shrink_loops] on the graph before
   computing the dominator tree, which adds artificial predecessors from
   outside the loop to "non-small" post-loop blocks. Those blocks then
   fall out of [h]'s dom-subtree, leaving the natural loop body plus any
   small chain blocks (typical of OCaml [match] arms that bind a constant
   and branch to a post-match join) — which is what we want to extract.

   Post-loop blocks that lead to [Stop] are the end of the toplevel
   (module registration, ...). They are left in the caller: they do not
   belong in a hot function. These post-loop blocks are small, hence
   contain no loop, so the recursion below terminates. *)
let loop_body blocks ~dom ~h =
  let rec walk pc acc =
    Addr.Set.fold walk (Structure.get_edges dom pc) (Addr.Set.add pc acc)
  in
  let body = walk h Addr.Set.empty in
  let in_loop = natural_loop blocks ~h body in
  let memo = Addr.Hashtbl.create 16 in
  let rec reaches_stop pc =
    (not (Addr.Set.mem pc in_loop))
    &&
    match Addr.Hashtbl.find_opt memo pc with
    | Some b -> b
    | None ->
        let b =
          match (Addr.Map.find pc blocks).branch with
          | Stop -> true
          | _ ->
              fold_children
                blocks
                pc
                (fun pc' acc -> acc || (Addr.Set.mem pc' body && reaches_stop pc'))
                false
        in
        Addr.Hashtbl.add memo pc b;
        b
  in
  let body = Addr.Set.filter (fun pc -> not (reaches_stop pc)) body in
  (* Drop the blocks only reachable through the blocks removed above *)
  let rec reachable pc acc =
    if Addr.Set.mem pc acc || not (Addr.Set.mem pc body)
    then acc
    else fold_children blocks pc reachable (Addr.Set.add pc acc)
  in
  reachable h Addr.Set.empty

(* Variables bound in [body_set]. Nested closures' bodies are not descended
   into; their bindings aren't visible outside the closure. *)
let bound_in_body blocks body_set =
  Addr.Set.fold
    (fun pc acc ->
      let acc = ref acc in
      Freevars.iter_block_bound_vars
        (fun v -> acc := Var.Set.add v !acc)
        (Addr.Map.find pc blocks);
      !acc)
    body_set
    Var.Set.empty

type loop =
  { header : Addr.t
  ; body_set : Addr.Set.t
  ; bound : Var.Set.t
  }

(* {1 Body-set analyses} *)

(* Live-out: variables bound in [body_set] that are referenced from a
   [p.start]-level block outside [body_set]. These must be returned from
   the helper and rebound in the dispatch block under their original
   names; otherwise their uses elsewhere in the function would dangle once
   the binder moved into the helper.

   [uses] only records [p.start]-level blocks ([compute_uses] attributes
   every use to the outermost closure-defining block via [attribute_to]),
   so a use of [v] from inside a closure defined in [body_set] shows up
   here as a use by that closure's defining block — which lives in
   [body_set] and is correctly classified as "inside". No separate
   inner-closure filter is needed.

   [uses] is computed once at the start of the pass and never updated. As
   later loops are extracted, some recorded uses get renamed to forks
   inside already-built helpers, but those user-blocks still sit outside
   any yet-to-be-processed loop's body, so [v] is at worst over-
   approximated as live-out rather than dropped. *)
let live_out_of_loop ~uses ~body_set ~bound =
  Var.Set.filter
    (fun v ->
      match Var.Hashtbl.find_opt uses v with
      | None -> false
      | Some user_blocks -> not (Addr.Set.subset user_blocks body_set))
    bound

(* The body contains an [Assign] whose target is bound outside the loop —
   the helper can't observe that side-effect on the caller's binding. *)
let has_outer_assign blocks body_set bound =
  Addr.Set.exists
    (fun pc ->
      let block = Addr.Map.find pc blocks in
      List.exists block.body ~f:(function
        | Assign (x, _) -> not (Var.Set.mem x bound)
        | _ -> false))
    body_set

exception Skip of string

(* How the loop is left. With [poptrap = true], the loop is directly
   inside a [try] body and is left by popping the corresponding
   exception handler, so the dispatch block must pop it as well. *)
type exit_kind =
  | No_exit
  | Exit of
      { pc : Addr.t
      ; poptrap : bool
      }

(* Traverse the body from [h], tracking the exception handler depth
   relative to [h]. Returns the depth of each body block and how the loop
   is left. The helper must leave the trap stack balanced: an edge
   leaving the body must be at depth 0, or be a [Poptrap] at depth 0
   (whose matching [Pushtrap] is then outside the loop), and no [Poptrap]
   can pop a handler pushed outside the loop while staying in the body. *)
let classify_exits blocks ~h body_set =
  let depths = Addr.Hashtbl.create 16 in
  let exit = ref None in
  let add_exit pc ~poptrap =
    match !exit with
    | None -> exit := Some (pc, poptrap)
    | Some (pc', poptrap') ->
        if pc <> pc' || Bool.(poptrap <> poptrap') then raise (Skip "multi-exit")
  in
  let rec visit pc d =
    match Addr.Hashtbl.find_opt depths pc with
    | Some d' -> if d <> d' then raise (Skip "inconsistent-trap-depth")
    | None ->
        Addr.Hashtbl.add depths pc d;
        let branch = (Addr.Map.find pc blocks).branch in
        fold_children
          blocks
          pc
          (fun pc' () ->
            let d' =
              match branch with
              | Pushtrap ((body_pc, _), _, _) when pc' = body_pc -> d + 1
              | Poptrap _ -> d - 1
              | _ -> d
            in
            if Addr.Set.mem pc' body_set
            then if d' < 0 then raise (Skip "outer-poptrap") else visit pc' d'
            else if d' = 0
            then add_exit pc' ~poptrap:false
            else if d' = -1
            then add_exit pc' ~poptrap:true
            else raise (Skip "exit-inside-try"))
          ()
  in
  visit h 0;
  (* Every block of the dominator subtree should be reachable from [h] *)
  if Addr.Hashtbl.length depths <> Addr.Set.cardinal body_set
  then raise (Skip "unreachable-block");
  ( depths
  , match !exit with
    | None -> No_exit
    | Some (pc, poptrap) -> Exit { pc; poptrap } )

(* {1 Program-wide analyses} *)

(* "var -> set of blocks that use it" map, restricted to variables in
   [tracked] (the variables bound in some loop body). A block at the
   [p.start] level is recorded as a user of [v] either if [v] appears free
   in the block itself or if [v] appears free in a closure (transitively)
   defined in that block — in which case the use is attributed to the
   outermost closure-defining block (the one at [p.start] level), since
   that is what [live_out_of_loop] checks against [body_set].

   The walk traverses every block reachable from [p.start] — practically
   the whole program. The benefit over a preliminary [Freevars.f] pass is
   that we never materialise a full per-closure free-var set: the
   [tracked] filter at [add] discards uses we don't care about as we go,
   keeping only the variables that any [body_set] could bind. *)
let compute_uses ~tracked p =
  let uses = Var.Hashtbl.create 64 in
  let add pc v =
    if Var.Set.mem v tracked
    then
      let set = Var.Hashtbl.find_opt uses v |> Option.value ~default:Addr.Set.empty in
      Var.Hashtbl.replace uses v (Addr.Set.add pc set)
  in
  let visited = BitSet.create' p.free_pc in
  let rec walk ~attribute_to pc =
    if not (BitSet.mem visited pc)
    then (
      BitSet.set visited pc;
      let block = Addr.Map.find pc p.blocks in
      let user_pc = Option.value attribute_to ~default:pc in
      Freevars.iter_block_free_vars (add user_pc) block;
      List.iter block.body ~f:(function
        | Let (_, Closure (_, (pc_clo, _), _)) -> walk ~attribute_to:(Some user_pc) pc_clo
        | _ -> ());
      fold_children p.blocks pc (fun pc' () -> walk ~attribute_to pc') ())
  in
  walk ~attribute_to:None p.start;
  uses

(* Outside-of-body predecessors of [h]. The [Structure.t] built once at the
   start of the pass gives the original predecessors in O(preds(h)); we
   union with [extra_preds.(h)], which tracks new edges introduced by
   earlier loop extractions (each dispatch block branches to its loop's
   exit, possibly landing on a future loop's header). *)
let outside_preds_current ~g ~extra_preds ~h ~body_set =
  let extra =
    Addr.Hashtbl.find_opt extra_preds h |> Option.value ~default:Addr.Set.empty
  in
  Addr.Set.diff (Addr.Set.union (Structure.preds g h) extra) body_set

(* {1 Body rewriting} *)

(* Rewrite the [last] instruction of a body block after the caller has
   already substituted its variable references via [Including_Binders.block].
   For each cont whose target leaves [body_set], redirect to a freshly-
   synthesised [Return] block that packs the cont args together with the
   helper's live-out forks and returns the result.

   A [Poptrap] at depth 0 pops a handler pushed outside the loop (it
   necessarily leaves the body, see [classify_exits]). Convert it to a
   plain [Branch]; the dispatch block pops the handler instead. *)
let rewrite_branch ~depth ~body_set ~live_out_forks ~make_return_block branch =
  let redirect (pc, args) =
    if Addr.Set.mem pc body_set
    then pc, args
    else make_return_block (args @ live_out_forks)
  in
  match branch with
  | Poptrap c when depth = 0 -> Branch (redirect c)
  | Return _ ->
      (* The entry function has no [Return] terminator. *)
      assert false
  | _ -> map_branch_conts redirect branch

(* {1 Main pass} *)

let try_extract_loop ~g ~uses ~extra_preds { header = h; body_set; bound } p =
  try
    if has_outer_assign p.blocks body_set bound then raise (Skip "outer-assign");
    let depths, exit = classify_exits p.blocks ~h body_set in
    (* Capture outside predecessors of [h] *before* we add new blocks
       below — [entry_pc] also branches to [h] and would otherwise show
       up here as a spurious outside-pred. *)
    let outside = outside_preds_current ~g ~extra_preds ~h ~body_set in
    let live_out = live_out_of_loop ~uses ~body_set ~bound in
    (* Fork the [live_out] vars so the dispatch block can rebind them
       under their original names without double-binding. Other
       variables bound in the body are only used there, so they can keep
       their names. Live-in vars remain free in the helper closure body
       and the JS / wasm backend captures them lexically from the
       enclosing scope. *)
    let fork_map = fork_set live_out in
    let s = Subst.from_map fork_map in
    let live_out_list = Var.Set.elements live_out in
    let live_out_forks = List.map live_out_list ~f:(fun v -> Var.Map.find v fork_map) in
    let h_block = Addr.Map.find h p.blocks in
    let fresh_for_h_params name =
      List.map h_block.params ~f:(fun _ -> Var.fresh_n name)
    in
    let helper_init_args = fresh_for_h_params "loop_init" in
    let free_pc = ref p.free_pc in
    let alloc_pc () =
      let n = !free_pc in
      incr free_pc;
      n
    in
    let entry_pc = alloc_pc () in
    let disp_pc = alloc_pc () in
    let new_blocks = ref Addr.Map.empty in
    let make_return_block packed_args =
      let pc = alloc_pc () in
      let body, ret = pack_for_return packed_args in
      let blk = { params = []; body; branch = Return ret } in
      new_blocks := Addr.Map.add pc blk !new_blocks;
      pc, []
    in
    (* Rewrite each body block: [Including_Binders] renames its binders
       and uses; [rewrite_branch] redirects exit edges to Return blocks.
       For closures defined in the body, [Excluding_Binders.cont']
       substitutes uses inside the closure body (binders inside the
       closure stay put). *)
    let updated_body =
      Addr.Set.fold
        (fun pc m ->
          let b = Addr.Map.find pc p.blocks in
          let new_b =
            if Var.Map.is_empty fork_map then b else Subst.Including_Binders.block s b
          in
          let branch =
            rewrite_branch
              ~depth:(Addr.Hashtbl.find depths pc)
              ~body_set
              ~live_out_forks
              ~make_return_block
              new_b.branch
          in
          let m = Addr.Map.add pc { new_b with branch } m in
          if Var.Map.is_empty fork_map
          then m
          else
            List.fold_left b.body ~init:m ~f:(fun m i ->
                match i with
                | Let (_, Closure (_, (pc_clo, _), _)) ->
                    let m, _ = Subst.Excluding_Binders.cont' s pc_clo m Addr.Set.empty in
                    m
                | _ -> m))
        body_set
        p.blocks
    in
    let updated_body = Addr.Map.fold Addr.Map.add !new_blocks updated_body in
    (* Helper entry: branch to [h] passing the closure's "initial args"
       parameters as values for h's params. *)
    let entry_block = { params = []; body = []; branch = Branch (h, helper_init_args) } in
    let updated_body = Addr.Map.add entry_pc entry_block updated_body in
    let helper_var = Var.fresh_n "loop_helper" in
    let helper_closure = Closure (helper_init_args, (entry_pc, []), (None, None)) in
    (* Dispatch block: define the helper, call it, unpack the return,
       and branch to the loop's exit (or [Stop] if there isn't one). *)
    let disp_params = fresh_for_h_params "disp_arg" in
    let ret_var = Var.fresh_n "loop_ret" in
    let setup =
      [ Let (helper_var, helper_closure)
      ; Let (ret_var, Apply { f = helper_var; args = disp_params; exact = true })
      ]
    in
    let unpack_body, disp_branch, branch_target =
      match exit with
      | No_exit ->
          (* No exit edges from the body: every path ends in [Raise] (or
             the loop never terminates). In well-formed SSA, no block
             outside [body_set] can reach a body-bound binder, so
             [live_out] is empty and there's nothing to unpack. The
             dispatch's branch is unreachable. *)
          assert (List.is_empty live_out_list);
          [], Stop, None
      | Exit { pc = exit_pc; poptrap } ->
          let exit_arity = List.length (Addr.Map.find exit_pc updated_body).params in
          (* Unpack the [exit_args ++ live_out_forks] tuple that
             [pack_for_return] built. *)
          let exit_unpack =
            List.init ~len:exit_arity ~f:(fun i ->
                let x = Var.fresh_n "loop_unpack" in
                x, Let (x, Field (ret_var, i, Non_float)))
          in
          let live_out_unpack =
            List.mapi live_out_list ~f:(fun j v ->
                Let (v, Field (ret_var, exit_arity + j, Non_float)))
          in
          let cont = exit_pc, List.map ~f:fst exit_unpack in
          ( List.map ~f:snd exit_unpack @ live_out_unpack
          , (if poptrap then Poptrap cont else Branch cont)
          , Some exit_pc )
    in
    let disp_block =
      { params = disp_params; body = setup @ unpack_body; branch = disp_branch }
    in
    let updated_body = Addr.Map.add disp_pc disp_block updated_body in
    (* Register the dispatch's outgoing edge so a future loop whose
       header is reachable from there sees [disp_pc] as a predecessor. *)
    Option.iter branch_target ~f:(fun target ->
        let s =
          Addr.Hashtbl.find_opt extra_preds target |> Option.value ~default:Addr.Set.empty
        in
        Addr.Hashtbl.replace extra_preds target (Addr.Set.add disp_pc s));
    (* Redirect outside predecessors of [h] from [h] to [disp_pc]. *)
    let redirect_to_disp (pc, args) = if pc = h then disp_pc, args else pc, args in
    let updated_body =
      Addr.Set.fold
        (fun s_pc blocks ->
          let b = Addr.Map.find s_pc blocks in
          let branch = map_branch_conts redirect_to_disp b.branch in
          Addr.Map.add s_pc { b with branch } blocks)
        outside
        updated_body
    in
    if debug ()
    then
      Format.eprintf
        "HOIST loop @%d (body=%d, live_out=%d)@."
        h
        (Addr.Set.cardinal body_set)
        (Var.Set.cardinal live_out);
    { p with blocks = updated_body; free_pc = !free_pc }, true
  with Skip reason ->
    if debug () then Format.eprintf "SKIP loop @%d: %s@." h reason;
    p, false

let f p =
  let t = Timer.make () in
  let p' = Structure.norm p in
  let g = Structure.build_graph p'.blocks p'.start in
  let dom = Structure.dominator_tree g in
  let headers = outermost_loop_headers ~g ~dom ~root:p'.start in
  let hoisted = ref 0 in
  let left_in_place = ref 0 in
  let p =
    match headers with
    | [] -> p
    | _ :: _ ->
        let loops =
          List.map headers ~f:(fun h ->
              let body_set = loop_body p'.blocks ~dom ~h in
              { header = h; body_set; bound = bound_in_body p'.blocks body_set })
        in
        let tracked =
          List.fold_left loops ~init:Var.Set.empty ~f:(fun s l -> Var.Set.union s l.bound)
        in
        let uses = compute_uses ~tracked p' in
        let extra_preds = Addr.Hashtbl.create 16 in
        let p' =
          List.fold_left loops ~init:p' ~f:(fun p l ->
              let p, did = try_extract_loop ~g ~uses ~extra_preds l p in
              if did then incr hoisted else incr left_in_place;
              p)
        in
        (* Leave the program untouched when nothing was hoisted *)
        if !hoisted > 0 then p' else p
  in
  if stats ()
  then
    Format.eprintf
      "Stats - loop hoisting: hoisted %d, left in place %d@."
      !hoisted
      !left_in_place;
  if times () then Format.eprintf "  loop hoisting: %a@." Timer.print t;
  if !hoisted > 0 then Code.invariant p;
  p
