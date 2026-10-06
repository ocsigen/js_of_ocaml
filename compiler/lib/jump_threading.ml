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

(* Jump threading.

   We look for blocks [J] which only perform a test ([Cond] or [Switch]),
   possibly after computing the tested value from a single primitive
   ([IsInt], [Not], an integer comparison, or [%direct_obj_tag]). When the
   outcome of the test is known on an edge [P -> J], the edge is redirected to
   the continuation [K] the test would select, so that the test is not
   performed on this path. The value of the tested variable is known on the
   edge when it is a constant or a block defined in [P] (or in its unique
   predecessors, up to a small depth), or when [P] itself tests the same
   variable ([if c then J (c) else ...]), or when [P] is a [Switch] on it.

   Single static assignment. The variables bound in [J] (its parameters and
   the variable defined in its body) may be used in the blocks dominated by
   [J]. Once an edge bypasses [J], some of these blocks may no longer be
   dominated by [J]. We require that each use of these variables outside of
   [J] occurs in a block dominated by a successor [S] of [J] whose only
   predecessor is [J] (in particular, we do not handle the uses in the
   functions defined inside the function containing [J]):
   - if [S] is not the target [K] of the threaded edge, [S] (and thus the
     use) remains dominated by [J], as the only predecessor of [S] is [J];
   - if [S = K], we add parameters to [K] for the variables of [J] used in
     the blocks it dominates, and replace these uses by the parameters. [J]
     passes its own variables, and the threaded edge passes their values on
     this edge (the arguments of [P]; we do not thread the edge when the
     variable defined in the body of [J] is needed). The blocks dominated by
     [K] remain dominated by [K], as the only new edge goes to [K].
   The arguments [J] passes to [K] are rewritten by replacing the parameters
   of [J] by the arguments of the edge. The other variables they mention are
   defined in a block that strictly dominates [J], hence dominates [P].

   Reducibility. We never thread an edge to a block [J] which is a loop
   header (target of a back edge), as determined at the beginning of the
   pass. Any cycle in the transformed graph maps to a closed walk in the
   original graph, by putting back the blocks that have been bypassed. In the
   original (reducible) graph, this walk contains a loop header which
   dominates it. This header is not one of the bypassed blocks, so it is on
   the new cycle. It still dominates the nodes of the cycle, since paths in
   the new graph also map to paths in the original graph by adding bypassed
   blocks only. Hence the transformed graph is still reducible: we never
   create a new entry into a loop.

   Exception handlers. Only [Branch], [Cond] and [Switch] edges are
   redirected, and [J] is never the entry block of a function, nor the body
   or handler of a [Pushtrap]. Since [P -> J] and [J -> K] are ordinary
   edges, [P], [J] and [K] are within the same exception handlers.

   Several edges can be threaded in one pass. To keep the dominance
   information valid, a block is not handled as [J] if it has already been
   modified during the pass (as [J], as a source [P], or as the target [K]
   of a threaded edge). Further opportunities are handled in the next
   optimization round. *)

open! Stdlib

let times = Debug.find "times"

let stats = Debug.find "stats"

let debug_stats = Debug.find "stats-debug"

open Code

(* What we know of the value of a variable *)
type value =
  | Known_int of Targetint.t
  | Known_block of int option  (** A block, with its tag when it is immutable *)
  | Truthy  (** A value [Cond] considers as true *)
  | Falsy  (** A value [Cond] considers as false *)

(* Consistent with [Eval.the_cond_of] and [Eval.is_int] *)
let constant_value (c : constant) =
  match c with
  | Int n -> Known_int n
  | Null_ -> Falsy
  | Tuple (tag, _, _) -> Known_block (Some tag)
  | String _
  | NativeString _
  | Float _
  | Float32 _
  | Float_array _
  | Int32 _
  | Int64 _
  | NativeInt _ -> Truthy

let bool_value b = Known_int (if b then Targetint.one else Targetint.zero)

let supported_prim prim =
  match prim with
  | IsInt _ | Not | Eq _ | Neq _ | Lt | Le | Ult | Extern ("%direct_obj_tag", _) -> true
  | Extern _ | Vectlength _ | Array_get _ -> false

(* If the block only performs a test, returns the definition of the tested
   variable, when it is defined in the block *)
let block_test block =
  match block.branch with
  | Cond (x, _, _) | Switch (x, _) ->
      let rec check def l =
        match l with
        | [] -> Some def
        | Event _ :: rem -> check def rem
        | Let (v, Prim (prim, args)) :: rem
          when Option.is_none def && Var.equal v x && supported_prim prim ->
            check (Some (v, prim, args)) rem
        | (Let _ | Assign _ | Set_field _ | Offset_ref _ | Array_set _) :: _ -> None
      in
      check None block.body
  | Return _ | Raise _ | Stop | Branch _ | Pushtrap _ | Poptrap _ -> None

let eval_prim value prim args =
  let arg a =
    match a with
    | Pc c -> Some (constant_value c)
    | Pv x -> value x
  in
  match prim, args with
  | IsInt _, [ a ] -> (
      match arg a with
      | Some (Known_int _) -> Some (bool_value true)
      | Some (Known_block _) -> Some (bool_value false)
      | Some (Truthy | Falsy) | None -> None)
  | Not, [ a ] -> (
      match arg a with
      | Some (Known_int n) -> Some (bool_value (Targetint.is_zero n))
      | Some (Known_block _ | Truthy | Falsy) | None -> None)
  | (Eq _ | Neq _ | Lt | Le | Ult), [ a; b ] -> (
      match arg a, arg b with
      | Some (Known_int i), Some (Known_int j) ->
          Some
            (bool_value
               (match prim with
               | Eq _ -> Targetint.equal i j
               | Neq _ -> not (Targetint.equal i j)
               | Lt -> Targetint.(i < j)
               | Le -> Targetint.(i <= j)
               | Ult -> Targetint.unsigned_lt i j
               | _ -> assert false))
      | _ -> None)
  | Extern ("%direct_obj_tag", _), [ a ] -> (
      match arg a with
      | Some (Known_block (Some tag)) -> Some (Known_int (Targetint.of_int_exn tag))
      | Some (Known_int _ | Known_block None | Truthy | Falsy) | None -> None)
  | _ -> None

let select_cont last v =
  match last, v with
  | Cond (_, k1, k2), v -> (
      match v with
      | Known_int n -> Some (if Targetint.is_zero n then k2 else k1)
      | Known_block _ | Truthy -> Some k1
      | Falsy -> Some k2)
  | Switch (_, a), Known_int n ->
      if Targetint.(n >= zero) && Targetint.(n < of_int_exn (Array.length a))
      then Some a.(Targetint.to_int_exn n)
      else None
  | _ -> None

type definition =
  | Absent
  | Defined of value
  | Unknown

(* What we know of the value of [x] at the end of block [pc]. We look for a
   definition of [x] in this block, and then in its unique predecessor, up to a
   given depth. A variable may be modified by an [Assign] instruction in the
   same function, so we make sure there is none after the definition. *)
let rec value_at_end_of_block ~get_block ~unique_pred pc x depth =
  let block = get_block pc in
  let def =
    List.fold_left block.body ~init:Absent ~f:(fun def i ->
        match i with
        | Let (y, Constant c) when Var.equal x y -> Defined (constant_value c)
        | Let (y, Block (tag, _, _, mut)) when Var.equal x y ->
            Defined
              (Known_block
                 (match mut with
                 | Immutable -> Some tag
                 | Maybe_mutable -> None))
        | Let (y, _) when Var.equal x y -> Unknown
        | Assign (y, _) when Var.equal x y -> Unknown
        | Let _ | Assign _ | Event _ | Set_field _ | Offset_ref _ | Array_set _ -> def)
  in
  match def with
  | Defined v -> Some v
  | Unknown -> None
  | Absent ->
      if depth = 0
      then None
      else
        let pc' = unique_pred pc in
        if pc' < 0
        then None
        else value_at_end_of_block ~get_block ~unique_pred pc' x (depth - 1)

(* The continuation selected by the test of block [jblock] on an edge from
   block [pc] with arguments [args]. [fact] is what the test of [pc] tells
   about one of its variables on this edge. *)
let target ~get_block ~unique_pred jblock def pc ~fact args =
  let value_at_source x =
    match fact with
    | Some (y, v) when Var.equal x y -> Some v
    | _ -> value_at_end_of_block ~get_block ~unique_pred pc x 2
  in
  let rec value params args x =
    match params, args with
    | y :: params, z :: args ->
        if Var.equal x y then value_at_source z else value params args x
    | [], [] -> value_at_source x
    | _ -> assert false
  in
  let value x = value jblock.params args x in
  let v =
    match def, jblock.branch with
    | None, (Cond (x, _, _) | Switch (x, _)) -> value x
    | Some (_, prim, prim_args), _ -> eval_prim value prim prim_args
    | None, _ -> assert false
  in
  match v with
  | None -> None
  | Some v -> select_cont jblock.branch v

let iter_edges last ~j ~f =
  match last with
  | Branch ((pc, _) as cont) -> if pc = j then f None cont
  | Cond (c, k1, k2) ->
      if fst k1 = j then f (Some (c, Truthy)) k1;
      if fst k2 = j then f (Some (c, Falsy)) k2
  | Switch (x, a) ->
      Array.iteri a ~f:(fun i k ->
          if fst k = j then f (Some (x, Known_int (Targetint.of_int_exn i))) k)
  | Return _ | Raise _ | Stop | Pushtrap _ | Poptrap _ -> ()

let map_edges last ~j ~f =
  match last with
  | Branch ((pc, _) as cont) -> if pc = j then Branch (f None cont) else last
  | Cond (c, k1, k2) ->
      let k1 = if fst k1 = j then f (Some (c, Truthy)) k1 else k1 in
      let k2 = if fst k2 = j then f (Some (c, Falsy)) k2 else k2 in
      if cont_equal k1 k2 then Branch k1 else Cond (c, k1, k2)
  | Switch (x, a) ->
      Switch
        ( x
        , Array.mapi a ~f:(fun i k ->
              if fst k = j then f (Some (x, Known_int (Targetint.of_int_exn i))) k else k)
        )
  | Return _ | Raise _ | Stop | Pushtrap _ | Poptrap _ -> last

let map_conts last ~f =
  match last with
  | Branch cont -> Branch (f cont)
  | Cond (c, k1, k2) -> Cond (c, f k1, f k2)
  | Switch (x, a) -> Switch (x, Array.map a ~f)
  | Return _ | Raise _ | Stop | Pushtrap _ | Poptrap _ -> assert false

let iter_successors f last =
  match last with
  | Return _ | Raise _ | Stop -> ()
  | Branch (pc, _) | Poptrap (pc, _) -> f pc
  | Pushtrap ((pc, _), _, (pc', _)) ->
      f pc;
      f pc'
  | Cond (_, (pc, _), (pc', _)) ->
      f pc;
      f pc'
  | Switch (_, a) -> Array.iter a ~f:(fun (pc, _) -> f pc)

(* Record the uses in block [pc] of the variables in [watched], and call
   [closure] on the entry of each function defined in the block *)
let scan_uses ~watched ~add_use ~closure pc block =
  let use x = if BitSet.mem watched (Var.idx x) then add_use pc x in
  let rec uses l =
    match l with
    | [] -> ()
    | x :: r ->
        use x;
        uses r
  in
  List.iter block.body ~f:(fun i ->
      match i with
      | Let (_, e) -> (
          match e with
          | Apply { f; args; _ } ->
              use f;
              uses args
          | Block (_, a, _, _) -> Array.iter a ~f:use
          | Field (x, _, _) -> use x
          | Closure (_, (pc', args), _) ->
              uses args;
              closure pc'
          | Constant _ | Special _ -> ()
          | Prim (_, l) ->
              List.iter l ~f:(fun a ->
                  match a with
                  | Pv x -> use x
                  | Pc _ -> ()))
      | Assign (_, y) -> use y
      | Set_field (x, _, _, y) ->
          use x;
          use y
      | Offset_ref (x, _) -> use x
      | Array_set (x, y, z) ->
          use x;
          use y;
          use z
      | Event _ -> ());
  match block.branch with
  | Return x | Raise (x, _) -> use x
  | Stop -> ()
  | Branch (_, args) | Poptrap (_, args) -> uses args
  | Cond (x, (_, args1), (_, args2)) ->
      use x;
      uses args1;
      uses args2
  | Switch (x, a) ->
      use x;
      Array.iter a ~f:(fun (_, args) -> uses args)
  | Pushtrap ((_, args1), _, (_, args2)) ->
      uses args1;
      uses args2

type candidate =
  { pc : Addr.t
  ; block : block
  ; def : (Var.t * prim * prim_arg list) option
  ; entry : Addr.t  (** Entry of the function containing the block *)
  }

let f p =
  let t = Timer.make () in
  let previous_p = p in
  let block_array = Array.make p.free_pc { params = []; body = []; branch = Stop } in
  Addr.Map.iter (fun pc block -> block_array.(pc) <- block) p.blocks;
  let get_block pc = block_array.(pc) in
  (* We traverse each function, starting from its entry, to find:
     - the loop headers (targets of back edges);
     - the predecessor of each block, when it is unique;
     - the blocks which only perform a test, and their predecessors. *)
  let visited = BitSet.create' p.free_pc in
  let on_stack = BitSet.create' p.free_pc in
  let loop_headers = BitSet.create' p.free_pc in
  (* Entries of functions, try bodies and exception handlers *)
  let special = BitSet.create' p.free_pc in
  (* -1: no predecessor, -2: several predecessors *)
  let pred = Array.make p.free_pc (-1) in
  (* 0: unknown, 1: performs a test, 2: other *)
  let shape = Bytes.make p.free_pc '\000' in
  let shaped_preds = Addr.Hashtbl.create 128 in
  let shaped = ref [] in
  let entries = Stack.create () in
  let is_shaped entry pc =
    match Bytes.get shape pc with
    | '\001' -> true
    | '\002' -> false
    | _ -> (
        let block = get_block pc in
        match block_test block with
        | Some def ->
            Bytes.set shape pc '\001';
            shaped := { pc; block; def; entry } :: !shaped;
            true
        | None ->
            Bytes.set shape pc '\002';
            false)
  in
  let rec scan_closures l =
    match l with
    | [] -> ()
    | Let (_, Closure (_, (pc', _), _)) :: rem ->
        BitSet.set special pc';
        Stack.push pc' entries;
        scan_closures rem
    | _ :: rem -> scan_closures rem
  in
  let rec visit entry pc =
    BitSet.set visited pc;
    BitSet.set on_stack pc;
    let block = get_block pc in
    scan_closures block.body;
    (match block.branch with
    | Return _ | Raise _ | Stop -> ()
    | Branch (pc', _) | Poptrap (pc', _) -> edge entry pc pc'
    | Pushtrap ((pc1, _), _, (pc2, _)) ->
        BitSet.set special pc1;
        BitSet.set special pc2;
        edge entry pc pc1;
        edge entry pc pc2
    | Cond (_, (pc1, _), (pc2, _)) ->
        edge entry pc pc1;
        edge entry pc pc2
    | Switch (_, a) ->
        for i = 0 to Array.length a - 1 do
          edge entry pc (fst a.(i))
        done);
    BitSet.unset on_stack pc
  and edge entry pc pc' =
    let p = pred.(pc') in
    if p <> pc
    then (
      pred.(pc') <- (if p = -1 then pc else -2);
      if is_shaped entry pc'
      then
        Addr.Hashtbl.replace
          shaped_preds
          pc'
          (pc :: Option.value ~default:[] (Addr.Hashtbl.find_opt shaped_preds pc')));
    if not (BitSet.mem visited pc')
    then visit entry pc'
    else if BitSet.mem on_stack pc'
    then BitSet.set loop_headers pc'
  in
  BitSet.set special p.start;
  Stack.push p.start entries;
  while not (Stack.is_empty entries) do
    let entry = Stack.pop entries in
    if not (BitSet.mem visited entry) then visit entry entry
  done;
  let preds pc = Option.value ~default:[] (Addr.Hashtbl.find_opt shaped_preds pc) in
  let unique_pred pc = if BitSet.mem special pc then -1 else max pred.(pc) (-1) in
  (* Candidate blocks: blocks that perform a test whose outcome is known on
     at least one of their incoming edges *)
  let candidates =
    List.filter !shaped ~f:(fun { pc = j; block = jblock; def; _ } ->
        (not (BitSet.mem special j))
        && (not (BitSet.mem loop_headers j))
        && List.exists (preds j) ~f:(fun pc ->
            let found = ref false in
            iter_edges (get_block pc).branch ~j ~f:(fun fact (_, args) ->
                if
                  Option.is_some (target ~get_block ~unique_pred jblock def pc ~fact args)
                then found := true);
            !found))
  in
  (* Iterate over the blocks of the function starting at [entry]. The
     blocks of distinct functions are disjoint, so [seen] can be shared. *)
  let iter_function_blocks ~seen ~f entry =
    let rec traverse pc =
      if not (BitSet.mem seen pc)
      then (
        BitSet.set seen pc;
        let block = get_block pc in
        f pc block;
        iter_successors traverse block.branch)
    in
    traverse entry
  in
  let count = ref 0 in
  let p =
    if List.is_empty candidates
    then p
    else
      (* Uses of the variables bound in candidate blocks, in the functions
         containing these blocks and the functions they define. For each
         use, we record the block and the entry of its function. *)
      let watched = BitSet.create' (Var.count ()) in
      List.iter candidates ~f:(fun { block; def; _ } ->
          List.iter block.params ~f:(fun x -> BitSet.set watched (Var.idx x));
          Option.iter def ~f:(fun (v, _, _) -> BitSet.set watched (Var.idx v)));
      let uses = Var.Hashtbl.create 128 in
      let add_use entry pc x =
        Var.Hashtbl.replace
          uses
          x
          ((pc, entry) :: (try Var.Hashtbl.find uses x with Not_found -> []))
      in
      let scanned = BitSet.create' p.free_pc in
      let rec scan_function entry =
        if not (BitSet.mem scanned entry)
        then
          iter_function_blocks ~seen:scanned entry ~f:(fun pc block ->
              scan_uses ~watched ~add_use:(add_use entry) ~closure:scan_function pc block)
      in
      List.iter candidates ~f:(fun { entry; _ } -> scan_function entry);
      (* Dominator trees, computed lazily for each function *)
      let dominators = Addr.Hashtbl.create 16 in
      let dominators blocks entry =
        try Addr.Hashtbl.find dominators entry
        with Not_found ->
          let g = Structure.control_flow_graph blocks entry in
          let idom = Structure.immediate_dominators g in
          Addr.Hashtbl.add dominators entry (g, idom);
          g, idom
      in
      let dirty = BitSet.create' p.free_pc in
      let blocks = ref p.blocks in
      (* Blocks which have been given additional parameters *)
      let extended = BitSet.create' p.free_pc in
      (* For the variables [vars] bound in [j], find the successor of [j]
         dominating each use of these variables outside of [j], checking that
         it has [j] as only predecessor. Returns, for each such successor, the
         variables used in the blocks it dominates and these blocks. *)
      let regions j entry vars =
        let res = Addr.Hashtbl.create 4 in
        let idom = lazy (dominators !blocks entry) in
        try
          List.iter vars ~f:(fun x ->
              List.iter
                (try Var.Hashtbl.find uses x with Not_found -> [])
                ~f:(fun (pc, entry') ->
                  if pc <> j
                  then (
                    if entry' <> entry then raise Exit;
                    let s =
                      if unique_pred pc = j
                      then pc
                      else
                        let g, idom = Lazy.force idom in
                        let rec child pc =
                          let pc' = Addr.Hashtbl.find idom pc in
                          if pc' = j
                          then pc
                          else if Structure.is_forward g j pc'
                          then child pc'
                          else raise Exit
                        in
                        child pc
                    in
                    if unique_pred s <> j then raise Exit;
                    let vars, pcs =
                      try Addr.Hashtbl.find res s with Not_found -> [], Addr.Set.empty
                    in
                    let vars =
                      if List.exists vars ~f:(Var.equal x) then vars else x :: vars
                    in
                    Addr.Hashtbl.replace res s (vars, Addr.Set.add pc pcs))));
          Some res
        with Exit | Not_found -> None
      in
      (* Add parameters to [k] for the variables [vars] of [j] used in the
         blocks [pcs] it dominates *)
      let extend j k (vars, pcs) =
        if not (BitSet.mem extended k)
        then (
          BitSet.set extended k;
          let vars' = List.map ~f:Var.fork vars in
          let s = Subst.from_map (Subst.build_mapping vars vars') in
          Addr.Set.iter
            (fun pc ->
              blocks :=
                Addr.Map.add
                  pc
                  (Subst.Excluding_Binders.block s (Addr.Map.find pc !blocks))
                  !blocks)
            pcs;
          let kblock = Addr.Map.find k !blocks in
          blocks := Addr.Map.add k { kblock with params = kblock.params @ vars' } !blocks;
          let jblock = Addr.Map.find j !blocks in
          blocks :=
            Addr.Map.add
              j
              { jblock with
                branch =
                  map_conts jblock.branch ~f:(fun ((pc, args) as cont) ->
                      if pc = k then pc, args @ vars else cont)
              }
              !blocks)
      in
      (* Functions where some edges have been redirected *)
      let affected = ref [] in
      let process { pc = j; def; entry; _ } =
        if not (BitSet.mem dirty j)
        then
          let jblock = Addr.Map.find j !blocks in
          let vars =
            match def with
            | None -> jblock.params
            | Some (v, _, _) -> v :: jblock.params
          in
          match regions j entry vars with
          | None -> ()
          | Some regions ->
              let defined_in_body x =
                match def with
                | None -> false
                | Some (v, _, _) -> Var.equal x v
              in
              List.iter
                (List.sort_uniq ~cmp:Int.compare (preds j))
                ~f:(fun pc ->
                  let block = Addr.Map.find pc !blocks in
                  let changed = ref false in
                  let branch =
                    map_edges block.branch ~j ~f:(fun fact ((_, args) as cont) ->
                        match target ~get_block ~unique_pred jblock def pc ~fact args with
                        | None -> cont
                        | Some (k, kargs) ->
                            let region = Addr.Hashtbl.find_opt regions k in
                            let needed =
                              match region with
                              | None -> []
                              | Some (vars, _) -> vars
                            in
                            if
                              List.exists ~f:defined_in_body kargs
                              || List.exists ~f:defined_in_body needed
                            then cont
                            else
                              let s =
                                Subst.from_map (Subst.build_mapping jblock.params args)
                              in
                              let args = List.map ~f:s (kargs @ needed) in
                              Option.iter region ~f:(fun region -> extend j k region);
                              List.iter args ~f:(fun x ->
                                  if BitSet.mem watched (Var.idx x)
                                  then add_use entry pc x);
                              pred.(k) <- -2;
                              BitSet.set dirty k;
                              changed := true;
                              incr count;
                              k, args)
                  in
                  if !changed
                  then (
                    BitSet.set dirty pc;
                    blocks := Addr.Map.add pc { block with branch } !blocks;
                    affected := entry :: !affected));
              BitSet.set dirty j
      in
      List.iter candidates ~f:process;
      (* Remove the blocks that are no longer reachable, as well as the
         functions they define. An affected function can be defined in
         a block removed this way, so reachability is computed first. *)
      let affected = List.sort_uniq ~cmp:Int.compare !affected in
      let reachable = BitSet.create' p.free_pc in
      let rec traverse pc =
        if not (BitSet.mem reachable pc)
        then (
          BitSet.set reachable pc;
          iter_successors traverse (Addr.Map.find pc !blocks).branch)
      in
      List.iter affected ~f:traverse;
      let seen = BitSet.create' p.free_pc in
      let removed = BitSet.create' p.free_pc in
      let rec remove_block pc block =
        blocks := Addr.Map.remove pc !blocks;
        List.iter block.body ~f:(fun i ->
            match i with
            | Let (_, Closure (_, (pc', _), _)) ->
                iter_function_blocks ~seen:removed ~f:remove_block pc'
            | _ -> ())
      in
      List.iter affected ~f:(fun entry ->
          iter_function_blocks ~seen entry ~f:(fun pc block ->
              if not (BitSet.mem reachable pc) then remove_block pc block));
      { p with blocks = !blocks }
  in
  if times () then Format.eprintf "  jump threading: %a@." Timer.print t;
  if stats () then Format.eprintf "Stats - jump threading: %d edges threaded@." !count;
  if debug_stats ()
  then Code.check_updates ~name:"jump threading" previous_p p ~updates:!count;
  Code.invariant p;
  p
