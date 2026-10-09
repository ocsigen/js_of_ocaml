(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2026
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
module Var = Code.Var

type node = int

type action =
  | Use of Var.Set.t
  | Def of Var.Set.t
  | DefUse of Var.Set.t * Var.Set.t
  | Nop

let defs_of_action = function
  | Def d | DefUse (d, _) -> d
  | Use _ | Nop -> Var.Set.empty

let uses_of_action = function
  | Use u | DefUse (_, u) -> u
  | Def _ | Nop -> Var.Set.empty

type graph =
  { entry : node
  ; size : int
  ; actions : action array
  ; succs : node list array
  ; hints : Var.t Var.Hashtbl.t
  ; try_blocks : node Int.Hashtbl.t
  }

module Builder = struct
  type t =
    { mutable nodes : (node * action * node list) list
    ; mutable next_id : int
    ; hints : Var.t Var.Hashtbl.t
    ; tries : node Int.Hashtbl.t
    }

  let create () =
    { nodes = []
    ; next_id = 0
    ; hints = Var.Hashtbl.create 16
    ; tries = Int.Hashtbl.create 8
    }

  let reserve b =
    let id = b.next_id in
    b.next_id <- id + 1;
    id

  let set b id action succs = b.nodes <- (id, action, succs) :: b.nodes

  let add b action succs =
    let id = reserve b in
    set b id action succs;
    id

  let hint b x y = Var.Hashtbl.replace b.hints x y

  let try_block b ~catch_entry ~body_entry =
    Int.Hashtbl.replace b.tries catch_entry body_entry

  let finish b ~entry =
    let size = b.next_id in
    let actions = Array.make size Nop in
    let succs = Array.make size [] in
    List.iter b.nodes ~f:(fun (id, action, s) ->
        actions.(id) <- action;
        succs.(id) <- s);
    { entry; size; actions; succs; hints = b.hints; try_blocks = b.tries }
end

module Node = struct
  type t = int
end

module NodeSet = struct
  type t = BitSet.t

  type elt = int

  let iter f t = BitSet.iter ~f t

  let mem = BitSet.mem

  let add = BitSet.set

  let remove = BitSet.unset

  let copy = BitSet.copy
end

module NodeTbl = struct
  type 'a t = 'a array

  type key = int

  type size = int

  let get t k = t.(k)

  let set t k v = t.(k) <- v

  let make = Array.make
end

module G = Dgraph.Make_Imperative (Node) (NodeSet) (NodeTbl)

module Domain = struct
  type t = Var.Set.t

  let equal = Var.Set.equal

  let bot = Var.Set.empty
end

module Solver = G.Solver (Domain)

(* Backward dataflow:
     LiveIn(n) = Use(n) ∪ (LiveOut(n) - Def(n))
     LiveOut(n) = ∪ LiveIn(succ) for all successors *)
let liveness g =
  let domain_set = BitSet.create' g.size in
  for i = 0 to g.size - 1 do
    BitSet.set domain_set i
  done;
  let preds = Array.make (Array.length g.succs) [] in
  Array.iteri g.succs ~f:(fun i l ->
      List.iter l ~f:(fun j -> preds.(j) <- i :: preds.(j)));
  let inv_graph =
    { G.domain = domain_set; iter_children = (fun f i -> List.iter ~f preds.(i)) }
  in
  let transfer state node_id =
    let live_out =
      List.fold_left g.succs.(node_id) ~init:Var.Set.empty ~f:(fun acc id ->
          Var.Set.union acc state.(id))
    in
    let def = defs_of_action g.actions.(node_id) in
    let use = uses_of_action g.actions.(node_id) in
    Var.Set.union use (Var.Set.diff live_out def)
  in
  Solver.f g.size inv_graph transfer

(* Live Range representation: sorted list of disjoint (start, end) intervals.
   We use 2x granularity for positions: each CFG node n at linear position i
   maps to positions 2*i (before the node, for uses) and 2*i+1 (after the node,
   for defs). This allows distinguishing between a variable that is live-in
   before a definition versus live-out after it at the same CFG node. *)
module Live_range = struct
  type interval =
    { start_pos : int
    ; end_pos : int
    }

  type t =
    { id : Var.t (* The variable associated with this range *)
    ; mutable ranges : interval list (* sorted by start_pos *)
    ; mutable free : bool (* dead and available for reuse: in the free pool *)
    }

  let create v = { id = v; ranges = []; free = false }

  let print_ranges f r =
    Format.fprintf
      f
      "@[%a@]"
      (Format.pp_print_list
         ~pp_sep:(fun f () -> Format.fprintf f ",@,")
         (fun f r -> Format.fprintf f "%d-%d" r.start_pos r.end_pos))
      r

  let print f t = print_ranges f t.ranges

  let add_range t start_pos end_pos =
    let rec loop s e acc = function
      | [] -> List.rev ({ start_pos = s; end_pos = e } :: acc)
      | r :: rest ->
          if e < r.start_pos - 1
          then List.rev_append acc ({ start_pos = s; end_pos = e } :: r :: rest)
          else if s > r.end_pos + 1
          then loop s e (r :: acc) rest
          else
            (* Overlap or adjacent, merge *)
            let new_start = min s r.start_pos in
            let new_end = max e r.end_pos in
            loop new_start new_end acc rest
    in
    t.ranges <-
      (match t.ranges with
      | [] -> [ { start_pos; end_pos } ]
      | r :: _ when r.start_pos > end_pos + 1 -> { start_pos; end_pos } :: t.ranges
      | _ -> loop start_pos end_pos [] t.ranges)

  let add_ranges t other_ranges =
    match t.ranges, other_ranges with
    | [], l | l, [] -> t.ranges <- l
    | l1, l2 ->
        let rec loop acc l1 l2 =
          match l1, l2 with
          | [], l | l, [] -> List.rev_append acc l
          | h1 :: t1, h2 :: t2 ->
              if h1.start_pos < h2.start_pos then step acc h1 t1 l2 else step acc h2 t2 l1
        and step acc current rest other =
          match acc with
          | prev :: acc_rest when prev.end_pos + 1 >= current.start_pos ->
              let merged = { prev with end_pos = max prev.end_pos current.end_pos } in
              loop (merged :: acc_rest) rest other
          | _ -> loop (current :: acc) rest other
        in
        t.ranges <- loop [] l1 l2

  let get_start_pos t =
    match t.ranges with
    | [] -> max_int
    | r :: _ -> r.start_pos

  let get_first_hole t =
    match t.ranges with
    | [] -> 0
    | r :: _ -> r.end_pos + 1

  (* This function consumes ranges before the current position *)
  let rec advance t position =
    match t.ranges with
    | [] -> `Dead
    | r :: rem ->
        if r.end_pos < position
        then (
          t.ranges <- rem;
          advance t position)
        else if r.start_pos > position
        then `Inactive
        else `Active

  let intersects t1 t2 =
    let rec loop l1 l2 =
      match l1, l2 with
      | [], _ | _, [] -> false
      | r1 :: rest1, r2 :: rest2 ->
          if r1.end_pos < r2.start_pos
          then loop rest1 l2
          else if r2.end_pos < r1.start_pos
          then loop l1 rest2
          else true
    in
    loop t1.ranges t2.ranges
end

let live_ranges g live_in_map ~candidates ~param_vars =
  (* Linearize the CFG (Reverse Post Order) *)
  let visited = Array.make g.size false in
  let layout = Array.make g.size 0 in
  let i = ref g.size in
  let rec list_rev_iter ~f l =
    match l with
    | [] -> ()
    | x :: r ->
        list_rev_iter ~f r;
        f x
  in
  let rec dfs n =
    if not visited.(n)
    then (
      visited.(n) <- true;
      let succs = g.succs.(n) in
      (* Natural order (important for exception handlers):
         if->else, try->catch->finally *)
      list_rev_iter succs ~f:dfs;
      decr i;
      layout.(!i) <- n)
  in
  dfs g.entry;
  let num_reachable = g.size - !i in
  let layout =
    (* Some nodes may be unreachable *)
    if !i = 0 then layout else Array.sub layout ~pos:!i ~len:num_reachable
  in

  (* Map Node -> Linear Index *)
  let node_order = Array.make g.size (-1) in
  Array.iteri ~f:(fun i n -> node_order.(n) <- i) layout;

  let ranges = Var.Hashtbl.create (Var.Set.cardinal candidates) in
  Var.Set.iter (fun v -> Var.Hashtbl.add ranges v (Live_range.create v)) candidates;

  (* Active ranges map: Var -> end_pos of current active range *)
  let active_ranges = Var.Hashtbl.create 64 in

  let commit_range v start_pos end_pos =
    let r = Var.Hashtbl.find ranges v in
    Live_range.add_range r start_pos end_pos
  in

  for order = num_reachable - 1 downto 0 do
    let node_id = layout.(order) in
    let start_idx = 2 * order in
    let end_idx = (2 * order) + 1 in
    let succs = g.succs.(node_id) in
    (* 2x granularity:
       - 2*order: state before the node (uses)
       - 2*order+1: state after the node (defs) *)
    (* Optimization: if the only successor is the next node in linear order
       (fallthrough), we can skip recomputing live_out since it equals the
       live_in of the next iteration. This avoids closing and reopening ranges
       for the common case of sequential statements. *)
    let is_fallthrough =
      match succs with
      | [ s ] -> order < num_reachable - 1 && layout.(order + 1) = s
      | _ -> false
    in
    if not is_fallthrough
    then (
      (* Calculate live_out from successors *)
      let live_out =
        List.fold_left succs ~init:Var.Set.empty ~f:(fun acc sid ->
            Var.Set.union acc live_in_map.(sid))
      in
      (* Process variables that are currently active but not in live_out. *)
      let to_remove = ref [] in
      Var.Hashtbl.iter
        (fun v high ->
          if not (Var.Set.mem v live_out)
          then (
            (* Range ends after this node *)
            commit_range v (end_idx + 1) high;
            to_remove := v :: !to_remove))
        active_ranges;
      List.iter ~f:(Var.Hashtbl.remove active_ranges) !to_remove;
      (* Process variables in live_out but are not active. *)
      Var.Set.iter
        (fun v ->
          if not (Var.Hashtbl.mem active_ranges v)
          then Var.Hashtbl.add active_ranges v end_idx)
        live_out);
    (* 2. Process Defs *)
    let defs = defs_of_action g.actions.(node_id) in
    Var.Set.iter
      (fun v ->
        match Var.Hashtbl.find_opt active_ranges v with
        | Some high ->
            commit_range v end_idx high;
            Var.Hashtbl.remove active_ranges v
        | None ->
            (* Defined but not live out: dead assignment. *)
            commit_range v end_idx end_idx)
      defs;
    (* 3. Process Uses *)
    let uses = uses_of_action g.actions.(node_id) in
    Var.Set.iter
      (fun v ->
        if not (Var.Hashtbl.mem active_ranges v)
        then
          (* Becomes live at use. *)
          Var.Hashtbl.add active_ranges v start_idx)
      uses;
    (* Try-catch/finally liveness extension: when we reach a catch or finally
       handler entry, any variable that is live at this point must have been
       live throughout the entire try body. This is because any statement in
       the try block might throw an exception and jump directly to the handler,
       so the variable's value at any point in the try body might be observed.
       We extend all active live ranges back to the start of the try body. *)
    match Int.Hashtbl.find_opt g.try_blocks node_id with
    | None -> ()
    | Some start_id ->
        let start_order = node_order.(start_id) in
        assert (start_order < order);
        let start_idx = 2 * start_order in
        Var.Hashtbl.iter (fun v high -> commit_range v start_idx high) active_ranges;
        Var.Hashtbl.clear active_ranges
  done;
  (* Close all remaining active ranges at 0 *)
  Var.Hashtbl.iter (fun v high -> commit_range v 0 high) active_ranges;
  (* Mark parameters as live at very start
     (they must have distinct names). *)
  Var.Set.iter (fun v -> commit_range v 0 0) param_vars;
  ranges

module Active_pqueue = Pqueue.Make (struct
  type t = Live_range.t

  let compare r r' = compare (Live_range.get_first_hole r) (Live_range.get_first_hole r')
end)

module Inactive_pqueue = Pqueue.Make (struct
  type t = int * Var.t

  let compare (p, _) (p', _) = compare (p : int) p'
end)

(* Linear scan register allocation. Assigns each variable to a representative
   variable (possibly itself) such that variables with the same representative
   have disjoint live ranges.

   The algorithm processes variables in order of their start position, maintaining:
   - active: variables currently live (sorted by next hole position)
   - inactive: variables in a "hole" in their live range (temporarily not live)
   - free_pool: variables whose live ranges have ended completely

   For each new variable, we try to find a representative in this order:
   1. Hint-based: if this variable is the target of a copy (x = y), try to reuse
      y's representative to enable copy propagation
   2. Inactive: reuse a variable currently in a hole if ranges don't interfere
   3. Free pool: reuse a completely dead variable
   4. Self: use the variable itself (no coalescing)

   To avoid quadratic behavior in pathological cases, the search for
   non-interfering inactive variables is limited to a constant number of
   candidates. *)
let allocate ?(compatible = fun _ _ -> true) g ranges ~assign =
  let hint_count = ref 0 in
  let opportunistic_count = ref 0 in
  (* Sort by Start Position *)
  let sorted_intervals =
    let intervals = Var.Hashtbl.fold (fun _ r acc -> r :: acc) ranges [] in
    List.sort
      ~cmp:(fun a b ->
        Int.compare (Live_range.get_start_pos a) (Live_range.get_start_pos b))
      intervals
  in
  (* List of variables that overlap with the current location *)
  let active = ref Active_pqueue.empty in
  (* List of variables that have started but are currently in a hole *)
  let inactive_queue = ref Inactive_pqueue.empty in
  let inactive = Var.Hashtbl.create 128 in
  (* List of variables which are no longer live. A range taken from the
     pool through a copy hint stays in the list, but no longer has its
     [free] flag set: such entries are skipped and discarded. The range is
     pushed again when it dies again. *)
  let free_pool = ref [] in
  let release r =
    r.Live_range.free <- true;
    free_pool := r :: !free_pool
  in
  (* Representative of every variable allocated so far *)
  let repr_of = Var.Hashtbl.create 128 in
  let rec update_active_queue position =
    match Active_pqueue.find_min !active with
    | exception Not_found -> ()
    | r -> (
        if Live_range.get_first_hole r <= position
        then
          let active' = Active_pqueue.remove_min !active in
          match Live_range.advance r position with
          | `Dead ->
              release r;
              active := active';
              update_active_queue position
          | `Inactive ->
              inactive_queue :=
                Inactive_pqueue.add (Live_range.get_start_pos r, r.id) !inactive_queue;
              Var.Hashtbl.replace inactive r.id r;
              active := active';
              update_active_queue position
          | `Active ->
              active := Active_pqueue.add r active';
              update_active_queue position)
  in
  let rec update_inactive_queue position =
    match Inactive_pqueue.find_min !inactive_queue with
    | exception Not_found -> ()
    | p, v -> (
        if p <= position
        then
          let inactive' = Inactive_pqueue.remove_min !inactive_queue in
          match Var.Hashtbl.find_opt inactive v with
          | None ->
              (* Was actually already removed from the queue *)
              inactive_queue := inactive';
              update_inactive_queue position
          | Some r -> (
              match Live_range.advance r position with
              | `Dead ->
                  release r;
                  inactive_queue := inactive';
                  Var.Hashtbl.remove inactive v;
                  update_inactive_queue position
              | `Inactive ->
                  inactive_queue :=
                    Inactive_pqueue.add (Live_range.get_start_pos r, r.id) inactive';
                  update_inactive_queue position
              | `Active ->
                  active := Active_pqueue.add r !active;
                  inactive_queue := inactive';
                  Var.Hashtbl.remove inactive v;
                  update_inactive_queue position))
  in
  let get_free wanted =
    let rec loop kept pool =
      match pool with
      | [] ->
          free_pool := List.rev kept;
          None
      | r :: rs ->
          if not r.Live_range.free
          then loop kept rs
          else if compatible r.Live_range.id wanted
          then (
            r.free <- false;
            free_pool := List.rev_append kept rs;
            Some r)
          else loop (r :: kept) rs
    in
    loop [] !free_pool
  in
  List.iter sorted_intervals ~f:(fun current ->
      let position = Live_range.get_start_pos current in
      (* Update queues *)
      update_active_queue position;
      update_inactive_queue position;
      (* Try hint-based coalescing first. If current variable is the target of
         a copy (current = src), we try to reuse src's representative. This is
         beneficial because if they share the same variable, the copy becomes
         a no-op. We can only do this if the representative is dead, inactive
         with non-interfering ranges, or not yet started. *)
      let hint_repr =
        match Var.Hashtbl.find_opt g.hints current.Live_range.id with
        | None -> None
        | Some src -> (
            match Var.Hashtbl.find_opt repr_of src with
            | None -> None
            | Some var -> (
                if not (compatible var current.id)
                then None
                else
                  let r = Var.Hashtbl.find ranges var in
                  match Live_range.advance r position with
                  | `Dead ->
                      (* Source is completely dead, safe to reuse *)
                      r.free <- false;
                      Some r
                  | `Inactive ->
                      (* Source is in a hole; check if remaining ranges
                         interfere *)
                      if Live_range.intersects r current
                      then None
                      else (
                        Var.Hashtbl.remove inactive r.id;
                        Some r)
                  | `Active ->
                      (* Source is still live, cannot coalesce *)
                      None))
      in
      let repr =
        match hint_repr with
        | Some r ->
            incr hint_count;
            r
        | None -> (
            (* Try to find a valid reuse from inactive.
               We prioritize the inactive list over the free list to implement a
               "Best Fit" strategy. Reusing a variable from the inactive list
               fills a specific "hole" in a lifetime, which is a more constrained
               resource. By using it first, we save the "universally compatible"
               free variables for later intervals that might not fit into
               available holes.

               We iterate using the priority queue [inactive_queue] to check
               variables with the smallest holes first, further optimizing the
               fit. *)
            let candidate =
              let rec loop q count =
                if count >= 50 || Inactive_pqueue.is_empty q
                then None
                else
                  let _, v = Inactive_pqueue.find_min q in
                  let q' = Inactive_pqueue.remove_min q in
                  match Var.Hashtbl.find_opt inactive v with
                  | None -> loop q' count (* Stale, don't count *)
                  | Some iv ->
                      if
                        compatible iv.id current.id
                        && not (Live_range.intersects iv current)
                      then Some iv
                      else loop q' (count + 1)
              in
              loop !inactive_queue 0
            in
            match candidate with
            | Some r ->
                incr opportunistic_count;
                Var.Hashtbl.remove inactive r.id;
                r
            | None -> (
                match get_free current.id with
                | Some r ->
                    incr opportunistic_count;
                    r
                | None -> current))
      in
      if not (Var.equal current.id repr.id)
      then (
        (* Remove the names since they can be confusing. *)
        Var.forget_generated_name current.id;
        Var.forget_generated_name repr.id;
        (* Merge current into repr *)
        Live_range.add_ranges repr current.Live_range.ranges);
      Var.Hashtbl.replace repr_of current.id repr.id;
      assign current.id repr.id;
      active := Active_pqueue.add repr !active);
  !hint_count, !opportunistic_count
