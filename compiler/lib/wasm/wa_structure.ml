open Stdlib
open Code

type graph = (Addr.t, Addr.Set.t) Hashtbl.t

let get_edges g src = try Hashtbl.find g src with Not_found -> Addr.Set.empty

let add_edge g src dst = Hashtbl.replace g src (Addr.Set.add dst (get_edges g src))

let reverse_graph g =
  let g' = Hashtbl.create 16 in
  Hashtbl.iter
    (fun child parents -> Addr.Set.iter (fun parent -> add_edge g' parent child) parents)
    g;
  g'

let reverse_tree t =
  let g = Hashtbl.create 16 in
  Hashtbl.iter (fun child parent -> add_edge g parent child) t;
  g

type control_flow_graph =
  { succs : (Addr.t, Addr.Set.t) Hashtbl.t
  ; preds : (Addr.t, Addr.Set.t) Hashtbl.t
  ; reverse_post_order : Addr.t list
  ; block_order : (Addr.t, int) Hashtbl.t
  }

let is_backward g pc pc' = Hashtbl.find g.block_order pc >= Hashtbl.find g.block_order pc'

let is_forward g pc pc' = Hashtbl.find g.block_order pc < Hashtbl.find g.block_order pc'

(* pc has at least two forward edges moving into it *)
let is_merge_node' block_order preds pc =
  let s = try Hashtbl.find preds pc with Not_found -> Addr.Set.empty in
  let o = Hashtbl.find block_order pc in
  let n =
    Addr.Set.fold (fun pc' n -> if Hashtbl.find block_order pc' < o then n + 1 else n) s 0
  in
  n > 1

let rec leave_try_body block_order preds blocks pc =
  if is_merge_node' block_order preds pc
  then false
  else
    match Addr.Map.find pc blocks with
    | { body = []; branch = (Return _ | Stop), _; _ } -> false
    | { body = []; branch = Branch (pc', _), _; _ } ->
        leave_try_body block_order preds blocks pc'
    | _ -> true

let build_graph blocks pc =
  let succs = Hashtbl.create 16 in
  let l = ref [] in
  let visited = Hashtbl.create 16 in
  let poptraps = ref [] in
  let rec traverse ~englobing_exn_handlers pc =
    if not (Hashtbl.mem visited pc)
    then (
      Hashtbl.add visited pc ();
      let successors = Code.fold_children blocks pc Addr.Set.add Addr.Set.empty in
      Hashtbl.add succs pc successors;
      let block = Addr.Map.find pc blocks in
      Addr.Set.iter
        (fun pc' ->
          let englobing_exn_handlers =
            match fst block.branch with
            | Pushtrap ((body_pc, _), _, _, _) when pc' = body_pc ->
                pc :: englobing_exn_handlers
            | Poptrap (leave_pc, _) -> (
                match englobing_exn_handlers with
                | [] -> assert false
                | enter_pc :: rem ->
                    poptraps := (enter_pc, leave_pc) :: !poptraps;
                    rem)
            | _ -> englobing_exn_handlers
          in
          traverse ~englobing_exn_handlers pc')
        successors;
      l := pc :: !l)
  in
  traverse ~englobing_exn_handlers:[] pc;
  let block_order = Hashtbl.create 16 in
  List.iteri !l ~f:(fun i pc -> Hashtbl.add block_order pc i);
  let preds = reverse_graph succs in
  List.iter !poptraps ~f:(fun (enter_pc, leave_pc) ->
      if leave_try_body block_order preds blocks leave_pc
      then (
        (* Add an edge to limit the [try] body *)
        Hashtbl.replace
          succs
          enter_pc
          (Addr.Set.add leave_pc (Hashtbl.find succs enter_pc));
        Hashtbl.replace
          preds
          leave_pc
          (Addr.Set.add enter_pc (Hashtbl.find preds leave_pc))));
  { succs; preds; reverse_post_order = !l; block_order }

let reversed_dominator_tree g =
  (* A Simple, Fast Dominance Algorithm
     Keith D. Cooper, Timothy J. Harvey, and Ken Kennedy *)
  let dom = Hashtbl.create 16 in
  let rec inter pc pc' =
    (* Compute closest common ancestor *)
    if pc = pc'
    then pc
    else if is_forward g pc pc'
    then inter pc (Hashtbl.find dom pc')
    else inter (Hashtbl.find dom pc) pc'
  in
  List.iter g.reverse_post_order ~f:(fun pc ->
      let l = Hashtbl.find g.succs pc in
      Addr.Set.iter
        (fun pc' ->
          if is_forward g pc pc'
          then
            let d = try inter pc (Hashtbl.find dom pc') with Not_found -> pc in
            Hashtbl.replace dom pc' d)
        l);
  (* Check we have reached a fixed point (reducible graph) *)
  List.iter g.reverse_post_order ~f:(fun pc ->
      let l = Hashtbl.find g.succs pc in
      Addr.Set.iter
        (fun pc' ->
          if is_forward g pc pc'
          then
            let d = Hashtbl.find dom pc' in
            assert (inter pc d = d))
        l);
  dom

let dominator_tree g = reverse_tree (reversed_dominator_tree g)

(* pc has at least two forward edges moving into it *)
let is_merge_node g pc = is_merge_node' g.block_order g.preds pc

let is_loop_header g pc =
  let s = try Hashtbl.find g.preds pc with Not_found -> Addr.Set.empty in
  let o = Hashtbl.find g.block_order pc in
  Addr.Set.exists (fun pc' -> Hashtbl.find g.block_order pc' >= o) s

let sort_in_post_order g l =
  List.sort
    ~cmp:(fun b b' ->
      compare (Hashtbl.find g.block_order b') (Hashtbl.find g.block_order b))
    l

(* Compute a map from each block to the set of loops it belongs to *)
let mark_loops g =
  let in_loop = Hashtbl.create 16 in
  Hashtbl.iter
    (fun pc preds ->
      let rec mark_loop pc' =
        if not (Addr.Set.mem pc (get_edges in_loop pc'))
        then (
          add_edge in_loop pc' pc;
          if pc' <> pc then Addr.Set.iter mark_loop (Hashtbl.find g.preds pc'))
      in
      Addr.Set.iter (fun pc' -> if is_backward g pc' pc then mark_loop pc') preds)
    g.preds;
  in_loop

let rec measure blocks g pc limit =
  let b = Addr.Map.find pc blocks in
  let limit = limit - List.length b.body in
  if limit < 0
  then limit
  else
    Addr.Set.fold
      (fun pc limit -> if limit < 0 then limit else measure blocks g pc limit)
      (get_edges g.succs pc)
      limit

let is_small blocks g pc = measure blocks g pc 20 >= 0

(* V8 uses the distance between the position of a backward jump and
   the loop header as an estimation of the cost of executing the loop,
   to decide whether to optimize a function containing a loop. So, for
   a large function when the loop includes all the remaining code, the
   estimation can be widely off. In particular, it may decide to
   optimize the toplevel code, which is especially costly since it is
   very large, and uncessary since it is executed only once. *)
let shrink_loops blocks ({ succs; preds; reverse_post_order; _ } as g) =
  let add_edge pred succ =
    Hashtbl.replace succs pred (Addr.Set.add succ (Hashtbl.find succs pred));
    Hashtbl.replace preds succ (Addr.Set.add pred (Hashtbl.find preds succ))
  in
  let in_loop = mark_loops g in
  let dom = dominator_tree g in
  let root = List.hd reverse_post_order in
  let rec traverse ignored pc =
    let succs = get_edges dom pc in
    let loops = get_edges in_loop pc in
    let block = Addr.Map.find pc blocks in
    Addr.Set.iter
      (fun pc' ->
        (* Whatever is in the scope of an exception handler should not be
           moved outside *)
        let ignored =
          match fst block.branch with
          | Pushtrap ((body_pc, _), _, _, _) when pc' = body_pc ->
              Addr.Set.union ignored loops
          | _ -> ignored
        in
        let loops' = get_edges in_loop pc' in
        let left_loops = Addr.Set.diff (Addr.Set.diff loops loops') ignored in
        (* If we leave a loop, we add an edge from a predecessor of
           the loop header to the current block, so that it is
           considered outside of the loop. *)
        if not (Addr.Set.is_empty left_loops || is_small blocks g pc')
        then
          Addr.Set.iter
            (fun pc0 ->
              match
                Addr.Set.find_first
                  (fun pc -> is_forward g pc pc0)
                  (get_edges g.preds pc0)
              with
              | pc -> add_edge pc pc'
              | exception Not_found -> ())
            left_loops;
        traverse ignored pc')
      succs
  in
  traverse Addr.Set.empty root

let build_graph blocks pc =
  let g = build_graph blocks pc in
  shrink_loops blocks g;
  g
