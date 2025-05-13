open Stdlib
open Code

let get_edges g src = try Addr.Hashtbl.find g src with Not_found -> Addr.Set.empty

let add_edge g src dst = Addr.Hashtbl.replace g src (Addr.Set.add dst (get_edges g src))

let reverse_tree t =
  let g = Addr.Hashtbl.create 16 in
  Addr.Hashtbl.iter (fun child parent -> add_edge g parent child) t;
  g

let reverse_graph g =
  let g' = Addr.Hashtbl.create 16 in
  Addr.Hashtbl.iter
    (fun child parents -> Addr.Set.iter (fun parent -> add_edge g' parent child) parents)
    g;
  g'

type graph = Addr.Set.t Addr.Hashtbl.t

type t =
  { succs : Addr.Set.t Addr.Hashtbl.t
  ; preds : Addr.Set.t Addr.Hashtbl.t
  ; reverse_post_order : Addr.t list
  ; block_order : int Addr.Hashtbl.t
  }

let get_nodes g =
  List.fold_left
    ~init:Addr.Set.empty
    ~f:(fun s pc -> Addr.Set.add pc s)
    g.reverse_post_order

let block_order g pc = Addr.Hashtbl.find g.block_order pc

let is_backward g pc pc' =
  Addr.Hashtbl.find g.block_order pc >= Addr.Hashtbl.find g.block_order pc'

let is_forward g pc pc' =
  Addr.Hashtbl.find g.block_order pc < Addr.Hashtbl.find g.block_order pc'

(* pc has at least two forward edges moving into it *)
let is_merge_node' block_order preds pc =
  let s = try Addr.Hashtbl.find preds pc with Not_found -> Addr.Set.empty in
  let o = Addr.Hashtbl.find block_order pc in
  try
    ignore
      (Addr.Set.fold
         (fun pc' found_first ->
           if Addr.Hashtbl.find block_order pc' < o
           then
             if found_first
             then (* Exit early to avoid quadratic behavior *) raise Exit
             else true
           else found_first)
         s
         false);
    false
  with Exit -> true

let empty_body body =
  List.for_all
    ~f:(fun i ->
      match i with
      | Event _ -> true
      | _ -> false)
    body

let rec leave_try_body block_order preds blocks pc =
  if is_merge_node' block_order preds pc
  then false
  else
    match Addr.Map.find pc blocks with
    | { body; branch = Return _ | Stop; _ } when empty_body body -> false
    | { body; branch = Branch (pc', _); _ } when empty_body body ->
        leave_try_body block_order preds blocks pc'
    | _ -> true

let build_graph blocks pc =
  let succs = Addr.Hashtbl.create 16 in
  let l = ref [] in
  let visited = Addr.Hashtbl.create 16 in
  let poptraps = ref [] in
  let rec traverse ~englobing_exn_handlers pc =
    if not (Addr.Hashtbl.mem visited pc)
    then (
      Addr.Hashtbl.add visited pc ();
      let successors = Code.fold_children blocks pc Addr.Set.add Addr.Set.empty in
      Addr.Hashtbl.add succs pc successors;
      let block = Addr.Map.find pc blocks in
      Addr.Set.iter
        (fun pc' ->
          let englobing_exn_handlers =
            match block.branch with
            | Pushtrap ((body_pc, _), _, _) when pc' = body_pc ->
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
  let block_order = Addr.Hashtbl.create 16 in
  List.iteri !l ~f:(fun i pc -> Addr.Hashtbl.add block_order pc i);
  let preds = reverse_graph succs in
  List.iter !poptraps ~f:(fun (enter_pc, leave_pc) ->
      if leave_try_body block_order preds blocks leave_pc
      then (
        (* Add an edge to limit the [try] body *)
        Addr.Hashtbl.replace
          succs
          enter_pc
          (Addr.Set.add leave_pc (Addr.Hashtbl.find succs enter_pc));
        Addr.Hashtbl.replace
          preds
          leave_pc
          (Addr.Set.add enter_pc (Addr.Hashtbl.find preds leave_pc))));
  { succs; preds; reverse_post_order = !l; block_order }

let dominator_tree g =
  (* A Simple, Fast Dominance Algorithm
     Keith D. Cooper, Timothy J. Harvey, and Ken Kennedy *)
  let dom = Addr.Hashtbl.create 16 in
  let rec inter pc pc' =
    (* Compute closest common ancestor *)
    if pc = pc'
    then pc
    else if is_forward g pc pc'
    then inter pc (Addr.Hashtbl.find dom pc')
    else inter (Addr.Hashtbl.find dom pc) pc'
  in
  List.iter g.reverse_post_order ~f:(fun pc ->
      let l = Addr.Hashtbl.find g.succs pc in
      Addr.Set.iter
        (fun pc' ->
          if is_forward g pc pc'
          then
            let d = try inter pc (Addr.Hashtbl.find dom pc') with Not_found -> pc in
            Addr.Hashtbl.replace dom pc' d)
        l);
  (* Check we have reached a fixed point (reducible graph) *)
  List.iter g.reverse_post_order ~f:(fun pc ->
      let l = Addr.Hashtbl.find g.succs pc in
      Addr.Set.iter
        (fun pc' ->
          if is_forward g pc pc'
          then
            let d = Addr.Hashtbl.find dom pc' in
            assert (inter pc d = d))
        l);
  reverse_tree dom

(* pc has at least two forward edges moving into it *)
let is_merge_node g pc = is_merge_node' g.block_order g.preds pc

let is_loop_header g pc =
  let s = try Addr.Hashtbl.find g.preds pc with Not_found -> Addr.Set.empty in
  let o = Addr.Hashtbl.find g.block_order pc in
  Addr.Set.exists (fun pc' -> Addr.Hashtbl.find g.block_order pc' >= o) s

let sort_in_post_order t l =
  List.sort ~cmp:(fun a b -> compare (block_order t b) (block_order t a)) l

let blocks_in_reverse_post_order g = g.reverse_post_order

(*

(* pc dominates pc' *)
let rec dominates g idom pc pc' =
  pc = pc' || (is_forward g pc pc' && dominates g idom pc (Addr.Hashtbl.find idom pc'))

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
*)

(* Compute a map from each block to the set of loops it belongs to *)
let mark_loops g =
  let in_loop = Addr.Hashtbl.create 16 in
  Addr.Hashtbl.iter
    (fun pc preds ->
      let rec mark_loop pc' =
        if not (Addr.Set.mem pc (get_edges in_loop pc'))
        then (
          add_edge in_loop pc' pc;
          if pc' <> pc then Addr.Set.iter mark_loop (Addr.Hashtbl.find g.preds pc'))
      in
      Addr.Set.iter (fun pc' -> if is_backward g pc' pc then mark_loop pc') preds)
    g.preds;
  in_loop

let rec measure blocks g pc limit =
  if is_loop_header g pc
  then -1
  else
    let b = Addr.Map.find pc blocks in
    let limit =
      List.fold_left b.body ~init:limit ~f:(fun acc x ->
          match x with
          (* A closure is never small *)
          | Let (_, Closure _) -> -1
          | Event _ -> acc
          | _ -> acc - 1)
    in
    if limit < 0
    then limit
    else
      Addr.Set.fold
        (fun pc limit -> if limit < 0 then limit else measure blocks g pc limit)
        (get_edges g.succs pc)
        limit

let is_small blocks g pc = measure blocks g pc 20 >= 0

let shrink_loops blocks ({ succs; preds; reverse_post_order; _ } as g) =
  let add_edge pred succ =
    Addr.Hashtbl.replace succs pred (Addr.Set.add succ (Addr.Hashtbl.find succs pred));
    Addr.Hashtbl.replace preds succ (Addr.Set.add pred (Addr.Hashtbl.find preds succ))
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
          match block.branch with
          | Pushtrap ((body_pc, _), _, _) when pc' = body_pc ->
              Addr.Set.union ignored loops
          | _ -> ignored
        in
        let loops' = get_edges in_loop pc' in
        let left_loops = Addr.Set.diff (Addr.Set.diff loops loops') ignored in
        (* If we leave a loop, we add an edge from predecessors of
           the loop header to the current block, so that it is
           considered outside of the loop. *)
        if not (Addr.Set.is_empty left_loops || is_small blocks g pc')
        then
          Addr.Set.iter
            (fun pc0 ->
              Addr.Set.iter
                (fun pc -> if is_forward g pc pc0 then add_edge pc pc')
                (get_edges g.preds pc0))
            left_loops;
        traverse ignored pc')
      succs
  in
  traverse Addr.Set.empty root

let build_graph blocks pc =
  let g = build_graph blocks pc in
  shrink_loops blocks g;
  g
