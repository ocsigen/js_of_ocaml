open Util

type state =
  { all_succs : (int, IntSet.t) Hashtbl.t;
    succs : (int, IntSet.t) Hashtbl.t;
    backs : (int, IntSet.t) Hashtbl.t;
    preds : (int, int) Hashtbl.t;
    mutable loops : IntSet.t;
    mutable blocks : IntSet.t;
    mutable interm_idx : int }

let get_preds st pc = try Hashtbl.find st.preds pc with Not_found -> 0
let incr_preds st pc = Hashtbl.replace st.preds pc (get_preds st pc + 1)
let decr_preds st pc = Hashtbl.replace st.preds pc (get_preds st pc - 1)

let rec build_graph st blocks pc anc =
  if not (IntSet.mem pc st.blocks) then begin
    st.blocks <- IntSet.add pc st.blocks;
    let anc = IntSet.add pc anc in
    let s = Code.fold_children blocks pc IntSet.add IntSet.empty in
    Hashtbl.add st.all_succs pc s;
    let succs = IntSet.diff s anc in
    Hashtbl.add st.succs pc succs;
    let backs = IntSet.inter s anc in
    Hashtbl.add st.backs pc backs;
    IntSet.iter
      (fun pc' -> st.loops <- IntSet.add pc' st.loops) backs;
    IntSet.iter
      (fun pc' -> incr_preds st pc'; build_graph st blocks pc' anc) succs
  end

let rec dominance_frontier_rec st blocks pc visited grey =
  let n = get_preds st pc in
  let v = try IntMap.find pc visited with Not_found -> 0 in
  if v < n then begin
    let v = v + 1 in
    let visited = IntMap.add pc v visited in
    if v = n then begin
      let grey = IntSet.remove pc grey in
      let s = Hashtbl.find st.all_succs pc in
      IntSet.fold
        (fun pc' (visited, grey) ->
           dominance_frontier_rec st blocks pc' visited grey)
        s (visited, grey)
    end else begin
      (visited, if v = 1 then IntSet.add pc grey else grey)
    end
  end else
    (visited, grey)

let dominance_frontier st blocks pc =
  snd (dominance_frontier_rec st blocks pc IntMap.empty IntSet.empty)

let rec resolve_node interm pc =
  try
    resolve_node interm (fst (IntMap.find pc interm))
  with Not_found ->
    pc

let resolve_nodes interm s =
  IntSet.fold (fun pc s' -> IntSet.add (resolve_node interm pc) s')
    s IntSet.empty

let rec compile_block st blocks pc frontier interm =
  if pc >= 0 then begin
    if IntSet.mem pc st.blocks then begin
      Format.eprintf "!!!! %d@." pc; assert false
    end;
    st.blocks <- IntSet.add pc st.blocks
  end;
  if IntSet.mem pc st.loops then Format.eprintf "@[<2>while (1) {@,";
  Format.eprintf "block %d;" pc;
  let succs = Hashtbl.find st.succs pc in
  let backs = Hashtbl.find st.backs pc in
  let grey =
    IntSet.fold
      (fun pc grey -> IntSet.union (dominance_frontier st blocks pc) grey)
      succs IntSet.empty
  in
  let new_frontier = resolve_nodes interm grey in
  let (_, _, last) = IntMap.find pc blocks in
  begin match last with
    Code.Pushtrap ((pc1, _), pc2, ((pc3, _) as cont)) ->
(* FIX: document *)
      let grey =  dominance_frontier st blocks pc2 in
      let grey' = resolve_nodes interm grey in
      let inner_frontier =
        if IntSet.is_empty grey'&& not (Code.is_dummy_cont cont) then
          IntSet.add pc3 grey'
        else grey'
      in
      if not (Code.is_dummy_cont cont) then incr_preds st pc3;
      assert (IntSet.cardinal inner_frontier <= 1);
      Format.eprintf "@[<2>try {@,";
      compile_block st blocks pc1 inner_frontier interm;
      Format.eprintf "} catch {@,";
      compile_block st blocks pc2 inner_frontier interm;
      Format.eprintf "}@]";
      if not (Code.is_dummy_cont cont) then decr_preds st pc3;
      if not (IntSet.is_empty inner_frontier) then begin
        let pc = IntSet.choose inner_frontier in
        if not (IntSet.mem pc frontier) then
          compile_block st blocks pc frontier interm
      end
  | _ ->
      let (new_frontier, new_interm, blocks) =
        if IntSet.cardinal new_frontier > 1 then begin
          let idx = st.interm_idx in
          st.interm_idx <- idx - 1;
          let blocks = IntMap.add idx (None, [], Code.Stop) blocks in
          IntSet.iter (fun pc -> incr_preds st pc) new_frontier;
          Hashtbl.add st.succs idx new_frontier;
          Hashtbl.add st.all_succs idx new_frontier;
          Hashtbl.add st.backs idx IntSet.empty;
          let x = Code.Var.fresh () in
          Format.eprintf "@ var %a;" Code.Var.print x;
          let a = Array.of_list (IntSet.elements new_frontier) in
          (IntSet.singleton idx,
           Array.fold_right
             (fun (pc, i) interm -> (IntMap.add pc (idx, (x, i)) interm))
             (Array.mapi (fun i pc -> (pc, i)) a) interm,
           blocks)
        end else
          (new_frontier, interm, blocks)
      in
      assert (IntSet.cardinal new_frontier <= 1);
      compile_switch st blocks pc backs new_frontier new_interm;
      if IntSet.cardinal new_frontier > 0 then begin
        let pc = IntSet.choose new_frontier in
        if not (IntSet.mem pc frontier) then
          compile_block st blocks pc frontier interm
      end
  end;
  if IntSet.mem pc st.loops then begin
    if IntSet.cardinal new_frontier > 0 then
      Format.eprintf "@ break; }@]"
    else
      Format.eprintf "}@]"
  end

and compile_switch st blocks pc backs frontier interm =
  let succs = Hashtbl.find st.all_succs pc in
  if IntSet.cardinal succs > 1 then begin
    Format.eprintf "@ @[<2>cond";
    IntSet.iter
      (fun pc ->
         Format.eprintf " @[<1>{";
         compile_branch st blocks pc backs frontier interm;
         Format.eprintf "}@]")
      succs;
    Format.eprintf "@]"
  end else if IntSet.cardinal succs > 0 then
    compile_branch st blocks (IntSet.choose succs) backs frontier interm

and compile_branch st blocks pc backs grey interm =
  if IntSet.mem pc backs then
    Format.eprintf "continue;"
  else if IntSet.mem pc grey || IntMap.mem pc interm then begin
    (* Just set a variable, if necessary *)
    Format.eprintf "(br %d)" pc;
    if IntMap.mem pc interm then decr_preds st pc;
    compile_branch_selection pc interm
  end else
    compile_block st blocks pc grey interm

and compile_branch_selection pc interm =
  try
    let (pc, (x, i)) = IntMap.find pc interm in
    Format.eprintf "@ %a=%d;" Code.Var.print x i;
    compile_branch_selection pc interm
  with Not_found ->
    ()

(****)

(* Debugging code *)

let analyze_node st blocks pc =
  let s = Hashtbl.find st.all_succs pc in
  let grey =
    IntSet.fold
      (fun pc grey -> IntSet.union (dominance_frontier st blocks pc) grey)
      s IntSet.empty
  in
  IntSet.cardinal grey > 1

let output ch st blocks =
  let s =
    IntSet.fold
      (fun pc s ->
         if analyze_node st blocks pc then IntSet.add pc s else s)
      st.blocks IntSet.empty
  in
(*  if not (IntSet.is_empty s) then *) begin
    IntSet.iter
      (fun pc ->
         if IntSet.mem pc s then
           Printf.fprintf ch "%d [color=red]\n" pc
         else
           Printf.fprintf ch "%d\n" pc)
      st.blocks;
    Hashtbl.iter
      (fun pc s ->
         IntSet.iter (fun pc' -> Printf.fprintf ch "%d -> %d\n" pc pc') s)
      st.succs;
    Hashtbl.iter
      (fun pc s ->
         IntSet.iter
           (fun pc' -> Printf.fprintf ch "%d -> %d [color=green]\n" pc pc') s)
      st.backs
  end

(****)

let analyze (_, blocks, _) ch pc =
  let st =
    { blocks = IntSet.empty; loops = IntSet.empty;
      all_succs = Hashtbl.create 17; succs = Hashtbl.create 17;
      backs = Hashtbl.create 17; preds = Hashtbl.create 17;
      interm_idx = -1 }
  in
  build_graph st blocks pc IntSet.empty;
  output ch st blocks;

  let current_blocks = st.blocks in
  st.blocks <- IntSet.empty;
  Format.eprintf "@[";
  compile_block st blocks pc IntSet.empty IntMap.empty;
  if IntSet.cardinal st.blocks <> IntSet.cardinal current_blocks then begin
    Format.eprintf "Some blocks not compiled!"; assert false
  end;
  Format.eprintf "@]@.@."

let f p =
  let ch = open_out "/tmp/p.dot" in
  Printf.fprintf ch "digraph G {\n";
  Code.fold_closures p (fun pc () -> analyze p ch pc) ();
  Printf.fprintf ch "}";
  close_out ch
