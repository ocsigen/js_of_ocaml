open Util

type t =
    Block of Code.addr
  | Seq of t list
  | Cond of t * t list
  | Loop of t
  | Shortcut of t * t

let seq s1 s2 =
  match s1, s2 with
    Seq s1, Seq s2 -> Seq (s1 @ s2)
  | Seq s1, _      -> Seq (s1 @ [s2])
  | _,      Seq s2 -> Seq (s1 :: s2)
  | _              -> Seq [s1; s2]

let rec cond s sl =
  match s with
    Seq [s; s']  -> seq s (cond s' sl)
  | Seq (s :: r) -> seq s (cond (Seq r) sl)
  | _            -> Cond (s, sl)

let rec shortcut s1 s2 =
  match s1 with
    Seq [s1; s1'] -> seq s1 (shortcut s1' s2)
  | Seq (s1 :: r) -> seq s1 (shortcut (Seq r) s2)
  | _             -> Shortcut (s1, s2)

type state' =
  { mutable block_set : IntSet.t;
    blocks : (int, t) Hashtbl.t;
    preds : (int, IntSet.t) Hashtbl.t;
    succs : (int, IntSet.t) Hashtbl.t;
    initial_succs : (int, IntSet.t) Hashtbl.t; }

(****)

let cl = ref 0

let rec output_structure ch s =
  match s with
    Block pc ->
      Printf.fprintf ch "%d\n" pc
  | Seq l ->
      let nm = Format.sprintf "cluster_%d" !cl in
      incr cl;
      Printf.fprintf ch "subgraph %s {\n" nm;
      Printf.fprintf ch "label = Seq\n";
      List.iter (output_structure ch) l;
      Printf.fprintf ch "}\n"
  | Cond (s, l) ->
      let nm = Format.sprintf "cluster_%d" !cl in
      incr cl;
      Printf.fprintf ch "subgraph %s {\n" nm;
      Printf.fprintf ch "label = Cond\n";
      List.iter (output_structure ch) (s :: l);
      Printf.fprintf ch "}\n"
  | Loop s ->
      let nm = Format.sprintf "cluster_%d" !cl in
      incr cl;
      Printf.fprintf ch "subgraph %s {\n" nm;
      Printf.fprintf ch "label = Loop\n";
      output_structure ch s;
      Printf.fprintf ch "}\n"
  | Shortcut (s, s') ->
      let nm = Format.sprintf "cluster_%d" !cl in
      incr cl;
      Printf.fprintf ch "subgraph %s {\n" nm;
      Printf.fprintf ch "label = Shortcut\n";
      output_structure ch s;
      output_structure ch s';
      Printf.fprintf ch "}\n"

let output ch st =
  Hashtbl.iter (fun _ s -> output_structure ch s) st.blocks;
  Hashtbl.iter
    (fun pc s ->
       IntSet.iter (fun pc' -> Printf.fprintf ch "%d -> %d\n" pc pc') s)
    st.initial_succs

(****)

let add_pred st pc pc' =
  Hashtbl.replace st.preds pc'
    (IntSet.add pc
       (try Hashtbl.find st.preds pc' with Not_found -> IntSet.empty))

let rec build st blocks pc =
  if not (Hashtbl.mem st.blocks pc) then begin
    st.block_set <- IntSet.add pc st.block_set;
    Hashtbl.add st.blocks pc (Block pc);
    let s = Code.fold_children blocks pc IntSet.add IntSet.empty in
    Hashtbl.add st.succs pc s;
    Hashtbl.add st.initial_succs pc s;
    IntSet.iter (fun pc' -> add_pred st pc pc'; build st blocks pc') s;
    if not (Hashtbl.mem st.preds pc) then
      Hashtbl.add st.preds pc IntSet.empty
  end

let unlink st pc pc' =
  Hashtbl.replace st.succs pc (IntSet.remove pc' (Hashtbl.find st.succs pc));
  Hashtbl.replace st.preds pc' (IntSet.remove pc (Hashtbl.find st.preds pc'))

let relink_succ st pc pc' =
  let s = Hashtbl.find st.succs pc' in
  Hashtbl.replace st.succs pc' IntSet.empty;
  Hashtbl.replace st.succs pc (IntSet.union (Hashtbl.find st.succs pc) s);
  IntSet.iter
    (fun pc'' ->
       Hashtbl.replace st.preds pc''
         (IntSet.add pc (IntSet.remove pc' (Hashtbl.find st.preds pc''))))
    s

let make_cond_ret st pc pc1 =
  IntSet.is_empty (Hashtbl.find st.succs pc1)
    &&
  IntSet.cardinal (Hashtbl.find st.preds pc1) = 1
    &&
  begin
    let b = Hashtbl.find st.blocks pc in
    let b1 = Hashtbl.find st.blocks pc1 in
    Hashtbl.replace st.blocks pc (cond b [b1]);
    Hashtbl.remove st.blocks pc1;
    unlink st pc pc1;
    true
  end

let make_cond_single st pc pc1 pc2 =
  let s1 = Hashtbl.find st.succs pc1 in
  IntSet.cardinal s1 = 1 && IntSet.choose s1 = pc2
    &&
  IntSet.cardinal (Hashtbl.find st.preds pc1) = 1
    &&
  begin
    let b = Hashtbl.find st.blocks pc in
    let b1 = Hashtbl.find st.blocks pc1 in
    Hashtbl.replace st.blocks pc (cond b [b1]);
    Hashtbl.remove st.blocks pc1;
    unlink st pc pc1;
    relink_succ st pc pc1;
    true
  end

let make_cond_bin st pc pc1 pc2 =
  let s1 = Hashtbl.find st.succs pc1 in
  let s2 = Hashtbl.find st.succs pc2 in
  IntSet.cardinal s1 = 1 && IntSet.cardinal s2 = 1
    &&
  IntSet.choose s1 = IntSet.choose s2
    &&
  IntSet.cardinal (Hashtbl.find st.preds pc1) = 1
    &&
  IntSet.cardinal (Hashtbl.find st.preds pc2) = 1
    &&
  begin
    let b = Hashtbl.find st.blocks pc in
    let b1 = Hashtbl.find st.blocks pc1 in
    let b2 = Hashtbl.find st.blocks pc2 in
    Hashtbl.replace st.blocks pc (cond b [b1; b2]);
    Hashtbl.remove st.blocks pc1;
    Hashtbl.remove st.blocks pc2;
    unlink st pc pc1;
    unlink st pc pc2;
    relink_succ st pc pc1;
    relink_succ st pc pc2;
    true
  end

let make_shortcut st pc pc1 pc2 =
  let s1 = Hashtbl.find st.succs pc1 in
  (*IntSet.cardinal s1 = 2 &&*) IntSet.mem pc2 s1
    &&
  IntSet.cardinal (Hashtbl.find st.preds pc1) = 1
    &&
(*
  IntSet.cardinal (Hashtbl.find st.preds pc2) = 2
    &&
*)
  begin
    let b = Hashtbl.find st.blocks pc in
    let b1 = Hashtbl.find st.blocks pc1 in
    Hashtbl.replace st.blocks pc (shortcut b b1);
    Hashtbl.remove st.blocks pc1;
    unlink st pc pc1;
    relink_succ st pc pc1;
    true
  end

let make_cond st pc =
  let s = Hashtbl.find st.succs pc in
  IntSet.cardinal s = 2
     &&
  begin
    let pc1 = IntSet.choose s in
    let pc2 = IntSet.choose (IntSet.remove pc1 s) in
    make_cond_ret st pc pc1 || make_cond_ret st pc pc2 ||
    make_cond_single st pc pc1 pc2 || make_cond_single st pc pc2 pc1 ||
    make_cond_bin st pc pc1 pc2 ||
    make_shortcut st pc pc1 pc2 || make_shortcut st pc pc2 pc1
  end

let make_seq st pc =
  let s = Hashtbl.find st.succs pc in
  IntSet.cardinal s = 1
     &&
  let pc' = IntSet.choose s in
  IntSet.cardinal (Hashtbl.find st.preds pc') = 1
     &&
  begin
    let b = Hashtbl.find st.blocks pc in
    let b' = Hashtbl.find st.blocks pc' in
    Hashtbl.replace st.blocks pc (seq b b');
    Hashtbl.remove st.blocks pc';
    unlink st pc pc';
    relink_succ st pc pc';
    true
  end

let make_loop st pc =
  let s = Hashtbl.find st.succs pc in
  IntSet.cardinal s <= 2
      &&
  IntSet.mem pc s
      &&
  begin
    let b = Hashtbl.find st.blocks pc in
    Hashtbl.replace st.blocks pc (Loop b);
    unlink st pc pc;
    true
  end

let make_simple_switch st pc =
  let s = Hashtbl.find st.succs pc in
  IntSet.cardinal s > 2
      &&
  let (s', s'') =
    IntSet.partition
      (fun pc ->
         IntSet.is_empty (Hashtbl.find st.succs pc)
             &&
         IntSet.cardinal (Hashtbl.find st.preds pc) = 1)
      s
  in
  IntSet.cardinal s'' < 2
      &&
    begin
    let b = Hashtbl.find st.blocks pc in
    let bl =
      List.map (fun pc' -> Hashtbl.find st.blocks pc') (IntSet.elements s') in
    Hashtbl.replace st.blocks pc (cond b bl);
    IntSet.iter
      (fun pc' ->
         Hashtbl.remove st.blocks pc';
         unlink st pc pc')
      s';
    true
  end

let make_simple_switch_2 st pc =
  let s = Hashtbl.find st.succs pc in
  IntSet.cardinal s > 2
      &&
  IntSet.for_all
    (fun pc ->
       IntSet.cardinal (Hashtbl.find st.preds pc) = 1
          &&
       IntSet.cardinal (Hashtbl.find st.succs pc) <= 1)
    s
    &&
  let s' =
    IntSet.fold
      (fun pc s -> IntSet.union (Hashtbl.find st.succs pc) s) s IntSet.empty
  in
  IntSet.cardinal s' = 1
    &&
  begin
    let b = Hashtbl.find st.blocks pc in
    let bl =
      List.map (fun pc' -> Hashtbl.find st.blocks pc') (IntSet.elements s) in
    Hashtbl.replace st.blocks pc (cond b bl);
    IntSet.iter
      (fun pc' ->
         Hashtbl.remove st.blocks pc';
         unlink st pc pc';
         relink_succ st pc pc')
      s;
    true
  end

let make_simple_switch_3' st pc s pc' =
  let s = IntSet.remove pc' s in
  IntSet.for_all
    (fun pc ->
       IntSet.cardinal (Hashtbl.find st.preds pc) = 1
          &&
       let s = Hashtbl.find st.succs pc in
       IntSet.cardinal s = 0 ||
       (IntSet.cardinal s = 1 && IntSet.choose s = pc'))
    s
    &&
  begin
    let b = Hashtbl.find st.blocks pc in
    let bl =
      List.map (fun pc' -> Hashtbl.find st.blocks pc') (IntSet.elements s) in
    Hashtbl.replace st.blocks pc (cond b bl);
    IntSet.iter
      (fun pc' ->
         Hashtbl.remove st.blocks pc';
         unlink st pc pc';
         relink_succ st pc pc')
      s;
    true
  end

let make_simple_switch_3 st pc =
  let s = Hashtbl.find st.succs pc in
  IntSet.cardinal s > 2
      &&
  IntSet.exists (fun pc' -> make_simple_switch_3' st pc s pc') s

let rec merge st =
  let changed = ref false in
  IntSet.iter
    (fun pc ->
       if Hashtbl.mem st.blocks pc then begin
         changed :=
           !changed || make_cond st pc || make_seq st pc || make_loop st pc ||
           make_simple_switch st pc || make_simple_switch_2 st pc ||
           make_simple_switch_3 st pc
       end else
         st.block_set <- IntSet.remove pc st.block_set)
    st.block_set;
  if !changed then merge st

let analyze (_, blocks, _) ch pc =
  let st =
    { block_set = IntSet.empty;
      blocks = Hashtbl.create 17; preds = Hashtbl.create 17;
      succs = Hashtbl.create 17; initial_succs = Hashtbl.create 17 }
  in
  build st blocks pc;
  merge st;
  if not (IntSet.is_empty (Hashtbl.find st.succs pc)) then output ch st

let f p =
  let ch = open_out "/tmp/p.dot" in
  Printf.fprintf ch "digraph G {\n";
  Code.fold_closures p (fun pc () -> analyze p ch pc) ();
  Printf.fprintf ch "}";
  close_out ch

(***************************)

let preds = Hashtbl.create 107

let add_pred pc = Hashtbl.replace preds pc (Hashtbl.find preds pc + 1)

let count_preds blocks =
  IntMap.iter (fun pc _ -> Hashtbl.add preds pc 0) blocks;
  IntMap.iter
    (fun pc _ ->
       let s = Code.fold_children blocks pc IntSet.add IntSet.empty in
       IntSet.iter (fun pc' -> add_pred pc') s)
    blocks

let rec traverse blocks pc visited grey =
  let n = Hashtbl.find preds pc in
  let v = try IntMap.find pc visited with Not_found -> 0 in
  if v < n then begin
    let v = v + 1 in
    let visited = IntMap.add pc v visited in
    if v = n then begin
      let grey = IntSet.remove pc grey in
      Code.fold_children blocks pc
        (fun pc' (visited, grey) -> traverse blocks pc' visited grey)
        (visited, grey)
    end else begin
      (visited, if v = 1 then IntSet.add pc grey else grey)
    end
  end else
    (visited, grey)

let analyze_node ch blocks pc =
  let grey =
    Code.fold_children blocks pc
      (fun pc grey ->
         IntSet.union (snd (traverse blocks pc IntMap.empty IntSet.empty))
           grey)
      IntSet.empty
  in
  Printf.fprintf ch "%d [color=%s]\n" pc
    (if IntSet.cardinal grey > 1 then "red" else "black");
if IntSet.cardinal grey > 1 then begin
Format.eprintf "%d:" pc;
IntSet.iter (fun pc' -> Format.eprintf " %d (%d)" pc' (Hashtbl.find preds pc')) grey;
Format.eprintf "@."
end;
  let s = Code.fold_children blocks pc IntSet.add IntSet.empty in
  IntSet.iter (fun pc' -> Printf.fprintf ch "%d -> %d\n" pc pc') s

let f (_, blocks, _) =
  let ch = open_out "/tmp/p.dot" in
  Printf.fprintf ch "digraph G {\n";
  count_preds blocks;
  IntMap.iter (fun pc _ -> analyze_node ch blocks pc) blocks;
  Printf.fprintf ch "}";
  close_out ch

(***************************)

let (>>) x f = f x

let fold_children blocks pc f accu =
  let (_, _, last) = IntMap.find pc blocks in
  match last with
    Code.Return _ | Code.Raise _ | Code.Stop ->
      accu
  | Code.Branch (pc', _) | Code.Poptrap (pc', _) ->
      f pc' accu
  | Code.Cond (_, _, (pc1, _), (pc2, _)) ->
      accu >> f pc1 >> f pc2
  | Code.Pushtrap ((pc1, _), pc2, _) ->
      accu >> f pc1 >> f pc2
  | Code.Switch (_, a1, a2) ->
      accu >> Array.fold_right (fun (pc, _) accu -> f pc accu) a1
           >> Array.fold_right (fun (pc, _) accu -> f pc accu) a2

(****)

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

let rec build st blocks pc anc =
  if not (IntSet.mem pc st.blocks) then begin
    st.blocks <- IntSet.add pc st.blocks;
    let anc = IntSet.add pc anc in
    let s = fold_children blocks pc IntSet.add IntSet.empty in
    Hashtbl.add st.all_succs pc s;
    let succs = IntSet.diff s anc in
    Hashtbl.add st.succs pc succs;
    let backs = IntSet.inter s anc in
    Hashtbl.add st.backs pc backs;
    IntSet.iter (fun pc' -> st.loops <- IntSet.add pc' st.loops) backs;
    IntSet.iter (fun pc' -> incr_preds st pc'; build st blocks pc' anc) succs
  end

let rec traverse st blocks pc visited grey =
  let n = get_preds st pc in
  let v = try IntMap.find pc visited with Not_found -> 0 in
  if v < n then begin
    let v = v + 1 in
    let visited = IntMap.add pc v visited in
    if v = n then begin
      let grey = IntSet.remove pc grey in
      let s = Hashtbl.find st.all_succs pc in
      IntSet.fold
        (fun pc' (visited, grey) -> traverse st blocks pc' visited grey)
        s (visited, grey)
    end else begin
      (visited, if v = 1 then IntSet.add pc grey else grey)
    end
  end else
    (visited, grey)

let rec resolve_node interm pc =
  try
    let (pc, _, _) = IntMap.find pc interm in
    resolve_node interm pc
  with Not_found ->
    pc

(* XXX: we should check that all blocks are visited, and exactly once *)
let rec compile st blocks pc conts interm =
  if pc >= 0 then begin
    if IntSet.mem pc st.blocks then begin
      Format.eprintf "!!!! %d@." pc;
      assert false
    end;
    st.blocks <- IntSet.add pc st.blocks
  end;
  if IntSet.mem pc st.loops then Format.eprintf "@[<2>while (1) {@,";
  Format.eprintf "block %d;" pc;
  let succs = Hashtbl.find st.succs pc in
  let backs = Hashtbl.find st.backs pc in
Format.eprintf "conts:<<";
IntSet.iter (fun pc -> Format.eprintf "%d " pc) conts;
Format.eprintf ">>@.";
Format.eprintf "succs:<<";
IntSet.iter (fun pc -> Format.eprintf "%d " pc) succs;
Format.eprintf ">>@.";
  let grey =
    IntSet.fold
      (fun pc grey ->
         IntSet.union (snd (traverse st blocks pc IntMap.empty IntSet.empty))
           grey)
      succs IntSet.empty
  in
prerr_endline "YYY";
  let (_, _, last) = IntMap.find pc blocks in
prerr_endline "AAA";
  begin match last with
    Code.Pushtrap ((pc1, _), pc2, ((pc3, _) as cont)) ->
(*
FIX:
catch converges with other branch ==> code below correct

catch does not converges (exception reraised) ==> compilation from pc1
should stop at poptrap(s)

continution resumes at grey node or after poptrap

===> get grey set from pc2; if set empty and we have a valid pc3, add
     it to the set; code below then correct

using [IntSet.union grey conts] is not the right solution
really need to interrupt the compilation of the body
(otherwise, we may wrong gray values when compiling the body...)
*)
      let grey = snd (traverse st blocks pc2 IntMap.empty IntSet.empty) in
      let grey' =
        IntSet.fold (fun pc g -> IntSet.add (resolve_node interm pc) g)
          grey IntSet.empty
      in
Format.eprintf "AAA %d %d@." (IntSet.cardinal grey) pc3;
      let grey' =
        if IntSet.is_empty grey'&& not (Code.is_dummy_cont cont) then IntSet.add pc3 grey' else grey' in
      if not (Code.is_dummy_cont cont) then incr_preds st pc3;
      assert (IntSet.cardinal grey <= 1);
      Format.eprintf "@[<2>try {@,";
      compile st blocks pc1 grey' interm;
      Format.eprintf "} catch {@,";
      compile st blocks pc2 grey' interm;
      Format.eprintf "}@]";
      if not (Code.is_dummy_cont cont) then decr_preds st pc3;
      if not (IntSet.is_empty grey') then begin
        let pc = IntSet.choose grey' in
        if not (IntSet.mem pc conts) then
          compile st blocks pc conts interm
      end
  | _ ->
prerr_endline "BBB";
Format.eprintf "grey<";
IntSet.iter (fun pc -> Format.eprintf "%d " pc) grey;
Format.eprintf ">@.";
      let grey' =
        IntSet.fold (fun pc g -> IntSet.add (resolve_node interm pc) g)
          grey IntSet.empty
      in
prerr_endline "CCC";
Format.eprintf "grey'<";
IntSet.iter (fun pc -> Format.eprintf "%d " pc) grey';
Format.eprintf ">@.";
      if IntSet.cardinal grey' > 1 then begin
        let idx = st.interm_idx in
        st.interm_idx <- idx - 1;
        let blocks = IntMap.add idx (None, [], Code.Stop) blocks in
        IntSet.iter (fun pc ->
Format.eprintf "incr %d@." pc;
 incr_preds st pc) grey';
        Hashtbl.add st.succs idx grey';
        Hashtbl.add st.all_succs idx grey';
        Hashtbl.add st.backs idx IntSet.empty;
        let x = Code.Var.fresh () in
        Format.eprintf "@ var %a;" Code.Var.print x;
        Format.eprintf "@ XXX";
        let a = Array.of_list (IntSet.elements grey') in
        let new_interm =
          Array.fold_right
            (fun (pc, i) interm -> (IntMap.add pc (idx, x, i) interm))
            (Array.mapi (fun i pc -> (pc, i)) a) interm
        in

        let s' = Hashtbl.find st.all_succs pc in
        if IntSet.cardinal s' > 1 then begin
          Format.eprintf "@ @[<2>cond";
          IntSet.iter
            (fun pc ->
               Format.eprintf " @[<1>{";
               compile_branch
                 st blocks pc backs (IntSet.singleton idx) new_interm;
               Format.eprintf "}@]")
            s';
          Format.eprintf "@]"
        end else if IntSet.cardinal s' > 0 then
          compile_branch
            st blocks
            (IntSet.choose s') backs (IntSet.singleton idx) new_interm;

        compile st blocks idx conts interm
(*
        Format.eprintf "@ @[<2>switch(%a)" Code.Var.print x;
        Array.iter
          (fun pc ->
             Format.eprintf " @[<1>{";
             compile_block st blocks pc IntSet.empty conts interm;
             Format.eprintf "}@]")
          a;
        Format.eprintf "@]"
*)

(*
Declare fresh variable;
a jump to one of the grey nodes is implemented by setting the variable

We have a remapping of some nodes to virtual nodes
  ===> count the nodes after remapping

We have a new node that hide some previous nodes
     node foo mapped to node bar, with variable x set to n
   apply recursively....
*)
      end else begin
Format.eprintf "DDD";
        let s' = Hashtbl.find st.all_succs pc in
Format.eprintf "EEE";
        if IntSet.cardinal s' > 1 then begin
          Format.eprintf "@ @[<2>cond";
          IntSet.iter
            (fun pc ->
               Format.eprintf " @[<1>{";
               compile_branch st blocks pc backs grey' interm;
               Format.eprintf "}@]")
            s';
          Format.eprintf "@]"
        end else if IntSet.cardinal s' > 0 then
          compile_branch st blocks (IntSet.choose s') backs grey' interm;
        if IntSet.cardinal grey' > 0 then begin
          let pc = IntSet.choose grey' in
Format.eprintf "[[%d]]" pc;
Format.eprintf "<<";
IntSet.iter (fun pc -> Format.eprintf "%d " pc) conts;
Format.eprintf ">>@.";
          if not (IntSet.mem pc conts) then
            compile st blocks pc conts interm
        end
      end
  end;
  if IntSet.mem pc st.loops then begin
    if IntSet.cardinal grey > 0 then
      Format.eprintf "@ break; }@]"
    else
      Format.eprintf "}@]"
  end

and compile_branch st blocks pc backs grey interm =
Format.eprintf "branch %d " pc;
Format.eprintf "((";
IntMap.iter (fun pc _ -> Format.eprintf "%d; " pc) interm;
Format.eprintf "))@.";
  if IntSet.mem pc backs then
    Format.eprintf "continue;"
  else if IntSet.mem pc grey || IntMap.mem pc interm then begin
    (* Just set a variable, if necessary *)
    Format.eprintf "(br %d)" pc;
    if IntMap.mem pc interm then (
decr_preds st pc;
Format.eprintf "decr %d (%d)@." pc (get_preds st pc));
    compile_branch_selection pc interm
  end else
    compile st blocks pc grey interm

and compile_branch_selection pc interm =
  try
    let (pc, x, i) = IntMap.find pc interm in
    Format.eprintf "@ %a=%d;" Code.Var.print x i;
    compile_branch_selection pc interm
  with Not_found ->
    ()

(****)

(* For tests *)

let output ch st s =
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

let analyze_node st blocks pc =
  let s = Hashtbl.find st.all_succs pc in
  let grey =
    IntSet.fold
      (fun pc grey ->
         IntSet.union (snd (traverse st blocks pc IntMap.empty IntSet.empty))
           grey)
      s IntSet.empty
  in
  IntSet.cardinal grey > 1

(****)

let analyze (_, blocks, _) ch pc =
  let st =
    { blocks = IntSet.empty; loops = IntSet.empty;
      all_succs = Hashtbl.create 17; succs = Hashtbl.create 17;
      backs = Hashtbl.create 17; preds = Hashtbl.create 17;
      interm_idx = -1 }
  in
  build st blocks pc IntSet.empty;


  let s =
    IntSet.fold
      (fun pc s ->
         if analyze_node st blocks pc then IntSet.add pc s else s)
      st.blocks IntSet.empty
  in
(*  if not (IntSet.is_empty s) then *)output ch st s;

  let current_blocks = st.blocks in
  st.blocks <- IntSet.empty;
  Format.eprintf "@[";
  compile st blocks pc IntSet.empty IntMap.empty;
  Format.eprintf "@]%s@.@."
    (if IntSet.cardinal st.blocks <> IntSet.cardinal current_blocks then
       "!!!"
     else
       "")

let f p =
  let ch = open_out "/tmp/p.dot" in
  Printf.fprintf ch "digraph G {\n";
  Code.fold_closures p (fun pc () -> analyze p ch pc) ();
  Printf.fprintf ch "}";
  close_out ch
