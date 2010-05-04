(*
FIX: is there a way to merge this with dead code elimination?
*)

open Util
open Code

(****)

(* Postorder traversal of the whole program. *)

let traverse blocks pc f accu =
  let rec traverse_rec visited pc accu =
    if IntSet.mem pc visited then (visited, accu) else begin
      let visited = IntSet.add pc visited in
      let block = IntMap.find pc blocks in
      let (visited, accu) =
        List.fold_left
          (fun ((visited, accu) as p) i ->
             match i with
               Let (_, Closure (_, (pc, _))) ->
                 traverse_rec visited pc accu
             | _ ->
                 p)
          (visited, accu) block.body
      in
      let (visited, accu) =
        match block.branch with
          Return _ | Raise _ | Stop ->
            (visited, accu)
        | Branch (pc, _) ->
            traverse_rec visited pc accu
        | Cond (_, _, (pc1, _), (pc2, _)) ->
            let (visited, accu) = traverse_rec visited pc1 accu in
            traverse_rec visited pc2 accu
        | Switch (_, a1, a2) ->
            let (visited, accu) =
              Array.fold_left
                (fun (visited, accu) (pc, _) -> traverse_rec visited pc accu)
                (visited, accu) a1 in
            let (visited, accu) =
              Array.fold_left
                (fun (visited, accu) (pc, _) -> traverse_rec visited pc accu)
                (visited, accu) a2 in
            (visited, accu)
        | Pushtrap ((pc1, _), _, (pc2, _), _) ->
            let (visited, accu) = traverse_rec visited pc1 accu in
            traverse_rec visited pc2 accu
        | Poptrap (pc, _) ->
            traverse_rec visited pc accu
      in
      (visited, f pc accu)
    end
  in
  snd (traverse_rec IntSet.empty pc accu)

(****)

let is_trivial instr last =
  instr = []
    &&
  begin match last with
    Return _ | Raise _ | Stop | Branch _       -> true
  | Cond _ | Poptrap _ | Switch _ | Pushtrap _ -> false
  end

let resolve_branch blocks (pc, args) =
  match IntMap.find pc blocks with
(* FIX: handler? *)
    {params = params; handler = None; body = [];
     branch = Branch (pc', args')} ->
      let m = Subst.build_mapping params args in
      let args'' =
        List.map (Subst.from_map m) args' in
      Some (pc', args'')
  | _ ->
      None

let concat_blocks pc instr params handler args params' instr' last' =
  (* This is only valid because we know that the params only occur in
     the block *)
  let m = Subst.build_mapping params' args in
  let s = Subst.from_map m in
    { params = params;
      handler = handler;
      body = instr @ Subst.instrs s instr';
      branch = Subst.last s last' }

let rec block_simpl pc (preds, blocks) =
  let block = IntMap.find pc blocks in
    match block.branch with
        Return _ | Raise _ | Stop | Poptrap _ ->
          (preds, blocks)
      | Branch (pc', args) ->
          let block' = IntMap.find pc' blocks in
(*XXX We can always rename variables so that params' = []... *)
            if
(*FIX: is that correct? in particular, function entry points
  may have only one predecessor... *)
              IntSet.cardinal (IntMap.find pc' preds) = 1
                &&
              block'.params = [] && block'.handler = None
            then begin
              (preds,
               IntMap.add pc
                 (concat_blocks pc block.body block.params block.handler args
                    block'.params block'.body block'.branch)
                 blocks)
            end else if is_trivial block'.body block'.branch then begin
              (IntMap.add pc' (IntSet.remove pc (IntMap.find pc' preds))
                 preds,
               IntMap.add
                 pc (concat_blocks
                       pc block.body block.params block.handler args
                       block'.params block'.body block'.branch)
                 blocks)
            end else
              (preds, blocks)
      | Cond (c, x, cont1, cont2) ->
          if cont1 = cont2 then begin
            let blocks =
              IntMap.add pc {block with branch = Branch cont1 } blocks in
            block_simpl pc (preds, blocks)
          end else begin
            match resolve_branch blocks cont1 with
              Some cont1' ->
                let pc1 = fst cont1 in let pc1' = fst cont1' in
                let preds =
                  IntMap.add pc1'
                    (IntSet.add pc
                       (IntSet.remove pc1 (IntMap.find pc1' preds)))
                    preds
                in
                let blocks =
                  IntMap.add pc
                    { block with branch = Cond (c, x, cont1', cont2) } blocks
                in
                block_simpl pc (preds, blocks)
            | None ->
                match resolve_branch blocks cont2 with
                  Some cont2' ->
                    let pc2 = fst cont2 in let pc2' = fst cont2' in
                    let preds =
                      IntMap.add pc2'
                        (IntSet.add pc
                           (IntSet.remove pc2 (IntMap.find pc2' preds)))
                        preds
                    in
                    let blocks =
                      IntMap.add pc
                        { block with branch = Cond (c, x, cont1, cont2') }
                        blocks
                    in
                    block_simpl pc (preds, blocks)
                | None ->
                    (preds, blocks)
          end
      | Switch (x, a1, a2) ->
          let a1 =
            Array.map
              (fun pc ->
                 match resolve_branch blocks pc with Some pc -> pc | None -> pc)
              a1 in
          let a2 =
            Array.map
              (fun pc ->
                 match resolve_branch blocks pc with Some pc -> pc | None -> pc)
              a2 in
            (preds,
             IntMap.add pc { block with branch = Switch (x, a1, a2) } blocks)
      | Pushtrap _ ->
          (preds, blocks)

let simpl (pc, blocks, free_pc) =
  (*
    let redirect blocks orig pc pc' =
    let (instr, last) = IntMap.find orig blocks in
    let last =
    match last with
    Return _ | Raise _ | Stop ->
    assert false
    | Branch (pc'', x) ->
    assert (pc'' = pc);
    Branch (pc', x)
    | Cond (c, x, pc1, pc2) ->
    assert (pc1 = pc || pc2 = pc);
    Cond (c, x,
    (if pc1 = pc then pc' else pc1),
    (if pc2 = pc then pc' else pc2))
    | Switch (x, a1, a2) ->
    Switch (x,
    Array.map (fun pc'' -> if pc'' = pc then pc' else pc'') a1,
    Array.map (fun pc'' -> if pc'' = pc then pc' else pc'') a2)
    | Pushtrap (pc1, x, pc2) ->
    assert (pc1 = pc || pc2 = pc);
    Pushtrap ((if pc1 = pc then pc' else pc1), x,
    (if pc2 = pc then pc' else pc2))
    in
    IntMap.add orig (instr, last) blocks
    in
    let (blocks, free_pc) =
    IntMap.fold
    (fun pc _ (blocks, free_pc) ->
    let (instr, last) = IntMap.find pc blocks in
    match instr with
    Let (x, Variable y) :: rem ->
    let s =
    List.fold_left
    (fun s (x, _) -> IntSet.add (Var.idx x) s) IntSet.empty l in
    if IntSet.cardinal s > 1 then begin
    let blocks = IntMap.add pc (rem, last) blocks in
    IntSet.fold
    (fun idx (blocks, free_pc) ->
    let l = List.filter (fun (x, _) -> Var.idx x = idx) l in
    let blocks =
    List.fold_left
    (fun blocks (_, orig) ->
    redirect blocks orig pc free_pc)
    blocks l
    in
    (IntMap.add free_pc ([Let (x, Phi l)], Branch pc) blocks,
    free_pc + 1))
    s (blocks, free_pc)
    end else
    (blocks, free_pc)
    | _ ->
    (blocks, free_pc))
    blocks (blocks, free_pc)
    in
  *)
  let preds = IntMap.map (fun _ -> IntSet.empty) blocks in
  let add_pred pc (pc', _) preds =
    IntMap.add pc' (IntSet.add pc (IntMap.find pc' preds)) preds in
  let preds =
    IntMap.fold
      (fun pc block preds ->
         match block.branch with
           Return _ | Raise _ | Stop ->
             preds
         | Branch cont | Poptrap cont ->
             add_pred pc cont preds
         | Cond (_, _, cont1, cont2) ->
             add_pred pc cont1 (add_pred pc cont2 preds)
         | Pushtrap (cont, _, _, _) ->
             add_pred pc cont preds
         | Switch (_, a1, a2) ->
             let preds =
               Array.fold_left
                 (fun preds cont -> add_pred pc cont preds) preds a1 in
             let preds =
               Array.fold_left
                 (fun preds cont -> add_pred pc cont preds) preds a2 in
             preds)
      blocks preds
  in
  let (_, blocks) = traverse blocks pc block_simpl (preds, blocks) in
  (pc, blocks, free_pc)
