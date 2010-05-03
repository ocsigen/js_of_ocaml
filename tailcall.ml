open Util

(* FIX: it should be possible to deal with tail-recursion in exception
   handlers, but we have to adapt the code generator for that *)

let rec remove_last l =
  match l with
    []     -> assert false
  | [_]    -> []
  | x :: r -> x :: remove_last r

let rec tail_call x f l =
  match l with
    [] -> None
  | [Code.Let (y, Code.Direct_apply (g, args))]
        when Code.Var.compare x y = 0 && Code.Var.compare f g = 0 ->
      Some args
  | i :: rem ->
      tail_call x f rem

let rewrite_block (f, f_params, f_pc, args) pc blocks =
  let (params, instr, last) = IntMap.find pc blocks in
  match last with
    Code.Return x ->
      begin match tail_call x f instr with
        Some f_args ->
          let m = Subst.build_mapping f_params f_args in
          IntMap.add pc
            (params,
             remove_last instr,
             Code.Branch
               (f_pc, List.map (fun x -> Subst.VarMap.find x m) args))
            blocks
      | _ ->
          blocks
      end
  | _ ->
      blocks

let (>>) x f = f x

(* Skip try body and exception handler *)
let fold_children blocks pc f accu =
  let (_, _, last) = IntMap.find pc blocks in
  match last with
    Code.Return _ | Code.Raise _ | Code.Stop ->
      accu
  | Code.Pushtrap (_, _, _, pc')
  | Code.Branch (pc', _) | Code.Poptrap (pc', _) ->
      f pc' accu
  | Code.Cond (_, _, (pc1, _), (pc2, _)) ->
      accu >> f pc1 >> f pc2
  | Code.Switch (_, a1, a2) ->
      accu >> Array.fold_right (fun (pc, _) accu -> f pc accu) a1
           >> Array.fold_right (fun (pc, _) accu -> f pc accu) a2

let rec traverse f pc visited blocks =
  if not (IntSet.mem pc visited) then begin
    let visited = IntSet.add pc visited in
    let blocks = rewrite_block f pc blocks in
    let (visited, blocks) =
      Code.fold_children blocks pc
        (fun pc (visited, blocks) ->
           let (visited, blocks) =
             traverse f pc visited blocks in
           (visited, blocks))
        (visited, blocks)
    in
    (visited, blocks)
  end else
    (visited, blocks)

let f ((pc, blocks, free_pc) as p) =
  let blocks =
    Code.fold_closures p
      (fun f params (pc, args) blocks ->
         match f with
           Some f ->
             let (_, blocks) =
               traverse (f, params, pc, args) pc IntSet.empty blocks in
             blocks
         | None ->
             blocks)
    blocks
  in
  (pc, blocks, free_pc)
