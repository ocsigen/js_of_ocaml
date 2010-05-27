
open Code

(****)

(* FIX: more or less duplicated from deadcode.ml *)
let pure_expr pure_funs e =
  match e with
    Const _  | Block _ | Field _ | Closure _ | Constant _ | Variable _ ->
      true
  | Apply (f, l, n) ->
      VarSet.mem f pure_funs ||
      begin match n with
        Some n -> List.length l < n
      | None   -> false
      end
  | Prim (p, l) ->
      match p with
        Extern f -> Primitive.is_pure f
      | _        -> false

let pure_instr pure_funs i =
  match i with
    Let (_, e) ->
      pure_expr pure_funs e
  | Set_field _ | Offset_ref _ | Array_set _ ->
      false

(****)

let rec traverse blocks pc visited funs =
  try
    (AddrMap.find pc visited, visited, funs)
  with Not_found ->
    let visited = AddrMap.add pc false visited in
    let (pure, visited, funs) =
      fold_children blocks pc
        (fun pc (pure, visited, funs) ->
           let (pure', visited, funs) = traverse blocks pc visited funs in
           (pure && pure', visited, funs))
        (true, visited, funs)
    in
    let (pure, visited, funs) = block blocks pc pure visited funs in
    (pure, AddrMap.add pc pure visited, funs)

and block blocks pc pure visited funs =
  let b = AddrMap.find pc blocks in
  let pure = match b.branch with Raise _ -> false | _ -> pure in
  List.fold_left
    (fun (pure, visited, funs) i ->
       let (visited, funs) =
         match i with
           Let (x, Closure (_, (pc, _))) ->
             let (pure, visited, funs) = traverse blocks pc visited funs in
             (visited, if pure then VarSet.add x funs else funs)
         | _ ->
             (visited, funs)
       in
       (pure && pure_instr funs i, visited, funs))
    (pure, visited, funs) b.body

let f (pc, blocks, _) =
  let (_, _, funs) = traverse blocks pc AddrMap.empty VarSet.empty in
  funs
