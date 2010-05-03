(*FIX:
do we handle correctly:

   while true do
      try raise Exit with e -> ... fun () -> ...
   done
*)

let (>>) x f = f x
let list_fold f b l s = List.fold_left (fun s x -> f b x s) s l
let array_fold f b a s = Array.fold_right (fun x s ->  f b x s) a s

module VarSet = Set.Make (Code.Var)

let add_var b x s = if VarSet.mem x b then s else VarSet.add x s

let closure_free_vars = ref (fun pc params (pc', _) -> assert false)

let cont_free_vars b (_, l) s = list_fold add_var b l s

let expr_free_vars pc b x e s =
  match e with
    Code.Const _ | Code.Constant _ ->
      s
  | Code.Apply (x, l) | Code.Direct_apply (x, l) ->
      s >> add_var b x >> list_fold add_var b l
  | Code.Block (_, a) ->
      Array.fold_right (fun x s -> add_var b x s) a s
  | Code.Field (x, _) ->
      add_var b x s
  | Code.Closure (params, cont) ->
      let s' = !closure_free_vars pc params cont in
(*
Format.eprintf "@[<2>Free vars of function %a:@ %a@]@."
Code.Var.print x
Code.print_var_list (VarSet.elements s');
*)
      VarSet.union s (VarSet.diff s' b)
  | Code.Prim (_, l) ->
      list_fold add_var b l s
  | Code.Variable x ->
      add_var b x s

let instr_free_vars pc b i s =
  match i with
    Code.Let (x, e) ->
      expr_free_vars pc b x e s
  | Code.Set_field (x, _, y) ->
      s >> add_var b x >> add_var b y
  | Code.Offset_ref (x, _) ->
      add_var b x s
  | Code.Array_set (x, y, z) ->
      s >> add_var b x >> add_var b y >> add_var b z

let last_free_var b l s =
  match l with
    Code.Return x
  | Code.Raise x ->
      add_var b x s
  | Code.Stop ->
      s
  | Code.Branch cont ->
      cont_free_vars b cont s
  | Code.Cond (_, x, cont1, cont2) ->
      s >> add_var b x >> cont_free_vars b cont1 >> cont_free_vars b cont2
  | Code.Switch (x, a1, a2) ->
      s
      >> add_var b x
      >> array_fold cont_free_vars b a1
      >> array_fold cont_free_vars b a2
  | Code.Pushtrap (cont1, _, cont2, _) ->
      s
      >> cont_free_vars b cont1
      >> cont_free_vars b cont2
  | Code.Poptrap cont ->
      cont_free_vars b cont s

let block_free_vars pc b (params, instr, last) s =
  s >> list_fold (instr_free_vars pc) b instr >> last_free_var b last

let instr_bound_vars b i s =
  match i with
    Code.Let (x, _) ->
      add_var b x s
  | Code.Set_field _ | Code.Offset_ref _ | Code.Array_set _ ->
      s

let last_bound_vars b l s =
  match l with
    Code.Return _ | Code.Raise _ | Code.Stop | Code.Branch _
  | Code.Cond _ | Code.Switch _ | Code.Poptrap _ ->
      s
  | Code.Pushtrap (_, x, _, _) ->
      add_var b x s

let block_bound_vars (params, instr, last) s =
  let b = VarSet.empty in
  s
  >> list_fold add_var b params
  >> list_fold instr_bound_vars b instr
  >> last_bound_vars b last

(****)


module G =
  Dgraph.Make
    (struct type t = int let compare (x : int) y = compare x y end)

let forward_graph (_, blocks, _) =
  let d =
    Util.IntMap.fold (fun x _ s -> G.NSet.add x s)
      blocks G.NSet.empty in
  { G.domain = d;
    G.fold_children = fun f x a -> Code.fold_children blocks x f a }

module D = struct
  type t = VarSet.t * VarSet.t
  let equal (_, x) (_, y) = VarSet.equal x y
  let bot = (VarSet.empty, VarSet.empty)
end

let propagate g bound_vars s x =
  let a =
    g.G.fold_children
      (fun y a -> VarSet.union (snd (G.NMap.find y s)) a) x VarSet.empty
  in
  (a, VarSet.union (Util.IntMap.find x bound_vars) a)

module Solver = G.Solver (D)

let solver ((_, blocks, _) as p) =
  let g = forward_graph p in
  let bound_vars =
    Util.IntMap.map (fun b -> block_bound_vars b VarSet.empty) blocks in
  G.NMap.map fst (Solver.f (G.invert g) (propagate g bound_vars))

(****)

let rec traverse pc visited blocks bound_vars free_vars =
  if not (Util.IntSet.mem pc visited) then begin
    let visited = Util.IntSet.add pc visited in
    let block = Util.IntMap.find pc blocks in
    let bound_vars = block_bound_vars block bound_vars in
    let free_vars = block_free_vars pc bound_vars block free_vars in
    let (visited, free_vars) =
      Code.fold_children blocks pc
        (fun pc (visited, free_vars) ->
           let (visited, free_vars) =
             traverse pc visited blocks bound_vars free_vars in
           (visited, free_vars))
        (visited, free_vars)
    in
    (visited, free_vars)
  end else
    (visited, free_vars)

let f ((pc, blocks, free_pc) as p) =
  let ctx = ref Util.IntMap.empty in
  let cont_bound_vars = solver p in
  closure_free_vars :=
    (fun pc params (pc', _) ->
       let bound_vars = list_fold add_var VarSet.empty params VarSet.empty in
       let (_, free_vars) =
         traverse pc' Util.IntSet.empty blocks bound_vars VarSet.empty in
       let cvars = VarSet.inter (G.NMap.find pc cont_bound_vars) free_vars in
(*
if not (VarSet.is_empty cvars) then begin
Format.eprintf "@[<2>Should protect at %d:@ %a@]@."
pc Code.print_var_list (VarSet.elements cvars);
(*
Format.eprintf "@[<2>%a@]@."
Code.print_var_list (VarSet.elements (G.NMap.find pc cont_bound_vars));
Format.eprintf "@[<2>%a@]@."
Code.print_var_list (VarSet.elements free_vars);
*)
end;
*)
       ctx := Util.IntMap.add pc' cvars !ctx;
       free_vars);
  let free_vars = !closure_free_vars pc [] (pc, []) in
(*
Format.eprintf "@[<2>Global free variables:@ %a@]@."
Code.print_var_list (VarSet.elements free_vars);
*)
  ignore free_vars;
  !ctx

(*
- Find loop entry points
- Reverse block pointers
- Traverse from entry point

- Find loop blocks ==> below a back branch (or in subloop...)
- Bound variables in loops
*)

