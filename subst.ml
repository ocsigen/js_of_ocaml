open Code

let expr s e =
  match e with
    Const _ | Constant _ ->
      e
  | Apply (f, l) ->
      Apply (s f, List.map (fun x -> s x) l)
  | Direct_apply (f, l) ->
      Direct_apply (s f, List.map (fun x -> s x) l)
  | Block (n, a) ->
      Block (n, Array.map (fun x -> s x) a)
  | Field (x, n) ->
      Field (s x, n)
  | Closure (l, pc) ->
      Closure (l, pc)
  | Prim (p, l) ->
      Prim (p, List.map (fun x -> s x) l)
  | Variable x ->
      Variable (s x)

let instr s i =
  match i with
    Let (x, e) ->
      Let (x, expr s e)
  | Set_field (x, n, y) ->
      Set_field (s x, n, s y)
  | Offset_ref (x, n) ->
      Offset_ref (s x, n)
  | Array_set (x, y, z) ->
      Array_set (s x, s y, s z)

let instrs s l = List.map (fun i -> instr s i) l

let subst_cont s (pc, arg) = (pc, List.map (fun x -> s x) arg)

let last s l =
  match l with
    Stop ->
      l
  | Branch cont ->
      Branch (subst_cont s cont)
  | Pushtrap (cont1, x, cont2, pc) ->
      Pushtrap (subst_cont s cont1, x, subst_cont s cont2, pc)
  | Return x ->
      Return (s x)
  | Raise x ->
      Raise (s x)
  | Cond (c, x, cont1, cont2) ->
      Cond (c, s x, subst_cont s cont1, subst_cont s cont2)
  | Switch (x, a1, a2) ->
      Switch (s x,
              Array.map (fun cont -> subst_cont s cont) a1,
              Array.map (fun cont -> subst_cont s cont) a2)
  | Poptrap cont ->
      Poptrap (subst_cont s cont)

let program s (pc, blocks, free_pc) =
  let blocks =
    Util.IntMap.map
      (fun (params, istrs, lst) -> params, instrs s istrs, last s lst) blocks
  in
  (pc, blocks, free_pc)

(****)

let from_array s =
  fun x -> match s.(Var.idx x) with Some y -> y | None -> x

(****)

module VarMap = Map.Make (Code.Var)

let rec build_mapping params args =
  match params, args with
    x :: params, y :: args ->
      VarMap.add x y (build_mapping params args)
  | [], _ ->
      VarMap.empty
  | _ ->
      assert false

let from_map m =
  fun x -> try VarMap.find x m with Not_found -> x
