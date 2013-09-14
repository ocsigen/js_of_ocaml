open Code
open Flow

let function_cardinality info x =
  get_approx info
    (fun x ->
       match info.info_defs.(Var.idx x) with
         Expr (Closure (l, _)) -> Some (List.length l)
       | _                     -> None)
    None
    (fun u v -> match u, v with Some n, Some m when n = m -> u | _ -> None)
    x

let specialize_instr info i =
  match i with
    | Let (x, Apply (f, l, _)) when Option.Optim.optcall () ->
      Let (x, Apply (f, l, function_cardinality info f))
    | _ ->
      i

let specialize_instrs info (pc, blocks, free_pc) =
  let blocks =
    AddrMap.map
      (fun block ->
         { block with Code.body =
             List.map (fun i -> specialize_instr info i) block.body })
      blocks
  in
  (pc, blocks, free_pc)

let f p info =
  let p = specialize_instrs info p in
  p
