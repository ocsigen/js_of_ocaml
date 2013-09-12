open Code

let static_eval_disabled = Util.disabled "static_eval"
let propagate_constant_disabled = Util.disabled "constant"

let eval_prim x =
  if static_eval_disabled ()
  then None
  else
    let bool b = Some (Int (if b then 1 else 0)) in
    match x with
      | Not, [Pc (Int i)] -> bool (i=0)
      | Lt,  [Pc (Int i);Pc (Int j) ] -> bool (i < j)
      | Le,  [Pc (Int i);Pc (Int j) ] -> bool (i <= j)
      | Eq,  [Pc (Int i);Pc (Int j) ] -> bool (i = j)
      | Neq, [Pc (Int i);Pc (Int j) ] -> bool (i <> j)
      | IsInt, [Pc (Int _)] -> bool (true)
      | Ult, [Pc (Int i);Pc (Int j) ] -> bool (j < 0 || i < j)
      | WrapInt, [Pc (Int i)] -> Some (Int i)
      | Extern name, l ->
        let name = Primitive.resolve name in
        let module Int = Int32 in
        let int2 = match l with
          | [Pc (Int i); Pc (Int j)] -> fun f -> (try Some (Int (Int.to_int (f (Int.of_int i) (Int.of_int j)))) with _ -> None)
          | _ -> fun _ -> None in
        let int2_1 = match l with
          | [Pc (Int i); Pc (Int j)] -> fun f -> (try Some (Int (Int.to_int (f (Int.of_int i) j))) with _ -> None)
          | _ -> fun _ -> None in
        let f2_aux =
          try
            let i,j = match l with
              | [Pc (Float i); Pc (Float j)]-> i,j
              | [Pc (Int i) ; Pc (Int j)] -> float_of_int i,float_of_int j
              | [Pc (Int i) ; Pc (Float j)] -> float_of_int i,j
              | [Pc (Float i) ; Pc (Int j)] -> i,float_of_int j
              | _ -> raise Not_found
            in
            fun f -> (try Some (f i j) with _ -> None)
          with _ -> fun _ -> None in
        let f2 f = f2_aux (fun i j -> Float (f i j)) in
        let f1 = match l with
          | [Pc (Float i)] -> fun f -> (try Some (Float (f i)) with _ -> None)
          | [Pc (Int i)] -> fun f -> (try Some (Float (f (float_of_int i))) with _ -> None)
          | _ -> fun _ -> None in
        let f2b f = f2_aux (fun i j -> Int (if f i j then 1 else 0)) in
        (match name, l with
          (* int *)
          | "%int_add", _ -> int2 (Int.add)
          | "%int_sub", _ -> int2 (Int.sub)
          | "%direct_int_mul", _ -> int2 (Int.mul )
          | "%direct_int_div", _ -> int2 (Int.div)
          | "%direct_int_mod", _ -> int2 (Int.rem)
          | "%int_and", _ -> int2 (Int.logand)
          | "%int_or", _  -> int2 (Int.logor)
          | "%int_xor", _ -> int2 (Int.logxor)
          | "%int_lsl", _ -> int2_1 (Int.shift_left)
          | "%int_lsr", _ -> int2_1 (Int.shift_right_logical)
          | "%int_asr", _ -> int2_1 (Int.shift_right)
          | "%int_neg", [Pc (Int i)] -> Some (Int (Int.to_int (Int.neg (Int.of_int i) )))
          (* float *)
          | "caml_eq_float", _ -> f2b (=)
          | "caml_neq_float", _ -> f2b (<>)
          | "caml_ge_float", _ -> f2b (>=)
          | "caml_le_float", _ -> f2b (<=)
          | "caml_gt_float", _ -> f2b (>)
          | "caml_lt_float", _ -> f2b (<)
          | "caml_add_float",_ -> f2 (+.)
          | "caml_sub_float",_ -> f2 (-.)
          | "caml_mul_float",_ -> f2 ( *. )
          | "caml_div_float",_ -> f2 ( /. )
          | "caml_fmod_float",_ -> f2 mod_float
          | "caml_int_of_float",[Pc (Float f)] -> Some (Int (int_of_float f))
          | "to_int",[Pc (Float f)]  -> Some (Int (int_of_float f))
          | "to_int",[Pc (Int i)] -> Some (Int i)
          (* Math *)
          | "caml_abs_float",_ -> f1 abs_float
          | "caml_acos_float",_ -> f1 acos
          | "caml_asin_float",_ -> f1 asin
          | "caml_atan_float",_ -> f1 atan
          | "caml_atan2_float",_ -> f2 atan2
          | "caml_ceil_float",_ -> f1 ceil
          | "caml_cos_float",_ -> f1 cos
          | "caml_exp_float",_ -> f1 exp
          | "caml_floor_float",_ -> f1 floor
          | "caml_log_float",_ -> f1 log
          | "caml_power_float",_ -> f2 ( ** )
          | "caml_sin_float",_ -> f1 sin
          | "caml_sqrt_float",_ -> f1 sqrt
          | "caml_tan_float",_ -> f1 tan
          (* other *)
          | ("caml_js_equals"|"caml_equal"), [Pc c1;Pc c2] -> bool (c1 = c2)
          | ("caml_js_equals"|"caml_equal"), [Pv x1;Pv x2] when x1 = x2 -> bool true
          | _ -> None)
      | _ -> None


let propagate constants defs blocks free_pc pc =
  let block = AddrMap.find pc blocks in
  let body,constants = List.fold_left (fun (acc,constants) i ->
      match i with
        | Let (x,Prim (prim, prim_args))  ->
          let prim_args = List.map (function
              | Pv x' when VarMap.mem x' constants -> Pc (VarMap.find x' constants)
              | x -> x) prim_args in
          let exp,constants = match eval_prim (prim,prim_args) with
            | Some c ->
              let constants =
                if defs.(Var.idx x) = 1
                then VarMap.add x c constants
                else constants in
              Constant c, constants
            | _ ->
              (* if List.for_all (function Pc _ -> true | Pv _ -> false) prim_args *)
              (* then (match prim with *)
              (*   | Extern name -> Format.eprintf "%s(%d)@." name (List.length prim_args) *)
              (*   | _ -> ()); *)
              Prim (prim, prim_args),constants in
          (Let (x,exp)::acc),constants
        | Let (x,Field(y,n)) when VarMap.mem y constants ->
          begin
            match VarMap.find y constants with
              | Tuple (_,tup) ->
                let c = tup.(n) in
                let constants =
                  if defs.(Var.idx x) = 1
                  then VarMap.add x c constants
                  else constants in
                Let (x, Constant c)::acc,constants
              | _ -> (Let(x,Field(y,n)))::acc, constants
          end
        | x -> (x::acc),constants
    ) ([],constants) block.body in
  let body = List.rev body in
  (* simplify branch *)
  let branch = match block.branch with
    | Cond (cond,x,ftrue,ffalse) when VarMap.mem x constants ->
      let res = match cond, VarMap.find x constants with
        | IsTrue, Int 1 -> true
        | IsTrue, Int 0 -> false
        | CEq i, Int j -> i = j
        | CLt i, Int j -> i < j
        | CLe i, Int j -> i<= j
        | CUlt i, Int j -> j < 0 || i < j
        | _ -> assert false in
      (match res with
        | true -> Branch ftrue
        | false -> Branch ffalse)
    | b -> b in
  let blocks = AddrMap.add pc {block with body;branch} blocks in
  blocks, free_pc, constants

let rec is_mutable = function
  | String _
  | Float_array _ -> true
  | Tuple (_,arr) ->
    for i = 0 to Array.length arr do
      ignore(not (is_mutable arr.(i)) || raise Not_found)
    done;
    false
  | _ -> false

let is_mutable x =
  try is_mutable x with _ -> true

let get_constant (_, blocks, _) defs =
  AddrMap.fold
    (fun _ block constants ->
      List.fold_left
        (fun constants i ->
          match i with
            | Let (x, Const i) when defs.(Var.idx x) = 1 ->
              VarMap.add x (Int i) constants
            | Let (x, Constant c) when not (is_mutable c) && defs.(Var.idx x) = 1 ->
              VarMap.add x c constants
            | _ -> constants)
        constants block.body)
    blocks VarMap.empty


let f ((pc,blocks,free_pc) as p) defs =
  if propagate_constant_disabled ()
  then p
  else
    let constants = get_constant p defs in
    let blocks,free_pc,_ =
      AddrMap.fold
        (fun pc _ (blocks, free_pc,constants) ->
          propagate constants defs blocks free_pc pc)
        blocks
        (blocks, free_pc,constants)
    in (pc,blocks,free_pc)
