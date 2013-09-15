open Code
open Flow

let eval_prim x =
  let bool b = Some (Int (if b then 1 else 0)) in
  match x with
    | Not, [Int i] -> bool (i=0)
    | Lt,  [Int i; Int j ] -> bool (i < j)
    | Le,  [Int i; Int j ] -> bool (i <= j)
    | Eq,  [Int i; Int j ] -> bool (i = j)
    | Neq, [Int i; Int j ] -> bool (i <> j)
    | IsInt, [Int _] -> bool true
    | Ult, [Int i; Int j ] -> bool (j < 0 || i < j)
    | WrapInt, [Int i] -> Some (Int i)
    | Extern name, l ->
      let name = Primitive.resolve name in
      let module Int = Int32 in
      let int2 = match l with
        | [Int i; Int j] -> fun f ->
          (try Some (Int (Int.to_int (f (Int.of_int i) (Int.of_int j)))) with _ -> None)
        | _ -> fun _ -> None in
      let int2_1 = match l with
        | [Int i; Int j] -> fun f ->
          (try Some (Int (Int.to_int (f (Int.of_int i) j))) with _ -> None)
        | _ -> fun _ -> None in
      let f2_aux =
        try
          let i,j = match l with
            | [Float i; Float j]-> i,j
            | [Int i ; Int j] -> float_of_int i,float_of_int j
            | [Int i ; Float j] -> float_of_int i,j
            | [Float i ; Int j] -> i,float_of_int j
            | _ -> raise Not_found
          in
          fun f -> (try Some (f i j) with _ -> None)
        with _ -> fun _ -> None in
      let f2 f = f2_aux (fun i j -> Float (f i j)) in
      let f1 = match l with
        | [Float i] -> fun f -> (try Some (Float (f i)) with _ -> None)
        | [Int i] -> fun f -> (try Some (Float (f (float_of_int i))) with _ -> None)
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
        | "%int_neg", [Int i] -> Some (Int (Int.to_int (Int.neg (Int.of_int i) )))
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
        | "caml_int_of_float",[Float f] -> Some (Int (int_of_float f))
        | "to_int",[Float f]  -> Some (Int (int_of_float f))
        | "to_int",[Int i] -> Some (Int i)
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
        | _ -> None)
    | _ -> None

exception Not_constant

let eval_instr info live i =
  match i with
    | Let (x, Prim (Extern ("caml_js_equals"|"caml_equal"), [Pv y; Pv z])) when Var.compare y z = 0 ->
      Let (x , Constant (Int 1))
    | Let (x, Prim (Extern ("caml_js_equals"|"caml_equal"), [y;z])) ->
      begin match the_def_of info y, the_def_of info z with
        | Some (Constant e1), Some (Constant e2) ->
          let c =
            if e1 = e2
            then 1
            else 0 in
          Let (x , Constant (Int c))
        | _ -> i
      end
    | Let (x,Prim (Extern ("caml_js_from_string"), [y])) ->
      begin match the_def_of info y with
        | Some (Constant (String str)) ->
          begin match y with
            | Pv y when live.(Var.idx y) <= 1 ->
              Let(x,(Constant (IString str)))
            | Pc _ ->
              Let(x, (Constant (IString str)))
            | _ -> i
          end
        | _ -> i
      end
    | Let (x,Prim (prim, prim_args)) when false ->
      begin
        let prim_args' = List.map (fun x ->
          match the_def_of info x with
            | Some (Constant c) -> Some c
            | Some (Const i) -> Some (Int i)
            | _ -> None) prim_args in
        let res =
          if List.for_all (function Some _ -> true | _ -> false) prim_args'
          then eval_prim (prim,List.map (function Some c -> c | None -> assert false) prim_args')
          else None in
        match res with
          | Some c -> Let (x,Constant c)
          | _ -> Let(x, Prim(prim, (List.map2 (fun arg c ->
            match c with
              | Some c -> Pc c
              | _ -> arg) prim_args prim_args')))
      end
    | _ -> i

let eval_branch info = function
  | Cond (cond,x,ftrue,ffalse) as b->
    begin
      match the_int info (Pv x) with
        | Some j ->
          let res = match cond with
            | IsTrue -> (match j with 0 -> false | 1 -> true | _ -> assert false)
            | CEq i -> i = j
            | CLt i -> i < j
            | CLe i -> i<= j
            | CUlt i -> j < 0 || i < j
          in
          (match res with
            | true -> Branch ftrue
            | false -> Branch ffalse)
        | _ -> b
    end
  | _ as b -> b



let f info live (pc, blocks, free_pc) =
  let blocks =
    AddrMap.map
      (fun block ->
         { block with
           Code.body =
             List.map (eval_instr info live) block.body;
           Code.branch = eval_branch info block.branch
         })
      blocks
  in
  (pc, blocks, free_pc)
