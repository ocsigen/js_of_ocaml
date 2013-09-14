open Code
open Flow

let the_def_of info x =
  match x with
    | Pv x ->
      get_approx info
        (fun x -> match info.info_defs.(Var.idx x) with Expr e -> Some e | _ -> None)
        None (fun u v -> None) x
    | Pc c -> Some (Constant c)

let the_int info x =
  match x with
    | Pv x ->
      get_approx info
        (fun x -> match info.info_defs.(Var.idx x) with Expr (Const i) -> Some i | _ -> None)
        None
        (fun u v -> match u, v with Some i, Some j when i = j -> u | _ -> None)
        x
    | Pc (Int i) -> Some i
    | _ -> None


let specialize_instr info i =
  match i with

  | Let (x, Prim (Extern "caml_js_var", [y])) ->
      begin match the_def_of info y with
        Some (Constant (String _ as c)) ->
          Let (x, Prim (Extern "caml_js_var", [Pc c]))
      | _ ->
          i
      end
  | Let (x, Prim (Extern "caml_js_const", [y])) ->
      begin match the_def_of info y with
        Some (Constant (String _ as c)) ->
          Let (x, Prim (Extern "caml_js_const", [Pc c]))
      | _ ->
          i
      end
  | Let (x, Prim (Extern "caml_js_call", [f; o; a])) ->
      begin match the_def_of info a with
        Some (Block (_, a)) ->
          let a = Array.map (fun x -> Pv x) a in
          Let (x, Prim (Extern "caml_js_opt_call",
                        f :: o :: Array.to_list a))
      | _ ->
          i
      end
  | Let (x, Prim (Extern "caml_js_fun_call", [f; a])) ->
      begin match the_def_of info a with
        Some (Block (_, a)) ->
          let a = Array.map (fun x -> Pv x) a in
          Let (x, Prim (Extern "caml_js_opt_fun_call",
                        f :: Array.to_list a))
      | _ ->
          i
      end
  | Let (x, Prim (Extern "caml_js_meth_call", [o; m; a])) ->
      begin match the_def_of info m with
        Some (Constant (String _ as m)) ->
          begin match the_def_of info a with
            Some (Block (_, a)) ->
              let a = Array.map (fun x -> Pv x) a in
              Let (x, Prim (Extern "caml_js_opt_meth_call",
                            o :: Pc m :: Array.to_list a))
          | _ ->
              i
          end
      | _ ->
          i
      end
  | Let (x, Prim (Extern "caml_js_new", [c; a])) ->
      begin match the_def_of info a with
        Some (Block (_, a)) ->
          let a = Array.map (fun x -> Pv x) a in
          Let (x, Prim (Extern "caml_js_opt_new",
                        c :: Array.to_list a))
      | _ ->
          i
      end
  | Let (x, Prim (Extern "caml_js_object", [a])) ->
      begin try
        let a =
          match the_def_of info a with
            Some (Block (_, a)) -> a
          | _                   -> raise Exit
        in
        let a =
          Array.map
            (fun x ->
               match the_def_of info (Pv x) with
                 Some (Block (_, [|k; v|])) ->
                   let k =
                     match the_def_of info (Pv k) with
                       Some (Constant (String _ as s)) -> Pc s
                     | _                               -> raise Exit
                   in
                   [k; Pv v]
               | _ ->
                   raise Exit)
            a
        in
        Let (x, Prim (Extern "caml_js_opt_object",
                      List.flatten (Array.to_list a)))
      with Exit ->
        i
      end
  | Let (x, Prim (Extern "caml_js_get", [o; f])) ->
      begin match the_def_of info f with
        Some (Constant (String _ as c)) ->
          Let (x, Prim (Extern "caml_js_get", [o; Pc c]))
      | _ ->
          i
      end
  | Let (x, Prim (Extern "caml_js_set", [o; f; v])) ->
      begin match the_def_of info f with
        Some (Constant (String _ as c)) ->
          Let (x, Prim (Extern "caml_js_set", [o; Pc c; v]))
      | _ ->
          i
      end
  | Let (x, Prim (Extern "caml_js_delete", [o; f])) ->
      begin match the_def_of info f with
        Some (Constant (String _ as c)) ->
          Let (x, Prim (Extern "caml_js_delete", [o; Pc c]))
      | _ ->
          i
      end
  | Let (x, Prim (Extern "%int_mul", [y; z])) ->
      begin match the_int info y, the_int info z with
        Some j, _ | _, Some j when abs j < 0x200000 ->
          Let (x, Prim (Extern "%direct_int_mul", [y; z]))
      | _ ->
          i
      end
  | Let (x, Prim (Extern "%int_div", [y; z])) ->
      begin match the_int info z with
        Some j when j <> 0 ->
          Let (x, Prim (Extern "%direct_int_div", [y; z]))
      | _ ->
          i
      end
  | Let (x, Prim (Extern "%int_mod", [y; z])) ->
      begin match the_int info z with
        Some j when j <> 0 ->
          Let (x, Prim (Extern "%direct_int_mod", [y; z]))
      | _ ->
          i
      end
  | _ -> i

let specialize_instrs info (pc, blocks, free_pc) =
  let blocks =
    AddrMap.map
      (fun block ->
         { block with Code.body =
             List.map (fun i -> specialize_instr info i) block.body })
      blocks
  in
  (pc, blocks, free_pc)

(****)

let f p info =
  let p = specialize_instrs info p in
  p
