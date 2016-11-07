(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2013 Hugo Heuzard
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

module Primitive = Jsoo_primitive
open Code
open Flow

let static_env = Hashtbl.create 17
let clear_static_env () = Hashtbl.clear static_env
let set_static_env s value = Hashtbl.add static_env s value
let get_static_env s = try Some (Hashtbl.find static_env s) with Not_found -> None

module Int = Int32
let int_binop l f = match l with
  | [Int i; Int j] -> Some (Int (f i j))
  | _ -> None
let shift l f = match l with
  | [Int i; Int j] -> Some (Int (f i ((Int32.to_int j) land 0x1f)))
  | _ -> None
let float_binop_aux l f =
  let args = match l with
    | [Float i; Float j]-> Some (i,j)
    | [Int i ; Int j] -> Some (Int32.to_float i,Int32.to_float j)
    | [Int i ; Float j] -> Some(Int32.to_float i,j)
    | [Float i ; Int j] -> Some(i,Int32.to_float j)
    | _ -> None in
  match args with
  | None -> None
  | Some (i,j) -> Some (f i j)

let float_binop l f = match float_binop_aux l f with
  | Some x -> Some (Float x)
  | None -> None

let float_unop l f = match l with
  | [Float i] -> Some (Float (f i))
  | [Int i] -> Some (Float (f (Int32.to_float i)))
  | _ -> None

let float_binop_bool l f = match float_binop_aux l f with
  | Some true -> Some (Int 1l)
  | Some false -> Some (Int 0l)
  | None -> None

let bool b = Some (Int (if b then 1l else 0l))

let eval_prim x =
  match x with
  | Not, [Int i] -> bool (i=0l)
  | Lt,  [Int i; Int j ] -> bool (i < j)
  | Le,  [Int i; Int j ] -> bool (i <= j)
  | Eq,  [Int i; Int j ] -> bool (i = j)
  | Neq, [Int i; Int j ] -> bool (i <> j)
  | Ult, [Int i; Int j ] -> bool (j < 0l || i < j)
  | Extern name, l ->
    let name = Primitive.resolve name in
    (match name, l with
     (* int *)
     | "%int_add", _ -> int_binop l (Int.add)
     | "%int_sub", _ -> int_binop l (Int.sub)
     | "%direct_int_mul", _ -> int_binop l (Int.mul )
     | "%direct_int_div", [_; Int 0l] -> None
     | "%direct_int_div", _ -> int_binop l (Int.div)
     | "%direct_int_mod", _ -> int_binop l (Int.rem)
     | "%int_and", _ -> int_binop l (Int.logand)
     | "%int_or", _  -> int_binop l (Int.logor)
     | "%int_xor", _ -> int_binop l (Int.logxor)
     | "%int_lsl", _ -> shift l (Int.shift_left)
     | "%int_lsr", _ -> shift l (Int.shift_right_logical)
     | "%int_asr", _ -> shift l (Int.shift_right)
     | "%int_neg", [Int i] -> Some (Int (Int.neg i ))
     (* float *)
     | "caml_eq_float", _ -> float_binop_bool l (=)
     | "caml_neq_float", _ -> float_binop_bool l (<>)
     | "caml_ge_float", _ -> float_binop_bool l (>=)
     | "caml_le_float", _ -> float_binop_bool l (<=)
     | "caml_gt_float", _ -> float_binop_bool l (>)
     | "caml_lt_float", _ -> float_binop_bool l (<)
     | "caml_add_float",_ -> float_binop l (+.)
     | "caml_sub_float",_ -> float_binop l (-.)
     | "caml_mul_float",_ -> float_binop l ( *. )
     | "caml_div_float",_ -> float_binop l ( /. )
     | "caml_fmod_float",_ -> float_binop l mod_float
     | "caml_int_of_float",[Float f] -> Some (Int (Int32.of_float f))
     | "to_int",[Float f]  -> Some (Int (Int32.of_float f))
     | "to_int",[Int i] -> Some (Int i)
     (* Math *)
     | "caml_abs_float",_ -> float_unop l abs_float
     | "caml_acos_float",_ -> float_unop l acos
     | "caml_asin_float",_ -> float_unop l asin
     | "caml_atan_float",_ -> float_unop l atan
     | "caml_atan2_float",_ -> float_binop l atan2
     | "caml_ceil_float",_ -> float_unop l ceil
     | "caml_cos_float",_ -> float_unop l cos
     | "caml_exp_float",_ -> float_unop l exp
     | "caml_floor_float",_ -> float_unop l floor
     | "caml_log_float",_ -> float_unop l log
     | "caml_power_float",_ -> float_binop l ( ** )
     | "caml_sin_float",_ -> float_unop l sin
     | "caml_sqrt_float",_ -> float_unop l sqrt
     | "caml_tan_float",_ -> float_unop l tan


     | "caml_string_equal", [String s1; String s2] ->
       bool (s1 = s2)
     | "caml_string_notequal", [String s1; String s2] ->
       bool (s1 <> s2)
     | "caml_sys_getenv", [String s] ->
       begin match get_static_env s with
       | Some env ->
         Some (String env)
       | None -> None
       end
     | "caml_sys_const_word_size" , [_] -> Some (Int 32l)
     | "caml_sys_const_int_size"  , [_] -> Some (Int 32l)
     | "caml_sys_const_big_endian", [_] -> Some (Int 0l)


     | _ -> None)
  | _ -> None

exception Not_constant

let the_length_of info x =
  get_approx info
    (fun x ->
       match info.info_defs.(Var.idx x) with
         | Expr (Constant (String s))
         | Expr (Constant (IString s)) -> Some (Int32.of_int (String.length s))
         | Expr (Prim (Extern "caml_create_string",[arg])) ->
           the_int info arg
         | _ -> None)
    None
    (fun u v -> match u,v with
       | Some l, Some l' when l = l' -> Some l
       | _ -> None)
    x

type is_int = Y | N | Unknown

let is_int info x =
  match x with
  | Pv x ->
    get_approx info
      (fun x -> match info.info_defs.(Var.idx x) with
         | Expr (Const _)
         | Expr (Constant (Int _)) -> Y
         | Expr (Block (_,_))
         | Expr (Constant _) -> N
         | _ -> Unknown)
      Unknown
      (fun u v ->
         match u, v with
         | Y, Y      -> Y
         | N, N      -> N
         | _         -> Unknown
      )
      x
  | Pc (Int _) -> Y
  | Pc _ -> N

let eval_instr info i =
  match i with
    | Let (x, Prim (Extern ("caml_js_equals"|"caml_equal"), [y;z])) ->
      begin match the_const_of info y, the_const_of info z with
        | Some e1, Some e2 ->
          let c =
            if e1 = e2
            then 1l
            else 0l in
          let c = Constant (Int c) in
          Flow.update_def info x c;
          Let (x, c)
        | _ -> i
      end
    | Let (x,Prim (Extern "caml_ml_string_length", [s])) ->
      let c = match s with
        | Pc (String s)
        | Pc (IString s) -> Some (Int32.of_int (String.length s))
        | Pv v -> the_length_of info v
        | _ -> None
      in
      (match c with
        | None -> i
        | Some c ->
          let c = Constant (Int c) in
          Flow.update_def info x c;
          Let(x,c))
    | Let (_, Prim (Extern
                      ("caml_array_unsafe_get"|"caml_array_unsafe_set"), _)) ->
        (* Fresh parameters can be introduced for these primitives
           in Specialize_js, which would make the call to [the_const_of]
           below fail. *)
      i
    | Let (x,Prim (IsInt, [y])) ->
      begin
        match is_int info y with
        | Unknown -> i
        | Y | N as b ->
          let b = if b = N then 0l else 1l in
          let c = Constant (Int b) in
          Flow.update_def info x c;
          Let (x,c)
      end
    | Let (x,Prim (prim, prim_args)) ->
      begin
        let prim_args' = List.map (fun x -> the_const_of info x) prim_args in
        let res =
          if List.for_all (function Some _ -> true | _ -> false) prim_args'
          then eval_prim (prim,List.map (function Some c -> c | None -> assert false) prim_args')
          else None in
        match res with
          | Some c ->
            let c = Constant c in
            Flow.update_def info x c;
            Let (x,c)
          | _ -> Let(x, Prim(prim, (List.map2 (fun arg c ->
            match c with
              | Some ((Int _ | Float _) as c) -> Pc c
              | Some _ (* do not be duplicated other constant as
                          they're not represented with constant in javascript. *)
              | None -> arg) prim_args prim_args')))
      end
    | _ -> i

type case_of = CConst of int | CTag of int | Unknown

let the_case_of info x =
  match x with
    | Pv x ->
      get_approx info
        (fun x -> match info.info_defs.(Var.idx x) with
                  | Expr (Const i)
                  | Expr (Constant (Int i))       -> CConst (Int32.to_int i)
                  | Expr (Block (j,_))            ->
                    if info.info_possibly_mutable.(Var.idx x)
                    then Unknown
                    else  CTag j
                  | Expr (Constant (Tuple (j,_))) -> CTag j
                  | _ -> Unknown)
        Unknown
        (fun u v -> match u, v with
           | CTag i, CTag j when i = j -> u
           | CConst i, CConst j when i = j -> u
           | _ -> Unknown)
        x
    | Pc (Int i) -> CConst (Int32.to_int i)
    | Pc (Tuple (j,_)) -> CTag j
    | _ -> Unknown


let eval_branch info = function
  | Cond (cond,x,ftrue,ffalse) as b->
    begin
      match the_int info (Pv x) with
        | Some j ->
          let res = match cond with
            | IsTrue -> (match j with 0l -> false | 1l -> true | _ -> assert false)
            | CEq i -> i = j
            | CLt i -> i < j
            | CLe i -> i<= j
            | CUlt i -> j < 0l || i < j
          in
          (match res with
            | true -> Branch ftrue
            | false -> Branch ffalse)
        | _ -> b
    end
  | Switch (x,const,tags) as b ->
    begin
      (* [the_case_of info (Pv x)] might be meaningless when we're inside a dead code.
         The proper fix would be to remove the deadcode entirely.
         Meanwhile, add guards to prevent Invalid_argument("index out of bounds")
         see https://github.com/ocsigen/js_of_ocaml/issues/485 *)
      match the_case_of info (Pv x) with
      | CConst j when j < Array.length const -> Branch const.(j)
      | CTag j   when j < Array.length tags  -> Branch tags.(j)
      | CConst _ | CTag _ | Unknown -> b
    end
  | _ as b -> b

exception May_raise

let rec do_not_raise pc visited blocks =
  if AddrSet.mem pc visited then visited
  else
  let visited = AddrSet.add pc visited in
  let b = AddrMap.find pc blocks in
  List.iter (function
    | Array_set (_,_,_)
    | Offset_ref (_,_)
    | Set_field (_,_,_) -> ()
    | Let (_, e) ->
      match e with
      | Const _
      | Block (_,_)
      | Field (_,_)
      | Constant _
      | Closure _ -> ()
      | Apply (_,_,_) -> raise May_raise
      | Prim (Extern name, _) when Jsoo_primitive.is_pure name -> ()
      | Prim (Extern _, _) -> raise May_raise
      | Prim (_,_) -> ()
  ) b.body;
  match b.branch with
  | Raise _ -> raise May_raise
  | Stop
  | Return _
  | Poptrap _ -> visited
  | Branch (pc,_) -> do_not_raise pc visited blocks
  | Cond (_,_,(pc1,_),(pc2,_)) ->
    let visited = do_not_raise pc1 visited blocks in
    let visited = do_not_raise pc2 visited blocks in
    visited
  | Switch (_, a1, a2) ->
    let visited = Array.fold_left (fun visited (pc,_) -> do_not_raise pc visited blocks) visited a1 in
    let visited = Array.fold_left (fun visited (pc,_) -> do_not_raise pc visited blocks) visited a2 in
    visited
  | Pushtrap _ -> raise May_raise

let drop_exception_handler blocks =
  AddrMap.fold (fun pc _ blocks ->
    match AddrMap.find pc blocks with
    | { branch = Pushtrap ((addr,_) as cont1,_x,_cont2,_); handler = parent_hander; _}  as b ->
      begin
        try
          let visited = do_not_raise addr AddrSet.empty blocks in
          let b = { b with branch = Branch cont1 } in
          let blocks = AddrMap.add pc b blocks in
          let blocks = AddrSet.fold (fun pc2 blocks ->
            let b = AddrMap.find pc2 blocks in
            assert(b.handler <> parent_hander);
            let branch =
              match b.branch with
              | Poptrap (cont,pushtrap) ->
                assert(pc = pushtrap);
                Branch cont
              | x -> x
            in
            let b = { b with branch; handler = parent_hander } in
            AddrMap.add pc2 b blocks
          ) visited blocks
          in
          blocks
        with May_raise -> blocks
      end
    | _ -> blocks) blocks blocks

let eval info blocks =
  AddrMap.map
    (fun block ->
       let body = List.map (eval_instr info) block.body in
       let branch = eval_branch info block.branch in
       { block with
         Code.body;
         Code.branch
       })
    blocks

let f info (pc, blocks, free_pc) =
  let blocks = eval info blocks in
  let blocks = drop_exception_handler blocks in
  (pc, blocks, free_pc)

