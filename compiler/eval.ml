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

open Code
open Flow


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
  | IsInt, [Int _] -> bool true
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


let eval_instr info i =
  match i with
    | Let (x, Prim (Extern ("caml_js_equals"|"caml_equal"), [y;z])) ->
      begin match the_const_of info y, the_const_of info z with
        | Some e1, Some e2 ->
          let c =
            if e1 = e2
            then 1l
            else 0l in
          Let (x , Constant (Int c))
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
        | Some c -> Let(x,Constant (Int c)))
    | Let (x,Prim (prim, prim_args)) ->
      begin
        let prim_args' = List.map (fun x -> the_const_of info x) prim_args in
        let res =
          if List.for_all (function Some _ -> true | _ -> false) prim_args'
          then eval_prim (prim,List.map (function Some c -> c | None -> assert false) prim_args')
          else None in
        match res with
          | Some c -> Let (x,Constant c)
          | _ -> Let(x, Prim(prim, (List.map2 (fun arg c ->
            match c with
              | Some ((Int _ | Float _) as c) -> Pc c
              | Some _ (* do not be duplicated other constant as
                          they're not represented with constant in javascript. *)
              | None -> arg) prim_args prim_args')))
      end
    | _ -> i

type case_of = CConst of int | CTag of int | N

let the_case_of info x =
  match x with
    | Pv x ->
      get_approx info
        (fun x -> match info.info_defs.(Var.idx x) with
                  | Expr (Const i)
                  | Expr (Constant (Int i)) -> CConst (Int32.to_int i)
                  | Expr (Block (j,_))
                  | Expr (Constant (Tuple (j,_))) -> CTag j
                  | _ -> N)
        N
        (fun u v -> match u, v with
           | CTag i, CTag j when i = j -> u
           | CConst i, CConst j when i = j -> u
           | _ -> N)
        x
    | Pc (Int i) -> CConst (Int32.to_int i)
    | Pc (Tuple (j,_)) -> CTag j
    | _ -> N


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
      match the_case_of info (Pv x) with
      | CConst j -> Branch const.(j)
      | CTag j -> Branch tags.(j)
      | N -> b
    end
  | _ as b -> b



let f info (pc, blocks, free_pc) =
  let blocks =
    AddrMap.map
      (fun block ->
         { block with
           Code.body =
             List.map (eval_instr info) block.body;
           Code.branch = eval_branch info block.branch
         })
      blocks
  in
  (pc, blocks, free_pc)
