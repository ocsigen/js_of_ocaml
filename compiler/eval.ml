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
      let int_binop = match l with
        | [Int i; Int j] -> fun f -> Some (Int (Int.to_int (f (Int.of_int i) (Int.of_int j))))
        | _ -> fun _ -> None in
      let shift = match l with
        | [Int i; Int j] -> fun f -> Some (Int (Int.to_int (f (Int.of_int i) (j land 0x1f))))
        | _ -> fun _ -> None in
      let float_binop_aux =
        let args = match l with
          | [Float i; Float j]-> Some (i,j)
          | [Int i ; Int j] -> Some (float_of_int i,float_of_int j)
          | [Int i ; Float j] -> Some(float_of_int i,j)
          | [Float i ; Int j] -> Some(i,float_of_int j)
          | _ -> None
        in
        match args with
          | None -> (fun _ -> None)
          | Some (i,j) -> fun f -> Some (f i j) in
      let float_binop f = float_binop_aux (fun i j -> Float (f i j)) in
      let float_unop = match l with
        | [Float i] -> fun f -> Some (Float (f i))
        | [Int i] -> fun f -> Some (Float (f (float_of_int i)))
        | _ -> fun _ -> None in
      let float_binop_bool f = float_binop_aux (fun i j -> Int (if f i j then 1 else 0)) in
      (match name, l with
        (* int *)
        | "%int_add", _ -> int_binop (Int.add)
        | "%int_sub", _ -> int_binop (Int.sub)
        | "%direct_int_mul", _ -> int_binop (Int.mul )
        | "%direct_int_div", [_; Int 0] -> None
        | "%direct_int_div", _ -> int_binop (Int.div)
        | "%direct_int_mod", _ -> int_binop (Int.rem)
        | "%int_and", _ -> int_binop (Int.logand)
        | "%int_or", _  -> int_binop (Int.logor)
        | "%int_xor", _ -> int_binop (Int.logxor)
        | "%int_lsl", _ -> shift (Int.shift_left)
        | "%int_lsr", _ -> shift (Int.shift_right_logical)
        | "%int_asr", _ -> shift (Int.shift_right)
        | "%int_neg", [Int i] -> Some (Int (Int.to_int (Int.neg (Int.of_int i) )))
        (* float *)
        | "caml_eq_float", _ -> float_binop_bool (=)
        | "caml_neq_float", _ -> float_binop_bool (<>)
        | "caml_ge_float", _ -> float_binop_bool (>=)
        | "caml_le_float", _ -> float_binop_bool (<=)
        | "caml_gt_float", _ -> float_binop_bool (>)
        | "caml_lt_float", _ -> float_binop_bool (<)
        | "caml_add_float",_ -> float_binop (+.)
        | "caml_sub_float",_ -> float_binop (-.)
        | "caml_mul_float",_ -> float_binop ( *. )
        | "caml_div_float",_ -> float_binop ( /. )
        | "caml_fmod_float",_ -> float_binop mod_float
        | "caml_int_of_float",[Float f] -> Some (Int (int_of_float f))
        | "to_int",[Float f]  -> Some (Int (int_of_float f))
        | "to_int",[Int i] -> Some (Int i)
          (* Math *)
        | "caml_abs_float",_ -> float_unop abs_float
        | "caml_acos_float",_ -> float_unop acos
        | "caml_asin_float",_ -> float_unop asin
        | "caml_atan_float",_ -> float_unop atan
        | "caml_atan2_float",_ -> float_binop atan2
        | "caml_ceil_float",_ -> float_unop ceil
        | "caml_cos_float",_ -> float_unop cos
        | "caml_exp_float",_ -> float_unop exp
        | "caml_floor_float",_ -> float_unop floor
        | "caml_log_float",_ -> float_unop log
        | "caml_power_float",_ -> float_binop ( ** )
        | "caml_sin_float",_ -> float_unop sin
        | "caml_sqrt_float",_ -> float_unop sqrt
        | "caml_tan_float",_ -> float_unop tan
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
    | Let (x,Prim (prim, prim_args)) ->
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
              (* this produce invalid code. why ??? *)
              (* | Some c -> Pc c *)
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
