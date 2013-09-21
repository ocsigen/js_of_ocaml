(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
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

module J = Javascript



let eplus_int e1 e2 =
  match e2,e1 with
    J.ENum n, _ when n < 0. ->
      J.EBin (J.Minus, e1, J.ENum (-. n))
  | _,J.ENum n when n < 0. ->
      J.EBin (J.Minus, e2, J.ENum (-. n))
  | _ ->
      J.EBin (J.Plus, e1, e2)

let eminus_int e1 e2 =
  match e2,e1 with
    J.ENum n,_  when n < 0. ->
      J.EBin (J.Plus, e1, J.ENum (-. n))
  | _ ->
      J.EBin (J.Minus, e1, e2)

let rec enot_rec e =
  let (_, cost) as res =
    match e with
      J.ESeq (e1, e2) ->
        let (e2', cost) = enot_rec e2 in
        (J.ESeq (e1, e2'), cost)
    | J.ECond (e1, e2, e3) ->
        let (e2', cost2) = enot_rec e2 in
        let (e3', cost3) = enot_rec e3 in
        (J.ECond (e1, e2', e3'), cost2 + cost3)
    | J.EBin (op, e1, e2) ->
        begin match op with
          J.Or ->
            let (e1', cost1) = enot_rec e1 in
            let (e2', cost2) = enot_rec e2 in
            (J.EBin (J.And, e1', e2'), cost1 + cost2)
        | J.And ->
            let (e1', cost1) = enot_rec e1 in
            let (e2', cost2) = enot_rec e2 in
            (J.EBin (J.Or, e1', e2'), cost1 + cost2)
        | J.EqEq ->
            (J.EBin (J.NotEq, e1, e2), 0)
        | J.NotEq ->
            (J.EBin (J.EqEq, e1, e2), 0)
        | J.EqEqEq ->
            (J.EBin (J.NotEqEq, e1, e2), 0)
        | J.NotEqEq ->
            (J.EBin (J.EqEqEq, e1, e2), 0)
(*Disabled: this is not correct!
        | J.Lt ->
            (J.EBin (J.Le, e2, e1), 0)
        | J.Le ->
            (J.EBin (J.Lt, e2, e1), 0)
*)
        | _ ->
            (J.EUn (J.Not, e), 1)
        end
    | J.EUn (J.Not, e) ->
        (e, 0)
    | J.EUn ((J.Neg | J.Pl | J.Typeof | J.Void | J.Delete | J.Bnot ), _) ->
        (J.EUn (J.Not, e), 0)

    | J.EBool b ->
        (J.EBool (not b), 0)
    | J.ECall _ | J.EAccess _ | J.EDot _ | J.ENew _ | J.EVar _ | J.EFun _
    | J.EStr _ | J.EArr _ | J.ENum _ | J.EObj _ | J.EQuote _
    | J.EUn
        (( J.IncrA
         | J.IncrB
         | J.DecrA
         | J.DecrB ),_) ->
      (J.EUn (J.Not, e), 1)
  in
  if cost <= 1 then res else (J.EUn (J.Not, e), 1)

let enot e = fst (enot_rec e)

let source_elements l =
  (* FIX: not tail recursive *)
  List.fold_right
    (fun st rem ->
       match st, rem with
       | J.Variable_statement [addr, Some (J.EFun ((None, params, body), pc))], _ ->
         J.Function_declaration (addr, params, body, pc) :: rem
       | J.Variable_statement l1,
         J.Statement (J.Variable_statement l2) :: rem' ->
           J.Statement (J.Variable_statement (l1 @ l2)) :: rem'
       | _ ->
           J.Statement st :: rem)
    l []

let tr = function
  | J.Div -> J.SlashEq
  | J.Mod -> J.ModEq
  | J.Lsl -> J.LslEq
  | J.Asr -> J.AsrEq
  | J.Lsr -> J.LsrEq
  | J.Band -> J.BandEq
  | J.Bor -> J.BorEq
  | J.Bxor -> J.BxorEq
  | J.Mul -> J.StarEq
  | J.Plus -> J.PlusEq
  | J.Minus -> J.MinusEq
  | _ -> assert false

let var isint = function
  | (x,Some (J.EBin (J.Plus,y, J.EVar x')))
  | (x,Some (J.EBin (J.Plus, J.EVar x',y))) when x = x' ->
    if y = J.ENum 1.
    then Some (J.EUn (J.IncrB,J.EVar x))
    else Some (J.EBin (J.PlusEq, J.EVar x,y))
  | (x,Some (J.EBin (J.Minus, J.EVar x',y))) when x = x' ->
    if y = J.ENum 1.
    then Some (J.EUn (J.DecrB,J.EVar x))
    else Some (J.EBin (J.MinusEq, J.EVar x,y))
  | (x,Some (J.EBin (J.Mul,y, J.EVar x')))
  | (x,Some (J.EBin (J.Mul, J.EVar x',y))) when x = x' ->
    Some (J.EBin (J.StarEq, J.EVar x,y))
  | (x,Some (J.EBin (J.Div | J.Mod | J.Lsl | J.Asr | J. Lsr | J.Band | J.Bxor | J.Bor as unop, J.EVar x',y))) when x = x' && not isint ->
    Some (J.EBin (tr unop, J.EVar x,y))
  | x -> None

let var = function
  | (x,Some (J.EBin (J.Bor,e,J.ENum 0.))) -> var true (x,Some e)
  | x -> var false x

let optim_hh l =
  List.fold_right (fun st rem ->
    match st with
      | J.Variable_statement l1 ->
        let x = List.map (function x ->
            match var x with
              | Some e -> J.Expression_statement (e,None)
              | None -> J.Variable_statement [x]) l1 in
        x@rem
      | _ -> st::rem
  ) l []

let statement_list l =
  List.fold_right
    (fun st rem ->
       match st, rem with
         J.Variable_statement l1, J.Variable_statement l2 :: rem' ->
           J.Variable_statement (l1 @ l2) :: rem'
       | _ ->
           st :: rem)
    (optim_hh l) []

let block l = match l with [s] -> s | _ -> J.Block (statement_list l)

let unblock st = match st with J.Block l -> l | _ -> [st]

exception Not_expression

let rec expression_of_statement_list l =
  match l with
    J.Return_statement (Some e) :: _ ->
      e
  | J.Expression_statement (e, pc) :: rem ->
      J.ESeq (e, expression_of_statement_list rem)
  | _ ->
      raise Not_expression

let expression_of_statement st =
  match st with
    J.Return_statement (Some e) -> e
  | J.Block l                   -> expression_of_statement_list l
  | _                           -> raise Not_expression

exception Not_assignment

let rec assignment_of_statement_list l =
  match l with
    [J.Variable_statement [x, Some e]] ->
      (x, e)
  | J.Expression_statement (e, pc) :: rem ->
      let (x, e') = assignment_of_statement_list rem in
      (x, J.ESeq (e, e'))
  | _ ->
      raise Not_assignment

let assignment_of_statement st =
  match st with
    J.Variable_statement [x, Some e] -> (x, e)
  | J.Block l                        -> assignment_of_statement_list l
  | _                                -> raise Not_assignment

let simplify_condition = function
    (* | J.ECond _  -> J.ENum 1. *)
    | J.ECond (e, J.ENum 1., J.ENum 0.) -> e
    | J.ECond (e, J.ENum 0., J.ENum 1.) -> J.EUn (J.Not, e)
    | J.ECond (J.EBin((J.NotEqEq | J.NotEq), J.ENum n, y ),e1,e2)
    | J.ECond (J.EBin((J.NotEqEq | J.NotEq), y, J.ENum n),e1,e2) ->
      J.ECond (J.EBin(J.Band,y,J.ENum n),e1,e2)
    | cond        -> cond

let rec if_statement_2 e iftrue truestop iffalse falsestop =
  let e = simplify_condition e in
  match iftrue, iffalse with
    (* Empty blocks *)
    J.Block [], J.Block [] ->
      [J.Expression_statement (e, None)]
  | J.Block [], _ ->
      if_statement_2 (enot e) iffalse falsestop iftrue truestop
  | _, J.Block [] ->
      [J.If_statement (e, iftrue, None)]
  | _ ->
      (* Generates conditional *)
      begin try
        let (x1, e1) = assignment_of_statement iftrue in
        let (x2, e2) = assignment_of_statement iffalse in
        if x1 <> x2 then raise Not_assignment;
        let exp =
          if e1 = e
          then J.EBin(J.Or,e,e2)
          else J.ECond (e, e1, e2) in
        [J.Variable_statement [x1, Some exp]]
      with Not_assignment -> try
        let e1 = expression_of_statement iftrue in
        let e2 = expression_of_statement iffalse in
        [J.Return_statement (Some (J.ECond (e, e1, e2)))]
      with Not_expression ->
        if truestop then
          J.If_statement (e, iftrue, None) :: unblock iffalse
        else if falsestop then
          J.If_statement (enot e, iffalse, None) :: unblock iftrue
        else
          [J.If_statement (e, iftrue, Some iffalse)]
      end

let unopt b = match b with Some b -> b | None -> J.Block []

let rec if_statement e iftrue truestop (iffalse : J.statement) falsestop =
  (*FIX: should be done at an earlier stage*)
  let e = simplify_condition e in
  match iftrue, iffalse with
    (* Shared statements *)
  | J.If_statement (e', iftrue', iffalse'), _
        when iffalse = unopt iffalse' ->
      if_statement_2 (J.EBin (J.And, e, e')) iftrue' truestop iffalse falsestop
  | J.If_statement (e', iftrue', iffalse'), _
        when iffalse = iftrue' ->
      if_statement_2 (J.EBin (J.And, e, J.EUn (J.Not, e')))
        (unopt iffalse') truestop iffalse falsestop
  | _, J.If_statement (e', iftrue', iffalse')
        when iftrue = iftrue' ->
      if_statement_2 (J.EBin (J.Or, e, e'))
        iftrue truestop (unopt iffalse') falsestop
  | _, J.If_statement (e', iftrue', iffalse')
        when iftrue = unopt iffalse' ->
      if_statement_2 (J.EBin (J.Or, e, (J.EUn (J.Not, e'))))
        iftrue truestop iftrue' falsestop
  | _ ->
      if_statement_2 e iftrue truestop iffalse falsestop


let rec get_variable acc = function
  | J.ESeq (e1,e2)
  | J.EBin (_,e1,e2)
  | J.EAccess (e1,e2) -> get_variable (get_variable acc e1) e2
  | J.ECond (e1,e2,e3) ->
    get_variable (
      get_variable (
        get_variable
          acc
          e1)
        e2)
      e2
  | J.EUn (_,e1)
  | J.EDot (e1,_)
  | J.ENew (e1,None) -> get_variable acc e1
  | J.ECall (e1,el)
  | J.ENew (e1,Some el) -> List.fold_left get_variable acc (e1::el)
  | J.EVar (J.V v) -> Code.VarSet.add v acc
  | J.EVar (J.S _) -> acc
  | J.EFun _
  | J.EStr _
  | J.EBool _
  | J.ENum _
  | J.EQuote _ -> acc
  | J.EArr a -> List.fold_left (fun acc i ->
                  match i with
                    | None -> acc
                    | Some e1 -> get_variable acc e1) acc a
  | J.EObj l -> List.fold_left (fun acc (_,e1) ->
                  get_variable acc e1) acc l
