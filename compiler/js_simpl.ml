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
  match e2 with
    J.ENum n when n < 0. ->
      J.EBin (J.Minus, e1, J.ENum (-. n))
  | _ ->
      J.EBin (J.Plus, e1, e2)

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
    | J.EUn ((J.Neg | J.Pl | J.Typeof), _) ->
        (J.EUn (J.Not, e), 0)
    | J.EBool b ->
        (J.EBool (not b), 0)
    | J.ECall _ | J.EAccess _ | J.EDot _ | J.ENew _ | J.EVar _ | J.EFun _
    | J.EStr _ | J.EArr _ | J.ENum _ | J.EObj _ | J.EQuote _ ->
        (J.EUn (J.Not, e), 1)
  in
  if cost <= 1 then res else (J.EUn (J.Not, e), 1)

let enot e = fst (enot_rec e)

let source_elements l =
  (* FIX: not tail recursive *)
  List.fold_right
    (fun st rem ->
       match st, rem with
         J.Variable_statement [addr, Some (J.EFun (None, params, body))], _ ->
           J.Function_declaration (addr, params, body) :: rem
       | J.Variable_statement l1,
         J.Statement (J.Variable_statement l2) :: rem' ->
           J.Statement (J.Variable_statement (l1 @ l2)) :: rem'
       | _ ->
           J.Statement st :: rem)
    l []

let statement_list l =
  List.fold_right
    (fun st rem ->
       match st, rem with
         J.Variable_statement l1, J.Variable_statement l2 :: rem' ->
           J.Variable_statement (l1 @ l2) :: rem'
       | _ ->
           st :: rem)
    l []

let block l = match l with [s] -> s | _ -> J.Block (statement_list l)

let unblock st = match st with J.Block l -> l | _ -> [st]

exception Not_expression

let rec expression_of_statement_list l =
  match l with
    J.Return_statement (Some e) :: _ ->
      e
  | J.Expression_statement e :: rem ->
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
  | J.Expression_statement e :: rem ->
      let (x, e') = assignment_of_statement_list rem in
      (x, J.ESeq (e, e'))
  | _ ->
      raise Not_assignment

let assignment_of_statement st =
  match st with
    J.Variable_statement [x, Some e] -> (x, e)
  | J.Block l                        -> assignment_of_statement_list l
  | _                                -> raise Not_assignment

let rec if_statement_2 e iftrue truestop iffalse falsestop =
  match iftrue, iffalse with
    (* Empty blocks *)
    J.Block [], J.Block [] ->
      [J.Expression_statement e]
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
        [J.Variable_statement [x1, Some (J.ECond (e, e1, e2))]]
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
  let e =
    (*FIX: should be done at an earlier stage*)
    match e with
      J.ECond (e, J.ENum 1., J.ENum 0.) -> e
    | _                                 -> e
  in
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
