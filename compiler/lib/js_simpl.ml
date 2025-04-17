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
open! Stdlib
module J = Javascript

let rec enot_rec e =
  let ((_, cost) as res) =
    match e with
    | J.ESeq (e1, e2) ->
        let e2', cost = enot_rec e2 in
        J.ESeq (e1, e2'), cost
    | J.ECond (e1, e2, e3) ->
        let e2', cost2 = enot_rec e2 in
        let e3', cost3 = enot_rec e3 in
        J.ECond (e1, e2', e3'), cost2 + cost3
    | J.EBin (op, e1, e2) -> (
        match op with
        | J.Or ->
            let e1', cost1 = enot_rec e1 in
            let e2', cost2 = enot_rec e2 in
            J.EBin (J.And, e1', e2'), cost1 + cost2
        | J.And ->
            let e1', cost1 = enot_rec e1 in
            let e2', cost2 = enot_rec e2 in
            J.EBin (J.Or, e1', e2'), cost1 + cost2
        | J.EqEq -> J.EBin (J.NotEq, e1, e2), 0
        | J.NotEq -> J.EBin (J.EqEq, e1, e2), 0
        | J.EqEqEq -> J.EBin (J.NotEqEq, e1, e2), 0
        | J.NotEqEq -> J.EBin (J.EqEqEq, e1, e2), 0
        | J.LtInt ->
            (* not (a < b) *)
            (* a >= b *)
            J.EBin (J.GeInt, e1, e2), 0
        | J.GtInt ->
            (* not (a > b) *)
            (* a <= b *)
            J.EBin (J.LeInt, e1, e2), 0
        | J.LeInt -> J.EBin (J.GtInt, e1, e2), 0
        | J.GeInt -> J.EBin (J.LtInt, e1, e2), 0
        (* Disabled: this is not correct!
           {[ !(x < 0) ]} and {[ x >= 0 ]} give different result when x = nan
           {[
             | J.Lt ->
                 (J.EBin (J.Le, e2, e1), 0)
             | J.Le ->
                 (J.EBin (J.Lt, e2, e1), 0)
           ]}
        *)
        | _ -> J.EUn (J.Not, e), 1)
    | J.EUn (J.Not, e) -> e, 0
    | J.EUn ((J.Neg | J.Pl | J.Typeof | J.Void | J.Delete | J.Bnot), _) ->
        J.EUn (J.Not, e), 0
    | J.EBool b -> J.EBool (not b), 0
    | J.ECall _
    | J.ECallTemplate _
    | J.EAccess _
    | J.EDot _
    | J.EDotPrivate _
    | J.ENew _
    | J.EVar _
    | J.EPrivName _
    | J.EFun _
    | J.EArrow _
    | J.EStr _
    | J.EArr _
    | J.ENum _
    | J.EObj _
    | J.ERegexp _
    | J.EYield _
    | J.ETemplate _
    | J.EAssignTarget _
    | J.EClass _
    | J.EUn (J.Await, _)
    | J.EUn ((J.IncrA | J.IncrB | J.DecrA | J.DecrB), _) -> J.EUn (J.Not, e), 1
    | J.CoverCallExpressionAndAsyncArrowHead _
    | J.CoverParenthesizedExpressionAndArrowParameterList _ -> assert false
  in
  if cost <= 1 then res else J.EUn (J.Not, e), 1

let enot e = fst (enot_rec e)

let unblock st =
  match st with
  | J.Block l, _ -> l
  | _ -> [ st ]

let block l =
  match l with
  | [ x ] -> x
  | l -> J.Block l, J.N

exception Not_expression

let rec expression_of_statement_list l =
  match l with
  | (J.Return_statement (Some e, _), _) :: _ -> e
  | (J.Expression_statement e, _) :: rem -> J.ESeq (e, expression_of_statement_list rem)
  | _ -> raise Not_expression

let expression_of_statement st =
  match fst st with
  | J.Return_statement (Some e, _) -> e
  | J.Block l -> expression_of_statement_list l
  | _ -> raise Not_expression

exception Not_assignment

let rec assignment_of_statement_list l =
  match l with
  | [ (J.Variable_statement (Var, [ (DeclIdent _ as vd) ]), _) ] -> vd
  | [ (J.Variable_statement (Var, [ (DeclPattern _ as vd) ]), _) ] -> vd
  | (J.Expression_statement e, _) :: rem -> (
      match assignment_of_statement_list rem with
      | DeclIdent (x, Some (e', nid)) -> DeclIdent (x, Some (J.ESeq (e, e'), nid))
      | DeclIdent (_, None) -> assert false
      | DeclPattern (p, (e', nid)) -> DeclPattern (p, (J.ESeq (e, e'), nid)))
  | _ -> raise Not_assignment

let assignment_of_statement st =
  match fst st with
  | J.Variable_statement (Var, [ (DeclIdent (_, Some _) as vd) ]) -> vd
  | J.Block l -> assignment_of_statement_list l
  | _ -> raise Not_assignment

let simplify_condition = function
  | J.ECond (e, J.ENum one, J.ENum zero) when J.Num.is_one one && J.Num.is_zero zero -> e
  | J.ECond (e, J.ENum zero, J.ENum one) when J.Num.is_one one && J.Num.is_zero zero ->
      J.EUn (J.Not, e)
  | J.ECond (J.EBin ((J.NotEqEq | J.NotEq), J.ENum n, y), e1, e2)
  | J.ECond (J.EBin ((J.NotEqEq | J.NotEq), y, J.ENum n), e1, e2) ->
      J.ECond (J.EBin (J.Band, y, J.ENum n), e1, e2)
  | cond -> cond

let rec depth = function
  | J.Block b -> depth_block b + 1
  | Function_declaration (_, (_, _, b, _)) -> depth_block b + 1
  | Class_declaration (_, cl) -> depth_class_block cl.body + 1
  | Variable_statement _ -> 1
  | Empty_statement -> 1
  | Expression_statement _ -> 1
  | If_statement (_, (t, _), None) -> depth t + 1
  | If_statement (_, (t, _), Some (f, _)) -> max (depth t) (depth f) + 1
  | Do_while_statement ((s, _), _) -> depth s + 1
  | While_statement (_, (s, _)) -> depth s + 1
  | For_statement (_, _, _, (s, _)) -> depth s + 1
  | ForIn_statement (_, _, (s, _)) -> depth s + 1
  | ForOf_statement (_, _, (s, _)) -> depth s + 1
  | ForAwaitOf_statement (_, _, (s, _)) -> depth s + 1
  | Continue_statement _ -> 1
  | Break_statement _ -> 1
  | Return_statement _ -> 1
  | Labelled_statement (_, (s, _)) -> depth s
  | Switch_statement (_, c1, None, c2) ->
      max
        (depth_block (List.concat_map ~f:snd c1))
        (depth_block (List.concat_map ~f:snd c2))
  | Switch_statement (_, c1, Some l, c2) ->
      max
        (max
           (depth_block (List.concat_map ~f:snd c1))
           (depth_block (List.concat_map ~f:snd c2)))
        (depth_block l)
  | Throw_statement _ -> 1
  | Try_statement (b, _, None) -> depth_block b + 1
  | Try_statement (b, _, Some b2) -> max (depth_block b) (depth_block b2) + 1
  | With_statement (_, (st, _)) -> depth st + 1
  | Debugger_statement -> 1
  | Import _ -> 1
  | Export _ -> 1

and depth_block b = List.fold_left b ~init:0 ~f:(fun acc (s, _) -> max acc (depth s))

and depth_class_block b =
  List.fold_left b ~init:0 ~f:(fun acc s ->
      match s with
      | J.CEMethod _ -> acc
      | J.CEField _ -> acc
      | J.CEStaticBLock b -> depth_block b + 2)

let expression_equal (a : J.expression) b = Poly.equal a b

let binding_pattern_equal (a : J.binding_pattern) b = Poly.equal a b

let statement_equal (a : J.statement * J.location) b = Poly.equal a b

let rec if_statement_2 ~function_end e loc iftrue truestop iffalse falsestop =
  let e = simplify_condition e in
  match fst iftrue, fst iffalse with
  (* Empty blocks *)
  | J.Block [], J.Block [] -> (
      match e with
      | J.EVar _ -> []
      | _ -> [ J.Expression_statement e, loc ])
  | J.Block [], _ ->
      if_statement_2 ~function_end (enot e) loc iffalse falsestop iftrue truestop
  | _, J.Block [] -> [ J.If_statement (e, iftrue, None), loc ]
  | _ -> (
      try
        (* Generates conditional *)
        let vd1 = assignment_of_statement iftrue in
        let vd2 = assignment_of_statement iffalse in
        match vd1, vd2 with
        | DeclIdent (x1, Some (e1, _)), DeclIdent (x2, Some (e2, _))
          when J.ident_equal x1 x2 ->
            let exp =
              if expression_equal e1 e then J.EBin (J.Or, e, e2) else J.ECond (e, e1, e2)
            in
            [ J.Variable_statement (Var, [ DeclIdent (x1, Some (exp, loc)) ]), loc ]
        | DeclPattern (p1, (e1, _)), DeclPattern (p2, (e2, _))
          when binding_pattern_equal p1 p2 ->
            let exp =
              if expression_equal e1 e then J.EBin (J.Or, e, e2) else J.ECond (e, e1, e2)
            in
            [ J.Variable_statement (Var, [ DeclPattern (p1, (exp, loc)) ]), loc ]
        | _ -> raise Not_assignment
      with Not_assignment -> (
        try
          let e1 = expression_of_statement iftrue in
          let e2 = expression_of_statement iffalse in
          [ J.Return_statement (Some (J.ECond (e, e1, e2)), function_end ()), loc ]
        with Not_expression ->
          let truestop, falsestop =
            if truestop && falsestop
            then
              let dtrue = depth (fst iftrue) in
              let dfalse = depth (fst iffalse) in
              if dtrue <= dfalse then true, false else false, true
            else truestop, falsestop
          in
          if truestop
          then (J.If_statement (e, iftrue, None), loc) :: unblock iffalse
          else if falsestop
          then (J.If_statement (enot e, iffalse, None), loc) :: unblock iftrue
          else [ J.If_statement (e, iftrue, Some iffalse), loc ]))

let unopt b =
  match b with
  | Some b -> b
  | None -> J.Block [], J.N

let if_statement ~function_end e loc iftrue truestop iffalse falsestop =
  (*FIX: should be done at an earlier stage*)
  let e = simplify_condition e in
  match iftrue, iffalse with
  (* Shared statements *)
  | (J.If_statement (e', iftrue', iffalse'), _), _
    when statement_equal iffalse (unopt iffalse') ->
      if_statement_2
        ~function_end
        (J.EBin (J.And, e, e'))
        loc
        iftrue'
        truestop
        iffalse
        falsestop
  | (J.If_statement (e', iftrue', iffalse'), _), _ when statement_equal iffalse iftrue' ->
      if_statement_2
        ~function_end
        (J.EBin (J.And, e, J.EUn (J.Not, e')))
        loc
        (unopt iffalse')
        truestop
        iffalse
        falsestop
  | _, (J.If_statement (e', iftrue', iffalse'), _) when statement_equal iftrue iftrue' ->
      if_statement_2
        ~function_end
        (J.EBin (J.Or, e, e'))
        loc
        iftrue
        truestop
        (unopt iffalse')
        falsestop
  | _, (J.If_statement (e', iftrue', iffalse'), _)
    when statement_equal iftrue (unopt iffalse') ->
      if_statement_2
        ~function_end
        (J.EBin (J.Or, e, J.EUn (J.Not, e')))
        loc
        iftrue
        truestop
        iftrue'
        falsestop
  | _ -> if_statement_2 ~function_end e loc iftrue truestop iffalse falsestop

let function_body b =
  (* We only check for a return at the end since it is by far the most
     common case in practice. *)
  let has_useless_return =
    let rec check l =
      match l with
      | [] -> false
      | [ (J.Return_statement (None, _), _) ] -> true
      | _ :: r -> check r
    in
    check b
  in
  if has_useless_return
  then
    let rec remove acc l =
      match l with
      | [] -> acc
      | [ (J.Return_statement (None, _), _) ] -> acc
      | i :: r -> remove (i :: acc) r
    in
    List.rev (remove [] b)
  else b
