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

(*
Rename bound variables...


Get the set of bound variables

Assign variable in inner functions

Get the set of globally free variables ==> black list

For each locally bound variable, gives it the first value possible

Return set of globally free variables + mark free variable with the
first possible value
*)

module StringSet = Util.StringSet

module J = Javascript

(****)

let (>>) x f = f x

let opt_app f x y = match x with None -> y | Some x -> f x y

let rec bound_in_statement st s =
  match st with
    J.Block b ->
      bound_in_block b s
  | J.Variable_statement l ->
      List.fold_right (fun (nm, _) s -> StringSet.add nm s) l s
  | J.Expression_statement _ | J.Continue_statement _ | J.Break_statement _
  | J.Return_statement _ | J.Throw_statement _ ->
      s
  | J.If_statement (_, st1, st2) ->
      s >> bound_in_statement st1 >> opt_app bound_in_statement st2
  | J.Do_while_statement (st, _) | J.While_statement (_, st)
  | J.For_statement (_, _, _, st) ->
      bound_in_statement st s
  | J.Switch_statement (_, l1, l2) ->
      s
      >> List.fold_right (fun (_, b) s -> bound_in_block b s) l1
      >> opt_app bound_in_block l2
  | J.Try_statement (b, h, f) ->
      s
      >> opt_app (fun (_, b) -> bound_in_block b) h
      >> opt_app bound_in_block f

and bound_in_block l s = List.fold_right bound_in_statement l s

let bound_in_function_body l s =
  List.fold_left
    (fun s e ->
       match e with
         J.Function_declaration (nm, _, _) ->
           StringSet.add nm s
       | J.Statement st ->
           bound_in_statement st s)
    s l

let bound_in_function (_, l, b) =
  bound_in_function_body b (List.fold_right StringSet.add l StringSet.empty)

(****)


let rec expr_fold_closures f e accu =
  match e with
    J.ESeq (e1, e2)
  | J.EBin (_, e1, e2)
  | J.EAccess (e1, e2) ->
      accu >> expr_fold_closures f e1 >> expr_fold_closures f e2
  | J.ECond (e1, e2, e3) ->
      accu
      >> expr_fold_closures f e1
      >> expr_fold_closures f e2
      >> expr_fold_closures f e3
  | J.EUn (_, e1) | J.EDot (e1, _) | J.ENew (e1, None) ->
      expr_fold_closures f e1 accu
  | J.ECall (e1, l) | J.ENew (e1, Some l) ->
      accu >> expr_fold_closures f e1 >>
      List.fold_right (fun e accu -> expr_fold_closures f e accu) l
  | J.EVar _ | J.EStr _ | J.EBool _ | J.ENum _ ->
      accu
  | J.EFun cl ->
      f cl accu
  | J.EArr l ->
      List.fold_right
        (fun e accu -> opt_app (expr_fold_closures f) e accu) l accu
  | J.EObj l ->
      List.fold_right (fun (_, e) accu -> expr_fold_closures f e accu) l accu
  | J.EQuote _ ->
      assert false

(*
- Allocate variables in inner closures
  ==> subst + constraint on free variables (not in some set)
*)



(*
Use the fact that variables are globally unique

We map an integer to each variable
*)

(*
type ctx =
  { subst : int Hashtbl.t;
    bound_vars : StringSet.t;
    mutable next : int }

let rename_statement ctx st free_vars =
  match st with
    J.Block b ->
      rename_block ctx b free_vars
  | J.Variable_statement l ->
      List.fold_left
        (fun free_vars (nm, def) ->
           if not (Hashtbl.mem nm ctx.subst) then begin
             Hashtbl.add nm ctx.next ctx.subst;
             ctx.next <- ctx.next + 1
           end;
           free_vars)
        free_vars l
  | J.Expression_statement _ | J.Continue_statement _ | J.Break_statement _
  | J.Return_statement _ | J.Throw_statement _ ->
      s
  | J.If_statement (_, st1, st2) ->
      s >> bound_in_statement st1 >> opt_app bound_in_statement st2
  | J.Do_while_statement (st, _) | J.While_statement (_, st)
  | J.For_statement (_, _, _, st) ->
      bound_in_statement st s
  | J.Switch_statement (_, l1, l2) ->
      s
      >> List.fold_right (fun (_, b) s -> bound_in_block b s) l1
      >> opt_app bound_in_block l2
  | J.Try_statement (b, h, f) ->
      s
      >> opt_app (fun (_, b) -> bound_in_block b) h
      >> opt_app bound_in_block f

let rec rename_body ctx b =
  let free_vars =
    List.fold_left
      (fun free_vars e ->
         match e with
           J.Function_declaration _ ->
             free_vars (*XXX*)
         | J.Statement st ->
             rename_statement ctx st free_vars)
      StringSet.empty b
*)
