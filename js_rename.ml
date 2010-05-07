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

let rename_body bound_vars b =
  ()
