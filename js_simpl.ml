module J = Javascript

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

exception Not_expression

let rec expression_of_statement_list l =
  match l with
    J.Return_statement (Some e) :: _ ->
      e
  | J.Expression_statement e :: rem ->
      J.ESeq (e, expression_of_statement_list rem)
  | _ ->
      raise Not_expression

and expression_of_statement st =
  match st with
    J.Return_statement (Some e) -> e
  | J.Block l                   -> expression_of_statement_list l
  | _                           -> raise Not_expression

let if_statement e iftrue iffalse =
  let e =
    (*FIX: should be done at an earlier stage*)
    match e with
      J.ECond (e, J.ENum 1., J.ENum 0.) -> e
    | _                                 -> e
  in
  match iftrue, iffalse with
    J.If_statement (e', iftrue', iffalse'), _
        when iffalse = iffalse' ->
      J.If_statement (J.EBin (J.And, e, e'), iftrue', iffalse)
  | J.If_statement (e', iftrue', Some iffalse'), _
        when iffalse = Some iftrue' ->
      J.If_statement (J.EBin (J.And, e, J.EUn (J.Not, e')), iffalse', iffalse)
  | _, Some (J.If_statement (e', iftrue', iffalse'))
        when iftrue = iftrue' ->
      J.If_statement (J.EBin (J.Or, e, e'), iftrue, iffalse')
  | _, Some (J.If_statement (e', iftrue', iffalse'))
        when Some iftrue = iffalse' ->
      J.If_statement (J.EBin (J.Or, e, (J.EUn (J.Not, e'))),
                      iftrue, Some iftrue')
  | _, Some iffalse ->
      begin try
        let e1 = expression_of_statement iftrue in
        let e2 = expression_of_statement iffalse in
        J.Return_statement (Some (J.ECond (e, e1, e2)))
      with Not_expression ->
        J.If_statement (e, iftrue, Some iffalse)
      end
  | _ ->
      J.If_statement (e, iftrue, iffalse)
