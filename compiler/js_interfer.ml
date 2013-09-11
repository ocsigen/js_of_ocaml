open Javascript


(* let rec interfer x = function *)
(*   | Statement st -> *)
(*   | Function_declaration f -> *)


(* let rec graph_of_sources l = *)
(*   List.fold_left graph_of_source [] l *)

(* and graph_of_source g s = *)
(*   match s with *)
(*     | Statement st -> g *)
(*     | Function_declaration f -> graph_of_fun_decl g f *)
(* and graph_of_fun_decl g (ident, params, body, _) = *)
(*   let vars = compute_var body in *)
(*   List.iter (interfer ident) vars; *)


module V = struct


type t = {
  def : Code.VarSet.t;
  use : Code.VarSet.t;
}

let union t1 t2 = {
  use = Code.VarSet.union t1.use t1.use;
  def = Code.VarSet.union t1.def t1.def
}

let use_var t = function
  | S _ -> t
  | V i -> { t with use = Code.VarSet.add i t.use }

let def_var t = function
  | S _ -> t
  | V i -> { t with def = Code.VarSet.add i t.def }

let empty = {
  def = Code.VarSet.empty;
  use = Code.VarSet.empty
}

let rec expression t e =
  match e with
    | ECond (e1,e2,e3) ->
      expression
        (expression
           (expression t e1)
           e2
        )
        e3
    | ESeq (e1,e2)
    | EAccess (e1,e2)
    | EBin (_,e1,e2) ->
      expression (expression t e1) e2
    | EUn (_,e1)
    | EDot (e1,_)
    | ENew (e1,None) -> expression t e1
    | ECall (e,args)
    | ENew (e,Some args) ->
      List.fold_left (fun acc x ->
        expression acc x) (expression t e) args
    | EVar v -> use_var t v
    | EFun ((ident,params,body),_) ->
      let tbody = List.fold_left def_var empty params in
      let tbody = match ident with
        | None -> tbody
        | Some v -> def_var tbody v in
      let tbody = source_elts tbody body in
      let tfree = Code.VarSet.diff tbody.use tbody.def in
      {t with use = Code.VarSet.union t.use tfree }
    | EStr _
    | EBool _
    | ENum _
    | EQuote _ -> t
    | EObj l ->
      List.fold_left (fun acc (_,e) ->
      expression acc e) t l
    | EArr l ->
      List.fold_left (fun acc x ->
        match x with
          | None -> acc
          | Some e -> expression acc e) t l

and source_elts t l =
    List.fold_left (fun acc s ->
      source_elt acc s) t l

and source_elt t e =
  match e with
    | Statement s -> statement t s
    | Function_declaration (id,params, body, _) ->
      let tbody = List.fold_left def_var empty params in
      let tbody = def_var tbody id in
      let tbody = source_elts tbody body in
      let tfree = Code.VarSet.diff tbody.use tbody.def in
      { t with use = Code.VarSet.union t.use tfree }

and statements t l = List.fold_left statement t l

and statement t s =
  match s with
    | Block l -> List.fold_left statement t l
    | Variable_statement l ->
      List.fold_left (fun t (id,eopt) ->
        let t = def_var t id in
        match eopt with
          | None -> t
          | Some e -> expression t e) t l
    | Expression_statement (e,_) -> expression t e
    | If_statement(e1,s2,e3opt) ->
      let t = statement (expression t e1) s2 in
      begin
        match e3opt with
          | None -> t
          | Some e -> statement t e
      end
    | Do_while_statement (s,e)
    | While_statement (e,s) ->
      statement (expression t e) s
    | For_statement (e1,e2,e3,s,_) ->
      let t = List.fold_left (fun acc x ->
        match x with
          | None -> acc
          | Some e -> expression acc e ) t [e1;e2;e3] in
      statement t s
    | Continue_statement _
    | Break_statement _ -> t
    | Return_statement None -> t
    | Return_statement (Some e) -> expression t e
    | Labelled_statement (_,s) -> statement t s
    | Switch_statement(e,cl,sl) ->
      let t = expression t e in
      let t = List.fold_left (fun t (e, sl) ->
        let t = expression t e in
        statements t sl) t cl in
      begin match sl with
        | None -> t
        | Some sl -> statements t sl
      end
    | Throw_statement e ->
      expression t e
    | Try_statement (b,w,f,_) ->
      let t = statements t b in
      let t = match w with
        | None -> t
        | Some (id,block) ->
          let t = def_var t id in
          statements t block in
      let t = match f with
        | None -> t
        | Some block -> statements t block
      in t
end
