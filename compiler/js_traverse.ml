open Javascript

class type mapper = object
  method expression : Javascript.expression -> Javascript.expression
  method expression_o : Javascript.expression option -> Javascript.expression option
  method statement : Javascript.statement -> Javascript.statement
  method statement_o : Javascript.statement option -> Javascript.statement option
  method source : Javascript.source_element -> Javascript.source_element
  method ident : Javascript.ident -> Javascript.ident
  method program : Javascript.program -> Javascript.program
end


(* generic js ast walk/map *)
class map : mapper = object(m)
  method ident i = i
  method statement s = match s with
    | Block b -> Block (List.map m#statement b)
    | Variable_statement l ->
      Variable_statement (
        List.map
          (fun (id, eo) -> m#ident id, m#expression_o eo) l)
    | Empty_statement -> Empty_statement
    | Expression_statement (e,nid) ->
      Expression_statement (m#expression e, nid)
    | If_statement(e,s,sopt) ->
      If_statement(m#expression e,
    m#statement s,
    m#statement_o sopt)
  | Do_while_statement (s,e) ->
    Do_while_statement (m#statement s, m#expression e)
  | While_statement(e,s) ->
    While_statement(m#expression e,m#statement s)
  | For_statement(e1,e2,e3,s,nid) ->
    let e1 = match  e1 with
      | Left o -> Left(m#expression_o o)
      | Right l -> Right(List.map (fun (id, eo) -> m#ident id, m#expression_o eo) l) in
    For_statement(
      e1,
      m#expression_o e2,
      m#expression_o  e3,
      m#statement  s,
      nid)
  | ForIn_statement(e1,e2,s,nid) ->
    let e1 = match e1 with
      | Left e -> Left(m#expression e)
      | Right ((id,e)) -> Right ((id,m#expression_o e)) in
    ForIn_statement(
      e1,
      m#expression e2,
      m#statement s,
      nid)
  | Continue_statement s -> Continue_statement s
  | Break_statement s -> Break_statement s
  | Return_statement e ->
    Return_statement (m#expression_o e)
  | Labelled_statement(l,s) -> Labelled_statement(l,m#statement s)
  | Throw_statement e -> Throw_statement (m#expression e)
  | Switch_statement (e,l,def) ->
    Switch_statement (
      m#expression e,
      List.map (fun (e,s) -> m#expression e, List.map m#statement s) l,
      match def with
        | None -> None
        | Some l -> Some (List.map m#statement l))
  | Try_statement (b,catch,final,nid) ->
    Try_statement (
    List.map m#statement b,
    (match catch with
      | None -> None
      | Some (id,b) -> Some (m#ident id, List.map m#statement b)),
    (match final with
      | None -> None
      | Some s -> Some (List.map m#statement s)),
    nid)

  method statement_o x = match x with
    | None -> None
    | Some s -> Some (m#statement s)

  method expression x = match x with
    | ESeq(e1,e2) -> ESeq(m#expression  e1, m#expression  e2)
  | ECond(e1,e2,e3) ->
    ECond(m#expression  e1,m#expression  e2,m#expression  e3)
  | EBin(b,e1,e2) ->
    EBin(b,m#expression  e1,m#expression  e2)
  | EUn(b,e1) -> EUn(b,m#expression  e1)
  | ECall(e1,e2) ->
    ECall(m#expression  e1,List.map (m#expression ) e2)
  | EAccess(e1,e2) ->
    EAccess(m#expression  e1,m#expression  e2)
  | EDot(e1,id) -> EDot(m#expression  e1, id)
  | ENew(e1,Some args) ->
    ENew(m#expression  e1,Some (List.map (fun x -> m#expression  x) args))
  | ENew(e1,None) ->
    ENew(m#expression  e1,None)
  | EVar v -> EVar (m#ident v)
  | EFun ((idopt, params, body) ,nid) ->
    let idopt = match idopt with
      | None -> None
      | Some i -> Some (m#ident i) in
    EFun ((idopt, List.map m#ident params, List.map m#source body) ,nid)
  | EArr l ->
    EArr (List.map (fun x -> m#expression_o x) l)
  | EObj l ->
    EObj (List.map (fun (i,e) -> i, m#expression  e) l)
  | (EStr _ as x)
  | (EBool _ as x)
  | (ENum _ as x)
  | (EQuote _ as x) -> x


  method expression_o x = match x with
    | None -> None
    | Some s -> Some (m#expression s)

  method source x = match x with
    | Statement s -> Statement (m#statement s)
    | Function_declaration(id,params,body,nid) ->
      Function_declaration(m#ident id, List.map m#ident params, List.map m#source body,nid)

  method program x = List.map m#source x
end

(* var substitution *)
class subst sub = object
    inherit map
    method ident x = sub x
end

open Util
module S = Code.VarSet
type t = {
  use_name : StringSet.t;
  def_name : StringSet.t;
  def : S.t;
  use : S.t;
}

let empty = {
  def = S.empty;
  use = S.empty;
  use_name = StringSet.empty;
  def_name = StringSet.empty;
}

let ident s =
  try
    let i = String.index s '.' in
    String.sub s 0 i
  with Not_found -> s

(* def/used/free variable *)

class type freevar =
  object('a)
    inherit mapper
    method merge_info : 'a -> unit
    method block : Javascript.ident list -> unit

    method def_var : Javascript.ident -> unit
    method use_var : Javascript.ident -> unit
    method state : t
    method get_free_name : Util.StringSet.t
    method get_free : Code.VarSet.t
  end

class free =
  object(m : 'test)
    inherit map as super
  val mutable state_ : t = empty
  method state = state_

  method get_free =
    S.diff m#state.use m#state.def

  method get_free_name =
    StringSet.diff m#state.use_name m#state.def_name

  method merge_info from =
    let free_name = from#get_free_name in
    let free = from#get_free in
    state_ <- { state_ with
      use_name = StringSet.union state_.use_name free_name;
      use = S.union state_.use free }


  method use_var x = match x with
    | S name  ->
      let name = ident name in
      state_ <- { state_ with use_name = StringSet.add name state_.use_name  }
    | V v ->
      state_ <- { state_ with use = S.add v state_.use  }
  method def_var x = match x with
    | S name ->
      let name = ident name in
      state_ <- { state_ with def_name = StringSet.add name state_.def_name }
    | V v ->
      state_ <- { state_ with def = S.add v state_.def  }

  method expression x = match x with
    | EVar v -> m#use_var v; x
    | EFun ((ident,params,body),nid) ->
      let tbody  = ({< state_ = empty  >} :> 'test) in
      let () = List.iter tbody#def_var params in
      let () = match ident with
        | None -> ()
        | Some v -> tbody#def_var v in
      let body = List.map tbody#source body in
      tbody#block params;
      m#merge_info tbody;
      EFun ((ident,params,body),nid)
    | _ -> super#expression x

  method source x = match x with
    | Function_declaration (id,params, body, nid) ->
      let tbody = {< state_ = empty >} in
      let () = List.iter tbody#def_var params in
      let body = List.map tbody#source body in
      tbody#block params;
      m#def_var id;
      m#merge_info tbody;
      Function_declaration (id,params, body, nid)
    | _ -> super#source x

  method block params = ()


  method statement x = match x with
    | Variable_statement l ->
      let l = List.map (fun (id,eopt) ->
        m#def_var id;
        match eopt with
          | None -> (id,None)
          | Some e ->
            let e = m#expression e in
            (id,Some e)) l in
      Variable_statement l
    | For_statement (Right l,e2,e3,s,nid) ->
      let l = List.map (fun (id,eopt) ->
          m#def_var id;
          match eopt with
            | None -> (id,None)
            | Some e ->
              let e = m#expression e in
              (id,Some e)) l in
      For_statement (Right l, m#expression_o e2, m#expression_o e3, m#statement s,nid)
    | ForIn_statement(Right ((id,eopt)),e2,s,nid) ->
      m#def_var id;
      let r = match eopt with
        | None -> (id,None)
        | Some e ->
          let e = m#expression e in
          (id,Some e) in
      ForIn_statement(Right r,m#expression e2,m#statement s,nid)
    | Try_statement (b,w,f,nid) ->
      let b = List.map m#statement b in
      let tbody = {< state_ = empty >} in
      let w = match w with
        | None -> None
        | Some (id,block) ->
          let block = List.map tbody#statement block in
          let () = tbody#def_var id in
          tbody#block [id];
          (* special merge here *)
          (* we need to propagate both def and use .. *)
          (* .. except 'id' because its scope is limitied to 'block' *)
          let clean set sets = match id with
            | S s -> set,StringSet.remove s sets
            | V i -> S.remove i set, sets in
          let def,def_name = clean tbody#state.def tbody#state.def_name in
          let use,use_name = clean tbody#state.use tbody#state.use_name in
          state_ <- {
            use = S.union state_.use use;
            use_name = StringSet.union state_.use_name use_name;
            def = S.union state_.def def;
            def_name = StringSet.union state_.def_name def_name };
          Some (id,block)
      in
      let f = match f with
        | None -> None
        | Some block -> Some (List.map m#statement block)
      in
      Try_statement (b,w,f,nid)
    | _ -> super#statement x
  end



class rename_str = object(m : 'test)
  inherit free as super

  val mutable sub_ = new subst (fun x -> x)

  method merge_info from =
    let h = Hashtbl.create 17 in
    let _ = StringSet.iter (fun name -> Hashtbl.add h name (Code.Var.fresh ())) from#state.def_name in
    let f = function
      | (S name) when Hashtbl.mem h name -> V (Hashtbl.find h name)
      | s -> s in
    sub_ <- new subst f

  (* method block params *)

  method expression x =
    let x = super#expression x in
    match x with
      | EFun _ -> sub_#expression x
      | _ -> x

  method source x =
    let x = super#source x in
    match x with
      | Function_declaration _ -> sub_#source x
      | _ -> x

end

class compact_vardecl = object(m)
    inherit free as super

    val mutable exc_ = IdentSet.empty
    val mutable insert_ = IdentSet.empty

    method exc = exc_

    method private translate l =
      Util.filter_map (fun (id,eopt) ->
        match eopt with
          | None -> None
          | Some e -> Some (EBin (Eq,EVar id,e))) l

    method private translate_st l =
      let l = m#translate l in
      match l with
        | [] -> Empty_statement
        | x::l -> Expression_statement (List.fold_left (fun acc e -> ESeq(acc,e)) x l,None)

    method private translate_ex l =
      let l = m#translate l in
      match l with
        | [] -> None
        | x::l -> Some (List.fold_left (fun acc e -> ESeq(acc,e)) x l)

    method private except e = exc_ <- IdentSet.add e exc_

    method statement s =
      let s = super#statement s in
      match s with
        | Variable_statement l -> m#translate_st l
        | For_statement (Right l,e2,e3,s,nid) ->
          For_statement (Left (m#translate_ex l), e2, e3, s,nid)
        | ForIn_statement(Right (id,op),e2,s,nid) ->
          (match op with
            | Some _ -> assert false
            | None -> ());
          ForIn_statement(Left (EVar id),e2,s,nid)
        | Try_statement (b,w,f,nid) ->
          (match w with
            | None -> ()
            | Some (id,_) -> m#except id);
          Try_statement (b,w,f,nid)
        | s -> s

    method block params =
      List.iter m#except params;
      super#block params;

    method merge_info from =
      super#merge_info from;
      let all = S.fold (fun e acc -> IdentSet.add (V e) acc) from#state.def IdentSet.empty in
      let all = StringSet.fold (fun e acc -> IdentSet.add (S e) acc) from#state.def_name all in
      insert_ <- IdentSet.diff all from#exc

    method source x =
      let x = super#source x in
      match x with
        | Function_declaration (id,params, body, nid) ->
          let all = IdentSet.diff insert_ exc_ in
          let d = IdentSet.fold (fun x acc ->
              assert (not (List.mem x params));
              (x,None)::acc) all [] in
          let d = Statement (Variable_statement d) in
          m#except id;
          Function_declaration (id,params, d::body, nid)
        | _ -> x

    method expression x =
      let x = super#expression x in
      match x with
        | EFun ((ident,params,body),nid) ->
          let all = IdentSet.diff insert_ exc_ in
          let d = IdentSet.fold (fun x acc -> (x,None)::acc) all [] in
          let d = Statement (Variable_statement d) in
          (match ident with
            | Some id -> m#except id;
            | None -> ());
          EFun ((ident,params,d::body),nid)
        | _ -> x

end
