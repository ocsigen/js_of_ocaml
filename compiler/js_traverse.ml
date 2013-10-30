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

open Javascript

class type mapper = object
  method expression : Javascript.expression -> Javascript.expression
  method expression_o : Javascript.expression option -> Javascript.expression option
  method statement : Javascript.statement -> Javascript.statement
  method statement_o : Javascript.statement option -> Javascript.statement option
  method statements : Javascript.statement list -> Javascript.statement list
  method source : Javascript.source_element -> Javascript.source_element
  method sources : Javascript.source_element list -> Javascript.source_element list
  method ident : Javascript.ident -> Javascript.ident
  method program : Javascript.program -> Javascript.program
end


(* generic js ast walk/map *)
class map : mapper = object(m)
  method ident i = i

  method statements l = List.map m#statement l

  method statement s = match s with
    | Block b -> Block (m#statements b)
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
      List.map (fun (e,s) -> m#expression e, m#statements s) l,
      match def with
        | None -> None
        | Some l -> Some (m#statements l))
  | Try_statement (b,catch,final,nid) ->
    Try_statement (
    m#statements b,
    (match catch with
      | None -> None
      | Some (id,b) -> Some (m#ident id, m#statements b)),
    (match final with
      | None -> None
      | Some s -> Some (m#statements s)),
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
    ECall(m#expression  e1,List.map m#expression e2)
  | EAccess(e1,e2) ->
    EAccess(m#expression  e1,m#expression  e2)
  | EDot(e1,id) -> EDot(m#expression  e1, id)
  | ENew(e1,Some args) ->
    ENew(m#expression  e1,Some (List.map m#expression args))
  | ENew(e1,None) ->
    ENew(m#expression  e1,None)
  | EVar v -> EVar (m#ident v)
  | EFun ((idopt, params, body) ,nid) ->
    let idopt = match idopt with
      | None -> None
      | Some i -> Some (m#ident i) in
    EFun ((idopt, List.map m#ident params, m#sources body) ,nid)
  | EArr l ->
    EArr (List.map (fun x -> m#expression_o x) l)
  | EObj l ->
    EObj (List.map (fun (i,e) -> i, m#expression  e) l)
  | (EStr _ as x)
  | (EBool _ as x)
  | (ENum _ as x)
  | (EQuote _ as x)
  | (ERegexp _ as x) -> x


  method expression_o x = match x with
    | None -> None
    | Some s -> Some (m#expression s)

  method source x = match x with
    | Statement s -> Statement (m#statement s)
    | Function_declaration(id,params,body,nid) ->
      Function_declaration(m#ident id, List.map m#ident params, m#sources body,nid)

  method sources x = List.map m#source x

  method program x = m#sources x
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
    method block : ?catch:bool -> Javascript.ident list -> unit

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
      let body = tbody#sources body in
      tbody#block params;
      m#merge_info tbody;
      EFun ((ident,params,body),nid)
    | _ -> super#expression x

  method source x = match x with
    | Function_declaration (id,params, body, nid) ->
      let tbody = {< state_ = empty >} in
      let () = List.iter tbody#def_var params in
      let body = tbody#sources body in
      tbody#block params;
      m#def_var id;
      m#merge_info tbody;
      Function_declaration (id,params, body, nid)
    | _ -> super#source x

  method block ?catch params = ()


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
      let b = m#statements b in
      let tbody = {< state_ = empty >} in
      let w = match w with
        | None -> None
        | Some (id,block) ->
          let block = tbody#statements block in
          let () = tbody#def_var id in
          tbody#block ~catch:true [id];
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
        | Some block -> Some (m#statements block)
      in
      Try_statement (b,w,f,nid)
    | _ -> super#statement x
  end



class rename_str keeps = object(m : 'test)
  inherit free as super

  val mutable sub_ = new subst (fun x -> x)

  method merge_info from =
    let h = Hashtbl.create 17 in
    let _ = StringSet.iter (fun name ->
        if StringSet.mem name keeps
        then ()
        else
          let v = Code.Var.fresh () in
          Code.Var.name v name;
          Hashtbl.add h name v) from#state.def_name in
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

  method statement x =
    let x = super#statement x in
    match x with
      | Try_statement (b,w,f,nid) ->
        let w = match w with
          | Some(S name,block) ->
            let v = Code.Var.fresh () in
            Code.Var.name v name;
            let sub = function
              | S name' when name' = name -> V v
              | x -> x in
            let s = new subst sub in
            Some(V v ,s#statements block)
          | x -> x in
        Try_statement (b,w,f,nid)
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

    method block ?(catch=false) params =
      List.iter m#except params;
      super#block params;

    method merge_info from =
      super#merge_info from;
      let all = S.fold (fun e acc -> IdentSet.add (V e) acc) from#state.def IdentSet.empty in
      let all = StringSet.fold (fun e acc -> IdentSet.add (S e) acc) from#state.def_name all in
      insert_ <- IdentSet.diff all from#exc

    method private split x =
      let rec loop = function
        | ESeq(e1,e2) -> loop e1 @ loop e2
        | e -> [e] in
      loop x

    method private pack all sources =
      let may_flush rem vars s instr =
        if vars = []
        then rem,[],s::instr
        else rem,[],s::Statement (Variable_statement (List.rev vars))::instr in

      let rem,vars,instr = List.fold_left (fun (rem,vars,instr) s ->
        match s with
          | Statement (Expression_statement (e,nid)) -> begin
            let l = m#split e in
            List.fold_left (fun (rem,vars,instr) e -> match e with
              | EBin(Eq,EVar id,exp) when IdentSet.mem id rem ->
                (IdentSet.remove id rem,(id,Some exp)::vars,instr)
              | x -> may_flush rem vars (Statement(Expression_statement (x,nid))) instr)
              (rem,vars,instr) l
          end
          | Statement _ as s -> may_flush rem vars s instr
          | Function_declaration _ as x -> (rem,vars,x::instr)
      ) (all,[],[]) sources in
      let instr = match vars with
        | [] -> (List.rev instr)
        | d ->
          let d = Statement (Variable_statement (List.rev d)) in
          List.rev (d::instr) in
      let l = IdentSet.fold (fun x acc -> (x,None)::acc) rem [] in
      match l,instr with
        | [],_ -> instr
        | l, (Statement (Variable_statement l')::rest) -> Statement (Variable_statement (List.rev_append l l')) :: rest
        | l,_ -> (Statement (Variable_statement l))::instr

    method source x =
      let x = super#source x in
      match x with
        | Function_declaration (id,params, body, nid) ->
          let all = IdentSet.diff insert_ exc_ in
          let body = m#pack all body in
          m#except id;
          Function_declaration (id,params, body, nid)
        | _ -> x

    method expression x =
      let x = super#expression x in
      match x with
        | EFun ((ident,params,body),nid) ->
          let all = IdentSet.diff insert_ exc_ in
          let body = m#pack all body in
          (match ident with
            | Some id -> m#except id;
            | None -> ());
          EFun ((ident,params,body),nid)
        | _ -> x

    method statements l =
      let l = super#statements l in
      let l = List.fold_left (fun acc x ->
        match x with
          | Expression_statement (e,nid) ->
            let l = m#split e in
            let l = List.fold_left (fun acc e -> Expression_statement (e,nid)::acc) acc l in
            l
          | x -> x::acc) [] l in
      List.rev l

end

class clean = object(m)
  inherit map as super
  method statements l =
    let l = super#statements l in
    let l = List.filter (function
        | Empty_statement -> false
        | Expression_statement (EVar _, pc) -> false
        | _ -> true) l in
    match l with
      | [] -> [Empty_statement]
      | l -> l
  method sources l =
    let l = super#sources l in
    let l = List.filter (function
        | Statement (Empty_statement) -> false
        | Statement (Expression_statement (EVar _, pc)) -> false
        | _ -> true) l in
    l
end
