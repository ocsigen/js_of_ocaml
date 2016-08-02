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
  method switch_case : Javascript.expression -> Javascript.expression
  method initialiser : (Javascript.expression * Javascript.location) -> (Javascript.expression * Javascript.location)
  method initialiser_o : (Javascript.expression * Javascript.location) option -> (Javascript.expression * Javascript.location) option
  method statement : Javascript.statement -> Javascript.statement
  method statement_o : (Javascript.statement * Javascript.location) option -> (Javascript.statement * Javascript.location) option
  method statements : Javascript.statement_list -> Javascript.statement_list
  method source : Javascript.source_element -> Javascript.source_element
  method sources : Javascript.source_elements -> Javascript.source_elements
  method ident : Javascript.ident -> Javascript.ident
  method program : Javascript.program -> Javascript.program
end


(* generic js ast walk/map *)
class map : mapper = object(m)
  method ident i = i

  method statements l = List.map (fun (s, pc) -> (m#statement s, pc)) l

  method statement s =
    match s with
    | Block b ->
        Block (m#statements b)
    | Variable_statement l ->
        Variable_statement
          (List.map (fun (id, eo) -> m#ident id, m#initialiser_o eo) l)
    | Empty_statement ->
        Empty_statement
    | Debugger_statement ->
        Debugger_statement
    | Expression_statement e ->
        Expression_statement (m#expression e)
    | If_statement (e, (s, loc), sopt) ->
        If_statement (m#expression e, (m#statement s, loc), m#statement_o sopt)
    | Do_while_statement ((s, loc), e) ->
        Do_while_statement ((m#statement s, loc), m#expression e)
    | While_statement(e, (s, loc)) ->
        While_statement(m#expression e, (m#statement s, loc))
    | For_statement(e1, e2, e3, (s, loc)) ->
        let e1 =
          match e1 with
          | Left o ->
              Left(m#expression_o o)
          | Right l ->
              Right(List.map (fun (id, eo) -> m#ident id,m#initialiser_o eo) l)
        in
        For_statement (e1, m#expression_o e2, m#expression_o e3,
                       (m#statement s, loc))
    | ForIn_statement (e1, e2, (s, loc)) ->
        let e1 =
          match e1 with
          | Left e         -> Left(m#expression e)
          | Right ((id,e)) -> Right ((m#ident id,m#initialiser_o e))
        in
        ForIn_statement (e1, m#expression e2, (m#statement s, loc))
    | Continue_statement s ->
        Continue_statement s
    | Break_statement s ->
        Break_statement s
    | Return_statement e ->
        Return_statement (m#expression_o e)
    | Labelled_statement (l, (s, loc)) ->
        Labelled_statement (l, (m#statement s, loc))
    | Throw_statement e ->
        Throw_statement (m#expression e)
    | Switch_statement (e, l, def, l') ->
        Switch_statement
          (m#expression e,
           List.map (fun (e,s) -> m#switch_case e, m#statements s) l,
           begin match def with
           | None   -> None
           | Some l -> Some (m#statements l)
           end,
           List.map (fun (e,s) -> m#switch_case e, m#statements s) l')
    | Try_statement (b, catch, final) ->
        Try_statement
          (m#statements b,
           (match catch with
            | None        -> None
            | Some (id,b) -> Some (m#ident id, m#statements b)),
           (match final with
            | None   -> None
            | Some s -> Some (m#statements s)))

  method statement_o x = match x with
    | None          -> None
    | Some (s, loc) -> Some (m#statement s, loc)

  method switch_case e = m#expression e

  method expression x = match x with
  | ESeq(e1,e2) -> ESeq(m#expression  e1, m#expression  e2)
  | ECond(e1,e2,e3) ->
    ECond(m#expression  e1,m#expression  e2,m#expression  e3)
  | EBin(b,e1,e2) ->
    EBin(b,m#expression  e1,m#expression  e2)
  | EUn(b,e1) -> EUn(b,m#expression  e1)
  | ECall(e1,e2,loc) ->
    ECall(m#expression  e1,List.map m#expression e2,loc)
  | EAccess(e1,e2) ->
    EAccess(m#expression  e1,m#expression  e2)
  | EDot(e1,id) -> EDot(m#expression  e1, id)
  | ENew(e1,Some args) ->
    ENew(m#expression  e1,Some (List.map m#expression args))
  | ENew(e1,None) ->
    ENew(m#expression  e1,None)
  | EVar v -> EVar (m#ident v)
  | EFun (idopt, params, body ,nid) ->
    let idopt = match idopt with
      | None -> None
      | Some i -> Some (m#ident i) in
    EFun (idopt, List.map m#ident params, m#sources body ,nid)
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

  method initialiser (e,pc) = (m#expression e,pc)
  method initialiser_o x = match x with
    | None -> None
    | Some i -> Some (m#initialiser i)

  method source x = match x with
    | Statement s -> Statement (m#statement s)
    | Function_declaration(id,params,body,nid) ->
      Function_declaration(m#ident id, List.map m#ident params, m#sources body,nid)

  method sources x = List.map (fun (s, loc) -> (m#source s, loc)) x

  method program x = m#sources x
end

(* var substitution *)
class subst sub = object
    inherit map
    method ident x = sub x
end

class replace_expr f = object(m)
  inherit map as super
  method expression e = try EVar (f e) with Not_found -> super#expression e

  (* do not replace constant in switch case *)
  method switch_case e =
    match e with
    | ENum _ | EStr _ -> e
    | _ -> m#expression e
end

open Util

(* this optimisation should be done at the lowest common scope *)
class share_constant = object(m)
  inherit map as super

  val count = Hashtbl.create 17

  method expression e =
    let e = match e with
      | EStr (s,`Utf8) when not(Util.has_backslash s) && Util.is_ascii s ->
        let e = EStr (s,`Bytes) in
        let n = try Hashtbl.find count e with Not_found -> 0 in
        Hashtbl.replace count e (n+1);
        e
      | EStr (_,_)
      | ENum _ ->
        let n = try Hashtbl.find count e with Not_found -> 0 in
        Hashtbl.replace count e (n+1);
        e
      | _ -> e in
    super#expression e

  (* do not replace constant in switch case *)
  method switch_case e =
    match e with
    | ENum _ | EStr _ -> e
    | _ -> m#expression e

  method sources l =
    let (revl, _) =
      List.fold_left
        (fun (l,prolog) (x, loc) ->
           match x with
           | Statement (Expression_statement (EStr _)) when prolog ->
               (x, loc) :: l, prolog
           | x ->
               (m#source x, loc)::l, false)
        ([],true) l
    in
    List.rev revl

  method program p =
    let p = super#program p in

    let all = Hashtbl.create 17 in
    Hashtbl.iter (fun x n ->
        let shareit = match x with
          (* JavaScript engines recognize the pattern
             'typeof x==="number"'; if the string is shared,
             less efficient code is generated. *)
          | EStr ("number", _) -> None
          | EStr(s,_) when n > 1 ->
            if String.length s < 20
            then Some ("str_"^s)
            else Some ("str_"^(String.sub s 0 16)^"_abr")
          | ENum f when n > 1 ->
            let s = Javascript.string_of_number f in
            let l = String.length s in
            if l > 2
            then Some ("num_"^s)
            else None
          | _ -> None in
        match shareit with
        | Some name ->
          let v = Code.Var.fresh_n name in
          Hashtbl.add all x (V v)
        | _ -> ()
      ) count ;
    if Hashtbl.length all = 0
    then p
    else
      let f = Hashtbl.find all in
      let p = (new replace_expr f)#program p in
      let all = Hashtbl.fold (fun e v acc ->
          (v, Some (e,N)) :: acc) all [] in
      (Statement (Variable_statement all), N) :: p
end

module S = Code.VarSet
type t = {
  use_name : StringSet.t;
  def_name : StringSet.t;
  def : S.t;
  use : S.t;
  count : int Javascript.IdentMap.t ;
}

let empty = {
  def = S.empty;
  use = S.empty;
  use_name = StringSet.empty;
  def_name = StringSet.empty;
  count = Javascript.IdentMap.empty;
}

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
    method get_def_name : Util.StringSet.t
    method get_def : Code.VarSet.t
    method get_use_name : Util.StringSet.t
    method get_use : Code.VarSet.t
  end

class free =
  object(m : 'test)
    inherit map as super
  val level : int = 0
  val mutable state_ : t = empty
  method state = state_

  method get_free =
    S.diff m#state.use m#state.def

  method get_def = m#state.def

  method get_free_name =
    StringSet.diff m#state.use_name m#state.def_name

  method get_def_name = m#state.def_name

  method get_use_name = m#state.use_name
  method get_use = m#state.use

  method merge_info from =
    let free_name = from#get_free_name in
    let free = from#get_free in
    let count = IdentMap.fold (fun v k acc ->
        let n = try IdentMap.find v acc with Not_found -> 0 in
        IdentMap.add v (k + n) acc
      ) from#state.count m#state.count in
    state_ <- { state_ with
      use_name = StringSet.union state_.use_name free_name;
      use = S.union state_.use free;
      count }

  method use_var x =
    let n = try IdentMap.find x state_.count with Not_found -> 0 in
    let count = IdentMap.add x (succ n) state_.count in
    match x with
      | S {name}  ->
        state_ <- { state_ with use_name = StringSet.add name state_.use_name;count }
      | V v ->
        state_ <- { state_ with use = S.add v state_.use;count  }
  method def_var x =
    let n = try IdentMap.find x state_.count with Not_found -> 0 in
    let count = IdentMap.add x (succ n) state_.count in
    match x with
    | S {name} ->
      state_ <- { state_ with def_name = StringSet.add name state_.def_name;count }
    | V v ->
      state_ <- { state_ with def = S.add v state_.def;count  }

  method expression x = match x with
    | EVar v -> m#use_var v; x
    | EFun (ident,params,body,nid) ->
      let tbody  = ({< state_ = empty; level = succ level  >} :> 'test) in
      let () = List.iter tbody#def_var params in
      let body = tbody#sources body in
      let ident = match ident with
        | Some (V v) when not(S.mem v tbody#state.use) -> None
        | Some (S {name})when not(StringSet.mem name tbody#state.use_name) -> None
        | Some id -> tbody#def_var id;ident
        | None -> None in
      tbody#block params;
      m#merge_info tbody;
      EFun (ident,params,body,nid)
    | _ -> super#expression x

  method source x = match x with
    | Function_declaration (id,params, body, nid) ->
      let tbody = {< state_ = empty; level = succ level >} in
      let () = List.iter tbody#def_var params in
      let body = tbody#sources body in
      tbody#block params;
      m#def_var id;
      m#merge_info tbody;
      Function_declaration (id,params, body, nid)
    | _ -> super#source x

  method block ?catch:_ _ = ()


  method statement x = match x with
    | Variable_statement l ->
      let l = List.map (fun (id,eopt) ->
        m#def_var id;
        match eopt with
          | None -> (id,None)
          | Some (e,pc) ->
            let e = m#expression e in
            (id,Some (e,pc))) l in
      Variable_statement l
    | For_statement (Right l, e2, e3, (s, loc)) ->
      let l = List.map (fun (id,eopt) ->
          m#def_var id;
          match eopt with
            | None -> (id,None)
            | Some (e,pc) ->
              let e = m#expression e in
              (id,Some (e,pc))) l in
      For_statement (Right l, m#expression_o e2, m#expression_o e3,
                     (m#statement s, loc))
    | ForIn_statement (Right (id,eopt), e2, (s, loc)) ->
      m#def_var id;
      let r = match eopt with
        | None -> (id,None)
        | Some (e,pc) ->
          let e = m#expression e in
          (id,Some (e,pc)) in
      ForIn_statement (Right r,m#expression e2, (m#statement s, loc))
    | Try_statement (b,w,f) ->
      let b = m#statements b in
      let tbody = {< state_ = empty; level = level >} in
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
            | S {name} -> set,StringSet.remove name sets
            | V i -> S.remove i set, sets in
          let def,def_name = clean tbody#state.def tbody#state.def_name in
          let use,use_name = clean tbody#state.use tbody#state.use_name in
          let count = IdentMap.fold (fun v k acc ->
              let n = try IdentMap.find v acc with Not_found -> 0 in
              IdentMap.add v (k + n) acc
            ) tbody#state.count m#state.count in
          state_ <- {
            use = S.union state_.use use;
            use_name = StringSet.union state_.use_name use_name;
            def = S.union state_.def def;
            def_name = StringSet.union state_.def_name def_name;
            count};
          Some (id,block)
      in
      let f = match f with
        | None -> None
        | Some block -> Some (m#statements block)
      in
      Try_statement (b,w,f)
    | _ -> super#statement x
  end



class rename_variable keeps = object
  inherit free as super

  val mutable sub_ = new subst (fun x -> x)

  method merge_info from =
    super#merge_info from;
    let h = Hashtbl.create 17 in
    let _ = StringSet.iter (fun name ->
        if StringSet.mem name keeps
        then ()
        else
          let v = Code.Var.fresh_n name in
          Hashtbl.add h name v) from#state.def_name in
    let f = function
      | (S {name}) when Hashtbl.mem h name -> V (Hashtbl.find h name)
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
      | Try_statement (b,w,f) ->
        let w = match w with
          | Some(S {name},block) ->
            let v = Code.Var.fresh_n name in
            let sub = function
              | S {name=name'} when name' = name -> V v
              | x -> x in
            let s = new subst sub in
            Some(V v ,s#statements block)
          | x -> x in
        Try_statement (b,w,f)
      | _ -> x


  method source x =
    let x = super#source x in
    match x with
      | Function_declaration (id,params,body,nid) ->
        Function_declaration (id,List.map sub_#ident params,sub_#sources body,nid)
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
          | Some (e,_) -> Some (EBin (Eq,EVar id,e))) l

    method private translate_st l =
      let l = m#translate l in
      match l with
        | [] -> Empty_statement
        | x::l -> Expression_statement (List.fold_left (fun acc e -> ESeq(acc,e)) x l)

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
        | For_statement (Right l,e2,e3,s) ->
          For_statement (Left (m#translate_ex l), e2, e3, s)
        | ForIn_statement(Right (id,op),e2,s) ->
          (match op with
            | Some _ -> assert false
            | None -> ());
          ForIn_statement(Left (EVar id),e2,s)
        | Try_statement (b,w,f) ->
          (match w with
            | None -> ()
            | Some (id,_) -> m#except id);
          Try_statement (b,w,f)
        | s -> s

    method block ?(catch=false) params =
      ignore catch;
      List.iter m#except params;
      super#block params;

    method merge_info from =
      super#merge_info from;
      let all = S.fold (fun e acc -> IdentSet.add (V e) acc) from#state.def IdentSet.empty in
      let all = StringSet.fold (fun e acc -> IdentSet.add (S {name=e;var=None}) acc) from#state.def_name all in
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
        else rem,[],s::(Statement (Variable_statement (List.rev vars)), N)::instr in

      let rem,vars,instr = List.fold_left (fun (rem,vars,instr) (s, loc) ->
        match s with
          | Statement (Expression_statement e) -> begin
            let l = m#split e in
            List.fold_left (fun (rem,vars,instr) e -> match e with
              | EBin(Eq,EVar id,exp) when IdentSet.mem id rem ->
                (IdentSet.remove id rem,(id,Some (exp,N))::vars,instr)
              | x -> may_flush rem vars (Statement(Expression_statement x), N) instr)
              (rem,vars,instr) l
          end
          | Statement _ as s -> may_flush rem vars (s, loc) instr
          | Function_declaration _ as x -> (rem,vars,(x, loc)::instr)
      ) (all,[],[]) sources in
      let instr = match vars with
        | [] -> (List.rev instr)
        | d ->
          let d = Statement (Variable_statement (List.rev d)) in
          List.rev ((d, N)::instr) in
      let l = IdentSet.fold (fun x acc -> (x,None)::acc) rem [] in
      match l,instr with
        | [],_ -> instr
        | l, (Statement (Variable_statement l'), loc)::rest ->
          (Statement (Variable_statement (List.rev_append l l')), loc) :: rest
        | l,_ -> (Statement (Variable_statement l), N)::instr

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
        | EFun (ident,params,body,nid) ->
          let all = IdentSet.diff insert_ exc_ in
          let body = m#pack all body in
          (match ident with
            | Some id -> m#except id;
            | None -> ());
          EFun (ident,params,body,nid)
        | _ -> x

    method statements l =
      let l = super#statements l in
      let l = List.fold_left (fun acc (x, loc) ->
        match x with
          | Expression_statement e ->
            let l = m#split e in
            let l = List.fold_left (fun acc e -> (Expression_statement e, N)::acc) acc l in
            l
          | _ -> (x, loc)::acc) [] l in
      List.rev l

end

class clean = object(m)
  inherit map as super

  method statements l =
    let rev_append_st x l = match x with
      | (Block b, _) -> List.rev_append b l
      | x -> x::l in
    let l = super#statements l in
    let vars_rev,vars_loc,instr_rev =
      List.fold_left (fun (vars_rev,vars_loc,instr_rev) (x, loc) ->
        match x with
          | Variable_statement l when Option.Optim.compact () ->
            let vars_loc = match vars_loc with
              | Pi _ as x -> x
              | _           -> loc in
            (List.rev_append l vars_rev,vars_loc,instr_rev)
          | Empty_statement
          | Expression_statement (EVar _) -> vars_rev,vars_loc,instr_rev
          | _ when vars_rev = [] -> ([],vars_loc,rev_append_st (x, loc) instr_rev)
          | _ -> ([],vars_loc,rev_append_st (x, loc) ((Variable_statement (List.rev vars_rev), vars_loc)::instr_rev))
      ) ([],N,[]) l in
    let instr_rev = match vars_rev with
      | [] -> instr_rev
      | vars_rev -> (Variable_statement (List.rev vars_rev), vars_loc) :: instr_rev
    in List.rev instr_rev

   method statement s =
     let s = super#statement s in
    let b = function
      | Block [], loc -> (Empty_statement, loc)
      | Block [x], _ -> x
      | b -> b in
    let bopt = function
      | Some (Block [], _) -> None
      | Some (Block [x], _) -> Some x
      | Some b -> Some b
      | None -> None in
     match s with
    | If_statement (if',then',else') -> If_statement (if',b then',bopt else')
    | Do_while_statement (do',while') -> Do_while_statement (b do',while')
    | While_statement (cond,st) -> While_statement (cond,b st)
    | For_statement (p1,p2,p3,st) -> For_statement (p1,p2,p3,b st)
    | ForIn_statement (param,e,st) -> ForIn_statement (param,e,b st)
    | Switch_statement(e,l,Some [],[]) -> Switch_statement(e,l,None,[])
    | s -> s

  method sources l =
    let l = super#sources l in
    let append_st st_rev sources_rev =
      let st = m#statements (List.rev st_rev) in
      let st = List.map (fun (s, loc) -> (Statement s, loc)) st in
      List.rev_append st sources_rev in

    let (st_rev,sources_rev) = List.fold_left (fun (st_rev,sources_rev) (x, loc) ->
        match x with
          | Statement s -> (s, loc)::st_rev,sources_rev
          | x when st_rev = [] -> [],(x, loc)::sources_rev
          | x -> [],((x, loc)::(append_st st_rev sources_rev))
      ) ([],[]) l in
    let sources_rev = match st_rev with
      | [] -> sources_rev
      | st_rev -> append_st st_rev sources_rev in
    List.rev sources_rev
end

let translate_assign_op = function
  | Div -> SlashEq
  | Mod -> ModEq
  | Lsl -> LslEq
  | Asr -> AsrEq
  | Lsr -> LsrEq
  | Band -> BandEq
  | Bor -> BorEq
  | Bxor -> BxorEq
  | Mul -> StarEq
  | Plus -> PlusEq
  | Minus -> MinusEq
  | _ -> assert false

let assign_op = function
  | (exp,EBin (Plus, exp',exp'')) ->
    begin
      match exp=exp',exp=exp'' with
        | false,false -> None
        | true, false ->
          if exp'' = ENum 1.
          then Some (EUn (IncrB,exp))
          else Some (EBin (PlusEq,exp,exp''))
        | false, true ->
          if exp' = ENum 1.
          then Some (EUn (IncrB,exp))
          else Some (EBin (PlusEq,exp,exp'))
        | true, true ->
          Some(EBin(StarEq,exp,ENum 2.))
    end
  | (exp,EBin (Minus, exp',y)) when exp = exp' ->
    if y = ENum 1.
    then Some (EUn (DecrB, exp))
    else Some (EBin (MinusEq, exp,y))
  | (exp,EBin (Mul, exp',exp'')) ->
    begin
      match exp=exp',exp=exp'' with
        | false,false -> None
        | true,_ ->
          Some (EBin (StarEq, exp,exp''))
        | _,true ->
          Some (EBin (StarEq, exp,exp'))
    end
  | (exp,EBin (Div | Mod | Lsl | Asr |  Lsr | Band | Bxor | Bor as unop, exp',y)) when exp = exp' ->
    Some (EBin (translate_assign_op unop, exp,y))
  | _ -> None

class simpl = object(m)
  inherit map as super
  method expression e =
    let e = super#expression e in
    match e with
    | EBin (Plus,e1,e2) -> begin
        match e2,e1 with
        | ENum n, _ when n < 0. ->
          EBin (Minus, e1, ENum (-. n))
        | _,ENum n when n < 0. ->
          EBin (Minus, e2, ENum (-. n))
        | ENum 0., (ENum _ as x) -> x
        | (ENum _ as x), ENum 0. -> x
        | _ -> e
      end
    | EBin (Minus,e1,e2) -> begin
        match e2,e1 with
        | ENum n,_  when n < 0. ->
          EBin (Plus, e1, ENum (-. n))
        | (ENum _ as x), ENum 0. -> x
        | _ -> e
      end
    | _ -> e

  method statement s =
    let s = super#statement s in
    match s with
    | Block [x] -> fst x
    | _ -> s

  method statements s =
    let s = super#statements s in
    List.fold_right (fun (st, loc) rem ->
        match st with
        | If_statement(
            cond,
            (Return_statement (Some e1), _),
            Some (Return_statement (Some e2), _)) ->
          (Return_statement (Some (ECond(cond,e1,e2))), loc)::rem
        | If_statement(
            cond,
            (Expression_statement (EBin(Eq,v1,e1)), _),
            Some (Expression_statement (EBin(Eq,v2,e2)),_)) when v1 = v2 ->
          (Expression_statement (EBin(Eq,v1,ECond(cond,e1,e2))),loc)::rem

        | Variable_statement l1 ->
          let x = List.map (function
              | (ident,None) -> (Variable_statement [(ident,None)], loc)
              | (ident,Some (exp,pc)) ->
              match assign_op (EVar ident,exp) with
              | Some e -> (Expression_statement e, loc)
              | None -> (Variable_statement [(ident,Some (exp,pc))],loc)) l1 in
          x@rem
        | _ -> (st, loc)::rem
      ) s []


  method sources l =
    let l = super#sources l in
    let append_st st_rev sources_rev =
      let st = m#statements (List.rev st_rev) in
      let st = List.map (function
          | (Variable_statement
              [addr, Some (EFun (None, params, body, loc'), loc)], _) ->
            (Function_declaration (addr, params, body, loc'), loc)
          | (s, loc) -> (Statement s, loc)) st in
      List.rev_append st sources_rev in

    let (st_rev,sources_rev) = List.fold_left (fun (st_rev,sources_rev) x ->
        match x with
          | (Statement s, loc) -> (s, loc)::st_rev,sources_rev
          | x when st_rev = [] -> [],x::sources_rev
          | x -> [],(x::(append_st st_rev sources_rev))
      ) ([],[]) l in
    let sources_rev = match st_rev with
      | [] -> sources_rev
      | st_rev -> append_st st_rev sources_rev in
    List.rev sources_rev


end
