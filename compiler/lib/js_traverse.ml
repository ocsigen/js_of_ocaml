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

open! Stdlib
open Javascript

class type mapper =
  object
    method expression : Javascript.expression -> Javascript.expression

    method expression_o : Javascript.expression option -> Javascript.expression option

    method switch_case : Javascript.expression -> Javascript.expression

    method initialiser :
         Javascript.expression * Javascript.location
      -> Javascript.expression * Javascript.location

    method initialiser_o :
         (Javascript.expression * Javascript.location) option
      -> (Javascript.expression * Javascript.location) option

    method variable_declaration :
      Javascript.variable_declaration -> Javascript.variable_declaration

    method statement : Javascript.statement -> Javascript.statement

    method statement_o :
         (Javascript.statement * Javascript.location) option
      -> (Javascript.statement * Javascript.location) option

    method statements : Javascript.statement_list -> Javascript.statement_list

    method source : Javascript.source_element -> Javascript.source_element

    method sources : Javascript.source_elements -> Javascript.source_elements

    method ident : Javascript.ident -> Javascript.ident

    method program : Javascript.program -> Javascript.program
  end

(* generic js ast walk/map *)
class map : mapper =
  object (m)
    method ident i = i

    method statements l = List.map l ~f:(fun (s, pc) -> m#statement s, pc)

    method variable_declaration (id, eo) = m#ident id, m#initialiser_o eo

    method statement s =
      match s with
      | Block b -> Block (m#statements b)
      | Variable_statement l -> Variable_statement (List.map l ~f:m#variable_declaration)
      | Empty_statement -> Empty_statement
      | Debugger_statement -> Debugger_statement
      | Expression_statement e -> Expression_statement (m#expression e)
      | If_statement (e, (s, loc), sopt) ->
          If_statement (m#expression e, (m#statement s, loc), m#statement_o sopt)
      | Do_while_statement ((s, loc), e) ->
          Do_while_statement ((m#statement s, loc), m#expression e)
      | While_statement (e, (s, loc)) ->
          While_statement (m#expression e, (m#statement s, loc))
      | For_statement (e1, e2, e3, (s, loc)) ->
          let e1 =
            match e1 with
            | Left o -> Left (m#expression_o o)
            | Right l -> Right (List.map l ~f:(fun d -> m#variable_declaration d))
          in
          For_statement (e1, m#expression_o e2, m#expression_o e3, (m#statement s, loc))
      | ForIn_statement (e1, e2, (s, loc)) ->
          let e1 =
            match e1 with
            | Left e -> Left (m#expression e)
            | Right d -> Right (m#variable_declaration d)
          in
          ForIn_statement (e1, m#expression e2, (m#statement s, loc))
      | Continue_statement s -> Continue_statement s
      | Break_statement s -> Break_statement s
      | Return_statement e -> Return_statement (m#expression_o e)
      | Labelled_statement (l, (s, loc)) -> Labelled_statement (l, (m#statement s, loc))
      | Throw_statement e -> Throw_statement (m#expression e)
      | Switch_statement (e, l, def, l') ->
          Switch_statement
            ( m#expression e
            , List.map l ~f:(fun (e, s) -> m#switch_case e, m#statements s)
            , (match def with
              | None -> None
              | Some l -> Some (m#statements l))
            , List.map l' ~f:(fun (e, s) -> m#switch_case e, m#statements s) )
      | Try_statement (b, catch, final) ->
          Try_statement
            ( m#statements b
            , (match catch with
              | None -> None
              | Some (id, b) -> Some (m#ident id, m#statements b))
            , match final with
              | None -> None
              | Some s -> Some (m#statements s) )

    method statement_o x =
      match x with
      | None -> None
      | Some (s, loc) -> Some (m#statement s, loc)

    method switch_case e = m#expression e

    method expression x =
      match x with
      | ESeq (e1, e2) -> ESeq (m#expression e1, m#expression e2)
      | ECond (e1, e2, e3) -> ECond (m#expression e1, m#expression e2, m#expression e3)
      | EBin (b, e1, e2) -> EBin (b, m#expression e1, m#expression e2)
      | EUn (b, e1) -> EUn (b, m#expression e1)
      | ECall (e1, e2, loc) ->
          ECall
            ( m#expression e1
            , List.map e2 ~f:(fun (e, spread) -> m#expression e, spread)
            , loc )
      | EAccess (e1, e2) -> EAccess (m#expression e1, m#expression e2)
      | EDot (e1, id) -> EDot (m#expression e1, id)
      | ENew (e1, Some args) ->
          ENew
            ( m#expression e1
            , Some (List.map args ~f:(fun (e, spread) -> m#expression e, spread)) )
      | ENew (e1, None) -> ENew (m#expression e1, None)
      | EVar v -> EVar (m#ident v)
      | EFun (idopt, params, body, nid) ->
          let idopt =
            match idopt with
            | None -> None
            | Some i -> Some (m#ident i)
          in
          EFun (idopt, List.map params ~f:m#ident, m#sources body, nid)
      | EArr l -> EArr (List.map l ~f:(fun x -> m#expression_o x))
      | EObj l -> EObj (List.map l ~f:(fun (i, e) -> i, m#expression e))
      | (EStr _ as x) | (EBool _ as x) | (ENum _ as x) | (ERegexp _ as x) -> x

    method expression_o x =
      match x with
      | None -> None
      | Some s -> Some (m#expression s)

    method initialiser (e, pc) = m#expression e, pc

    method initialiser_o x =
      match x with
      | None -> None
      | Some i -> Some (m#initialiser i)

    method source x =
      match x with
      | Statement s -> Statement (m#statement s)
      | Function_declaration (id, params, body, nid) ->
          Function_declaration
            (m#ident id, List.map params ~f:m#ident, m#sources body, nid)

    method sources x = List.map x ~f:(fun (s, loc) -> m#source s, loc)

    method program x = m#sources x
  end

class type iterator =
  object
    method expression : Javascript.expression -> unit

    method expression_o : Javascript.expression option -> unit

    method switch_case : Javascript.expression -> unit

    method initialiser : Javascript.expression * Javascript.location -> unit

    method initialiser_o : (Javascript.expression * Javascript.location) option -> unit

    method variable_declaration : Javascript.variable_declaration -> unit

    method statement : Javascript.statement -> unit

    method statement_o : (Javascript.statement * Javascript.location) option -> unit

    method statements : Javascript.statement_list -> unit

    method source : Javascript.source_element -> unit

    method sources : Javascript.source_elements -> unit

    method ident : Javascript.ident -> unit

    method program : Javascript.program -> unit
  end

(* generic js ast iterator *)
class iter : iterator =
  object (m)
    method ident _ = ()

    method statements l = List.iter l ~f:(fun (s, _) -> m#statement s)

    method variable_declaration (id, eo) =
      m#ident id;
      m#initialiser_o eo

    method statement s =
      match s with
      | Block b -> m#statements b
      | Variable_statement l -> List.iter l ~f:m#variable_declaration
      | Empty_statement -> ()
      | Debugger_statement -> ()
      | Expression_statement e -> m#expression e
      | If_statement (e, (s, _), sopt) ->
          m#expression e;
          m#statement s;
          m#statement_o sopt
      | Do_while_statement ((s, _), e) ->
          m#statement s;
          m#expression e
      | While_statement (e, (s, _)) ->
          m#expression e;
          m#statement s
      | For_statement (e1, e2, e3, (s, _)) ->
          (match e1 with
          | Left o -> m#expression_o o
          | Right l -> List.iter l ~f:(fun d -> m#variable_declaration d));
          m#expression_o e2;
          m#expression_o e3;
          m#statement s
      | ForIn_statement (e1, e2, (s, _)) ->
          (match e1 with
          | Left e -> m#expression e
          | Right d -> m#variable_declaration d);

          m#expression e2;
          m#statement s
      | Continue_statement _ -> ()
      | Break_statement _ -> ()
      | Return_statement e -> m#expression_o e
      | Labelled_statement (_, (s, _)) -> m#statement s
      | Throw_statement e -> m#expression e
      | Switch_statement (e, l, def, l') ->
          m#expression e;
          List.iter l ~f:(fun (e, s) ->
              m#switch_case e;
              m#statements s);
          (match def with
          | None -> ()
          | Some l -> m#statements l);
          List.iter l' ~f:(fun (e, s) ->
              m#switch_case e;
              m#statements s)
      | Try_statement (b, catch, final) -> (
          m#statements b;
          (match catch with
          | None -> ()
          | Some (id, b) ->
              m#ident id;
              m#statements b);
          match final with
          | None -> ()
          | Some s -> m#statements s)

    method statement_o x =
      match x with
      | None -> ()
      | Some (s, _) -> m#statement s

    method switch_case e = m#expression e

    method expression x =
      match x with
      | ESeq (e1, e2) ->
          m#expression e1;
          m#expression e2
      | ECond (e1, e2, e3) ->
          m#expression e1;
          m#expression e2;
          m#expression e3
      | EBin (_, e1, e2) ->
          m#expression e1;
          m#expression e2
      | EUn (_, e1) -> m#expression e1
      | ECall (e1, e2, _) ->
          m#expression e1;
          List.iter e2 ~f:(fun (e, _) -> m#expression e)
      | EAccess (e1, e2) ->
          m#expression e1;
          m#expression e2
      | EDot (e1, _) -> m#expression e1
      | ENew (e1, Some args) ->
          m#expression e1;
          List.iter args ~f:(fun (e, _) -> m#expression e)
      | ENew (e1, None) -> m#expression e1
      | EVar v -> m#ident v
      | EFun (idopt, params, body, _) ->
          (match idopt with
          | None -> ()
          | Some i -> m#ident i);
          List.iter params ~f:m#ident;
          m#sources body
      | EArr l -> List.iter l ~f:(fun x -> m#expression_o x)
      | EObj l -> List.iter l ~f:(fun (_, e) -> m#expression e)
      | EStr _ | EBool _ | ENum _ | ERegexp _ -> ()

    method expression_o x =
      match x with
      | None -> ()
      | Some s -> m#expression s

    method initialiser (e, _) = m#expression e

    method initialiser_o x =
      match x with
      | None -> ()
      | Some i -> m#initialiser i

    method source x =
      match x with
      | Statement s -> m#statement s
      | Function_declaration (id, params, body, _) ->
          m#ident id;
          List.iter params ~f:m#ident;
          m#sources body

    method sources x = List.iter x ~f:(fun (s, _) -> m#source s)

    method program x = m#sources x
  end

(* var substitution *)
class subst sub =
  object
    inherit map

    method ident x = sub x
  end

class map_for_share_constant =
  object (m)
    inherit map as super

    method expression e =
      match e with
      (* JavaScript engines recognize the pattern
         'typeof x==="number"'; if the string is shared,
         less efficient code is generated. *)
      | EBin (op, EUn (Typeof, e1), (EStr _ as e2)) ->
          EBin (op, EUn (Typeof, super#expression e1), e2)
      | EBin (op, (EStr _ as e1), EUn (Typeof, e2)) ->
          EBin (op, e1, EUn (Typeof, super#expression e2))
      (* Some js bundler get confused when the argument
         of 'require' is not a literal *)
      | ECall
          ( EVar (S { var = None; name = Utf8 "requires"; _ })
          , [ (EStr _, `Not_spread) ]
          , _ ) -> e
      | _ -> super#expression e

    (* do not replace constant in switch case *)
    method switch_case e =
      match e with
      | ENum _ | EStr _ -> e
      | _ -> m#expression e

    method sources l =
      match l with
      | [] -> []
      | ((Statement (Expression_statement (EStr _)), _) as prolog) :: rest ->
          prolog :: List.map rest ~f:(fun (x, loc) -> m#source x, loc)
      | rest -> List.map rest ~f:(fun (x, loc) -> m#source x, loc)
  end

class replace_expr f =
  object
    inherit map_for_share_constant as super

    method expression e = try EVar (f e) with Not_found -> super#expression e
  end

(* this optimisation should be done at the lowest common scope *)
class share_constant =
  object
    inherit map_for_share_constant as super

    val count = Hashtbl.create 17

    method expression e =
      let e =
        match e with
        | EStr _ | ENum _ ->
            let n = try Hashtbl.find count e with Not_found -> 0 in
            Hashtbl.replace count e (n + 1);
            e
        | _ -> e
      in
      super#expression e

    method program p =
      let p = super#program p in
      let all = Hashtbl.create 17 in
      Hashtbl.iter
        (fun x n ->
          let shareit =
            match x with
            | EStr (Utf8 s) when n > 1 ->
                if String.length s < 20
                then Some ("str_" ^ s)
                else Some ("str_" ^ String.sub s ~pos:0 ~len:16 ^ "_abr")
            | ENum s when n > 1 ->
                let s = Javascript.Num.to_string s in
                let l = String.length s in
                if l > 2 then Some ("num_" ^ s) else None
            | _ -> None
          in
          match shareit with
          | Some name ->
              let v = Code.Var.fresh_n name in
              Hashtbl.add all x (V v)
          | _ -> ())
        count;
      if Hashtbl.length all = 0
      then p
      else
        let f = Hashtbl.find all in
        let p = (new replace_expr f)#program p in
        let all = Hashtbl.fold (fun e v acc -> (v, Some (e, N)) :: acc) all [] in
        (Statement (Variable_statement all), N) :: p
  end

module S = Code.Var.Set

type t =
  { use_name : Utf8_string_set.t
  ; def_name : Utf8_string_set.t
  ; def : S.t
  ; use : S.t
  }

let empty =
  { def = S.empty
  ; use = S.empty
  ; use_name = Utf8_string_set.empty
  ; def_name = Utf8_string_set.empty
  }

(* def/used/free variable *)

type block =
  | Catch of ident
  | Params of ident list

class type freevar =
  object ('a)
    inherit mapper

    method merge_info : 'a -> unit

    method block : block -> unit

    method state : t

    method def_var : Javascript.ident -> unit

    method use_var : Javascript.ident -> unit

    method get_count : int Javascript.IdentMap.t

    method get_free_name : Utf8_string_set.t

    method get_free : Code.Var.Set.t

    method get_def_name : Utf8_string_set.t

    method get_def : Code.Var.Set.t

    method get_use_name : Utf8_string_set.t

    method get_use : Code.Var.Set.t
  end

class free =
  object (m : 'test)
    inherit map as super

    val level : int = 0

    val mutable state_ : t = empty

    val count = ref Javascript.IdentMap.empty

    method state = state_

    method get_count = !count

    method get_free = S.diff m#state.use m#state.def

    method get_def = m#state.def

    method get_free_name = Utf8_string_set.diff m#state.use_name m#state.def_name

    method get_def_name = m#state.def_name

    method get_use_name = m#state.use_name

    method get_use = m#state.use

    method merge_info from =
      let free_name = from#get_free_name in
      let free = from#get_free in
      state_ <-
        { state_ with
          use_name = Utf8_string_set.union state_.use_name free_name
        ; use = S.union state_.use free
        }

    method use_var x =
      let n = try IdentMap.find x !count with Not_found -> 0 in
      count := IdentMap.add x (succ n) !count;
      (* FIXME: S ident are raw Utf-8 ident (including escaped sequences).
         They need to be normalized for the analysis to be correct. *)
      match x with
      | S { name; _ } ->
          state_ <- { state_ with use_name = Utf8_string_set.add name state_.use_name }
      | V v -> state_ <- { state_ with use = S.add v state_.use }

    method def_var x =
      let n = try IdentMap.find x !count with Not_found -> 0 in
      count := IdentMap.add x (succ n) !count;
      match x with
      | S { name; _ } ->
          state_ <- { state_ with def_name = Utf8_string_set.add name state_.def_name }
      | V v -> state_ <- { state_ with def = S.add v state_.def }

    method expression x =
      match x with
      | EVar v ->
          m#use_var v;
          x
      | EFun (ident, params, body, nid) ->
          let tbody = ({<state_ = empty; level = succ level>} :> 'test) in
          let () = List.iter params ~f:tbody#def_var in
          let body = tbody#sources body in
          let ident =
            match ident with
            | Some (V v) when not (S.mem v tbody#state.use) -> None
            | Some (S { name; _ })
              when not (Utf8_string_set.mem name tbody#state.use_name) -> None
            | Some id ->
                tbody#def_var id;
                ident
            | None -> None
          in
          tbody#block (Params params);
          m#merge_info tbody;
          EFun (ident, params, body, nid)
      | _ -> super#expression x

    method source x =
      match x with
      | Function_declaration (id, params, body, nid) ->
          let tbody = {<state_ = empty; level = succ level>} in
          let () = List.iter params ~f:tbody#def_var in
          let body = tbody#sources body in
          tbody#block (Params params);
          m#def_var id;
          m#merge_info tbody;
          Function_declaration (id, params, body, nid)
      | Statement _ -> super#source x

    method block _ = ()

    method variable_declaration ((id, _) as d) =
      m#def_var id;
      super#variable_declaration d

    method statement x =
      match x with
      | Try_statement (b, w, f) ->
          let b = m#statements b in
          let same_level = level in
          let tbody = {<state_ = empty; level = same_level>} in
          let w =
            match w with
            | None -> None
            | Some (id, block) ->
                let block = tbody#statements block in
                tbody#block (Catch id);
                (* special merge here *)
                (* we need to propagate both def and use .. *)
                (* .. except the use of 'id' since its scope is limited
                   to 'block' *)
                let clean set sets =
                  match id with
                  | S { name; _ } -> set, Utf8_string_set.remove name sets
                  | V i -> S.remove i set, sets
                in
                let def, def_name = tbody#state.def, tbody#state.def_name in
                let use, use_name = clean tbody#state.use tbody#state.use_name in
                state_ <-
                  { use = S.union state_.use use
                  ; use_name = Utf8_string_set.union state_.use_name use_name
                  ; def = S.union state_.def def
                  ; def_name = Utf8_string_set.union state_.def_name def_name
                  };
                Some (id, block)
          in
          let f =
            match f with
            | None -> None
            | Some block -> Some (m#statements block)
          in
          Try_statement (b, w, f)
      | _ -> super#statement x
  end

class rename_variable =
  object (m)
    inherit map as super

    val subst = StringMap.empty

    val decl = StringSet.empty

    method private update_state ident params body =
      let declared_names = ref StringSet.empty in
      let decl_var x =
        match x with
        | S { name = Utf8 name; _ } ->
            declared_names := StringSet.add name !declared_names
        | _ -> ()
      in
      Option.iter ~f:decl_var ident;
      List.iter ~f:decl_var params;
      (object
         inherit iter as super

         method expression _ = ()

         method source x =
           match x with
           | Function_declaration (id, _, _, _) -> decl_var id
           | Statement _ -> super#source x

         method variable_declaration (id, _) = decl_var id
      end)
        #sources
        body;
      {<subst = StringSet.fold
                  (fun name subst -> StringMap.add name (Code.Var.fresh_n name) subst)
                  !declared_names
                  subst
       ; decl = !declared_names>}

    method ident x =
      match x with
      | V _ -> x
      | S { name = Utf8 name; _ } -> (
          try V (StringMap.find name subst) with Not_found -> x)

    method expression e =
      match e with
      | EFun (ident, params, body, nid) ->
          let m' = m#update_state ident params body in
          EFun
            ( Option.map ident ~f:m'#ident
            , List.map params ~f:m'#ident
            , m'#sources body
            , nid )
      | _ -> super#expression e

    method statement s =
      match s with
      | Try_statement (b, Some ((S { name = Utf8 name; _ } as id), block), final)
        when not (StringSet.mem name decl) ->
          (* If [name] is declared in [block] but not outside, then
             we cannot replace [id] by a fresh variable. As a fast
             approximation, we only use a fresh variable when [name]
             is not declared. *)
          Try_statement
            ( m#statements b
            , (let m' = {<subst = StringMap.add name (Code.Var.fresh_n name) subst>} in
               Some (m'#ident id, m'#statements block))
            , match final with
              | None -> None
              | Some s -> Some (m#statements s) )
      | _ -> super#statement s

    method source s =
      match s with
      | Function_declaration (id, params, body, nid) ->
          let m' = m#update_state None params body in
          Function_declaration
            (m#ident id, List.map params ~f:m'#ident, m'#sources body, nid)
      | _ -> super#source s
  end

class compact_vardecl =
  object (m)
    inherit free as super

    val mutable exc_ = IdentSet.empty

    val mutable insert_ = IdentSet.empty

    method exc = exc_

    method private translate l =
      List.filter_map l ~f:(fun (id, eopt) ->
          match eopt with
          | None -> None
          | Some (e, _) -> Some (EBin (Eq, EVar id, e)))

    method private translate_st l =
      let l = m#translate l in
      match l with
      | [] -> Empty_statement
      | x :: l ->
          Expression_statement (List.fold_left l ~init:x ~f:(fun acc e -> ESeq (acc, e)))

    method private translate_ex l =
      let l = m#translate l in
      match l with
      | [] -> None
      | x :: l -> Some (List.fold_left l ~init:x ~f:(fun acc e -> ESeq (acc, e)))

    method private except e = exc_ <- IdentSet.add e exc_

    method statement s =
      let s = super#statement s in
      match s with
      | Variable_statement l -> m#translate_st l
      | For_statement (Right l, e2, e3, s) ->
          For_statement (Left (m#translate_ex l), e2, e3, s)
      | ForIn_statement (Right (id, op), e2, s) ->
          (match op with
          | Some _ -> assert false
          | None -> ());
          ForIn_statement (Left (EVar id), e2, s)
      | Try_statement (b, w, f) ->
          (match w with
          | None -> ()
          | Some (id, _) -> m#except id);
          Try_statement (b, w, f)
      | s -> s

    method block block =
      (match block with
      | Catch e -> m#except e
      | Params p -> List.iter p ~f:m#except);
      super#block block

    method merge_info from =
      super#merge_info from;
      let all =
        S.fold (fun e acc -> IdentSet.add (V e) acc) from#state.def IdentSet.empty
      in
      let all =
        Utf8_string_set.fold
          (fun e acc -> IdentSet.add (ident e) acc)
          from#state.def_name
          all
      in
      insert_ <- IdentSet.diff all from#exc

    method private split x =
      let rec loop = function
        | ESeq (e1, e2) -> loop e1 @ loop e2
        | e -> [ e ]
      in
      loop x

    method private pack all sources =
      let may_flush rem vars s instr =
        if List.is_empty vars
        then rem, [], s :: instr
        else rem, [], s :: (Statement (Variable_statement (List.rev vars)), N) :: instr
      in
      let rem, vars, instr =
        List.fold_left sources ~init:(all, [], []) ~f:(fun (rem, vars, instr) (s, loc) ->
            match s with
            | Statement (Expression_statement e) ->
                let l = m#split e in
                List.fold_left l ~init:(rem, vars, instr) ~f:(fun (rem, vars, instr) e ->
                    match e with
                    | EBin (Eq, EVar id, exp) when IdentSet.mem id rem ->
                        IdentSet.remove id rem, (id, Some (exp, N)) :: vars, instr
                    | x ->
                        may_flush rem vars (Statement (Expression_statement x), N) instr)
            | Statement _ as s -> may_flush rem vars (s, loc) instr
            | Function_declaration _ as x -> rem, vars, (x, loc) :: instr)
      in
      let instr =
        match vars with
        | [] -> List.rev instr
        | d ->
            let d = Statement (Variable_statement (List.rev d)) in
            List.rev ((d, N) :: instr)
      in
      let l = IdentSet.fold (fun x acc -> (x, None) :: acc) rem [] in
      match l, instr with
      | [], _ -> instr
      | l, (Statement (Variable_statement l'), loc) :: rest ->
          (Statement (Variable_statement (List.rev_append l l')), loc) :: rest
      | l, _ -> (Statement (Variable_statement l), N) :: instr

    method source x =
      let x = super#source x in
      match x with
      | Function_declaration (id, params, body, nid) ->
          let all = IdentSet.diff insert_ exc_ in
          let body = m#pack all body in
          m#except id;
          Function_declaration (id, params, body, nid)
      | Statement _ -> x

    method expression x =
      let x = super#expression x in
      match x with
      | EFun (ident, params, body, nid) ->
          let all = IdentSet.diff insert_ exc_ in
          let body = m#pack all body in
          (match ident with
          | Some id -> m#except id
          | None -> ());
          EFun (ident, params, body, nid)
      | _ -> x

    method statements l =
      let l = super#statements l in
      let l =
        List.fold_left l ~init:[] ~f:(fun acc (x, loc) ->
            match x with
            | Expression_statement e ->
                let l = m#split e in
                let l =
                  List.fold_left l ~init:acc ~f:(fun acc e ->
                      (Expression_statement e, N) :: acc)
                in
                l
            | _ -> (x, loc) :: acc)
      in
      List.rev l

    method program p =
      let p = super#program p in
      m#merge_info m;
      let all = IdentSet.diff insert_ exc_ in
      let body = m#pack all p in
      body
  end

class clean =
  object (m)
    inherit map as super

    method statements l =
      let rev_append_st x l =
        match x with
        | Block b, _ -> List.rev_append b l
        | x -> x :: l
      in
      let l = super#statements l in
      let vars_rev, vars_loc, instr_rev =
        List.fold_left
          l
          ~init:([], N, [])
          ~f:(fun (vars_rev, vars_loc, instr_rev) (x, loc) ->
            match x with
            | Variable_statement l when Config.Flag.compact () ->
                let vars_loc =
                  match vars_loc with
                  | Pi _ as x -> x
                  | _ -> loc
                in
                List.rev_append l vars_rev, vars_loc, instr_rev
            | Empty_statement | Expression_statement (EVar _) ->
                vars_rev, vars_loc, instr_rev
            | _ when List.is_empty vars_rev ->
                [], vars_loc, rev_append_st (x, loc) instr_rev
            | _ ->
                ( []
                , vars_loc
                , rev_append_st
                    (x, loc)
                    ((Variable_statement (List.rev vars_rev), vars_loc) :: instr_rev) ))
      in
      let instr_rev =
        match vars_rev with
        | [] -> instr_rev
        | vars_rev -> (Variable_statement (List.rev vars_rev), vars_loc) :: instr_rev
      in
      List.rev instr_rev

    method statement s =
      let s = super#statement s in
      let b = function
        | Block [], loc -> Empty_statement, loc
        | Block [ x ], _ -> x
        | b -> b
      in
      let bopt = function
        | Some (Block [], _) -> None
        | Some (Block [ x ], _) -> Some x
        | Some b -> Some b
        | None -> None
      in
      match s with
      | If_statement (if', then', else') -> If_statement (if', b then', bopt else')
      | Do_while_statement (do', while') -> Do_while_statement (b do', while')
      | While_statement (cond, st) -> While_statement (cond, b st)
      | For_statement (p1, p2, p3, st) -> For_statement (p1, p2, p3, b st)
      | ForIn_statement (param, e, st) -> ForIn_statement (param, e, b st)
      | Switch_statement (e, l, Some [], []) -> Switch_statement (e, l, None, [])
      | s -> s

    method sources l =
      let append_st st_rev sources_rev =
        let st = m#statements (List.rev st_rev) in
        let st = List.map st ~f:(fun (s, loc) -> Statement s, loc) in
        List.rev_append st sources_rev
      in
      let st_rev, sources_rev =
        List.fold_left l ~init:([], []) ~f:(fun (st_rev, sources_rev) (x, loc) ->
            match x with
            | Statement s -> (s, loc) :: st_rev, sources_rev
            | Function_declaration _ as x when List.is_empty st_rev ->
                [], (m#source x, loc) :: sources_rev
            | Function_declaration _ as x ->
                [], (m#source x, loc) :: append_st st_rev sources_rev)
      in
      let sources_rev =
        match st_rev with
        | [] -> sources_rev
        | st_rev -> append_st st_rev sources_rev
      in
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

let is_one = function
  | ENum n -> Num.is_one n
  | _ -> false

let assign_op = function
  | exp, EBin (Plus, exp', exp'') -> (
      match Poly.(exp = exp'), Poly.(exp = exp'') with
      | false, false -> None
      | true, false ->
          if is_one exp''
          then Some (EUn (IncrB, exp))
          else Some (EBin (PlusEq, exp, exp''))
      | false, true ->
          if is_one exp' then Some (EUn (IncrB, exp)) else Some (EBin (PlusEq, exp, exp'))
      | true, true -> Some (EBin (StarEq, exp, ENum (Num.of_int32 2l))))
  | exp, EBin (Minus, exp', y) when Poly.(exp = exp') ->
      if is_one y then Some (EUn (DecrB, exp)) else Some (EBin (MinusEq, exp, y))
  | exp, EBin (Mul, exp', exp'') -> (
      match Poly.(exp = exp'), Poly.(exp = exp'') with
      | false, false -> None
      | true, _ -> Some (EBin (StarEq, exp, exp''))
      | _, true -> Some (EBin (StarEq, exp, exp')))
  | exp, EBin (((Div | Mod | Lsl | Asr | Lsr | Band | Bxor | Bor) as unop), exp', y)
    when Poly.(exp = exp') -> Some (EBin (translate_assign_op unop, exp, y))
  | _ -> None

let opt_cons b l =
  match b with
  | Some b -> b :: l
  | None -> l

class simpl =
  object (m)
    inherit map as super

    method expression e =
      let e = super#expression e in
      let is_zero x =
        match Num.to_string x with
        | "0" | "0." -> true
        | _ -> false
      in
      match e with
      | EBin (Plus, e1, e2) -> (
          match e2, e1 with
          | ENum n, _ when Num.is_neg n -> EBin (Minus, e1, ENum (Num.neg n))
          | _, ENum n when Num.is_neg n -> EBin (Minus, e2, ENum (Num.neg n))
          | ENum zero, (ENum _ as x) when is_zero zero -> x
          | (ENum _ as x), ENum zero when is_zero zero -> x
          | _ -> e)
      | EBin (Minus, e1, e2) -> (
          match e2, e1 with
          | ENum n, _ when Num.is_neg n -> EBin (Plus, e1, ENum (Num.neg n))
          | (ENum _ as x), ENum zero when is_zero zero -> x
          | _ -> e)
      | _ -> e

    method statement s =
      let s = super#statement s in
      match s with
      | Block [ x ] -> fst x
      | _ -> s

    method statements s =
      let s = super#statements s in
      List.fold_right s ~init:[] ~f:(fun (st, loc) rem ->
          match st with
          | If_statement (ENum n, iftrue, _) when Num.is_one n -> iftrue :: rem
          | If_statement (ENum n, _, iffalse) when Num.is_zero n -> opt_cons iffalse rem
          | If_statement
              (cond, (Return_statement (Some e1), _), Some (Return_statement (Some e2), _))
            -> (Return_statement (Some (ECond (cond, e1, e2))), loc) :: rem
          | If_statement
              ( cond
              , (Expression_statement (EBin (Eq, v1, e1)), _)
              , Some (Expression_statement (EBin (Eq, v2, e2)), _) )
            when Poly.(v1 = v2) ->
              (Expression_statement (EBin (Eq, v1, ECond (cond, e1, e2))), loc) :: rem
          | Variable_statement l1 ->
              let x =
                List.map l1 ~f:(function
                    | ident, None -> Variable_statement [ ident, None ], loc
                    | ident, Some (exp, pc) -> (
                        match assign_op (EVar ident, exp) with
                        | Some e -> Expression_statement e, loc
                        | None -> Variable_statement [ ident, Some (exp, pc) ], loc))
              in
              x @ rem
          | _ -> (st, loc) :: rem)

    method sources l =
      let append_st st_rev sources_rev =
        let st = m#statements (List.rev st_rev) in
        let st =
          List.map st ~f:(function
              | ( Variable_statement
                    [ (addr, Some (EFun (None, params, body, loc'), loc)) ]
                , _ ) -> Function_declaration (addr, params, body, loc'), loc
              | s, loc -> Statement s, loc)
        in
        List.rev_append st sources_rev
      in
      let st_rev, sources_rev =
        List.fold_left l ~init:([], []) ~f:(fun (st_rev, sources_rev) x ->
            match x with
            | Statement s, loc -> (s, loc) :: st_rev, sources_rev
            | (Function_declaration _ as x), loc when List.is_empty st_rev ->
                [], (m#source x, loc) :: sources_rev
            | (Function_declaration _ as x), loc ->
                [], (m#source x, loc) :: append_st st_rev sources_rev)
      in
      let sources_rev =
        match st_rev with
        | [] -> sources_rev
        | st_rev -> append_st st_rev sources_rev
      in
      List.rev sources_rev
  end
