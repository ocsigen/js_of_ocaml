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

class type mapper = object
  method loc : Javascript.location -> Javascript.location

  method parse_info : Parse_info.t -> Parse_info.t

  method expression : Javascript.expression -> Javascript.expression

  method expression_o : Javascript.expression option -> Javascript.expression option

  method switch_case : Javascript.expression -> Javascript.expression

  method block : Javascript.statement_list -> Javascript.statement_list

  method fun_decl : Javascript.function_declaration -> Javascript.function_declaration

  method class_decl : Javascript.class_declaration -> Javascript.class_declaration

  method class_element : Javascript.class_element -> Javascript.class_element

  method initialiser :
       Javascript.expression * Javascript.location
    -> Javascript.expression * Javascript.location

  method initialiser_o :
       (Javascript.expression * Javascript.location) option
    -> (Javascript.expression * Javascript.location) option

  method for_binding :
       Javascript.variable_declaration_kind
    -> Javascript.for_binding
    -> Javascript.for_binding

  method binding_property : Javascript.binding_property -> Javascript.binding_property

  method variable_declaration :
       Javascript.variable_declaration_kind
    -> Javascript.variable_declaration
    -> Javascript.variable_declaration

  method statement : Javascript.statement -> Javascript.statement

  method statement_o :
       (Javascript.statement * Javascript.location) option
    -> (Javascript.statement * Javascript.location) option

  method statements : Javascript.statement_list -> Javascript.statement_list

  method formal_parameter_list :
    Javascript.formal_parameter_list -> Javascript.formal_parameter_list

  method ident : Javascript.ident -> Javascript.ident

  method program : Javascript.program -> Javascript.program

  method function_body : statement_list -> statement_list

  method import : import -> import

  method export : export -> export
end

(* generic js ast walk/map *)
class map : mapper =
  object (m)
    method loc =
      function
      | N -> N
      | U -> U
      | Pi x -> Pi (m#parse_info x)

    method parse_info i = i

    method ident i =
      match i with
      | V v -> V v
      | S { name; var; loc } -> S { name; var; loc = m#loc loc }

    method private early_error { reason; loc } = { reason; loc = m#parse_info loc }

    method statements l = List.map l ~f:(fun (s, pc) -> m#statement s, m#loc pc)

    method variable_declaration _ x =
      match x with
      | DeclIdent (id, eo) -> DeclIdent (m#ident id, m#initialiser_o eo)
      | DeclPattern (p, i) -> DeclPattern (m#binding_pattern p, m#initialiser i)

    method for_binding _ x = m#binding x

    method formal_parameter_list { list; rest } =
      { list = List.map list ~f:m#param; rest = Option.map rest ~f:m#binding }

    method private property_name x =
      match x with
      | (PNI _ | PNS _ | PNN _) as x -> x
      | PComputed e -> PComputed (m#expression e)

    method fun_decl (k, params, body, nid) =
      k, m#formal_parameter_list params, m#function_body body, m#loc nid

    method class_decl x =
      { extends = Option.map x.extends ~f:m#expression
      ; body = List.map x.body ~f:m#class_element
      }

    method class_element x =
      match x with
      | CEMethod (s, n, meth) -> CEMethod (s, m#class_element_name n, m#method_ meth)
      | CEField (s, n, i) -> CEField (s, m#class_element_name n, m#initialiser_o i)
      | CEStaticBLock b -> CEStaticBLock (m#block b)

    method private class_element_name x =
      match x with
      | PropName n -> PropName (m#property_name n)
      | PrivName x -> PrivName x

    method block l = m#statements l

    method statement s =
      match s with
      | Block b -> Block (m#block b)
      | Variable_statement (k, l) ->
          Variable_statement (k, List.map l ~f:(m#variable_declaration k))
      | Function_declaration (id, fun_decl) ->
          Function_declaration (m#ident id, m#fun_decl fun_decl)
      | Class_declaration (id, cl_decl) ->
          Class_declaration (m#ident id, m#class_decl cl_decl)
      | Empty_statement -> Empty_statement
      | Debugger_statement -> Debugger_statement
      | Expression_statement e -> Expression_statement (m#expression e)
      | If_statement (e, (s, loc), sopt) ->
          If_statement (m#expression e, (m#statement s, m#loc loc), m#statement_o sopt)
      | Do_while_statement ((s, loc), e) ->
          Do_while_statement ((m#statement s, m#loc loc), m#expression e)
      | While_statement (e, (s, loc)) ->
          While_statement (m#expression e, (m#statement s, m#loc loc))
      | For_statement (e1, e2, e3, (s, loc)) ->
          let e1 =
            match e1 with
            | Left o -> Left (m#expression_o o)
            | Right (k, l) ->
                Right (k, List.map l ~f:(fun d -> m#variable_declaration k d))
          in
          For_statement
            (e1, m#expression_o e2, m#expression_o e3, (m#statement s, m#loc loc))
      | ForIn_statement (e1, e2, (s, loc)) ->
          let e1 =
            match e1 with
            | Left e -> Left (m#expression e)
            | Right (k, d) -> Right (k, m#for_binding k d)
          in
          ForIn_statement (e1, m#expression e2, (m#statement s, m#loc loc))
      | ForOf_statement (e1, e2, (s, loc)) ->
          let e1 =
            match e1 with
            | Left e -> Left (m#expression e)
            | Right (k, d) -> Right (k, m#for_binding k d)
          in
          ForOf_statement (e1, m#expression e2, (m#statement s, m#loc loc))
      | ForAwaitOf_statement (e1, e2, (s, loc)) ->
          let e1 =
            match e1 with
            | Left e -> Left (m#expression e)
            | Right (k, d) -> Right (k, m#for_binding k d)
          in
          ForAwaitOf_statement (e1, m#expression e2, (m#statement s, m#loc loc))
      | Continue_statement s -> Continue_statement s
      | Break_statement s -> Break_statement s
      | Return_statement (e, loc) -> Return_statement (m#expression_o e, m#loc loc)
      | Labelled_statement (l, (s, loc)) ->
          Labelled_statement (l, (m#statement s, m#loc loc))
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
            ( m#block b
            , (match catch with
              | None -> None
              | Some (id, b) -> Some (Option.map ~f:m#param id, m#block b))
            , match final with
              | None -> None
              | Some s -> Some (m#block s) )
      | With_statement (e, (s, loc)) ->
          With_statement (m#expression e, (m#statement s, m#loc loc))
      | Import (import, loc) -> Import (m#import import, m#parse_info loc)
      | Export (export, loc) -> Export (m#export export, m#parse_info loc)

    method import { from; kind } =
      let kind =
        match kind with
        | Namespace (iopt, i) -> Namespace (Option.map ~f:m#ident iopt, m#ident i)
        | Named (iopt, l) ->
            Named
              (Option.map ~f:m#ident iopt, List.map ~f:(fun (s, id) -> s, m#ident id) l)
        | Default import_default -> Default (m#ident import_default)
        | SideEffect -> SideEffect
      in
      { from; kind }

    method export e =
      match e with
      | ExportVar (k, l) -> (
          match m#statement (Variable_statement (k, l)) with
          | Variable_statement (k, l) -> ExportVar (k, l)
          | _ -> assert false)
      | ExportFun (id, f) -> (
          match m#statement (Function_declaration (id, f)) with
          | Function_declaration (id, f) -> ExportFun (id, f)
          | _ -> assert false)
      | ExportClass (id, f) -> (
          match m#statement (Class_declaration (id, f)) with
          | Class_declaration (id, f) -> ExportClass (id, f)
          | _ -> assert false)
      | ExportNames l -> ExportNames (List.map ~f:(fun (id, s) -> m#ident id, s) l)
      | ExportDefaultFun (Some id, decl) -> (
          match m#statement (Function_declaration (id, decl)) with
          | Function_declaration (id, decl) -> ExportDefaultFun (Some id, decl)
          | _ -> assert false)
      | ExportDefaultFun (None, decl) -> (
          match m#expression (EFun (None, decl)) with
          | EFun (None, decl) -> ExportDefaultFun (None, decl)
          | _ -> assert false)
      | ExportDefaultClass (Some id, decl) -> (
          match m#statement (Class_declaration (id, decl)) with
          | Class_declaration (id, decl) -> ExportDefaultClass (Some id, decl)
          | _ -> assert false)
      | ExportDefaultClass (None, decl) -> (
          match m#expression (EClass (None, decl)) with
          | EClass (None, decl) -> ExportDefaultClass (None, decl)
          | _ -> assert false)
      | ExportDefaultExpression e -> ExportDefaultExpression (m#expression e)
      | ExportFrom l -> ExportFrom l
      | CoverExportFrom e -> CoverExportFrom (m#early_error e)

    method statement_o x =
      match x with
      | None -> None
      | Some (s, loc) -> Some (m#statement s, m#loc loc)

    method switch_case e = m#expression e

    method private argument a =
      match a with
      | Arg e -> Arg (m#expression e)
      | ArgSpread e -> ArgSpread (m#expression e)

    method private template l =
      List.map l ~f:(function
        | TStr s -> TStr s
        | TExp e -> TExp (m#expression e))

    method expression x =
      match x with
      | ESeq (e1, e2) -> ESeq (m#expression e1, m#expression e2)
      | ECond (e1, e2, e3) -> ECond (m#expression e1, m#expression e2, m#expression e3)
      | EBin (b, e1, e2) -> EBin (b, m#expression e1, m#expression e2)
      | EAssignTarget x -> (
          match x with
          | ArrayTarget l ->
              EAssignTarget
                (ArrayTarget
                   (List.map l ~f:(function
                     | TargetElementHole -> TargetElementHole
                     | TargetElementId (i, e) ->
                         TargetElementId (m#ident i, m#initialiser_o e)
                     | TargetElement e -> TargetElement (m#expression e)
                     | TargetElementSpread e -> TargetElementSpread (m#expression e))))
          | ObjectTarget l ->
              EAssignTarget
                (ObjectTarget
                   (List.map l ~f:(function
                     | TargetPropertyId (Prop_and_ident i, e) ->
                         TargetPropertyId (Prop_and_ident (m#ident i), m#initialiser_o e)
                     | TargetProperty (n, e, i) ->
                         TargetProperty
                           (m#property_name n, m#expression e, m#initialiser_o i)
                     | TargetPropertyMethod (n, x) ->
                         TargetPropertyMethod (m#property_name n, m#method_ x)
                     | TargetPropertySpread e -> TargetPropertySpread (m#expression e)))))
      | EUn (b, e1) -> EUn (b, m#expression e1)
      | ECallTemplate (e1, t, loc) ->
          ECallTemplate (m#expression e1, m#template t, m#loc loc)
      | ECall (e1, ak, e2, loc) ->
          ECall (m#expression e1, ak, List.map e2 ~f:m#argument, m#loc loc)
      | EAccess (e1, ak, e2) -> EAccess (m#expression e1, ak, m#expression e2)
      | EDot (e1, ak, id) -> EDot (m#expression e1, ak, id)
      | EDotPrivate (e1, ak, id) -> EDotPrivate (m#expression e1, ak, id)
      | ENew (e1, args, loc) ->
          ENew (m#expression e1, Option.map ~f:(List.map ~f:m#argument) args, m#loc loc)
      | EVar v -> EVar (m#ident v)
      | EFun (idopt, fun_decl) ->
          let idopt = Option.map ~f:m#ident idopt in
          EFun (idopt, m#fun_decl fun_decl)
      | EClass (id, cl_decl) -> EClass (Option.map ~f:m#ident id, m#class_decl cl_decl)
      | EArrow (fun_decl, consise, x) -> EArrow (m#fun_decl fun_decl, consise, x)
      | EArr l ->
          EArr
            (List.map l ~f:(function
              | ElementHole -> ElementHole
              | Element e -> Element (m#expression e)
              | ElementSpread e -> ElementSpread (m#expression e)))
      | EObj l ->
          EObj
            (List.map l ~f:(fun p ->
                 match p with
                 | Property (i, e) -> Property (m#property_name i, m#expression e)
                 | PropertyMethod (n, x) -> PropertyMethod (m#property_name n, m#method_ x)
                 | PropertySpread e -> PropertySpread (m#expression e)
                 | CoverInitializedName (e, a, b) ->
                     CoverInitializedName (m#early_error e, a, b)))
      | (EStr _ as x) | (EBool _ as x) | (ENum _ as x) | (ERegexp _ as x) -> x
      | ETemplate t -> ETemplate (m#template t)
      | EYield { delegate; expr } -> EYield { delegate; expr = m#expression_o expr }
      | EPrivName i -> EPrivName i
      | CoverParenthesizedExpressionAndArrowParameterList e ->
          CoverParenthesizedExpressionAndArrowParameterList (m#early_error e)
      | CoverCallExpressionAndAsyncArrowHead e ->
          CoverCallExpressionAndAsyncArrowHead (m#early_error e)

    method private method_ x =
      match x with
      | MethodSet fun_decl -> MethodSet (m#fun_decl fun_decl)
      | MethodGet fun_decl -> MethodGet (m#fun_decl fun_decl)
      | Method fun_decl -> Method (m#fun_decl fun_decl)

    method private param p = m#binding_element p

    method private binding_element (b, e) = m#binding b, m#initialiser_o e

    method private binding x =
      match x with
      | BindingIdent x -> BindingIdent (m#ident x)
      | BindingPattern x -> BindingPattern (m#binding_pattern x)

    method private binding_pattern x =
      match x with
      | ObjectBinding { list; rest } ->
          ObjectBinding
            { list = List.map list ~f:m#binding_property
            ; rest = Option.map rest ~f:m#ident
            }
      | ArrayBinding { list; rest } ->
          ArrayBinding
            { list = List.map list ~f:m#binding_array_elt
            ; rest = Option.map rest ~f:m#binding
            }

    method private binding_array_elt x =
      match x with
      | None -> None
      | Some (b, e) -> Some (m#binding b, m#initialiser_o e)

    method binding_property x =
      match x with
      | Prop_binding (i, e) -> Prop_binding (m#property_name i, m#binding_element e)
      | Prop_ident (Prop_and_ident i, e) ->
          Prop_ident (Prop_and_ident (m#ident i), m#initialiser_o e)

    method expression_o x =
      match x with
      | None -> None
      | Some s -> Some (m#expression s)

    method initialiser (e, loc) = m#expression e, m#loc loc

    method initialiser_o x =
      match x with
      | None -> None
      | Some i -> Some (m#initialiser i)

    method program x = m#statements x

    method function_body x = m#statements x
  end

class type iterator = object
  method fun_decl : Javascript.function_declaration -> unit

  method class_decl : Javascript.class_declaration -> unit

  method early_error : Javascript.early_error -> unit

  method expression : Javascript.expression -> unit

  method expression_o : Javascript.expression option -> unit

  method switch_case : Javascript.expression -> unit

  method block : Javascript.statement_list -> unit

  method initialiser : Javascript.expression * Javascript.location -> unit

  method initialiser_o : (Javascript.expression * Javascript.location) option -> unit

  method for_binding :
    Javascript.variable_declaration_kind -> Javascript.for_binding -> unit

  method variable_declaration :
    Javascript.variable_declaration_kind -> Javascript.variable_declaration -> unit

  method statement : Javascript.statement -> unit

  method statement_o : (Javascript.statement * Javascript.location) option -> unit

  method statements : Javascript.statement_list -> unit

  method ident : Javascript.ident -> unit

  method program : Javascript.program -> unit

  method function_body : Javascript.statement_list -> unit

  method import : import -> unit

  method export : export -> unit
end

(* generic js ast iterator *)
class iter : iterator =
  object (m)
    method ident _ = ()

    method early_error _ = ()

    method block l = m#statements l

    method statements l = List.iter l ~f:(fun (s, _) -> m#statement s)

    method variable_declaration _ x =
      match x with
      | DeclIdent (id, eo) ->
          m#ident id;
          m#initialiser_o eo
      | DeclPattern (p, (e, (_ : location))) ->
          m#binding_pattern p;
          m#expression e

    method for_binding _ x = m#binding x

    method private formal_parameter_list { list; rest } =
      List.iter list ~f:m#param;
      Option.iter rest ~f:m#binding

    method private property_name x =
      match x with
      | PNI _ | PNS _ | PNN _ -> ()
      | PComputed e -> m#expression e

    method fun_decl (_k, params, body, _loc) =
      m#formal_parameter_list params;
      m#function_body body

    method class_decl x =
      Option.iter x.extends ~f:m#expression;
      List.iter x.body ~f:m#class_element

    method private class_element x =
      match x with
      | CEMethod (_static, name, x) ->
          m#class_element_name name;
          m#method_ x
      | CEField (_static, n, i) ->
          m#class_element_name n;
          m#initialiser_o i
      | CEStaticBLock b -> m#block b

    method private class_element_name x =
      match x with
      | PropName n -> m#property_name n
      | PrivName (Utf8 _) -> ()

    method statement s =
      match s with
      | Block b -> m#block b
      | Variable_statement (k, l) -> List.iter l ~f:(m#variable_declaration k)
      | Function_declaration (id, fun_decl) ->
          m#ident id;
          m#fun_decl fun_decl
      | Class_declaration (id, cl_decl) ->
          m#ident id;
          m#class_decl cl_decl
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
          | Right (k, l) -> List.iter l ~f:(fun d -> m#variable_declaration k d));
          m#expression_o e2;
          m#expression_o e3;
          m#statement s
      | ForIn_statement (e1, e2, (s, _)) ->
          (match e1 with
          | Left e -> m#expression e
          | Right (k, d) -> m#for_binding k d);

          m#expression e2;
          m#statement s
      | ForOf_statement (e1, e2, (s, _)) ->
          (match e1 with
          | Left e -> m#expression e
          | Right (k, d) -> m#for_binding k d);
          m#expression e2;
          m#statement s
      | ForAwaitOf_statement (e1, e2, (s, _)) ->
          (match e1 with
          | Left e -> m#expression e
          | Right (k, d) -> m#for_binding k d);
          m#expression e2;
          m#statement s
      | Continue_statement _ -> ()
      | Break_statement _ -> ()
      | Return_statement (e, _) -> m#expression_o e
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
          m#block b;
          (match catch with
          | None -> ()
          | Some (id, b) ->
              Option.iter ~f:m#param id;
              m#block b);
          match final with
          | None -> ()
          | Some s -> m#block s)
      | With_statement (e, (s, _)) ->
          m#expression e;
          m#statement s
      | Import (x, _loc) -> m#import x
      | Export (x, _loc) -> m#export x

    method import { from = _; kind } =
      match kind with
      | Namespace (iopt, i) ->
          Option.iter ~f:m#ident iopt;
          m#ident i
      | Named (iopt, l) ->
          Option.iter ~f:m#ident iopt;
          List.iter ~f:(fun (_, id) -> m#ident id) l
      | Default import_default -> m#ident import_default
      | SideEffect -> ()

    method export e =
      match e with
      | ExportVar (k, l) -> m#statement (Variable_statement (k, l))
      | ExportFun (id, f) -> m#statement (Function_declaration (id, f))
      | ExportClass (id, f) -> m#statement (Class_declaration (id, f))
      | ExportNames l -> List.iter ~f:(fun (id, _) -> m#ident id) l
      | ExportDefaultFun (Some id, decl) -> m#statement (Function_declaration (id, decl))
      | ExportDefaultFun (None, decl) -> m#expression (EFun (None, decl))
      | ExportDefaultClass (Some id, decl) -> m#statement (Class_declaration (id, decl))
      | ExportDefaultClass (None, decl) -> m#expression (EClass (None, decl))
      | ExportDefaultExpression e -> m#expression e
      | ExportFrom { from = _; kind = _ } -> ()
      | CoverExportFrom e -> m#early_error e

    method statement_o x =
      match x with
      | None -> ()
      | Some (s, _) -> m#statement s

    method switch_case e = m#expression e

    method private argument a =
      match a with
      | Arg e -> m#expression e
      | ArgSpread e -> m#expression e

    method private template l =
      List.iter l ~f:(function
        | TStr _ -> ()
        | TExp e -> m#expression e)

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
      | EAssignTarget x -> (
          match x with
          | ArrayTarget l ->
              List.iter l ~f:(function
                | TargetElementHole -> ()
                | TargetElementId (i, e) ->
                    m#ident i;
                    m#initialiser_o e
                | TargetElement e -> m#expression e
                | TargetElementSpread e -> m#expression e)
          | ObjectTarget l ->
              List.iter l ~f:(function
                | TargetPropertyId (Prop_and_ident i, e) ->
                    m#ident i;
                    m#initialiser_o e
                | TargetProperty (n, e, i) ->
                    m#property_name n;
                    m#expression e;
                    m#initialiser_o i
                | TargetPropertyMethod (n, x) ->
                    m#property_name n;
                    m#method_ x
                | TargetPropertySpread e -> m#expression e))
      | EUn (_, e1) -> m#expression e1
      | ECall (e1, _ak, e2, _) ->
          m#expression e1;
          List.iter e2 ~f:m#argument
      | ECallTemplate (e1, a, _) ->
          m#expression e1;
          m#template a
      | EAccess (e1, _ak, e2) ->
          m#expression e1;
          m#expression e2
      | EDot (e1, _ak, _) -> m#expression e1
      | EDotPrivate (e1, _, _) -> m#expression e1
      | ENew (e1, Some args, _) ->
          m#expression e1;
          List.iter args ~f:m#argument
      | ENew (e1, None, _) -> m#expression e1
      | EVar v -> m#ident v
      | EFun (idopt, fun_decl) ->
          (match idopt with
          | None -> ()
          | Some i -> m#ident i);
          m#fun_decl fun_decl
      | EClass (i, cl_decl) ->
          Option.iter ~f:m#ident i;
          m#class_decl cl_decl
      | EArrow (fun_decl, _, _) -> m#fun_decl fun_decl
      | EArr l ->
          List.iter l ~f:(function
            | ElementHole -> ()
            | Element e -> m#expression e
            | ElementSpread e -> m#expression e)
      | EObj l ->
          List.iter l ~f:(fun p ->
              match p with
              | Property (i, e) ->
                  m#property_name i;
                  m#expression e
              | PropertyMethod (n, x) ->
                  m#property_name n;
                  m#method_ x
              | PropertySpread e -> m#expression e
              | CoverInitializedName (e, _, _) -> m#early_error e)
      | EStr _ | EBool _ | ENum _ | ERegexp _ -> ()
      | ETemplate l -> m#template l
      | EYield { delegate = _; expr } -> m#expression_o expr
      | EPrivName (Utf8 _) -> ()
      | CoverParenthesizedExpressionAndArrowParameterList e -> m#early_error e
      | CoverCallExpressionAndAsyncArrowHead e -> m#early_error e

    method private method_ x =
      match x with
      | MethodSet fun_decl -> m#fun_decl fun_decl
      | MethodGet fun_decl -> m#fun_decl fun_decl
      | Method fun_decl -> m#fun_decl fun_decl

    method private param p = m#binding_element p

    method private binding_element (b, e) =
      m#binding b;
      m#initialiser_o e

    method private binding x =
      match x with
      | BindingIdent x -> m#ident x
      | BindingPattern x -> m#binding_pattern x

    method private binding_pattern x =
      match x with
      | ObjectBinding { list; rest } ->
          List.iter list ~f:m#binding_property;
          Option.iter rest ~f:m#ident
      | ArrayBinding { list; rest } ->
          List.iter list ~f:m#binding_array_elt;
          Option.iter rest ~f:m#binding

    method private binding_array_elt x =
      match x with
      | None -> ()
      | Some (b, e) ->
          m#binding b;
          m#initialiser_o e

    method private binding_property x =
      match x with
      | Prop_binding ((_ : property_name), e) -> m#binding_element e
      | Prop_ident (Prop_and_ident i, e) ->
          m#ident i;
          m#initialiser_o e

    method expression_o x =
      match x with
      | None -> ()
      | Some s -> m#expression s

    method initialiser (e, _) = m#expression e

    method initialiser_o x =
      match x with
      | None -> ()
      | Some i -> m#initialiser i

    method program x = m#statements x

    method function_body x = m#statements x
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
          , (ANormal | ANullish)
          , [ Arg (EStr _) ]
          , _ ) -> e
      | _ -> super#expression e

    (* do not replace constant in switch case *)
    method switch_case e =
      match e with
      | ENum _ | EStr _ -> e
      | _ -> m#expression e

    method statements l =
      match l with
      | [] -> []
      | ((Expression_statement (EStr _), _) as prolog) :: rest ->
          prolog :: List.map rest ~f:(fun (x, loc) -> m#statement x, loc)
      | rest -> List.map rest ~f:(fun (x, loc) -> m#statement x, loc)
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
        let all =
          Hashtbl.fold (fun e v acc -> DeclIdent (v, Some (e, N)) :: acc) all []
        in
        (Variable_statement (Var, all), N) :: p
  end

type t =
  { use : IdentSet.t
  ; def_var : IdentSet.t
  ; def_local : IdentSet.t
  }

let empty = { use = IdentSet.empty; def_var = IdentSet.empty; def_local = IdentSet.empty }

(* def/used/free variable *)

type block =
  | Catch of formal_parameter
  | Params of formal_parameter_list
  | Normal

class type freevar = object ('a)
  inherit mapper

  method merge_info : 'a -> unit

  method merge_block_info : 'a -> unit

  method record_block : block -> unit

  method state : t

  method def_var : Javascript.ident -> unit

  method def_local : Javascript.ident -> unit

  method use_var : Javascript.ident -> unit

  method get_count : int Javascript.IdentMap.t

  method get_free : IdentSet.t

  method get_def : IdentSet.t

  method get_use : IdentSet.t
end

class free =
  object (m : 'test)
    inherit map as super

    val level : int = 0

    val mutable state_ : t = empty

    val count = ref Javascript.IdentMap.empty

    method state = state_

    method get_count = !count

    method get_free =
      IdentSet.diff m#state.use (IdentSet.union m#state.def_var m#state.def_local)

    method get_def = IdentSet.union m#state.def_var m#state.def_local

    method get_use = m#state.use

    method merge_info from =
      let free = from#get_free in
      state_ <- { state_ with use = IdentSet.union state_.use free }

    method merge_block_info from =
      let use =
        let state = from#state in
        IdentSet.diff state.use state.def_local
      in
      let def_var = from#state.def_var in
      state_ <-
        { use = IdentSet.union state_.use use
        ; def_var = IdentSet.union state_.def_var def_var
        ; def_local = state_.def_local
        }

    method use_var x =
      count :=
        IdentMap.update
          x
          (function
            | None -> Some 1
            | Some n -> Some (succ n))
          !count;
      state_ <- { state_ with use = IdentSet.add x state_.use }

    method def_var x =
      count :=
        IdentMap.update
          x
          (function
            | None -> Some 1
            | Some n -> Some (succ n))
          !count;
      state_ <- { state_ with def_var = IdentSet.add x state_.def_var }

    method def_local x =
      count :=
        IdentMap.update
          x
          (function
            | None -> Some 1
            | Some n -> Some (succ n))
          !count;
      state_ <- { state_ with def_local = IdentSet.add x state_.def_local }

    method fun_decl (k, params, body, nid) =
      let tbody = ({<state_ = empty; level = succ level>} :> 'test) in
      let ids = bound_idents_of_params params in
      List.iter ids ~f:tbody#def_var;
      let body = tbody#function_body body in
      let params = tbody#formal_parameter_list params in
      tbody#record_block (Params params);
      m#merge_info tbody;
      k, params, body, nid

    method expression x =
      match x with
      | EVar v ->
          m#use_var v;
          x
      | EFun (ident, (k, params, body, nid)) ->
          let tbody = ({<state_ = empty; level = succ level>} :> 'test) in
          let ids = bound_idents_of_params params in
          List.iter ids ~f:tbody#def_var;
          let body = tbody#function_body body in
          let params = tbody#formal_parameter_list params in
          let ident =
            match ident with
            | Some i ->
                if IdentSet.mem i tbody#state.use
                then (
                  tbody#def_var i;
                  ident)
                else None
            | None -> None
          in
          tbody#record_block (Params params);
          m#merge_info tbody;
          EFun (ident, (k, params, body, nid))
      | EClass (ident_o, cl_decl) ->
          let same_level = level in
          let cbody = {<state_ = empty; level = same_level>} in
          let ident_o =
            Option.map
              ~f:(fun id ->
                cbody#def_var id;
                id)
              ident_o
          in
          let cl_decl = cbody#class_decl cl_decl in
          cbody#record_block Normal;
          m#merge_block_info cbody;
          EClass (ident_o, cl_decl)
      | _ -> super#expression x

    method record_block _ = ()

    method variable_declaration k x =
      let ids = bound_idents_of_variable_declaration x in
      (match k with
      | Let | Const -> List.iter ids ~f:m#def_local
      | Var -> List.iter ids ~f:m#def_var);
      super#variable_declaration k x

    method block b =
      let same_level = level in
      let tbody = {<state_ = empty; level = same_level>} in
      let b = tbody#statements b in
      tbody#record_block Normal;
      m#merge_block_info tbody;
      b

    method class_element x =
      match x with
      | CEStaticBLock l ->
          let tbody = {<state_ = empty; level = level + 1>} in
          let l = tbody#statements l in
          tbody#record_block Normal;
          m#merge_info tbody;
          CEStaticBLock l
      | _ -> super#class_element x

    method statement x =
      match x with
      | Function_declaration (id, (k, params, body, nid)) ->
          let tbody = {<state_ = empty; level = succ level>} in
          let ids = bound_idents_of_params params in
          List.iter ids ~f:tbody#def_var;
          let body = tbody#function_body body in
          let params = tbody#formal_parameter_list params in
          tbody#record_block (Params params);
          m#def_local id;
          m#merge_info tbody;
          Function_declaration (id, (k, params, body, nid))
      | Class_declaration (id, cl_decl) ->
          let same_level = level in
          let cbody = {<state_ = empty; level = same_level>} in
          let cl_decl = cbody#class_decl cl_decl in
          cbody#record_block Normal;
          m#merge_block_info cbody;
          m#def_local id;
          Class_declaration (id, cl_decl)
      | Block b -> Block (m#block b)
      | For_statement (Right (((Const | Let) as k), l), e1, e2, (st, loc)) ->
          let same_level = level in
          let m' = {<state_ = empty; level = same_level>} in
          let l = List.map ~f:(m'#variable_declaration k) l in
          let e1 = Option.map ~f:m'#expression e1 in
          let e2 = Option.map ~f:m'#expression e2 in
          let st = m'#statement st in
          m'#record_block Normal;
          m#merge_block_info m';
          For_statement (Right (k, l), e1, e2, (st, m#loc loc))
      | ForIn_statement (Right (((Const | Let) as k), l), e2, (st, loc)) ->
          let same_level = level in
          let m' = {<state_ = empty; level = same_level>} in
          let l = m'#for_binding k l in
          let e2 = m'#expression e2 in
          let st = m'#statement st in
          m'#record_block Normal;
          m#merge_block_info m';
          ForIn_statement (Right (k, l), e2, (st, m#loc loc))
      | ForOf_statement (Right (((Const | Let) as k), l), e2, (st, loc)) ->
          let same_level = level in
          let m' = {<state_ = empty; level = same_level>} in
          let l = m'#for_binding k l in
          let e2 = m'#expression e2 in
          let st = m'#statement st in
          m'#record_block Normal;
          m#merge_block_info m';
          ForOf_statement (Right (k, l), e2, (st, m#loc loc))
      | ForAwaitOf_statement (Right (((Const | Let) as k), l), e2, (st, loc)) ->
          let same_level = level in
          let m' = {<state_ = empty; level = same_level>} in
          let l = m'#for_binding k l in
          let e2 = m'#expression e2 in
          let st = m'#statement st in
          m'#record_block Normal;
          m#merge_block_info m';
          ForAwaitOf_statement (Right (k, l), e2, (st, m#loc loc))
      | Switch_statement (e, l, def, l') ->
          let same_level = level in
          let m' = {<state_ = empty; level = same_level>} in
          let l = List.map l ~f:(fun (e, s) -> m'#switch_case e, m'#statements s) in
          let l' = List.map l' ~f:(fun (e, s) -> m'#switch_case e, m'#statements s) in
          let def =
            match def with
            | None -> None
            | Some l -> Some (m'#statements l)
          in
          let e = m#expression e in
          m'#record_block Normal;
          m#merge_block_info m';
          Switch_statement (e, l, def, l')
      | Try_statement (b, w, f) ->
          let same_level = level in
          let b = m#block b in
          let w =
            match w with
            | None -> None
            | Some (None, b) -> Some (None, m#block b)
            | Some (Some id, block) ->
                let tw = {<state_ = empty; level = same_level>} in
                let block = tw#statements block in
                tw#record_block (Catch id);
                (* special merge here *)
                (* we need to propagate both def and use .. *)
                (* .. except the use of 'id' since its scope is limited
                   to 'block' *)
                let ids = bound_idents_of_binding (fst id) in
                let clean set =
                  List.fold_left ids ~init:set ~f:(fun set id -> IdentSet.remove id set)
                in
                let def_var = tw#state.def_var in
                let use = clean (IdentSet.diff tw#state.use tw#state.def_local) in
                state_ <-
                  { use = IdentSet.union state_.use use
                  ; def_var = IdentSet.union state_.def_var def_var
                  ; def_local = state_.def_local
                  };
                Some (Some id, block)
          in
          let f =
            match f with
            | None -> None
            | Some f -> Some (m#block f)
          in
          Try_statement (b, w, f)
      | Import ({ from = _; kind }, _) ->
          (match kind with
          | Namespace (iopt, i) ->
              Option.iter ~f:m#def_local iopt;
              m#def_local i
          | Named (iopt, l) ->
              Option.iter ~f:m#def_local iopt;
              List.iter ~f:(fun (_, id) -> m#def_local id) l
          | Default import_default -> m#def_local import_default
          | SideEffect -> ());
          super#statement x
      | _ -> super#statement x

    method for_binding k x =
      (match x with
      | BindingIdent x -> (
          match k with
          | Let | Const -> m#def_local x
          | Var -> m#def_var x)
      | BindingPattern x -> (
          let ids = bound_idents_of_pattern x in
          match k with
          | Let | Const -> List.iter ids ~f:m#def_local
          | Var -> List.iter ids ~f:m#def_var));
      super#for_binding k x
  end

type scope =
  | Module
  | Lexical_block
  | Fun_block of ident option

class rename_variable ~esm =
  let declared scope params body =
    let declared_names = ref StringSet.empty in
    let decl_var x =
      match x with
      | S { name = Utf8 name; _ } -> declared_names := StringSet.add name !declared_names
      | _ -> ()
    in
    (match scope with
    | Module -> ()
    | Lexical_block -> ()
    | Fun_block None -> ()
    | Fun_block (Some x) -> decl_var x);
    List.iter params ~f:(fun x -> decl_var x);
    (object (self)
       val depth = 0

       inherit iter as super

       method expression _ = ()

       method fun_decl _ = ()

       method class_decl _ = ()

       method statement x =
         match scope, x with
         | (Fun_block _ | Module), Function_declaration (id, fd) ->
             decl_var id;
             self#fun_decl fd
         | Lexical_block, Function_declaration (_, fd) -> self#fun_decl fd
         | (Fun_block _ | Module), Class_declaration (id, cl_decl) ->
             decl_var id;
             self#class_decl cl_decl
         | Lexical_block, Class_declaration (_, cl_decl) -> self#class_decl cl_decl
         | _, For_statement (Right (((Const | Let) as k), l), _e1, _e2, (st, _loc)) ->
             let m = {<depth = depth + 1>} in
             List.iter ~f:(m#variable_declaration k) l;
             m#statement st
         | _, ForOf_statement (Right (((Const | Let) as k), l), _e2, (st, _loc)) ->
             let m = {<depth = depth + 1>} in
             m#for_binding k l;
             m#statement st
         | _, ForAwaitOf_statement (Right (((Const | Let) as k), l), _e2, (st, _loc)) ->
             let m = {<depth = depth + 1>} in
             m#for_binding k l;
             m#statement st
         | _, ForIn_statement (Right (((Const | Let) as k), l), _e2, (st, _loc)) ->
             let m = {<depth = depth + 1>} in
             m#for_binding k l;
             m#statement st
         | _, Switch_statement (_, l, def, l') ->
             let m = {<depth = depth + 1>} in
             List.iter l ~f:(fun (_, s) -> m#statements s);
             Option.iter def ~f:(fun l -> m#statements l);
             List.iter l' ~f:(fun (_, s) -> m#statements s)
         | _, Import ({ kind; from = _ }, _loc) -> (
             match kind with
             | Namespace (iopt, i) ->
                 Option.iter ~f:decl_var iopt;
                 decl_var i
             | Named (iopt, l) ->
                 Option.iter ~f:decl_var iopt;
                 List.iter ~f:(fun (_, id) -> decl_var id) l
             | Default import_default -> decl_var import_default
             | SideEffect -> ())
         | (Fun_block _ | Lexical_block | Module), _ -> super#statement x

       method export e =
         match e with
         | ExportVar (_k, _l) -> ()
         | ExportFun (_id, _f) -> ()
         | ExportClass (_id, _f) -> ()
         | ExportNames l -> List.iter ~f:(fun (id, _) -> self#ident id) l
         | ExportDefaultFun (Some id, decl) ->
             self#statement (Function_declaration (id, decl))
         | ExportDefaultClass (Some id, decl) ->
             self#statement (Class_declaration (id, decl))
         | ExportDefaultFun (None, decl) -> self#fun_decl decl
         | ExportDefaultClass (None, decl) -> self#class_decl decl
         | ExportDefaultExpression e -> self#expression e
         | ExportFrom { from = _; kind = _ } -> ()
         | CoverExportFrom _ -> ()

       method variable_declaration k l =
         if
           match scope, k with
           | (Lexical_block | Fun_block _ | Module), (Let | Const) -> depth = 0
           | Lexical_block, Var -> false
           | (Fun_block _ | Module), Var -> true
         then
           let ids = bound_idents_of_variable_declaration l in
           List.iter ids ~f:decl_var

       method block l =
         let m = {<depth = depth + 1>} in
         m#statements l

       method for_binding k p =
         if
           match scope, k with
           | (Lexical_block | Fun_block _ | Module), (Let | Const) -> depth = 0
           | Lexical_block, Var -> false
           | (Fun_block _ | Module), Var -> true
         then
           match p with
           | BindingIdent i -> decl_var i
           | BindingPattern p ->
               let ids = bound_idents_of_pattern p in
               List.iter ids ~f:decl_var
    end)
      #statements
      body;
    !declared_names
  in
  object (m)
    inherit map as super

    val subst = StringMap.empty

    val decl = StringSet.empty

    val labels = StringMap.empty

    method update_state scope params iter_body =
      let declared_names = declared scope params iter_body in
      {<subst = StringSet.fold
                  (fun name subst -> StringMap.add name (Code.Var.fresh_n name) subst)
                  declared_names
                  subst
       ; decl = declared_names>}

    method ident x =
      match x with
      | V _ -> x
      | S { name = Utf8 name; _ } -> (
          try V (StringMap.find name subst) with Not_found -> x)

    method class_element x =
      match x with
      | CEStaticBLock l ->
          let m' = m#update_state (Fun_block None) [] l in
          CEStaticBLock (m'#statements l)
      | _ -> super#class_element x

    method fun_decl (k, params, body, nid) =
      let ids = bound_idents_of_params params in
      let m' = m#update_state (Fun_block None) ids body in
      k, m'#formal_parameter_list params, m'#function_body body, m#loc nid

    method program p =
      if esm
      then
        let m' = m#update_state Module [] p in
        m'#statements p
      else
        let m' = m#update_state Lexical_block [] p in
        m'#statements p

    method binding_property x =
      match x with
      | Prop_ident (Prop_and_ident (S { name = Utf8 name' as name; _ } as ident), e)
        when StringMap.mem name' subst ->
          let x = Prop_binding (PNI name, (BindingIdent ident, e)) in
          super#binding_property x
      | x -> super#binding_property x

    method expression e =
      match e with
      | EFun (ident, (k, params, body, nid)) ->
          let ids = bound_idents_of_params params in
          let m' = m#update_state (Fun_block ident) ids body in
          EFun
            ( Option.map ident ~f:m'#ident
            , (k, m'#formal_parameter_list params, m'#function_body body, m#loc nid) )
      | EClass (Some id, cl_decl) ->
          let m' = m#update_state Lexical_block [ id ] [] in
          EClass (Some (m'#ident id), m'#class_decl cl_decl)
      | EAssignTarget (ObjectTarget l) ->
          let l =
            List.map l ~f:(function
              | TargetPropertyId
                  (Prop_and_ident (S { name = Utf8 name' as name; _ } as ident), rhs)
                when StringMap.mem name' subst ->
                  TargetProperty (PNI name, EVar ident, rhs)
              | b -> b)
          in
          super#expression (EAssignTarget (ObjectTarget l))
      | _ -> super#expression e

    method statement s =
      match s with
      | Labelled_statement (l, (s, loc)) ->
          let l, m =
            match l with
            | L _ -> l, m
            | S (Utf8 u) ->
                let l = Label.fresh () in
                let m = {<labels = StringMap.add u l labels>} in
                l, m
          in
          Labelled_statement (l, (m#statement s, loc))
      | Break_statement (Some l) -> (
          match l with
          | L _ -> s
          | S (Utf8 l) -> (
              match StringMap.find_opt l labels with
              | None -> s
              | Some l -> Break_statement (Some l)))
      | Continue_statement (Some l) -> (
          match l with
          | L _ -> s
          | S (Utf8 l) -> (
              match StringMap.find_opt l labels with
              | None -> s
              | Some l -> Continue_statement (Some l)))
      | Function_declaration (id, (k, params, body, nid)) ->
          let ids = bound_idents_of_params params in
          let m' = m#update_state (Fun_block None) ids body in
          Function_declaration
            ( m#ident id
            , (k, m'#formal_parameter_list params, m'#function_body body, m#loc nid) )
      | For_statement (Right (((Const | Let) as k), l), e1, e2, (st, loc)) ->
          let ids = List.concat_map ~f:bound_idents_of_variable_declaration l in
          let m' = m#update_state Lexical_block ids [] in
          For_statement
            ( Right (k, List.map ~f:(m'#variable_declaration k) l)
            , Option.map ~f:m'#expression e1
            , Option.map ~f:m'#expression e2
            , (m'#statement st, m'#loc loc) )
      | ForOf_statement (Right (((Const | Let) as k), l), e2, (st, loc)) ->
          let ids = bound_idents_of_binding l in
          let m' = m#update_state Lexical_block ids [] in
          ForOf_statement
            ( Right (k, m'#for_binding k l)
            , m'#expression e2
            , (m'#statement st, m'#loc loc) )
      | ForAwaitOf_statement (Right (((Const | Let) as k), l), e2, (st, loc)) ->
          let ids = bound_idents_of_binding l in
          let m' = m#update_state Lexical_block ids [] in
          ForAwaitOf_statement
            ( Right (k, m'#for_binding k l)
            , m'#expression e2
            , (m'#statement st, m'#loc loc) )
      | ForIn_statement (Right (((Const | Let) as k), l), e2, (st, loc)) ->
          let ids = bound_idents_of_binding l in
          let m' = m#update_state Lexical_block ids [] in
          ForIn_statement
            ( Right (k, m'#for_binding k l)
            , m'#expression e2
            , (m'#statement st, m'#loc loc) )
      | Block l ->
          let m' = m#update_state Lexical_block [] l in
          Block (m'#statements l)
      | Try_statement (block, catch, final) ->
          let block =
            let m' = m#update_state Lexical_block [] block in
            m'#statements block
          in
          let final =
            match final with
            | None -> None
            | Some final ->
                let m' = m#update_state Lexical_block [] final in
                Some (m'#statements final)
          in
          let catch =
            match catch with
            | None -> None
            | Some (i, catch) ->
                let i, l =
                  match i with
                  | None -> None, []
                  | Some ((pat, _) as p) ->
                      let ids = bound_idents_of_binding pat in
                      let l =
                        List.filter ids ~f:(function
                          | S { name = Utf8 name; _ } -> not (StringSet.mem name decl)
                          | V _ -> false)
                      in
                      Some p, l
                in
                let m' = m#update_state Lexical_block l catch in
                let i =
                  match i with
                  | None -> None
                  | Some i -> (
                      match m'#formal_parameter_list (list [ i ]) with
                      | { list = [ i ]; rest = None } -> Some i
                      | _ -> assert false)
                in
                Some (i, m'#statements catch)
          in
          Try_statement (block, catch, final)
      | Switch_statement (e, l, def, l') ->
          let all =
            let r = ref [] in
            Option.iter def ~f:(fun l -> r := List.rev_append l !r);
            List.iter l ~f:(fun (_, s) -> r := List.rev_append s !r);
            List.iter l' ~f:(fun (_, s) -> r := List.rev_append s !r);
            !r
          in
          let m' = m#update_state Lexical_block [] all in
          Switch_statement
            ( m#expression e
            , List.map l ~f:(fun (e, s) -> m'#switch_case e, m'#statements s)
            , (match def with
              | None -> None
              | Some l -> Some (m'#statements l))
            , List.map l' ~f:(fun (e, s) -> m'#switch_case e, m'#statements s) )
      | _ -> super#statement s
  end

class compact_vardecl =
  let expr_eq id e = EBin (Eq, EVar id, e) in
  object (m)
    inherit map as super

    val mutable insert_ = IdentSet.empty

    method private var x = insert_ <- IdentSet.add x insert_

    method fun_decl (k, params, body, nid) =
      let m' = {<insert_ = IdentSet.empty>} in
      let params = m'#formal_parameter_list params in
      let body = m'#function_body body in
      let body = m'#pack body in
      k, params, body, m#loc nid

    method private split x =
      let rec loop = function
        | ESeq (e1, e2) -> loop e1 @ loop e2
        | e -> [ e ]
      in
      loop x

    method pack rest =
      let all = insert_ in
      let may_flush rem vars s instr =
        if List.is_empty vars
        then rem, [], s :: instr
        else rem, [], s :: (Variable_statement (Var, List.rev vars), N) :: instr
      in
      let rem, vars, instr =
        List.fold_left rest ~init:(all, [], []) ~f:(fun (rem, vars, instr) (s, loc) ->
            match s with
            | Expression_statement e ->
                let l = m#split e in
                List.fold_left l ~init:(rem, vars, instr) ~f:(fun (rem, vars, instr) e ->
                    match e with
                    | EBin (Eq, EVar id, exp) when IdentSet.mem id rem ->
                        ( IdentSet.remove id rem
                        , DeclIdent (id, Some (exp, N)) :: vars
                        , instr )
                    | x -> may_flush rem vars (Expression_statement x, N) instr)
            | Function_declaration _ as x -> rem, vars, (x, loc) :: instr
            | _ as s -> may_flush rem vars (s, loc) instr)
      in
      let instr =
        match vars with
        | [] -> List.rev instr
        | d ->
            let d = Variable_statement (Var, List.rev d) in
            List.rev ((d, N) :: instr)
      in
      let l = IdentSet.fold (fun x acc -> DeclIdent (x, None) :: acc) rem [] in
      match l, instr with
      | [], _ -> instr
      | l, (Variable_statement (Var, l'), loc) :: rest ->
          (Variable_statement (Var, List.rev_append l l'), loc) :: rest
      | l, _ -> (Variable_statement (Var, l), N) :: instr

    method statements s =
      let s = super#statements s in
      List.concat_map s ~f:(fun (s, loc) ->
          match s with
          | Variable_statement (Var, l) ->
              List.filter_map l ~f:(function
                | DeclIdent (x, Some (init, loc)) ->
                    m#var x;
                    Some (Expression_statement (expr_eq x init), loc)
                | DeclIdent (x, None) ->
                    m#var x;
                    None
                | DeclPattern _ as x -> Some (Variable_statement (Var, [ x ]), loc))
          | s -> [ s, loc ])

    method program p =
      let p = super#program p in
      let body = m#pack p in
      body
  end

(* - Group variable_statement together *)
(* - Remove unnecessary block *)
class clean =
  object (_m)
    inherit map as super

    method statements l =
      let l = super#statements l in
      List.filter l ~f:(function
        | (Empty_statement | Expression_statement (EVar _)), _ -> false
        | _ -> true)
      |> List.group ~f:(fun (x, _) (prev, _) ->
             match prev, x with
             | Variable_statement (k1, _), Variable_statement (k2, _) when Poly.(k1 = k2)
               -> true
             | _, _ -> false)
      |> List.map ~f:(function
           | (Variable_statement (k1, _), _) :: _ as l ->
               let loc =
                 List.find_map l ~f:(fun (_, loc) ->
                     match loc with
                     | N | U -> None
                     | Pi _ -> Some loc)
                 |> function
                 | None -> N
                 | Some x -> x
               in

               ( Variable_statement
                   ( k1
                   , List.concat_map l ~f:(function
                       | Variable_statement (_, l), _ -> l
                       | _ -> assert false) )
               , loc )
           | [ x ] -> x
           | [] | _ :: _ :: _ -> assert false)

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
      | ForOf_statement (param, e, st) -> ForOf_statement (param, e, b st)
      | ForAwaitOf_statement (param, e, st) -> ForAwaitOf_statement (param, e, b st)
      | Switch_statement (e, l, Some [], []) -> Switch_statement (e, l, None, [])
      | s -> s
  end

let opt_cons b l =
  match b with
  | Some b -> b :: l
  | None -> l

let use_fun_context l =
  let exception True in
  try
    (object
       inherit iter as super

       method fun_decl _ = ()

       method ident x =
         match x with
         (* An ArrowFunction does not define local bindings for
            arguments, super, this, or new.target. *)
         | S { name = Utf8 ("this" | "arguments" | "super" | "new" (* new.target *)); _ }
           -> raise True
         | _ -> ()

       method expression x =
         match x with
         | EArrow (_, _, ANo_fun_context) -> ()
         | EArrow (_, _, AUse_parent_fun_context) -> raise True
         | EArrow (fun_decl, _, AUnknown) -> super#fun_decl fun_decl
         | _ -> super#expression x
    end)
      #statements
      l;
    false
  with True -> true

(* - Split variable_statement *)
(* - rewrite assign_op *)
(* - rewrite function_expression into function_declaration *)
(* - if simplification *)
(* - arithmetic simplification *)
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
      | EFun
          (None, (({ generator = false; async = true | false }, _, body, _) as fun_decl))
        when Config.Flag.es6 () && not (use_fun_context body) ->
          let consise =
            match body with
            | [ (Return_statement _, _) ] -> true
            | _ -> false
          in
          EArrow (fun_decl, consise, ANo_fun_context)
      | EArrow (((_, _, body, _) as fun_decl), consise, AUnknown) ->
          if use_fun_context body
          then EArrow (fun_decl, consise, AUse_parent_fun_context)
          else EArrow (fun_decl, consise, ANo_fun_context)
      | e -> e

    method statement s =
      let s = super#statement s in
      match s with
      | Block [ x ] -> fst x
      | _ -> s

    method program p = m#statements_top (m#statements p)

    method function_body b = m#statements_top (m#statements b)

    method private statements_top l =
      (* In strict mode, functions inside blocks are scoped to that
         block. Prior to ES2015, block-level functions were forbidden
         in strict mode. *)
      List.map l ~f:(function s, loc ->
          (match s with
          | Variable_statement
              ((Var | Let | Const), [ DeclIdent (addr, Some (EFun (None, decl), loc)) ])
            -> Function_declaration (addr, decl), loc
          | Variable_statement ((Var | Let | Const), ([] | _ :: _ :: _)) -> assert false
          | s -> s, loc))

    method statements s =
      let s = super#statements s in
      List.fold_right s ~init:[] ~f:(fun (st, loc) rem ->
          match st with
          (* if (1) e1 ... --> e1 *)
          | If_statement (ENum n, iftrue, _) when Num.is_one n -> iftrue :: rem
          (* if (0) e1 else e2 --> e2 *)
          | If_statement (ENum n, _, iffalse) when Num.is_zero n -> opt_cons iffalse rem
          (* if (e1) return e2 else return e3 --> return e1 ? e2 : e3 *)
          | If_statement
              ( cond
              , (Return_statement (Some e1, _), _)
              , Some (Return_statement (Some e2, _), _) ) ->
              ( Return_statement
                  ( Some (ECond (cond, e1, e2))
                  , U
                    (*TODO: it would be better to use the location of the
                      end of the function, but we can't easily get it. *)
                  )
              , loc )
              :: rem
          (* if (e1) v1 = e2 else v1 = e3 --> v1 = e1 ? e2 : e3 *)
          | If_statement
              ( cond
              , (Expression_statement (EBin (Eq, v1, e1)), _)
              , Some (Expression_statement (EBin (Eq, v2, e2)), _) )
            when Poly.(v1 = v2) ->
              (Expression_statement (EBin (Eq, v1, ECond (cond, e1, e2))), loc) :: rem
          (* The following optimizations cause the generated JS to compress less.
             (* if (e1) e2 else e3 --> e1 ? e2 : e3 *)
             | If_statement
                 (e1, (Expression_statement e2, _), Some (Expression_statement e3, _)) ->
                 (Expression_statement (ECond (e1, e2, e3)), loc) :: rem
             (* if (!e1) e2 --> e1 || e2 *)
             | If_statement (EUn (Not, e1), (Expression_statement e2, _), None) ->
                 (Expression_statement (EBin (Or, e1, e2)), loc) :: rem
             (* if (e1) e2 --> e1 && e2 *)
             | If_statement (e1, (Expression_statement e2, _), None) ->
                 (Expression_statement (EBin (And, e1, e2)), loc) :: rem
          *)
          | Variable_statement (((Var | Let | Const) as k), l1) ->
              let x = List.map l1 ~f:(fun d -> Variable_statement (k, [ d ]), loc) in
              x @ rem
          | _ -> (st, loc) :: rem)
  end
