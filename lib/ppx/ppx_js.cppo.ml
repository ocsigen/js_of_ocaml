open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree

open Ast_convenience

(** Check if an expression is an identifier and returns it.
    Raise a Location.error if it's not.
*)
let exp_to_string = function
  | {pexp_desc= Pexp_ident {txt = Longident.Lident s}} -> s
  | {pexp_loc} ->
     Location.raise_errorf
       ~loc:pexp_loc
       "Javascript methods or attributes can only be simple identifiers."


let rnd = Random.State.make [|0x313511d4|]
let random_var () =
  Format.sprintf "jsoo_%08Lx" (Random.State.int64 rnd 0x100000000L)
let random_tvar () =
  Format.sprintf "jsoo_%08Lx" (Random.State.int64 rnd 0x100000000L)

let inside_Js = lazy
  (try
     Filename.basename @@
     Filename.chop_extension !Location.input_name = "js"
   with Invalid_argument _ -> false)

module Js = struct

  let type_ ?loc s args =
    if Lazy.force inside_Js
    then Typ.constr ?loc (lid s) args
    else Typ.constr ?loc (lid @@ "Js."^s) args

#if OCAML_VERSION < (4,03,0)
  let nolabel = ""
#else
  let nolabel = Nolabel
#endif

  let unsafe ?loc s args =
    let args = List.map (fun x -> nolabel,x) args in
    if Lazy.force inside_Js
    then Exp.(apply (ident ?loc @@ lid ("Unsafe."^s)) args)
    else Exp.(apply (ident ?loc @@ lid ("Js.Unsafe."^s)) args)

  let fun_ ?loc s args =
    let args = List.map (fun x -> nolabel,x) args in
    if Lazy.force inside_Js
    then Exp.(apply (ident ?loc @@ lid s) args)
    else Exp.(apply (ident ?loc @@ lid ("Js."^s)) args)

  let string ?loc arg = fun_ ?loc "string" [str arg]

end


let fresh_type loc = Typ.var ~loc @@ random_tvar ()

let unescape lab =
  assert (lab <> "");
  let lab =
    if lab.[0] = '_' then String.sub lab 1 (String.length lab - 1) else lab
  in
  try
    let i = String.rindex lab '_' in
    if i = 0 then raise Not_found;
    String.sub lab 0 i
  with Not_found ->
    lab

(** Constraints for the various types.
    Synthesize new types and create dummy declaration with type constraints.
*)
let constrain_types obj res res_typ meth meth_typ args =
  default_loc := obj.pexp_loc ;

  let typ_var = fresh_type obj.pexp_loc in

  (* [($obj$ : <typ_var> Js.t)] *)
  let cstr =
    Exp.constraint_
      obj
      (Js.type_ "t" [typ_var] )
  in

  let x = evar "x" in
  (* [($x$#meth : $meth_typ$)] *)
  let body =
    Exp.constraint_
      (Exp.send x meth)
      meth_typ
  in

  let res_constr = [%expr ([%e Exp.ident res] : [%t res_typ])][@metaloc res.loc] in
  let res_bindings =
    List.fold_right
      (fun (e, x, (_,t)) e' ->
         [%expr let _ = ([%e evar x] : [%t t]) in [%e e']])
      args
      res_constr
  in
  [%expr
    let module M = struct
      let res =
        let _ = [%e cstr] in
        let _ = fun (x : [%t typ_var]) -> [%e body] in
        [%e res_bindings];
    end in M.res
  ]



let arrows args ret =
  List.fold_right (fun (l, ty) fun_ -> Typ.arrow l ty fun_)
    args
    ret

let sequence l last =
  List.fold_right Exp.sequence l last

let method_call obj meth args =
  let args = List.map (fun (l,e) -> (e, random_var (), (l, fresh_type obj.pexp_loc))) args in
  let ret_type = fresh_type obj.pexp_loc in
  let method_type =
    arrows (List.map (fun (_,_,x) -> x) args) @@ Js.type_ "meth" [ret_type] in
  let o = random_var () in
  let obj' = Exp.ident ~loc:obj.pexp_loc @@ lid o in
  let res = random_var () in
  let meth_args =
    Exp.array @@
    List.map
      (fun (_, x, _) -> Js.unsafe "inject" [evar x])
      args
  in
  List.fold_left
    (fun e' (e, x, _) -> [%expr let [%p pvar x] = [%e e] in [%e e']])
    [%expr
      let [%p pvar o] = [%e obj] in
      let [%p pvar res] =
        [%e Js.unsafe "meth_call" [ evar o ; str @@ unescape meth ; meth_args] ]
      in
      [%e constrain_types obj' (lid res) ret_type meth method_type args]
    ]
    args

(** Instantiation of a class, used by new%js. *)
let new_object constr args =
  let args = List.map (fun (l,e) -> (e, (l,fresh_type constr.loc))) args in
  let obj_type = Js.type_ "t" [fresh_type constr.loc] in
  let constr_fun_type = arrows (List.map snd args) obj_type in
  let args =
    Exp.array @@
    List.map
      (fun (e, (l,t)) -> Js.unsafe "inject" [Exp.constraint_ e t])
      args
  in
  let x = random_var () in
  let constr =
    Exp.constraint_
      (Exp.ident constr)
      (Js.type_ "constr" [constr_fun_type])
  in
  [%expr
    ( let [%p pvar x] = [%e constr] in
      [%e Js.unsafe "new_obj" [evar x ; args]]
    : [%t obj_type] )
  ]


module S = Map.Make(String)

(** We remove Pexp_poly as it should never be in the parsetree except after a method call.
*)
let format_meth body =
  match body.pexp_desc with
  | Pexp_poly (e, _) -> e
  | _ -> body

(** Ensure basic sanity rules about fields of a literal object:
    - No duplicated declaration
    - Only relevant declarations (val and method, for now).
*)
let preprocess_literal_object fields =

  let check_name id names =
    if S.mem id.txt names then
      let id' = S.find id.txt names in
      (* We point out both definitions in locations (more convenient for the user). *)
      let sub = [Location.errorf ~loc:id'.loc "Duplicated val or method %S." id'.txt] in
      Location.raise_errorf ~loc:id.loc ~sub "Duplicated val or method %S." id.txt
    else
      S.add id.txt id names
  in

  let f (names, fields) exp = match exp.pcf_desc with
    | Pcf_val (id, mut, Cfk_concrete (bang, body)) ->
      let names = check_name id names in
      names, (`Val (id, mut, bang, body) :: fields)
    | Pcf_method (id, priv, Cfk_concrete (bang, body)) ->
      let names = check_name id names in
      names, (`Meth (id, priv, bang, format_meth body) :: fields)
    | _ ->
      Location.raise_errorf ~loc:exp.pcf_loc
        "This field is not valid inside a js literal object."
  in
  try `Fields (snd @@ List.fold_left f (S.empty, []) fields)
  with Location.Error error -> `Error (extension_of_error error)

(** Desugar something like this:

  object%js (self)
    val mutable foo = 3
    method bar x = 3 + x
  end

to:

  let bar
    : ('self,'jsoo_173316d7 -> 'jsoo_32b5ee21) Js.meth_callback
    = Js.wrap_meth_callback (fun self -> fun x -> 3 + x)
  and foo
    : 'jsoo_29529091 = 3
  in
  ( Js.Unsafe.obj
      [| ("bar", (Js.Unsafe.inject bar)) ;
         ("foo", (Js.Unsafe.inject foo)) |]
   : < bar :'jsoo_173316d7 -> 'jsoo_32b5ee21 Js.meth ;
       foo :'jsoo_29529091 Js.prop >
     Js.t as 'self)

 *)

let literal_object ?loc self_id fields =
  let self_type = random_tvar () in

  let fields =
    List.map (function
      | `Val (n, mut, bang, body) ->
        let ty = fresh_type n.loc in
        `Val (n, mut, bang, body, ty)
      | `Meth (n, priv, bang, body) ->
        let rec create_meth_ty exp = match exp.pexp_desc with
          | Pexp_fun (label,_,_,body) ->
            (label, fresh_type exp.pexp_loc) :: create_meth_ty body
          | _ -> []
        in
        let ret_ty = fresh_type body.pexp_loc in
        let fun_ty = create_meth_ty body in
        let self_and_body = [%expr fun [%p self_id] -> [%e body]] in
        `Meth (n, priv, bang, self_and_body, (fun_ty, ret_ty))
    )
      fields
  in

  let create_method_type = function
    | `Val  (id, Mutable, _, _body, ty) ->
      (id.txt, [], Js.type_ "prop" [ty])

    | `Val  (id, Immutable, _, _body, ty) ->
      (id.txt, [], Js.type_ "readonly_prop" [ty])

    | `Meth (id, _, _, _body, (fun_ty, ret_ty)) ->
      (id.txt, [], arrows fun_ty (Js.type_ "meth" [ret_ty]))
  in

  let obj_type =
    let l = List.map create_method_type fields
    in Typ.object_ l Closed
  in


  let create_value = function
    | `Val (id, _, _, body, ty) ->
      (id.txt, Exp.constraint_ body ty)
      [@metaloc body.pexp_loc]

    | `Meth (id, _, _, body, (fun_ty, ret_ty)) ->
      (id.txt,
       Exp.constraint_
         (Js.fun_ "wrap_meth_callback" [ body ])
         (Js.type_ "meth_callback" [
           Typ.var self_type ;
           arrows fun_ty ret_ty
         ]))

  in

  let values = List.map create_value fields in

  let object_creation =
    Js.unsafe
      "obj"
      [Exp.array (
        List.map
          (fun (name, _exp) ->
           tuple [str name ; Js.unsafe "inject" [evar name] ]
          )
          values
       )]
  in

  let value_declaration =
    Exp.let_
      Nonrecursive
      (List.map (fun (name, exp) -> Vb.mk (pvar name) exp) values)
      (Exp.constraint_
         object_creation
         (Typ.alias (Js.type_ "t" [obj_type]) self_type)
      )
  in

  value_declaration

let js_mapper _args =
  { default_mapper with
    expr = (fun mapper expr ->
      default_loc := expr.pexp_loc;
      let { pexp_attributes } = expr in
      match expr with

      (** obj##.var *)
      | [%expr [%e? obj] ##. [%e? meth] ] ->
        let meth = exp_to_string meth in
        let o = random_var () in
        let obj' = Exp.ident ~loc:obj.pexp_loc @@ lid o in
        let res = random_var () in
        let new_expr =
          [%expr
            let [%p pvar o] = [%e obj] in
            let [%p pvar res] = [%e Js.unsafe "get" [obj' ; str @@ unescape meth]] in
            [%e
              constrain_types
                obj'
                (lid res) [%type: 'A]
                meth (Js.type_ "gen_prop" [[%type: <get : 'A; ..> ]])
                []
            ]
          ]
        in mapper.expr mapper { new_expr with pexp_attributes }

      (** obj##.var := value *)
      | [%expr [%e? obj] ##. [%e? meth] := [%e? value]] ->
        let meth = exp_to_string meth in
        let o = random_var () in
        let obj' = Exp.ident ~loc:obj.pexp_loc @@ lid o in
        let v = random_var () in
        let v_lid = lid v in
        let value' = Exp.ident ~loc:value.pexp_loc v_lid in
        let new_expr =
          [%expr
            let [%p pvar v] = [%e value] in
            let [%p pvar o] = [%e obj] in
            let _ = [%e
              constrain_types
                obj'
                v_lid [%type: 'A]
                meth (Js.type_ "gen_prop" [[%type: <set : 'A -> unit ; ..> ]])
                []
            ]
            in [%e Js.unsafe "set" [ obj' ; str @@ unescape meth ; value']]
          ]
        in mapper.expr mapper { new_expr with pexp_attributes }

      (** obj##meth arg1 arg2 .. *)
      | {pexp_desc = Pexp_apply
             ([%expr [%e? obj] ## [%e? meth]] , args)
        } ->
        let meth = exp_to_string meth in
        let new_expr =
          method_call obj meth args
        in mapper.expr mapper { new_expr with pexp_attributes }
      (** obj##meth *)
      | [%expr [%e? obj] ## [%e? meth]] ->
        let meth = exp_to_string meth in
        let new_expr =
          method_call obj meth []
        in mapper.expr mapper { new_expr with pexp_attributes }


      (** new%js constr] *)
      | [%expr [%js [%e? {pexp_desc = Pexp_new constr}]]] ->
        let new_expr =
          new_object constr []
        in mapper.expr mapper { new_expr with pexp_attributes }
      (** new%js constr arg1 arg2 ..)] *)
      | {pexp_desc = Pexp_apply
             ([%expr [%js [%e? {pexp_desc = Pexp_new constr}]]]
             , args)
        } ->
        let new_expr =
          new_object constr args
        in mapper.expr mapper { new_expr with pexp_attributes }


      (** object%js ... end *)
      | [%expr [%js [%e? {pexp_desc = Pexp_object class_struct} ]]] ->
        let fields = preprocess_literal_object class_struct.pcstr_fields in
        let new_expr = match fields with
          | `Fields fields ->
             literal_object class_struct.pcstr_self fields
          | `Error e -> Exp.extension e
        in mapper.expr mapper { new_expr with pexp_attributes }

      | _ -> default_mapper.expr mapper expr
    )
  }

let () = run_main js_mapper
