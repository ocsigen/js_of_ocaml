open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree

open Ast_convenience


let with_loc f { txt ; loc } =
  (f txt) [@metaloc loc]



let rnd = Random.State.make [|0x313511d4|]
let random_var () =
  Format.sprintf "x%08Lx" (Random.State.int64 rnd 0x100000000L)
let random_tvar () =
  Format.sprintf "A%08Lx" (Random.State.int64 rnd 0x100000000L)

let inside_Js = lazy
  (try
     String.lowercase @@
     Filename.basename @@
     Filename.chop_extension !Location.input_name = "js"
   with Invalid_argument _ -> false)

module Js = struct

  let type_ ?loc s args =
    if Lazy.force inside_Js
    then Typ.constr ?loc (lid s) args
    else Typ.constr ?loc (lid @@ "Js."^s) args

  let unsafe ?loc s args =
    let args = List.map (fun x -> "",x) args in
    if Lazy.force inside_Js
    then Exp.(apply (ident ?loc @@ lid ("Unsafe."^s)) args)
    else Exp.(apply (ident ?loc @@ lid ("Js.Unsafe."^s)) args)

  let fun_ ?loc s args =
    let args = List.map (fun x -> "",x) args in
    if Lazy.force inside_Js
    then Exp.(apply (ident ?loc @@ lid s) args)
    else Exp.(apply (ident ?loc @@ lid ("Js."^s)) args)

  let string ?loc arg = fun_ ?loc "string" [str arg]

end


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

  (* [($obj$ : 'B Js.t)] *)
  let cstr =
    Exp.constraint_
      obj
      (Js.type_ "t" [Typ.var "B"])
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
        let _ = fun (x : 'B) -> [%e body] in
        [%e res_bindings];
    end in M.res
  ]


let fresh_type loc = Typ.var ~loc @@ random_tvar ()

let arrows ?loc args ret =
  List.fold_right (fun (l, ty) fun_ -> Typ.arrow ?loc l ty fun_)
    args
    ret

let sequence ?loc l last =
  match l with
  | [] -> last
  | h :: t ->
    let e =
      List.fold_left (Exp.sequence ?loc) h t
    in Exp.sequence ?loc e last

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


let tunit () = Typ.constr (lid "unit") []

module S = Map.Make(String)

(** For each method 1) we remove Pexp_poly (should only be inside methods)
    and we add the self argument.
*)
let format_meth self_id body =
  match body.pexp_desc with
  | Pexp_poly (e, _) ->
    [%expr
      fun [%p self_id] -> [%e e ]
    ] [@metaloc body.pexp_loc]
  | _ -> body

let is_optional attrs =
  List.exists (fun ({txt},_) -> txt = "optional") attrs

let preprocess_literal_object ?(optional=false) self_id fields =
  let is_opt e = is_optional e.pcf_attributes in

  let check_name id names =
    if S.mem id.txt names then
      let id' = S.find id.txt names in
      let sub = [Location.errorf ~loc:id'.loc "Duplicated val or method %S." id'.txt] in
      Location.raise_errorf ~loc:id.loc ~sub "Duplicated val or method %S." id.txt
    else
      S.add id.txt id names
  in

  let f (names, fields) exp = match exp.pcf_desc with
    | Pcf_val (id, mut, Cfk_concrete (bang, body)) ->
      let names = check_name id names in
      names, (`Val (id, mut, bang, body, optional || is_opt exp) :: fields)
    | Pcf_method (id, priv, Cfk_concrete (bang, body)) ->
      let names = check_name id names in
      names, (`Meth (id, priv, bang, format_meth self_id body, optional || is_opt exp) :: fields)
    | _ ->
      Location.raise_errorf ~loc:exp.pcf_loc
        "This field is not valid inside a js literal object."
  in
  try `Fields (snd @@ List.fold_left f (S.empty, []) fields)
  with Location.Error error -> `Error (extension_of_error error)


let literal_object ?loc fields =
  let obj_lid = random_var () in
  let constr_lid = random_var () in
  let self_ty = random_tvar () in

  let fields =
    List.map (function
      | `Val (n, mut, bang, body, is_opt) ->
        let ty = fresh_type n.loc in
        `Val (n, mut, bang, body, ty, is_opt)
      | `Meth (n, priv, bang, body, is_opt) ->
        let rec create_meth_ty exp = match exp.pexp_desc with
          | Pexp_fun (label,_,_,body) ->
            (label, fresh_type exp.pexp_loc) :: create_meth_ty body
          | _ -> []
        in
        let ret_ty = fresh_type body.pexp_loc in
        let fun_ty = create_meth_ty body in
        `Meth (n, priv, bang, body, (fun_ty, ret_ty), is_opt)
    )
      fields
  in

  let set_field = function
    | `Val (id, _, _, body, ty, _) ->
      Js.unsafe "set" [ evar obj_lid ; Js.string id.txt ; Exp.constraint_ body ty ]
        [@metaloc body.pexp_loc]
    | `Meth (id, _, _, body, (fun_ty, ret_ty), _) ->
      Js.unsafe "set" [
        evar obj_lid ;
        Js.string id.txt ;
        Exp.constraint_
          (Js.fun_ "wrap_meth_callback" [ (evar id.txt)[@metaloc id.loc] ])
          (Js.type_ "meth_callback" [
             Typ.var self_ty ;
             arrows (List.tl fun_ty) ret_ty
           ])
      ]
  in

  let assignments = sequence (List.map set_field fields) (unit ()) in
  let meth_def n body (fun_ty,ret_ty) e =
    [%expr let [%p Pat.var n] = ([%e body] : [%t arrows fun_ty ret_ty]) in [%e e] ] in
  let meth_defs_and_field_assignments =
    List.fold_right
      (fun field e -> match field with
         | `Val _ -> e
         | `Meth (n, _, _, body, ty, _) -> meth_def n body ty e)
      fields
      assignments in

  let obj_field_meth_type = function
    | `Val  (id, _, _, _body, ty, _) ->
      (id.txt, [], Js.type_ "prop" [ty])
    | `Meth (id, _, _, _body, (fun_ty, ret_ty), _) ->
      (id.txt, [], arrows (List.tl fun_ty) (Js.type_ "meth" [ret_ty]))
  in

  let ty_rows = List.map obj_field_meth_type fields in

  let optional_part_ty = Typ.object_ ty_rows Open in
  let obj_ty =
    (* We deliberately leave optional properties/methods out
       so that we don't need to cast the object manually. *)
    let l =
      ("__optional__", [], optional_part_ty) ::
      (List.map obj_field_meth_type
        ( List.filter
            (function `Val  (_, _, _, _, _, is_opt) |
                      `Meth (_, _, _, _, _, is_opt) -> not is_opt)
            fields )
      )
    in Typ.object_ l Closed
  in
  [%expr
    let ([%p pvar obj_lid] : [%t Typ.alias (Js.type_ "t" [obj_ty]) self_ty]) =
      let [%p pvar constr_lid] : [%t Js.type_ "constr" [Typ.var self_ty]] =
        [%e Js.unsafe "variable" [str "Object"]] in
      let [%p pvar obj_lid] = [%e Js.unsafe "new_obj" [ evar constr_lid ; Exp.array []]] in
      [%e meth_defs_and_field_assignments] ;
      [%e evar obj_lid]
    in [%e evar obj_lid]
  ]

let js_mapper _args =
  { default_mapper with
    expr = (fun mapper expr ->
      default_loc := expr.pexp_loc;
      let { pexp_attributes } = expr in
      match expr with

      (** [%js obj.var] *)
      | [%expr [%js [%e? {pexp_desc = Pexp_field (obj, meth) }]]] ->
        let o = random_var () in
        let meth = String.concat "." @@ Longident.flatten meth.txt in
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

      (** [%js obj.var := value] *)
      | [%expr [%js [%e? {pexp_desc = Pexp_field (obj, meth) }] := [%e? value]]] ->
        let meth = String.concat "." @@ Longident.flatten meth.txt in
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

      (** [%js obj#meth] arg1 arg2 .. *)
      | {pexp_desc = Pexp_apply
             ([%expr [%js [%e? {pexp_desc = Pexp_send (obj, meth) }] ]]
             , args)
        } ->
        let new_expr =
          method_call obj meth args
        in mapper.expr mapper { new_expr with pexp_attributes }
      (** [%js obj#meth] *)
      | [%expr [%js [%e? {pexp_desc = Pexp_send (obj, meth) }] ]] ->
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
      | [%expr [%js [%e? {pexp_desc = Pexp_object class_struct} as obj_exp ]]] ->
        let optional = is_optional obj_exp.pexp_attributes in
        let fields =
          preprocess_literal_object
            ~optional class_struct.pcstr_self
            class_struct.pcstr_fields
        in
        let new_expr = match fields with
          | `Fields fields -> literal_object fields
          | `Error e -> Exp.extension e
        in mapper.expr mapper { new_expr with pexp_attributes }

      | _ -> default_mapper.expr mapper expr
    )
  }

let () = run_main js_mapper
