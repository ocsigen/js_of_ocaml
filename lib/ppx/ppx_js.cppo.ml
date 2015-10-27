(* For implicit optional argument elimination. Annoying with Ast_helper. *)
[@@@ocaml.warning "-48"]
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Ast_convenience

(** Check if an expression is an identifier and returns it.
    Raise a Location.error if it's not.
*)
let exp_to_string = function
  | {pexp_desc= Pexp_ident {txt = Longident.Lident s; _}; _} -> s
  | {pexp_desc= Pexp_construct ({txt = Longident.Lident s; _}, None); _}
    when String.length s > 0
      && s.[0] >= 'A'
      && s.[0] <= 'Z' -> "_"^s
  | {pexp_loc; _} ->
     Location.raise_errorf
       ~loc:pexp_loc
       "Javascript methods or attributes can only be simple identifiers."

let lid ?(loc= !default_loc) str =
  Location.mkloc (Longident.parse str) loc

let mk_loc ?(loc= !default_loc) x = Location.mkloc x loc

let mk_id ?loc str =
  let exp = Exp.ident ?loc (lid ?loc str)
  and pat = Pat.var ?loc (mk_loc ?loc str) in
  (exp, pat)


(** arg1 -> arg2 -> ... -> ret *)
let arrows args ret =
  List.fold_right (fun (l, ty) fun_ -> Typ.arrow l ty fun_)
    args
    ret

(** fun arg1 arg2 ... -> ret *)
let funs args ret =
  List.fold_right (fun pat next_fun_ -> lam pat next_fun_)
    args
    ret

let rnd = Random.State.make [|0x313511d4|]
let random_var () =
  Format.sprintf "jsoo_%08Lx" (Random.State.int64 rnd 0x100000000L)
let random_tvar () =
  Format.sprintf "jsoo_%08Lx" (Random.State.int64 rnd 0x100000000L)

let inside_Js = lazy
  (try
     Filename.basename (Filename.chop_extension !Location.input_name) = "js"
   with Invalid_argument _ -> false)

module Js = struct

  let type_ ?loc s args =
    if Lazy.force inside_Js
    then Typ.constr ?loc (lid s) args
    else Typ.constr ?loc (lid ("Js."^s)) args

#if OCAML_VERSION < (4, 03, 0)
  let nolabel = ""
#else
  let nolabel = Nolabel
#endif

  let unsafe ?loc s args =
    let args = List.map (fun x -> nolabel,x) args in
    if Lazy.force inside_Js
    then Exp.(apply ?loc (ident ?loc (lid ?loc ("Unsafe."^s))) args)
    else Exp.(apply ?loc (ident ?loc (lid ?loc ("Js.Unsafe."^s))) args)

  let fun_ ?loc s args =
    let args = List.map (fun x -> nolabel,x) args in
    if Lazy.force inside_Js
    then Exp.(apply ?loc (ident ?loc (lid ?loc s)) args)
    else Exp.(apply ?loc (ident ?loc (lid ?loc ("Js."^s))) args)

end


let fresh_type loc = Typ.var ~loc (random_tvar ())

let unescape lab =
  if lab = "" then lab
  else
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
let constrain_types ?loc obj res res_typ meth meth_typ args =
  let typ_var = fresh_type obj.pexp_loc in


  (* We occasionally use double constraints :
     ( (jsoo_self : < .. > Js.t) : 'jsoo_ad7fbbdd Js.t)
     It improves error messages by hiding the hairy type variable if jsoo_self
     is not even of the right shape.
  *)

  (* [($obj$ : <typ_var> Js.t)] *)
  let cstr =
    Exp.constraint_
      [%expr ([%e obj] : [%t Js.type_ "t" [ [%type: < .. > ] ] ] ) ]
      (Js.type_ "t" [typ_var] )
  in

  let e_x, p_x = mk_id ~loc:obj.pexp_loc "x" in
  (* [($x$#meth : $meth_typ$)] *)
  let gloc = match loc with
    | None -> None
    | Some l -> Some {l with Location.loc_ghost = true}
  in
  let interesting = (Location.mknoloc "merlin.teresting", PStr []) in
  let body =
    Exp.constraint_ ?loc:gloc (Exp.send ~attrs:[interesting] ?loc e_x meth)
      (* TODO ajouter contraintes _ -> _ -> _ Js.meth *)
      meth_typ
  in

  let res_constr = [%expr ([%e res] : [%t res_typ])] in
  let res_bindings =
    List.fold_right
      (fun (ev, t) e' ->
         [%expr let _ = ([%e ev] : [%t t]) in [%e e']])
      args
      res_constr
  in
  [%expr
    let module M = struct
      let res =
        let _ = [%e cstr] in
        let _ = fun ([%p p_x] : [%t typ_var]) -> [%e body] in
        [%e res_bindings];
    end in M.res
  ]

let invoker labels =
  let default_loc' = !default_loc in
  default_loc := Location.none;
  let arg i _ = "a" ^ string_of_int i in
  let args = List.mapi arg labels in

  let typ s = Typ.constr (lid s) [] in

  let targs = List.map2 (fun l s -> l, typ s) labels args in
  let eargs = List.map (fun s -> Js.unsafe "inject" [Exp.ident (lid s)]) args in

  let tobj = typ "obj" and tres = typ "res" in
  let tmeth = arrows targs (Js.type_ "meth" [tres]) in
  let tfunc = arrows targs tres in

  let ebody =
    Js.unsafe "meth_call" [[%expr obj]; [%expr meth]; Exp.array eargs] in

  let efun label arg expr =
    Exp.fun_ label None (Pat.var (Location.mknoloc arg)) expr
  in
  let efun = List.fold_right2 efun labels args ebody in

  let invoker = [%expr
    ((fun obj meth _ -> [%e efun]) :
       [%t tobj] Js.t -> string -> ([%t tobj] -> [%t tmeth]) -> [%t tfunc])
  ] in

  let result =
    List.fold_right Exp.newtype ("obj" :: "res" :: args) invoker
  in

  default_loc := default_loc';
  result

let method_call ~loc obj meth args =
  let gloc = {obj.pexp_loc with Location.loc_ghost = true} in
  let obj = [%expr ([%e obj] : [%t Js.type_ "t" [ [%type: < .. > ] ] ] ) ] in
  let invoker = invoker (List.map fst args) in
  let arg e = Js.nolabel, e in
  Exp.apply invoker (arg obj ::
                     arg (str (unescape meth)) ::
                     arg (Exp.fun_ ~loc Js.nolabel None
                            (Pat.var ~loc:Location.none (Location.mknoloc "x"))
                            (Exp.send ~loc (Exp.ident ~loc:gloc (lid ~loc:gloc "x")) meth))
                     ::
                     args)

(** Instantiation of a class, used by new%js. *)
let new_object constr args =
  let args = List.map (fun (l,e) -> (e, (l,fresh_type constr.loc))) args in
  let obj_type = Js.type_ "t" [fresh_type constr.loc] in
  let constr_fun_type = arrows (List.map snd args) obj_type in
  let args =
    Exp.array
      (List.map
         (fun (e, (_l,t)) -> Js.unsafe "inject" [Exp.constraint_ e t])
         args)
  in
  let x = random_var () in
  let constr =
    Exp.constraint_
      (Exp.ident ~loc:constr.loc constr)
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

let preprocess_literal_object mappper fields =

  let check_name id names =
    let txt = unescape id.txt in
    if S.mem txt names then
      let id' = S.find txt names in
      (* We point out both definitions in locations (more convenient for the user). *)
      let details id =
        if id.txt <> txt
        then Printf.sprintf " (normalized to %S)" txt
        else ""
      in
      let sub = [Location.errorf ~loc:id'.loc
                   "Duplicated val or method %S%s." id'.txt (details id')] in
      Location.raise_errorf ~loc:id.loc ~sub
        "Duplicated val or method %S%s." id.txt (details id)
    else
      S.add txt id names
  in

  let f (names, fields) exp = match exp.pcf_desc with
    | Pcf_val (id, mut, Cfk_concrete (bang, body)) ->
      let ty = fresh_type id.loc in
      let names = check_name id names in
      let body = mappper body in
      names, (`Val (id, mut, bang, body, ty) :: fields)
    | Pcf_method (id, priv, Cfk_concrete (bang, body)) ->
      let names = check_name id names in
      let body = format_meth (mappper body) in
      let rec create_meth_ty exp = match exp.pexp_desc with
          | Pexp_fun (label,_,_,body) ->
            (label, fresh_type exp.pexp_loc) :: create_meth_ty body
          | _ -> []
      in
      let ret_ty = fresh_type body.pexp_loc in
      let fun_ty = create_meth_ty body in
      names, (`Meth (id, priv, bang, body, (fun_ty, ret_ty)) :: fields)
    | _ ->
      Location.raise_errorf ~loc:exp.pcf_loc
        "This field is not valid inside a js literal object."
  in
  try
    `Fields (List.rev (snd (List.fold_left f (S.empty, []) fields)))
  with Location.Error error -> `Error (extension_of_error error)

(** Desugar something like this:

  object%js (self)
    val mutable foo = 3
    method bar x = 3 + x
  end

to:

  let make_obj foo bar =
    (Js.Unsafe.obj
       [|("foo", (Js.Unsafe.inject foo));("bar", (Js.Unsafe.inject bar))|]
     : < foo :'jsoo_173316d7 Js.prop ;
         bar :'jsoo_6d9ac43e -> 'jsoo_29529091 Js.meth >
       Js.t as 'jsoo_32b5ee21)
  and foo: 'jsoo_173316d7 = 3
  and bar =
    Js.wrap_meth_callback
      (fun self  -> fun x -> 3 + x
      : 'jsoo_32b5ee21 -> 'jsoo_6d9ac43e -> 'jsoo_29529091)
  in make_obj foo bar


 *)

let literal_object self_id fields =
  let self_type = random_tvar () in

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

  let rec annotate_body fun_ty ret_ty body = match fun_ty, body with
    | (_,ty) :: types,
      ({ pexp_desc = Pexp_fun (label, e_opt, pat, body); _} as expr) ->
       let constr = Pat.constraint_ pat ty in
       {expr with
         pexp_desc =
           Pexp_fun (label, e_opt, constr,
                     annotate_body types ret_ty body)
       }
    | [], body -> Exp.constraint_ ~loc:body.pexp_loc body ret_ty
    | _ -> raise (Invalid_argument "Inconsistent number of arguments")
  in

  let create_value = function
    | `Val (id, _, _, body, ty) ->
      (id.txt, Exp.constraint_ ~loc:body.pexp_loc body ty)

    | `Meth (id, _, _, body, (fun_ty, ret_ty)) ->
       (id.txt,
        Js.fun_
          "wrap_meth_callback"
          [
            annotate_body ((Js.nolabel,Typ.var self_type) :: fun_ty) ret_ty [%expr fun [%p self_id] -> [%e body]]
          ])

  in

  let values = List.map create_value fields in

  let make_obj =
    funs
      (List.map (fun (name,_exp) -> pvar name) values)
      (Exp.constraint_
         (Js.unsafe
            "obj"
            [Exp.array (
               List.map
                 (fun (name, _exp) ->
                    tuple [str (unescape name); Js.unsafe "inject" [evar name] ]
                 )
                 values
             )])
         (Typ.alias (Js.type_ "t" [obj_type]) self_type))
  in

  let value_declaration =
    Exp.let_
      Nonrecursive
      ( Vb.mk (pvar "make_obj") make_obj ::
        List.map (fun (name, exp) -> Vb.mk (pvar name) exp) values
      )
      (app (evar "make_obj") (List.map (fun (name,_) -> evar name) values))
  in

  value_declaration

let js_mapper _args =
  { default_mapper with
    expr = (fun mapper expr ->
      let prev_default_loc = !default_loc in
      default_loc := expr.pexp_loc;
      let { pexp_attributes; _ } = expr in
      let new_expr = match expr with
        (* obj##.var *)
        | [%expr [%e? obj] ##. [%e? meth] ] ->
          let obj = mapper.expr mapper obj in
          let meth = exp_to_string meth in
          let e_obj, p_obj = mk_id ~loc:obj.pexp_loc "jsoo_obj" in
          let e_res, p_res = mk_id ~loc:expr.pexp_loc "jsoo_res" in
          let new_expr =
            [%expr
              let [%p p_obj] = [%e obj] in
              let [%p p_res] = [%e Js.unsafe "get" [e_obj ; str (unescape meth)]] in
              [%e
                constrain_types
                  e_obj
                  e_res [%type: 'jsoo_res]
                  meth (Js.type_ "gen_prop" [[%type: <get : 'jsoo_res; ..> ]])
                  []
              ]
            ]
          in
          mapper.expr mapper  { new_expr with pexp_attributes }

        (* obj##.var := value *)
        | [%expr [%e? [%expr [%e? obj] ##. [%e? meth]] as res] := [%e? value]] ->
          default_loc := res.pexp_loc ;
          let obj = mapper.expr mapper  obj in
          let value = mapper.expr mapper  value in
          let meth = exp_to_string meth in
          let e_obj, p_obj = mk_id ~loc:obj.pexp_loc "jsoo_obj" in
          let e_value, p_value = mk_id ~loc:value.pexp_loc "jsoo_arg" in
          let new_expr =
            [%expr
              let [%p p_obj] = [%e obj]
              and [%p p_value] = [%e value] in
              let _ = [%e
                constrain_types
                  e_obj
                  e_value [%type: 'jsoo_arg]
                  meth (Js.type_ "gen_prop" [[%type: <set : 'jsoo_arg -> unit ; ..> ]])
                  []
              ]
              in
              [%e Js.unsafe ~loc:expr.pexp_loc "set" [ e_obj ; str (unescape meth) ; e_value]]
            ]
          in
          mapper.expr mapper  { new_expr with pexp_attributes }

        (* obj##meth arg1 arg2 .. *)
        (* obj##(meth arg1 arg2) .. *)
        | {pexp_desc = Pexp_apply (([%expr [%e? obj] ## [%e? meth]] as expr), args); _}
        | [%expr [%e? obj] ## [%e? {pexp_desc = Pexp_apply((meth as expr),args); _}]]
          ->
          let meth = exp_to_string meth in
          let obj = mapper.expr mapper  obj in
          let args = List.map (fun (s,e) -> s, mapper.expr mapper e) args in
          let new_expr = method_call ~loc:expr.pexp_loc obj meth args in
          mapper.expr mapper  { new_expr with pexp_attributes }
        (* obj##meth *)
        | ([%expr [%e? obj] ## [%e? meth]] as expr) ->
          let obj = mapper.expr mapper  obj in
          let meth = exp_to_string meth in
          let new_expr = method_call ~loc:expr.pexp_loc obj meth [] in
          mapper.expr mapper  { new_expr with pexp_attributes }

        (* new%js constr] *)
        | [%expr [%js [%e? {pexp_desc = Pexp_new constr; _}]]] ->
          let new_expr = new_object constr [] in
          mapper.expr mapper { new_expr with pexp_attributes }
        (* new%js constr arg1 arg2 ..)] *)
        | {pexp_desc = Pexp_apply
                         ([%expr [%js [%e? {pexp_desc = Pexp_new constr; _}]]]
                         , args); _ } ->
          let args = List.map (fun (s,e) -> s, mapper.expr mapper e) args in
          let new_expr =
            new_object constr args
          in
          mapper.expr mapper  { new_expr with pexp_attributes }

        (* object%js ... end *)
        | [%expr [%js [%e? {pexp_desc = Pexp_object class_struct; _} ]]] ->
          let fields = preprocess_literal_object (mapper.expr mapper) class_struct.pcstr_fields in
          let new_expr = match fields with
            | `Fields fields ->
              literal_object class_struct.pcstr_self fields
            | `Error e -> Exp.extension e in
          mapper.expr mapper  { new_expr with pexp_attributes }

        | _ -> default_mapper.expr mapper expr
      in
      default_loc := prev_default_loc;
      new_expr
    )
  }
