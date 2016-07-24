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

(** arg1 -> arg2 -> ... -> ret *)
let arrows args ret =
  List.fold_right (fun (l, ty) fun_ -> Typ.arrow l ty fun_)
    args
    ret

let targs_arrows targs =
  List.map (fun (l,args,res) -> l, arrows args res) targs

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

let app_arg e = (Js.nolabel, e)

let inject_arg e = Js.unsafe "inject" [e]

let inject_args args =
  Exp.array (List.map (fun e -> Js.unsafe "inject" [e]) args)

let obj_arrows targs tres =
  let lbl, tobj, tobjres = List.hd targs and targs = List.tl targs in
  arrows ((lbl, Js.type_ "t" [arrows tobj tobjres]) :: (targs_arrows targs)) tres

let invoker uplift downlift body desc =
  let labels = List.map fst desc in
  let default_loc' = !default_loc in
  default_loc := Location.none;
  let arg i _ = "a" ^ string_of_int i in
  let args = List.mapi arg labels in

  let typ s = Typ.constr (lid s) [] in

  let argi s i = s ^ "_" ^ string_of_int i
  in
  let targs = List.map2 (fun (l,args) s -> l, List.mapi (fun i l -> l, typ (argi s i)) args, typ (s ^ "_ret") ) desc args in

  let ntargs =
    List.map2 (fun (_l,args) s -> (s ^ "_ret") :: List.mapi (fun i _l -> argi s i) args) desc args
    |> List.concat
  in

  let res = "res" in
  let tres = typ res in

  let twrap = uplift targs tres in
  let tfunc = downlift targs tres in

  let ebody = body (List.map (fun s -> Exp.ident (lid s)) args) in

  let efun label arg expr =
    Exp.fun_ label None (Pat.var (Location.mknoloc arg)) expr
  in
  let efun = List.fold_right2 efun labels args ebody in

  let invoker = [%expr ((fun _ -> [%e efun]) : [%t twrap] -> [%t tfunc]) ] in

  let result = List.fold_right Exp.newtype (res :: ntargs) invoker in

  default_loc := default_loc';
  result

let method_call ~loc obj meth args =
  let gloc = {obj.pexp_loc with Location.loc_ghost = true} in
  let obj = Exp.constraint_ ~loc:gloc obj (Js.type_ "t" [ [%type: < .. > ] ]) in
  let invoker = invoker
      (fun targs tres ->
         arrows (targs_arrows targs) (Js.type_ "meth" [tres]))
      obj_arrows
      (fun eargs ->
         let eobj = List.hd eargs in
         let eargs = inject_args (List.tl eargs) in
         Js.unsafe "meth_call" [eobj; str (unescape meth); eargs])
      ((Js.nolabel,[]) :: List.map (fun (l,_) -> l,[]) args)
  in
  Exp.apply invoker (
    app_arg
      (Exp.fun_ ~loc Js.nolabel None
         (Pat.var ~loc:Location.none (Location.mknoloc "x"))
         (Exp.send ~loc (Exp.ident ~loc:gloc (lid ~loc:gloc "x")) meth)) ::
    app_arg obj :: args
  )

let prop_get ~loc obj prop =
  let gloc = {obj.pexp_loc with Location.loc_ghost = true} in
  let obj = Exp.constraint_ ~loc:gloc obj (Js.type_ "t" [ [%type: < .. > ] ]) in
  let invoker = invoker
      (fun targs tres ->
         arrows (targs_arrows targs) (Js.type_ "gen_prop" [[%type: <get: [%t tres]; ..> ]]))
      obj_arrows
      (fun eargs -> Js.unsafe "get" [List.hd eargs; str (unescape prop)])
      [Js.nolabel,[]]
  in
  Exp.apply invoker (
    app_arg
      (Exp.fun_ ~loc Js.nolabel None
         (Pat.var ~loc:Location.none (Location.mknoloc "x"))
         (Exp.send ~loc (Exp.ident ~loc:gloc (lid ~loc:gloc "x")) prop)) ::
    [app_arg obj]
  )

let prop_set ~loc obj prop value =
  let gloc = {obj.pexp_loc with Location.loc_ghost = true} in
  let obj = Exp.constraint_ ~loc:gloc obj (Js.type_ "t" [ [%type: < .. > ] ]) in
  let invoker = invoker
      (fun targs _tres -> match targs with
         | [_,[],tobj; _,[],targ] ->
           arrows [Js.nolabel,tobj]
             (Js.type_ "gen_prop" [[%type: <set: [%t targ] -> unit; ..> ]])
         | _ -> assert false)
      (fun targs _tres -> obj_arrows targs [%type: unit])
      (function
        | [obj; arg] ->
          Js.unsafe "set" [obj; str (unescape prop); inject_arg arg]
        | _ -> assert false)
      [Js.nolabel, []; Js.nolabel, []]
  in
  Exp.apply invoker (
    app_arg
      (Exp.fun_ ~loc Js.nolabel None
         (Pat.var ~loc:Location.none (Location.mknoloc "x"))
         (Exp.send ~loc (Exp.ident ~loc:gloc (lid ~loc:gloc "x")) prop)) ::
    [app_arg obj; app_arg value]
  )

(** Instantiation of a class, used by new%js. *)
let new_object constr args =
  let invoker = invoker
      (fun _targs _tres -> [%type: unit])
      (fun targs tres ->
         let _unit = List.hd targs and targs = List.tl targs in
         let tres = Js.type_ "t" [tres] in
         let arrow = arrows (targs_arrows targs) tres in
         arrows [(Js.nolabel, Js.type_ "constr" [arrow])] arrow)
      (function
        | (constr :: args) ->
          Js.unsafe "new_obj" [constr; inject_args args]
        | _ -> assert false)
      ((Js.nolabel,[]) :: List.map (fun (l,_) -> l, []) args)
  in
  Exp.apply invoker (
    app_arg [%expr ()] :: app_arg (Exp.ident ~loc:constr.loc constr) :: args
  )


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
type field_desc =
  [ `Meth of string Asttypes.loc * Asttypes.private_flag * Asttypes.override_flag * Parsetree.expression * Asttypes.arg_label list
  | `Val  of string Asttypes.loc * Asttypes.mutable_flag * Asttypes.override_flag * Parsetree.expression ]


let preprocess_literal_object mappper fields : [ `Fields of field_desc list | `Error of _ ] =

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
      let names = check_name id names in
      let body = mappper body in
      names, (`Val (id, mut, bang, body) :: fields)
    | Pcf_method (id, priv, Cfk_concrete (bang, body)) ->
      let names = check_name id names in
      let body = format_meth (mappper body) in
      let rec create_meth_ty exp = match exp.pexp_desc with
          | Pexp_fun (label,_,_,body) ->
            label :: create_meth_ty body
          | _ -> []
      in
      let fun_ty = create_meth_ty body in
      names, (`Meth (id, priv, bang, body, fun_ty) :: fields)
    | _ ->
      Location.raise_errorf ~loc:exp.pcf_loc
        "This field is not valid inside a js literal object."
  in
  try
    `Fields (List.rev (snd (List.fold_left f (S.empty, []) fields)))
  with Location.Error error -> `Error (extension_of_error error)

let literal_object ~loc self_id ( fields : field_desc list) =

  let create_method_type f (_l,args,ret_ty) =
    match f with
    | `Val  (_id, Mutable, _,   _body)    -> Js.type_ "prop" [ret_ty]
    | `Val  (_id, Immutable, _, _body)    -> Js.type_ "readonly_prop" [ret_ty]
    | `Meth (_id, _, _,         _body, _) -> arrows (List.tl args) (Js.type_ "meth" [ret_ty])
  in

  let name = function
    | `Val  (id, _, _, _body)          -> id.txt
    | `Meth (id, _, _, _body, _fun_ty) -> id.txt
  in

  let get_loc = function
    | `Val  (id, _, _, _body)          -> id.loc
    | `Meth (id, _, _, _body, _fun_ty) -> id.loc
  in

  let name_loc = function
    | `Val  (id, _, _, _body)          -> id
    | `Meth (id, _, _, _body, _fun_ty) -> id
  in

  let body = function
    | `Val  (_id, _, _, body)          -> body
    | `Meth (_id, _, _, body, _fun_ty) -> [%expr fun [%p self_id] -> [%e body] ]
  in

  let wrap arg = function
    | `Val  (_id, _, _, _body)          -> arg
    | `Meth (_id, _, _, _body, _fun_ty) -> Js.fun_ "wrap_meth_callback" [ arg ]
  in

  let invoker = invoker

      (fun targs tres ->
         let targs' =
           List.map2 (fun f ((l,_,_) as a) ->
             let ty = create_method_type f a in
             let ty = match f with
               | `Val _  -> ty
               | `Meth _ -> Typ.arrow Js.nolabel (Js.type_ "t" [tres]) ty
             in
             l, ty) fields targs
         in
         arrows targs' tres)
      (fun targs tres ->
         arrows
           (targs_arrows
              (List.map (fun (l,args,ret) ->
                 let args =
                   match args with
                   | [] as x -> x
                   | _::xs   -> (Js.nolabel, Js.type_ "t" [tres])::xs
                 in
                 l,args,ret)
                 targs
              )
           )
           (Js.type_ "t" [tres])
      )
      (fun args ->
         Js.unsafe
           "obj"
           [Exp.array (
              List.map2
                (fun arg f ->
                   tuple [str (unescape (name f)); inject_arg (wrap arg f) ]
                )
                args fields
            )]
      )
      (List.map (function
         | `Val _ -> Js.nolabel, []
         | `Meth (_id, _, _, _body, fun_ty) -> Js.nolabel, Js.nolabel :: fun_ty) fields)
  in

  let fake_object =
    Exp.object_ ~loc:loc
      { pcstr_self = (Pat.var ~loc:Location.none (Location.mknoloc "self"));
        pcstr_fields =
          (List.map
             (fun f ->
                let gloc = { (get_loc f) with Location.loc_ghost = true} in
                let apply e = match f with
                  | `Val _ -> e
                  | `Meth _ -> Exp.apply e [Js.nolabel, [%expr ((fun x -> assert false) : 'a -> 'a Js.t) self ] ]
                in
                { pcf_loc = get_loc f;
                  pcf_attributes = [];
                  pcf_desc =
                    Pcf_method
                      (name_loc f,
                       Public,
                       Cfk_concrete (Fresh, apply (Exp.ident ~loc:gloc (lid ~loc:gloc (name f))))
                      )
                })
             fields)
      }
  in
  Exp.apply invoker (
    app_arg (List.fold_right (fun f fun_ ->
      (Exp.fun_ ~loc Js.nolabel None
         (Pat.var ~loc:Location.none (Location.mknoloc (name f)))
         fun_))
      fields
      fake_object
    ) :: (List.map (fun f -> app_arg (body f)) fields))

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
          let prop = exp_to_string meth in
          let new_expr = prop_get ~loc:meth.pexp_loc obj prop in
          mapper.expr mapper  { new_expr with pexp_attributes }

        (* obj##.var := value *)
        | [%expr [%e? [%expr [%e? obj] ##. [%e? meth]] as res] := [%e? value]] ->
          let obj = mapper.expr mapper obj in
          let value = mapper.expr mapper value in
          let prop = exp_to_string meth in
          let new_expr = prop_set ~loc:res.pexp_loc obj prop value in
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
        | [%expr [%js [%e? {pexp_desc = Pexp_object class_struct; _} ]]] as expr ->
          let loc = expr.pexp_loc in
          let fields = preprocess_literal_object (mapper.expr mapper) class_struct.pcstr_fields in
          let new_expr = match fields with
            | `Fields fields ->
              literal_object ~loc class_struct.pcstr_self fields
            | `Error e -> Exp.extension e in
          mapper.expr mapper  { new_expr with pexp_attributes }

        | _ -> default_mapper.expr mapper expr
      in
      default_loc := prev_default_loc;
      new_expr
    )
  }
