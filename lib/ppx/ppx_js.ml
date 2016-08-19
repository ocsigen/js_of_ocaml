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

(* -- FIXME --
   [merlin_noloc] is an attempt to hide some ast node from merlin
   when using merlin-type-enclosing-go-{up,down}.
   It turns out not to be working properly, just hiding location instead of the all
   ast node.
*)
let merlin_noloc = { txt = "merlin.loc"; loc = Location.none }, PStr []

module Js : sig
  val type_  : ?loc:Ast_helper.loc -> string -> Parsetree.core_type list  -> Parsetree.core_type
  val unsafe : ?loc:Ast_helper.loc -> string -> Parsetree.expression list -> Parsetree.expression
  val fun_   : ?loc:Ast_helper.loc -> string -> Parsetree.expression list -> Parsetree.expression
end = struct

  let js_dot name =
    if Lazy.force inside_Js
    then name
    else "Js." ^ name

  let js_unsafe_dot name = js_dot ("Unsafe." ^ name)

  let type_ ?loc s args =
    Typ.constr ?loc (lid (js_dot s)) args

  let apply_ ~where ?loc s args =
    let args = List.map (fun x -> Label.nolabel,x) args in
    Exp.(apply ?loc (ident ?loc (lid ?loc (where s))) args)
  let unsafe = apply_ ~where:js_unsafe_dot
  let fun_ = apply_ ~where:js_dot
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

let app_arg e = (Label.nolabel, e)

let inject_arg e = Js.unsafe "inject" [e]

let inject_args args =
  Exp.array (List.map (fun e -> Js.unsafe "inject" [e]) args)

let obj_arrows targs tres =
  let lbl, tobj, tobjres = List.hd targs and targs = List.tl targs in
  ((lbl, Js.type_ "t" [arrows tobj tobjres]) :: (targs_arrows targs)), tres

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

  let twrap_args,twrap_res = uplift targs tres in
  let twrap = arrows twrap_args twrap_res in
  let tfunc_args,tfunc_res = downlift targs tres in

  let ebody = Exp.constraint_ (body (List.map (fun s -> Exp.ident (lid s)) args)) tfunc_res in

  let pats = List.map (fun arg -> (Pat.var (Location.mknoloc arg))) args in

  let efun label pat (label',typ) expr =
    assert(label' = label);
    Exp.fun_ label None (Pat.constraint_ pat typ) expr
  in
  let rec fold3 f l a t acc =
    match l,a,t with
    | [],[],[] -> acc
    | [],_,_ | _,[],_ | _,_,[] -> assert false
    | l::lx, a::ax, t::tx -> f l a t (fold3 f lx ax tx acc)
  in
  let invoker =
    fold3 efun labels pats tfunc_args
      (efun Label.nolabel (Pat.any ()) (Label.nolabel,twrap) ebody)
  in
  let result = List.fold_right Exp.newtype (res :: ntargs) invoker in

  default_loc := default_loc';
  result

let open_t loc = Js.type_ ~loc "t" [Typ.object_ ~loc [] Open]

let method_call ~loc obj meth args =
  let gloc = {obj.pexp_loc with Location.loc_ghost = true} in
  let obj = Exp.constraint_ ~loc:gloc obj (open_t gloc) in
  let invoker =
    invoker
      (fun targs tres -> targs_arrows targs, Js.type_ "meth" [tres])
      obj_arrows
      (fun eargs ->
         let eobj = List.hd eargs in
         let eargs = inject_args (List.tl eargs) in
         Js.unsafe "meth_call" [eobj; str (unescape meth); eargs])
      ((Label.nolabel,[]) :: List.map (fun (l,_) -> l,[]) args)
  in
  Exp.apply invoker (
    app_arg obj :: args
    @ [app_arg
         (Exp.fun_ ~loc ~attrs:[merlin_noloc] Label.nolabel None
            (Pat.var ~loc ~attrs:[merlin_noloc] (Location.mknoloc "x"))
            (Exp.send ~loc ~attrs:[merlin_noloc]
               (Exp.ident ~loc:gloc (lid ~loc:gloc "x")) meth))]
  )

let prop_get ~loc:_ ~prop_loc obj prop =
  let gloc = {obj.pexp_loc with Location.loc_ghost = true} in
  let obj = Exp.constraint_ ~loc:gloc obj (open_t gloc) in
  let invoker =
    invoker
      (fun targs tres ->
         targs_arrows targs,Js.type_ "gen_prop" [[%type: <get: [%t tres]; ..> ]])
      obj_arrows
      (fun eargs -> Js.unsafe "get" [List.hd eargs; str (unescape prop)])
      [Label.nolabel,[]]
  in
  Exp.apply invoker (
    [ app_arg obj
    ; app_arg
        (Exp.fun_ ~loc:gloc Label.nolabel None
           (Pat.var ~loc:gloc ~attrs:[merlin_noloc] (Location.mknoloc "x"))
           (Exp.send ~loc:prop_loc ~attrs:[merlin_noloc]
              (Exp.ident ~loc:gloc (lid ~loc:gloc "x")) prop))
    ]
  )

let prop_set ~loc ~prop_loc obj prop value =
  let gloc = {obj.pexp_loc with Location.loc_ghost = true} in
  let obj = Exp.constraint_ ~loc:gloc obj (open_t gloc) in
  let invoker =
    invoker
      (fun targs _tres -> match targs with
         | [_,[],tobj; _,[],targ] ->
           [Label.nolabel,tobj],
           Js.type_ "gen_prop" [[%type: <set: [%t targ] -> unit; ..> ]]
         | _ -> assert false)
      (fun targs _tres -> obj_arrows targs [%type: unit])
      (function
        | [obj; arg] ->
          Js.unsafe "set" [obj; str (unescape prop); inject_arg arg]
        | _ -> assert false)
      [Label.nolabel, []; Label.nolabel, []]
  in
  Exp.apply invoker (
    [ app_arg obj
    ; app_arg value
    ; app_arg
        (Exp.fun_ ~loc Label.nolabel None
           (Pat.var ~loc:gloc ~attrs:[merlin_noloc] (Location.mknoloc "x"))
           (Exp.send ~loc:prop_loc ~attrs:[merlin_noloc]
              (Exp.ident ~loc:gloc (lid ~loc:gloc "x")) prop))
    ]
  )

(** Instantiation of a class, used by new%js. *)
let new_object constr args =
  let invoker =
    invoker
      (fun _targs _tres -> [],[%type: unit])
      (fun targs tres ->
         let _unit = List.hd targs and targs = List.tl targs in
         let tres = Js.type_ "t" [tres] in
         let args = targs_arrows targs in
         (Label.nolabel, Js.type_ "constr" [arrows args tres]) :: args, tres)
      (function
        | (constr :: args) ->
          Js.unsafe "new_obj" [constr; inject_args args]
        | _ -> assert false)
      ((Label.nolabel,[]) :: List.map (fun (l,_) -> l, []) args)
  in
  Exp.apply invoker (
    app_arg (Exp.ident ~loc:constr.loc constr) :: args
    @ [app_arg (Exp.construct ~loc:constr.loc (lid ~loc:constr.loc "()") None)]
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

let literal_object self_id ( fields : field_desc list) =

  let name = function
    | `Val  (id, _, _, _)    -> id
    | `Meth (id, _, _, _, _) -> id
  in

  let body = function
    | `Val  (_, _, _, body)    -> body
    | `Meth (_, _, _, body, _) -> [%expr fun [%p self_id] -> [%e body] ]
  in

  let invoker =
    invoker
      (fun targs tres ->
         let targs =
           List.map2 (fun f (l,args,ret_ty) ->
             match f with
             | `Val  (_, Mutable,   _, _)    ->
               l, Js.type_ "prop" [ret_ty]
             | `Val  (_, Immutable, _, _)    ->
               l, Js.type_ "readonly_prop" [ret_ty]
             | `Meth (_, _,         _, _, _) ->
               l, arrows (
                 (Label.nolabel,Js.type_ "t" [tres]) :: List.tl args)
                 (Js.type_ "meth" [ret_ty])
           ) fields targs
         in
         ((Label.nolabel, Js.type_ "t" [tres]) :: targs),
         tres)
      (fun targs tres ->
         let targs =
           List.map2 (fun f (l,args,ret) ->
             match f with
             | `Val _  -> l, args, ret
             | `Meth _ -> l, (Label.nolabel, Js.type_ "t" [tres])::List.tl args, ret
           ) fields targs
         in
         targs_arrows targs, Js.type_ "t" [tres]
      )
      (fun args ->
         Js.unsafe
           "obj"
           [Exp.array (
              List.map2
                (fun f arg ->
                   tuple [str (unescape (name f).txt);
                          inject_arg (match f with
                            | `Val  _ -> arg
                            | `Meth _ -> Js.fun_ "wrap_meth_callback" [ arg ]) ]
                ) fields args
            )]
      )
      (List.map (function
         | `Val _ -> Label.nolabel, []
         | `Meth (_, _, _, _, fun_ty) -> Label.nolabel, Label.nolabel :: fun_ty) fields)
  in

  let self = "self" in

  let gloc = {!default_loc with Location.loc_ghost = true} in

  let fake_object =
    Exp.object_
      { pcstr_self = Pat.any ~loc:gloc ();
        pcstr_fields =
          (List.map
             (fun f ->
                let loc = (name f).loc in
                let apply e = match f with
                  | `Val _ -> e
                  | `Meth _ -> Exp.apply e [Label.nolabel, Exp.ident (lid ~loc:Location.none self) ]
                in
                { pcf_loc = loc;
                  pcf_attributes = [];
                  pcf_desc =
                    Pcf_method
                      (name f,
                       Public,
                       Cfk_concrete (Fresh, apply (Exp.ident ~loc (lid ~loc:Location.none (name f).txt)))
                      )
                })
             fields)
      }
  in
  Exp.apply invoker (
    (List.map (fun f -> app_arg (body f)) fields)
    @ [
      app_arg (List.fold_right (fun name fun_ ->
        (Exp.fun_ ~loc:gloc Label.nolabel None
           (Pat.var ~loc:gloc (Location.mknoloc name))
           fun_))
        (self :: List.map (fun f -> (name f).txt) fields)
        fake_object
      )] )

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
          let new_expr = prop_get ~loc:meth.pexp_loc ~prop_loc:expr.pexp_loc  obj prop in
          mapper.expr mapper  { new_expr with pexp_attributes }

        (* obj##.var := value *)
        | [%expr [%e? [%expr [%e? obj] ##. [%e? meth]] as prop] := [%e? value]] ->
          let obj = mapper.expr mapper obj in
          let value = mapper.expr mapper value in
          let prop_loc = prop.pexp_loc in
          let prop = exp_to_string meth in
          let new_expr = prop_set ~loc:meth.pexp_loc ~prop_loc obj prop value in
          mapper.expr mapper  { new_expr with pexp_attributes }

        (* obj##(meth arg1 arg2) .. *)
        | [%expr [%e? obj] ## [%e? {pexp_desc = Pexp_apply(meth,args); _}]]
          ->
          let meth_str = exp_to_string meth in
          let obj = mapper.expr mapper  obj in
          let args = List.map (fun (s,e) -> s, mapper.expr mapper e) args in
          let new_expr = method_call ~loc:meth.pexp_loc obj meth_str args in
          mapper.expr mapper  { new_expr with pexp_attributes }

        (* obj##meth arg1 arg2 .. *)
        | {pexp_desc = Pexp_apply (([%expr [%e? obj] ## [%e? meth]]) as prop, args); _}
          ->
          let meth_str = exp_to_string meth in
          let obj = mapper.expr mapper  obj in
          let args = List.map (fun (s,e) -> s, mapper.expr mapper e) args in
          let new_expr = method_call ~loc:prop.pexp_loc obj meth_str args in
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
