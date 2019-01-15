(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
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

open StdLabels
open Migrate_parsetree
open OCaml_406.Ast

(* For implicit optional argument elimination. Annoying with Ast_helper. *)
[@@@ocaml.warning "-48"]

open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Ast_convenience_406

(** Check if an expression is an identifier and returns it.
    Raise a Location.error if it's not.
*)
let exp_to_string = function
  | {pexp_desc = Pexp_ident {txt = Longident.Lident s; _}; _} -> s
  | {pexp_desc = Pexp_construct ({txt = Longident.Lident s; _}, None); _}
    when String.length s > 0 && s.[0] >= 'A' && s.[0] <= 'Z' ->
      "_" ^ s
  | {pexp_loc; _} ->
      Location.raise_errorf
        ~loc:pexp_loc
        "Javascript methods or attributes can only be simple identifiers."

let lid ?(loc = !default_loc) str = Location.mkloc (Longident.parse str) loc

let typ s = Typ.constr (lid s) []

(** arg1 -> arg2 -> ... -> ret *)
let arrows args ret =
  List.fold_right args ~init:ret ~f:(fun (l, ty) fun_ -> Typ.arrow l ty fun_)

let wrapper = ref None

let make_str ?loc s =
  match loc with None -> Location.mknoloc s | Some loc -> Location.mkloc s loc

let inside_Js =
  lazy
    ( try Filename.basename (Filename.chop_extension !Location.input_name) = "js"
      with Invalid_argument _ -> false )

(* -- FIXME --
   [merlin_noloc] is an attempt to hide some ast node from merlin
   when using merlin-type-enclosing-go-{up,down}.
   It turns out not to be working properly, just hiding location instead of the all
   ast node.
*)
let merlin_noloc = {txt = "merlin.loc"; loc = Location.none}, PStr []

module Js : sig
  val type_ :
    ?loc:Ast_helper.loc -> string -> Parsetree.core_type list -> Parsetree.core_type

  val unsafe :
    ?loc:Ast_helper.loc -> string -> Parsetree.expression list -> Parsetree.expression

  val fun_ :
    ?loc:Ast_helper.loc -> string -> Parsetree.expression list -> Parsetree.expression
end = struct
  let js_dot name =
    if Lazy.force inside_Js
    then name
    else match !wrapper with None -> "Js." ^ name | Some m -> m ^ ".Js." ^ name

  let js_unsafe_dot name = js_dot ("Unsafe." ^ name)

  let type_ ?loc s args = Typ.constr ?loc (lid (js_dot s)) args

  let apply_ ~where ?loc s args =
    let args = List.map ~f:(fun x -> Label.nolabel, x) args in
    Exp.(apply ?loc (ident ?loc (lid ?loc (where s))) args)

  let unsafe = apply_ ~where:js_unsafe_dot

  let fun_ = apply_ ~where:js_dot
end

let unescape lab =
  if lab = ""
  then lab
  else
    let lab =
      if lab.[0] = '_' then String.sub lab ~pos:1 ~len:(String.length lab - 1) else lab
    in
    try
      let i = String.rindex lab '_' in
      if i = 0 then raise Not_found;
      String.sub lab ~pos:0 ~len:i
    with Not_found -> lab

let app_arg e = Label.nolabel, e

let inject_arg e = Js.unsafe "inject" [e]

let inject_args args = Exp.array (List.map ~f:(fun e -> Js.unsafe "inject" [e]) args)

module Arg : sig
  type t

  val make : ?label:Label.t -> unit -> t

  val name : t -> string

  val typ : t -> core_type

  val label : t -> Label.t

  val args : t list -> (Label.t * core_type) list
end = struct
  type arg =
    { label : Label.t
    ; name : string }

  type t = arg

  let count = ref 0

  let make ?(label = Label.nolabel) () =
    let c = !count in
    incr count;
    {label; name = "t" ^ string_of_int c}

  let label arg = arg.label

  let name arg = arg.name

  let typ arg = typ (name arg)

  let args l = List.map ~f:(fun x -> label x, typ x) l
end

let js_dot_t_the_first_arg args =
  match args with
  | [] -> assert false
  | x :: xs -> (Arg.label x, Js.type_ "t" [Arg.typ x]) :: Arg.args xs

(* uplift    : type of the unused value - ties all types together
   downlift  : types of individual components (arguments and result)
*)
let invoker ?(extra_types = []) uplift downlift body arguments =
  let default_loc' = !default_loc in
  default_loc := Location.none;
  let res = "res" in
  let typ_res = typ res in
  let twrap = uplift arguments typ_res in
  let tfunc_args, tfunc_res = downlift arguments typ_res in
  (* Build the main body *)
  let ebody =
    let ident d = Exp.ident (lid (Arg.name d)) in
    let args = List.map ~f:ident arguments in
    body args
  in
  let annotated_ebody = Exp.constraint_ ebody tfunc_res in
  (* Build the function.
     The last arguments is just used to tie all types together.
     It's unused in the implementation.
     {[ fun (t1 : type_of_t1) (t2 : type_of_t2) (_ : uplift_type) -> e]}
  *)
  let labels_and_pats =
    List.map arguments ~f:(fun d ->
        let label = Arg.label d in
        let patt = Pat.var (Location.mknoloc (Arg.name d)) in
        label, patt )
  in
  let make_fun (label, pat) (label', typ) expr =
    assert (label' = label);
    Exp.fun_ label None (Pat.constraint_ pat typ) expr
  in
  let invoker =
    List.fold_right2
      labels_and_pats
      tfunc_args
      ~f:make_fun
      ~init:(make_fun (Label.nolabel, Pat.any ()) (Label.nolabel, twrap) annotated_ebody)
  in
  (* Introduce all local types:
     {[ fun (type res t0 t1 ..) arg1 arg2 -> e ]}
  *)
  let local_types =
    make_str res :: List.map (extra_types @ arguments) ~f:(fun x -> make_str (Arg.name x))
  in
  let result = List.fold_right local_types ~init:invoker ~f:Exp.newtype in
  default_loc := default_loc';
  result

let open_t loc = Js.type_ ~loc "t" [Typ.object_ ~loc [] Open]

(* {[ obj##meth x y ]} generates
   {[
     (
       fun (type res a2 a0 a1) ->
       fun (a2 : a2 Js.t)  ->
       fun (a0 : a0)  ->
       fun (a1 : a1)  ->
       fun (_ : a2 -> a0 -> a1 -> res Js.meth)  ->
         (Js.Unsafe.meth_call a2 "meth"
            [|(Js.Unsafe.inject a0);
              (Js.Unsafe.inject a1)
            |] : res)
     )
       (obj : < .. > Js.t)
       x
       y
       (fun x  -> x#meth)
   ]} *)
let method_call ~loc obj meth args =
  let gloc = {obj.pexp_loc with Location.loc_ghost = true} in
  let obj = Exp.constraint_ ~loc:gloc obj (open_t gloc) in
  let invoker =
    invoker
      (fun args tres -> arrows (Arg.args args) (Js.type_ "meth" [tres]))
      (fun args tres -> js_dot_t_the_first_arg args, tres)
      (fun eargs ->
        match eargs with
        | [] -> assert false
        | eobj :: eargs ->
            let eargs = inject_args eargs in
            Js.unsafe "meth_call" [eobj; str (unescape meth); eargs] )
      (Arg.make () :: List.map args ~f:(fun (label, _) -> Arg.make ~label ()))
  in
  Exp.apply
    invoker
    ( (app_arg obj :: args)
    @ [ app_arg
          (Exp.fun_
             ~loc
             ~attrs:[merlin_noloc]
             Label.nolabel
             None
             (Pat.var ~loc ~attrs:[merlin_noloc] (Location.mknoloc "x"))
             (Exp.send
                ~loc
                ~attrs:[merlin_noloc]
                (Exp.ident ~loc:gloc (lid ~loc:gloc "x"))
                (make_str ~loc meth))) ] )

(* {[ obj##.prop ]} generates
   {[
     (
       fun (type res a0) ->
       fun (a0 : a0 Js.t)  ->
       fun (_ : a0 -> < get :res  ;.. > Js.gen_prop)  ->
         (Js.Unsafe.get a0 "prop" : res)
     )
       (obj : < .. > Js.t)
       (fun x -> x#prop)
   ]} *)
let prop_get ~loc:_ ~prop_loc obj prop =
  let gloc = {obj.pexp_loc with Location.loc_ghost = true} in
  let obj = Exp.constraint_ ~loc:gloc obj (open_t gloc) in
  let invoker =
    invoker
      (fun args tres ->
        arrows (Arg.args args) (Js.type_ "gen_prop" [[%type: < get : [%t tres] ; .. > ]])
        )
      (fun args tres -> js_dot_t_the_first_arg args, tres)
      (fun eargs ->
        match eargs with
        | [] | _ :: _ :: _ -> assert false
        | [only_arg] -> Js.unsafe "get" [only_arg; str (unescape prop)] )
      [Arg.make ()]
  in
  Exp.apply
    invoker
    [ app_arg obj
    ; app_arg
        (Exp.fun_
           ~loc:gloc
           Label.nolabel
           None
           (Pat.var ~loc:gloc ~attrs:[merlin_noloc] (Location.mknoloc "x"))
           (Exp.send
              ~loc:prop_loc
              ~attrs:[merlin_noloc]
              (Exp.ident ~loc:gloc (lid ~loc:gloc "x"))
              (make_str ~loc:prop_loc prop))) ]

(* {[ obj##.prop := expr ]} generates
   {[
     (
       fun (type res a1  a0) ->
       fun (a1 : a1 Js.t)  ->
       fun (a0 : a0)  ->
       fun (_ : a1 -> < set :a0 -> unit  ;.. > Js.gen_prop)  ->
         (Js.Unsafe.set a1 "prop" (Js.Unsafe.inject a0) : unit)
     )
       (obj : < .. > Js.t)
       expr
       (fun x -> x#prop)
   ]} *)
let prop_set ~loc ~prop_loc obj prop value =
  let gloc = {obj.pexp_loc with Location.loc_ghost = true} in
  let obj = Exp.constraint_ ~loc:gloc obj (open_t gloc) in
  let invoker =
    invoker
      (fun args _tres ->
        match args with
        | [obj; arg] ->
            assert (Arg.label obj = Label.nolabel);
            assert (Arg.label arg = Label.nolabel);
            arrows
              [Label.nolabel, Arg.typ obj]
              (Js.type_ "gen_prop" [[%type: < set : [%t Arg.typ arg] -> unit ; .. > ]])
        | _ -> assert false )
      (fun args _tres -> js_dot_t_the_first_arg args, [%type: unit])
      (function
        | [obj; arg] -> Js.unsafe "set" [obj; str (unescape prop); inject_arg arg]
        | _ -> assert false)
      [Arg.make (); Arg.make ()]
  in
  Exp.apply
    invoker
    [ app_arg obj
    ; app_arg value
    ; app_arg
        (Exp.fun_
           ~loc
           Label.nolabel
           None
           (Pat.var ~loc:gloc ~attrs:[merlin_noloc] (Location.mknoloc "x"))
           (Exp.send
              ~loc:prop_loc
              ~attrs:[merlin_noloc]
              (Exp.ident ~loc:gloc (lid ~loc:gloc "x"))
              (make_str ~loc:prop_loc prop))) ]

(* {[ new%js constr x y ]} generates
   {[
     (
       fun (type res a2 a0 a1) ->
       fun (a2 : (a0 -> a1 -> res Js.t) Js.constr)  ->
       fun (a0 : a0)  ->
       fun (a1 : a1)  ->
       fun (_ : unit)  ->
         (Js.Unsafe.new_obj a2
               [|(Js.Unsafe.inject a0);
                 (Js.Unsafe.inject a1)
               |] : res Js.t)
     )
       constr x y ()
   ]}
*)

(** Instantiation of a class, used by new%js. *)
let new_object constr args =
  let invoker =
    invoker
      (fun _args _tres -> [%type: unit])
      (fun args tres ->
        let tres = Js.type_ "t" [tres] in
        match args with
        | [] -> assert false
        | unit :: args ->
            assert (Arg.label unit = Label.nolabel);
            let args = Arg.args args in
            (Label.nolabel, Js.type_ "constr" [arrows args tres]) :: args, tres )
      (function
        | constr :: args -> Js.unsafe "new_obj" [constr; inject_args args]
        | _ -> assert false)
      (Arg.make () :: List.map args ~f:(fun (label, _) -> Arg.make ~label ()))
  in
  Exp.apply
    invoker
    ( (app_arg (Exp.ident ~loc:constr.loc constr) :: args)
    @ [app_arg (Exp.construct ~loc:constr.loc (lid ~loc:constr.loc "()") None)] )

module S = Map.Make (String)

(** We remove Pexp_poly as it should never be in the parsetree except after a method call.
*)
let format_meth body = match body.pexp_desc with Pexp_poly (e, _) -> e | _ -> body

(** Ensure basic sanity rules about fields of a literal object:
    - No duplicated declaration
    - Only relevant declarations (val and method, for now).
*)

module Prop_kind = struct
  type t =
    [ `Readonly
    | `Writeonly
    | `Readwrite
    | `Optdef ]

  let prop_type constr ty =
    let constr =
      match constr with
      | `Readonly -> "readonly_prop"
      | `Writeonly -> "writeonly_prop"
      | `Readwrite -> "prop"
      | `Optdef -> "optdef_prop"
    in
    Js.type_ constr [ty]

  let wrap_arg_type constr ty =
    match constr with
    | `Readonly | `Writeonly | `Readwrite -> ty
    | `Optdef -> Js.type_ "optdef" [ty]
end

type field_desc =
  | Meth of
      string Asttypes.loc
      * Asttypes.private_flag
      * Asttypes.override_flag
      * Parsetree.expression
      * Arg.t list
  | Val of
      string Asttypes.loc * Prop_kind.t * Asttypes.override_flag * Parsetree.expression

let filter_map f l =
  let l =
    List.fold_left l ~init:[] ~f:(fun acc x ->
        match f x with Some x -> x :: acc | None -> acc )
  in
  List.rev l

let preprocess_literal_object mappper fields : [`Fields of field_desc list | `Error of _]
    =
  let check_name id names =
    let txt = unescape id.txt in
    if S.mem txt names
    then
      let id' = S.find txt names in
      (* We point out both definitions in locations (more convenient for the user). *)
      let details id =
        if id.txt <> txt then Printf.sprintf " (normalized to %S)" txt else ""
      in
      let sub =
        [ Location.errorf
            ~loc:id'.loc
            "Duplicated val or method %S%s."
            id'.txt
            (details id') ]
      in
      Location.raise_errorf
        ~loc:id.loc
        ~sub
        "Duplicated val or method %S%s."
        id.txt
        (details id)
    else S.add txt id names
  in
  let drop_prefix ~prefix s =
    let prefix_len = String.length prefix in
    if String.length s > prefix_len && String.sub s ~pos:0 ~len:prefix_len = prefix
    then true, String.sub s ~pos:prefix_len ~len:(String.length s - prefix_len)
    else false, s
  in
  let parse_attribute x =
    match drop_prefix ~prefix:"jsoo." x with
    | _, "optdef" -> Some `Optdef
    | _, "writeonly" -> Some `Writeonly
    | _, "readonly" -> Some `Readonly
    | _, "readwrite" -> Some `Readwrite
    | false, _ -> None
    | true, _ -> Some (`Unkown x)
  in
  let jsoo_attributes = filter_map (fun ({txt; _}, _) -> parse_attribute txt) in
  let f (names, fields) exp =
    match exp.pcf_desc with
    | Pcf_val (id, mut, Cfk_concrete (bang, body)) ->
        let names = check_name id names in
        let body = mappper body in
        let kind =
          match mut, jsoo_attributes exp.pcf_attributes with
          | Immutable, [] -> `Readonly
          | Mutable, [] -> `Readwrite
          | Immutable, [`Readonly] -> `Readonly
          | (Immutable | Mutable), [`Optdef] -> `Optdef
          | (Immutable | Mutable), [`Writeonly] -> `Writeonly
          | (Immutable | Mutable), [`Readwrite] -> `Readwrite
          | (Immutable | Mutable), [`Unkown s] ->
              Location.raise_errorf
                ~loc:exp.pcf_loc
                "Unkown jsoo attribute ([@@@@%s])."
                s
          | Mutable, [`Readonly] ->
              Location.raise_errorf
                ~loc:exp.pcf_loc
                "A mutable field cannot be readonly."
          | _, _ :: _ :: _ ->
              Location.raise_errorf ~loc:exp.pcf_loc "Too many attributes."
        in
        names, Val (id, kind, bang, body) :: fields
    | Pcf_method (id, priv, Cfk_concrete (bang, body)) ->
        let names = check_name id names in
        let body = format_meth (mappper body) in
        let rec create_meth_ty exp =
          match exp.pexp_desc with
          | Pexp_fun (label, _, _, body) -> Arg.make ~label () :: create_meth_ty body
          | _ -> []
        in
        let fun_ty = create_meth_ty body in
        names, Meth (id, priv, bang, body, fun_ty) :: fields
    | _ ->
        Location.raise_errorf
          ~loc:exp.pcf_loc
          "This field is not valid inside a js literal object."
  in
  try `Fields (List.rev (snd (List.fold_left fields ~init:(S.empty, []) ~f)))
  with Location.Error error -> `Error (extension_of_error error)

(* {[ object%js (self)
     val readonlyprop = e1
     val prop = e2
     method meth x = e3
   end ]} generates
   {[
     (
       fun (type res a6 a7 a8 a9) ->
       fun (a7 : a7)  ->
       fun (a8 : a8)  ->
       fun (a9 : res Js.t -> a6 -> a9)  ->
       fun
         (_ :
            res Js.t ->
          a7 Js.readonly_prop ->
          a8 Js.prop ->
          (res Js.t -> a6 -> a9 Js.meth) ->
          res)
         ->
           (Js.Unsafe.obj
              [|("readonlyprop", (Js.Unsafe.inject a7));
                ("prop", (Js.Unsafe.inject a8));
                ("meth", (Js.Unsafe.inject (Js.wrap_meth_callback a9)))
              |] : res Js.t)
     )
       e1
       e2
       (fun self -> fun x  -> e3)
     (fun self read_only_prop prop meth  ->
          object
            method read_only_prop = read_only_prop
            method prop = prop
            method meth = meth self
          end)
   ]} *)
let literal_object self_id (fields : field_desc list) =
  let name = function Val (id, _, _, _) -> id | Meth (id, _, _, _, _) -> id in
  let body = function
    | Val (_, _, _, body) -> body
    | Meth (_, _, _, body, _) -> [%expr fun [%p self_id] -> [%e body]]
  in
  let extra_types =
    List.concat (List.map fields ~f:(function Val _ -> [] | Meth (_, _, _, _, l) -> l))
  in
  let invoker =
    invoker
      ~extra_types
      (fun args tres ->
        let args =
          List.map2 fields args ~f:(fun f desc ->
              let ret_ty = Arg.typ desc in
              let label = Arg.label desc in
              match f with
              | Val (_, constr, _, _) -> label, Prop_kind.prop_type constr ret_ty
              | Meth (_, _, _, _, args) ->
                  ( label
                  , arrows
                      ((Label.nolabel, Js.type_ "t" [tres]) :: Arg.args args)
                      (Js.type_ "meth" [ret_ty]) ) )
        in
        arrows ((Label.nolabel, Js.type_ "t" [tres]) :: args) tres )
      (fun args tres ->
        let args =
          List.map2 fields args ~f:(fun f desc ->
              let ret_ty = Arg.typ desc in
              let label = Arg.label desc in
              match f with
              | Val (_, constr, _, _) -> label, Prop_kind.wrap_arg_type constr ret_ty
              | Meth (_, _, _, _, args) ->
                  ( label
                  , arrows ((Label.nolabel, Js.type_ "t" [tres]) :: Arg.args args) ret_ty
                  ) )
        in
        args, Js.type_ "t" [tres] )
      (fun args ->
        Js.unsafe
          "obj"
          [ Exp.array
              (List.map2 fields args ~f:(fun f arg ->
                   tuple
                     [ str (unescape (name f).txt)
                     ; inject_arg
                         ( match f with
                         | Val _ -> arg
                         | Meth _ -> Js.fun_ "wrap_meth_callback" [arg] ) ] )) ] )
      (List.map fields ~f:(function
          | Val _ -> Arg.make ()
          | Meth (_, _, _, _, _fun_ty) -> Arg.make () ))
  in
  let self = "self" in
  let gloc = {!default_loc with Location.loc_ghost = true} in
  let fake_object =
    Exp.object_
      { pcstr_self = Pat.any ~loc:gloc ()
      ; pcstr_fields =
          List.map fields ~f:(fun f ->
              let loc = (name f).loc in
              let apply e =
                match f with
                | Val _ -> e
                | Meth _ ->
                    Exp.apply e [Label.nolabel, Exp.ident (lid ~loc:Location.none self)]
              in
              { pcf_loc = loc
              ; pcf_attributes = []
              ; pcf_desc =
                  Pcf_method
                    ( name f
                    , Public
                    , Cfk_concrete
                        ( Fresh
                        , apply (Exp.ident ~loc (lid ~loc:Location.none (name f).txt)) )
                    ) } ) }
  in
  Exp.apply
    invoker
    ( List.map fields ~f:(fun f -> app_arg (body f))
    @ [ app_arg
          (List.fold_right
             (self :: List.map fields ~f:(fun f -> (name f).txt))
             ~init:fake_object
             ~f:(fun name fun_ ->
               Exp.fun_
                 ~loc:gloc
                 Label.nolabel
                 None
                 (Pat.var ~loc:gloc (Location.mknoloc name))
                 fun_ )) ] )

let mapper =
  { default_mapper with
    expr =
      (fun mapper expr ->
        let prev_default_loc = !default_loc in
        default_loc := expr.pexp_loc;
        let {pexp_attributes; _} = expr in
        let new_expr =
          match expr with
          (* obj##.var *)
          | [%expr [%e? obj] ##. [%e? meth]] ->
              let obj = mapper.expr mapper obj in
              let prop = exp_to_string meth in
              let new_expr =
                prop_get ~loc:meth.pexp_loc ~prop_loc:expr.pexp_loc obj prop
              in
              mapper.expr mapper {new_expr with pexp_attributes}
          (* obj##.var := value *)
          | [%expr [%e? [%expr [%e? obj] ##. [%e? meth]] as prop] := [%e? value]] ->
              let obj = mapper.expr mapper obj in
              let value = mapper.expr mapper value in
              let prop_loc = prop.pexp_loc in
              let prop = exp_to_string meth in
              let new_expr = prop_set ~loc:meth.pexp_loc ~prop_loc obj prop value in
              mapper.expr mapper {new_expr with pexp_attributes}
          (* obj##(meth arg1 arg2) .. *)
          | [%expr [%e? obj] ## [%e? {pexp_desc = Pexp_apply (meth, args); _}]] ->
              let meth_str = exp_to_string meth in
              let obj = mapper.expr mapper obj in
              let args = List.map args ~f:(fun (s, e) -> s, mapper.expr mapper e) in
              let new_expr = method_call ~loc:meth.pexp_loc obj meth_str args in
              mapper.expr mapper {new_expr with pexp_attributes}
          (* obj##meth arg1 arg2 .. *)
          | {pexp_desc = Pexp_apply (([%expr [%e? obj] ## [%e? meth]] as prop), args); _}
            ->
              let meth_str = exp_to_string meth in
              let obj = mapper.expr mapper obj in
              let args = List.map args ~f:(fun (s, e) -> s, mapper.expr mapper e) in
              let new_expr = method_call ~loc:prop.pexp_loc obj meth_str args in
              mapper.expr mapper {new_expr with pexp_attributes}
          (* obj##meth *)
          | [%expr [%e? obj] ## [%e? meth]] as expr ->
              let obj = mapper.expr mapper obj in
              let meth = exp_to_string meth in
              let new_expr = method_call ~loc:expr.pexp_loc obj meth [] in
              mapper.expr mapper {new_expr with pexp_attributes}
          (* new%js constr] *)
          | [%expr [%js [%e? {pexp_desc = Pexp_new constr; _}]]] ->
              let new_expr = new_object constr [] in
              mapper.expr mapper {new_expr with pexp_attributes}
          (* new%js constr arg1 arg2 ..)] *)
          | { pexp_desc =
                Pexp_apply ([%expr [%js [%e? {pexp_desc = Pexp_new constr; _}]]], args); _
            } ->
              let args = List.map args ~f:(fun (s, e) -> s, mapper.expr mapper e) in
              let new_expr = new_object constr args in
              mapper.expr mapper {new_expr with pexp_attributes}
          (* object%js ... end *)
          | [%expr [%js [%e? {pexp_desc = Pexp_object class_struct; _}]]] ->
              let fields =
                preprocess_literal_object (mapper.expr mapper) class_struct.pcstr_fields
              in
              let new_expr =
                match fields with
                | `Fields fields -> literal_object class_struct.pcstr_self fields
                | `Error e -> Exp.extension e
              in
              mapper.expr mapper {new_expr with pexp_attributes}
          | _ -> default_mapper.expr mapper expr
        in
        default_loc := prev_default_loc;
        new_expr ) }

let () =
  Driver.register
    ~name:"ppx_js"
    Migrate_parsetree.Versions.ocaml_406
    (fun _config _cookies -> mapper )
