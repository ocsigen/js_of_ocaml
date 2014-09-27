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


let js_t_id ?loc s args =
  if Lazy.force inside_Js
  then Typ.constr ?loc (lid s) args
  else Typ.constr ?loc (lid @@ "Js."^s) args

let js_u_id ?loc s args =
  let args = List.map (fun x -> "",x) args in
  if Lazy.force inside_Js
  then Exp.(apply (ident ?loc @@ lid ("Unsafe."^s)) args)
  else Exp.(apply (ident ?loc @@ lid ("Js.Unsafe."^s)) args)


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
      (js_t_id "t" [Typ.var "B"])
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
      (fun (e, x, t) e' ->
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

let arrows args ret =
  List.fold_right
    (fun arg_ty rem_ty -> Typ.arrow "" arg_ty rem_ty)
    args
    ret

let method_call obj meth args =
  let args = List.map (fun e -> (e, random_var (), fresh_type obj.pexp_loc)) args in
  let ret_type = js_t_id "meth" [fresh_type obj.pexp_loc] in
  let method_type = arrows (List.map (fun (_,_,x) -> x) args) ret_type in
  let o = random_var () in
  let obj' = Exp.ident ~loc:obj.pexp_loc @@ lid o in
  let res = random_var () in
  let meth' = unescape meth in
  let meth_args =
    Exp.array @@
    List.map
      (fun (_, x, _) -> js_u_id "inject" [evar x])
      args
  in
  List.fold_left
    (fun e' (e, x, _) -> [%expr let [%p pvar x] = [%e e] in [%e e']])
    [%expr
      let [%p pvar o] = [%e obj] in
      let [%p pvar res] =
        [%e js_u_id "meth_call" [ evar o ; str meth' ; meth_args] ]
      in
      [%e constrain_types obj' (lid res) ret_type meth' method_type args]
    ]
    args

let new_object constr args =
  let args = List.map (fun e -> (e, fresh_type constr.loc)) args in
  let obj_type = js_t_id "t" [fresh_type constr.loc] in
  let constr_fun_type = arrows (List.map snd args) obj_type in
  let args =
    Exp.array @@
    List.map
      (fun (e, t) -> js_u_id "inject" [Exp.constraint_ e t])
      args
  in
  let x = random_var () in
  let constr =
    Exp.constraint_
      (Exp.ident constr)
      (js_t_id "constr" [constr_fun_type])
  in
  [%expr
    ( let [%p pvar x] = [%e constr] in
      [%e js_u_id "new_obj" [evar x ; args]]
    : [%t obj_type] )
  ]

let js_mapper _args =
  { default_mapper with
    expr = (fun mapper expr ->
      default_loc := expr.pexp_loc;
      match expr with

      (** [%js obj#method] *)
      | [%expr [%js [%e? {pexp_desc = Pexp_send (obj, meth) }]]] ->
        let o = random_var () in
        let obj' = Exp.ident ~loc:obj.pexp_loc @@ lid o in
        let res = random_var () in
        let meth' = unescape meth in
        [%expr
          let [%p pvar o] = [%e obj] in
          let [%p pvar res] = [%e js_u_id "get" [obj' ; str meth']] in
          [%e
            constrain_types
              obj'
              (lid res) [%type: 'A]
              meth'
              (js_t_id "gen_prop" [[%type: <get : 'A; ..> ]]) []
          ]
        ]

      (** [%js obj#meth := value] *)
      | [%expr [%js [%e? {pexp_desc = Pexp_send (obj, meth) }] := [%e? value]]] ->
        let o = random_var () in
        let obj' = Exp.ident ~loc:obj.pexp_loc @@ lid o in
        let v = random_var () in
        let v_lid = lid v in
        let value' = Exp.ident ~loc:value.pexp_loc v_lid in
        let meth' = unescape meth in
        [%expr
          let [%p pvar v] = [%e value] in
          let [%p pvar o] = [%e obj] in
          let _ = [%e
            constrain_types
              obj'
              v_lid [%type: 'A]
              meth'
              (js_t_id "gen_prop" [[%type: <set : 'A -> unit ; ..> ]]) []
          ]
          in [%e js_u_id "set" [ obj' ; str meth' ; value']]
        ]

      (** [%js obj#meth ()] *)
      | [%expr [%js [%e? {pexp_desc = Pexp_send (obj, meth) }] () ]] ->
         method_call obj meth []
      (** [%js obj#meth (args, ..)] *)
      | [%expr [%js [%e? {pexp_desc = Pexp_send (obj, meth) }]
                    [%e? {pexp_desc = Pexp_tuple args}]
               ]] ->
         method_call obj meth args
      (** [%js obj#meth arg] *)
      | [%expr [%js [%e? {pexp_desc = Pexp_send (obj, meth) }] [%e? arg] ]] ->
         method_call obj meth [arg]


      (** new%js constr ()] *)
      | [%expr [%js [%e? {pexp_desc = Pexp_new constr}]] ()] ->
        new_object constr []
      (** new%js constr (args, ..)] *)
      | [%expr [%js [%e? {pexp_desc = Pexp_new constr}]]
                    [%e? {pexp_desc = Pexp_tuple args}]
        ] ->
        new_object constr args
      (** new%js constr arg] *)
      | [%expr [%js [%e? {pexp_desc = Pexp_new constr}]] [%e? arg] ] ->
        new_object constr [arg]

      | _ -> default_mapper.expr mapper expr
    );
    structure_item = (fun mapper stri ->
      default_loc := stri.pstr_loc;
      match stri with
      | _ -> default_mapper.structure_item mapper stri);
  }

let () = run_main js_mapper
