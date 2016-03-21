(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
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

let rnd = Random.State.make [|0x313511d4|]
let random_var () =
  Format.sprintf "jsoo_%08Lx" (Random.State.int64 rnd 0x100000000L)
let random_tvar () =
  Format.sprintf "jsoo_%08Lx" (Random.State.int64 rnd 0x100000000L)

module StringMap = Map.Make(String)

open Camlp4

module Id : Sig.Id = struct
  let name = "Javascript"
  let version = "1.0"
end


module Make (Syntax : Sig.Camlp4Syntax) = struct
  open Sig
  include Syntax

  let inside_Js = lazy
    (try
       String.lowercase (
         Filename.basename (
           Filename.chop_extension (!Camlp4_config.current_input_file))) = "js"
     with Invalid_argument _ -> false)

  let js_t_id _loc s = if Lazy.force inside_Js then <:ctyp< $lid:s$ >> else <:ctyp< Js.$lid:s$ >>
  let js_u_id _loc s = if Lazy.force inside_Js then <:expr< Unsafe.$lid:s$ >> else <:expr< Js.Unsafe.$lid:s$ >>
  let js_id _loc s = if Lazy.force inside_Js then <:expr< $lid:s$ >> else <:expr< Js.$lid:s$ >>


  let rec filter stream =
    match stream with parser
      [< '(KEYWORD "#", loc); rest >] ->
        begin match rest with parser
          [< '(KEYWORD "#", loc') >] ->
             [< '(KEYWORD "##", Loc.merge loc loc'); filter rest >]
        | [< >] ->
             [< '(KEYWORD "#", loc); filter rest >]
        end
    | [< 'other; rest >] -> [< 'other; filter rest >]

  let _ =
    Token.Filter.define_filter (Gram.get_filter ())
      (fun old_filter stream -> old_filter (filter stream))

  let rec parse_comma_list e =
    match e with
      <:expr< $e1$, $e2$ >> -> e1 :: parse_comma_list e2
    | _                    -> [e]

  let rec to_sem_expr _loc l =
    match l with
      []        -> assert false
    | [e]       -> e
    | e1 :: rem -> <:expr< $e1$; $to_sem_expr _loc rem$ >>

  let make_array _loc l =
    match l with
      [] -> <:expr< [| |] >>
    | _  -> <:expr< [| $to_sem_expr _loc l$ |] >>

  let with_type e t =
    let _loc = Ast.loc_of_expr e in <:expr< ($e$ : $t$) >>

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

  let fresh_type _loc = <:ctyp< '$random_tvar ()$ >>

  let arrows _loc args ret =
    List.fold_right
      (fun arg_typ rem_typ -> <:ctyp< $arg_typ$ -> $rem_typ$ >>)
      args ret

  let funs _loc args ret =
    List.fold_right (fun x next_fun -> <:expr< fun $lid:x$ -> $next_fun$ >> ) args ret


  let rec apply _loc init = function
    | [] -> init
    | x::xs -> apply _loc <:expr< $init$ $x$ >> xs

  let constrain_types
          _loc e_loc (e:string) v_loc (v:string) v_typ m_loc m m_typ args =
    let typ_var = fresh_type e_loc in
    let cstr =
      let _loc = e_loc in <:expr<(($lid:e$ : $js_t_id _loc "t"$ < .. >) : $js_t_id _loc "t"$ $typ_var$)>> in
    let x = let _loc = e_loc in <:expr<x>> in
    let body =
      let _loc = Syntax.Loc.merge e_loc m_loc in
      <:expr<($x$#$lid:m$ : $m_typ$)>> in
    let y = let _loc = v_loc in <:expr<($lid:v$ : $v_typ$)>> in
    let res =
      List.fold_right
        (fun (e, x, t) e' ->
           let _loc = Ast.loc_of_expr e in
           <:expr< let _ = ($lid:x$ : $t$) in $e'$ >>)
        args
        <:expr<
          $y$
        >>
    in
    <:expr<
      let module M = struct
        value res =
          let _ = $cstr$ in
          let _ = fun (x : $typ_var$) -> $body$ in
          $res$;
      end in M.res >>

  let method_call _loc obj lab lab_loc args =
    let args = List.map (fun e ->
      let my_var = random_var () in
      let my_typ = fresh_type _loc in
      (e, my_var, my_typ)) args in
    let ret_typ = fresh_type _loc in
    let method_type =
      arrows _loc (List.map (fun (_,_,ty) -> ty) args)
             <:ctyp< $js_t_id _loc "meth"$ $ret_typ$ >>
    in
    let o = "jsoo_self" in
    let res = "jsoo_res" in
    let meth_args =
      List.map (fun (_, x, _) -> <:expr< $js_u_id _loc "inject"$ $lid:x$ >>)
        args
    in
    let meth_args = make_array _loc meth_args in
    let o_loc = Ast.loc_of_expr obj in
    let binding = List.map (fun (e, x, _) -> <:binding< $lid:x$ = $e$ >>) args in
    let body =
      <:expr<
        let $lid:o$ = $obj$ in
        let $lid:res$ =
          $js_u_id _loc "meth_call"$ $lid:o$ $str:unescape lab$ $meth_args$ in
        $constrain_types _loc o_loc o _loc res ret_typ
           lab_loc lab method_type args$
      >>
    in
    match args with
    | [] -> body
    | _ -> <:expr< let $list:binding$ in $body$ >>

  let new_object _loc constructor args =
    let args = List.map (fun e -> (e, fresh_type _loc)) args in
    let obj_type = <:ctyp< $js_t_id _loc "t"$ $fresh_type _loc$ >> in
    let constr_fun_type = arrows _loc (List.map snd args) obj_type in
    let args =
      List.map
        (fun (e, t) -> <:expr< $js_u_id _loc "inject"$ $with_type e t$ >>) args
    in
    let args = make_array _loc args in

    let x = random_var () in
    let constr =
      with_type constructor
        <:ctyp< $js_t_id _loc "constr"$ $constr_fun_type$ >> in
    with_type
      <:expr< let $lid:x$ = $constr$ in
              $js_u_id _loc "new_obj"$ $lid:x$ $args$ >>
      <:ctyp< $obj_type$ >>


  let rec parse_field_list l =
    match l with
      <:rec_binding< $f1$; $f2$ >> -> f1 :: parse_field_list f2
    | _                            -> [l]

  let rec parse_class_str_list l =
    match l with
    | <:class_str_item< $f1$; $f2$ >> -> f1 :: parse_class_str_list f2
    | _                              -> [l]

  type 'a loc = 'a * Loc.t

  type val_ = {
    val_label    : string loc ;
    val_mutabl : bool ;
    val_body     : Ast.expr loc ;
    val_typ    : Ast.ctyp ;
  }

  type meth_ = {
    meth_label    : string loc ;
    meth_body     : Ast.expr loc ;
    meth_fun_typ  : Ast.ctyp list ;
    meth_ret_typ    : Ast.ctyp ;
  }

  type val_and_meth = [`Val of val_ | `Meth of meth_]

  let parse_field f : val_and_meth =
    match f with
      <:rec_binding< $label$ = $e$ >> ->
        let lab_loc,lab = match label with
          | Ast.IdLid(loc,lab) -> loc,lab
          | _ -> assert false in
        let e_loc = Ast.loc_of_expr e in
        let t = fresh_type lab_loc in
        `Val { val_label    = lab, lab_loc;
               val_mutabl   = false;
               val_body     = e,e_loc;
               val_typ      = t }
    | _ ->
        assert false

  let parse_class_item c : val_and_meth =
    let _loc = Ast.loc_of_class_str_item c in
    match c with
    | <:class_str_item< value $lab$ = $e$ >> ->
       let e_loc = Ast.loc_of_expr e in
       let t = fresh_type _loc in
        `Val { val_label    = lab, _loc ;
               val_mutabl   = false;
               val_body     = e,e_loc;
               val_typ      = t }
    | <:class_str_item< value mutable $label$ = $e$ >> ->
       let e_loc = Ast.loc_of_expr e in
       let t = fresh_type _loc in
        `Val { val_label    = label, _loc;
               val_mutabl   = true;
               val_body     = e,e_loc;
               val_typ      = t }
    | <:class_str_item< method $label$ = $e$ >> ->
       let e_loc = Ast.loc_of_expr e in

       let rec get_arg x =
         match x with
         | <:expr< fun $_x$ -> $e$ >> -> (fresh_type e_loc )::get_arg e
         | _ -> [] in
       let _loc = e_loc in
       let t = fresh_type _loc in
        `Meth { meth_label     = label, _loc;
                meth_body      = e,e_loc;
                meth_fun_typ   = get_arg e;
                meth_ret_typ   = t }
    | c ->
      let loc = Ast.loc_of_class_str_item c in
      Format.eprintf "This field is not valid inside a js literal object (%s)@."
        (Loc.to_string loc);
      failwith "Error while preprocessing with with Js_of_ocaml extention syntax"

  let literal_object _loc ?self (fields : val_and_meth list) =

     let self_typ = fresh_type _loc in

     let _ = List.fold_left (
                 fun acc
                     ( `Val  {val_label=(lab,loc);_}
                     | `Meth {meth_label=(lab,loc);_} ) ->
           let txt = unescape lab in
           if StringMap.mem txt acc
           then
             let details name =
               if name <> txt
               then Printf.sprintf " (normalized to %S)" txt
               else ""
             in

             let (loc',name') = StringMap.find txt acc in
             Format.eprintf "Duplicated label %S%s at %s@.%S%s previously seen at %s@."
               lab (details lab) (Loc.to_string loc) name' (details name') (Loc.to_string loc');
             failwith "Error while preprocessing with with Js_of_ocaml extention syntax"
           else StringMap.add txt (loc,lab) acc) StringMap.empty fields in

    let create_method_type = function
      | `Val {val_label=(label,_loc); val_mutabl=true; val_typ; _} ->
	 <:ctyp< $lid:label$ : ( $js_t_id _loc "prop"$ $val_typ$) >>
      | `Val {val_label=(label,_loc); val_mutabl=false; val_typ; _} ->
	 <:ctyp< $lid:label$ : ($js_t_id _loc "readonly_prop"$ $val_typ$) >>
      | `Meth {meth_label=(label,_loc); meth_fun_typ; meth_ret_typ; _} ->
	 let all = arrows _loc meth_fun_typ <:ctyp< $js_t_id _loc "meth"$ $meth_ret_typ$ >> in
	 <:ctyp< $lid:label$ : $all$ >>
    in

    let obj_type = <:ctyp< < $list:List.map create_method_type fields$  > >> in

    let rec annotate_body fun_ty ret_ty body = match fun_ty, body with
      | ty :: types,
        (<:expr< fun $pat$ -> $body$ >>) ->
        <:expr< fun ($pat$ : $ty$) -> $annotate_body types ret_ty body$ >>
      | [], body -> <:expr< ($body$ : $ret_ty$) >>
      | _ -> raise (Invalid_argument "Inconsistent number of arguments")
    in

    let create_value = function
      | `Val {val_label=(lab,_loc); val_body=(e,_);val_typ; _} ->
         lab,
         <:expr< $with_type e val_typ$ >>
      | `Meth {meth_label=(lab,_loc); meth_body=(e,_);meth_fun_typ; meth_ret_typ; _} ->
         let e,wrapper = match self with
           | None -> e,"wrap_callback"
           | Some self_pat ->
             annotate_body
               (self_typ :: meth_fun_typ)
               meth_ret_typ
               <:expr< fun $self_pat$ -> $e$ >>,
             "wrap_meth_callback" in
	 lab,
         <:expr< $js_id _loc wrapper$ $e$ >>
    in
    let args = List.map create_value fields in
    let make_obj =
      funs _loc (List.map (fun (name, _expr) -> name) args)
        (<:expr<
          ( $js_u_id _loc "obj"$
            $make_array _loc (List.map (fun (name,_) ->
               <:expr< ($str:unescape name$ , $js_u_id _loc "inject"$ $lid:name$) >>) args)$
            : $js_t_id _loc "t"$ $obj_type$ as $self_typ$ )
        >>)in
    let bindings =
      List.map
        (fun (lab, expr) -> <:binding< $lid:lab$ = $expr$ >>)
        (("make_obj",make_obj)::args)
    in
    <:expr<
      let $list:bindings$ in
      $apply _loc <:expr< make_obj >> (List.map (fun (lab,_) -> <:expr< $lid:lab$ >>) args) $
    >>


  let jsmeth = Gram.Entry.mk "jsmeth"
  let opt_class_self_patt_jsoo = Gram.Entry.mk "opt_class_self_patt_jsoo"
  EXTEND Gram
    jsmeth: [["##"; lab = label -> (_loc, lab) ]];
    opt_class_self_patt_jsoo:
      [[ "("; p = patt; ")" -> p
       | "("; p = patt; ":"; t = ctyp; ")" -> <:patt< ($p$ : $t$) >>
       | -> <:patt<_>> ]];

    expr: BEFORE "."
    ["##" RIGHTA
     [ e = SELF; (lab_loc, lab) = jsmeth ->
       let o = "jsoo_obj" in
       let o_loc = Ast.loc_of_expr e in
       let res = "jsoo_res" in
       <:expr<
         let $lid:o$ = $e$ in
         let $lid:res$ = $js_u_id _loc "get"$ $lid:o$ $str:unescape lab$ in
         $constrain_types _loc o_loc o _loc res <:ctyp< 'jsoo_res >>
            lab_loc lab <:ctyp< $js_t_id _loc "gen_prop"$ <get : 'jsoo_res; ..> >> []$
       >>
     | e1 = SELF; (lab_loc, lab) = jsmeth; "<-"; e2 = expr LEVEL "top" ->
       let o = "jsoo_obj" in
       let o_loc = Ast.loc_of_expr e1 in
       let v = "jsoo_arg" in
       <:expr<
         let $lid:o$ = $e1$
         and $lid:v$ = $e2$ in
         let _ = $constrain_types _loc o_loc o (Ast.loc_of_expr e2) v
                    <:ctyp< 'jsoo_arg >> lab_loc lab
                    <:ctyp< $js_t_id _loc "gen_prop"$ <set : 'jsoo_arg -> unit; ..> >>
                    []$ in
         $js_u_id _loc "set"$ $lid:o$ $str:unescape lab$ ($lid:v$)
         >>
     | e = SELF; (lab_loc, lab) = jsmeth; "("; ")" ->
         method_call _loc e lab lab_loc []
     | e = SELF; (lab_loc, lab) = jsmeth; "("; l = comma_expr; ")" ->
         method_call _loc e lab lab_loc (parse_comma_list l)
     ]];
    expr: LEVEL "simple"
    [[ "jsnew"; e = expr LEVEL "label"; "("; ")" ->
         new_object _loc e []
     | "jsnew"; e = expr LEVEL "label"; "("; l = comma_expr; ")" ->
         new_object _loc e (parse_comma_list l)
     | "jsobject"; "end" -> <:expr< ($js_u_id _loc "obj"$ [| |] : $js_t_id _loc "t"$ < > ) >>
     | "jsobject"; self = opt_class_self_patt_jsoo; l = class_structure ; "end" ->
       let field_list = parse_class_str_list l in
       let fields = List.map parse_class_item field_list in
       literal_object _loc ~self fields
     (* | "{:"; ":}" -> <:expr< ($js_u_id _loc "obj"$ [| |] : Js.t < > ) >> *)
     (* | "{:"; l = field_expr_list; ":}" -> *)
     (*   let field_list = parse_field_list l in *)
     (*   let fields = List.map parse_field field_list in *)
     (*   literal_object _loc fields *)
    ]];
    END

(*XXX n-ary methods

how to express optional fields?  if they are there, they must have
some type, but  they do not have to be there

use variant types instead of object types?
   in a negative position...  (but then we have to negate again...)

    { foo: "bar", baz : 7 } : [`foo of string field | `baz of int field] obj

    let f (x : t) = (x : [< `foo of string field | `baz of int field| `x of string field] obj)


XXXX
module WEIRDMODULENAME = struct type 'a o = 'a Js.t val unsafe_get = Js.Unsafe.get ... end
(let module M = WEIRDMODULENAME in (M.unsafe_get : <x : 'a M.meth> -> 'a))

XXXX be more careful with error messages:
  put coercions against arguments or whole expression
*)

end

module M = Register.OCamlSyntaxExtension(Id)(Make)

(* open Camlp4.PreCast *)
(* let expand _loc _ str = *)
(*   let lex = Compiler.Parse_js.lexer_from_string ~rm_comment:true str in *)
(*   let p = Compiler.Parse_js.parse lex in *)
(*   <:expr< 5 >> *)

(* let _ = Syntax.Quotation.add "js" Syntax.Quotation.DynAst.expr_tag expand *)
