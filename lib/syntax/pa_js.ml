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
  Format.sprintf "a%08Lx" (Random.State.int64 rnd 0x100000000L)

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

  let fresh_type _loc = <:ctyp< '$random_var ()$ >>

  let constrain_types _loc (e:string) (v:string) v_typ m m_typ =
    <:expr<
      let module M = struct
        value _ =
          let _ign1 = ($lid:e$ : $js_t_id _loc "t"$ 'b) in
          let _ign2 = ($lid:v$ : $v_typ$) in
          (fun (x : 'b) -> (x#$lid:m$ : $m_typ$ ));
        end
      in ()
    >>

  let method_call _loc obj lab lab_loc args =
    let args = List.map (fun e -> (e, fresh_type _loc)) args in
    let ret_type = fresh_type _loc in
    let method_type =
      List.fold_right
        (fun (_, arg_ty) rem_ty -> <:ctyp< $arg_ty$ -> $rem_ty$ >>)
        args <:ctyp< $js_t_id _loc "meth"$ $ret_type$ >>
    in
    let o = random_var () in
    let fakev = random_var () in
    let typ = fresh_type _loc in
    let args =
      List.map (fun (e, t) -> <:expr< $js_u_id _loc "inject"$ $with_type e t$ >>) args
    in
    let args = make_array _loc args in
    <:expr<
      let $lid:o$ = $obj$ in
      let $lid:fakev$ : $typ$ = Obj.magic () in
      let () = $constrain_types _loc o fakev ret_type lab method_type$ in
      (($js_u_id _loc "meth_call"$ $lid:o$ $str:unescape lab$ $args$): $typ$)
    >>

  let new_object _loc constructor args =
    let args = List.map (fun e -> (e, fresh_type _loc)) args in
    let obj_type = <:ctyp< $js_t_id _loc "t"$ $fresh_type _loc$ >> in
    let constr_fun_type =
      List.fold_right
        (fun (_, arg_ty) rem_ty -> <:ctyp< $arg_ty$ -> $rem_ty$ >>)
        args obj_type
    in
    let args =
      List.map (fun (e, t) -> <:expr< $js_u_id _loc "inject"$ $with_type e t$ >>) args
    in
    let args = make_array _loc args in
    let x = random_var () in
    let constr =
      with_type constructor <:ctyp< $js_t_id _loc "constr"$ $constr_fun_type$ >> in
    with_type
      <:expr< let $lid:x$ = $constr$ in
              $js_u_id _loc "new_obj"$ $lid:x$ $args$ >>
      <:ctyp< $obj_type$ >>

  let jsmeth = Gram.Entry.mk "jsmeth"

  EXTEND Gram
    jsmeth: [["##"; lab = label -> (_loc, lab) ]];
    expr: BEFORE "."
    ["##" RIGHTA
     [ e = SELF; (lab_loc, lab) = jsmeth ->
       let o = random_var () in
       let fakev = random_var () in
       let typ = fresh_type _loc in
       <:expr<
         let $lid:o$ = $e$ in
         let $lid:fakev$ : $typ$ = Obj.magic () in
         let () = $constrain_types _loc o fakev <:ctyp< 'a >> lab <:ctyp< $js_t_id _loc "gen_prop"$ <get : 'a; ..> >>$ in
         ($js_u_id _loc "get"$ $lid:o$ $str:unescape lab$ : $typ$)
         >>
     | e1 = SELF; (lab_loc, lab) = jsmeth; "<-"; e2 = expr LEVEL "top" ->
       let o = random_var () in
       let v = random_var () in
       <:expr<
         let $lid:o$ = $e1$ in
         let $lid:v$ = $e2$ in
         let () = $constrain_types _loc o v <:ctyp< 'a >> lab <:ctyp< $js_t_id _loc "gen_prop"$ <set : 'a -> unit; ..> >>$ in
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
         new_object _loc e (parse_comma_list l) ]];
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
