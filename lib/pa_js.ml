(*
let ch = open_in "/dev/urandom";;
for i = 0 to 15 do
 let x = input_binary_int ch in
 Format.eprintf "%x@." x
done;;
*)

let rnd = Random.State.make [|0x513511d4|]
let random_var () =
  Format.sprintf "a%08Lx" (Random.State.int64 rnd 0x100000000L)

open Camlp4

module Id : Sig.Id = struct
  let name = "Javascript"
  let version = "0.1"
end

module Make (Syntax : Sig.Camlp4Syntax) = struct
  open Sig
  include Syntax

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

  let method_call _loc obj lab args =
    let args = List.map (fun e -> (e, fresh_type _loc)) args in
    let ret_type = fresh_type _loc in
    let method_type =
      List.fold_right
        (fun (_, arg_ty) rem_ty -> <:ctyp< $arg_ty$ -> $rem_ty$ >>)
        args <:ctyp< Js.meth $ret_type$ >>
    in
    let simple_object_type = <:ctyp< Js.t < $lid:lab$ : _; .. > >> in
    let object_type = <:ctyp< Js.t < $lid:lab$ : $method_type$; .. > >> in
    let obj = with_type (with_type obj simple_object_type) object_type in
    let args =
      List.map (fun (e, t) -> <:expr< Js.Unsafe.inject $with_type e t$ >>) args
    in
    let args = make_array _loc args in
    let x = random_var () in
    with_type
      <:expr< let $lid:x$ = $obj$ in
              Js.Unsafe.meth_call $lid:x$ $str:unescape lab$ $args$ >>
      <:ctyp< $ret_type$ >>

  let new_object _loc constructor args =
    let args = List.map (fun e -> (e, fresh_type _loc)) args in
    let obj_type = <:ctyp< Js.t $fresh_type _loc$ >> in
    let constr_fun_type =
      List.fold_right
        (fun (_, arg_ty) rem_ty -> <:ctyp< $arg_ty$ -> $rem_ty$ >>)
        args obj_type
    in
    let args =
      List.map (fun (e, t) -> <:expr< Js.Unsafe.inject $with_type e t$ >>) args
    in
    let args = make_array _loc args in
    let x = random_var () in
    let constr =
      with_type constructor <:ctyp< Js.constr $constr_fun_type$ >> in
    with_type
      <:expr< let $lid:x$ = $constr$ in
              Js.Unsafe.new_obj $lid:x$ $args$ >>
      <:ctyp< $obj_type$ >>

  EXTEND Gram
    expr: BEFORE "."
    ["##" RIGHTA
     [ e = SELF; "##"; lab = label ->
         let t = fresh_type _loc in
         let obj_typ =
           <:ctyp< Js.t < $lid:lab$ : Js.gen_prop <get:$t$; ..>; .. > >>
         in
         <:expr< (Js.Unsafe.get ($e$ : $obj_typ$)
                    $str:unescape lab$ : $t$) >>
     | e1 = SELF; "##"; lab = label; "<-"; e2 = expr LEVEL "top" ->
         let t = fresh_type _loc in
         let typ =
           <:ctyp< Js.t < $lid:lab$ : Js.gen_prop <set:$t$; ..>; .. > -> _ -> $t$ -> _ >>
         in
         <:expr< (Js.Unsafe.set : $typ$) $e1$ $str:unescape lab$ $e2$ >>
     | e = SELF; "##"; lab = label; "("; ")" ->
         method_call _loc e lab []
     | e = SELF; "##"; lab = label; "("; l = comma_expr; ")" ->
         method_call _loc e lab (parse_comma_list l)
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
