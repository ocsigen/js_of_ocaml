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

  let rec comma_list e =
    match e with
      <:expr< $e1$, $e2$ >> -> e1 :: comma_list e2
    | _                    -> [e]

  let rec to_sem_expr _loc l =
    match l with
      []        -> assert false
    | [e]       -> e
    | e1 :: rem -> <:expr< $e1$; $to_sem_expr _loc rem$ >>

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

  let method_call _loc e lab l =
    let t = random_var () in
    let l = List.map (fun e -> (e, random_var ())) l in
    let mtyp =
      List.fold_right (fun (_, t) ty -> <:ctyp< '$t$ -> $ty$ >>)
        l <:ctyp< Js.meth '$t$ >>
    in
    let typ =
      <:ctyp< Js.t < $lid:lab$ : $mtyp$; .. > -> _ -> _ -> '$t$ >>
    in
    let l =
      List.map (fun (e, t) -> <:expr< Js.Unsafe.inject ($e$ : '$t$) >>) l
    in
    let a =
      match l with
        [] -> <:expr< [| |] >>
      | _  -> <:expr< [| $to_sem_expr _loc l$ |] >>
    in
    <:expr< (Js.Unsafe.meth_call : $typ$) $e$ $str:unescape lab$ $a$ >>

  EXTEND Gram
    expr: BEFORE "."
    ["##" RIGHTA
     [ e = SELF; "##"; lab = label ->
         let t = random_var () in
         let obj_typ =
           <:ctyp< Js.t < $lid:lab$ : Js.gen_prop <get:'$t$; ..>; .. > >>
         in
         <:expr< (Js.Unsafe.get ($e$ : $obj_typ$)
                    $str:unescape lab$ : '$t$) >>
     | e1 = SELF; "##"; lab = label; "<-"; e2 = expr LEVEL "top" ->
         let t = random_var () in
         let typ =
           <:ctyp< Js.t < $lid:lab$ : Js.gen_prop <set:'$t$; ..>; .. > -> _ -> '$t$ -> _ >>
         in
         <:expr< (Js.Unsafe.set : $typ$) $e1$ $str:unescape lab$ $e2$ >>
     | e = SELF; "##"; lab = label; "("; ")" ->
         method_call _loc e lab []
     | e = SELF; "##"; lab = label; "("; l = comma_expr; ")" ->
         method_call _loc e lab (comma_list l)
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
