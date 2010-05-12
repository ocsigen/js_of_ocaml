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

  let method_call _loc e lab l =
    let t = random_var () in
    let l = List.map (fun e -> (e, random_var ())) l in
    let mtyp =
      List.fold_right (fun (_, t) ty -> <:ctyp< '$t$ -> $ty$ >>)
        l <:ctyp< Js.Obj.meth '$t$ >>
    in
    let typ =
      <:ctyp< Js.Obj.t < $lid:lab$ : $mtyp$; .. > -> _ -> _ -> '$t$ >>
    in
    let l =
      List.map (fun (e, t) -> <:expr< Js.Obj.unsafe_inject ($e$ : '$t$) >>) l
    in
    let a =
      match l with
        [] -> <:expr< [| |] >>
      | _  -> <:expr< [| $to_sem_expr _loc l$ |] >>
    in
    <:expr< (Js.Obj.unsafe_meth_call : $typ$) $e$ $str:lab$ $a$ >>

  EXTEND Gram
    expr: BEFORE "."
    ["##" RIGHTA
     [ e = SELF; "##"; lab = label ->
         let t = random_var () in
         let typ =
           <:ctyp< Js.Obj.t < $lid:lab$ : Js.Obj.prop '$t$; .. > -> _ -> '$t$ >> in
         <:expr< (Js.Obj.unsafe_get : $typ$) $e$ $str:lab$ >>
     | e1 = SELF; "##"; lab = label; "<-"; e2 = SELF ->
         let t = random_var () in
         let typ =
           <:ctyp< Js.Obj.t < $lid:lab$ : Js.Obj.prop '$t$; .. > -> _ -> '$t$ -> _ >>
         in
         <:expr< (Js.Obj.unsafe_set : $typ$) $e1$ $str:lab$ $e2$ >>
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
module WEIRDMODULENAME = struct type 'a o = 'a Js.Obj.t val unsafe_get = Js.Obj.unsafe_get ... end
(let module M = WEIRDMODULENAME in (M.unsafe_get : <x : 'a M.meth> -> 'a))
*)

end

module M = Register.OCamlSyntaxExtension(Id)(Make)
