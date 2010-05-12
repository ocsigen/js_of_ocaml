(*
let ch = open_in "/dev/urandom";;
for i = 0 to 15 do
 let x = input_binary_int ch in
 Format.eprintf "%x@." x
done;;
*)

let rnd = Random.State.make [|0x513511d4|]
let random_var () =
  Format.sprintf "a%8Lx" (Random.State.int64 rnd 0x100000000L)

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

  EXTEND Gram
    expr: BEFORE "."
    ["##" RIGHTA
     [ e = SELF; "##"; lab = label ->
         let t = random_var () in
         let typ = <:ctyp< Js.o < $lid:lab$ : '$t$; .. > -> _ -> '$t$ >> in
         <:expr< (Js.unsafe_get : $typ$) $e$ $str:lab$ >>
     | e1 = SELF; "##"; lab = label; "<-"; e2 = SELF ->
         let t = random_var () in
         let typ =
           <:ctyp< Js.o < $lid:lab$ : '$t$; .. > -> _ -> '$t$ -> _ >> in
         <:expr< (Js.unsafe_set : $typ$) $e1$ $str:lab$ $e2$ >>
        ]];
  END

end

module M = Register.OCamlSyntaxExtension(Id)(Make)
