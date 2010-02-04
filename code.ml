
open Util

(*FIX: this should probably be somewhere else... *)
module VarPrinter = struct
  let reserved = Hashtbl.create 107

  let add_reserved s =
    if String.length s <= 5 then Hashtbl.replace reserved s ()

  let _ =
    List.iter add_reserved
      ["break"; "case"; "catch"; "do"; "else"; "for"; "if"; "in"; "new";
       "this"; "throw"; "try"; "var"; "void"; "while"; "with"; "class";
       "enum"; "super"; "const"; "yield"; "let"]

  let known = Hashtbl.create 1001

  let last = ref (-1)

  let c1 = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$"
  let c2 = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_$"

  let rec format_var x =
    let char c x = String.make 1 (c.[x]) in
    if x < 54 then
       char c1 x
    else
      format_var ((x - 54) / 64) ^ char c2 ((x - 54) mod 64)

  let rec to_string i =
    try
      Hashtbl.find known i
    with Not_found ->
      incr last;
      let j = !last in
      let s = format_var j in
      if Hashtbl.mem reserved s then
        to_string i
      else begin
        Hashtbl.add known i s;
        s
      end
end

let add_reserved_name = VarPrinter.add_reserved

module Var : sig
  type t
  val print : Format.formatter -> t -> unit
  val idx : t -> int
  val to_string : t -> string

  type stream
  val make_stream : unit -> stream
  val next : stream -> t * stream

  val fresh : unit -> t

  val count : unit -> int

  val compare : t -> t -> int
end = struct

  type t = int * int

  let last_var = ref 0

  type stream = int

  let c = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_$"

  let rec format_var x =
    let char x = String.make 1 (c.[x]) in
    if x < 65 then
       char (x - 1)
    else
      format_var (x / 64) ^ char (x mod 64)

(*
  let to_string (x, i) = "o$" ^ format_var i(*format_var x ^ Format.sprintf "%d" i*)
*)
  let to_string (x, i) = VarPrinter.to_string i

  let print f x = Format.fprintf f "%s" (to_string x)

  let make_stream () = 1

  let next current =
    incr last_var;
    ((current, !last_var), current + 1)

  let fresh () = incr last_var; (0, !last_var)

  let count () = !last_var + 1

  let idx v = snd v

  let compare v1 v2 = compare (idx v1) (idx v2)
end

type addr = int

type prim =
    Vectlength
  | Array_get
  | C_call of string
  | Not | Neg | IsInt
  | Add | Sub | Mul | Div | Mod | And | Or | Xor | Lsl | Lsr | Asr
  | Eq | Neq | Lt | Le | Ult
  | Offset of int

type expr =
    Const of int
  | Apply of Var.t * Var.t list
  | Direct_apply of Var.t * Var.t list
  | Block of int * Var.t array
  | Field of Var.t * int
  | Closure of Var.t list * addr
  | Constant of Obj.t
  | Prim of prim * Var.t list
  | Variable of Var.t

type instr =
    Let of Var.t * expr
  | Assign of Var.t * Var.t
  | Set_field of Var.t * int * Var.t
  | Offset_ref of Var.t * int
  | Array_set of Var.t * Var.t * Var.t

type cond = IsTrue | CEq of int | CLt of int | CLe of int | CUlt of int

type cont = addr * Var.t option

type last =
    Return of Var.t
  | Raise of Var.t
  | Stop
  | Branch of cont
  | Cond of cond * Var.t * cont * cont
  | Switch of Var.t * cont array * cont array
  | Pushtrap of cont * addr * cont
  | Poptrap of cont

type block = Var.t option * instr list * last

type program = addr * block IntMap.t * addr

(****)

let dummy_cont = (-1, None)

let is_dummy_cont (pc, _) = pc < 0

(****)

let rec print_var_list f l =
  match l with
    []     -> ()
  | [x]    -> Var.print f x
  | x :: r -> Format.fprintf f "%a, %a" Var.print x print_var_list r

let print_prim f p l =
  match p, l with
    Vectlength, [x]   -> Format.fprintf f "%a.length" Var.print x
  | Array_get, [x; y] -> Format.fprintf f "%a[%a]" Var.print x Var.print y
  | C_call s, l       -> Format.fprintf f "\"%s\"(%a)" s print_var_list l
  | Not, [x]          -> Format.fprintf f "!%a" Var.print x
  | Neg, [x]          -> Format.fprintf f "-%a" Var.print x
  | IsInt, [x]        -> Format.fprintf f "is_int(%a)" Var.print x
  | Add, [x; y]       -> Format.fprintf f "%a + %a" Var.print x Var.print y
  | Sub, [x; y]       -> Format.fprintf f "%a - %a" Var.print x Var.print y
  | Mul, [x; y]       -> Format.fprintf f "%a * %a" Var.print x Var.print y
  | Div, [x; y]       -> Format.fprintf f "%a / %a" Var.print x Var.print y
  | Mod, [x; y]       -> Format.fprintf f "%a %% %a" Var.print x Var.print y
  | And, [x; y]       -> Format.fprintf f "%a & %a" Var.print x Var.print y
  | Or,  [x; y]       -> Format.fprintf f "%a | %a" Var.print x Var.print y
  | Xor, [x; y]       -> Format.fprintf f "%a ^ %a" Var.print x Var.print y
  | Lsl, [x; y]       -> Format.fprintf f "%a << %a" Var.print x Var.print y
  | Lsr, [x; y]       -> Format.fprintf f "%a >>> %a" Var.print x Var.print y
  | Asr, [x; y]       -> Format.fprintf f "%a >> %a" Var.print x Var.print y
  | Eq,  [x; y]       -> Format.fprintf f "%a === %a" Var.print x Var.print y
  | Neq, [x; y]       -> Format.fprintf f "!(%a === %a)" Var.print x Var.print y
  | Lt,  [x; y]       -> Format.fprintf f "%a < %a" Var.print x Var.print y
  | Le,  [x; y]       -> Format.fprintf f "%a <= %a" Var.print x Var.print y
  | Ult, [x; y]       -> Format.fprintf f "%a <= %a" Var.print x Var.print y
  | Offset i, [x]     -> Format.fprintf f "%a + %i" Var.print x i
  | _                 -> assert false

let print_expr f e =
  match e with
    Const i ->
      Format.fprintf f "%d" i
  | Apply (g, l) ->
      Format.fprintf f "%a(%a)" Var.print g print_var_list l
  | Direct_apply (g, l) ->
      Format.fprintf f "%a!(%a)" Var.print g print_var_list l
  | Block (t, a) ->
      Format.fprintf f "{tag=%d" t;
      for i = 0 to Array.length a - 1 do
        Format.fprintf f "; %d = %a" i Var.print a.(i)
      done;
      Format.fprintf f "}"
  | Field (x, i) ->
      Format.fprintf f "%a[%d]" Var.print x i
  | Closure (l, pc) ->
      Format.fprintf f "fun(%a){%d}" print_var_list l pc
  | Constant c ->
      Format.fprintf f "CONST{%a}" Instr.print_obj c
  | Prim (p, l) ->
      print_prim f p l
  | Variable x ->
      Format.fprintf f "%a" Var.print x

let print_instr f i =
  match i with
    Let (x, e)    ->
      Format.fprintf f "%a = %a" Var.print x print_expr e
  | Assign (x, y) ->
      Format.fprintf f "%a := %a" Var.print x Var.print y
  | Set_field (x, i, y) ->
      Format.fprintf f "%a[%d] = %a" Var.print x i Var.print y
  | Offset_ref (x, i) ->
      Format.fprintf f "%a[0] += %d" Var.print x i
  | Array_set (x, y, z) ->
      Format.fprintf f "%a[%a] = %a" Var.print x Var.print y Var.print z

let print_cond f (c, x) =
  match c with
    IsTrue -> Var.print f x
  | CEq n  -> Format.fprintf f "%d = %a" n Var.print x
  | CLt n  -> Format.fprintf f "%d < %a" n Var.print x
  | CLe n  -> Format.fprintf f "%d <= %a" n Var.print x
  | CUlt n -> Format.fprintf f "%d < %a" n Var.print x

let print_cont f (pc, arg) =
  if pc < 0 then Format.fprintf f "<dummy>" else
  match arg with
    None   -> Format.fprintf f "%d" pc
  | Some x -> Format.fprintf f "%d (%a)" pc Var.print x

let print_last f l =
  match l with
    Return x ->
      Format.fprintf f "return %a" Var.print x
  | Raise x ->
      Format.fprintf f "raise %a" Var.print x
  | Stop ->
      Format.fprintf f "stop"
  | Branch cont ->
      Format.fprintf f "branch %a" print_cont cont
  | Cond (cond, x, cont1, cont2) ->
      Format.fprintf f "if %a then %a else %a" print_cond (cond, x)
        print_cont cont1 print_cont cont2
  | Switch (x, a1, a2) ->
      Format.fprintf f "switch %a {" Var.print x;
      Array.iteri
        (fun i cont -> Format.fprintf f "int %d -> %a; " i print_cont cont) a1;
      Array.iteri
        (fun i cont -> Format.fprintf f "tag %d -> %a; " i print_cont cont) a2;
      Format.fprintf f "}"
  | Pushtrap (cont1, pc, cont2) ->
      Format.fprintf f "pushtrap %a handler %d continuation %a"
        print_cont cont1 pc print_cont cont2
  | Poptrap cont ->
      Format.fprintf f "poptrap %a" print_cont cont

type xinstr = Instr of instr | Last of last

let print_block annot pc (param, instr, last) =
  begin match param with
    None   -> Format.eprintf "==== %d ====@." pc
  | Some x -> Format.eprintf "==== %d (%a) ====@." pc Var.print x
  end;
  List.iter
    (fun i -> Format.eprintf " %s %a@." (annot pc (Instr i)) print_instr i)
    instr;
  Format.eprintf " %s %a@." (annot pc (Last last)) print_last last;
  Format.eprintf "@."

let print_program annot (pc, blocks, _) =
  Format.eprintf "Entry point: %d@.@." pc;
  IntMap.iter (print_block annot) blocks

(****)

let fold_closures blocks f accu =
  IntMap.fold
    (fun _ (_, instrs, _) accu ->
       List.fold_left
         (fun accu i ->
            match i with Let (_, Closure (_, pc)) -> f pc accu | _ -> accu)
         accu instrs)
    blocks accu

(****)

let (>>) x f = f x

let fold_children blocks pc f accu =
  let (_, _, last) = IntMap.find pc blocks in
  match last with
    Return _ | Raise _ | Stop ->
      accu
  | Branch (pc', _) | Poptrap (pc', _) ->
      f pc' accu
  | Cond (_, _, (pc1, _), (pc2, _)) | Pushtrap ((pc1, _), pc2, _) ->
      accu >> f pc1 >> f pc2
  | Switch (_, a1, a2) ->
      accu >> Array.fold_right (fun (pc, _) accu -> f pc accu) a1
           >> Array.fold_right (fun (pc, _) accu -> f pc accu) a2
