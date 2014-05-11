(* Js_of_ocaml compiler
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

type addr = int

module DebugAddr : sig
  type dbg = private addr
  val of_addr : addr -> dbg
  val to_addr : dbg -> addr
  val no : dbg
end = struct
  type dbg = int
  let of_addr (x : addr) = (x : dbg)
  let no = 0
  let to_addr (x : dbg) = (x : addr)
end

module Var : sig
  type t

  val print : Format.formatter -> t -> unit
  val idx : t -> int
  val of_idx : int -> t
  val to_string : ?origin:t -> t -> string

  val fresh : unit -> t
  val fork : t -> t

  val count : unit -> int

  val compare : t -> t -> int

  val name : t -> string -> unit
  val propagate_name : t -> t -> unit

  val reset : unit -> unit
  val set_pretty : bool -> unit

  val dummy : t
end = struct

  open Util
  type t = int

  let printer = VarPrinter.create ()

  let last_var = ref 0

  let reset () =
    last_var := 0;
    VarPrinter.reset printer

  let to_string ?origin i = VarPrinter.to_string printer ?origin i

  let print f x = Format.fprintf f "v%d" x
    (* Format.fprintf f "%s" (to_string x) *)

  let fresh () = incr last_var; !last_var

  let count () = !last_var + 1

  let idx v = v

  let of_idx v = v

  let compare v1 v2 = v1 - v2

  let name i nm = VarPrinter.name printer i nm
  let propagate_name i j = VarPrinter.propagate_name printer i j
  let set_pretty b = VarPrinter.set_pretty printer b

  let fork o =
    let n = fresh () in
    propagate_name o n;
    n

  let dummy = -1
end

module VarSet = Set.Make (Var)
module VarMap = Map.Make (Var)
module VarTbl = struct
  type 'a t = 'a array
  type key = Var.t
  type size = unit
  let get t x = t.(Var.idx x)
  let set t x v = t.(Var.idx x) <- v
  let make () v = Array.make (Var.count ()) v
end
module VarISet = struct
  type t = Var.t array
  type elt = Var.t
  let iter f t =
    for i = 0 to Array.length t - 1 do
      let x = t.(i) in
      if Var.compare x Var.dummy <> 0 then f x
    done
  let mem t x = Var.compare t.(Var.idx x) Var.dummy <> 0
  let add t x = t.(Var.idx x) <- x
  let remove t x = t.(Var.idx x) <- Var.dummy
  let copy = Array.copy
  let empty v = Array.make (Var.count ()) Var.dummy
end


module AddrSet = Util.IntSet
module AddrMap = Util.IntMap

type cont = addr * Var.t list

type prim =
    Vectlength
  | Array_get
  | Extern of string
  | Not | IsInt
  | Eq | Neq | Lt | Le | Ult

type constant =
    String of string
  | IString of string
  | Float of float
  | Float_array of float array
  | Int64 of int64
  | Tuple of int * constant array
  | Int of int32

type prim_arg =
    Pv of Var.t
  | Pc of constant

type expr =
    Const of int32
  | Apply of Var.t * Var.t list * bool
  | Block of int * Var.t array
  | Field of Var.t * int
  | Closure of Var.t list * cont
  | Constant of constant
  | Prim of prim * prim_arg list

type instr =
    Let of Var.t * expr
  | Set_field of Var.t * int * Var.t
  | Offset_ref of Var.t * int
  | Array_set of Var.t * Var.t * Var.t

type cond = IsTrue | CEq of int32 | CLt of int32 | CLe of int32 | CUlt of int32

type last =
    Return of Var.t
  | Raise of Var.t
  | Stop
  | Branch of cont
  | Cond of cond * Var.t * cont * cont
  | Switch of Var.t * cont array * cont array
  | Pushtrap of cont * Var.t * cont * addr
  | Poptrap of cont

type block =
  { params : Var.t list;
    handler : (Var.t * cont) option;
    body : instr list;
    branch : last }

type program = addr * block AddrMap.t * addr

(****)

let rec print_list pr f l =
  match l with
    []     -> ()
  | [x]    -> pr f x
  | x :: r -> Format.fprintf f "%a, %a" pr x (print_list pr) r

let print_var_list = print_list Var.print

let print_cont f (pc, args) =
  Format.fprintf f "%d (%a)" pc print_var_list args

let rec print_constant f x =
  match x with
    String s ->
      Format.fprintf f "%S" s
  | IString s ->
      Format.fprintf f "%S" s
  | Float fl ->
      Format.fprintf f "%.12g" fl
  | Float_array a ->
      Format.fprintf f "[|";
      for i = 0 to Array.length a - 1 do
        if i > 0 then Format.fprintf f ", ";
        Format.fprintf f "%.12g" a.(i)
      done;
      Format.fprintf f "|]"
  | Int64 i ->
      Format.fprintf f "%LdL" i
  | Tuple (tag, a) ->
      Format.fprintf f "<%d>" tag;
      begin match Array.length a with
        0 -> ()
      | 1 ->
          Format.fprintf f "("; print_constant f a.(0); Format.fprintf f ")"
      | n ->
          Format.fprintf f "("; print_constant f a.(0);
          for i = 1 to n - 1 do
            Format.fprintf f ", "; print_constant f a.(i)
          done;
          Format.fprintf f ")"
      end
  | Int i -> Format.fprintf f "%ld" i

let print_arg f a =
  match a with
    Pv x -> Var.print f x
  | Pc c -> print_constant f c

let binop s =
  match s with
    "%int_add" -> "+"
  | "%int_sub" -> "-"
  | "%int_mul" -> "*"
  | "%int_div" -> "/"
  | "%int_mod" -> "%"
  | "%int_and" -> "&"
  | "%int_or"  -> "|"
  | "%int_xor" -> "^"
  | "%int_lsl" -> "<<"
  | "%int_lsr" -> ">>>"
  | "%int_asr" -> ">>"
  | _          -> raise Not_found


let unop s =
  match s with
    "%int_neg" -> "-"
  | _          -> raise Not_found

let print_prim f p l =
  match p, l with
    Vectlength, [x]   -> Format.fprintf f "%a.length" print_arg x
  | Array_get, [x; y] -> Format.fprintf f "%a[%a]" print_arg x print_arg y
  | Extern s, [x; y]  ->
      begin try
        Format.fprintf f "%a %s %a" print_arg x (binop s) print_arg y
      with Not_found ->
        Format.fprintf f "\"%s\"(%a)" s (print_list print_arg) l
      end
  | Extern s, [x]  ->
      begin try
        Format.fprintf f "%s %a" (unop s) print_arg x
      with Not_found ->
        Format.fprintf f "\"%s\"(%a)" s (print_list print_arg) l
      end
  | Extern s, _       -> Format.fprintf f "\"%s\"(%a)"
                           s (print_list print_arg) l
  | Not, [x]          -> Format.fprintf f "!%a" print_arg x
  | IsInt, [x]        -> Format.fprintf f "is_int(%a)" print_arg x
  | Eq,  [x; y]       -> Format.fprintf f "%a === %a" print_arg x print_arg y
  | Neq, [x; y]       -> Format.fprintf f "!(%a === %a)" print_arg x print_arg y
  | Lt,  [x; y]       -> Format.fprintf f "%a < %a" print_arg x print_arg y
  | Le,  [x; y]       -> Format.fprintf f "%a <= %a" print_arg x print_arg y
  | Ult, [x; y]       -> Format.fprintf f "%a <= %a" print_arg x print_arg y
  | _                 -> assert false

let print_expr f e =
  match e with
    Const i ->
      Format.fprintf f "%ld" i
  | Apply (g, l, exact) ->
      if exact then
        Format.fprintf f "%a!(%a)" Var.print g print_var_list l
      else
        Format.fprintf f "%a(%a)" Var.print g print_var_list l
  | Block (t, a) ->
      Format.fprintf f "{tag=%d" t;
      for i = 0 to Array.length a - 1 do
        Format.fprintf f "; %d = %a" i Var.print a.(i)
      done;
      Format.fprintf f "}"
  | Field (x, i) ->
      Format.fprintf f "%a[%d]" Var.print x i
  | Closure (l, cont) ->
      Format.fprintf f "fun(%a){%a}" print_var_list l print_cont cont
  | Constant c ->
      Format.fprintf f "CONST{%a}" print_constant c
  | Prim (p, l) ->
      print_prim f p l

let print_instr f i =
  match i with
    Let (x, e)    ->
      Format.fprintf f "%a = %a" Var.print x print_expr e
  | Set_field (x, i, y) ->
      Format.fprintf f "%a[%d] = %a" Var.print x i Var.print y
  | Offset_ref (x, i) ->
      Format.fprintf f "%a[0] += %d" Var.print x i
  | Array_set (x, y, z) ->
      Format.fprintf f "%a[%a] = %a" Var.print x Var.print y Var.print z

let print_cond f (c, x) =
  match c with
    IsTrue -> Var.print f x
  | CEq n  -> Format.fprintf f "%ld = %a" n Var.print x
  | CLt n  -> Format.fprintf f "%ld < %a" n Var.print x
  | CLe n  -> Format.fprintf f "%ld <= %a" n Var.print x
  | CUlt n -> Format.fprintf f "%ld < %a" n Var.print x

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
  | Pushtrap (cont1, x, cont2, pc) ->
      Format.fprintf f "pushtrap %a handler %a => %a continuation %d"
        print_cont cont1 Var.print x print_cont cont2 pc
  | Poptrap cont ->
      Format.fprintf f "poptrap %a" print_cont cont

type xinstr = Instr of instr | Last of last

let print_block annot pc block =
  Format.eprintf "==== %d (%a) ====@." pc print_var_list block.params;
  begin match block.handler with
    Some (x, cont) ->
      Format.eprintf "    handler %a => %a@." Var.print x print_cont cont
  | None ->
      ()
  end;
  List.iter
    (fun i -> Format.eprintf " %s %a@." (annot pc (Instr i)) print_instr i)
    block.body;
  Format.eprintf " %s %a@." (annot pc (Last block.branch))
    print_last block.branch;
  Format.eprintf "@."

let print_program annot (pc, blocks, _) =
  Format.eprintf "Entry point: %d@.@." pc;
  AddrMap.iter (print_block annot) blocks

(****)

let fold_closures (pc, blocks, _) f accu =
  AddrMap.fold
    (fun _ block accu ->
       List.fold_left
         (fun accu i ->
            match i with
              Let (x, Closure (params, cont)) ->
                f (Some x) params cont accu
            | _ ->
                accu)
         accu block.body)
    blocks (f None [] (pc, []) accu)

(****)

let prepend (start, blocks, free_pc) body =
  let new_start = free_pc in
  let blocks =
    AddrMap.add new_start
      { params = [];
        handler = None;
        body = body;
        branch = Branch (start, []) }
      blocks
  in
  let free_pc = free_pc + 1 in
  (new_start, blocks, free_pc)


let (>>) x f = f x

let fold_children blocks pc f accu =
  let block = AddrMap.find pc blocks in
  let accu =
    match block.handler with
      Some (_, (pc, _)) -> f pc accu
    | None              -> accu
  in
  match block.branch with
    Return _ | Raise _ | Stop ->
      accu
  | Branch (pc', _) | Poptrap (pc', _) | Pushtrap ((pc', _), _, _, _) ->
      f pc' accu
  | Cond (_, _, (pc1, _), (pc2, _)) ->
      f pc1 accu >> f pc1 >> f pc2
  | Switch (_, a1, a2) ->
      accu >> Array.fold_right (fun (pc, _) accu -> f pc accu) a1
           >> Array.fold_right (fun (pc, _) accu -> f pc accu) a2

let eq (pc1, blocks1, _) (pc2, blocks2, _) =
  pc1 = pc2 &&
  AddrMap.cardinal blocks1 = AddrMap.cardinal blocks2 &&
  AddrMap.fold (fun pc block1 b ->
    b &&
    try
      let block2 = AddrMap.find pc blocks2 in
      block1.params = block2.params &&
      block1.branch = block2.branch &&
      block1.body   = block2.body
    with Not_found -> false
  ) blocks1 true
