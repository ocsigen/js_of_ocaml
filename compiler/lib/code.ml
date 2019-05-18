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
open! Stdlib

module Addr = struct
  type t = int

  module Set = Set.Make (Int)
  module Map = Map.Make (Int)

  let to_string = string_of_int

  let zero = 0

  let pred = pred

  let succ = succ
end

module DebugAddr : sig
  type t = private Addr.t

  val of_addr : Addr.t -> t

  val to_addr : t -> Addr.t

  val no : t
end = struct
  type t = int

  let of_addr (x : Addr.t) : t = x

  let no = 0

  let to_addr (x : t) : Addr.t = x
end

module Var : sig
  type t

  val print : Format.formatter -> t -> unit

  val equal : t -> t -> bool

  val idx : t -> int

  val of_idx : int -> t

  val to_string : ?origin:t -> t -> string

  val fresh : unit -> t

  val fresh_n : string -> t

  val fork : t -> t

  val count : unit -> int

  val compare : t -> t -> int

  val get_loc : t -> Parse_info.t option

  val loc : t -> Parse_info.t -> unit

  val name : t -> string -> unit

  val get_name : t -> string option

  val propagate_name : t -> t -> unit

  val reset : unit -> unit

  val set_pretty : bool -> unit

  val set_stable : bool -> unit

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t

  module Tbl : sig
    type key = t

    type 'a t

    type size = unit

    val get : 'a t -> key -> 'a

    val set : 'a t -> key -> 'a -> unit

    val make : size -> 'a -> 'a t
  end

  module ISet : sig
    type elt = t

    type t

    val empty : unit -> t

    val iter : (elt -> unit) -> t -> unit

    val mem : t -> elt -> bool

    val add : t -> elt -> unit

    val remove : t -> elt -> unit

    val copy : t -> t
  end
end = struct
  module T = struct
    type t = int

    let compare : t -> t -> int = compare

    let equal (a : t) (b : t) = a = b
  end

  include T

  let printer = VarPrinter.create VarPrinter.Alphabet.javascript

  let locations = Hashtbl.create 17

  let last_var = ref 0

  let reset () =
    last_var := 0;
    Hashtbl.clear locations;
    VarPrinter.reset printer

  let to_string ?origin i = VarPrinter.to_string printer ?origin i

  let print f x = Format.fprintf f "v%d" x

  (* Format.fprintf f "%s" (to_string x) *)

  let name i nm = VarPrinter.name printer i nm

  let loc i pi = Hashtbl.add locations i pi

  (*;
    Format.eprintf "loc for %d : %d-%d\n%!"
                   i pi.Parse_info.line pi.Parse_info.col
                               *)
  let get_loc i = try Some (Hashtbl.find locations i) with Not_found -> None

  let fresh () =
    incr last_var;
    !last_var

  let fresh_n nm =
    incr last_var;
    name !last_var nm;
    !last_var

  let count () = !last_var + 1

  let idx v = v

  let of_idx v = v

  let get_name i = VarPrinter.get_name printer i

  let propagate_name i j =
    VarPrinter.propagate_name printer i j;
    match get_loc i with
    | None -> ()
    | Some l -> loc j l

  let set_pretty b = VarPrinter.set_pretty printer b

  let set_stable b = VarPrinter.set_stable printer b

  let fork o =
    let n = fresh () in
    propagate_name o n;
    n

  let dummy = -1

  module Set = Set.Make (T)
  module Map = Map.Make (T)

  module Tbl = struct
    type 'a t = 'a array

    type key = T.t

    type size = unit

    let get t x = t.(x)

    let set t x v = t.(x) <- v

    let make () v = Array.make (count ()) v
  end

  module ISet = struct
    type t = T.t array

    type elt = T.t

    let iter f t =
      for i = 0 to Array.length t - 1 do
        let x = t.(i) in
        if compare x dummy <> 0 then f x
      done

    let mem t x = compare t.(x) dummy <> 0

    let add t x = t.(x) <- x

    let remove t x = t.(x) <- dummy

    let copy = Array.copy

    let empty _v = Array.make (count ()) dummy
  end
end

type cont = Addr.t * Var.t list

type prim =
  | Vectlength
  | Array_get
  | Extern of string
  | Not
  | IsInt
  | Eq
  | Neq
  | Lt
  | Le
  | Ult

type array_or_not =
  | Array
  | NotArray
  | Unknown

type constant =
  | String of string
  | IString of string
  | Float of float
  | Float_array of float array
  | Int64 of int64
  | Tuple of int * constant array * array_or_not
  | Int of int32

type prim_arg =
  | Pv of Var.t
  | Pc of constant

type expr =
  | Const of int32
  | Apply of Var.t * Var.t list * bool
  | Block of int * Var.t array * array_or_not
  | Field of Var.t * int
  | Closure of Var.t list * cont
  | Constant of constant
  | Prim of prim * prim_arg list

type instr =
  | Let of Var.t * expr
  | Set_field of Var.t * int * Var.t
  | Offset_ref of Var.t * int
  | Array_set of Var.t * Var.t * Var.t

type cond =
  | IsTrue
  | CEq of int32
  | CLt of int32
  | CLe of int32
  | CUlt of int32

type last =
  | Return of Var.t
  | Raise of Var.t * [`Normal | `Notrace | `Reraise]
  | Stop
  | Branch of cont
  | Cond of cond * Var.t * cont * cont
  | Switch of Var.t * cont array * cont array
  | Pushtrap of cont * Var.t * cont * Addr.Set.t
  | Poptrap of cont * Addr.t

type block =
  { params : Var.t list
  ; handler : (Var.t * cont) option
  ; body : instr list
  ; branch : last }

type program = Addr.t * block Addr.Map.t * Addr.t

(****)

let rec print_list pr f l =
  match l with
  | [] -> ()
  | [x] -> pr f x
  | x :: r -> Format.fprintf f "%a, %a" pr x (print_list pr) r

let print_var_list = print_list Var.print

let print_cont f (pc, args) = Format.fprintf f "%d (%a)" pc print_var_list args

let rec print_constant f x =
  match x with
  | String s -> Format.fprintf f "%S" s
  | IString s -> Format.fprintf f "%S" s
  | Float fl -> Format.fprintf f "%.12g" fl
  | Float_array a ->
      Format.fprintf f "[|";
      for i = 0 to Array.length a - 1 do
        if i > 0 then Format.fprintf f ", ";
        Format.fprintf f "%.12g" a.(i)
      done;
      Format.fprintf f "|]"
  | Int64 i -> Format.fprintf f "%LdL" i
  | Tuple (tag, a, _) -> (
      Format.fprintf f "<%d>" tag;
      match Array.length a with
      | 0 -> ()
      | 1 ->
          Format.fprintf f "(";
          print_constant f a.(0);
          Format.fprintf f ")"
      | n ->
          Format.fprintf f "(";
          print_constant f a.(0);
          for i = 1 to n - 1 do
            Format.fprintf f ", ";
            print_constant f a.(i)
          done;
          Format.fprintf f ")")
  | Int i -> Format.fprintf f "%ld" i

let print_arg f a =
  match a with
  | Pv x -> Var.print f x
  | Pc c -> print_constant f c

let binop s =
  match s with
  | "%int_add" -> "+"
  | "%int_sub" -> "-"
  | "%int_mul" -> "*"
  | "%int_div" -> "/"
  | "%int_mod" -> "%"
  | "%int_and" -> "&"
  | "%int_or" -> "|"
  | "%int_xor" -> "^"
  | "%int_lsl" -> "<<"
  | "%int_lsr" -> ">>>"
  | "%int_asr" -> ">>"
  | _ -> raise Not_found

let unop s =
  match s with
  | "%int_neg" -> "-"
  | _ -> raise Not_found

let print_prim f p l =
  match p, l with
  | Vectlength, [x] -> Format.fprintf f "%a.length" print_arg x
  | Array_get, [x; y] -> Format.fprintf f "%a[%a]" print_arg x print_arg y
  | Extern s, [x; y] -> (
    try Format.fprintf f "%a %s %a" print_arg x (binop s) print_arg y
    with Not_found -> Format.fprintf f "\"%s\"(%a)" s (print_list print_arg) l)
  | Extern s, [x] -> (
    try Format.fprintf f "%s %a" (unop s) print_arg x
    with Not_found -> Format.fprintf f "\"%s\"(%a)" s (print_list print_arg) l)
  | Extern s, _ -> Format.fprintf f "\"%s\"(%a)" s (print_list print_arg) l
  | Not, [x] -> Format.fprintf f "!%a" print_arg x
  | IsInt, [x] -> Format.fprintf f "is_int(%a)" print_arg x
  | Eq, [x; y] -> Format.fprintf f "%a === %a" print_arg x print_arg y
  | Neq, [x; y] -> Format.fprintf f "!(%a === %a)" print_arg x print_arg y
  | Lt, [x; y] -> Format.fprintf f "%a < %a" print_arg x print_arg y
  | Le, [x; y] -> Format.fprintf f "%a <= %a" print_arg x print_arg y
  | Ult, [x; y] -> Format.fprintf f "%a <= %a" print_arg x print_arg y
  | _ -> assert false

let print_expr f e =
  match e with
  | Const i -> Format.fprintf f "%ld" i
  | Apply (g, l, exact) ->
      if exact
      then Format.fprintf f "%a!(%a)" Var.print g print_var_list l
      else Format.fprintf f "%a(%a)" Var.print g print_var_list l
  | Block (t, a, _) ->
      Format.fprintf f "{tag=%d" t;
      for i = 0 to Array.length a - 1 do
        Format.fprintf f "; %d = %a" i Var.print a.(i)
      done;
      Format.fprintf f "}"
  | Field (x, i) -> Format.fprintf f "%a[%d]" Var.print x i
  | Closure (l, cont) -> Format.fprintf f "fun(%a){%a}" print_var_list l print_cont cont
  | Constant c -> Format.fprintf f "CONST{%a}" print_constant c
  | Prim (p, l) -> print_prim f p l

let print_instr f i =
  match i with
  | Let (x, e) -> Format.fprintf f "%a = %a" Var.print x print_expr e
  | Set_field (x, i, y) -> Format.fprintf f "%a[%d] = %a" Var.print x i Var.print y
  | Offset_ref (x, i) -> Format.fprintf f "%a[0] += %d" Var.print x i
  | Array_set (x, y, z) ->
      Format.fprintf f "%a[%a] = %a" Var.print x Var.print y Var.print z

let print_cond f (c, x) =
  match c with
  | IsTrue -> Var.print f x
  | CEq n -> Format.fprintf f "%ld = %a" n Var.print x
  | CLt n -> Format.fprintf f "%ld < %a" n Var.print x
  | CLe n -> Format.fprintf f "%ld <= %a" n Var.print x
  | CUlt n -> Format.fprintf f "%ld < %a" n Var.print x

let print_last f l =
  match l with
  | Return x -> Format.fprintf f "return %a" Var.print x
  | Raise (x, `Normal) -> Format.fprintf f "raise %a" Var.print x
  | Raise (x, `Reraise) -> Format.fprintf f "reraise %a" Var.print x
  | Raise (x, `Notrace) -> Format.fprintf f "raise_notrace %a" Var.print x
  | Stop -> Format.fprintf f "stop"
  | Branch cont -> Format.fprintf f "branch %a" print_cont cont
  | Cond (cond, x, cont1, cont2) ->
      Format.fprintf
        f
        "if %a then %a else %a"
        print_cond
        (cond, x)
        print_cont
        cont1
        print_cont
        cont2
  | Switch (x, a1, a2) ->
      Format.fprintf f "switch %a {" Var.print x;
      Array.iteri a1 ~f:(fun i cont ->
          Format.fprintf f "int %d -> %a; " i print_cont cont);
      Array.iteri a2 ~f:(fun i cont ->
          Format.fprintf f "tag %d -> %a; " i print_cont cont);
      Format.fprintf f "}"
  | Pushtrap (cont1, x, cont2, pcs) ->
      Format.fprintf
        f
        "pushtrap %a handler %a => %a continuation %s"
        print_cont
        cont1
        Var.print
        x
        print_cont
        cont2
        (String.concat ~sep:", " (List.map (Addr.Set.elements pcs) ~f:string_of_int))
  | Poptrap (cont, _) -> Format.fprintf f "poptrap %a" print_cont cont

type xinstr =
  | Instr of instr
  | Last of last

let print_block annot pc block =
  Format.eprintf "==== %d (%a) ====@." pc print_var_list block.params;
  (match block.handler with
  | Some (x, cont) -> Format.eprintf "    handler %a => %a@." Var.print x print_cont cont
  | None -> ());
  List.iter block.body ~f:(fun i ->
      Format.eprintf " %s %a@." (annot pc (Instr i)) print_instr i);
  Format.eprintf " %s %a@." (annot pc (Last block.branch)) print_last block.branch;
  Format.eprintf "@."

let print_program annot (pc, blocks, _) =
  Format.eprintf "Entry point: %d@.@." pc;
  Addr.Map.iter (print_block annot) blocks

(****)

let fold_closures (pc, blocks, _) f accu =
  Addr.Map.fold
    (fun _ block accu ->
      List.fold_left block.body ~init:accu ~f:(fun accu i ->
          match i with
          | Let (x, Closure (params, cont)) -> f (Some x) params cont accu
          | _ -> accu))
    blocks
    (f None [] (pc, []) accu)

(****)

let prepend ((start, blocks, free_pc) as p) body =
  match body with
  | [] -> p
  | _ ->
      let new_start = free_pc in
      let branch = if Addr.Map.mem start blocks then Branch (start, []) else Stop in
      let blocks =
        Addr.Map.add new_start {params = []; handler = None; body; branch} blocks
      in
      let free_pc = free_pc + 1 in
      new_start, blocks, free_pc

let empty =
  let start = 0 in
  let free = 1 in
  let blocks =
    Addr.Map.singleton start {params = []; handler = None; body = []; branch = Stop}
  in
  start, blocks, free

let ( >> ) x f = f x

let fold_children blocks pc f accu =
  let block = Addr.Map.find pc blocks in
  let accu =
    match block.handler with
    | Some (_, (pc, _)) -> f pc accu
    | None -> accu
  in
  match block.branch with
  | Return _ | Raise _ | Stop -> accu
  | Branch (pc', _) | Poptrap ((pc', _), _) | Pushtrap ((pc', _), _, _, _) -> f pc' accu
  | Cond (_, _, (pc1, _), (pc2, _)) -> f pc1 accu >> f pc1 >> f pc2
  | Switch (_, a1, a2) ->
      let accu = Array.fold_right ~init:accu ~f:(fun (pc, _) accu -> f pc accu) a1 in
      let accu = Array.fold_right ~init:accu ~f:(fun (pc, _) accu -> f pc accu) a2 in
      accu

let rec traverse' fold f pc visited blocks acc =
  if not (Addr.Set.mem pc visited)
  then
    let visited = Addr.Set.add pc visited in
    let visited, acc =
      fold
        blocks
        pc
        (fun pc (visited, acc) ->
          let visited, acc = traverse' fold f pc visited blocks acc in
          visited, acc)
        (visited, acc)
    in
    let acc = f pc acc in
    visited, acc
  else visited, acc

let traverse fold f pc blocks acc = snd (traverse' fold f pc Addr.Set.empty blocks acc)

let eq (pc1, blocks1, _) (pc2, blocks2, _) =
  pc1 = pc2
  && Addr.Map.cardinal blocks1 = Addr.Map.cardinal blocks2
  && Addr.Map.fold
       (fun pc block1 b ->
         b
         &&
         try
           let block2 = Addr.Map.find pc blocks2 in
           Poly.(block1.params = block2.params)
           && Poly.(block1.branch = block2.branch)
           && Poly.(block1.body = block2.body)
         with Not_found -> false)
       blocks1
       true

let with_invariant = Debug.find "invariant"

let check_defs = false

let invariant (_, blocks, _) =
  if with_invariant ()
  then
    let defs = Array.make (Var.count ()) false in
    let check_cont (cont, args) =
      let b = Addr.Map.find cont blocks in
      assert (List.length args >= List.length b.params)
    in
    let define x =
      if check_defs
      then (
        assert (not defs.(Var.idx x));
        defs.(Var.idx x) <- true)
    in
    let check_expr = function
      | Const _ -> ()
      | Apply (_, _, _) -> ()
      | Block (_, _, _) -> ()
      | Field (_, _) -> ()
      | Closure (l, cont) ->
          List.iter l ~f:define;
          check_cont cont
      | Constant _ -> ()
      | Prim (_, _) -> ()
    in
    let check_instr = function
      | Let (x, e) ->
          define x;
          check_expr e
      | Set_field (_, _i, _) -> ()
      | Offset_ref (_x, _i) -> ()
      | Array_set (_x, _y, _z) -> ()
    in
    let check_last = function
      | Return _ -> ()
      | Raise _ -> ()
      | Stop -> ()
      | Branch cont -> check_cont cont
      | Cond (_cond, _x, cont1, cont2) ->
          check_cont cont1;
          check_cont cont2
      | Switch (_x, a1, a2) ->
          Array.iteri a1 ~f:(fun _ cont -> check_cont cont);
          Array.iteri a2 ~f:(fun _ cont -> check_cont cont)
      | Pushtrap (cont1, _x, cont2, _pcs) ->
          check_cont cont1;
          check_cont cont2
      | Poptrap (cont, _) -> check_cont cont
    in
    Addr.Map.iter
      (fun _pc block ->
        List.iter block.params ~f:define;
        Option.iter block.handler ~f:(fun (_, cont) -> check_cont cont);
        List.iter block.body ~f:check_instr;
        check_last block.branch)
      blocks
