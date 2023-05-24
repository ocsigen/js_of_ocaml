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
  type t [@@ocaml.immediate]

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

    val iter : (key -> 'a -> unit) -> 'a t -> unit
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

  let printer = Var_printer.create Var_printer.Alphabet.javascript

  let locations = Hashtbl.create 17

  let last_var = ref 0

  let reset () =
    last_var := 0;
    Hashtbl.clear locations;
    Var_printer.reset printer

  let to_string ?origin i = Var_printer.to_string printer ?origin i

  let print f x = Format.fprintf f "v%d" x

  (* Format.fprintf f "%s" (to_string x) *)

  let name i nm = Var_printer.name printer i nm

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

  let get_name i = Var_printer.get_name printer i

  let propagate_name i j =
    Var_printer.propagate_name printer i j;
    match get_loc i with
    | None -> ()
    | Some l -> loc j l

  let set_pretty b = Var_printer.set_pretty printer b

  let set_stable b = Var_printer.set_stable printer b

  let fork o =
    let n = fresh () in
    propagate_name o n;
    n

  module Set = Set.Make (T)
  module Map = Map.Make (T)

  module Tbl = struct
    type 'a t = 'a array

    type key = T.t

    type size = unit

    let get t x = t.(x)

    let set t x v = t.(x) <- v

    let make () v = Array.make (count ()) v

    let iter f t =
      for i = 0 to Array.length t - 1 do
        f i (Array.unsafe_get t i)
      done
  end

  module ISet = struct
    type t = BitSet.t

    type elt = T.t

    let iter f t = BitSet.iter ~f t

    let mem t x = BitSet.mem t x

    let add t (x : int) = BitSet.set t x

    let remove t x = BitSet.unset t x

    let copy = BitSet.copy

    let empty _v = BitSet.create' (count ())
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

module Native_string = struct
  type t =
    | Byte of string
    | Utf of Utf8_string.t

  let of_string x = Utf (Utf8_string.of_string_exn x)

  let of_bytestring x = if String.is_ascii x then of_string x else Byte x

  let equal a b =
    match a, b with
    | Byte x, Byte y -> String.equal x y
    | Utf (Utf8 x), Utf (Utf8 y) -> String.equal x y
    | Utf _, Byte _ | Byte _, Utf _ -> false
end

type int_kind =
  | Regular
  | Int32
  | Native

type constant =
  | String of string
  | NativeString of Native_string.t
  | Float of float
  | Float_array of float array
  | Int64 of int64
  | Tuple of int * constant array * array_or_not
  | Int of int_kind * int32

let rec constant_equal a b =
  match a, b with
  | String a, String b -> Some (String.equal a b)
  | NativeString a, NativeString b -> Some (Native_string.equal a b)
  | Tuple (ta, a, _), Tuple (tb, b, _) ->
      if ta <> tb || Array.length a <> Array.length b
      then Some false
      else
        let same = ref (Some true) in
        for i = 0 to Array.length a - 1 do
          match !same, constant_equal a.(i) b.(i) with
          | None, _ -> ()
          | _, None -> same := None
          | Some s, Some c -> same := Some (s && c)
        done;
        !same
  | Int64 a, Int64 b -> Some (Int64.equal a b)
  | Float_array a, Float_array b -> Some (Array.equal Float.equal a b)
  | Int (k, a), Int (k', b) -> if Poly.(k = k') then Some (Int32.equal a b) else None
  | Float a, Float b -> Some (Float.equal a b)
  | String _, NativeString _ | NativeString _, String _ -> None
  | Int _, Float _ | Float _, Int _ -> None
  | Tuple ((0 | 254), _, _), Float_array _ -> None
  | Float_array _, Tuple ((0 | 254), _, _) -> None
  | Tuple _, (String _ | NativeString _ | Int64 _ | Int _ | Float _ | Float_array _) ->
      Some false
  | Float_array _, (String _ | NativeString _ | Int64 _ | Int _ | Float _ | Tuple _) ->
      Some false
  | String _, (Int64 _ | Int _ | Float _ | Tuple _ | Float_array _) -> Some false
  | NativeString _, (Int64 _ | Int _ | Float _ | Tuple _ | Float_array _) -> Some false
  | Int64 _, (String _ | NativeString _ | Int _ | Float _ | Tuple _ | Float_array _) ->
      Some false
  | Float _, (String _ | NativeString _ | Float_array _ | Int64 _ | Tuple (_, _, _)) ->
      Some false
  | Int _, (String _ | NativeString _ | Float_array _ | Int64 _ | Tuple (_, _, _)) ->
      Some false

type loc =
  | No
  | Before of Addr.t
  | After of Addr.t

type prim_arg =
  | Pv of Var.t
  | Pc of constant

type expr =
  | Apply of
      { f : Var.t
      ; args : Var.t list
      ; exact : bool
      }
  | Block of int * Var.t array * array_or_not
  | Field of Var.t * int
  | Closure of Var.t list * cont
  | Constant of constant
  | Prim of prim * prim_arg list

type instr =
  | Let of Var.t * expr
  | Assign of Var.t * Var.t
  | Set_field of Var.t * int * Var.t
  | Offset_ref of Var.t * int
  | Array_set of Var.t * Var.t * Var.t

type last =
  | Return of Var.t
  | Raise of Var.t * [ `Normal | `Notrace | `Reraise ]
  | Stop
  | Branch of cont
  | Cond of Var.t * cont * cont
  | Switch of Var.t * cont array * cont array
  | Pushtrap of cont * Var.t * cont * Addr.Set.t
  | Poptrap of cont

type block =
  { params : Var.t list
  ; body : (instr * loc) list
  ; branch : last * loc
  }

type program =
  { start : Addr.t
  ; blocks : block Addr.Map.t
  ; free_pc : Addr.t
  }

let noloc = No

let location_of_pc pc = Before pc
(****)

module Print = struct
  let rec list pr f l =
    match l with
    | [] -> ()
    | [ x ] -> pr f x
    | x :: r -> Format.fprintf f "%a, %a" pr x (list pr) r

  let var_list = list Var.print

  let cont f (pc, args) = Format.fprintf f "%d (%a)" pc var_list args

  let rec constant f x =
    match x with
    | String s -> Format.fprintf f "%S" s
    | NativeString (Byte s) -> Format.fprintf f "%Sj" s
    | NativeString (Utf (Utf8 s)) -> Format.fprintf f "%Sj" s
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
            constant f a.(0);
            Format.fprintf f ")"
        | n ->
            Format.fprintf f "(";
            constant f a.(0);
            for i = 1 to n - 1 do
              Format.fprintf f ", ";
              constant f a.(i)
            done;
            Format.fprintf f ")")
    | Int (k, i) ->
        Format.fprintf
          f
          "%ld%s"
          i
          (match k with
          | Regular -> ""
          | Int32 -> "l"
          | Native -> "n")

  let arg f a =
    match a with
    | Pv x -> Var.print f x
    | Pc c -> constant f c

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

  let prim f p l =
    match p, l with
    | Vectlength, [ x ] -> Format.fprintf f "%a.length" arg x
    | Array_get, [ x; y ] -> Format.fprintf f "%a[%a]" arg x arg y
    | Extern s, [ x; y ] -> (
        try Format.fprintf f "%a %s %a" arg x (binop s) arg y
        with Not_found -> Format.fprintf f "\"%s\"(%a)" s (list arg) l)
    | Extern s, [ x ] -> (
        try Format.fprintf f "%s %a" (unop s) arg x
        with Not_found -> Format.fprintf f "\"%s\"(%a)" s (list arg) l)
    | Extern s, _ -> Format.fprintf f "\"%s\"(%a)" s (list arg) l
    | Not, [ x ] -> Format.fprintf f "!%a" arg x
    | IsInt, [ x ] -> Format.fprintf f "is_int(%a)" arg x
    | Eq, [ x; y ] -> Format.fprintf f "%a === %a" arg x arg y
    | Neq, [ x; y ] -> Format.fprintf f "!(%a === %a)" arg x arg y
    | Lt, [ x; y ] -> Format.fprintf f "%a < %a" arg x arg y
    | Le, [ x; y ] -> Format.fprintf f "%a <= %a" arg x arg y
    | Ult, [ x; y ] -> Format.fprintf f "%a <= %a" arg x arg y
    | _ -> assert false

  let expr f e =
    match e with
    | Apply { f = g; args; exact } ->
        if exact
        then Format.fprintf f "%a!(%a)" Var.print g var_list args
        else Format.fprintf f "%a(%a)" Var.print g var_list args
    | Block (t, a, _) ->
        Format.fprintf f "{tag=%d" t;
        for i = 0 to Array.length a - 1 do
          Format.fprintf f "; %d = %a" i Var.print a.(i)
        done;
        Format.fprintf f "}"
    | Field (x, i) -> Format.fprintf f "%a[%d]" Var.print x i
    | Closure (l, c) -> Format.fprintf f "fun(%a){%a}" var_list l cont c
    | Constant c -> Format.fprintf f "CONST{%a}" constant c
    | Prim (p, l) -> prim f p l

  let instr f (i, _loc) =
    match i with
    | Let (x, e) -> Format.fprintf f "%a = %a" Var.print x expr e
    | Assign (x, y) -> Format.fprintf f "(assign) %a = %a" Var.print x Var.print y
    | Set_field (x, i, y) -> Format.fprintf f "%a[%d] = %a" Var.print x i Var.print y
    | Offset_ref (x, i) -> Format.fprintf f "%a[0] += %d" Var.print x i
    | Array_set (x, y, z) ->
        Format.fprintf f "%a[%a] = %a" Var.print x Var.print y Var.print z

  let last f (l, _loc) =
    match l with
    | Return x -> Format.fprintf f "return %a" Var.print x
    | Raise (x, `Normal) -> Format.fprintf f "raise %a" Var.print x
    | Raise (x, `Reraise) -> Format.fprintf f "reraise %a" Var.print x
    | Raise (x, `Notrace) -> Format.fprintf f "raise_notrace %a" Var.print x
    | Stop -> Format.fprintf f "stop"
    | Branch c -> Format.fprintf f "branch %a" cont c
    | Cond (x, cont1, cont2) ->
        Format.fprintf f "if %a then %a else %a" Var.print x cont cont1 cont cont2
    | Switch (x, a1, a2) ->
        Format.fprintf f "switch %a {" Var.print x;
        Array.iteri a1 ~f:(fun i c -> Format.fprintf f "int %d -> %a; " i cont c);
        Array.iteri a2 ~f:(fun i c -> Format.fprintf f "tag %d -> %a; " i cont c);
        Format.fprintf f "}"
    | Pushtrap (cont1, x, cont2, pcs) ->
        Format.fprintf
          f
          "pushtrap %a handler %a => %a continuation %s"
          cont
          cont1
          Var.print
          x
          cont
          cont2
          (String.concat ~sep:", " (List.map (Addr.Set.elements pcs) ~f:string_of_int))
    | Poptrap c -> Format.fprintf f "poptrap %a" cont c

  type xinstr =
    | Instr of (instr * loc)
    | Last of (last * loc)

  let block annot pc block =
    Format.eprintf "==== %d (%a) ====@." pc var_list block.params;
    List.iter block.body ~f:(fun i ->
        Format.eprintf " %s %a@." (annot pc (Instr i)) instr i);
    Format.eprintf " %s %a@." (annot pc (Last block.branch)) last block.branch;
    Format.eprintf "@."

  let program annot { start; blocks; _ } =
    Format.eprintf "Entry point: %d@.@." start;
    Addr.Map.iter (block annot) blocks
end

(****)

let fold_closures p f accu =
  Addr.Map.fold
    (fun _ block accu ->
      List.fold_left block.body ~init:accu ~f:(fun accu (i, _loc) ->
          match i with
          | Let (x, Closure (params, cont)) -> f (Some x) params cont accu
          | _ -> accu))
    p.blocks
    (f None [] (p.start, []) accu)

(****)

let prepend ({ start; blocks; free_pc } as p) body =
  match body with
  | [] -> p
  | _ -> (
      match Addr.Map.find start blocks with
      | block ->
          { p with
            blocks = Addr.Map.add start { block with body = body @ block.body } blocks
          }
      | exception Not_found ->
          let new_start = free_pc in
          let blocks =
            Addr.Map.add new_start { params = []; body; branch = Stop, noloc } blocks
          in
          let free_pc = free_pc + 1 in
          { start = new_start; blocks; free_pc })

let empty_block = { params = []; body = []; branch = Stop, noloc }

let empty =
  let start = 0 in
  let blocks = Addr.Map.singleton start empty_block in
  { start; blocks; free_pc = start + 1 }

let is_empty p =
  match Addr.Map.cardinal p.blocks with
  | 0 -> true
  | 1 -> (
      let _, v = Addr.Map.choose p.blocks in
      match v with
      | { body; branch = Stop, _; params = _ } -> (
          match body with
          | ([] | [ (Let (_, Prim (Extern "caml_get_global_data", _)), _) ]) when true ->
              true
          | _ -> false)
      | _ -> false)
  | _ -> false

let fold_children blocks pc f accu =
  let block = Addr.Map.find pc blocks in
  match fst block.branch with
  | Return _ | Raise _ | Stop -> accu
  | Branch (pc', _) | Poptrap (pc', _) -> f pc' accu
  | Pushtrap ((pc', _), _, (pc_h, _), _) ->
      let accu = f pc' accu in
      let accu = f pc_h accu in
      accu
  | Cond (_, (pc1, _), (pc2, _)) ->
      let accu = f pc1 accu in
      let accu = f pc2 accu in
      accu
  | Switch (_, a1, a2) ->
      let accu = Array.fold_right ~init:accu ~f:(fun (pc, _) accu -> f pc accu) a1 in
      let accu = Array.fold_right ~init:accu ~f:(fun (pc, _) accu -> f pc accu) a2 in
      accu

type 'c fold_blocs = block Addr.Map.t -> Addr.t -> (Addr.t -> 'c -> 'c) -> 'c -> 'c

type fold_blocs_poly = { fold : 'a. 'a fold_blocs } [@@unboxed]

let rec traverse' { fold } f pc visited blocks acc =
  if not (Addr.Set.mem pc visited)
  then
    let visited = Addr.Set.add pc visited in
    let visited, acc =
      fold
        blocks
        pc
        (fun pc (visited, acc) ->
          let visited, acc = traverse' { fold } f pc visited blocks acc in
          visited, acc)
        (visited, acc)
    in
    let acc = f pc acc in
    visited, acc
  else visited, acc

let traverse fold f pc blocks acc = snd (traverse' fold f pc Addr.Set.empty blocks acc)

let rec preorder_traverse' { fold } f pc visited blocks acc =
  if not (Addr.Set.mem pc visited)
  then
    let visited = Addr.Set.add pc visited in
    let acc = f pc acc in
    fold
      blocks
      pc
      (fun pc (visited, acc) ->
        let visited, acc = preorder_traverse' { fold } f pc visited blocks acc in
        visited, acc)
      (visited, acc)
  else visited, acc

let preorder_traverse fold f pc blocks acc =
  snd (preorder_traverse' fold f pc Addr.Set.empty blocks acc)

let fold_closures_innermost_first { start; blocks; _ } f accu =
  let rec visit blocks pc f accu =
    traverse
      { fold = fold_children }
      (fun pc accu ->
        let block = Addr.Map.find pc blocks in
        List.fold_left block.body ~init:accu ~f:(fun accu i ->
            match i with
            | Let (x, Closure (params, cont)), _ ->
                let accu = visit blocks (fst cont) f accu in
                f (Some x) params cont accu
            | _ -> accu))
      pc
      blocks
      accu
  in
  let accu = visit blocks start f accu in
  f None [] (start, []) accu

let fold_closures_outermost_first { start; blocks; _ } f accu =
  let rec visit blocks pc f accu =
    traverse
      { fold = fold_children }
      (fun pc accu ->
        let block = Addr.Map.find pc blocks in
        List.fold_left block.body ~init:accu ~f:(fun accu i ->
            match i with
            | Let (x, Closure (params, cont)), _ ->
                let accu = f (Some x) params cont accu in
                visit blocks (fst cont) f accu
            | _ -> accu))
      pc
      blocks
      accu
  in
  let accu = f None [] (start, []) accu in
  visit blocks start f accu

let eq p1 p2 =
  p1.start = p2.start
  && Addr.Map.cardinal p1.blocks = Addr.Map.cardinal p2.blocks
  && Addr.Map.fold
       (fun pc block1 b ->
         b
         &&
         try
           let block2 = Addr.Map.find pc p2.blocks in
           Poly.(block1.params = block2.params)
           && Poly.(block1.branch = block2.branch)
           && Poly.(block1.body = block2.body)
         with Not_found -> false)
       p1.blocks
       true

let with_invariant = Debug.find "invariant"

let check_defs = false

let invariant { blocks; start; _ } =
  if with_invariant ()
  then (
    assert (Addr.Map.mem start blocks);
    let defs = Var.ISet.empty () in
    let check_cont (cont, args) =
      let b = Addr.Map.find cont blocks in
      assert (List.length args >= List.length b.params)
    in
    let define x =
      if check_defs
      then (
        assert (not (Var.ISet.mem defs x));
        Var.ISet.add defs x)
    in
    let check_expr = function
      | Apply _ -> ()
      | Block (_, _, _) -> ()
      | Field (_, _) -> ()
      | Closure (l, cont) ->
          List.iter l ~f:define;
          check_cont cont
      | Constant _ -> ()
      | Prim (_, _) -> ()
    in
    let check_instr (i, _loc) =
      match i with
      | Let (x, e) ->
          define x;
          check_expr e
      | Assign _ -> ()
      | Set_field (_, _i, _) -> ()
      | Offset_ref (_x, _i) -> ()
      | Array_set (_x, _y, _z) -> ()
    in
    let check_last (l, _loc) =
      match l with
      | Return _ -> ()
      | Raise _ -> ()
      | Stop -> ()
      | Branch cont -> check_cont cont
      | Cond (_x, cont1, cont2) ->
          check_cont cont1;
          check_cont cont2
      | Switch (_x, a1, a2) ->
          Array.iteri a1 ~f:(fun _ cont -> check_cont cont);
          Array.iteri a2 ~f:(fun _ cont -> check_cont cont)
      | Pushtrap (cont1, _x, cont2, _pcs) ->
          check_cont cont1;
          check_cont cont2
      | Poptrap cont -> check_cont cont
    in
    Addr.Map.iter
      (fun _pc block ->
        List.iter block.params ~f:define;
        List.iter block.body ~f:check_instr;
        check_last block.branch)
      blocks)
