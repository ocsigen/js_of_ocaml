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

(*XXX
  Patterns:
  => loops should avoid absorbing the whole continuation...
     (detect when the continuation does not loop anymore and close
      the loop at this point)
  => should have special code for switches that include the preceding
     if statement when possible
  => if e1 then {if e2 then P else Q} else {if e3 then P else Q}
  => if e then return e1; return e2
  => if e then var x = e1; else var x = e2;
  => while (true) {.... if (e) continue; break; }

  - CLEAN UP!!!
*)

open! Stdlib

let debug = Debug.find "gen"

let times = Debug.find "times"

open Code
module J = Javascript

(****)

let string_of_set s =
  String.concat ~sep:", " (List.map ~f:Addr.to_string (Addr.Set.elements s))

let rec list_group_rec f g l b m n =
  match l with
  | [] -> List.rev ((b, List.rev m) :: n)
  | a :: r ->
      let fa = f a in
      if Poly.(fa = b)
      then list_group_rec f g r b (g a :: m) n
      else list_group_rec f g r fa [ g a ] ((b, List.rev m) :: n)

let list_group f g l =
  match l with
  | [] -> []
  | a :: r -> list_group_rec f g r (f a) [ g a ] []

(****)

type application_description =
  { arity : int
  ; exact : bool
  ; cps : bool
  }

module Share = struct
  module AppMap = Map.Make (struct
    type t = application_description

    let compare = Poly.compare
  end)

  type 'a aux =
    { byte_strings : 'a StringMap.t
    ; utf_strings : 'a StringMap.t
    ; applies : 'a AppMap.t
    ; prims : 'a StringMap.t
    }

  let empty_aux =
    { prims = StringMap.empty
    ; byte_strings = StringMap.empty
    ; utf_strings = StringMap.empty
    ; applies = AppMap.empty
    }

  type t =
    { count : int aux
    ; mutable vars : J.ident aux
    ; alias_prims : bool
    ; alias_strings : bool
    ; alias_apply : bool
    }

  let add_byte_string s t =
    let n = try StringMap.find s t.byte_strings with Not_found -> 0 in
    { t with byte_strings = StringMap.add s (n + 1) t.byte_strings }

  let add_utf_string s t =
    let n = try StringMap.find s t.utf_strings with Not_found -> 0 in
    { t with utf_strings = StringMap.add s (n + 1) t.utf_strings }

  let add_prim s t =
    let n = try StringMap.find s t.prims with Not_found -> 0 in
    if n < 0 then t else { t with prims = StringMap.add s (n + 1) t.prims }

  let add_special_prim_if_exists s t =
    if Primitive.exists s then { t with prims = StringMap.add s (-1) t.prims } else t

  let add_apply i t =
    let n = try AppMap.find i t.applies with Not_found -> 0 in
    { t with applies = AppMap.add i (n + 1) t.applies }

  let add_code_string s share =
    let share =
      if String.is_ascii s then add_utf_string s share else add_byte_string s share
    in
    if Config.Flag.use_js_string ()
    then share
    else add_prim "caml_string_of_jsbytes" share

  let add_code_native_string (s : Code.Native_string.t) share =
    match s with
    | Utf (Utf8 s) -> add_utf_string s share
    | Byte s -> add_byte_string s share

  let rec get_constant c t =
    match c with
    | String s -> add_code_string s t
    | NativeString s -> add_code_native_string s t
    | Tuple (_, args, _) -> Array.fold_left args ~init:t ~f:(fun t c -> get_constant c t)
    | _ -> t

  let add_args args t =
    List.fold_left args ~init:t ~f:(fun t a ->
        match a with
        | Pc c -> get_constant c t
        | _ -> t)

  let get
      ~cps_calls
      ?alias_strings
      ?(alias_prims = false)
      ?(alias_apply = true)
      { blocks; _ } : t =
    let alias_strings =
      match alias_strings with
      | None -> Config.Flag.use_js_string () && not (Config.Flag.share_constant ())
      | Some x -> x
    in
    let count =
      Addr.Map.fold
        (fun _ block share ->
          List.fold_left block.body ~init:share ~f:(fun share (i, _) ->
              match i with
              | Let (_, Constant c) -> get_constant c share
              | Let (x, Apply { args; exact; _ }) ->
                  let cps = Var.Set.mem x cps_calls in
                  if (not exact) || cps
                  then add_apply { arity = List.length args; exact; cps } share
                  else share
              | Let (_, Prim (Extern "%closure", [ Pc (String name) ])) ->
                  let name = Primitive.resolve name in
                  let share =
                    if Primitive.exists name then add_prim name share else share
                  in
                  share
              | Let (_, Prim (Extern name, args)) ->
                  let name = Primitive.resolve name in
                  let share =
                    if Primitive.exists name then add_prim name share else share
                  in
                  add_args args share
              | Let (_, Prim (_, args)) -> add_args args share
              | _ -> share))
        blocks
        empty_aux
    in
    let count =
      List.fold_left
        [ "caml_trampoline"
        ; "caml_trampoline_return"
        ; "caml_wrap_exception"
        ; "caml_list_of_js_array"
        ; "caml_maybe_attach_backtrace"
        ; "jsoo_effect_not_supported"
        ]
        ~init:count
        ~f:(fun acc x -> add_special_prim_if_exists x acc)
    in
    { count; vars = empty_aux; alias_strings; alias_prims; alias_apply }

  let get_byte_string gen s t =
    if not t.alias_strings
    then gen s
    else
      try
        let c = StringMap.find s t.count.byte_strings in
        if c > 1
        then (
          try J.EVar (StringMap.find s t.vars.byte_strings)
          with Not_found ->
            let x = Var.fresh_n (Printf.sprintf "cst_%s" s) in
            let v = J.V x in
            t.vars <- { t.vars with byte_strings = StringMap.add s v t.vars.byte_strings };
            J.EVar v)
        else gen s
      with Not_found -> gen s

  let get_utf_string gen s t =
    if not t.alias_strings
    then gen s
    else
      try
        let c = StringMap.find s t.count.utf_strings in
        if c > 1
        then (
          try J.EVar (StringMap.find s t.vars.utf_strings)
          with Not_found ->
            let x = Var.fresh_n (Printf.sprintf "cst_%s" s) in
            let v = J.V x in
            t.vars <- { t.vars with utf_strings = StringMap.add s v t.vars.utf_strings };
            J.EVar v)
        else gen s
      with Not_found -> gen s

  let get_prim gen s t =
    let s = Primitive.resolve s in
    if not t.alias_prims
    then gen s
    else
      try
        let c = StringMap.find s t.count.prims in
        if c > 1 || c = -1
        then (
          try J.EVar (StringMap.find s t.vars.prims)
          with Not_found ->
            let x = Var.fresh_n s in
            let v = J.V x in
            t.vars <- { t.vars with prims = StringMap.add s v t.vars.prims };
            J.EVar v)
        else gen s
      with Not_found -> gen s

  let get_apply gen desc t =
    if not t.alias_apply
    then gen desc
    else
      try J.EVar (AppMap.find desc t.vars.applies)
      with Not_found ->
        let x =
          let { arity; exact; cps } = desc in
          Var.fresh_n
            (Printf.sprintf
               "caml_%scall%d"
               (match exact, cps with
               | true, false -> assert false
               | true, true -> "cps_exact_"
               | false, false -> ""
               | false, true -> "cps_")
               arity)
        in
        let v = J.V x in
        t.vars <- { t.vars with applies = AppMap.add desc v t.vars.applies };
        J.EVar v
end

module Ctx = struct
  type t =
    { blocks : block Addr.Map.t
    ; live : Deadcode.variable_uses
    ; share : Share.t
    ; debug : Parse_bytecode.Debug.t
    ; exported_runtime : (Code.Var.t * bool ref) option
    ; should_export : bool
    ; effect_warning : bool ref
    ; cps_calls : Effects.cps_calls
    }

  let initial
      ~warn_on_unhandled_effect
      ~exported_runtime
      ~should_export
      blocks
      live
      cps_calls
      share
      debug =
    { blocks
    ; live
    ; share
    ; debug
    ; exported_runtime
    ; should_export
    ; effect_warning = ref (not warn_on_unhandled_effect)
    ; cps_calls
    }
end

let var x = J.EVar (J.V x)

let int n = J.ENum (J.Num.of_int32 (Int32.of_int n))

let int32 n = J.ENum (J.Num.of_int32 n)

let to_int cx = J.EBin (J.Bor, cx, int 0)

let unsigned' x = J.EBin (J.Lsr, x, int 0)

let unsigned x =
  let x =
    match x with
    | J.EBin (J.Bor, x, J.ENum maybe_zero) when J.Num.is_zero maybe_zero -> x
    | _ -> x
  in
  let pos_int32 =
    match x with
    | J.ENum num -> ( try Int32.(J.Num.to_int32 num >= 0l) with _ -> false)
    | _ -> false
  in
  if pos_int32 then x else unsigned' x

let one = int 1

let zero = int 0

let plus_int x y =
  match x, y with
  | J.ENum y, x when J.Num.is_zero y -> x
  | x, J.ENum y when J.Num.is_zero y -> x
  | J.ENum x, J.ENum y -> J.ENum (J.Num.add x y)
  | x, y -> J.EBin (J.Plus, x, y)

let bool e = J.ECond (e, one, zero)

(****)

let source_location ctx ?force (pc : Code.loc) =
  match Parse_bytecode.Debug.find_loc ctx.Ctx.debug ?force pc with
  | Some pi -> J.Pi pi
  | None -> J.N

(****)

let float_const f = J.ENum (J.Num.of_float f)

let s_var name = J.EVar (J.ident (Utf8_string.of_string_exn name))

let runtime_fun ctx name =
  match ctx.Ctx.exported_runtime with
  | Some (runtime, runtime_needed) ->
      runtime_needed := true;
      let name = Utf8_string.of_string_exn name in
      J.dot (J.EVar (J.V runtime)) name
  | None -> s_var name

let str_js_byte s =
  let b = Buffer.create (String.length s) in
  String.iter s ~f:(function
      | '\\' -> Buffer.add_string b "\\\\"
      | '\128' .. '\255' as c ->
          Buffer.add_string b "\\x";
          Buffer.add_char_hex b c
      | c -> Buffer.add_char b c);
  let s = Buffer.contents b in
  J.EStr (Utf8_string.of_string_exn s)

let str_js_utf8 s =
  let b = Buffer.create (String.length s) in
  String.iter s ~f:(function
      | '\\' -> Buffer.add_string b "\\\\"
      | c -> Buffer.add_char b c);
  let s = Buffer.contents b in
  J.EStr (Utf8_string.of_string_exn s)

(****)

(*
Some variables are constant:   x = 1
Some may change after effectful operations : x = y[z]

There can be at most one effectful operations in the queue at once

let (e, expr_queue) = ... in
flush_queue expr_queue e
*)

let const_p = 0, Var.Set.empty

let mutable_p = 1, Var.Set.empty

let mutator_p = 2, Var.Set.empty

let flush_p = 3, Var.Set.empty

let or_p (p, s1) (q, s2) = max p q, Var.Set.union s1 s2

let is_mutable (p, _) = p >= fst mutable_p

let kind k =
  match k with
  | `Pure -> const_p
  | `Mutable -> mutable_p
  | `Mutator -> mutator_p

let ocaml_string ~ctx ~loc s =
  if Config.Flag.use_js_string ()
  then s
  else
    let p = Share.get_prim (runtime_fun ctx) "caml_string_of_jsbytes" ctx.Ctx.share in
    J.call p [ s ] loc

let rec constant_rec ~ctx x level instrs =
  match x with
  | String s ->
      let e =
        if String.is_ascii s
        then Share.get_utf_string str_js_byte s ctx.Ctx.share
        else Share.get_byte_string str_js_byte s ctx.Ctx.share
      in
      let e = ocaml_string ~ctx ~loc:J.N e in
      e, instrs
  | NativeString s -> (
      match s with
      | Byte x -> Share.get_byte_string str_js_byte x ctx.Ctx.share, instrs
      | Utf (Utf8 x) -> Share.get_utf_string str_js_utf8 x ctx.Ctx.share, instrs)
  | Float f -> float_const f, instrs
  | Float_array a ->
      ( Mlvalue.Array.make
          ~tag:Obj.double_array_tag
          ~args:(Array.to_list (Array.map a ~f:float_const))
      , instrs )
  | Int64 i ->
      let p =
        Share.get_prim (runtime_fun ctx) "caml_int64_create_lo_mi_hi" ctx.Ctx.share
      in
      let lo = int (Int64.to_int i land 0xffffff)
      and mi = int (Int64.to_int (Int64.shift_right i 24) land 0xffffff)
      and hi = int (Int64.to_int (Int64.shift_right i 48) land 0xffff) in
      J.call p [ lo; mi; hi ] J.N, instrs
  | Tuple (tag, a, _) -> (
      let constant_max_depth = Config.Param.constant_max_depth () in
      let rec detect_list n acc = function
        | Tuple (0, [| x; l |], _) -> detect_list (succ n) (x :: acc) l
        | Int (_, 0l) -> if n > constant_max_depth then Some acc else None
        | _ -> None
      in
      match detect_list 0 [] x with
      | Some elts_rev ->
          let elements, instrs =
            List.fold_left elts_rev ~init:([], instrs) ~f:(fun (arr, instrs) elt ->
                let js, instrs = constant_rec ~ctx elt level instrs in
                js :: arr, instrs)
          in
          let p =
            Share.get_prim (runtime_fun ctx) "caml_list_of_js_array" ctx.Ctx.share
          in
          J.call p [ J.array elements ] J.N, instrs
      | None ->
          let split = level = constant_max_depth in
          let level = if split then 0 else level + 1 in
          let l, instrs =
            List.fold_left (Array.to_list a) ~init:([], instrs) ~f:(fun (l, instrs) cc ->
                let js, instrs = constant_rec ~ctx cc level instrs in
                js :: l, instrs)
          in
          let l, instrs =
            if split
            then
              List.fold_left l ~init:([], instrs) ~f:(fun (acc, instrs) js ->
                  match js with
                  | J.EArr _ ->
                      let v = Code.Var.fresh_n "partial" in
                      let instrs =
                        (J.variable_declaration [ J.V v, (js, J.N) ], J.N) :: instrs
                      in
                      J.EVar (J.V v) :: acc, instrs
                  | _ -> js :: acc, instrs)
            else List.rev l, instrs
          in
          Mlvalue.Block.make ~tag ~args:l, instrs)
  | Int (_, i) -> int32 i, instrs

let constant ~ctx x level =
  let expr, instr = constant_rec ~ctx x level [] in
  expr, List.rev instr

type queue_elt =
  { prop : int
  ; cardinal : int
  ; ce : J.expression
  ; loc : J.location
  ; deps : Code.Var.Set.t
  }

let access_queue queue x =
  try
    let elt = List.assoc x queue in
    if elt.cardinal = 1
    then ((elt.prop, elt.deps), elt.ce), List.remove_assoc x queue
    else
      ( ((elt.prop, elt.deps), elt.ce)
      , List.map queue ~f:(function
            | x', elt when Var.equal x x' -> x', { elt with cardinal = pred elt.cardinal }
            | x -> x) )
  with Not_found -> ((fst const_p, Code.Var.Set.singleton x), var x), queue

let access_queue' ~ctx queue x =
  match x with
  | Pc c ->
      let js, instrs = constant ~ctx c (Config.Param.constant_max_depth ()) in
      assert (List.is_empty instrs);
      (* We only have simple constants here *)
      (const_p, js), queue
  | Pv x -> access_queue queue x

let access_queue_may_flush queue v x =
  let tx, queue = access_queue queue x in
  let _, instrs, queue =
    List.fold_left
      queue
      ~init:(Code.Var.Set.singleton v, [], [])
      ~f:(fun (deps, instrs, queue) ((y, elt) as eq) ->
        if not (Code.Var.Set.disjoint deps elt.deps)
        then
          ( Code.Var.Set.add y deps
          , (J.variable_declaration [ J.V y, (elt.ce, elt.loc) ], elt.loc) :: instrs
          , queue )
        else deps, instrs, eq :: queue)
  in
  instrs, (tx, List.rev queue)

let should_flush (cond, _) prop = cond <> fst const_p && cond + prop >= fst flush_p

let flush_queue expr_queue prop (l : J.statement_list) =
  let instrs, expr_queue =
    if fst prop >= fst flush_p
    then expr_queue, []
    else List.partition ~f:(fun (_, elt) -> should_flush prop elt.prop) expr_queue
  in
  let instrs =
    List.map instrs ~f:(fun (x, elt) ->
        J.variable_declaration [ J.V x, (elt.ce, elt.loc) ], elt.loc)
  in
  List.rev_append instrs l, expr_queue

let flush_all expr_queue l = fst (flush_queue expr_queue flush_p l)

let enqueue expr_queue prop x ce loc cardinal acc =
  let instrs, expr_queue =
    if Config.Flag.compact ()
    then if is_mutable prop then flush_queue expr_queue prop [] else [], expr_queue
    else flush_queue expr_queue flush_p []
  in
  let prop, deps = prop in
  let deps =
    List.fold_left expr_queue ~init:deps ~f:(fun deps (x', elt) ->
        if Code.Var.Set.mem x' deps then Code.Var.Set.union elt.deps deps else deps)
  in
  instrs @ acc, (x, { prop; deps; ce; loc; cardinal }) :: expr_queue

(****)

module Interm : sig
  type elt =
    { pc : Addr.t
    ; var : Var.t
    ; value : int
    ; default : bool
    }

  type t

  val empty : t

  val mem : Addr.t -> t -> bool

  val find : Addr.t -> t -> elt

  val add : t -> idx:Addr.t -> var:Var.t -> (Addr.t * int * bool) list -> t

  val resolve_nodes : t -> Addr.Set.t -> Addr.Set.t
end = struct
  type elt =
    { pc : Addr.t
    ; var : Var.t
    ; value : int
    ; default : bool
    }

  type t = elt Addr.Map.t

  let empty = Addr.Map.empty

  let mem pc t = Addr.Map.mem pc t

  let find pc t = Addr.Map.find pc t

  let add t ~idx ~var members =
    List.fold_left members ~init:t ~f:(fun acc (pc, value, default) ->
        Addr.Map.add pc { pc = idx; var; value; default } acc)

  let rec resolve_node interm pc =
    try
      let int = find pc interm in
      resolve_node interm int.pc
    with Not_found -> pc

  let resolve_nodes interm s =
    Addr.Set.fold (fun pc s' -> Addr.Set.add (resolve_node interm pc) s') s Addr.Set.empty
end

type state =
  { succs :
      (Addr.t, int list) Hashtbl.t (* List of forward successors for a given block *)
  ; backs : (Addr.t, Addr.Set.t) Hashtbl.t (* Set of back edges for a given block *)
  ; preds : (Addr.t, int) Hashtbl.t (* Number of predecessors for a given block *)
  ; seen : (Addr.t, int) Hashtbl.t
        (* For blocks that are member of a frontier, it's the number of predecessor already compiled *)
  ; loops : Addr.Set.t
        (* Set of blocks that are start a loop / have incoming back edges *)
  ; visited_blocks : Addr.Set.t ref
  ; dominance_frontier_cache : (Addr.t, int Addr.Map.t) Hashtbl.t
        (* dominance_frontier of a block. The frontier is a map containing number of edges to each member of the frontier. *)
  ; last_interm_idx : int ref
  ; ctx : Ctx.t
  ; blocks : Code.block Addr.Map.t
  }

let get_preds st pc = Hashtbl.find st.preds pc

let get_succs st pc = Hashtbl.find st.succs pc

let get_seen st pc = try Hashtbl.find st.seen pc with Not_found -> 0

let incr_seen st pc = Hashtbl.replace st.seen pc (get_seen st pc + 1)

module DTree = struct
  (* This as to be kept in sync with the way we build conditionals
     and switches! *)

  type cond =
    | IsTrue
    | CEq of int32
    | CLt of int32
    | CLe of int32

  type 'a t =
    | If of cond * 'a t * 'a t
    | Switch of (int list * 'a t) array
    | Branch of 'a
    | Empty

  let normalize a =
    a
    |> Array.to_list
    |> List.sort ~cmp:(fun (cont1, _) (cont2, _) -> Poly.compare cont1 cont2)
    |> list_group fst snd
    |> List.map ~f:(fun (cont1, l1) -> cont1, List.flatten l1)
    |> List.sort ~cmp:(fun (_, l1) (_, l2) -> compare (List.length l1) (List.length l2))
    |> Array.of_list

  let build_if b1 b2 = If (IsTrue, Branch b1, Branch b2)

  let build_switch (a : cont array) : 'a t =
    let m = Config.Param.switch_max_case () in
    let ai = Array.mapi a ~f:(fun i x -> x, i) in
    (* group the contiguous cases with the same continuation *)
    let ai : (Code.cont * int list) array =
      Array.of_list (list_group fst snd (Array.to_list ai))
    in
    let rec loop low up =
      let array_norm : (Code.cont * int list) array =
        normalize (Array.sub ai ~pos:low ~len:(up - low + 1))
      in
      let array_len = Array.length array_norm in
      if array_len = 1 (* remaining cases all jump to the same branch *)
      then Branch (fst array_norm.(0))
      else
        try
          (* try to optimize when there are only 2 branch *)
          match array_norm with
          | [| (b1, [ i1 ]); (b2, _l2) |] ->
              If (CEq (Int32.of_int i1), Branch b1, Branch b2)
          | [| (b1, _l1); (b2, [ i2 ]) |] ->
              If (CEq (Int32.of_int i2), Branch b2, Branch b1)
          | [| (b1, l1); (b2, l2) |] ->
              let bound l1 =
                match l1, List.rev l1 with
                | min :: _, max :: _ -> min, max
                | _ -> assert false
              in
              let min1, max1 = bound l1 in
              let min2, max2 = bound l2 in
              if max1 < min2
              then If (CLt (Int32.of_int max1), Branch b2, Branch b1)
              else if max2 < min1
              then If (CLt (Int32.of_int max2), Branch b1, Branch b2)
              else raise Not_found
          | _ -> raise Not_found
        with Not_found -> (
          (* do we have to split again ? *)
          (* we count the number of cases, default/last case count for one *)
          let nbcases = ref 1 (* default case *) in
          for i = 0 to array_len - 2 do
            nbcases := !nbcases + List.length (snd array_norm.(i))
          done;
          if !nbcases <= m
          then Switch (Array.map array_norm ~f:(fun (x, l) -> l, Branch x))
          else
            let h = (up + low) / 2 in
            let b1 = loop low h and b2 = loop (succ h) up in
            let range1 = snd ai.(h) and range2 = snd ai.(succ h) in
            match range1, range2 with
            | [], _ | _, [] -> assert false
            | _, lower_bound2 :: _ -> If (CLe (Int32.of_int lower_bound2), b2, b1))
    in
    let len = Array.length ai in
    if len = 0 then Empty else loop 0 (len - 1)

  let rec fold_cont f b acc =
    match b with
    | If (_, b1, b2) ->
        let acc = fold_cont f b1 acc in
        let acc = fold_cont f b2 acc in
        acc
    | Switch a -> Array.fold_left a ~init:acc ~f:(fun acc (_, b) -> fold_cont f b acc)
    | Branch (pc, _) -> f pc acc
    | Empty -> acc

  let nbcomp a =
    let rec loop c = function
      | Empty -> c
      | Branch _ -> c
      | If (_, a, b) ->
          let c = succ c in
          let c = loop c a in
          let c = loop c b in
          c
      | Switch a ->
          let c = succ c in
          Array.fold_left a ~init:c ~f:(fun acc (_, b) -> loop acc b)
    in
    loop 0 a
end

let fold_children blocks pc f accu =
  let block = Addr.Map.find pc blocks in
  match fst block.branch with
  | Return _ | Raise _ | Stop -> accu
  | Branch (pc', _) | Poptrap (pc', _) -> f pc' accu
  | Pushtrap ((pc1, _), _, (pc2, _), _) ->
      let accu = f pc1 accu in
      let accu = f pc2 accu in
      accu
  | Cond (_, cont1, cont2) -> DTree.fold_cont f (DTree.build_if cont1 cont2) accu
  | Switch (_, a1, a2) ->
      let a1 = DTree.build_switch a1 and a2 = DTree.build_switch a2 in
      let accu = DTree.fold_cont f a1 accu in
      let accu = DTree.fold_cont f a2 accu in
      accu

let build_graph ctx pc =
  let visited_blocks = ref Addr.Set.empty in
  let loops = ref Addr.Set.empty in
  let succs = Hashtbl.create 17 in
  let poptrap = Hashtbl.create 17 in
  let backs = Hashtbl.create 17 in
  let preds = Hashtbl.create 17 in
  let seen = Hashtbl.create 17 in
  let blocks = ctx.Ctx.blocks in
  let dominance_frontier_cache = Hashtbl.create 17 in
  let incr_prec pc =
    match Hashtbl.find preds pc with
    | exception Not_found -> Hashtbl.add preds pc 1
    | n -> Hashtbl.replace preds pc (succ n)
  in
  let add_cf_frontier pc front =
    let prev = Hashtbl.find succs pc in
    Addr.Set.iter incr_prec front;
    Hashtbl.replace succs pc (Addr.Set.elements front @ prev)
  in
  let rec loop pc anc pushtrap =
    if not (Addr.Set.mem pc !visited_blocks)
    then (
      visited_blocks := Addr.Set.add pc !visited_blocks;
      let anc = Addr.Set.add pc anc in
      let s = Code.fold_children blocks pc Addr.Set.add Addr.Set.empty in
      let pc_backs = Addr.Set.inter s anc in
      Hashtbl.add backs pc pc_backs;
      let s = fold_children blocks pc (fun x l -> x :: l) [] in
      let pc_succs = List.filter s ~f:(fun pc -> not (Addr.Set.mem pc anc)) in
      let b = Addr.Map.find pc blocks in
      Hashtbl.add succs pc pc_succs;
      Addr.Set.iter (fun pc' -> loops := Addr.Set.add pc' !loops) pc_backs;
      List.iter pc_succs ~f:(fun pc' ->
          let pushtrap =
            match fst b.branch with
            | Pushtrap ((pc1, _), _, (pc2, _), _remove) ->
                if pc' = pc1
                then (
                  Hashtbl.add poptrap pc Addr.Set.empty;
                  pc :: pushtrap)
                else (
                  assert (pc' = pc2);
                  pushtrap)
            | Poptrap (pc1, _) -> (
                match pushtrap with
                | [] -> assert false
                | p :: rest ->
                    let old = Hashtbl.find poptrap p in
                    Hashtbl.replace poptrap p (Addr.Set.add pc1 old);
                    rest)
            | _ -> pushtrap
          in
          loop pc' anc pushtrap);
      List.iter pc_succs ~f:incr_prec)
  in
  loop pc Addr.Set.empty [];
  Hashtbl.add preds pc 1;
  let () =
    (* Create an artificial frontier when we pop an exception handler *)
    let rec keep_front pc =
      if Hashtbl.find preds pc > 1
      then (* already part of a merge node *) false
      else
        match Addr.Map.find pc blocks with
        | { body = []; branch = Return _, _; _ } -> false
        | { body = []; branch = Stop, _; _ } -> false
        | { body = []; branch = Branch (pc', _), _; _ } -> keep_front pc'
        | _ -> true
    in
    Hashtbl.iter
      (fun pc_pushtrap pc3 ->
        let pc3 = Addr.Set.filter keep_front pc3 in
        add_cf_frontier pc_pushtrap pc3)
      poptrap
  in
  { visited_blocks
  ; dominance_frontier_cache
  ; seen
  ; loops = !loops
  ; succs
  ; backs
  ; preds
  ; last_interm_idx = ref (-1)
  ; ctx
  ; blocks
  }

let rec frontier_of_pc st pc =
  match Hashtbl.find st.dominance_frontier_cache pc with
  | d -> d
  | exception Not_found ->
      let visited = frontier_of_succs st (get_succs st pc) in
      Hashtbl.add st.dominance_frontier_cache pc visited;
      visited

and frontier_of_succs st succs =
  let visited = ref Addr.Map.empty in
  let q = Queue.create () in
  let incr pc n = Queue.add (Addr.Map.singleton pc n) q in
  List.iter succs ~f:(fun pc -> incr pc 1);
  while not (Queue.is_empty q) do
    visited :=
      Addr.Map.merge
        (fun k a b ->
          let sum = Option.value ~default:0 a + Option.value ~default:0 b in
          if get_preds st k = sum
          then (
            Queue.add (frontier_of_pc st k) q;
            None)
          else Some sum)
        !visited
        (Queue.take q)
  done;
  !visited

(* [seen] can be used to specify how many predecessor have been
   handled already. It is used when compiling merge_nodes. *)
let dominance_frontier ?(seen = 1) st pc =
  let pred = get_preds st pc in
  assert (pred >= seen);
  if pred > seen
  then Addr.Set.singleton pc
  else
    (* pred = seen *)
    let grey = frontier_of_pc st pc in
    Addr.Map.fold (fun k _ acc -> Addr.Set.add k acc) grey Addr.Set.empty

(****)

let rec visit visited prev s m x l =
  if not (Var.Set.mem x visited)
  then
    let visited = Var.Set.add x visited in
    let y = Var.Map.find x m in
    if Code.Var.compare x y = 0
    then visited, None, l
    else if Var.Set.mem y prev
    then
      let t = Code.Var.fresh () in
      visited, Some (y, t), (x, t) :: l
    else if Var.Set.mem y s
    then
      let visited, aliases, l = visit visited (Var.Set.add x prev) s m y l in
      match aliases with
      | Some (a, b) when Code.Var.compare a x = 0 -> visited, None, (b, a) :: (x, y) :: l
      | _ -> visited, aliases, (x, y) :: l
    else visited, None, (x, y) :: l
  else visited, None, l

let visit_all params args =
  let m = Subst.build_mapping params args in
  let s = List.fold_left params ~init:Var.Set.empty ~f:(fun s x -> Var.Set.add x s) in
  let _, l =
    Var.Set.fold
      (fun x (visited, l) ->
        let visited, _, l = visit visited Var.Set.empty s m x l in
        visited, l)
      s
      (Var.Set.empty, [])
  in
  l

let parallel_renaming params args continuation queue =
  let l = List.rev (visit_all params args) in
  List.fold_left
    l
    ~f:(fun continuation (y, x) queue ->
      let instrs, ((px, cx), queue) = access_queue_may_flush queue y x in
      let st, queue =
        flush_queue
          queue
          px
          (instrs @ [ J.variable_declaration [ J.V y, (cx, J.N) ], J.N ])
      in
      let never, code = continuation queue in
      never, st @ code)
    ~init:continuation
    queue

(****)

let apply_fun_raw ctx f params exact cps =
  let n = List.length params in
  let apply_directly =
    (* Make sure we are performing a regular call, not a (slower)
       method call *)
    match f with
    | J.EAccess _ | J.EDot _ ->
        J.call (J.dot f (Utf8_string.of_string_exn "call")) (s_var "null" :: params) J.N
    | _ -> J.call f params J.N
  in
  let apply =
    (* We skip the arity check when we know that we have the right
       number of parameters, since this test is expensive. *)
    if exact
    then apply_directly
    else
      let l = Utf8_string.of_string_exn "l" in
      J.ECond
        ( J.EBin
            ( J.EqEq
            , J.ECond
                ( J.EBin (J.Ge, J.dot f l, int 0)
                , J.dot f l
                , J.EBin (J.Eq, J.dot f l, J.dot f (Utf8_string.of_string_exn "length"))
                )
            , int n )
        , apply_directly
        , J.call (runtime_fun ctx "caml_call_gen") [ f; J.array params ] J.N )
  in
  if cps
  then (
    assert (Config.Flag.effects ());
    (* When supporting effect, we systematically perform tailcall
       optimization. To implement it, we check the stack depth and
       bounce to a trampoline if needed, to avoid a stack overflow.
       The trampoline then performs the call in an shorter stack. *)
    J.ECond
      ( J.call (runtime_fun ctx "caml_stack_check_depth") [] J.N
      , apply
      , J.call (runtime_fun ctx "caml_trampoline_return") [ f; J.array params ] J.N ))
  else apply

let generate_apply_fun ctx { arity; exact; cps } =
  let f' = Var.fresh_n "f" in
  let f = J.V f' in
  let params =
    Array.to_list
      (Array.init arity ~f:(fun i ->
           let a = Var.fresh_n (Printf.sprintf "a%d" i) in
           J.V a))
  in
  let f' = J.EVar f in
  let params' = List.map params ~f:(fun x -> J.EVar x) in
  J.EFun
    ( None
    , J.fun_
        (f :: params)
        [ J.Return_statement (Some (apply_fun_raw ctx f' params' exact cps)), J.N ]
        J.N )

let apply_fun ctx f params exact cps loc =
  (* We always go through an intermediate function when doing CPS
     calls. This function first checks the stack depth to prevent
     a stack overflow. This makes the code smaller than inlining
     the test, and we expect the performance impact to be low
     since the function should get inlined by the JavaScript
     engines. *)
  if Config.Flag.inline_callgen () || (exact && not cps)
  then apply_fun_raw ctx f params exact cps
  else
    let y =
      Share.get_apply
        (generate_apply_fun ctx)
        { arity = List.length params; exact; cps }
        ctx.Ctx.share
    in
    J.call y (f :: params) loc

(****)

let internal_primitives = Hashtbl.create 31

let internal_prim name =
  try
    let _, f = Hashtbl.find internal_primitives name in
    Some f
  with Not_found -> None

let register_prim name k f = Hashtbl.add internal_primitives name (k, f)

let invalid_arity name l ~loc ~expected =
  failwith
    (Printf.sprintf
       "%sInvalid arity for primitive %s. Expecting %d but used with %d."
       (match (loc : J.location) with
       | Pi { name = Some name; col; line; _ } ->
           Printf.sprintf "%s:%d:%d: " name line col
       | Pi _ | N | U -> "")
       name
       expected
       (List.length l))

let register_un_prim name k f =
  register_prim name k (fun l queue ctx loc ->
      match l with
      | [ x ] ->
          let (px, cx), queue = access_queue' ~ctx queue x in
          f cx loc, or_p (kind k) px, queue
      | l -> invalid_arity name l ~loc ~expected:1)

let register_un_prim_ctx name k f =
  register_prim name k (fun l queue ctx loc ->
      match l with
      | [ x ] ->
          let (px, cx), queue = access_queue' ~ctx queue x in
          f ctx cx loc, or_p (kind k) px, queue
      | _ -> invalid_arity name l ~loc ~expected:1)

let register_bin_prim name k f =
  register_prim name k (fun l queue ctx loc ->
      match l with
      | [ x; y ] ->
          let (px, cx), queue = access_queue' ~ctx queue x in
          let (py, cy), queue = access_queue' ~ctx queue y in
          f cx cy loc, or_p (kind k) (or_p px py), queue
      | _ -> invalid_arity name l ~loc ~expected:2)

let register_tern_prim name f =
  register_prim name `Mutator (fun l queue ctx loc ->
      match l with
      | [ x; y; z ] ->
          let (px, cx), queue = access_queue' ~ctx queue x in
          let (py, cy), queue = access_queue' ~ctx queue y in
          let (pz, cz), queue = access_queue' ~ctx queue z in
          f cx cy cz loc, or_p mutator_p (or_p px (or_p py pz)), queue
      | _ -> invalid_arity name l ~loc ~expected:3)

let register_un_math_prim name prim =
  let prim = Utf8_string.of_string_exn prim in
  register_un_prim name `Pure (fun cx loc ->
      J.call (J.dot (s_var "Math") prim) [ cx ] loc)

let register_bin_math_prim name prim =
  let prim = Utf8_string.of_string_exn prim in
  register_bin_prim name `Pure (fun cx cy loc ->
      J.call (J.dot (s_var "Math") prim) [ cx; cy ] loc)

let _ =
  register_un_prim_ctx "%caml_format_int_special" `Pure (fun ctx cx loc ->
      let s = J.EBin (J.Plus, str_js_utf8 "", cx) in
      ocaml_string ~ctx ~loc s);
  register_bin_prim "caml_array_unsafe_get" `Mutable (fun cx cy _ ->
      Mlvalue.Array.field cx cy);
  register_bin_prim "%int_add" `Pure (fun cx cy _ -> to_int (plus_int cx cy));
  register_bin_prim "%int_sub" `Pure (fun cx cy _ -> to_int (J.EBin (J.Minus, cx, cy)));
  register_bin_prim "%direct_int_mul" `Pure (fun cx cy _ ->
      to_int (J.EBin (J.Mul, cx, cy)));
  register_bin_prim "%direct_int_div" `Pure (fun cx cy _ ->
      to_int (J.EBin (J.Div, cx, cy)));
  register_bin_prim "%direct_int_mod" `Pure (fun cx cy _ ->
      to_int (J.EBin (J.Mod, cx, cy)));
  register_bin_prim "%int_and" `Pure (fun cx cy _ -> J.EBin (J.Band, cx, cy));
  register_bin_prim "%int_or" `Pure (fun cx cy _ -> J.EBin (J.Bor, cx, cy));
  register_bin_prim "%int_xor" `Pure (fun cx cy _ -> J.EBin (J.Bxor, cx, cy));
  register_bin_prim "%int_lsl" `Pure (fun cx cy _ -> J.EBin (J.Lsl, cx, cy));
  register_bin_prim "%int_lsr" `Pure (fun cx cy _ -> to_int (J.EBin (J.Lsr, cx, cy)));
  register_bin_prim "%int_asr" `Pure (fun cx cy _ -> J.EBin (J.Asr, cx, cy));
  register_un_prim "%int_neg" `Pure (fun cx _ -> to_int (J.EUn (J.Neg, cx)));
  register_bin_prim "caml_eq_float" `Pure (fun cx cy _ -> bool (J.EBin (J.EqEq, cx, cy)));
  register_bin_prim "caml_neq_float" `Pure (fun cx cy _ ->
      bool (J.EBin (J.NotEq, cx, cy)));
  register_bin_prim "caml_ge_float" `Pure (fun cx cy _ -> bool (J.EBin (J.Le, cy, cx)));
  register_bin_prim "caml_le_float" `Pure (fun cx cy _ -> bool (J.EBin (J.Le, cx, cy)));
  register_bin_prim "caml_gt_float" `Pure (fun cx cy _ -> bool (J.EBin (J.Lt, cy, cx)));
  register_bin_prim "caml_lt_float" `Pure (fun cx cy _ -> bool (J.EBin (J.Lt, cx, cy)));
  register_bin_prim "caml_add_float" `Pure (fun cx cy _ -> J.EBin (J.Plus, cx, cy));
  register_bin_prim "caml_sub_float" `Pure (fun cx cy _ -> J.EBin (J.Minus, cx, cy));
  register_bin_prim "caml_mul_float" `Pure (fun cx cy _ -> J.EBin (J.Mul, cx, cy));
  register_bin_prim "caml_div_float" `Pure (fun cx cy _ -> J.EBin (J.Div, cx, cy));
  register_un_prim "caml_neg_float" `Pure (fun cx _ -> J.EUn (J.Neg, cx));
  register_bin_prim "caml_fmod_float" `Pure (fun cx cy _ -> J.EBin (J.Mod, cx, cy));
  register_tern_prim "caml_array_unsafe_set" (fun cx cy cz _ ->
      J.EBin (J.Eq, Mlvalue.Array.field cx cy, cz));
  register_un_prim "caml_alloc_dummy" `Pure (fun _ _ -> J.array []);
  register_un_prim "caml_obj_dup" `Mutable (fun cx loc ->
      J.call (J.dot cx (Utf8_string.of_string_exn "slice")) [] loc);
  register_un_prim "caml_int_of_float" `Pure (fun cx _loc -> to_int cx);
  register_un_math_prim "caml_abs_float" "abs";
  register_un_math_prim "caml_acos_float" "acos";
  register_un_math_prim "caml_asin_float" "asin";
  register_un_math_prim "caml_atan_float" "atan";
  register_bin_math_prim "caml_atan2_float" "atan2";
  register_un_math_prim "caml_ceil_float" "ceil";
  register_un_math_prim "caml_cos_float" "cos";
  register_un_math_prim "caml_exp_float" "exp";
  register_un_math_prim "caml_floor_float" "floor";
  register_un_math_prim "caml_log_float" "log";
  register_bin_math_prim "caml_power_float" "pow";
  register_un_math_prim "caml_sin_float" "sin";
  register_un_math_prim "caml_sqrt_float" "sqrt";
  register_un_math_prim "caml_tan_float" "tan";
  register_un_prim "caml_js_from_bool" `Pure (fun cx _ ->
      J.EUn (J.Not, J.EUn (J.Not, cx)));
  register_un_prim "caml_js_to_bool" `Pure (fun cx _ -> to_int cx);

  register_tern_prim "caml_js_set" (fun cx cy cz _ ->
      J.EBin (J.Eq, J.EAccess (cx, ANormal, cy), cz));
  (* [caml_js_get] can have side effect, we declare it as mutator.
     see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions/get *)
  register_bin_prim "caml_js_get" `Mutator (fun cx cy _ -> J.EAccess (cx, ANormal, cy));
  register_bin_prim "caml_js_delete" `Mutator (fun cx cy _ ->
      J.EUn (J.Delete, J.EAccess (cx, ANormal, cy)));
  register_bin_prim "caml_js_equals" `Mutable (fun cx cy _ ->
      bool (J.EBin (J.EqEq, cx, cy)));
  register_bin_prim "caml_js_strict_equals" `Mutable (fun cx cy _ ->
      bool (J.EBin (J.EqEqEq, cx, cy)));
  register_bin_prim "caml_js_instanceof" `Mutator (fun cx cy _ ->
      bool (J.EBin (J.InstanceOf, cx, cy)));
  register_un_prim "caml_js_typeof" `Mutator (fun cx _ -> J.EUn (J.Typeof, cx))

(* This is not correct when switching the js-string flag *)
(* {[
    register_un_prim "caml_jsstring_of_string" `Mutable (fun cx loc ->
      J.ECall (J.EDot (cx, "toString"), [], loc));
    register_bin_prim "caml_string_notequal" `Pure (fun cx cy _ ->
      J.EBin (J.NotEqEq, cx, cy));
    register_bin_prim "caml_string_equal" `Pure (fun cx cy _ ->
      bool (J.EBin (J.EqEq, cx, cy)))
     ]}
*)

(****)
(* when raising ocaml exception and [improved_stacktrace] is enabled,
   tag the ocaml exception with a Javascript error (that contain js stacktrace).
   {[ throw e ]}
   becomes
   {[ throw (caml_exn_with_js_backtrace(e,false)) ]}
*)
let throw_statement ctx cx k loc =
  match (k : [ `Normal | `Reraise | `Notrace ]) with
  | `Notrace -> [ J.Throw_statement cx, loc ]
  | (`Normal | `Reraise) as m ->
      let force =
        match m with
        | `Normal -> true
        | `Reraise -> false
      in
      [ ( J.Throw_statement
            (J.call
               (Share.get_prim (runtime_fun ctx) "caml_maybe_attach_backtrace" ctx.share)
               [ cx; (if force then int 1 else int 0) ]
               loc)
        , loc )
      ]

let rec translate_expr ctx queue loc x e level : _ * J.statement_list =
  match e with
  | Apply { f; args; exact } ->
      let cps = Var.Set.mem x ctx.Ctx.cps_calls in
      let args, prop, queue =
        List.fold_right
          ~f:(fun x (args, prop, queue) ->
            let (prop', cx), queue = access_queue queue x in
            cx :: args, or_p prop prop', queue)
          args
          ~init:([], mutator_p, queue)
      in
      let (prop', f), queue = access_queue queue f in
      let prop = or_p prop prop' in
      let e = apply_fun ctx f args exact cps loc in
      (e, prop, queue), []
  | Block (tag, a, array_or_not) ->
      let contents, prop, queue =
        List.fold_right
          ~f:(fun x (args, prop, queue) ->
            let (prop', cx), queue = access_queue queue x in
            cx :: args, or_p prop prop', queue)
          (Array.to_list a)
          ~init:([], const_p, queue)
      in
      let x =
        match array_or_not with
        | Array -> Mlvalue.Array.make ~tag ~args:contents
        | NotArray | Unknown -> Mlvalue.Block.make ~tag ~args:contents
      in
      (x, prop, queue), []
  | Field (x, n) ->
      let (px, cx), queue = access_queue queue x in
      (Mlvalue.Block.field cx n, or_p px mutable_p, queue), []
  | Closure (args, ((pc, _) as cont)) ->
      let loc = source_location ctx ~force:After (After pc) in
      let clo = compile_closure ctx cont in
      let clo =
        match clo with
        | (st, x) :: rem ->
            let loc =
              match x, source_location ctx (Before pc) with
              | (J.U | J.N), (J.U | J.N) -> J.U
              | x, (J.U | J.N) -> x
              | (J.U | J.N), x -> x
              | _, x -> x
            in
            (st, loc) :: rem
        | _ -> clo
      in
      let clo = J.EFun (None, J.fun_ (List.map args ~f:(fun v -> J.V v)) clo loc) in
      (clo, flush_p, queue), []
  | Constant c ->
      let js, instrs = constant ~ctx c level in
      (js, const_p, queue), instrs
  | Prim (Extern "debugger", _) ->
      let ins =
        if Config.Flag.debugger () then J.Debugger_statement else J.Empty_statement
      in
      (int 0, const_p, queue), [ ins, loc ]
  | Prim (p, l) ->
      let res =
        match p, l with
        | Vectlength, [ x ] ->
            let (px, cx), queue = access_queue' ~ctx queue x in
            Mlvalue.Array.length cx, px, queue
        | Array_get, [ x; y ] ->
            let (px, cx), queue = access_queue' ~ctx queue x in
            let (py, cy), queue = access_queue' ~ctx queue y in
            Mlvalue.Array.field cx cy, or_p mutable_p (or_p px py), queue
        | Extern "caml_js_var", [ Pc (String nm) ]
        | Extern ("caml_js_expr" | "caml_pure_js_expr"), [ Pc (String nm) ] -> (
            try
              let pos =
                match loc with
                | J.N | J.U -> None
                | J.Pi pi -> (
                    (* [pi] is the position of the call, not the
                       string.  We don't have enough information to
                       recover the start column *)
                    match pi.src with
                    | Some pos_fname ->
                        Some
                          { Lexing.pos_fname
                          ; pos_lnum = pi.line
                          ; pos_cnum = pi.idx
                          ; pos_bol = pi.idx
                          }
                    | None -> None)
              in
              let lex = Parse_js.Lexer.of_string ?pos nm in
              let e = Parse_js.parse_expr lex in
              e, const_p, queue
            with Parse_js.Parsing_error pi ->
              failwith
                (Printf.sprintf
                   "Parsing error %S%s at l:%d col:%d"
                   nm
                   (match pi.Parse_info.src with
                   | None -> ""
                   | Some s -> Printf.sprintf ", file %S" s)
                   pi.Parse_info.line
                   pi.Parse_info.col))
        | Extern "%js_array", l ->
            let args, prop, queue =
              List.fold_right
                ~f:(fun x (args, prop, queue) ->
                  let (prop', cx), queue = access_queue' ~ctx queue x in
                  cx :: args, or_p prop prop', queue)
                l
                ~init:([], const_p, queue)
            in
            J.array args, prop, queue
        | Extern "%closure", [ Pc (String name) ] ->
            let prim = Share.get_prim (runtime_fun ctx) name ctx.Ctx.share in
            prim, const_p, queue
        | Extern "%closure", _ -> assert false
        | Extern "%caml_js_opt_call", f :: o :: l ->
            let (pf, cf), queue = access_queue' ~ctx queue f in
            let (po, co), queue = access_queue' ~ctx queue o in
            let args, prop, queue =
              List.fold_right
                ~f:(fun x (args, prop, queue) ->
                  let (prop', cx), queue = access_queue' ~ctx queue x in
                  cx :: args, or_p prop prop', queue)
                l
                ~init:([], mutator_p, queue)
            in
            ( J.call (J.dot cf (Utf8_string.of_string_exn "call")) (co :: args) loc
            , or_p (or_p pf po) prop
            , queue )
        | Extern "%caml_js_opt_fun_call", f :: l ->
            let (pf, cf), queue = access_queue' ~ctx queue f in
            let args, prop, queue =
              List.fold_right
                ~f:(fun x (args, prop, queue) ->
                  let (prop', cx), queue = access_queue' ~ctx queue x in
                  cx :: args, or_p prop prop', queue)
                l
                ~init:([], mutator_p, queue)
            in
            J.call cf args loc, or_p pf prop, queue
        | Extern "%caml_js_opt_meth_call", o :: Pc (NativeString (Utf m)) :: l ->
            let (po, co), queue = access_queue' ~ctx queue o in
            let args, prop, queue =
              List.fold_right
                ~f:(fun x (args, prop, queue) ->
                  let (prop', cx), queue = access_queue' ~ctx queue x in
                  cx :: args, or_p prop prop', queue)
                l
                ~init:([], mutator_p, queue)
            in
            J.call (J.dot co m) args loc, or_p po prop, queue
        | Extern "%caml_js_opt_meth_call", _ -> assert false
        | Extern "%caml_js_opt_new", c :: l ->
            let (pc, cc), queue = access_queue' ~ctx queue c in
            let args, prop, queue =
              List.fold_right
                ~f:(fun x (args, prop, queue) ->
                  let (prop', cx), queue = access_queue' ~ctx queue x in
                  J.Arg cx :: args, or_p prop prop', queue)
                l
                ~init:([], mutator_p, queue)
            in
            ( J.ENew (cc, if List.is_empty args then None else Some args)
            , or_p pc prop
            , queue )
        | Extern "caml_js_get", [ Pv o; Pc (NativeString (Utf f)) ] when J.is_ident' f ->
            let (po, co), queue = access_queue queue o in
            J.dot co f, or_p po mutable_p, queue
        | Extern "caml_js_set", [ Pv o; Pc (NativeString (Utf f)); v ] when J.is_ident' f
          ->
            let (po, co), queue = access_queue queue o in
            let (pv, cv), queue = access_queue' ~ctx queue v in
            J.EBin (J.Eq, J.dot co f, cv), or_p (or_p po pv) mutator_p, queue
        | Extern "caml_js_delete", [ Pv o; Pc (NativeString (Utf f)) ] when J.is_ident' f
          ->
            let (po, co), queue = access_queue queue o in
            J.EUn (J.Delete, J.dot co f), or_p po mutator_p, queue
        (*
           This is only useful for debugging:
           {[
           | Extern "caml_js_get", [ _; Pc (String _) ] -> assert false
           | Extern "caml_js_set", [ _; Pc (String s); _ ] -> assert false
           | Extern "caml_js_delete", [ _; Pc (String _) ] -> assert false
           ]}
        *)
        | Extern "caml_js_global", _ ->
            J.EVar (J.ident Constant.global_object_), const_p, queue
        | Extern "%overrideMod", [ Pc (String m); Pc (String f) ] ->
            runtime_fun ctx (Printf.sprintf "caml_%s_%s" m f), const_p, queue
        | Extern "%overrideMod", _ -> assert false
        | Extern "%caml_js_opt_object", fields ->
            let rec build_fields queue l =
              match l with
              | [] -> const_p, [], queue
              | Pc (NativeString (Utf nm)) :: x :: r ->
                  let (prop, cx), queue = access_queue' ~ctx queue x in
                  let prop', r', queue = build_fields queue r in
                  let p_name = if J.is_ident' nm then J.PNI nm else J.PNS nm in
                  or_p prop prop', J.Property (p_name, cx) :: r', queue
              | _ -> assert false
            in
            let prop, fields, queue = build_fields queue fields in
            J.EObj fields, prop, queue
        | Extern "caml_alloc_dummy_function", [ _; size ] ->
            let i, queue =
              let (_px, cx), queue = access_queue' ~ctx queue size in
              match cx with
              | J.ENum i -> Int32.to_int (J.Num.to_int32 i), queue
              | _ -> assert false
            in
            let args = Array.to_list (Array.init i ~f:(fun _ -> J.V (Var.fresh ()))) in
            let f = J.V (Var.fresh ()) in
            let call =
              J.call
                (J.dot (J.EVar f) (Utf8_string.of_string_exn "fun"))
                (List.map args ~f:(fun v -> J.EVar v))
                loc
            in
            let e =
              J.EFun (Some f, J.fun_ args [ J.Return_statement (Some call), J.N ] J.N)
            in
            e, const_p, queue
        | Extern "caml_alloc_dummy_function", _ -> assert false
        | Extern ("%resume" | "%perform" | "%reperform"), _ ->
            if Config.Flag.effects () then assert false;
            if not !(ctx.effect_warning)
            then (
              warn
                "Warning: your program contains effect handlers; you should probably run \
                 js_of_ocaml with option '--enable=effects'@.";
              ctx.effect_warning := true);
            let name = "jsoo_effect_not_supported" in
            let prim = Share.get_prim (runtime_fun ctx) name ctx.Ctx.share in
            let prim_kind = kind (Primitive.kind name) in
            J.call prim [] loc, prim_kind, queue
        | Extern name, l -> (
            let name = Primitive.resolve name in
            match internal_prim name with
            | Some f -> f l queue ctx loc
            | None ->
                if String.is_prefix name ~prefix:"%"
                then failwith (Printf.sprintf "Unresolved internal primitive: %s" name);
                let prim = Share.get_prim (runtime_fun ctx) name ctx.Ctx.share in
                let prim_kind = kind (Primitive.kind name) in
                let args, prop, queue =
                  List.fold_right
                    ~f:(fun x (args, prop, queue) ->
                      let (prop', cx), queue = access_queue' ~ctx queue x in
                      cx :: args, or_p prop prop', queue)
                    l
                    ~init:([], prim_kind, queue)
                in
                J.call prim args loc, prop, queue)
        | Not, [ x ] ->
            let (px, cx), queue = access_queue' ~ctx queue x in
            J.EBin (J.Minus, one, cx), px, queue
        | Lt, [ x; y ] ->
            let (px, cx), queue = access_queue' ~ctx queue x in
            let (py, cy), queue = access_queue' ~ctx queue y in
            bool (J.EBin (J.LtInt, cx, cy)), or_p px py, queue
        | Le, [ x; y ] ->
            let (px, cx), queue = access_queue' ~ctx queue x in
            let (py, cy), queue = access_queue' ~ctx queue y in
            bool (J.EBin (J.LeInt, cx, cy)), or_p px py, queue
        | Eq, [ x; y ] ->
            let (px, cx), queue = access_queue' ~ctx queue x in
            let (py, cy), queue = access_queue' ~ctx queue y in
            bool (J.EBin (J.EqEqEq, cx, cy)), or_p px py, queue
        | Neq, [ x; y ] ->
            let (px, cx), queue = access_queue' ~ctx queue x in
            let (py, cy), queue = access_queue' ~ctx queue y in
            bool (J.EBin (J.NotEqEq, cx, cy)), or_p px py, queue
        | IsInt, [ x ] ->
            let (px, cx), queue = access_queue' ~ctx queue x in
            bool (Mlvalue.is_immediate cx), px, queue
        | Ult, [ x; y ] ->
            let (px, cx), queue = access_queue' ~ctx queue x in
            let (py, cy), queue = access_queue' ~ctx queue y in
            bool (J.EBin (J.LtInt, unsigned cx, unsigned cy)), or_p px py, queue
        | (Vectlength | Array_get | Not | IsInt | Eq | Neq | Lt | Le | Ult), _ ->
            assert false
      in
      res, []

and translate_instr ctx expr_queue instr =
  let instr, pc = instr in
  match instr with
  | Assign (x, y) ->
      let loc = source_location ctx pc in
      let (_py, cy), expr_queue = access_queue expr_queue y in
      flush_queue
        expr_queue
        mutator_p
        [ J.Expression_statement (J.EBin (J.Eq, J.EVar (J.V x), cy)), loc ]
  | Let (x, e) -> (
      let loc = source_location ctx pc in
      let (ce, prop, expr_queue), instrs = translate_expr ctx expr_queue loc x e 0 in
      let keep_name x =
        match Code.Var.get_name x with
        | None -> false
        (* "switcher" is emitted by the OCaml compiler when compiling
           pattern matching, it does not help much to keep it in the
           generated js, let's drop it *)
        | Some "" -> false
        | Some s -> (not (generated_name s)) && not (String.is_prefix s ~prefix:"jsoo_")
      in
      match ctx.Ctx.live.(Var.idx x), e with
      | 0, _ ->
          (* deadcode is off *)
          flush_queue expr_queue prop (instrs @ [ J.Expression_statement ce, loc ])
      | 1, _
        when Config.Flag.compact () && ((not (Config.Flag.pretty ())) || not (keep_name x))
        -> enqueue expr_queue prop x ce loc 1 instrs
      (* We could inline more.
         size_v : length of the variable after serialization
         size_c : length of the constant after serialization
         num : number of occurrence
         size_c * n < size_v * n + size_v + 1 + size_c
      *)
      | n, Constant (Int _ | Float _) -> enqueue expr_queue prop x ce loc n instrs
      | _ ->
          flush_queue
            expr_queue
            prop
            (instrs @ [ J.variable_declaration [ J.V x, (ce, loc) ], loc ]))
  | Set_field (x, n, y) ->
      let loc = source_location ctx pc in
      let (_px, cx), expr_queue = access_queue expr_queue x in
      let (_py, cy), expr_queue = access_queue expr_queue y in
      flush_queue
        expr_queue
        mutator_p
        [ J.Expression_statement (J.EBin (J.Eq, Mlvalue.Block.field cx n, cy)), loc ]
  | Offset_ref (x, 1) ->
      let loc = source_location ctx pc in
      (* FIX: may overflow.. *)
      let (_px, cx), expr_queue = access_queue expr_queue x in
      flush_queue
        expr_queue
        mutator_p
        [ J.Expression_statement (J.EUn (J.IncrA, Mlvalue.Block.field cx 0)), loc ]
  | Offset_ref (x, n) ->
      let loc = source_location ctx pc in
      (* FIX: may overflow.. *)
      let (_px, cx), expr_queue = access_queue expr_queue x in
      flush_queue
        expr_queue
        mutator_p
        [ J.Expression_statement (J.EBin (J.PlusEq, Mlvalue.Block.field cx 0, int n)), loc
        ]
  | Array_set (x, y, z) ->
      let loc = source_location ctx pc in
      let (_px, cx), expr_queue = access_queue expr_queue x in
      let (_py, cy), expr_queue = access_queue expr_queue y in
      let (_pz, cz), expr_queue = access_queue expr_queue z in
      flush_queue
        expr_queue
        mutator_p
        [ J.Expression_statement (J.EBin (J.Eq, Mlvalue.Array.field cx cy, cz)), loc ]

and translate_instrs ctx expr_queue instr last =
  match instr with
  | [] -> [], expr_queue
  | instr :: rem ->
      let st, expr_queue = translate_instr ctx expr_queue instr in
      let instrs, expr_queue = translate_instrs ctx expr_queue rem last in
      st @ instrs, expr_queue

(* Compile loops. *)
and compile_block st queue (pc : Addr.t) loop_stack frontier interm =
  if (not (List.is_empty queue))
     && (Addr.Set.mem pc st.loops || not (Config.Flag.inline ()))
  then
    let never, code = compile_block st [] pc loop_stack frontier interm in
    never, flush_all queue code
  else
    match Addr.Set.mem pc st.loops with
    | false -> compile_block_no_loop st queue pc loop_stack frontier interm
    | true ->
        if debug () then Format.eprintf "@[<hv 2>for(;;) {@,";
        let never_body, body =
          let lab =
            match loop_stack with
            | (_, (l, _)) :: _ -> J.Label.succ l
            | [] -> J.Label.zero
          in
          let lab_used = ref false in
          let loop_stack = (pc, (lab, lab_used)) :: loop_stack in
          let never_body, body =
            compile_block_no_loop st queue pc loop_stack frontier interm
          in
          let body =
            let rec remove_tailing_continue acc = function
              | [] -> body
              | [ (J.Continue_statement None, _) ] -> List.rev acc
              | x :: xs -> remove_tailing_continue (x :: acc) xs
            in
            remove_tailing_continue [] body
          in
          let for_loop =
            ( J.For_statement
                ( J.Left None
                , None
                , None
                , Js_simpl.block
                    (if never_body
                     then (
                       if debug () then Format.eprintf "}@]@,";
                       body)
                     else (
                       if debug () then Format.eprintf "break;@;}@]@,";
                       body @ [ J.Break_statement None, J.N ])) )
            , source_location st.ctx (Code.location_of_pc pc) )
          in
          let label = if !lab_used then Some lab else None in
          let for_loop =
            match label with
            | None -> for_loop
            | Some label -> J.Labelled_statement (label, for_loop), J.N
          in
          never_body, [ for_loop ]
        in
        never_body, body

(* Compile block. Loops have already been handled. *)
and compile_block_no_loop st queue (pc : Addr.t) loop_stack frontier interm =
  if pc < 0 then assert false;
  if Addr.Set.mem pc !(st.visited_blocks)
  then (
    Format.eprintf "Trying to compile a block twice !!!! %d@." pc;
    assert false);
  let seen = get_seen st pc and pred = get_preds st pc in
  if seen > pred
  then (
    Format.eprintf "This block has too many incoming edges. !!!! %d@." pc;
    assert false);
  if seen < pred
  then (
    Format.eprintf
      "Trying to compile %d, but some (%d) of its predecessors have not been compiled \
       yet. !!!!."
      pc
      (pred - seen);
    assert false);
  assert (seen = pred);
  st.visited_blocks := Addr.Set.add pc !(st.visited_blocks);
  if debug () then Format.eprintf "block %d; frontier: %s;@," pc (string_of_set frontier);
  let block = Addr.Map.find pc st.blocks in
  let seq, queue = translate_instrs st.ctx queue block.body block.branch in
  let new_frontier =
    List.fold_left
      (get_succs st pc)
      ~f:(fun acc pc ->
        let grey = dominance_frontier st pc in
        Addr.Set.union acc grey)
      ~init:Addr.Set.empty
  in
  let prefix, frontier_cont, new_interm, merge_node =
    colapse_frontier "default" st new_frontier interm
  in
  List.iter (get_succs st pc) ~f:(fun pc -> incr_seen st pc);
  (* Beware evaluation order! *)
  let never_cond, cond =
    compile_conditional
      st
      queue
      block.branch
      loop_stack
      (Hashtbl.find st.backs pc)
      (Addr.Set.union frontier frontier_cont)
      new_interm
  in
  let never_after, after =
    compile_merge_node st frontier_cont loop_stack frontier interm merge_node
  in
  never_cond || never_after, seq @ prefix @ cond @ after

(* Compile a merge_node if present *)
and compile_merge_node
    st
    (pc : Addr.Set.t)
    loop_stack
    (frontier : Addr.Set.t)
    interm
    merge_node =
  assert (Addr.Set.cardinal pc <= 1);
  match Addr.Set.choose_opt pc, merge_node with
  | None, Some _ -> assert false
  | None, None -> (* Nothing to compile *) false, []
  | Some pc, None ->
      (* merge node with a one block frontier *)
      compile_branch st [] (pc, []) loop_stack Addr.Set.empty frontier interm
  | Some _, Some (members, branch) ->
      (* merge node *)
      let new_frontier =
        members
        |> List.map ~f:(fun pc ->
               let seen = get_seen st pc in
               dominance_frontier ~seen st pc)
        |> List.fold_left ~init:Addr.Set.empty ~f:Addr.Set.union
      in
      (* The frontier has to move when compiling a merge node. Fail early instead of infinite recursion. *)
      if List.for_all members ~f:(fun pc -> Addr.Set.mem pc new_frontier)
      then assert false;
      let prefix, frontier_cont, new_interm, merge_node =
        colapse_frontier "merge_node" st new_frontier interm
      in
      let never_cond, cond =
        compile_conditional
          st
          []
          branch
          loop_stack
          Addr.Set.empty
          (Addr.Set.union frontier frontier_cont)
          new_interm
      in
      let never_after, after =
        compile_merge_node st frontier_cont loop_stack frontier interm merge_node
      in
      never_cond || never_after, prefix @ cond @ after

and colapse_frontier name st (new_frontier' : Addr.Set.t) interm =
  let new_frontier = Interm.resolve_nodes interm new_frontier' in
  if debug ()
  then
    Format.eprintf
      "Resove %s to %s;@,"
      (string_of_set new_frontier')
      (string_of_set new_frontier);
  if Addr.Set.cardinal new_frontier <= 1
  then [], new_frontier, interm, None
  else
    let idx =
      decr st.last_interm_idx;
      !(st.last_interm_idx)
    in
    if debug ()
    then
      Format.eprintf
        "colapse frontier(%s) into %d: %s@,"
        name
        idx
        (string_of_set new_frontier);
    let x = Code.Var.fresh_n "switch" in
    let a =
      Addr.Set.elements new_frontier
      |> List.map ~f:(fun pc -> pc, get_preds st pc - get_seen st pc)
      |> List.sort ~cmp:(fun (pc1, (c1 : int)) (pc2, (c2 : int)) ->
             match compare c2 c1 with
             | 0 -> compare pc1 pc2
             | c -> c)
      |> List.map ~f:fst
    in
    if debug () then Format.eprintf "var %a;@," Code.Var.print x;
    Hashtbl.add st.succs idx a;
    Hashtbl.add st.preds idx (List.length a);
    let pc_i = List.mapi a ~f:(fun i pc -> pc, i) in
    let default = 0 in
    let interm =
      Interm.add interm ~idx ~var:x (List.map pc_i ~f:(fun (pc, i) -> pc, i, default = i))
    in
    let branch =
      let cases = Array.of_list (List.map a ~f:(fun pc -> pc, [])) in
      if Array.length cases > 2
      then Code.Switch (x, cases, [||]), Code.noloc
      else Code.Cond (x, cases.(1), cases.(0)), Code.noloc
    in
    ( [ J.variable_declaration [ J.V x, (int default, J.N) ], J.N ]
    , Addr.Set.singleton idx
    , interm
    , Some (a, branch) )

and compile_decision_tree st loop_stack backs frontier interm loc cx dtree =
  (* Some changes here may require corresponding changes
     in function [DTree.fold_cont] above. *)
  let rec loop cx : _ -> bool * _ = function
    | DTree.Empty -> assert false
    | DTree.Branch cont ->
        if debug () then Format.eprintf "@[<hv 2>case {@;";
        let never, code = compile_branch st [] cont loop_stack backs frontier interm in
        if debug () then Format.eprintf "}@]@;";
        never, code
    | DTree.If (cond, cont1, cont2) ->
        let never1, iftrue = loop cx cont1 in
        let never2, iffalse = loop cx cont2 in
        let e' =
          match cond with
          | IsTrue -> cx
          | CEq n -> J.EBin (J.EqEqEq, int32 n, cx)
          | CLt n -> J.EBin (J.LtInt, int32 n, cx)
          | CLe n -> J.EBin (J.LeInt, int32 n, cx)
        in
        ( never1 && never2
        , Js_simpl.if_statement
            e'
            loc
            (Js_simpl.block iftrue)
            never1
            (Js_simpl.block iffalse)
            never2 )
    | DTree.Switch a ->
        let all_never = ref true in
        let len = Array.length a in
        let last_index = len - 1 in
        let arr =
          Array.mapi a ~f:(fun i (ints, cont) ->
              let never, cont = loop cx cont in
              if not never then all_never := false;
              let cont =
                if never || (* default case *) i = last_index
                then cont
                else cont @ [ J.Break_statement None, J.N ]
              in
              ints, cont)
        in
        let _, last = arr.(last_index) in
        let l = Array.to_list (Array.sub arr ~pos:0 ~len:(len - 1)) in
        let l =
          List.flatten
            (List.map l ~f:(fun (ints, br) ->
                 List.map_last ~f:(fun last i -> int i, if last then br else []) ints))
        in
        !all_never, [ J.Switch_statement (cx, l, Some last, []), loc ]
  in
  let cx, binds =
    match cx with
    | (J.EVar _ | _) when DTree.nbcomp dtree <= 1 -> cx, []
    | _ ->
        let v = J.V (Code.Var.fresh ()) in
        J.EVar v, [ J.variable_declaration [ v, (cx, J.N) ], J.N ]
  in
  let never, code = loop cx dtree in
  never, binds @ code

and compile_conditional st queue last loop_stack backs frontier interm =
  let last, pc = last in
  (if debug ()
   then
     match last with
     | Branch _ | Poptrap _ -> ()
     | Pushtrap _ -> Format.eprintf "@[<hv 2>try {@;"
     | Return _ -> Format.eprintf "ret;@;"
     | Raise _ -> Format.eprintf "raise;@;"
     | Stop -> Format.eprintf "stop;@;"
     | Cond (x, _, _) -> Format.eprintf "@[<hv 2>cond(%a){@;" Code.Var.print x
     | Switch (x, _, _) -> Format.eprintf "@[<hv 2>switch(%a){@;" Code.Var.print x);
  let loc = source_location st.ctx pc in
  let res =
    match last with
    | Return x ->
        let (_px, cx), queue = access_queue queue x in
        true, flush_all queue [ J.Return_statement (Some cx), loc ]
    | Raise (x, k) ->
        let (_px, cx), queue = access_queue queue x in
        true, flush_all queue (throw_statement st.ctx cx k loc)
    | Stop ->
        let e_opt =
          if st.ctx.Ctx.should_export then Some (s_var Constant.exports) else None
        in
        true, flush_all queue [ J.Return_statement e_opt, loc ]
    | Branch cont -> compile_branch st queue cont loop_stack backs frontier interm
    | Pushtrap (c1, x, e1, _) ->
        let never_body, body = compile_branch st [] c1 loop_stack backs frontier interm in
        if debug () then Format.eprintf "@,}@]@,@[<hv 2>catch {@;";
        let never_handler, handler =
          compile_branch st [] e1 loop_stack backs frontier interm
        in
        let exn_var, handler =
          assert (not (List.mem x ~set:(snd e1)));
          let wrap_exn x =
            J.call
              (Share.get_prim (runtime_fun st.ctx) "caml_wrap_exception" st.ctx.Ctx.share)
              [ J.EVar (J.V x) ]
              J.N
          in
          match st.ctx.Ctx.live.(Var.idx x) with
          | 0 -> x, handler
          | _ ->
              let handler_var = Code.Var.fork x in
              ( handler_var
              , (J.variable_declaration [ J.V x, (wrap_exn handler_var, J.N) ], J.N)
                :: handler )
        in

        ( never_body && never_handler
        , flush_all
            queue
            [ ( J.Try_statement (body, Some (Some (J.param' (J.V exn_var)), handler), None)
              , loc )
            ] )
    | Poptrap cont ->
        let never, code = compile_branch st [] cont loop_stack backs frontier interm in
        never, flush_all queue code
    | Cond (x, c1, c2) ->
        let (_px, cx), queue = access_queue queue x in
        let never, b =
          compile_decision_tree
            st
            loop_stack
            backs
            frontier
            interm
            loc
            cx
            (DTree.build_if c1 c2)
        in
        never, flush_all queue b
    | Switch (x, [||], a2) ->
        let (_px, cx), queue = access_queue queue x in
        let never, code =
          compile_decision_tree
            st
            loop_stack
            backs
            frontier
            interm
            loc
            (Mlvalue.Block.tag cx)
            (DTree.build_switch a2)
        in
        never, flush_all queue code
    | Switch (x, a1, [||]) ->
        let (_px, cx), queue = access_queue queue x in
        let never, code =
          compile_decision_tree
            st
            loop_stack
            backs
            frontier
            interm
            loc
            cx
            (DTree.build_switch a1)
        in
        never, flush_all queue code
    | Switch (x, a1, a2) ->
        (* The variable x is accessed several times, so we can directly
           refer to it *)
        let never1, b1 =
          compile_decision_tree
            st
            loop_stack
            backs
            frontier
            interm
            loc
            (var x)
            (DTree.build_switch a1)
        in
        let never2, b2 =
          compile_decision_tree
            st
            loop_stack
            backs
            frontier
            interm
            loc
            (Mlvalue.Block.tag (var x))
            (DTree.build_switch a2)
        in
        let code =
          Js_simpl.if_statement
            (Mlvalue.is_immediate (var x))
            loc
            (Js_simpl.block b1)
            never1
            (Js_simpl.block b2)
            never2
        in
        never1 && never2, flush_all queue code
  in
  (if debug ()
   then
     match last with
     | Branch _ | Poptrap _ | Return _ | Raise _ | Stop -> ()
     | Switch _ | Cond _ | Pushtrap _ -> Format.eprintf "}@]@;");
  res

and compile_argument_passing ctx queue (pc, args) _backs continuation =
  if List.is_empty args
  then continuation queue
  else
    let block = Addr.Map.find pc ctx.Ctx.blocks in
    parallel_renaming block.params args continuation queue

and compile_branch st queue ((pc, _) as cont) loop_stack backs frontier interm : bool * _
    =
  compile_argument_passing st.ctx queue cont backs (fun queue ->
      if Addr.Set.mem pc backs
      then (
        let label =
          match loop_stack with
          | [] -> assert false
          | (pc', _) :: rem ->
              if pc = pc'
              then None
              else
                let lab, used = List.assoc pc rem in
                used := true;
                Some lab
        in
        if debug ()
        then
          if Option.is_none label
          then Format.eprintf "continue;@,"
          else Format.eprintf "continue (%d);@," pc;
        true, flush_all queue [ J.Continue_statement label, J.N ])
      else if Addr.Set.mem pc frontier || Interm.mem pc interm
      then (
        if debug () then Format.eprintf "(br %d)@;" pc;
        false, flush_all queue (compile_branch_selection pc interm))
      else compile_block st queue pc loop_stack frontier interm)

and compile_branch_selection pc interm =
  try
    let { Interm.pc; var = x; value = i; default } = Interm.find pc interm in
    if debug () then Format.eprintf "%a=%d;@;" Code.Var.print x i;
    let branch = compile_branch_selection pc interm in
    if default
    then branch
    else (J.Expression_statement (EBin (Eq, EVar (J.V x), int i)), J.N) :: branch
  with Not_found -> []

and compile_closure ctx (pc, args) =
  let st = build_graph ctx pc in
  let current_blocks = !(st.visited_blocks) in
  st.visited_blocks := Addr.Set.empty;
  if debug () then Format.eprintf "@[<hv 2>closure {@;";
  let backs = Addr.Set.empty in
  let loop_stack = [] in
  incr_seen st pc;
  let _never, res =
    compile_branch st [] (pc, args) loop_stack backs Addr.Set.empty Interm.empty
  in
  if Addr.Set.cardinal !(st.visited_blocks) <> Addr.Set.cardinal current_blocks
  then (
    let missing = Addr.Set.diff current_blocks !(st.visited_blocks) in
    Format.eprintf "Some blocks not compiled %s!@." (string_of_set missing);
    assert false);
  if debug () then Format.eprintf "}@]@;";
  res

let generate_shared_value ctx =
  let strings =
    ( J.variable_declaration
        ((match ctx.Ctx.exported_runtime with
         | None -> []
         | Some (_, { contents = false }) -> []
         | Some (v, _) ->
             [ ( J.V v
               , ( J.dot
                     (s_var Constant.global_object)
                     (Utf8_string.of_string_exn "jsoo_runtime")
                 , J.N ) )
             ])
        @ List.map
            (StringMap.bindings ctx.Ctx.share.Share.vars.Share.byte_strings)
            ~f:(fun (s, v) -> v, (str_js_byte s, J.N))
        @ List.map
            (StringMap.bindings ctx.Ctx.share.Share.vars.Share.utf_strings)
            ~f:(fun (s, v) -> v, (str_js_utf8 s, J.N))
        @ List.map
            (StringMap.bindings ctx.Ctx.share.Share.vars.Share.prims)
            ~f:(fun (s, v) -> v, (runtime_fun ctx s, J.N)))
    , J.U )
  in
  if not (Config.Flag.inline_callgen ())
  then
    let applies =
      List.map
        (Share.AppMap.bindings ctx.Ctx.share.Share.vars.Share.applies)
        ~f:(fun (desc, v) ->
          match generate_apply_fun ctx desc with
          | J.EFun (_, decl) -> J.Function_declaration (v, decl), J.U
          | _ -> assert false)
    in
    strings :: applies
  else [ strings ]

let compile_program ctx pc =
  if debug () then Format.eprintf "@[<v 2>";
  let res = compile_closure ctx (pc, []) in
  let res = generate_shared_value ctx @ res in
  if debug () then Format.eprintf "@]@.";
  res

let init () =
  List.iter
    ~f:(fun (nm, nm') -> Primitive.alias nm nm')
    [ "%int_mul", "caml_mul"
    ; "%int_div", "caml_div"
    ; "%int_mod", "caml_mod"
    ; "caml_int32_neg", "%int_neg"
    ; "caml_int32_add", "%int_add"
    ; "caml_int32_sub", "%int_sub"
    ; "caml_int32_mul", "%int_mul"
    ; "caml_int32_div", "%int_div"
    ; "caml_int32_mod", "%int_mod"
    ; "caml_int32_and", "%int_and"
    ; "caml_int32_or", "%int_or"
    ; "caml_int32_xor", "%int_xor"
    ; "caml_int32_shift_left", "%int_lsl"
    ; "caml_int32_shift_right", "%int_asr"
    ; "caml_int32_shift_right_unsigned", "%int_lsr"
    ; "caml_int32_of_int", "%identity"
    ; "caml_int32_to_int", "%identity"
    ; "caml_int32_of_float", "caml_int_of_float"
    ; "caml_int32_to_float", "%identity"
    ; "caml_int32_format", "caml_format_int"
    ; "caml_int32_of_string", "caml_int_of_string"
    ; "caml_int32_compare", "caml_int_compare"
    ; "caml_nativeint_neg", "%int_neg"
    ; "caml_nativeint_add", "%int_add"
    ; "caml_nativeint_sub", "%int_sub"
    ; "caml_nativeint_mul", "%int_mul"
    ; "caml_nativeint_div", "%int_div"
    ; "caml_nativeint_mod", "%int_mod"
    ; "caml_nativeint_and", "%int_and"
    ; "caml_nativeint_or", "%int_or"
    ; "caml_nativeint_xor", "%int_xor"
    ; "caml_nativeint_shift_left", "%int_lsl"
    ; "caml_nativeint_shift_right", "%int_asr"
    ; "caml_nativeint_shift_right_unsigned", "%int_lsr"
    ; "caml_nativeint_of_int", "%identity"
    ; "caml_nativeint_to_int", "%identity"
    ; "caml_nativeint_of_float", "caml_int_of_float"
    ; "caml_nativeint_to_float", "%identity"
    ; "caml_nativeint_of_int32", "%identity"
    ; "caml_nativeint_to_int32", "%identity"
    ; "caml_nativeint_format", "caml_format_int"
    ; "caml_nativeint_of_string", "caml_int_of_string"
    ; "caml_nativeint_compare", "caml_int_compare"
    ; "caml_nativeint_bswap", "caml_int32_bswap"
    ; "caml_int64_of_int", "caml_int64_of_int32"
    ; "caml_int64_to_int", "caml_int64_to_int32"
    ; "caml_int64_of_nativeint", "caml_int64_of_int32"
    ; "caml_int64_to_nativeint", "caml_int64_to_int32"
    ; "caml_float_of_int", "%identity"
    ; "caml_array_get_float", "caml_array_get"
    ; "caml_floatarray_get", "caml_array_get"
    ; "caml_array_get_addr", "caml_array_get"
    ; "caml_array_set_float", "caml_array_set"
    ; "caml_floatarray_set", "caml_array_set"
    ; "caml_array_set_addr", "caml_array_set"
    ; "caml_array_unsafe_get_float", "caml_array_unsafe_get"
    ; "caml_floatarray_unsafe_get", "caml_array_unsafe_get"
    ; "caml_array_unsafe_set_float", "caml_array_unsafe_set"
    ; "caml_floatarray_unsafe_set", "caml_array_unsafe_set"
    ; "caml_alloc_dummy_float", "caml_alloc_dummy"
    ; "caml_make_array", "%identity"
    ; "caml_ensure_stack_capacity", "%identity"
    ; "caml_js_from_float", "%identity"
    ; "caml_js_to_float", "%identity"
    ];
  Hashtbl.iter
    (fun name (k, _) -> Primitive.register name k None None)
    internal_primitives

let f
    (p : Code.program)
    ~exported_runtime
    ~live_vars
    ~cps_calls
    ~should_export
    ~warn_on_unhandled_effect
    debug =
  let t' = Timer.make () in
  let share = Share.get ~cps_calls ~alias_prims:exported_runtime p in
  let exported_runtime =
    if exported_runtime then Some (Code.Var.fresh_n "runtime", ref false) else None
  in
  let ctx =
    Ctx.initial
      ~warn_on_unhandled_effect
      ~exported_runtime
      ~should_export
      p.blocks
      live_vars
      cps_calls
      share
      debug
  in
  let p = compile_program ctx p.start in
  if times () then Format.eprintf "  code gen.: %a@." Timer.print t';
  p
