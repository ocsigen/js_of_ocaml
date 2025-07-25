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

let debug = Debug.find "gen"

let times = Debug.find "times"

let cps_transform () =
  match Config.effects () with
  | `Cps | `Double_translation -> true
  | `Disabled -> false
  | `Jspi -> assert false

open Code
module J = Javascript

(****)

let string_of_set s =
  String.concat ~sep:", " (List.map ~f:Addr.to_string (Addr.Set.elements s))

let rec list_group_rec ~equal f g l b m n =
  match l with
  | [] -> List.rev ((b, List.rev m) :: n)
  | a :: r ->
      let fa = f a in
      if equal fa b
      then list_group_rec ~equal f g r b (g a :: m) n
      else list_group_rec ~equal f g r fa [ g a ] ((b, List.rev m) :: n)

let list_group ~equal f g l =
  match l with
  | [] -> []
  | a :: r -> list_group_rec ~equal f g r (f a) [ g a ] []

(****)

type fall_through =
  | Block of Addr.t
  | Return

type application_description =
  { arity : int
  ; exact : bool
  ; trampolined : bool
  ; in_cps : bool
  }

module Share = struct
  module AppMap = Map.Make (struct
    type t = application_description

    let compare { arity; exact; trampolined; in_cps } b =
      let c = compare arity b.arity in
      if c <> 0
      then c
      else
        let c = Bool.compare exact b.exact in
        if c <> 0
        then c
        else
          let c = Bool.compare trampolined b.trampolined in
          if c <> 0 then c else Bool.compare in_cps b.in_cps
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
      ~trampolined_calls
      ~in_cps
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
          List.fold_left block.body ~init:share ~f:(fun share i ->
              match i with
              | Let (_, Constant c) -> get_constant c share
              | Let (x, Apply { args; exact; _ }) ->
                  let trampolined = Var.Set.mem x trampolined_calls in
                  let in_cps = Var.Set.mem x in_cps in
                  if (not exact) || trampolined
                  then
                    add_apply
                      { arity = List.length args; exact; trampolined; in_cps }
                      share
                  else share
              | Let (_, Special (Alias_prim name)) ->
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
        ; "caml_int64_create_lo_mi_hi"
        ; "caml_stack_check_depth"
        ; "caml_call_gen"
        ; "caml_call_gen_cps"
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
          let { arity; exact; trampolined; in_cps } = desc in
          Var.fresh_n
            (Printf.sprintf
               "caml_%scall%d"
               (match exact, trampolined, in_cps with
               | true, false, false -> assert false (* inlined *)
               | true, false, true -> "exact_cps_"
               | true, true, false -> "exact_trampoline_"
               | false, false, true ->
                   assert false (* CPS functions are always trampolined *)
               | false, false, false -> ""
               | false, true, false -> "trampoline_"
               | false, true, true -> "trampoline_cps_"
               | true, true, true -> "exact_trampoline_cps_")
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
    ; exported_runtime : (Code.Var.t * bool ref) option
    ; should_export : bool
    ; effect_warning : bool ref
    ; trampolined_calls : Effects.trampolined_calls
    ; deadcode_sentinal : Var.t
    ; mutated_vars : Code.Var.Set.t Code.Addr.Map.t
    ; freevars : Code.Var.Set.t Code.Addr.Map.t
    ; in_cps : Effects.in_cps
    }

  let initial
      ~warn_on_unhandled_effect
      ~exported_runtime
      ~should_export
      ~deadcode_sentinal
      ~mutated_vars
      ~freevars
      ~in_cps
      blocks
      live
      trampolined_calls
      share =
    { blocks
    ; live
    ; share
    ; exported_runtime
    ; should_export
    ; effect_warning = ref (not warn_on_unhandled_effect)
    ; trampolined_calls
    ; deadcode_sentinal
    ; mutated_vars
    ; freevars
    ; in_cps
    }
end

type edge_kind =
  | Loop
  | Exit_loop of bool ref
  | Exit_switch of bool ref
  | Forward

let var x = J.EVar (J.V x)

let int n = J.ENum (J.Num.of_targetint (Targetint.of_int_exn n))

let targetint n = J.ENum (J.Num.of_targetint n)

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
    | J.ENum num -> ( try Targetint.(J.Num.to_targetint num >= zero) with _ -> false)
    | _ -> false
  in
  if pos_int32 then x else unsigned' x

let one = J.ENum (J.Num.of_targetint Targetint.one)

let zero = J.ENum (J.Num.of_targetint Targetint.zero)

let plus_int x y =
  match x, y with
  | J.ENum y, x when J.Num.is_zero y -> x
  | x, J.ENum y when J.Num.is_zero y -> x
  | J.ENum x, J.ENum y -> J.ENum (J.Num.add x y)
  | x, y -> J.EBin (J.Plus, x, y)

let bool e = J.ECond (e, one, zero)

(****)

let source_location loc =
  match loc with
  | Some pi -> J.Pi pi
  | None -> J.N

(****)

let float_const f = J.ENum (J.Num.of_float (Int64.float_of_bits f))

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

type prop' =
  | Const
  | Mutable
  | Mutator
  | Flush

type prop = prop' * Code.Var.Set.t

let max_prop' a b =
  match a, b with
  | Flush, _ | _, Flush -> Flush
  | Mutator, _ | _, Mutator -> Mutator
  | Mutable, _ | _, Mutable -> Mutable
  | Const, Const -> Const

let const_p = Const, Var.Set.empty

and mutable_p = Mutable, Var.Set.empty

and mutator_p = Mutator, Var.Set.empty

and flush_p = Flush, Var.Set.empty

let or_p (p, s1) (q, s2) = max_prop' p q, Var.Set.union s1 s2

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
          ~args:(Array.to_list (Array.map a ~f:(fun x -> J.Element (float_const x))))
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
        | Int maybe_zero when Targetint.is_zero maybe_zero ->
            if n > constant_max_depth then Some acc else None
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
                      J.Element (J.EVar (J.V v)) :: acc, instrs
                  | _ -> J.Element js :: acc, instrs)
            else List.map ~f:(fun x -> J.Element x) (List.rev l), instrs
          in
          Mlvalue.Block.make ~tag ~args:l, instrs)
  | Int i -> targetint i, instrs
  | Int32 i | NativeInt i -> targetint (Targetint.of_int32_exn i), instrs

let constant ~ctx x level =
  let expr, instr = constant_rec ~ctx x level [] in
  expr, List.rev instr

module Q : sig
  type queue

  val access_queue :
    live:int array -> queue -> Var.t -> (prop * J.expression * J.location option) * queue

  val access_queue_loc :
       ctx:Ctx.t
    -> queue
    -> J.location
    -> Var.t
    -> (prop * J.expression * J.location) * queue

  val enqueue :
       queue
    -> prop
    -> Var.t
    -> J.expression
    -> J.location
    -> J.location option
    -> J.statement_list
    -> J.statement_list * queue

  val flush_queue :
       queue
    -> prop
    -> J.location
    -> J.statement_list
    -> (J.statement * J.location) list * queue

  val flush_all :
    queue -> J.location -> J.statement_list -> (J.statement * J.location) list

  val empty : queue

  val is_empty : queue -> bool
end = struct
  type elt =
    { prop : prop'
    ; ce : J.expression
    ; loc : J.location option
    ; deps : Code.Var.Set.t
    ; rank : int
    }

  type queue =
    { map : elt Var.Map.t
    ; muts : Var.Set.t
    ; rank : int
    }

  let empty = { map = Var.Map.empty; muts = Var.Set.empty; rank = 0 }

  let is_empty t = Var.Map.is_empty t.map

  let access_queue ~live queue x =
    let idx = Var.idx x in
    if idx < Array.length live && Array.unsafe_get live idx = 1
    then
      match Var.Map.find_opt x queue.map with
      | None -> ((Const, Code.Var.Set.singleton x), var x, None), queue
      | Some { prop; deps; ce; loc; rank = _ } ->
          ( ((prop, deps), ce, loc)
          , { map = Var.Map.remove x queue.map
            ; muts =
                (match prop with
                | Const -> queue.muts
                | _ -> Var.Set.remove x queue.muts)
            ; rank = queue.rank
            } )
    else ((Const, Code.Var.Set.singleton x), var x, None), queue

  let access_queue_loc ~ctx queue loc' x =
    let (prop, c, loc), queue = access_queue ~live:ctx.Ctx.live queue x in
    (prop, c, Option.value ~default:loc' loc), queue

  let flush_queue queue prop loc (l : J.statement_list) =
    let instrs, queue =
      let prop = fst prop in
      match prop with
      | Const -> [], queue
      | Flush -> Var.Map.bindings queue.map, empty
      | Mutable ->
          let flush = ref [] in
          let muts =
            Var.Set.filter
              (fun x ->
                let elt = Var.Map.find x queue.map in
                match elt.prop with
                | Mutator | Flush ->
                    flush := (x, elt) :: !flush;
                    false
                | _ -> true)
              queue.muts
          in
          ( !flush
          , { muts
            ; map =
                List.fold_left !flush ~init:queue.map ~f:(fun acc (x, _) ->
                    Var.Map.remove x acc)
            ; rank = queue.rank
            } )
      | Mutator ->
          let flush = ref [] in
          let muts = Var.Set.empty in
          Var.Set.iter
            (fun x ->
              let elt = Var.Map.find x queue.map in
              assert (
                match elt.prop with
                | Mutator | Mutable | Flush -> true
                | Const -> false);
              flush := (x, elt) :: !flush)
            queue.muts;
          ( !flush
          , { muts
            ; map = Var.Set.fold (fun x acc -> Var.Map.remove x acc) queue.muts queue.map
            ; rank = queue.rank
            } )
    in
    let instrs =
      List.stable_sort
        ~cmp:(fun (_, ({ rank = a; _ } : elt)) (_, { rank = b; _ }) -> compare b a)
        instrs
    in
    let instrs =
      List.map instrs ~f:(fun (x, elt) ->
          let loc = Option.value ~default:loc elt.loc in
          J.variable_declaration [ J.V x, (elt.ce, loc) ], loc)
    in
    List.rev_append instrs l, queue

  let flush_all queue loc l = fst (flush_queue queue flush_p loc l)

  let enqueue queue prop x ce flush_loc expr_loc acc =
    let instrs, queue =
      if Config.Flag.compact ()
      then
        match fst prop with
        | Mutable | Mutator | Flush -> flush_queue queue prop flush_loc acc
        | Const -> acc, queue
      else flush_queue queue flush_p flush_loc acc
    in
    let rank = queue.rank in
    let prop, deps = prop in
    ( instrs
    , { map = Var.Map.add x { prop; deps; ce; loc = expr_loc; rank } queue.map
      ; muts =
          (match prop with
          | Const -> queue.muts
          | _ -> Var.Set.add x queue.muts)
      ; rank = rank + 1
      } )
end

module Expr_builder : sig
  type 'a t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val return : 'a -> 'a t

  val access : ctx:Ctx.t -> Var.t -> J.expression t

  val access' : ctx:Ctx.t -> prim_arg -> J.expression t

  val info : ?need_loc:bool -> prop -> unit t

  val statement_loc : J.location -> J.location t

  val flush_all : Q.queue -> J.location -> J.statement_list t -> J.statement_list

  val flush_queue :
    Q.queue -> J.location -> J.statement_list t -> J.statement_list * Q.queue

  val enqueue :
       Q.queue
    -> Var.t
    -> J.location
    -> (J.expression * J.statement_list) t
    -> J.statement_list * Q.queue

  val get : Q.queue -> J.location -> 'a t -> 'a * J.location * Q.queue

  val list_map : ('a -> 'b t) -> 'a list -> 'b list t
end = struct
  type state =
    { queue : Q.queue
    ; prop : prop
    ; need_loc : bool
    ; loc : J.location option
    }

  type 'a t = state -> 'a * state

  let ( let* ) (type a b) (e : a t) (f : a -> b t) : b t =
   fun st ->
    let v, st = e st in
    f v st

  let return x st = x, st

  let info ?(need_loc = false) prop st =
    (), { st with prop = or_p st.prop prop; need_loc = need_loc || st.need_loc }

  let access ~ctx x st =
    let (prop, c, loc), queue = Q.access_queue ~live:ctx.Ctx.live st.queue x in
    ( c
    , { st with
        prop = or_p st.prop prop
      ; queue
      ; loc =
          (match st.loc with
          | None -> loc
          | _ -> st.loc)
      } )

  let access' ~ctx x =
    match x with
    | Pc c ->
        let js, instrs = constant ~ctx c (Config.Param.constant_max_depth ()) in
        assert (List.is_empty instrs);
        (* We only have simple constants here *)
        fun st -> js, st
    | Pv x -> access ~ctx x

  let statement_loc loc st =
    ( (match st.loc with
      | None -> loc
      | Some loc -> loc)
    , st )

  let initial_state queue = { queue; prop = const_p; loc = None; need_loc = false }

  let flush_queue queue loc instrs =
    let v, { queue; prop; _ } = instrs (initial_state queue) in
    Q.flush_queue queue prop loc v

  let flush_all queue loc instrs =
    let v, { queue; _ } = instrs (initial_state queue) in
    Q.flush_all queue loc v

  let enqueue queue x flush_loc expr =
    let (ce, instrs), { queue; prop; loc; need_loc } = expr (initial_state queue) in
    let expr_loc =
      match loc with
      | None when need_loc -> Some flush_loc
      | _ -> loc
    in
    Q.enqueue queue prop x ce flush_loc expr_loc instrs

  let get queue loc' x =
    let x, { queue; loc; _ } = x (initial_state queue) in
    let loc =
      match loc with
      | None -> loc'
      | Some loc -> loc
    in
    x, loc, queue

  let rec list_map f l st =
    match l with
    | [] -> [], st
    | x :: r ->
        let x', st = f x st in
        let r', st = list_map f r st in
        x' :: r', st
end

(****)

type state =
  { structure : Structure.t
  ; dom : Structure.graph
  ; visited_blocks : Addr.Set.t ref
  ; ctx : Ctx.t
  ; cloc : Parse_info.t option
  }

module DTree = struct
  (* This has to be kept in sync with the way we build conditionals
     and switches! *)

  type cond =
    | IsTrue
    | CEq of Targetint.t
    | CLt of Targetint.t
    | CLe of Targetint.t

  type 'a branch = int list * 'a

  type 'a t =
    | If of cond * 'a t * 'a t
    | Switch of 'a branch array
    | Branch of 'a branch

  let normalize a =
    a
    |> Array.to_list
    |> List.sort ~cmp:(fun (cont1, _) (cont2, _) -> cont_compare cont1 cont2)
    |> list_group ~equal:cont_equal fst snd
    |> List.map ~f:(fun (cont1, l1) -> cont1, List.flatten l1)
    |> List.sort ~cmp:(fun (_, l1) (_, l2) -> List.compare_lengths l1 l2)
    |> Array.of_list

  let build_if b1 b2 = If (IsTrue, Branch ([ 1 ], b1), Branch ([ 0 ], b2))

  let build_switch (a : cont array) : cont t =
    let m = Config.Param.switch_max_case () in
    let ai = Array.mapi a ~f:(fun i x -> x, i) in
    (* group the contiguous cases with the same continuation *)
    let ai : (Code.cont * int list) array =
      Array.of_list (list_group ~equal:cont_equal fst snd (Array.to_list ai))
    in
    let rec loop low up =
      let array_norm : (Code.cont * int list) array =
        normalize (Array.sub ai ~pos:low ~len:(up - low + 1))
      in
      let array_len = Array.length array_norm in
      if array_len = 1 (* remaining cases all jump to the same branch *)
      then Branch (snd array_norm.(0), fst array_norm.(0))
      else
        try
          (* try to optimize when there are only 2 branch *)
          match array_norm with
          | [| (b1, ([ i1 ] as l1)); (b2, l2) |] ->
              If (CEq (Targetint.of_int_exn i1), Branch (l1, b1), Branch (l2, b2))
          | [| (b1, l1); (b2, ([ i2 ] as l2)) |] ->
              If (CEq (Targetint.of_int_exn i2), Branch (l2, b2), Branch (l1, b1))
          | [| (b1, l1); (b2, l2) |] ->
              let bound l1 =
                match l1, List.rev l1 with
                | min :: _, max :: _ -> min, max
                | _ -> assert false
              in
              let min1, max1 = bound l1 in
              let min2, max2 = bound l2 in
              if max1 < min2
              then If (CLt (Targetint.of_int_exn max1), Branch (l2, b2), Branch (l1, b1))
              else if max2 < min1
              then If (CLt (Targetint.of_int_exn max2), Branch (l1, b1), Branch (l2, b2))
              else raise Not_found
          | _ -> raise Not_found
        with Not_found -> (
          (* do we have to split again ? *)
          (* we count the number of cases, default/last case count for one *)
          let nbcases =
            ref 1
            (* default case *)
          in
          for i = 0 to array_len - 2 do
            nbcases := !nbcases + List.length (snd array_norm.(i))
          done;
          if !nbcases <= m
          then Switch (Array.map array_norm ~f:(fun (x, l) -> l, x))
          else
            let h = (up + low) / 2 in
            let b1 = loop low h and b2 = loop (succ h) up in
            let range1 = snd ai.(h) and range2 = snd ai.(succ h) in
            match range1, range2 with
            | [], _ | _, [] -> assert false
            | _, lower_bound2 :: _ -> If (CLe (Targetint.of_int_exn lower_bound2), b2, b1))
    in
    let len = Array.length ai in
    assert (len > 0);
    loop 0 (len - 1)

  let nbbranch (a : cont t) pc =
    let rec loop c : cont t -> int = function
      | Branch (_, (pc', _)) -> if pc' = pc then succ c else c
      | If (_, a, b) ->
          let c = loop c a in
          let c = loop c b in
          c
      | Switch a ->
          Array.fold_left a ~init:c ~f:(fun acc (_, (pc', _)) ->
              if pc' = pc then succ acc else acc)
    in
    loop 0 a

  let nbcomp a =
    let rec loop c = function
      | Branch _ -> c
      | If (_, a, b) ->
          let c = succ c in
          let c = loop c a in
          let c = loop c b in
          c
      | Switch _ ->
          let c = succ c in
          c
    in

    loop 0 a
end

let build_graph ctx pc cloc =
  let visited_blocks = ref Addr.Set.empty in
  let structure = Structure.build_graph ctx.Ctx.blocks pc in
  let dom = Structure.dominator_tree structure in
  { visited_blocks; structure; dom; ctx; cloc }

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

let parallel_renaming ctx loc back_edge params args continuation queue =
  if
    back_edge && Config.Flag.es6 ()
    (* This is likely slower than using explicit temp variable
        but let's experiment with es6 a bit *)
  then
    let args, params =
      List.map2 args params ~f:(fun a p -> if Var.equal a p then None else Some (a, p))
      |> List.filter_map ~f:(fun x -> x)
      |> List.split
    in
    let open Expr_builder in
    let args, loc, queue =
      get
        queue
        loc
        (List.fold_left args ~init:(return []) ~f:(fun acc a ->
             let* acc = acc in
             let* cx = access ~ctx a in
             return (cx :: acc)))
    in
    let never, code = continuation queue in
    match params, args with
    | [ p ], [ a ] ->
        never, (J.Expression_statement (J.EBin (J.Eq, J.EVar (J.V p), a)), loc) :: code
    | params, args ->
        let lhs =
          J.EAssignTarget
            (J.ArrayTarget (List.map params ~f:(fun p -> J.TargetElementId (J.V p, None))))
        in
        let rhs = J.EArr (List.rev_map args ~f:(fun x -> J.Element x)) in
        never, (J.Expression_statement (J.EBin (J.Eq, lhs, rhs)), loc) :: code
  else
    let l = visit_all params args in
    (* if not back_edge
     * then assert (Poly.equal l (List.rev_map2 params args ~f:(fun a b -> a, b))); *)
    let queue, before, renaming, _ =
      List.fold_left
        l
        ~init:(queue, [], [], Code.Var.Set.empty)
        ~f:(fun (queue, before, renaming, seen) (y, x) ->
          let ((_, deps_x), cx, locx), queue = Q.access_queue_loc ~ctx queue loc x in
          let seen' = Code.Var.Set.add y seen in
          if not Code.Var.Set.(is_empty (inter seen deps_x))
          then
            let () = assert back_edge in
            let before = (J.variable_declaration [ J.V x, (cx, locx) ], locx) :: before in
            let renaming = (y, J.EVar (J.V x)) :: renaming in
            queue, before, renaming, seen'
          else
            let renaming = (y, cx) :: renaming in
            queue, before, renaming, seen')
    in
    let renaming =
      if back_edge
      then
        List.map renaming ~f:(fun (t, e) ->
            J.Expression_statement (J.EBin (J.Eq, J.EVar (J.V t), e)), loc)
      else
        List.map renaming ~f:(fun (t, e) ->
            J.variable_declaration [ J.V t, (e, loc) ], loc)
    in
    let never, code = continuation queue in
    never, List.rev_append before (List.rev_append renaming code)

(****)

let apply_fun_raw =
  let cps_field = Utf8_string.of_string_exn "cps" in
  fun ctx f params exact trampolined cps loc ->
    let apply_directly f params =
      (* Make sure we are performing a regular call, not a (slower)
         method call *)
      match f with
      | J.EAccess _ | J.EDot _ ->
          J.call (J.dot f (Utf8_string.of_string_exn "call")) (s_var "null" :: params) loc
      | _ -> J.call f params loc
    in
    let apply ~cps f params =
      (* Adapt if [f] is a (direct-style, CPS) closure pair *)
      let real_closure =
        match Config.effects () with
        | `Double_translation when cps ->
            (* Effects enabled, CPS version, not single-version *)
            J.EDot (f, J.ANormal, cps_field)
        | `Cps | `Double_translation | `Disabled -> f
        | `Jspi -> assert false
      in
      (* We skip the arity check when we know that we have the right
         number of parameters, since this test is expensive. *)
      if exact
      then apply_directly real_closure params
      else
        let l = Utf8_string.of_string_exn "l" in
        J.ECond
          ( J.EBin
              ( J.EqEqEq
              , J.ECond
                  ( J.EBin (J.Ge, J.dot real_closure l, int 0)
                  , J.dot real_closure l
                  , J.EBin
                      ( J.Eq
                      , J.dot real_closure l
                      , J.dot real_closure (Utf8_string.of_string_exn "length") ) )
              , int (List.length params) )
          , apply_directly real_closure params
          , J.call
              (* Note: when double translation is enabled, [caml_call_gen*] functions takes a two-version function *)
              (runtime_fun
                 ctx
                 (match Config.effects () with
                 | `Double_translation when cps -> "caml_call_gen_cps"
                 | `Double_translation | `Cps | `Disabled -> "caml_call_gen"
                 | `Jspi -> assert false))
              [ f; J.array params ]
              J.N )
    in
    let apply =
      match Config.effects () with
      | `Double_translation when cps ->
          let n = List.length params in
          J.ECond
            ( J.EDot (f, J.ANormal, cps_field)
            , apply ~cps:true f params
            , J.call
                (List.nth params (n - 1))
                [ apply ~cps:false f (fst (List.take (n - 1) params)) ]
                J.N )
      | `Double_translation | `Cps | `Disabled -> apply ~cps f params
      | `Jspi -> assert false
    in
    if trampolined
    then (
      assert (cps_transform ());
      (* When supporting effect, we systematically perform tailcall
         optimization. To implement it, we check the stack depth and
         bounce to a trampoline if needed, to avoid a stack overflow.
         The trampoline then performs the call in an shorter stack. *)
      J.ECond
        ( J.call (runtime_fun ctx "caml_stack_check_depth") [] loc
        , apply
        , J.call
            (runtime_fun ctx "caml_trampoline_return")
            [ f; J.array params; (if cps then zero else one) ]
            loc ))
    else apply

let generate_apply_fun ctx { arity; exact; trampolined; in_cps } =
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
        [ ( J.Return_statement
              (Some (apply_fun_raw ctx f' params' exact trampolined in_cps J.N), J.N)
          , J.N )
        ]
        J.N )

let apply_fun ctx f params exact trampolined in_cps loc =
  (* We always go through an intermediate function when doing CPS
     calls. This function first checks the stack depth to prevent
     a stack overflow. This makes the code smaller than inlining
     the test, and we expect the performance impact to be low
     since the function should get inlined by the JavaScript
     engines. *)
  if Config.Flag.inline_callgen () || (exact && not trampolined)
  then apply_fun_raw ctx f params exact trampolined in_cps loc
  else
    let y =
      Share.get_apply
        (generate_apply_fun ctx)
        { arity = List.length params; exact; trampolined; in_cps }
        ctx.Ctx.share
    in
    J.call y (f :: params) loc

(****)

let internal_primitives = String.Hashtbl.create 31

let internal_prim name =
  try
    let _, f = String.Hashtbl.find internal_primitives name in
    Some f
  with Not_found -> None

let register_prims names k (f : string -> _ list -> _ -> _ -> _) =
  List.iter names ~f:(fun name -> String.Hashtbl.add internal_primitives name (k, f))

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

let register_un_prims names ?(need_loc = false) k f =
  register_prims names k (fun name l ctx loc ->
      match l with
      | [ x ] ->
          let open Expr_builder in
          let* cx = access' ~ctx x in
          let* () = info ~need_loc (kind k) in
          return (f cx loc)
      | l -> invalid_arity name l ~loc ~expected:1)

let register_un_prim name k f = register_un_prims [ name ] k f

let register_un_prim_ctx name k f =
  register_prims [ name ] k (fun name l ctx loc ->
      match l with
      | [ x ] ->
          let open Expr_builder in
          let* cx = access' ~ctx x in
          let* () = info (kind k) in
          return (f ctx cx loc)
      | _ -> invalid_arity name l ~loc ~expected:1)

let register_bin_prims names k f =
  register_prims names k (fun name l ctx loc ->
      match l with
      | [ x; y ] ->
          let open Expr_builder in
          let* cx = access' ~ctx x in
          let* cy = access' ~ctx y in
          let* () = info (kind k) in
          return (f cx cy loc)
      | _ -> invalid_arity name l ~loc ~expected:2)

let register_bin_prim name k f = register_bin_prims [ name ] k f

let register_tern_prims names k f =
  register_prims names k (fun name l ctx loc ->
      match l with
      | [ x; y; z ] ->
          let open Expr_builder in
          let* cx = access' ~ctx x in
          let* cy = access' ~ctx y in
          let* cz = access' ~ctx z in
          let* () = info mutator_p in
          return (f cx cy cz loc)
      | _ -> invalid_arity name l ~loc ~expected:3)

let register_tern_prim name k f = register_tern_prims [ name ] k f

let register_un_math_prim name prim =
  let prim = Utf8_string.of_string_exn prim in
  register_un_prim name `Pure (fun cx loc ->
      J.call (J.dot (s_var "Math") prim) [ cx ] loc)

let register_bin_math_prim name prim =
  let prim = Utf8_string.of_string_exn prim in
  register_bin_prims [ name ] `Pure (fun cx cy loc ->
      J.call (J.dot (s_var "Math") prim) [ cx; cy ] loc)

let _ =
  register_un_prim_ctx "%caml_format_int_special" `Pure (fun ctx cx loc ->
      let s = J.EBin (J.Plus, str_js_utf8 "", cx) in
      ocaml_string ~ctx ~loc s);
  register_un_prim "%direct_obj_tag" `Pure (fun cx _loc -> Mlvalue.Block.tag cx);
  register_bin_prims
    [ "caml_array_unsafe_get"
    ; "caml_array_unsafe_get_float"
    ; "caml_floatarray_unsafe_get"
    ]
    `Mutable
    (fun cx cy _ -> Mlvalue.Array.field cx cy);
  register_un_prims
    [ "caml_int32_of_int"
    ; "caml_int32_to_int"
    ; "caml_int32_to_float"
    ; "caml_nativeint_of_int"
    ; "caml_nativeint_to_int"
    ; "caml_nativeint_to_int32"
    ; "caml_nativeint_of_int32"
    ; "caml_nativeint_to_float"
    ; "caml_float_of_int"
    ]
    `Pure
    (fun cx _ -> cx);
  register_bin_prims
    [ "%int_add"; "caml_int32_add"; "caml_nativeint_add" ]
    `Pure
    (fun cx cy _ ->
      match cx, cy with
      | J.EBin (J.Minus, cz, J.ENum n), J.ENum m ->
          to_int (J.EBin (J.Plus, cz, J.ENum (J.Num.add m (J.Num.neg n))))
      | _ -> to_int (plus_int cx cy));
  register_bin_prims
    [ "%int_sub"; "caml_int32_sub"; "caml_nativeint_sub" ]
    `Pure
    (fun cx cy _ ->
      match cx, cy with
      | J.EBin (J.Minus, cz, J.ENum n), J.ENum m ->
          to_int (J.EBin (J.Minus, cz, J.ENum (J.Num.add n m)))
      | _ -> to_int (J.EBin (J.Minus, cx, cy)));
  register_bin_prim "%direct_int_mul" `Pure (fun cx cy _ ->
      to_int (J.EBin (J.Mul, cx, cy)));
  register_bin_prim "%direct_int_div" `Pure (fun cx cy _ ->
      to_int (J.EBin (J.Div, cx, cy)));
  register_bin_prim "%direct_int_mod" `Pure (fun cx cy _ ->
      to_int (J.EBin (J.Mod, cx, cy)));
  register_bin_prims
    [ "%int_and"; "caml_int32_and"; "caml_nativeint_and" ]
    `Pure
    (fun cx cy _ -> J.EBin (J.Band, cx, cy));
  register_bin_prims
    [ "%int_or"; "caml_int32_or"; "caml_nativeint_or" ]
    `Pure
    (fun cx cy _ -> J.EBin (J.Bor, cx, cy));
  register_bin_prims
    [ "%int_xor"; "caml_int32_xor"; "caml_nativeint_xor" ]
    `Pure
    (fun cx cy _ -> J.EBin (J.Bxor, cx, cy));
  register_bin_prims
    [ "%int_lsl"; "caml_int32_shift_left"; "caml_nativeint_shift_left" ]
    `Pure
    (fun cx cy _ -> J.EBin (J.Lsl, cx, cy));
  register_bin_prims
    [ "%int_lsr"
    ; "caml_int32_shift_right_unsigned"
    ; "caml_nativeint_shift_right_unsigned"
    ]
    `Pure
    (fun cx cy _ -> to_int (J.EBin (J.Lsr, cx, cy)));
  register_bin_prims
    [ "%int_asr"; "caml_int32_shift_right"; "caml_nativeint_shift_right" ]
    `Pure
    (fun cx cy _ -> J.EBin (J.Asr, cx, cy));
  register_un_prims
    [ "%int_neg"; "caml_int32_neg"; "caml_nativeint_neg" ]
    `Pure
    (fun cx _ -> to_int (J.EUn (J.Neg, cx)));
  register_bin_prim "caml_eq_float" `Pure (fun cx cy _ ->
      bool (J.EBin (J.EqEqEq, cx, cy)));
  register_bin_prim "caml_neq_float" `Pure (fun cx cy _ ->
      bool (J.EBin (J.NotEqEq, cx, cy)));
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
  register_tern_prims
    [ "caml_array_unsafe_set"
    ; "caml_array_unsafe_set_float"
    ; "caml_floatarray_unsafe_set"
    ; "caml_array_unsafe_set_addr"
    ]
    `Mutator
    (fun cx cy cz _ -> J.EBin (J.Eq, Mlvalue.Array.field cx cy, cz));
  register_un_prims [ "caml_alloc_dummy"; "caml_alloc_dummy_float" ] `Pure (fun _ _ ->
      J.array []);
  register_un_prims
    [ "caml_int_of_float"
    ; "caml_int32_of_float"
    ; "caml_nativeint_of_float"
    ; "caml_js_to_int32"
    ; "caml_js_to_nativeint"
    ]
    `Pure
    (fun cx _loc -> to_int cx);
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

  register_tern_prim "caml_js_set" `Mutator (fun cx cy cz _ ->
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

let remove_unused_tail_args ctx exact trampolined args =
  if exact && not trampolined
  then
    let has_unused_tail_args =
      List.fold_left
        ~f:(fun _ x -> Var.equal x ctx.Ctx.deadcode_sentinal)
        ~init:false
        args
    in
    if has_unused_tail_args
    then
      List.fold_right
        ~f:(fun x args ->
          match args with
          | [] when Var.equal x ctx.Ctx.deadcode_sentinal -> []
          | _ -> x :: args)
        ~init:[]
        args
    else args
  else args

let keep_name x =
  match Code.Var.get_name x with
  | None -> false
  | Some "" -> false
  | Some s ->
      (* "switcher" is emitted by the OCaml compiler when compiling
        pattern matching, it does not help much to keep it in the
        generated js, let's drop it *)
      (not (generated_name s)) && not (String.starts_with s ~prefix:"jsoo_")

let rec translate_expr ctx loc x e level : (_ * J.statement_list) Expr_builder.t =
  let open Expr_builder in
  match e with
  | Apply { f; args; exact } ->
      let trampolined = Var.Set.mem x ctx.Ctx.trampolined_calls in
      let args = remove_unused_tail_args ctx exact trampolined args in
      let* () = info ~need_loc:true mutator_p in
      let in_cps = Var.Set.mem x ctx.Ctx.in_cps in
      let* args = list_map (access ~ctx) args in
      let* f = access ~ctx f in
      return (apply_fun ctx f args exact trampolined in_cps loc, [])
  | Block (tag, a, array_or_not, _mut) ->
      let* contents =
        list_map
          (fun x ->
            let* cx = access ~ctx x in
            let cx =
              match cx with
              | J.EVar (J.V v) ->
                  if Var.equal v ctx.deadcode_sentinal
                  then J.ElementHole
                  else J.Element cx
              | _ -> J.Element cx
            in
            return cx)
          (Array.to_list a)
      in
      let x =
        match array_or_not with
        | Array -> Mlvalue.Array.make ~tag ~args:contents
        | NotArray | Unknown -> Mlvalue.Block.make ~tag ~args:contents
      in
      return (x, [])
  | Field (x, n, _) ->
      let* cx = access ~ctx x in
      let* () = info mutable_p in
      return (Mlvalue.Block.field cx n, [])
  | Closure (args, ((pc, _) as cont), cloc) ->
      let loc = source_location cloc in
      let fv = Addr.Map.find pc ctx.freevars in
      let clo = compile_closure ctx cont cloc in
      let clo =
        J.EFun
          ( None
          , J.fun_ (List.map args ~f:(fun v -> J.V v)) (Js_simpl.function_body clo) loc )
      in
      let* () = info (Const, fv) in
      return (clo, [])
  | Constant c -> return (constant ~ctx c level)
  | Special (Alias_prim name) ->
      let prim = Share.get_prim (runtime_fun ctx) name ctx.Ctx.share in
      return (prim, [])
  | Prim (Extern "debugger", _) ->
      let ins =
        if Config.Flag.debugger () then J.Debugger_statement else J.Empty_statement
      in
      return (int 0, [ ins, loc ])
  | Prim (p, l) ->
      let* res =
        match p, l with
        | Vectlength, [ x ] ->
            let* cx = access' ~ctx x in
            return (Mlvalue.Array.length cx)
        | Array_get, [ x; y ] ->
            let* cx = access' ~ctx x in
            let* cy = access' ~ctx y in
            let* () = info mutable_p in
            return (Mlvalue.Array.field cx cy)
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
              return (Parse_js.parse_expr lex)
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
            let* args = list_map (fun x -> access' ~ctx x) l in
            return (J.array args)
        | Extern "%caml_js_opt_call", f :: o :: l ->
            let* () = info ~need_loc:true mutator_p in
            let* cf = access' ~ctx f in
            let* co = access' ~ctx o in
            let* args = list_map (fun x -> access' ~ctx x) l in
            return (J.call (J.dot cf (Utf8_string.of_string_exn "call")) (co :: args) loc)
        | Extern "%caml_js_opt_fun_call", f :: l ->
            let* () = info ~need_loc:true mutator_p in
            let* cf = access' ~ctx f in
            let* args = list_map (fun x -> access' ~ctx x) l in
            return (J.call cf args loc)
        | Extern "%caml_js_opt_meth_call", o :: Pc (NativeString (Utf m)) :: l ->
            let* () = info ~need_loc:true mutator_p in
            let* co = access' ~ctx o in
            let* args = list_map (fun x -> access' ~ctx x) l in
            return (J.call (J.dot co m) args loc)
        | Extern "%caml_js_opt_meth_call", _ -> assert false
        | Extern "%caml_js_opt_new", c :: l ->
            let* () = info ~need_loc:true mutator_p in
            let* cc = access' ~ctx c in
            let* args =
              list_map
                (fun x ->
                  let* cx = access' ~ctx x in
                  return (J.Arg cx))
                l
            in
            return (J.ENew (cc, (if List.is_empty args then None else Some args), loc))
        | Extern "caml_js_get", [ Pv o; Pc (NativeString (Utf f)) ] when J.is_ident' f ->
            let* co = access ~ctx o in
            let* () = info mutable_p in
            return (J.dot co f)
        | Extern "caml_js_set", [ Pv o; Pc (NativeString (Utf f)); v ] when J.is_ident' f
          ->
            let* co = access ~ctx o in
            let* cv = access' ~ctx v in
            let* () = info mutator_p in
            return (J.EBin (J.Eq, J.dot co f, cv))
        | Extern "caml_js_delete", [ Pv o; Pc (NativeString (Utf f)) ] when J.is_ident' f
          ->
            let* co = access ~ctx o in
            let* () = info mutator_p in
            return (J.EUn (J.Delete, J.dot co f))
            (*
           This is only useful for debugging:
         {[
           | Extern "caml_js_get", [ _; Pc (String _) ] -> assert false
           | Extern "caml_js_set", [ _; Pc (String s); _ ] -> assert false
           | Extern "caml_js_delete", [ _; Pc (String _) ] -> assert false
           ]}
        *)
        | Extern "%caml_js_opt_object", fields ->
            let rec build_fields l =
              match l with
              | [] -> return []
              | Pc (NativeString (Utf nm)) :: x :: r ->
                  let* cx = access' ~ctx x in
                  let* r' = build_fields r in
                  let p_name = if J.is_ident' nm then J.PNI nm else J.PNS nm in
                  return (J.Property (p_name, cx) :: r')
              | _ -> assert false
            in
            let* fields = build_fields fields in
            return (J.EObj fields)
        | Extern "caml_alloc_dummy_function", [ _; size ] ->
            (* Removed in Ocaml 5.2 *)
            let* i =
              let* cx = access' ~ctx size in
              return
                (match cx with
                | J.ENum i -> Targetint.to_int_exn (J.Num.to_targetint i)
                | _ -> assert false)
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
              J.EFun (Some f, J.fun_ args [ J.Return_statement (Some call, J.N), J.N ] J.N)
            in
            return e
        | Extern "caml_alloc_dummy_function", _ -> assert false
        | Extern ("%resume" | "%perform" | "%reperform"), _ ->
            assert (not (cps_transform ()));
            if not !(ctx.effect_warning)
            then (
              Warning.warn
                `Effect_handlers_without_effect_backend
                "your program contains effect handlers; you should probably run \
                 js_of_ocaml with option '--effects=cps'@.";
              ctx.effect_warning := true);
            let name = "jsoo_effect_not_supported" in
            let prim = Share.get_prim (runtime_fun ctx) name ctx.Ctx.share in
            let* () = info ~need_loc:true (kind (Primitive.kind name)) in
            return (J.call prim [] loc)
        | Extern "caml_string_notequal", [ a; b ] when Config.Flag.use_js_string () ->
            let* cx = access' ~ctx a in
            let* cy = access' ~ctx b in
            return (bool (J.EBin (J.NotEqEq, cx, cy)))
        | Extern "caml_string_equal", [ a; b ] when Config.Flag.use_js_string () ->
            let* cx = access' ~ctx a in
            let* cy = access' ~ctx b in
            return (bool (J.EBin (J.EqEqEq, cx, cy)))
        | Extern "caml_string_concat", [ a; b ] when Config.Flag.use_js_string () ->
            let* ca = access' ~ctx a in
            let* cb = access' ~ctx b in
            let rec add ca cb =
              match cb with
              | J.EBin (J.Plus, cb1, cb2) -> J.EBin (J.Plus, add ca cb1, cb2)
              | _ -> J.EBin (J.Plus, ca, cb)
            in
            return (add ca cb)
        | Extern name_orig, l -> (
            let name = Primitive.resolve name_orig in
            match internal_prim name with
            | Some f -> f name l ctx loc
            | None ->
                if String.starts_with name ~prefix:"%"
                then failwith (Printf.sprintf "Unresolved internal primitive: %s" name);
                let prim = Share.get_prim (runtime_fun ctx) name ctx.Ctx.share in
                let* () = info ~need_loc:true (kind (Primitive.kind name)) in
                let* args = list_map (fun x -> access' ~ctx x) l in
                return (J.call prim args loc))
        | Not, [ x ] ->
            let* cx = access' ~ctx x in
            return (J.EBin (J.Minus, one, cx))
        | Lt, [ x; y ] ->
            let* cx = access' ~ctx x in
            let* cy = access' ~ctx y in
            return (bool (J.EBin (J.LtInt, cx, cy)))
        | Le, [ x; y ] ->
            let* cx = access' ~ctx x in
            let* cy = access' ~ctx y in
            return (bool (J.EBin (J.LeInt, cx, cy)))
        | Eq, [ x; y ] ->
            let* cx = access' ~ctx x in
            let* cy = access' ~ctx y in
            return (bool (J.EBin (J.EqEqEq, cx, cy)))
        | Neq, [ x; y ] ->
            let* cx = access' ~ctx x in
            let* cy = access' ~ctx y in
            return (bool (J.EBin (J.NotEqEq, cx, cy)))
        | IsInt, [ x ] ->
            let* cx = access' ~ctx x in
            return (bool (Mlvalue.is_immediate cx))
        | Ult, [ x; y ] ->
            let* cx = access' ~ctx x in
            let* cy = access' ~ctx y in
            return (bool (J.EBin (J.LtInt, unsigned cx, unsigned cy)))
        | (Vectlength | Array_get | Not | IsInt | Eq | Neq | Lt | Le | Ult), _ ->
            assert false
      in
      return (res, [])

and translate_instr ctx expr_queue loc instr =
  let open Expr_builder in
  match instr with
  | Assign (x, y) ->
      flush_queue
        expr_queue
        loc
        (let* cy = access ~ctx y in
         let* () = info mutator_p in
         let* loc = statement_loc loc in
         return [ J.Expression_statement (J.EBin (J.Eq, J.EVar (J.V x), cy)), loc ])
  | Let (x, e) -> (
      let e' = translate_expr ctx loc x e 0 in
      match ctx.Ctx.live.(Var.idx x), e with
      | 0, _ ->
          (* deadcode is off *)
          flush_queue
            expr_queue
            loc
            (let* ce, instrs = e' in
             let* loc = statement_loc loc in
             return (instrs @ [ J.Expression_statement ce, loc ]))
      | 1, Constant (Int _ | Int32 _ | NativeInt _ | Float _) ->
          enqueue expr_queue x loc e'
      | 1, _
        when Config.Flag.compact () && ((not (Config.Flag.pretty ())) || not (keep_name x))
        -> enqueue expr_queue x loc e'
      | _ ->
          flush_queue
            expr_queue
            loc
            (let* ce, instrs = e' in
             let* loc = statement_loc loc in
             return (instrs @ [ J.variable_declaration [ J.V x, (ce, loc) ], loc ])))
  | Set_field (x, n, _, y) ->
      flush_queue
        expr_queue
        loc
        (let* cx = access ~ctx x in
         let* cy = access ~ctx y in
         let* () = info mutator_p in
         let* loc = statement_loc loc in
         return
           [ J.Expression_statement (J.EBin (J.Eq, Mlvalue.Block.field cx n, cy)), loc ])
  | Offset_ref (x, n) ->
      (* FIX: may overflow.. *)
      flush_queue
        expr_queue
        loc
        (let* cx = access ~ctx x in
         let expr = Mlvalue.Block.field cx 0 in
         let expr' =
           match n with
           | 1 -> J.EUn (J.IncrA, expr)
           | -1 -> J.EUn (J.DecrA, expr)
           | n when n < 0 (* *) -> J.EBin (J.MinusEq, expr, int (-n))
           | n (*   n > 0    *) -> J.EBin (J.PlusEq, expr, int n)
         in
         let* () = info mutator_p in
         let* loc = statement_loc loc in
         return [ J.Expression_statement expr', loc ])
  | Array_set (x, y, z) ->
      flush_queue
        expr_queue
        loc
        (let* cx = access ~ctx x in
         let* cy = access ~ctx y in
         let* cz = access ~ctx z in
         let* () = info mutator_p in
         let* loc = statement_loc loc in
         return
           [ J.Expression_statement (J.EBin (J.Eq, Mlvalue.Array.field cx cy, cz)), loc ])
  | Event _ -> [], expr_queue

and translate_instrs_rev (ctx : Ctx.t) loc expr_queue instrs acc_rev muts_map =
  match instrs with
  | [] -> loc, acc_rev, expr_queue
  | Let (_, Closure _) :: _ ->
      let names, pcs, all, rem, loc = collect_closures loc instrs in
      let fvs =
        List.fold_left pcs ~init:Code.Var.Set.empty ~f:(fun acc pc ->
            Code.Var.Set.union acc (Addr.Map.find pc ctx.freevars))
      in
      let muts =
        List.fold_left pcs ~init:Code.Var.Set.empty ~f:(fun acc pc ->
            Code.Var.Set.union acc (Code.Addr.Map.find pc ctx.Ctx.mutated_vars))
      in
      let names =
        List.fold_left names ~init:Code.Var.Set.empty ~f:(fun acc name ->
            Code.Var.Set.add name acc)
      in
      assert (List.compare_length_with all ~len:(Code.Var.Set.cardinal names) = 0);
      assert (Code.Var.Set.(is_empty (diff muts fvs)));
      let old_muts_map = muts_map in
      let muts_map_l =
        Code.Var.Set.elements muts
        |> List.map ~f:(fun x ->
               ( x
               , match Code.Var.Map.find_opt x old_muts_map with
                 | None -> Code.Var.fork x
                 | Some x' -> x' ))
      in
      let muts_map =
        List.fold_left muts_map_l ~init:old_muts_map ~f:(fun acc (x, x') ->
            Var.Map.add x x' acc)
      in
      (* Rewrite blocks using well-scoped closure variables *)
      let ctx =
        if List.is_empty muts_map_l
        then ctx
        else
          let subst = Subst.from_map muts_map in
          let p, _visited =
            List.fold_left
              pcs
              ~init:(ctx.blocks, Addr.Set.empty)
              ~f:(fun (blocks, visited) pc ->
                Subst.Excluding_Binders.cont' subst pc blocks visited)
          in
          { ctx with blocks = p }
      in
      let vd kind = function
        | [] -> []
        | l -> [ J.variable_declaration ~kind (List.rev l), J.N ]
      in
      (* flush variables part of closures env from the queue *)
      let bind_fvs, bind_fvs_muts, expr_queue =
        let expr_queue, vars, lets =
          Code.Var.Set.fold
            (fun v (expr_queue, vars, lets) ->
              assert (not (Code.Var.Set.mem v names));
              let (px, cx, locx), expr_queue = Q.access_queue_loc ~ctx expr_queue loc v in
              let flushed = Code.Var.Set.(equal (snd px) (singleton v)) in
              match
                ( flushed
                , Code.Var.Map.find_opt v muts_map
                , Code.Var.Map.find_opt v old_muts_map )
              with
              | true, None, _ -> expr_queue, vars, lets
              | (true | false), Some _, Some _ -> expr_queue, vars, lets
              | (true | false), Some v', None ->
                  let lets = (J.V v', (cx, locx)) :: lets in
                  expr_queue, vars, lets
              | false, None, _ ->
                  let vars = (J.V v, (cx, locx)) :: vars in
                  expr_queue, vars, lets)
            (Code.Var.Set.diff fvs names)
            (expr_queue, [], [])
        in
        vars, lets, expr_queue
      in
      (* Mutually recursive functions need to be properly scoped. *)
      let bind_fvs_rec, funs_rev, expr_queue =
        List.fold_left
          all
          ~init:([], [], expr_queue)
          ~f:(fun (mut_rec, st_rev, expr_queue) (i, loc) ->
            let x' =
              match i with
              | Let (x', _) -> x'
              | _ -> assert false
            in
            let l, expr_queue = translate_instr ctx expr_queue loc i in
            if Code.Var.Set.mem x' fvs
            then
              let mut_rec =
                match Code.Var.Map.find_opt x' muts_map with
                | None -> mut_rec
                | Some v' -> (J.V v', (J.EVar (J.V x'), J.N)) :: mut_rec
              in
              match l with
              | [ i ] -> mut_rec, i :: st_rev, expr_queue
              | [] ->
                  let (_px, cx, locx), expr_queue =
                    Q.access_queue_loc ~ctx expr_queue loc x'
                  in
                  ( mut_rec
                  , (J.variable_declaration [ J.V x', (cx, locx) ], locx) :: st_rev
                  , expr_queue )
              | _ :: _ :: _ -> assert false
            else mut_rec, List.rev_append l st_rev, expr_queue)
      in
      let acc_rev = vd Var bind_fvs @ acc_rev in
      let acc_rev = vd Let bind_fvs_muts @ acc_rev in
      let acc_rev = funs_rev @ acc_rev in
      let acc_rev = vd Let bind_fvs_rec @ acc_rev in
      translate_instrs_rev ctx loc expr_queue rem acc_rev muts_map
  | Event loc :: rem ->
      translate_instrs_rev ctx (J.Pi loc) expr_queue rem acc_rev muts_map
  | instr :: rem ->
      let st, expr_queue = translate_instr ctx expr_queue loc instr in
      let acc_rev = List.rev_append st acc_rev in
      translate_instrs_rev ctx loc expr_queue rem acc_rev muts_map

and translate_instrs (ctx : Ctx.t) loc expr_queue instrs =
  let loc, st_rev, expr_queue =
    translate_instrs_rev (ctx : Ctx.t) loc expr_queue instrs [] Var.Map.empty
  in
  loc, List.rev st_rev, expr_queue

(* Compile loops. *)
and compile_block st loc (queue : Q.queue) (pc : Addr.t) scope_stack ~fall_through =
  if
    (not (Q.is_empty queue))
    && (Structure.is_loop_header st.structure pc
       ||
       (* Do not inline expressions across block boundaries when --no-inline is used
              Single-stepping in the debugger should work better this way (fixes #290). *)
       not (Config.Flag.inline ()))
  then
    let never, code = compile_block st loc Q.empty pc scope_stack ~fall_through in
    never, Q.flush_all queue loc code
  else
    match Structure.is_loop_header st.structure pc with
    | false -> compile_block_no_loop st loc queue pc scope_stack ~fall_through
    | true ->
        if debug () then Format.eprintf "@[<hv 2>for(;;) {@,";
        let never_body, body =
          let lab = J.Label.fresh () in
          let lab_used = ref false in
          let exit_branch_used = ref false in
          let scope_stack = (pc, (lab, lab_used, Loop)) :: scope_stack in
          let scope_stack =
            match fall_through with
            | Block fall_through ->
                (fall_through, (lab, lab_used, Exit_loop exit_branch_used)) :: scope_stack
            | Return -> scope_stack
          in
          let never_body, body =
            compile_block_no_loop st loc queue pc scope_stack ~fall_through:(Block pc)
          in
          if debug () then Format.eprintf "}@]@,";
          let for_loop =
            J.For_statement (J.Left None, None, None, Js_simpl.block body), loc
          in
          let label = if !lab_used then Some lab else None in
          let for_loop =
            match label with
            | None -> for_loop
            | Some label -> J.Labelled_statement (label, for_loop), J.N
          in
          (not !exit_branch_used) && never_body, [ for_loop ]
        in
        never_body, body

(* Compile block. Loops have already been handled. *)
and compile_block_no_loop st loc queue (pc : Addr.t) ~fall_through scope_stack =
  if pc < 0 then assert false;
  if Addr.Set.mem pc !(st.visited_blocks)
  then (
    Format.eprintf "Trying to compile a block twice !!!! %d@." pc;
    assert false);
  if debug () then Format.eprintf "Compiling block %d@;" pc;
  st.visited_blocks := Addr.Set.add pc !(st.visited_blocks);
  let block = Addr.Map.find pc st.ctx.blocks in
  let loc, seq, queue = translate_instrs st.ctx loc queue block.body in
  let nbbranch =
    match block.branch with
    | Switch (_, a) ->
        (* Build an artifical dtree with the correct layout so that
           [Dtree.nbbranch dtree pc] is correct *)
        let dtree = DTree.build_switch a in
        fun pc -> DTree.nbbranch dtree pc
    | Cond (_, a, b) ->
        let dtree = DTree.build_if a b in
        fun pc -> DTree.nbbranch dtree pc
    | _ -> fun _pc -> 0
  in
  let new_scopes =
    Structure.get_edges st.dom pc
    |> Addr.Set.elements
    |> List.filter ~f:(fun pc' ->
           nbbranch pc' >= 2 || Structure.is_merge_node st.structure pc')
    |> Structure.sort_in_post_order st.structure
  in
  let rec loop ~scope_stack ~fall_through l =
    match l with
    | [] -> compile_conditional st queue ~fall_through loc block.branch scope_stack
    | x :: xs -> (
        let l = J.Label.fresh () in
        let used = ref false in
        let scope_stack = (x, (l, used, Forward)) :: scope_stack in
        let _never_inner, inner = loop ~scope_stack ~fall_through:(Block x) xs in
        let never, code = compile_block st loc Q.empty x scope_stack ~fall_through in
        match !used with
        | true -> never, [ J.Labelled_statement (l, (J.Block inner, J.N)), J.N ] @ code
        | false -> never, inner @ code)
  in
  let never_after, after = loop ~scope_stack ~fall_through new_scopes in
  never_after, seq @ after

and compile_decision_tree kind st scope_stack loc_before cx loc_after dtree ~fall_through
    =
  (* Some changes here may require corresponding changes
     in function [DTree.fold_cont] above. *)
  let rec loop loc cx scope_stack : _ -> bool * _ = function
    | DTree.Branch (l, cont) ->
        if debug ()
        then
          Format.eprintf
            "@[<hv 2>case %s(%a)  {@;"
            kind
            Format.(
              pp_print_list
                ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
                (fun fmt pc -> Format.fprintf fmt "%d" pc))
            l;
        let never, code =
          compile_branch st loc_after Q.empty cont scope_stack ~fall_through
        in
        if debug () then Format.eprintf "}@]@;";
        never, code
    | DTree.If (cond, cont1, cont2) ->
        let never1, iftrue = loop loc_after cx scope_stack cont1 in
        let never2, iffalse = loop loc_after cx scope_stack cont2 in
        let e' =
          match cond with
          | IsTrue -> cx
          | CEq n -> J.EBin (J.EqEqEq, targetint n, cx)
          | CLt n -> J.EBin (J.LtInt, targetint n, cx)
          | CLe n -> J.EBin (J.LeInt, targetint n, cx)
        in
        ( never1 && never2
        , Js_simpl.if_statement
            ~function_end:(fun () -> source_location st.cloc)
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
        let lab = J.Label.fresh () in
        let label_used = ref false in
        let exit_branch_used = ref false in
        let scope_stack =
          match fall_through with
          | Block fall_through ->
              (fall_through, (lab, label_used, Exit_switch exit_branch_used))
              :: scope_stack
          | Return -> scope_stack
        in
        let last =
          let case = a.(last_index) in
          let never, code = loop loc_after cx scope_stack (Branch case) in
          if not never then all_never := false;
          code
        in
        let rec loop_cases pos acc =
          let ((ints, _cont) as case) = a.(pos) in
          let never, code = loop loc_after cx scope_stack (Branch case) in
          if not never then all_never := false;
          let _, acc =
            List.fold_right ints ~init:(true, acc) ~f:(fun i (first, acc) ->
                ( false
                , ( int i
                  , if first
                    then if not never then code @ [ Break_statement None, J.N ] else code
                    else [] )
                  :: acc ))
          in
          if pos = 0 then acc else loop_cases (pred pos) acc
        in
        let l = loop_cases (last_index - 1) [] in
        let code =
          if !label_used
          then
            [ ( J.Labelled_statement
                  (lab, (J.Switch_statement (cx, l, Some last, []), loc))
              , loc )
            ]
          else [ J.Switch_statement (cx, l, Some last, []), loc ]
        in
        (not !exit_branch_used) && !all_never, code
  in
  let cx, binds, loc =
    match cx with
    | (J.EVar _ | _) when DTree.nbcomp dtree <= 1 -> cx, [], loc_before
    | _ ->
        let v = J.V (Code.Var.fresh ()) in
        J.EVar v, [ J.variable_declaration [ v, (cx, loc_before) ], J.N ], loc_after
  in
  let never, code = loop loc cx scope_stack dtree in
  never, binds @ code

and compile_conditional st queue ~fall_through loc last scope_stack : _ * _ =
  (if debug ()
   then
     match last with
     | Branch _ | Poptrap _ -> ()
     | Pushtrap _ -> Format.eprintf "@[<hv 2>try {@;"
     | Return _ -> Format.eprintf "ret;@;"
     | Raise _ -> Format.eprintf "raise;@;"
     | Stop -> Format.eprintf "stop;@;"
     | Cond (x, _, _) -> Format.eprintf "@[<hv 2>cond(%a){@;" Code.Var.print x
     | Switch (x, _) -> Format.eprintf "@[<hv 2>switch(%a){@;" Code.Var.print x);
  let ctx = st.ctx in
  let res =
    match last with
    | Return x ->
        let open Expr_builder in
        let instrs =
          let* cx = access ~ctx x in
          let return_expr =
            if Var.equal st.ctx.deadcode_sentinal x then None else Some cx
          in
          let loc' =
            match cx with
            | ECall _ -> (
                (* We usually don't have a good locations for tail
                   calls, so use the end of the function instead *)
                match source_location st.cloc with
                | J.N -> loc
                | loc -> loc)
            | _ -> loc
          in
          let* loc = statement_loc loc in
          return [ J.Return_statement (return_expr, loc'), loc ]
        in
        true, flush_all queue loc instrs
    | Raise (x, k) ->
        let open Expr_builder in
        let instrs =
          let* cx = access ~ctx x in
          let* loc = statement_loc loc in
          return (throw_statement st.ctx cx k loc)
        in
        true, flush_all queue loc instrs
    | Stop ->
        let e_opt =
          if st.ctx.Ctx.should_export then Some (s_var Global_constant.exports) else None
        in
        true, Q.flush_all queue loc [ J.Return_statement (e_opt, loc), loc ]
    | Branch cont -> compile_branch st loc queue cont scope_stack ~fall_through
    | Pushtrap (c1, x, e1) ->
        let never_body, body =
          compile_branch st J.N Q.empty c1 scope_stack ~fall_through
        in
        if debug () then Format.eprintf "@,}@]@,@[<hv 2>catch {@;";
        let exn_var, never_handler, handler =
          match st.ctx.Ctx.live.(Var.idx x) with
          | 0 ->
              let never_handler, handler =
                compile_branch st J.U Q.empty e1 scope_stack ~fall_through
              in
              x, never_handler, handler
          | n ->
              let handler_var = Code.Var.fork x in
              let wrapped_exn =
                J.call
                  (Share.get_prim
                     (runtime_fun st.ctx)
                     "caml_wrap_exception"
                     st.ctx.Ctx.share)
                  [ J.EVar (J.V handler_var) ]
                  J.N
              in
              let instrs, queue =
                if List.mem ~eq:Var.equal x (snd e1)
                then (
                  assert (n = 1);
                  Q.enqueue Q.empty const_p x wrapped_exn J.U None [])
                else
                  [ J.variable_declaration [ J.V x, (wrapped_exn, J.U) ], J.N ], Q.empty
              in
              let never_handler, handler =
                compile_branch st J.U queue e1 scope_stack ~fall_through
              in
              handler_var, never_handler, instrs @ handler
        in
        ( never_body && never_handler
        , Q.flush_all
            queue
            loc
            [ ( J.Try_statement (body, Some (Some (J.param' (J.V exn_var)), handler), None)
              , loc )
            ] )
    | Poptrap cont ->
        let never, code = compile_branch st J.N Q.empty cont scope_stack ~fall_through in
        never, Q.flush_all queue loc code
    | Cond (x, c1, c2) ->
        let cx, loc_before, queue =
          Expr_builder.get queue loc (Expr_builder.access ~ctx x)
        in
        (* We keep track of the location [loc_before] before the
           expression is evaluated and of the location after [loc]. *)
        let never, b =
          compile_decision_tree
            "Bool"
            st
            scope_stack
            ~fall_through
            loc_before
            cx
            loc
            (DTree.build_if c1 c2)
        in
        never, Q.flush_all queue loc_before b
    | Switch (x, a1) ->
        let cx, loc_before, queue =
          Expr_builder.get queue loc (Expr_builder.access ~ctx x)
        in
        (* We keep track of the location [loc_before] before the
           expression is evaluated and of the location after [loc]. *)
        let never, code =
          compile_decision_tree
            "Int"
            st
            scope_stack
            ~fall_through
            loc_before
            cx
            loc
            (DTree.build_switch a1)
        in
        never, Q.flush_all queue loc_before code
  in
  (if debug ()
   then
     match last with
     | Branch _ | Poptrap _ | Return _ | Raise _ | Stop -> ()
     | Switch _ | Cond _ | Pushtrap _ -> Format.eprintf "}@]@;");
  res

and compile_argument_passing ctx loc queue (pc, args) back_edge continuation =
  if List.is_empty args
  then continuation queue
  else
    let block = Addr.Map.find pc ctx.Ctx.blocks in
    parallel_renaming ctx loc back_edge block.params args continuation queue

and compile_branch st loc queue ((pc, _) as cont) scope_stack ~fall_through : bool * _ =
  let scope =
    List.find_map ~f:(fun (pc', x) -> if pc = pc' then Some x else None) scope_stack
  in
  let back_edge =
    List.exists
      ~f:(function
        | pc', (_, _, Loop) when pc' = pc -> true
        | _ -> false)
      scope_stack
  in
  compile_argument_passing st.ctx loc queue cont back_edge (fun queue ->
      if
        match fall_through with
        | Block pc' -> pc' = pc
        | Return -> false
      then false, Q.flush_all queue loc []
      else
        match scope with
        | Some (l, used, Loop) ->
            (* Loop back to the beginning of the loop using continue.
               We can skip the label if we're not inside a nested loop. *)
            let rec can_skip_label scope_stack =
              match scope_stack with
              | [] -> assert false
              | (_, (_, _, (Forward | Exit_switch _))) :: rem -> can_skip_label rem
              | (pc', (l', _, (Loop | Exit_loop _))) :: rem ->
                  J.Label.equal l' l && (pc = pc' || can_skip_label rem)
            in
            let label =
              if can_skip_label scope_stack
              then None
              else (
                used := true;
                Some l)
            in
            if debug ()
            then
              if Option.is_none label
              then Format.eprintf "continue;@,"
              else Format.eprintf "continue (%d);@," pc;
            true, Q.flush_all queue loc [ J.Continue_statement label, J.N ]
        | Some (l, used, (Exit_loop branch_used | Exit_switch branch_used)) ->
            (* Break out of a loop or switch (using Break)
               We can skip the label if we're not inside a nested loop or switch.
            *)
            branch_used := true;
            let rec can_skip_label scope_stack =
              match scope_stack with
              | [] -> assert false
              | (_, (_, _, Forward)) :: rem -> can_skip_label rem
              | (pc', (l', _, (Loop | Exit_loop _ | Exit_switch _))) :: rem ->
                  J.Label.equal l' l && (pc = pc' || can_skip_label rem)
            in
            let label =
              if can_skip_label scope_stack
              then None
              else (
                used := true;
                Some l)
            in
            if debug ()
            then
              if Option.is_none label
              then Format.eprintf "break;@,"
              else Format.eprintf "break (%d);@," pc;
            true, Q.flush_all queue loc [ J.Break_statement label, J.N ]
        | Some (l, used, Forward) ->
            (* break outside a labelled statement. The label is mandatory in this case. *)
            if debug () then Format.eprintf "(br %d)@;" pc;
            used := true;
            true, Q.flush_all queue loc [ J.Break_statement (Some l), J.N ]
        | None -> compile_block st loc queue pc scope_stack ~fall_through)

and compile_closure ctx (pc, args) (cloc : Parse_info.t option) =
  let st = build_graph ctx pc cloc in
  let current_blocks = Structure.get_nodes st.structure in
  if debug () then Format.eprintf "@[<hv 2>closure {@;";
  let scope_stack = [] in
  let start_loc =
    let block = Addr.Map.find pc ctx.Ctx.blocks in
    match block.body with
    | Event loc :: _ -> J.Pi loc
    | _ -> J.U
  in
  let _never, res =
    compile_branch st start_loc Q.empty (pc, args) scope_stack ~fall_through:Return
  in
  if Addr.Set.cardinal !(st.visited_blocks) <> Addr.Set.cardinal current_blocks
  then (
    let missing = Addr.Set.diff current_blocks !(st.visited_blocks) in
    Format.eprintf "Some blocks not compiled %s!@." (string_of_set missing);
    assert false);
  if debug () then Format.eprintf "}@]@;";
  res

and collect_closures loc l =
  match l with
  | Event loc :: (Let (_, Closure _) :: _ as rem) -> collect_closures (J.Pi loc) rem
  | (Let (x, Closure (_, (pc, _), _)) as i) :: rem ->
      let names', pcs', i', rem', loc' = collect_closures loc rem in
      x :: names', pc :: pcs', (i, loc) :: i', rem', loc'
  | _ -> [], [], [], l, loc

let generate_shared_value ctx =
  let strings =
    ( J.variable_declaration
        ((match ctx.Ctx.exported_runtime with
         | None -> []
         | Some (_, { contents = false }) -> []
         | Some (v, _) ->
             [ ( J.V v
               , ( J.dot
                     (s_var Global_constant.global_object)
                     (Utf8_string.of_string_exn "jsoo_runtime")
                 , J.U ) )
             ])
        @ List.map
            (StringMap.bindings ctx.Ctx.share.Share.vars.Share.byte_strings)
            ~f:(fun (s, v) -> v, (str_js_byte s, J.U))
        @ List.map
            (StringMap.bindings ctx.Ctx.share.Share.vars.Share.utf_strings)
            ~f:(fun (s, v) -> v, (str_js_utf8 s, J.U))
        @ List.map
            (StringMap.bindings ctx.Ctx.share.Share.vars.Share.prims)
            ~f:(fun (s, v) -> v, (runtime_fun ctx s, J.U)))
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
  let res = compile_closure ctx (pc, []) None in
  let res = generate_shared_value ctx @ res in
  if debug () then Format.eprintf "@]@.";
  res

let f
    (p : Code.program)
    ~exported_runtime
    ~live_vars
    ~trampolined_calls
    ~in_cps
    ~should_export
    ~warn_on_unhandled_effect
    ~deadcode_sentinal =
  let p = Structure.norm p in
  let mutated_vars = Freevars.f_mutable p in
  let freevars = Freevars.f p in
  let t' = Timer.make () in
  let share = Share.get ~trampolined_calls ~in_cps ~alias_prims:exported_runtime p in
  let exported_runtime =
    if exported_runtime then Some (Code.Var.fresh_n "runtime", ref false) else None
  in
  let ctx =
    Ctx.initial
      ~warn_on_unhandled_effect
      ~exported_runtime
      ~should_export
      ~deadcode_sentinal
      ~mutated_vars
      ~freevars
      ~in_cps
      p.blocks
      live_vars
      trampolined_calls
      share
  in
  let p = compile_program ctx p.start in
  if times () then Format.eprintf "  code gen.: %a@." Timer.print t';
  p

let init () =
  String.Hashtbl.iter
    (fun name (k, _) -> Primitive.register name k None None)
    internal_primitives
