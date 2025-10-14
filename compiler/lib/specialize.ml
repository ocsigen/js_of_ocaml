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
open Code

let times = Debug.find "times"

let stats = Debug.find "stats"

let debug_stats = Debug.find "stats-debug"

let add_event loc instrs =
  match loc with
  | Some loc -> Event loc :: instrs
  | None -> instrs

let unknown_apply = function
  | Let (_, Apply { f = _; args = _; exact = false }) -> true
  | _ -> false

let specialize_apply opt_count shape update_def =
  let rec loop x f args shape loc (acc, free_pc, extra) =
    match (shape : Shape.t) with
    | Top | Block _ -> Let (x, Apply { f; args; exact = false }) :: acc, free_pc, extra
    | Function { arity; res; _ } ->
        let nargs = List.length args in
        if arity = nargs
        then (
          incr opt_count;
          let expr = Apply { f; args; exact = true } in
          update_def x expr;
          Let (x, expr) :: acc, free_pc, extra)
        else if arity > nargs
        then (
          (* under application *)
          incr opt_count;
          let missing = Array.init (arity - nargs) ~f:(fun _ -> Code.Var.fresh ()) in
          let missing = Array.to_list missing in
          let block =
            let params' = List.map missing ~f:Code.Var.fork in
            let return' = Code.Var.fresh () in
            let args = args @ params' in
            assert (List.length args = arity);
            { params = params'
            ; body = add_event loc [ Let (return', Apply { f; args; exact = true }) ]
            ; branch = Return return'
            }
          in
          let expr = Closure (missing, (free_pc, missing), None) in
          update_def x expr;
          Let (x, expr) :: acc, free_pc + 1, (free_pc, block) :: extra)
        else (
          assert (arity < nargs);
          (* over application *)
          incr opt_count;
          let v = Code.Var.fresh () in
          let args, rest = List.take arity args in
          let exact_expr = Apply { f; args; exact = true } in
          let body =
            (* Reversed *)
            add_event loc (Let (v, exact_expr) :: acc)
          in
          loop x v rest res loc (body, free_pc, extra))
  in
  fun i (((body_rev, free_pc, extra) as acc), loc) ->
    match i with
    | Let (x, Apply { f; args; exact = false }) -> loop x f args (shape f) loc acc
    | _ -> i :: body_rev, free_pc, extra

let specialize_instrs ~shape ~update_def opt_count p =
  let blocks, free_pc =
    let specialize_instrs = specialize_apply opt_count shape update_def in
    Addr.Map.fold
      (fun pc block (blocks, free_pc) ->
        if List.exists ~f:unknown_apply block.body
        then
          let (body_rev, free_pc, extra), _ =
            List.fold_left
              block.body
              ~init:(([], free_pc, []), None)
              ~f:(fun acc i ->
                match i with
                | Event loc ->
                    let (body_rev, free_pc, extra), _ = acc in
                    (i :: body_rev, free_pc, extra), Some loc
                | _ -> specialize_instrs i acc, None)
          in
          let blocks =
            List.fold_left extra ~init:blocks ~f:(fun blocks (pc, b) ->
                Addr.Map.add pc b blocks)
          in
          Addr.Map.add pc { block with Code.body = List.rev body_rev } blocks, free_pc
        else blocks, free_pc)
      p.blocks
      (p.blocks, p.free_pc)
  in
  { p with blocks; free_pc }

let f ~shape ~update_def p =
  Code.invariant p;
  let previous_p = p in
  let t = Timer.make () in
  let opt_count = ref 0 in
  let p =
    if Config.Flag.optcall () then specialize_instrs ~shape ~update_def opt_count p else p
  in
  if times () then Format.eprintf "  optcall: %a@." Timer.print t;
  if stats () then Format.eprintf "Stats - optcall: %d@." !opt_count;
  if debug_stats ()
  then Code.check_updates ~name:"optcall" previous_p p ~updates:!opt_count;
  Code.invariant p;
  p

(***)

module Simple_block : sig
  type t

  val hash : t -> int

  val equal : t -> t -> bool

  val make : block -> t
end = struct
  type t = block

  let subst_cont s (pc, arg) = pc, List.map arg ~f:s

  let expr s e =
    match e with
    | Constant _ -> e
    | Apply { f; args; exact } -> Apply { f = s f; args = List.map args ~f:s; exact }
    | Block (n, a, k, mut) -> Block (n, Array.map a ~f:s, k, mut)
    | Field (x, n, typ) -> Field (s x, n, typ)
    | Closure (l, pc, loc) -> Closure (l, subst_cont s pc, loc)
    | Special _ -> e
    | Prim (p, l) ->
        Prim
          ( p
          , List.map l ~f:(fun x ->
                match x with
                | Pv x -> Pv (s x)
                | Pc _ -> x) )

  let instr s d i =
    match i with
    | Let (x, e) ->
        let x = d x in
        Let (x, expr s e)
    | Assign (x, y) -> Assign (s x, s y)
    | Set_field (x, n, typ, y) -> Set_field (s x, n, typ, s y)
    | Offset_ref (x, n) -> Offset_ref (s x, n)
    | Array_set (x, y, z) -> Array_set (s x, s y, s z)
    | Event _ -> Event Parse_info.zero

  let instrs s d l = List.map l ~f:(fun i -> instr s d i)

  let last s l =
    match l with
    | Stop -> l
    | Branch cont -> Branch (subst_cont s cont)
    | Pushtrap (cont1, x, cont2) -> Pushtrap (subst_cont s cont1, s x, subst_cont s cont2)
    | Return x -> Return (s x)
    | Raise (x, k) -> Raise (s x, k)
    | Cond (x, cont1, cont2) -> Cond (s x, subst_cont s cont1, subst_cont s cont2)
    | Switch (x, conts) -> Switch (s x, Array.map conts ~f:(fun cont -> subst_cont s cont))
    | Poptrap cont -> Poptrap (subst_cont s cont)

  let block s d block =
    let params = List.map block.params ~f:s in
    let body = instrs s d block.body in
    let branch = last s block.branch in
    { params; body; branch }

  let make blk =
    let t = Var.Hashtbl.create 17 in
    let s x =
      match Var.Hashtbl.find_opt t x with
      | None -> x
      | Some x -> x
    in
    let d x =
      let v = Var.of_idx (-Var.Hashtbl.length t) in
      Var.Hashtbl.add t x v;
      v
    in
    block s d blk

  let instr_equal a b =
    match a, b with
    | Event _, Event _ -> true
    | Event _, _ | _, Event _ -> false
    | a, b -> Poly.equal a b

  let equal a b =
    List.equal ~eq:Var.equal a.params b.params
    && List.equal ~eq:instr_equal a.body b.body
    && Poly.equal a.branch b.branch

  let hash (x : block) = Hashtbl.hash x
end

module SBT = Hashtbl.Make (Simple_block)

(* For switches, at this point, we know that this it is sufficient to
   check the [pc]. *)
let equal (pc, _) (pc', _) = pc = pc'

type switch_to_cond =
  [ `All_equals
  | `Distinguished of int
  | `Splitted of int
  | `Splitted_shifted of int * int
  ]

let find_outlier_index arr : [ switch_to_cond | `Many_cases ] =
  let len = Array.length arr in
  let rec find w i =
    if i >= len
    then `All_equals
    else if equal arr.(i) w
    then find w (i + 1)
    else `Distinguished i
  in
  let a0 = arr.(0) in
  match find a0 0 with
  | `All_equals as res -> res
  | `Distinguished i -> (
      match find arr.(i) i with
      | `All_equals ->
          if i = 1
          then `Distinguished 0
          else if i = len - 1
          then `Distinguished i
          else `Splitted i
      | `Distinguished j -> (
          match find a0 j with
          | `All_equals -> if j = i + 1 then `Distinguished i else `Splitted_shifted (i, j)
          | `Distinguished _ -> `Many_cases))

let optimize_switch_to_cond block x l (opt : switch_to_cond) =
  match opt with
  | `All_equals -> { block with branch = Branch l.(0) }
  | `Distinguished i ->
      let c = Var.fresh () in
      { block with
        body =
          block.body @ [ Let (c, Prim (Eq, [ Pc (Int (Targetint.of_int_exn i)); Pv x ])) ]
      ; branch = Cond (c, l.(i), l.((i + 1) mod Array.length l))
      }
  | `Splitted i ->
      let c = Var.fresh () in
      { block with
        body =
          block.body @ [ Let (c, Prim (Lt, [ Pv x; Pc (Int (Targetint.of_int_exn i)) ])) ]
      ; branch = Cond (c, l.(i - 1), l.(i))
      }
  | `Splitted_shifted (i, j) ->
      let shifted = Var.fresh () in
      let c = Var.fresh () in
      { block with
        body =
          block.body
          @ [ Let
                ( shifted
                , Prim
                    ( Extern ("%int_sub", None)
                    , [ Pv x; Pc (Int (Targetint.of_int_exn i)) ] ) )
            ; Let (c, Prim (Ult, [ Pv shifted; Pc (Int (Targetint.of_int_exn (j - i))) ]))
            ]
      ; branch = Cond (c, l.(i), l.(j))
      }

let switches p =
  let previous_p = p in
  let t = Timer.make () in
  let opt_count = ref 0 in
  let p =
    { p with
      blocks =
        Addr.Map.fold
          (fun pc block blocks ->
            match block.branch with
            | Switch (x, l) -> (
                match find_outlier_index l with
                | #switch_to_cond as opt ->
                    incr opt_count;
                    let block = optimize_switch_to_cond block x l opt in
                    Addr.Map.add pc block blocks
                | `Many_cases ->
                    let t = SBT.create 0 in
                    let rewrite = ref Addr.Set.empty in
                    let l =
                      Array.map l ~f:(fun ((pc, _) as cont) ->
                          let block = Code.Addr.Map.find pc blocks in
                          if List.compare_length_with block.body ~len:7 <= 0
                          then (
                            let sb = Simple_block.make block in
                            match SBT.find_opt t sb with
                            | Some cont' when not (equal cont' cont) ->
                                rewrite := Addr.Set.add (fst cont') !rewrite;
                                cont'
                            | Some _ | None ->
                                SBT.add t sb cont;
                                cont)
                          else cont)
                    in
                    if not (Addr.Set.is_empty !rewrite)
                    then (
                      incr opt_count;
                      let blocks =
                        Addr.Set.fold
                          (fun pc blocks ->
                            let block = Code.Addr.Map.find pc blocks in
                            Addr.Map.add
                              pc
                              { block with
                                body =
                                  List.filter
                                    ~f:(function
                                      | Event _ -> false
                                      | _ -> true)
                                    block.body
                              }
                              blocks)
                          !rewrite
                          blocks
                      in
                      match find_outlier_index l with
                      | #switch_to_cond as opt ->
                          let block = optimize_switch_to_cond block x l opt in
                          Addr.Map.add pc block blocks
                      | `Many_cases ->
                          Addr.Map.add pc { block with branch = Switch (x, l) } blocks)
                    else blocks)
            | _ -> blocks)
          p.blocks
          p.blocks
    }
  in
  if times () then Format.eprintf "  switches: %a@." Timer.print t;
  if stats () then Format.eprintf "Stats - switches: %d@." !opt_count;
  if debug_stats ()
  then Code.check_updates ~name:"switches" previous_p p ~updates:!opt_count;
  Deadcode.remove_unused_blocks p
