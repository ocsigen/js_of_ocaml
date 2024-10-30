(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
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

open Code
open Stdlib

let debug = Debug.find "globaldeadcode"

let times = Debug.find "times"

(** Definition of a variable [x]. *)
type def =
  | Expr of expr  (** [x] is defined by an expression. *)
  | Param  (** [x] is a block or closure parameter. *)

module Domain : sig
  (** Liveness of a variable [x], forming a lattice structure. *)
  type t = private
    | Top  (** [x] is live and not a block. *)
    | Live of t IntMap.t
        (** [x] is a live block with a (non-empty) set of live fields. *)
    | Dead  (** [x] is dead. *)

  val equal : t -> t -> bool

  val bot : t

  val top : t

  val live_field : int -> t -> t

  val join : t -> t -> t
end = struct
  type t =
    | Top
    | Live of t IntMap.t
    | Dead

  let rec equal l1 l2 =
    match l1, l2 with
    | Top, Top | Dead, Dead -> true
    | Live f1, Live f2 -> IntMap.equal equal f1 f2
    | Top, (Dead | Live _) | Live _, (Dead | Top) | Dead, (Live _ | Top) -> false

  let bot = Dead

  let top = Top

  let rec depth l =
    match l with
    | Top | Dead -> 0
    | Live f -> 1 + IntMap.fold (fun _ l' acc -> max (depth l') acc) f 0

  let rec truncate depth l =
    match l with
    | Top | Dead -> l
    | Live f ->
        if depth = 0 then Top else Live (IntMap.map (fun l' -> truncate (depth - 1) l') f)

  let depth_treshold = 4

  let live_field i l =
    (* We need to limit the depth of the liveness information,
       otherwise the information can get more and more precise without
       ever converging. Modules are rarely very deeply nested, so this is
       not an issue. *)
    Live
      (IntMap.singleton
         i
         (if depth l > depth_treshold then truncate depth_treshold l else l))

  (** Join the liveness according to lattice structure. *)
  let rec join l1 l2 =
    match l1, l2 with
    | _, Top | Top, _ -> Top
    | Live f1, Live f2 -> Live (IntMap.union (fun _ l1 l2 -> Some (join l1 l2)) f1 f2)
    | Dead, Live f | Live f, Dead -> Live f
    | Dead, Dead -> Dead
end

let iter_with_scope prog f =
  Code.fold_closures
    prog
    (fun scope _ (pc, _) () ->
      Code.traverse
        { fold = fold_children }
        (fun pc () -> f scope (Addr.Map.find pc prog.blocks))
        pc
        prog.blocks
        ())
    ()

let definitions prog =
  let defs = Var.Tbl.make () Param in
  let set_def x d = Var.Tbl.set defs x d in
  Addr.Map.iter
    (fun _ block ->
      (* Add defs from block body *)
      List.iter
        ~f:(fun i ->
          match i with
          | Let (x, e) -> set_def x (Expr e)
          | Assign (x, _) -> set_def x Param
          | Event _ | Set_field (_, _, _, _) | Offset_ref (_, _) | Array_set (_, _, _) ->
              ())
        block.body)
    prog.blocks;
  defs

let variable_may_escape x (global_info : Global_flow.info) =
  match global_info.info_variable_may_escape.(Var.idx x) with
  | Escape | Escape_constant -> true
  | No -> false

(** Type of variable usage. *)
type usage_kind =
  | Compute  (** variable y is used to compute x *)
  | Propagate of
      { scope : Var.t list
      ; src : Var.t
      }  (** values of y propagate to x when the scope is live *)
  | Scope  (** variable x is defined in function y *)

(** Compute the adjacency list for the dependency graph of given program. An edge between
    variables [x] and [y] is marked [Compute] if [x] is used in the definition of [y]. It
    is marked as [Propagate] if [x] is applied as a closure or block argument the
    parameter [y].

    We use information from global flow to try to add edges between function calls and
    their return values at known call sites. *)
let usages prog (global_info : Global_flow.info) scoped_live_vars :
    (usage_kind * Var.Set.t) list Var.Tbl.t =
  let uses = Var.Tbl.make () [] in
  let add_uses kind x vars =
    let p = kind, vars in
    Var.Tbl.set uses x (p :: Var.Tbl.get uses x);
    match kind with
    | Propagate { scope; _ } ->
        List.iter ~f:(fun z -> Var.Tbl.set uses z (p :: Var.Tbl.get uses z)) scope
    | _ -> ()
  in
  let add_use kind x y = add_uses kind x (Var.Set.singleton y) in
  let add_arg_dep params args =
    List.iter2 ~f:(fun x y -> add_use (Propagate { scope = []; src = x }) x y) params args
  in
  let add_cont_deps (pc, args) =
    match try Some (Addr.Map.find pc prog.blocks) with Not_found -> None with
    | Some block -> add_arg_dep block.params args
    | None -> () (* Dead continuation *)
  in
  let add_expr_uses scope x e : unit =
    match e with
    | Apply { f; args; _ } ->
        (match Var.Tbl.get global_info.info_approximation f with
        | Top -> ()
        | Values { known; _ } ->
            Var.Set.iter (* For each known closure value of f *)
              (fun k ->
                (* 1. Look at return values, and add edge between x and these values. *)
                (* 2. Add an edge pairwise between the parameters and arguments *)
                match global_info.info_defs.(Var.idx k) with
                | Expr (Closure (params, _)) ->
                    (* If the function is under/over-applied then global flow will mark arguments and return value as escaping.
                       So we only need to consider the case when there is an exact application. *)
                    if List.compare_lengths params args = 0
                    then (
                      (* Both the function and the call-site must be live *)
                      let scope = k :: scope in
                      add_uses
                        (Propagate { scope; src = x })
                        x
                        (Var.Map.find k global_info.info_return_vals);
                      List.iter2
                        ~f:(fun x y -> add_use (Propagate { scope; src = x }) x y)
                        params
                        args)
                | _ -> ())
              known);
        add_use Compute x f;
        List.iter
          ~f:(fun a -> if variable_may_escape a global_info then add_use Compute x a)
          args
    | Block (_, vars, _, _) -> Array.iter ~f:(add_use Compute x) vars
    | Field (z, _, _) -> add_use Compute x z
    | Constant _ -> ()
    | Special _ -> ()
    | Closure (_, cont) -> add_cont_deps cont
    | Prim (_, args) ->
        List.iter
          ~f:(fun arg ->
            match arg with
            | Pv v -> add_use Compute x v
            | Pc _ -> ())
          args
  in
  let add_block_uses scope block =
    (* Add uses from block body *)
    List.iter
      ~f:(fun i ->
        match i with
        | Let (x, e) -> add_expr_uses scope x e
        (* For assignment, propagate liveness from new to old variable like a block parameter *)
        | Assign (x, y) -> add_use (Propagate { scope = []; src = x }) x y
        | Event _ | Set_field (_, _, _, _) | Offset_ref (_, _) | Array_set (_, _, _) -> ())
      block.body;
    (* Add uses from block branch *)
    match block.branch with
    | Return _ | Raise _ | Stop -> ()
    | Branch cont -> add_cont_deps cont
    | Cond (_, cont1, cont2) ->
        add_cont_deps cont1;
        add_cont_deps cont2
    | Switch (_, a) -> Array.iter ~f:add_cont_deps a
    | Pushtrap (cont, _, cont_h) ->
        add_cont_deps cont;
        add_cont_deps cont_h
    | Poptrap cont -> add_cont_deps cont
  in
  iter_with_scope prog (fun f block ->
      add_block_uses
        (match f with
        | Some f -> [ f ]
        | None -> [])
        block);
  Var.Tbl.iter
    (fun scope h ->
      match h with
      | None -> ()
      | Some h -> Var.Hashtbl.iter (fun x _ -> add_use Scope scope x) h)
    scoped_live_vars;
  uses

(** Return the set of variables used in a given expression *)
let expr_vars e =
  let vars = Var.Set.empty in
  match e with
  | Apply { f; args; _ } ->
      let vars = Var.Set.add f vars in
      List.fold_left ~f:(fun acc x -> Var.Set.add x acc) ~init:vars args
  | Block (_, params, _, _) ->
      Array.fold_left ~f:(fun acc x -> Var.Set.add x acc) ~init:vars params
  | Field (z, _, _) -> Var.Set.add z vars
  | Prim (_, args) ->
      List.fold_left
        ~f:(fun acc v ->
          match v with
          | Pv v -> Var.Set.add v acc
          | Pc _ -> acc)
        ~init:vars
        args
  (* We can ignore closures. We want the set of previously defined variables used
     in the expression, so not parameters. The continuation may use some variables
     but we will add these when we visit the body *)
  | Constant _ | Closure (_, _) | Special _ -> vars

(** Compute the initial liveness of each variable in the program.

    A variable [x] is marked as [Top] if
    + It is used in an impure expression (as defined by [Pure_fun.pure_expr]);
    + It is used in a conditonal/switch;
    + It is raised by an exception;
    + It is used in another stateful instruction (like setting a block or array field);
    + Or, it is returned or applied to a function and the global flow analysis marked it
      as escaping.

    A variable [x[i]] is marked as [Live {i}] if it is used in an instruction where field
    [i] is referenced or set. *)
let liveness prog pure_funs (global_info : Global_flow.info) =
  let live_vars = Var.Tbl.make () Domain.bot in
  let scoped_live_vars = Var.Tbl.make () None in
  let get_hashtbl scope =
    match Var.Tbl.get scoped_live_vars scope with
    | Some h -> h
    | None ->
        let h = Var.Hashtbl.create 8 in
        Var.Tbl.set scoped_live_vars scope (Some h);
        h
  in
  let add_top scope v =
    match scope with
    | None -> Var.Tbl.set live_vars v Domain.top
    | Some scope ->
        let h = get_hashtbl scope in
        Var.Hashtbl.replace h v Domain.top
  in
  let add_live_field scope v i =
    let update_field l i = Domain.join l (Domain.live_field i Domain.top) in
    match scope with
    | None -> Var.Tbl.set live_vars v (update_field (Var.Tbl.get live_vars v) i)
    | Some scope ->
        let h = get_hashtbl scope in
        Var.Hashtbl.replace
          h
          v
          (update_field (try Var.Hashtbl.find h v with Not_found -> Domain.bot) i)
  in
  let live_instruction scope i =
    match i with
    (* If e is impure, set all variables in e as Top. The only exception is for function applications,
       where we may be able to do better. Global flow gives us information about which arguments in
       a function application escape, so set only these as top. *)
    | Let (_, e) -> (
        if not (Pure_fun.pure_expr pure_funs e)
        then
          match e with
          | Apply { f; args; _ } ->
              add_top scope f;
              List.iter
                ~f:(fun x -> if variable_may_escape x global_info then add_top scope x)
                args
          | Block (_, _, _, _)
          | Field (_, _, _)
          | Closure (_, _)
          | Constant _
          | Prim (_, _)
          | Special _ ->
              let vars = expr_vars e in
              Var.Set.iter (fun x -> add_top scope x) vars)
    | Set_field (x, i, _, y) ->
        add_live_field scope x i;
        add_top scope y
    | Array_set (x, y, z) ->
        add_top scope x;
        add_top scope y;
        add_top scope z
    | Offset_ref (x, _) -> add_live_field scope x 0
    (* Assignment can be ignored. Liveness of old variable is just propagated to new variable. See [usages]. *)
    | Event _ | Assign (_, _) -> ()
  in
  let live_block scope block =
    List.iter ~f:(fun i -> live_instruction scope i) block.body;
    match block.branch with
    | Return x -> if variable_may_escape x global_info then add_top scope x
    | Raise (x, _) -> add_top scope x
    | Cond (x, _, _) -> add_top scope x
    | Switch (x, _) -> add_top scope x
    | Stop | Branch _ | Poptrap _ | Pushtrap _ -> ()
  in
  iter_with_scope prog live_block;
  live_vars, scoped_live_vars

(* Returns the set of variables given a table of variables. *)
let variables deps =
  let vars = Var.ISet.empty () in
  Var.Tbl.iter (fun v _ -> Var.ISet.add vars v) deps;
  vars

(** Propagate liveness of the usages of a variable [x] to [x]. The liveness of [x] is
    defined by joining its current liveness and the contribution of each vairable [y] that
    uses [x]. *)
let propagate defs scoped_live_vars ~state ~dep:y ~target:x ~action:usage_kind =
  (* Variable [y] uses [x] either in its definition ([Compute]) or as a closure/block parameter
      ([Propagate]). In the latter case, the contribution is simply the liveness of [y]. In the former,
       the contribution depends on the liveness of [y] and its definition. *)
  match usage_kind with
  (* If x is used to compute y, we consider the liveness of y *)
  | Compute -> (
      match Var.Tbl.get state y with
      (* If y is dead, then x is dead. *)
      | Domain.Dead -> Domain.bot
      (* If y is a live block, then x is the join of liveness fields that are x *)
      | Live fields as l -> (
          match Var.Tbl.get defs y with
          | Expr (Block (_, vars, _, _)) ->
              let live = ref Domain.bot in
              Array.iteri
                ~f:(fun i v ->
                  if Var.equal v x
                  then
                    match IntMap.find_opt i fields with
                    | Some l -> live := Domain.join !live l
                    | None -> ())
                vars;
              !live
          | Expr (Field (_, i, _)) -> Domain.live_field i l
          | _ -> Domain.top)
      (* If y is top and y is a field access, x depends only on that field *)
      | Top -> (
          match Var.Tbl.get defs y with
          | Expr (Field (_, i, _)) -> Domain.live_field i Domain.top
          | _ -> Domain.top))
  (* If x is used as an argument for parameter y, then contribution is liveness of y *)
  | Propagate { scope; src } ->
      if
        List.for_all scope ~f:(fun z ->
            match Var.Tbl.get state z with
            | Dead -> false
            | _ -> true)
      then Var.Tbl.get state src
      else Domain.bot
  | Scope -> (
      match Var.Tbl.get state y with
      | Dead -> Domain.bot
      | _ -> (
          match Var.Tbl.get scoped_live_vars y with
          | Some h -> Var.Hashtbl.find h x
          | None -> assert false))

module Solver =
  Dgraph.Solver (Var) (Var.ISet) (Var.Tbl)
    (struct
      type t = usage_kind
    end)
    (Domain)

let solver vars uses defs live_vars scoped_live_vars =
  let g =
    { Solver.domain = vars
    ; iter_children =
        (fun f x ->
          List.iter
            ~f:(fun (usage_kind, vars) -> Var.Set.iter (fun y -> f y usage_kind) vars)
            (Var.Tbl.get uses x))
    }
  in
  Solver.f ~state:live_vars g (propagate defs scoped_live_vars)

(** Replace each instance of a dead variable with a sentinal value. Blocks that end in
    dead variables are compacted to the first live entry. Dead variables are replaced when
    + They appear in a dead field of a block; or
    + They are returned; or
    + They are applied to a function. *)
let zero prog sentinal live_table =
  let compact_vars vars =
    let i = ref (Array.length vars - 1) in
    while !i >= 0 && Var.equal vars.(!i) sentinal do
      i := !i - 1
    done;
    if !i + 1 < Array.length vars then Array.sub vars ~pos:0 ~len:(!i + 1) else vars
  in
  let is_live v =
    match Var.Tbl.get live_table v with
    | Domain.Dead -> false
    | Top | Live _ -> true
  in
  let zero_var x = if is_live x then x else sentinal in
  let zero_instr instr =
    match instr with
    | Let (x, e) -> (
        match e with
        | Block (start, vars, is_array, mut) -> (
            match Var.Tbl.get live_table x with
            | Live fields ->
                let vars =
                  Array.mapi
                    ~f:(fun i v -> if IntMap.mem i fields then v else sentinal)
                    vars
                  |> compact_vars
                in
                let e = Block (start, vars, is_array, mut) in
                Let (x, e)
            | _ -> instr)
        | Apply ap ->
            let args = List.map ~f:zero_var ap.args in
            Let (x, Apply { ap with args })
        | Field (_, _, _) | Closure (_, _) | Constant _ | Prim (_, _) | Special _ -> instr
        )
    | Event _
    | Assign (_, _)
    | Set_field (_, _, _, _)
    | Offset_ref (_, _)
    | Array_set (_, _, _) -> instr
  in
  let zero_block block =
    (* Analyze block instructions *)
    let body = List.map ~f:(fun instr -> zero_instr instr) block.body in
    (* Analyze branch *)
    let branch =
      (* Zero out return values in last instruction, otherwise do nothing. *)
      match block.branch with
      | Return x ->
          let tc =
            (* We don't want to break tailcalls. *)
            match List.last body with
            | Some (Let (x', Apply _)) when Code.Var.equal x' x -> true
            | Some _ | None -> false
          in
          if tc then Return x else Return (zero_var x)
      | Raise (_, _)
      | Stop | Branch _
      | Cond (_, _, _)
      | Switch (_, _)
      | Pushtrap (_, _, _)
      | Poptrap _ -> block.branch
    in
    { block with body; branch }
  in
  let blocks = prog.blocks |> Addr.Map.map zero_block in
  { prog with blocks }

module Print = struct
  let rec live_to_string = function
    | Domain.Live fields ->
        "live { "
        ^ IntMap.fold
            (fun i l s -> s ^ Format.sprintf "%d: %s; " i (live_to_string l))
            fields
            ""
        ^ "}"
    | Top -> "top"
    | Dead -> "dead"

  let print_uses uses =
    Format.eprintf "Usages:\n";
    Var.Tbl.iter
      (fun v ds ->
        Format.eprintf "%a: { " Var.print v;
        List.iter
          ~f:(fun (k, s) ->
            Var.Set.iter
              (fun d ->
                Format.eprintf
                  "(%a, %s) "
                  Var.print
                  d
                  (match k with
                  | Compute -> "C"
                  | Propagate { scope; src } ->
                      "P("
                      ^ String.concat
                          ~sep:" "
                          (List.map ~f:(fun x -> Format.asprintf "%a" Var.print x) scope)
                      ^ Format.asprintf "/%a)" Var.print src
                  | Scope -> "S"))
              s)
          ds;
        Format.eprintf "}\n")
      uses

  let print_live_tbl live_table =
    Format.eprintf "Liveness:\n";
    Var.Tbl.iter
      (fun v l -> Format.eprintf "%a: %s\n" Var.print v (live_to_string l))
      live_table
end

(** Add a sentinal variable declaration to the IR. The fresh variable is assigned to
    `undefined`. *)
let add_sentinal p sentinal =
  let instr = Let (sentinal, Constant (Int Targetint.zero)) in
  Code.prepend p [ instr ]

(** Run the liveness analysis and replace dead variables with the given sentinal. *)
let f p ~deadcode_sentinal global_info =
  Code.invariant p;
  let t = Timer.make () in
  (* Add sentinal variable *)
  let p = add_sentinal p deadcode_sentinal in
  (* Compute definitions *)
  let defs = definitions p in
  (* Compute initial liveness *)
  let pure_funs = Pure_fun.f p in
  let live_table, scoped_live_vars = liveness p pure_funs global_info in
  (* Compute usages *)
  let uses = usages p global_info scoped_live_vars in
  (* Propagate liveness to dependencies *)
  let vars = variables uses in
  solver vars uses defs live_table scoped_live_vars;
  (* Print debug info *)
  if debug ()
  then (
    Format.eprintf "Before Zeroing:@.";
    Code.Print.program (fun _ _ -> "") p;
    Print.print_uses uses;
    Print.print_live_tbl live_table);
  (* Zero out dead fields *)
  let p = zero p deadcode_sentinal live_table in
  if debug ()
  then (
    Format.eprintf "After Zeroing:@.";
    Code.Print.program (fun _ _ -> "") p);
  if times () then Format.eprintf "  global dead code elim.: %a@." Timer.print t;
  p
