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

(** Liveness of a variable [x], forming a lattice structure. *)
type live =
  | Top  (** [x] is live and not a block. *)
  | Live of IntSet.t  (** [x] is a live block with a (non-empty) set of live fields. *)
  | Dead  (** [x] is dead. *)

module G = Dgraph.Make_Imperative (Var) (Var.ISet) (Var.Tbl)

module Domain = struct
  type t = live

  let equal l1 l2 =
    match l1, l2 with
    | Top, Top | Dead, Dead -> true
    | Live f1, Live f2 -> IntSet.equal f1 f2
    | Top, (Dead | Live _) | Live _, (Dead | Top) | Dead, (Live _ | Top) -> false

  let bot = Dead

  (** Join the liveness according to lattice structure. *)
  let join l1 l2 =
    match l1, l2 with
    | _, Top | Top, _ -> Top
    | Live f1, Live f2 -> Live (IntSet.union f1 f2)
    | Dead, Live f | Live f, Dead -> Live f
    | Dead, Dead -> Dead
end

module Solver = G.Solver (Domain)

let definitions prog =
  let defs = Var.Tbl.make () Param in
  let set_def x d = Var.Tbl.set defs x d in
  Addr.Map.iter
    (fun _ block ->
      (* Add defs from block body *)
      List.iter
        ~f:(fun (i, _) ->
          match i with
          | Let (x, e) -> set_def x (Expr e)
          | Assign (x, _) -> set_def x Param
          | Set_field (_, _, _) | Offset_ref (_, _) | Array_set (_, _, _) -> ())
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
  | Propagate  (** values of y propagate to x *)

(** Compute the adjacency list for the dependency graph of given program. An edge between
    variables [x] and [y] is marked [Compute] if [x] is used in the definition of [y]. It is marked
    as [Propagate] if [x] is applied as a closure or block argument the parameter [y].

    We use information from global flow to try to add edges between function calls and their return values
    at known call sites. *)
let usages prog (global_info : Global_flow.info) : usage_kind Var.Map.t Var.Tbl.t =
  let uses = Var.Tbl.make () Var.Map.empty in
  let add_use kind x y = Var.Tbl.set uses y (Var.Map.add x kind (Var.Tbl.get uses y)) in
  let add_arg_dep params args =
    List.iter2 ~f:(fun x y -> add_use Propagate x y) params args
  in
  let add_cont_deps (pc, args) =
    match try Some (Addr.Map.find pc prog.blocks) with Not_found -> None with
    | Some block -> add_arg_dep block.params args
    | None -> () (* Dead continuation *)
  in
  let add_expr_uses x e : unit =
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
                      let return_values = Var.Map.find k global_info.info_return_vals in
                      Var.Set.iter (add_use Propagate x) return_values;
                      List.iter2 ~f:(add_use Propagate) params args)
                | _ -> ())
              known);
        add_use Compute x f;
        List.iter
          ~f:(fun a -> if variable_may_escape a global_info then add_use Compute x a)
          args
    | Block (_, vars, _) -> Array.iter ~f:(add_use Compute x) vars
    | Field (z, _) -> add_use Compute x z
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
  Addr.Map.iter
    (fun _ block ->
      (* Add uses from block body *)
      List.iter
        ~f:(fun (i, _) ->
          match i with
          | Let (x, e) -> add_expr_uses x e
          (* For assignment, propagate liveness from new to old variable like a block parameter *)
          | Assign (x, y) -> add_use Propagate x y
          | Set_field (_, _, _) | Offset_ref (_, _) | Array_set (_, _, _) -> ())
        block.body;
      (* Add uses from block branch *)
      match fst block.branch with
      | Return _ | Raise _ | Stop -> ()
      | Branch cont -> add_cont_deps cont
      | Cond (_, cont1, cont2) ->
          add_cont_deps cont1;
          add_cont_deps cont2
      | Switch (_, a) -> Array.iter ~f:add_cont_deps a
      | Pushtrap (cont, _, cont_h) ->
          add_cont_deps cont;
          add_cont_deps cont_h
      | Poptrap cont -> add_cont_deps cont)
    prog.blocks;
  uses

(** Return the set of variables used in a given expression *)
let expr_vars e =
  let vars = Var.Set.empty in
  match e with
  | Apply { f; args; _ } ->
      let vars = Var.Set.add f vars in
      List.fold_left ~f:(fun acc x -> Var.Set.add x acc) ~init:vars args
  | Block (_, params, _) ->
      Array.fold_left ~f:(fun acc x -> Var.Set.add x acc) ~init:vars params
  | Field (z, _) -> Var.Set.add z vars
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
    + Or, it is returned or applied to a function and the global flow analysis marked it as escaping.

    A variable [x[i]] is marked as [Live {i}] if it is used in an instruction where field [i] is referenced or set. *)
let liveness prog pure_funs (global_info : Global_flow.info) =
  let live_vars = Var.Tbl.make () Dead in
  let add_top v = Var.Tbl.set live_vars v Top in
  let add_live_field v i =
    let live_fields =
      match Var.Tbl.get live_vars v with
      | Live fields -> Live (IntSet.add i fields)
      | Top | Dead -> Live (IntSet.singleton i)
    in
    Var.Tbl.set live_vars v live_fields
  in
  let live_instruction i =
    match i with
    (* If e is impure, set all variables in e as Top. The only exception is for function applications,
       where we may be able to do better. Global flow gives us information about which arguments in
       a function application escape, so set only these as top. *)
    | Let (_, e) -> (
        if not (Pure_fun.pure_expr pure_funs e)
        then
          match e with
          | Apply { f; args; _ } ->
              add_top f;
              List.iter
                ~f:(fun x -> if variable_may_escape x global_info then add_top x)
                args
          | Block (_, _, _)
          | Field (_, _)
          | Closure (_, _)
          | Constant _
          | Prim (_, _)
          | Special _ ->
              let vars = expr_vars e in
              Var.Set.iter add_top vars)
    | Set_field (x, i, y) ->
        add_live_field x i;
        add_top y
    | Array_set (x, y, z) ->
        add_top x;
        add_top y;
        add_top z
    | Offset_ref (x, _) -> add_live_field x 0
    (* Assignment can be ignored. Liveness of old variable is just propagated to new variable. See [usages]. *)
    | Assign (_, _) -> ()
  in
  let live_block block =
    List.iter ~f:(fun (i, _) -> live_instruction i) block.body;
    match fst block.branch with
    | Return x -> if variable_may_escape x global_info then add_top x
    | Raise (x, _) -> add_top x
    | Cond (x, _, _) -> add_top x
    | Switch (x, _) -> add_top x
    | Stop | Branch _ | Poptrap _ | Pushtrap _ -> ()
  in
  Addr.Map.iter (fun _ block -> live_block block) prog.blocks;
  Code.traverse
    { Code.fold = Code.fold_children }
    (fun pc () ->
      match Addr.Map.find pc prog.blocks with
      | { branch = Return x, _; _ } -> add_top x
      | _ -> ())
    prog.start
    prog.blocks
    ();
  live_vars

(* Returns the set of variables given a table of variables. *)
let variables deps =
  let vars = Var.ISet.empty () in
  Var.Tbl.iter (fun v _ -> Var.ISet.add vars v) deps;
  vars

(** Propagate liveness of the usages of a variable [x] to [x]. The liveness of [x] is
    defined by joining its current liveness and the contribution of each vairable [y]
    that uses [x]. *)
let propagate uses defs live_vars live_table x =
  (* Variable [y] uses [x] either in its definition ([Compute]) or as a closure/block parameter
      ([Propagate]). In the latter case, the contribution is simply the liveness of [y]. In the former,
       the contribution depends on the liveness of [y] and its definition. *)
  let contribution y usage_kind =
    match usage_kind with
    (* If x is used to compute y, we consider the liveness of y *)
    | Compute -> (
        match Var.Tbl.get live_table y with
        (* If y is dead, then x is dead. *)
        | Dead -> Dead
        (* If y is a live block, then x is the join of liveness fields that are x *)
        | Live fields -> (
            match Var.Tbl.get defs y with
            | Expr (Block (_, vars, _)) ->
                let found = ref false in
                Array.iteri
                  ~f:(fun i v ->
                    if Var.equal v x && IntSet.mem i fields then found := true)
                  vars;
                if !found then Top else Dead
            | Expr (Field (_, i)) -> Live (IntSet.singleton i)
            | _ -> Top)
        (* If y is top and y is a field access, x depends only on that field *)
        | Top -> (
            match Var.Tbl.get defs y with
            | Expr (Field (_, i)) -> Live (IntSet.singleton i)
            | _ -> Top))
    (* If x is used as an argument for parameter y, then contribution is liveness of y *)
    | Propagate -> Var.Tbl.get live_table y
  in
  Var.Map.fold
    (fun y usage_kind live -> Domain.join (contribution y usage_kind) live)
    (Var.Tbl.get uses x)
    (Domain.join (Var.Tbl.get live_vars x) (Var.Tbl.get live_table x))

let solver vars uses defs live_vars =
  let g =
    { G.domain = vars
    ; G.iter_children = (fun f x -> Var.Map.iter (fun y _ -> f y) (Var.Tbl.get uses x))
    }
  in
  Solver.f () (G.invert () g) (propagate uses defs live_vars)

(** Replace each instance of a dead variable with a sentinal value.
  Blocks that end in dead variables are compacted to the first live entry.
  Dead variables are replaced when
    + They appear in a dead field of a block; or
    + They are returned; or
    + They are applied to a function.
 *)
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
    | Dead -> false
    | Top | Live _ -> true
  in
  let zero_var x = if is_live x then x else sentinal in
  let zero_instr instr =
    match instr with
    | Let (x, e) -> (
        match e with
        | Block (start, vars, is_array) -> (
            match Var.Tbl.get live_table x with
            | Live fields ->
                let vars =
                  Array.mapi
                    ~f:(fun i v -> if IntSet.mem i fields then v else sentinal)
                    vars
                  |> compact_vars
                in
                let e = Block (start, vars, is_array) in
                Let (x, e)
            | _ -> instr)
        | Apply ap ->
            let args = List.map ~f:zero_var ap.args in
            Let (x, Apply { ap with args })
        | Field (_, _) | Closure (_, _) | Constant _ | Prim (_, _) | Special _ -> instr)
    | Assign (_, _) | Set_field (_, _, _) | Offset_ref (_, _) | Array_set (_, _, _) ->
        instr
  in
  let zero_block block =
    (* Analyze block instructions *)
    let body = List.map ~f:(fun (instr, loc) -> zero_instr instr, loc) block.body in
    (* Analyze branch *)
    let branch =
      (* Zero out return values in last instruction, otherwise do nothing. *)
      match block.branch with
      | Return x, loc ->
          let tc =
            (* We don't want to break tailcalls. *)
            match List.last body with
            | Some (Let (x', Apply _), _) when Code.Var.equal x' x -> true
            | Some _ | None -> false
          in
          if tc then Return x, loc else Return (zero_var x), loc
      | Raise (_, _), _
      | Stop, _
      | Branch _, _
      | Cond (_, _, _), _
      | Switch (_, _), _
      | Pushtrap (_, _, _), _
      | Poptrap _, _ -> block.branch
    in
    { block with body; branch }
  in
  let blocks = prog.blocks |> Addr.Map.map zero_block in
  { prog with blocks }

module Print = struct
  let live_to_string = function
    | Live fields ->
        "live { " ^ IntSet.fold (fun i s -> s ^ Format.sprintf "%d " i) fields "" ^ "}"
    | Top -> "top"
    | Dead -> "dead"

  let print_uses uses =
    Format.eprintf "Usages:\n";
    Var.Tbl.iter
      (fun v ds ->
        Format.eprintf "%a: { " Var.print v;
        Var.Map.iter
          (fun d k ->
            Format.eprintf
              "(%a, %s) "
              Var.print
              d
              (match k with
              | Compute -> "C"
              | Propagate -> "P"))
          ds;
        Format.eprintf "}\n")
      uses

  let print_liveness live_vars =
    Format.eprintf "Liveness:\n";
    Var.Tbl.iter
      (fun v l -> Format.eprintf "%a: %s\n" Var.print v (live_to_string l))
      live_vars

  let print_live_tbl live_table =
    Format.eprintf "Liveness with dependencies:\n";
    Var.Tbl.iter
      (fun v l -> Format.eprintf "%a: %s\n" Var.print v (live_to_string l))
      live_table
end

(** Add a sentinal variable declaration to the IR. The fresh variable is assigned to `undefined`. *)
let add_sentinal p sentinal =
  let instr, loc = Let (sentinal, Special Undefined), noloc in
  Code.prepend p [ instr, loc ]

(** Run the liveness analysis and replace dead variables with the given sentinal. *)
let f p ~deadcode_sentinal global_info =
  Code.invariant p;
  let t = Timer.make () in
  (* Add sentinal variable *)
  let p = add_sentinal p deadcode_sentinal in
  (* Compute definitions *)
  let defs = definitions p in
  (* Compute usages *)
  let uses = usages p global_info in
  (* Compute initial liveness *)
  let pure_funs = Pure_fun.f p in
  let live_vars = liveness p pure_funs global_info in
  (* Propagate liveness to dependencies *)
  let vars = variables uses in
  let live_table = solver vars uses defs live_vars in
  (* Print debug info *)
  if debug ()
  then (
    Format.eprintf "Before Zeroing:\n";
    Code.Print.program (fun _ _ -> "") p;
    Print.print_liveness live_vars;
    Print.print_uses uses;
    Print.print_live_tbl live_table);
  (* Zero out dead fields *)
  let p = zero p deadcode_sentinal live_table in
  if debug ()
  then (
    Format.printf "After Zeroing:\n";
    Code.Print.program (fun _ _ -> "") p);
  if times () then Format.eprintf "  deadcode dgraph.: %a@." Timer.print t;
  p
