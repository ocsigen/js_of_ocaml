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
    | Top  (** [x] is live and may be used in any way. *)
    | Live of t IntMap.t
        (** [x] is a live block, of which only the given fields are used
            (possibly none, see [live_block]). *)
    | Dead  (** [x] is dead. *)

  val bot : t

  val top : t

  val live_block : t

  val live_field : int -> t -> t

  val join : t -> t -> t

  val join_diff : t -> t -> t * t
  (** [join_diff l1 l2] returns the join [l] of [l1] and [l2], together
      with a liveness [d] below [l2] such that [l] is the join of [l1]
      and [d]. This is [Dead] when [l2] adds nothing to [l1]; otherwise,
      [d] omits the parts of [l2] which are already in [l1]. *)
end = struct
  type t =
    | Top
    | Live of t IntMap.t
    | Dead

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

  let depth_threshold = 4

  let live_block = Live IntMap.empty

  let live_field i l =
    (* We need to limit the depth of the liveness information,
       otherwise the information can get more and more precise without
       ever converging. Modules are rarely very deeply nested, so this is
       not an issue. *)
    Live
      (IntMap.singleton
         i
         (if depth l > depth_threshold then truncate depth_threshold l else l))

  (** Join the liveness according to lattice structure. *)
  let rec join l1 l2 =
    match l1, l2 with
    | _, Top | Top, _ -> Top
    | Live f1, Live f2 ->
        (* Return [l1] itself when [l2] adds nothing: this avoids
           allocations and makes the solver's equality test cheap *)
        let f =
          IntMap.fold
            (fun i l f ->
              match IntMap.find_opt i f with
              | None -> IntMap.add i l f
              | Some l' ->
                  let l'' = join l' l in
                  if phys_equal l'' l' then f else IntMap.add i l'' f)
            f2
            f1
        in
        if phys_equal f f1 then l1 else Live f
    | Dead, l | l, Dead -> l

  let rec join_diff l1 l2 =
    match l1, l2 with
    | Top, _ | _, Dead -> l1, Dead
    | _, Top -> Top, Top
    | Dead, _ -> l2, l2
    | Live f1, Live f2 ->
        (* The cost is proportional to the size of [l2], which is
           usually itself a small difference, rather than to the size
           of [l1] *)
        let f, d =
          IntMap.fold
            (fun i l ((f, d) as acc) ->
              match IntMap.find_opt i f with
              | None -> IntMap.add i l f, IntMap.add i l d
              | Some l' -> (
                  match join_diff l' l with
                  | _, Dead -> acc
                  | l'', d' -> IntMap.add i l'' f, IntMap.add i d' d))
            f2
            (f1, IntMap.empty)
        in
        if IntMap.is_empty d then l1, Dead else Live f, Live d
end

let iter_with_scope prog f =
  Code.fold_closures
    prog
    (fun scope _ (pc, _) _ () ->
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
          | Assign (_, _)
          | Event _
          | Set_field (_, _, _, _)
          | Offset_ref (_, _)
          | Array_set (_, _, _) -> ())
        block.body)
    prog.blocks;
  defs

let variable_may_escape x (global_info : Global_flow.info) =
  match global_info.info_variable_may_escape.(Var.idx x) with
  | Escape | Escape_constant -> true
  | No -> false

type edge =
  { scope : Var.t list
  ; src : Var.t
  ; targets : Var.Set.t
  }
(** Propagation of liveness between variables: the variables in
    [targets] are at least as live as [src], provided all the functions
    in [scope] are live. *)

(** Compute the propagation edges of the given program: block and
    closure parameters propagate their liveness to the corresponding
    arguments, and likewise for assignments. The table maps each
    variable to the edges it is the source of, and to the edges it is in
    the scope of.

    We use information from global flow to add edges between function
    calls and their return values, and between parameters and
    arguments, at known call sites.

    The dependencies of a variable on the variables used to compute it
    are not materialized: they can be read from its definition (see
    [solver]). *)
let usages prog (global_info : Global_flow.info) : edge list Var.Tbl.t =
  let uses = Var.Tbl.make () [] in
  let add_uses ~scope src targets =
    let e = { scope; src; targets } in
    Var.Tbl.set uses src (e :: Var.Tbl.get uses src);
    List.iter ~f:(fun z -> Var.Tbl.set uses z (e :: Var.Tbl.get uses z)) scope
  in
  let add_use ~scope x y = add_uses ~scope x (Var.Set.singleton y) in
  let add_arg_dep params args =
    List.iter2 ~f:(fun x y -> add_use ~scope:[] x y) params args
  in
  let add_cont_deps (pc, args) =
    match Addr.Map.find_opt pc prog.blocks with
    | Some block -> add_arg_dep block.params args
    | None -> () (* Dead continuation *)
  in
  let add_expr_uses scope x e : unit =
    match e with
    | Apply { f; args; _ } -> (
        match Var.Tbl.get global_info.info_approximation f with
        | Top -> ()
        | Values { known; _ } ->
            Var.Set.iter (* For each known closure value of f *)
              (fun k ->
                (* 1. Look at return values, and add edge between x and these values. *)
                (* 2. Add an edge pairwise between the parameters and arguments *)
                match global_info.info_defs.(Var.idx k) with
                | Expr (Closure (params, _, _)) ->
                    (* If the function is under/over-applied then global flow will mark arguments and return value as escaping.
                       So we only need to consider the case when there is an exact application. *)
                    if List.compare_lengths params args = 0
                    then (
                      (* Both the function and the call-site must be live *)
                      let scope = k :: scope in
                      add_uses ~scope x (Var.Map.find k global_info.info_return_vals);
                      List.iter2 ~f:(fun x y -> add_use ~scope x y) params args)
                | _ -> ())
              known)
    | Closure (_, cont, _) -> add_cont_deps cont
    | Block _ | Field _ | Constant _ | Special _ | Prim _ -> ()
  in
  let add_block_uses scope block =
    (* Add uses from block body *)
    List.iter
      ~f:(fun i ->
        match i with
        | Let (x, e) -> add_expr_uses scope x e
        (* For assignment, propagate liveness from new to old variable like a block parameter *)
        | Assign (x, y) -> add_use ~scope:[] x y
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
  uses

(** Compute the initial liveness of each variable in the program.

    A variable [x] is marked as [Top] if
    + It is used in an impure expression (as defined by [Pure_fun.pure_expr]);
    + It is used in a conditional/switch;
    + It is raised by an exception;
    + It is used in another stateful instruction (like setting a block or array field);
    + Or, it is returned or applied to a function and the global flow analysis marked it as escaping.

    A variable [x] is marked as [Live {i}] if its field [i] is modified
    ([Set_field], [Offset_ref]). Reading a field does not make a block
    live by itself: this depends on whether the value read is live
    (see [solver]).

    The liveness of a variable used in the body of a function only
    holds when this function is live. It is thus recorded in a second
    table, indexed by functions. *)
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
          (update_field (Var.Hashtbl.find_opt h v |> Option.value ~default:Domain.bot) i)
  in
  let live_instruction scope i =
    match i with
    (* If e is impure, set all variables in e as Top. The only exception is for function applications,
       where we may be able to do better. Global flow gives us information about which arguments in
       a function application escape, so set only these as top. Only function applications and
       primitives can be impure. *)
    | Let (_, e) -> (
        if not (Pure_fun.pure_expr pure_funs e)
        then
          match e with
          | Apply { f; args; _ } ->
              add_top scope f;
              List.iter
                ~f:(fun x -> if variable_may_escape x global_info then add_top scope x)
                args
          | Prim (_, args) ->
              List.iter
                ~f:(fun arg ->
                  match arg with
                  | Pv x -> add_top scope x
                  | Pc _ -> ())
                args
          | Block (_, _, _, _, _)
          | Field (_, _, _)
          | Closure (_, _, _)
          | Constant _ | Special _ -> assert false)
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

(** Propagate liveness until a fixpoint is reached.

    A variable [x] which is live makes other variables live:
    + the variables used in its definition: for a block, each field
      variable gets the liveness of the corresponding field of [x]; for
      a field access, the accessed block gets a live field; otherwise,
      the variables are used in an arbitrary way;
    + when [x] is a function, the variables with some initial liveness in
      its body ([scoped_live_vars]);
    + the targets of the edges [x] is the source of;
    + the targets of the edges [x] is in the scope of, which get the
      liveness of the source of the edge.

    To deal efficiently with large blocks, we only propagate liveness
    differences: [pending] holds, for each queued variable, the part
    of its liveness which has not been propagated yet. This is correct
    since all the contributions above either distribute over joins or
    are constant. The latter are only propagated the first time a
    variable is visited. This way, making one more field of a block
    live has a cost independent of the size of the block. *)
let solver ~state ~defs ~uses ~scoped_live_vars (global_info : Global_flow.info) =
  let pending = Var.Tbl.make () Domain.bot in
  let visited = Var.ISet.empty () in
  let queue = Queue.create () in
  let schedule x d =
    (match Var.Tbl.get pending x with
    | Domain.Dead -> Queue.push x queue
    | Top | Live _ -> ());
    Var.Tbl.set pending x (Domain.join (Var.Tbl.get pending x) d)
  in
  let update x l =
    match Domain.join_diff (Var.Tbl.get state x) l with
    | _, Dead -> ()
    | l', d ->
        Var.Tbl.set state x l';
        schedule x d
  in
  let visit x =
    (* [d] is what was added to the liveness of [x] since the last visit *)
    let d = Var.Tbl.get pending x in
    Var.Tbl.set pending x Domain.bot;
    let first = not (Var.ISet.mem visited x) in
    if first then Var.ISet.add visited x;
    (match Var.Tbl.get defs x with
    | Expr (Block (_, vars, _, _, _)) -> (
        match d with
        | Top -> Array.iter ~f:(fun y -> update y Domain.top) vars
        | Live fields ->
            IntMap.iter
              (fun i l -> if i < Array.length vars then update vars.(i) l)
              fields
        | Dead -> assert false)
    | Expr (Field (y, i, _)) -> update y (Domain.live_field i d)
    | Expr (Prim (IsInt _, args)) ->
        if first
        then
          List.iter
            ~f:(fun arg ->
              match arg with
              | Pv y -> update y Domain.live_block
              | Pc _ -> ())
            args
    | Expr (Prim (_, args)) ->
        if first
        then
          List.iter
            ~f:(fun arg ->
              match arg with
              | Pv y -> update y Domain.top
              | Pc _ -> ())
            args
    | Expr (Apply { f; args; _ }) ->
        if first
        then (
          update f Domain.top;
          List.iter
            ~f:(fun y -> if variable_may_escape y global_info then update y Domain.top)
            args)
    | Expr (Constant _ | Closure _ | Special _) | Param -> ());
    (if first
     then
       match Var.Tbl.get scoped_live_vars x with
       | Some h -> Var.Hashtbl.iter update h
       | None -> ());
    List.iter
      ~f:(fun { scope; src; targets } ->
        if
          List.for_all scope ~f:(fun z ->
              match Var.Tbl.get state z with
              | Domain.Dead -> false
              | Top | Live _ -> true)
        then
          if Var.equal src x
          then Var.Set.iter (fun y -> update y d) targets
          else if first
          then
            (* A function in the scope has just become live. If the
               edge was not active up to now, the liveness of the
               source has not been propagated yet. *)
            let l = Var.Tbl.get state src in
            Var.Set.iter (fun y -> update y l) targets)
      (Var.Tbl.get uses x)
  in
  Var.Tbl.iter
    (fun x l ->
      match l with
      | Domain.Dead -> ()
      | Top | Live _ -> schedule x l)
    state;
  while not (Queue.is_empty queue) do
    visit (Queue.pop queue)
  done

(** Replace variables with a sentinel value where their value is not needed:
    + in the dead fields of a live block (whether the variable itself is
      dead or not); blocks that end with dead fields are then truncated
      after their last live field;
    + when a dead variable is returned; or
    + when a dead variable is passed as argument to a function.
 *)
let zero prog pure_funs sentinel live_table =
  let compact_vars vars =
    let i = ref (Array.length vars - 1) in
    while !i >= 0 && Var.equal vars.(!i) sentinel do
      i := !i - 1
    done;
    if !i + 1 < Array.length vars then Array.sub vars ~pos:0 ~len:(!i + 1) else vars
  in
  let is_live v =
    match Var.Tbl.get live_table v with
    | Domain.Dead -> false
    | Top | Live _ -> true
  in
  let zero_var x = if is_live x then x else sentinel in
  let zero_instr instr =
    match instr with
    | Let (x, e) -> (
        match e with
        | Block (start, vars, is_array, mut, desc) -> (
            match Var.Tbl.get live_table x with
            | Live fields ->
                let vars =
                  Array.mapi
                    ~f:(fun i v -> if IntMap.mem i fields then v else sentinel)
                    vars
                  |> compact_vars
                in
                let e = Block (start, vars, is_array, mut, desc) in
                Let (x, e)
            | _ -> instr)
        | Apply ap ->
            let args = List.map ~f:zero_var ap.args in
            Let (x, Apply { ap with args })
        | Field (_, _, _) | Closure (_, _, _) | Constant _ | Prim (_, _) | Special _ ->
            instr)
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
          let live_tc =
            (* Don't break tailcalls, it's needed for generate_closure
               and effects passes.  If the (tail)call is dead, it will
               be eliminated later by the deadcode pass, don't make it live again by
               returning its result. *)
            match Code.last_instr body with
            | Some (Let (x', (Apply _ as e))) ->
                Code.Var.equal x x' && not (Pure_fun.pure_expr pure_funs e)
            | Some _ | None -> false
          in
          if live_tc then Return x else Return (zero_var x)
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
      (fun v es ->
        match es with
        | [] -> ()
        | _ :: _ ->
            Format.eprintf "%a: { " Var.print v;
            List.iter
              ~f:(fun { scope; src; targets } ->
                Var.Set.iter
                  (fun d ->
                    Format.eprintf
                      "(%a, P(%s/%a)) "
                      Var.print
                      d
                      (String.concat
                         ~sep:" "
                         (List.map ~f:(fun x -> Format.asprintf "%a" Var.print x) scope))
                      Var.print
                      src)
                  targets)
              es;
            Format.eprintf "}\n")
      uses

  let print_live_tbl live_table =
    Format.eprintf "Liveness:\n";
    Var.Tbl.iter
      (fun v l -> Format.eprintf "%a: %s\n" Var.print v (live_to_string l))
      live_table
end

(** Add a sentinel variable declaration to the IR. The fresh variable is bound to the integer 0. *)
let add_sentinel p sentinel =
  let instr = Let (sentinel, Constant (Int Targetint.zero)) in
  Code.prepend p [ instr ]

(** Run the liveness analysis and replace dead variables with the given sentinel. *)
let f pure_funs p ~deadcode_sentinel global_info =
  Code.invariant p;
  let t = Timer.make () in
  (* Add sentinel variable *)
  let p = add_sentinel p deadcode_sentinel in
  (* Compute definitions *)
  let defs = definitions p in
  (* Compute initial liveness *)
  let live_table, scoped_live_vars = liveness p pure_funs global_info in
  (* Compute usages *)
  let uses = usages p global_info in
  (* Propagate liveness to dependencies *)
  solver ~state:live_table ~defs ~uses ~scoped_live_vars global_info;
  (* Print debug info *)
  if debug ()
  then (
    Format.eprintf "Before Zeroing:@.";
    Code.Print.program Format.err_formatter (fun _ _ -> "") p;
    Print.print_uses uses;
    Print.print_live_tbl live_table);
  (* Zero out dead fields *)
  let p = zero p pure_funs deadcode_sentinel live_table in
  if debug ()
  then (
    Format.eprintf "After Zeroing:@.";
    Code.Print.program Format.err_formatter (fun _ _ -> "") p);
  if times () then Format.eprintf "  global dead code elim.: %a@." Timer.print t;
  Code.invariant p;
  p
