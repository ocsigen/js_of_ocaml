(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2025 Jérome Vouillon
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

(* Tuple unboxing.

   Block parameters and function parameters that are only used through
   field accesses are replaced by the fields actually read. For function
   parameters, the original function becomes a wrapper around an
   unboxed function, and known call sites call the unboxed function
   directly.

   The fields are read when jumping to the block or calling the
   function, rather than at the original accesses. So we only unbox a
   block when it cannot have been mutated in between, and when the
   block itself does not escape (its identity is not observable). *)
open! Stdlib

let debug = Debug.find "unboxing"

let times = Debug.find "times"

let show_stats = Debug.find "stats"

open Code

type stats =
  { mutable continuations : int
  ; mutable call_sites : int
  }

type loc =
  | Block of Addr.t
  | Closure of Var.t * Var.t list

type tuple =
  { size : int
  ; kind : field_type
  ; loc : loc
  ; start_pc : Addr.t
  ; closure_pc : Addr.t option
  ; mutable needed : IntSet.t
  }

let is_unboxing_wrapper p pc =
  let block = Addr.Map.find pc p.blocks in
  let rec check instrs =
    match instrs with
    | Let (_, Field _) :: rem -> check rem
    | [ Let (x, Apply _) ] -> (
        match block.branch with
        | Return y -> Var.equal x y
        | _ -> false)
    | _ -> false
  in
  check block.body

let find_candidates p =
  let tbl = Var.Hashtbl.create 16 in
  let visited = BitSet.create' p.free_pc in
  let allowed_access kind loc =
    match kind, Config.target (), loc with
    | Non_float, _, _ | Float, `Wasm, _ | Float, `JavaScript, Block _ -> true
    | Float, `JavaScript, Closure _ ->
        (* Function parameters are boxed in JavaScript: we would pass
           several boxed floats rather than an array of unboxed
           floats. *)
        false
  in
  let rec traverse closure_pc start_pc pc params =
    if not (BitSet.mem visited pc)
    then (
      BitSet.set visited pc;
      let block = Addr.Map.find pc p.blocks in
      let params =
        List.fold_left
          ~f:(fun s x -> Var.Map.add x (Block pc) s)
          ~init:params
          block.params
      in
      List.iter
        ~f:(fun i ->
          match i with
          | Let (_, Field (x, n, kind))
            when match Var.Map.find_opt x params with
                 | Some loc -> allowed_access kind loc
                 | None -> false ->
              let size = n + 1 in
              let tuple =
                try Var.Hashtbl.find tbl x
                with Not_found ->
                  { size = 0
                  ; loc = Var.Map.find x params
                  ; start_pc
                  ; closure_pc
                  ; kind
                  ; needed = IntSet.empty
                  }
              in
              if tuple.size < size then Var.Hashtbl.replace tbl x { tuple with size }
          | Let (y, Closure (params, (pc', args), _)) when not (is_unboxing_wrapper p pc')
            ->
              traverse
                (Some pc)
                pc'
                pc'
                (List.fold_left
                   ~f:(fun s x -> Var.Map.remove x s)
                   ~init:
                     (List.fold_left
                        ~f:(fun s x -> Var.Map.add x (Closure (y, params)) s)
                        ~init:Var.Map.empty
                        params)
                   args)
          | _ -> ())
        block.body;
      match block.branch with
      | Branch (pc', _) -> traverse closure_pc start_pc pc' params
      | _ ->
          Code.fold_children
            p.blocks
            pc
            (fun pc' () -> traverse closure_pc start_pc pc' Var.Map.empty)
            ())
  in
  traverse None p.start p.start Var.Map.empty;
  tbl

(* Block parameters which are always bound to immutable blocks. Their
   fields cannot change, so we don't need to check for mutations, and
   they can be accessed from nested closures. *)
let immutable_block_parameters p tbl =
  let block_cands = Addr.Hashtbl.create 16 in
  Var.Hashtbl.iter
    (fun x tuple ->
      match tuple.loc with
      | Closure _ -> ()
      | Block pc ->
          let block = Addr.Map.find pc p.blocks in
          let i = Option.get (List.find_index ~f:(fun y -> Var.equal x y) block.params) in
          Addr.Hashtbl.add block_cands pc (x, i))
    tbl;
  let immutable = Var.Hashtbl.create 16 in
  Addr.Map.iter
    (fun _ block ->
      List.iter
        ~f:(fun i ->
          match i with
          | Let (x, Block (_, _, _, Immutable))
          | Let (x, Constant (Tuple _ | Float_array _)) ->
              Var.Hashtbl.replace immutable x ()
          | _ -> ())
        block.body)
    p.blocks;
  let mutable_ = ref Var.Set.empty in
  let check_cont (pc, args) =
    List.iter
      ~f:(fun (x, i) ->
        if not (Var.Hashtbl.mem immutable (List.nth args i))
        then mutable_ := Var.Set.add x !mutable_)
      (Addr.Hashtbl.find_all block_cands pc)
  in
  Addr.Map.iter
    (fun _ block ->
      List.iter
        ~f:(fun i ->
          match i with
          | Let (_, Closure (_, cont, _)) -> check_cont cont
          | _ -> ())
        block.body;
      match block.branch with
      | Branch cont | Poptrap cont -> check_cont cont
      | Cond (_, cont1, cont2) | Pushtrap (cont1, _, cont2) ->
          check_cont cont1;
          check_cont cont2
      | Switch (_, conts) -> Array.iter ~f:check_cont conts
      | Return _ | Raise _ | Stop -> ())
    p.blocks;
  Addr.Hashtbl.fold
    (fun _ (x, _) s -> if Var.Set.mem x !mutable_ then s else Var.Set.add x s)
    block_cands
    Var.Set.empty

(* Check that the tuples do not escape, that we don't access more
   fields than expected, and that all accesses are performed in the
   function where the tuple is bound (unless it is immutable): a
   nested closure can be called at any time, after the tuple has been
   mutated. *)
let check_tuple_accesses p tbl immutable =
  let discard x = Var.Hashtbl.remove tbl x in
  let visited = BitSet.create' p.free_pc in
  let rec traverse fun_pc pc =
    if not (BitSet.mem visited pc)
    then (
      BitSet.set visited pc;
      let block = Addr.Map.find pc p.blocks in
      List.iter
        ~f:(fun i ->
          match i with
          | Let (_, Field (x, n, kind)) -> (
              match Var.Hashtbl.find tbl x with
              | tuple ->
                  if
                    n < tuple.size
                    && Poly.equal kind tuple.kind
                    && (fun_pc = tuple.start_pc || Var.Set.mem x immutable)
                  then tuple.needed <- IntSet.add n tuple.needed
                  else discard x
              | exception Not_found -> ())
          | Let (_, Closure (_, (pc', args), _)) ->
              List.iter ~f:discard args;
              traverse pc' pc'
          | Assign (x, y) ->
              discard x;
              discard y
          | _ -> Freevars.iter_instr_free_vars discard i)
        block.body;
      Freevars.iter_last_free_var discard block.branch;
      Code.fold_children p.blocks pc (fun pc' () -> traverse fun_pc pc') ())
  in
  traverse p.start p.start

let may_mutate i =
  match i with
  | Let (_, Apply _) | Set_field _ | Offset_ref _ | Array_set _ -> true
  | Let
      ( _
      , Prim
          ( Extern
              (("caml_check_bound" | "caml_check_bound_float" | "caml_check_bound_gen"), _)
          , _ ) ) ->
      (* Bound checks are not pure since they can raise, but they
         don't mutate anything *)
      false
  | Let (_, Prim (Extern (name, _), _)) -> not (Primitive.is_pure name)
  | Let (_, (Block _ | Field _ | Closure _ | Constant _ | Prim _ | Special _))
  | Assign _ | Event _ -> false

(* Check that a tuple cannot have been mutated between the start of
   its scope (where its fields are read once unboxed) and its field
   accesses. This is a forward dataflow analysis within each function,
   computing the set of tuples that may have been mutated since the
   start of their scope. Accesses from nested closures have already
   been ruled out by [check_tuple_accesses]. *)
let check_no_mutation p tbl immutable =
  let functions = Addr.Hashtbl.create 16 in
  Var.Hashtbl.iter
    (fun x tuple ->
      if not (Var.Set.mem x immutable)
      then
        let tuples, starts =
          try Addr.Hashtbl.find functions tuple.start_pc
          with Not_found -> Var.Set.empty, Addr.Map.empty
        in
        let starts =
          match tuple.loc with
          | Closure _ -> starts
          | Block pc ->
              Addr.Map.update
                pc
                (fun s -> Some (Var.Set.add x (Option.value ~default:Var.Set.empty s)))
                starts
        in
        Addr.Hashtbl.replace functions tuple.start_pc (Var.Set.add x tuples, starts))
    tbl;
  let mutated = ref Var.Set.empty in
  Addr.Hashtbl.iter
    (fun start_pc (tuples, starts) ->
      let states = Addr.Hashtbl.create 16 in
      let rec traverse pc dirty =
        let dirty =
          match Addr.Map.find_opt pc starts with
          | Some s -> Var.Set.diff dirty s
          | None -> dirty
        in
        let previous = Addr.Hashtbl.find_opt states pc in
        match previous with
        | Some dirty' when Var.Set.subset dirty dirty' -> ()
        | _ ->
            let dirty =
              match previous with
              | Some dirty' -> Var.Set.union dirty dirty'
              | None -> dirty
            in
            Addr.Hashtbl.replace states pc dirty;
            let block = Addr.Map.find pc p.blocks in
            let dirty =
              List.fold_left
                ~f:(fun dirty i ->
                  match i with
                  | Let (_, Field (x, _, _)) when Var.Set.mem x dirty ->
                      mutated := Var.Set.add x !mutated;
                      dirty
                  | _ -> if may_mutate i then tuples else dirty)
                ~init:dirty
                block.body
            in
            Code.fold_children p.blocks pc (fun pc' () -> traverse pc' dirty) ()
      in
      traverse start_pc Var.Set.empty)
    functions;
  let mutated = !mutated in
  Var.Hashtbl.filter_map_inplace
    (fun x tuple -> if Var.Set.mem x mutated then None else Some tuple)
    tbl

let check_call_sites p tbl =
  let relevant_closures =
    Var.Hashtbl.fold
      (fun _ tuple s ->
        match tuple.loc with
        | Block _ -> s
        | Closure (x, _) -> Var.Set.add x s)
      tbl
      Var.Set.empty
  in
  let visited = BitSet.create' p.free_pc in
  let rec traverse pc state =
    if BitSet.mem visited pc
    then state
    else (
      BitSet.set visited pc;
      let block = Addr.Map.find pc p.blocks in
      let state =
        List.fold_left
          ~f:(fun state i ->
            match i with
            | Let (_, Apply { f; exact = true; _ }) when Var.Set.mem f relevant_closures
              ->
                let closures, locations = state in
                Var.Set.add f closures, Addr.Set.add pc locations
            | Let (_, Closure (_, (pc', _), _)) -> traverse pc' state
            | _ -> state)
          ~init:state
          block.body
      in
      Code.fold_children p.blocks pc traverse state)
  in
  let closures, locations =
    Var.Hashtbl.fold
      (fun _ tuple state ->
        match tuple.loc, tuple.closure_pc with
        | Block _, _ -> state
        | Closure _, Some pc -> traverse pc state
        | Closure _, None -> assert false)
      tbl
      (Var.Set.empty, Addr.Set.empty)
  in
  Var.Hashtbl.filter_map_inplace
    (fun _ tuple ->
      match tuple.loc with
      | Closure (x, _) when not (Var.Set.mem x closures) -> None
      | _ -> Some tuple)
    tbl;
  locations

let check_eliminates_tuple p tbl =
  (* 1. Collect all variables defined as Block literals *)
  let is_block = ref Var.Set.empty in
  let visited = BitSet.create' p.free_pc in
  let rec collect pc =
    if not (BitSet.mem visited pc)
    then (
      BitSet.set visited pc;
      let block = Addr.Map.find pc p.blocks in
      List.iter
        ~f:(fun i ->
          match i with
          | Let (x, Block _) | Let (x, Constant (Tuple _ | Float_array _)) ->
              is_block := Var.Set.add x !is_block
          | Let (_, Closure (_, (pc', _), _)) -> collect pc'
          | _ -> ())
        block.body;
      Code.fold_children p.blocks pc (fun pc' () -> collect pc') ())
  in
  collect p.start;
  let is_block = !is_block in
  (* 2. Build reverse maps for efficient lookup *)
  (* block_cands: target_pc -> (candidate var * position) list *)
  let block_cands = ref Addr.Map.empty in
  (* closure_cands: closure var -> (candidate var * position) list *)
  let closure_cands = Var.Hashtbl.create 16 in
  Var.Hashtbl.iter
    (fun x tuple ->
      match tuple.loc with
      | Block pc ->
          let block = Addr.Map.find pc p.blocks in
          let i = Option.get (List.find_index ~f:(fun y -> Var.equal x y) block.params) in
          block_cands :=
            Addr.Map.update
              pc
              (function
                | None -> Some [ x, i ]
                | Some l -> Some ((x, i) :: l))
              !block_cands
      | Closure (f, params) ->
          let i = Option.get (List.find_index ~f:(fun y -> Var.equal x y) params) in
          let existing = try Var.Hashtbl.find closure_cands f with Not_found -> [] in
          Var.Hashtbl.replace closure_cands f ((x, i) :: existing))
    tbl;
  let block_cands = !block_cands in
  (* 3. Traverse, check branches and call sites *)
  let useful = ref Var.Set.empty in
  let mark_useful_cont (pc', args) =
    match Addr.Map.find pc' block_cands with
    | lst ->
        List.iter
          ~f:(fun (x, pos) ->
            if pos < List.length args && Var.Set.mem (List.nth args pos) is_block
            then useful := Var.Set.add x !useful)
          lst
    | exception Not_found -> ()
  in
  let visited2 = BitSet.create' p.free_pc in
  let rec check pc =
    if not (BitSet.mem visited2 pc)
    then (
      BitSet.set visited2 pc;
      let block = Addr.Map.find pc p.blocks in
      List.iter
        ~f:(fun i ->
          match i with
          | Let (_, Apply { f; args; exact = true; _ }) -> (
              match Var.Hashtbl.find closure_cands f with
              | lst ->
                  List.iter
                    ~f:(fun (x, pos) ->
                      if
                        pos < List.length args && Var.Set.mem (List.nth args pos) is_block
                      then useful := Var.Set.add x !useful)
                    lst
              | exception Not_found -> ())
          | Let (_, Closure (_, (pc', _), _)) -> check pc'
          | _ -> ())
        block.body;
      (match block.branch with
      | Branch cont -> mark_useful_cont cont
      | Cond (_, c1, c2) ->
          mark_useful_cont c1;
          mark_useful_cont c2
      | Switch (_, conts) -> Array.iter ~f:mark_useful_cont conts
      | Pushtrap (c1, _, c2) ->
          mark_useful_cont c1;
          mark_useful_cont c2
      | Poptrap cont -> mark_useful_cont cont
      | Return _ | Raise _ | Stop -> ());
      Code.fold_children p.blocks pc (fun pc' () -> check pc') ())
  in
  check p.start;
  let useful = !useful in
  (* 4. Filter *)
  Var.Hashtbl.filter_map_inplace
    (fun x tuple -> if Var.Set.mem x useful then Some tuple else None)
    tbl

let unboxed_fields tuple =
  IntSet.fold (fun i m -> IntMap.add i (Var.fresh ()) m) tuple.needed IntMap.empty

(* Rebuild tuple [x] from the unboxed fields [vars]. Fields which are
   not accessed are filled with a dummy value. *)
let rebuild_tuple x tuple vars body =
  let c = Var.fresh () in
  let tag, dummy =
    match tuple.kind with
    | Float -> 254, (Float (Int64.bits_of_float 0.) : constant)
    | Non_float -> 0, Int (Targetint.of_int_exn 0)
  in
  let fields = Array.init ~f:(fun i -> IntMap.find_opt i vars) tuple.size in
  let block =
    Let
      ( x
      , Block
          ( tag
          , Array.map ~f:(fun v -> Option.value ~default:c v) fields
          , NotArray
          , Immutable ) )
    :: body
  in
  if Array.for_all ~f:Option.is_some fields
  then block
  else Let (c, Constant dummy) :: block

let rewrite_blocks p tbl =
  let ops = Int.Hashtbl.create 16 in
  let blocks =
    Var.Hashtbl.fold
      (fun x tuple blocks ->
        match tuple.loc with
        | Closure _ -> blocks
        | Block pc ->
            Addr.Map.update
              pc
              (fun block ->
                match block with
                | None -> assert false
                | Some block ->
                    let vars = unboxed_fields tuple in
                    let i = List.find_index ~f:(fun y -> Var.equal x y) block.params in
                    Int.Hashtbl.add ops pc (Option.get i, tuple.needed, tuple.kind);
                    Some
                      { block with
                        params =
                          List.map ~f:snd (IntMap.bindings vars)
                          @ List.filter ~f:(fun y -> not (Var.equal x y)) block.params
                      ; body = rebuild_tuple x tuple vars block.body
                      })
              blocks)
      tbl
      p.blocks
  in
  { p with blocks }, ops

(* Replace the tuples at the positions given by [lst] by their needed
   fields. The operations in [lst] are applied from last to first,
   each position being relative to the result of the previous
   operations. *)
let rewrite_args lst args =
  List.fold_right
    ~f:(fun (i, needed, kind) (code, args) ->
      let x = List.nth args i in
      let args = List.filteri ~f:(fun i' _ -> i <> i') args in
      let vars = IntSet.fold (fun i vars -> (i, Var.fresh ()) :: vars) needed [] in
      let code = List.map ~f:(fun (i, y) -> Let (y, Field (x, i, kind))) vars @ code in
      code, List.rev_map ~f:snd vars @ args)
    lst
    ~init:([], args)

let inserted_block ops (pc, args) =
  let body, args = rewrite_args (Int.Hashtbl.find_all ops pc) args in
  { params = []; body; branch = Branch (pc, args) }

(* Update the parameter representations of the closure hint of an
   unboxed function. *)
let rewrite_closure_hint lst (hint : Optimization_hint.closure_hint option) =
  match hint with
  | None -> None
  | Some hint ->
      let rec rewrite lst params =
        match lst with
        | [] -> Some params
        | (i, needed, _) :: rem -> (
            match rewrite rem params with
            | Some params when i < List.length params ->
                Some
                  (List.init ~len:(IntSet.cardinal needed) ~f:(fun _ ->
                       Optimization_hint.Value)
                  @ List.filteri ~f:(fun i' _ -> i <> i') params)
            | _ -> None)
      in
      Option.map
        ~f:(fun params -> { hint with Optimization_hint.params })
        (rewrite lst hint.params)

let rewrite_continuations stats p tbl ops closure_ops locations =
  let conts = Poly.Hashtbl.create 16 in
  let free_pc = ref p.free_pc in
  let new_blocks = ref [] in
  let add_block block =
    let pc = !free_pc in
    incr free_pc;
    new_blocks := (pc, block) :: !new_blocks;
    pc
  in
  let rewrite ((pc, _) as cont) =
    if Int.Hashtbl.mem ops pc
    then (
      try Hashtbl.find conts cont
      with Not_found ->
        stats.continuations <- stats.continuations + 1;
        let pc' = add_block (inserted_block ops cont) in
        let cont' = pc', [] in
        Hashtbl.add conts cont cont';
        cont')
    else cont
  in
  let rewritten = BitSet.create' p.free_pc in
  let rewrite_body body =
    List.fold_right
      ~f:(fun i rem ->
        match i with
        | Let (x, Apply { f; args; exact = true }) when Var.Hashtbl.mem closure_ops f ->
            stats.call_sites <- stats.call_sites + 1;
            let f', _, _, lst = Var.Hashtbl.find closure_ops f in
            let code, args = rewrite_args lst args in
            code @ (Let (x, Apply { f = f'; args; exact = true }) :: rem)
        | Let (f, Closure (params, cont, loc)) -> (
            match Var.Hashtbl.find closure_ops f with
            | f', params', body, lst ->
                let pc' =
                  add_block { params = []; body; branch = Branch (rewrite cont) }
                in
                let hint, pi = loc in
                Let (f', Closure (params', (pc', []), (rewrite_closure_hint lst hint, pi)))
                ::
                (let params'' = List.map ~f:(fun _ -> Var.fresh ()) params in
                 let pc'' =
                   let code, args'' = rewrite_args lst params'' in
                   let call = Apply { f = f'; args = args''; exact = true } in
                   let res = Var.fresh () in
                   add_block
                     { params = []
                     ; body = code @ [ Let (res, call) ]
                     ; branch = Return res
                     }
                 in
                 Let (f, Closure (params'', (pc'', []), loc)) :: rem)
            | exception Not_found -> Let (f, Closure (params, rewrite cont, loc)) :: rem)
        | _ -> i :: rem)
      body
      ~init:[]
  in
  let rewrite_block pc blocks =
    if BitSet.mem rewritten pc
    then blocks
    else (
      BitSet.set rewritten pc;
      Addr.Map.update
        pc
        (fun block ->
          match block with
          | None -> assert false
          | Some block ->
              let body =
                if
                  List.exists
                    ~f:(fun i ->
                      match i with
                      | Let (_, Closure _) -> true
                      | Let (_, Apply { f; exact = true; _ }) ->
                          Var.Hashtbl.mem closure_ops f
                      | _ -> false)
                    block.body
                then rewrite_body block.body
                else block.body
              in
              let branch =
                match block.branch with
                | Return _ | Raise _ | Stop -> block.branch
                | Branch cont -> Branch (rewrite cont)
                | Cond (x, cont, cont') -> Cond (x, rewrite cont, rewrite cont')
                | Switch (x, l) -> Switch (x, Array.map ~f:rewrite l)
                | Pushtrap (cont, x, cont') -> Pushtrap (rewrite cont, x, rewrite cont')
                | Poptrap cont -> Poptrap (rewrite cont)
              in
              Some { block with body; branch })
        blocks)
  in
  let visited = BitSet.create' p.free_pc in
  let rec traverse pc blocks =
    if BitSet.mem visited pc
    then blocks
    else (
      BitSet.set visited pc;
      let blocks = rewrite_block pc blocks in
      Code.fold_children p.blocks pc (fun pc' blocks -> traverse pc' blocks) blocks)
  in
  let blocks =
    Var.Hashtbl.fold
      (fun _ tuple blocks ->
        traverse
          tuple.start_pc
          (match tuple.closure_pc with
          | None -> blocks
          | Some pc -> rewrite_block pc blocks))
      tbl
      p.blocks
  in
  let blocks = Addr.Set.fold rewrite_block locations blocks in
  { p with
    free_pc = !free_pc
  ; blocks =
      List.fold_left
        ~f:(fun blocks (pc, block) -> Addr.Map.add pc block blocks)
        ~init:blocks
        !new_blocks
  }

let closure_operations tbl =
  let closure_ops = Var.Hashtbl.create 16 in
  Var.Hashtbl.iter
    (fun x tuple ->
      match tuple.loc with
      | Block _ -> ()
      | Closure (f, params) ->
          let f', params, body, lst =
            try Var.Hashtbl.find closure_ops f
            with Not_found -> Var.fork f, params, [], []
          in
          let vars = unboxed_fields tuple in
          let i = List.find_index ~f:(fun y -> Var.equal x y) params in
          let params =
            List.map ~f:snd (IntMap.bindings vars)
            @ List.filter ~f:(fun y -> not (Var.equal x y)) params
          in
          let body = rebuild_tuple x tuple vars body in
          let lst = (Option.get i, tuple.needed, tuple.kind) :: lst in
          Var.Hashtbl.replace closure_ops f (f', params, body, lst))
    tbl;
  closure_ops

let f p =
  let stats = { continuations = 0; call_sites = 0 } in
  let t = Timer.make () in
  (* Find parameters that could be unboxed *)
  let tbl = find_candidates p in
  if debug ()
  then (
    Format.eprintf "Unboxing candidates:@.";
    Var.Hashtbl.iter
      (fun x { size; _ } -> Format.eprintf "  %a: %d@." Var.print x size)
      tbl);
  let immutable = immutable_block_parameters p tbl in
  check_tuple_accesses p tbl immutable;
  check_no_mutation p tbl immutable;
  (* Do not unbox function parameters when we are using too many of
     their fields *)
  Var.Hashtbl.filter_map_inplace
    (fun _ tuple ->
      match tuple.loc with
      | Closure _ when IntSet.cardinal tuple.needed > 6 -> None
      | _ -> Some tuple)
    tbl;
  (* Only unbox closure parameters when they have at least one known
     call site *)
  let locations = check_call_sites p tbl in
  (* Only unbox when at least one call site / branch passes a Block
     literal, so that we actually eliminate a tuple allocation *)
  check_eliminates_tuple p tbl;
  if debug ()
  then (
    Format.eprintf "Unboxed tuples:@.";
    Var.Hashtbl.iter
      (fun x { size; loc; needed; _ } ->
        Format.eprintf
          "  %a: %d (%d) @@ %a@."
          Var.print
          x
          size
          (IntSet.cardinal needed)
          (fun f loc ->
            match loc with
            | Block pc -> Format.fprintf f "%d" pc
            | Closure (y, _) -> Format.fprintf f "%a" Var.print y)
          loc)
      tbl);
  let p, ops = rewrite_blocks p tbl in
  let closure_ops = closure_operations tbl in
  let p = rewrite_continuations stats p tbl ops closure_ops locations in
  if times () then Format.eprintf "  tuple unboxing: %a@." Timer.print t;
  if show_stats ()
  then
    Format.eprintf
      "Stats - tuple unboxing: %d blocks, %d continuations, %d functions, %d call sites@."
      (Int.Hashtbl.length ops)
      stats.continuations
      (Var.Hashtbl.length closure_ops)
      stats.call_sites;
  p
