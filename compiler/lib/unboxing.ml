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

(*
Unboxing wrappers should be inlined

Less aggressive variant: only apply if one branch / call site has an
explicit tuple argument

Also look at function calls to see whether we know more about a parameter.
*)

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
  let unboxed_floats =
    match Config.target () with
    | `JavaScript -> false
    | `Wasm -> false
  in
  let tbl = Var.Hashtbl.create 16 in
  let visited = BitSet.create' p.free_pc in
  let allowed_access kind =
    match kind with
    | Non_float -> true
    | Float -> unboxed_floats
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
          | Let (_, Field (x, n, kind)) when Var.Map.mem x params && allowed_access kind
            ->
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

let check_tuple_accesses p tbl =
  let discard x = Var.Hashtbl.remove tbl x in
  let visited = BitSet.create' p.free_pc in
  let rec traverse pc =
    if not (BitSet.mem visited pc)
    then (
      BitSet.set visited pc;
      let block = Addr.Map.find pc p.blocks in
      List.iter
        ~f:(fun i ->
          match i with
          | Let (_, Field (x, n, _)) ->
              if Var.Hashtbl.mem tbl x
              then
                let tuple = Var.Hashtbl.find tbl x in
                if n < tuple.size
                then tuple.needed <- IntSet.add n tuple.needed
                else discard x
          | Let (_, Closure (_, (pc', _), _)) -> traverse pc'
          | Assign (x, y) ->
              discard x;
              discard y
          | _ -> Freevars.iter_instr_free_vars discard i)
        block.body;
      Freevars.iter_last_free_var discard block.branch;
      Code.fold_children p.blocks pc (fun pc' () -> traverse pc') ())
  in
  Var.Hashtbl.iter
    (fun _ tuple ->
      traverse
        (match tuple.loc with
        | Block pc -> pc
        | Closure _ -> tuple.start_pc))
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
                    let vars = Array.init tuple.size ~f:(fun _ -> Var.fresh ()) in
                    let i = List.find_index ~f:(fun y -> Var.equal x y) block.params in
                    Int.Hashtbl.add ops pc (Option.get i, tuple.size, tuple.kind);
                    Some
                      { block with
                        params =
                          Array.to_list vars
                          @ List.filter ~f:(fun y -> not (Var.equal x y)) block.params
                      ; body =
                          Let
                            ( x
                            , Block
                                ( (match tuple.kind with
                                  | Float -> 254
                                  | Non_float -> 0)
                                , vars
                                , NotArray
                                , Immutable ) )
                          :: block.body
                      })
              blocks)
      tbl
      p.blocks
  in
  { p with blocks }, ops

let inserted_block ops (pc, args) =
  let l = Int.Hashtbl.find_all ops pc in
  let body, args' =
    List.fold_right
      ~f:(fun (i, n, kind) (body, args) ->
        let x = List.nth args i in
        let args = List.filteri ~f:(fun i' _ -> i <> i') args in
        let vars = List.init ~len:n ~f:(fun _ -> Var.fresh ()) in
        let body = List.mapi ~f:(fun i y -> Let (y, Field (x, i, kind))) vars @ body in
        body, vars @ args)
      l
      ~init:([], args)
  in
  { params = List.map ~f:(fun _ -> Var.fresh ()) args; body; branch = Branch (pc, args') }

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
  let rewrite ((pc, args) as cont) =
    if Int.Hashtbl.mem ops pc
    then (
      try Hashtbl.find conts cont
      with Not_found ->
        stats.continuations <- stats.continuations + 1;
        let pc' = add_block (inserted_block ops cont) in
        let cont' = pc', args in
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
                Let (f', Closure (params', (pc', []), loc))
                ::
                (*ZZZ events *)
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
          let vars =
            IntSet.fold
              (fun i m -> IntMap.add i (Var.fresh ()) m)
              tuple.needed
              IntMap.empty
          in
          let i = List.find_index ~f:(fun y -> Var.equal x y) params in
          let params =
            List.map ~f:snd (IntMap.bindings vars)
            @ List.filter ~f:(fun y -> not (Var.equal x y)) params
          in
          let c = Var.fresh () in
          let body =
            Let
              ( c
              , Constant
                  (match tuple.kind with
                  | Float -> Float (Int64.bits_of_float 0.)
                  | Non_float -> Int (Targetint.of_int_exn 0)) )
            :: Let
                 ( x
                 , Block
                     ( (match tuple.kind with
                       | Float -> 254
                       | Non_float -> 0)
                     , Array.init
                         ~f:(fun i -> try IntMap.find i vars with Not_found -> c)
                         tuple.size
                     , NotArray
                     , Immutable ) )
            :: body
          in
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
  (* Check that the tuples don't escape and that we don't access more
     fields than expected *)
  check_tuple_accesses p tbl;
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
