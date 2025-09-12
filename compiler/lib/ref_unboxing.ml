open! Stdlib
open Code

(*
ocamlc does not perform reference unboxing when emitting debugging
information. Inlining can also enable additional reference unboxing.

TODO:
- appropriate order
- handle assignment in handler
  If a ref is used in an exception handler:
  - add block that binds the contents of the reference right before pushtrap
  - insert assignements for each update
*)

let debug = Debug.find "unbox-refs"

let times = Debug.find "times"

let stats = Debug.find "stats"

let rewrite refs block m =
  let m, l =
    List.fold_left
      ~f:(fun (m, rem) i ->
        match i with
        | Let (x, Block (0, [| y |], (NotArray | Unknown), Maybe_mutable))
          when Var.Set.mem x refs -> Var.Map.add x y m, rem
        | Let (y, Field (x, 0, Non_float)) when Var.Map.mem x m ->
            (* Optimized away by Phisimpl *)
            m, Let (y, Prim (Extern "%identity", [ Pv (Var.Map.find x m) ])) :: rem
        | Offset_ref (x, n) when Var.Map.mem x m ->
            let y = Var.fresh () in
            ( Var.Map.add x y m
            , Let
                ( y
                , Prim
                    ( Extern "%int_add"
                    , [ Pv (Var.Map.find x m); Pc (Int (Targetint.of_int_exn n)) ] ) )
              :: rem )
        | Set_field (x, _, Non_float, y) when Var.Map.mem x m -> Var.Map.add x y m, rem
        | Event _
          when match rem with
               | Event _ :: _ -> true
               | _ -> false -> m, rem
        | _ -> m, i :: rem)
      block.body
      ~init:(m, [])
  in
  m, List.rev l

let rewrite_cont relevant_vars vars (pc', args) =
  let refs, _ = Int.Hashtbl.find relevant_vars pc' in
  let vars = Var.Map.filter (fun x _ -> Var.Set.mem x refs) vars in
  pc', List.map ~f:snd (Var.Map.bindings vars) @ args

let rewrite_function p variables pc =
  let relevant_vars = Int.Hashtbl.create 16 in
  let g = Structure.(dominator_tree (build_graph p.blocks pc)) in
  let rec traverse_tree g pc vars =
    let block = Addr.Map.find pc p.blocks in
    let vars' =
      List.fold_left
        ~f:(fun s i ->
          match i with
          | Let (x, Block (0, [| _ |], (NotArray | Unknown), Maybe_mutable))
            when Var.Hashtbl.mem variables x -> Var.Set.add x s
          | _ -> s)
        ~init:vars
        block.body
    in
    Int.Hashtbl.add relevant_vars pc (vars, vars');
    Addr.Set.iter (fun pc' -> traverse_tree g pc' vars') (Structure.get_edges g pc)
  in
  traverse_tree g pc Var.Set.empty;
  let rec traverse_tree' g pc blocks =
    let block = Addr.Map.find pc p.blocks in
    let vars, refs = Int.Hashtbl.find relevant_vars pc in
    let vars =
      Var.Set.fold (fun x m -> Var.Map.add x (Var.fork x) m) vars Var.Map.empty
    in
    let params = List.map ~f:snd (Var.Map.bindings vars) @ block.params in
    let vars, body = rewrite refs block vars in
    let branch =
      match block.branch with
      | Return _ | Raise _ | Stop -> block.branch
      | Branch cont -> Branch (rewrite_cont relevant_vars vars cont)
      | Cond (x, cont, cont') ->
          Cond
            ( x
            , rewrite_cont relevant_vars vars cont
            , rewrite_cont relevant_vars vars cont' )
      | Switch (x, a) ->
          Switch (x, Array.map ~f:(fun cont -> rewrite_cont relevant_vars vars cont) a)
      | Pushtrap (cont, x, cont') ->
          Pushtrap
            ( rewrite_cont relevant_vars vars cont
            , x
            , rewrite_cont relevant_vars vars cont' )
      | Poptrap cont -> Poptrap (rewrite_cont relevant_vars vars cont)
    in
    let blocks = Addr.Map.add pc { params; body; branch } blocks in
    Addr.Set.fold
      (fun pc' blocks -> traverse_tree' g pc' blocks)
      (Structure.get_edges g pc)
      blocks
  in
  let blocks = traverse_tree' g pc p.blocks in
  { p with blocks }

let f p =
  let t = Timer.make () in
  let candidates = Var.Hashtbl.create 128 in
  let updated = Var.Hashtbl.create 128 in
  let visited = BitSet.create' p.free_pc in
  let discard x = Var.Hashtbl.remove candidates x in
  let check_field_access depth x =
    match Var.Hashtbl.find candidates x with
    | exception Not_found -> false
    | depth' ->
        if depth' = depth
        then true
        else (
          Var.Hashtbl.remove candidates x;
          false)
  in
  let rec traverse depth start_pc pc =
    if not (BitSet.mem visited pc)
    then (
      BitSet.set visited pc;
      let block = Addr.Map.find pc p.blocks in
      List.iter
        ~f:(fun i ->
          match i with
          | Let (x, Block (0, [| _ |], (NotArray | Unknown), Maybe_mutable)) ->
              Freevars.iter_instr_free_vars discard i;
              Var.Hashtbl.replace candidates x depth
          | Let (_, Closure (_, (pc', _), _)) -> traverse (depth + 1) pc' pc'
          | Let (_, Field (x, 0, Non_float)) -> ignore (check_field_access depth x)
          | Offset_ref (x, _) ->
              if check_field_access depth x then Var.Hashtbl.replace updated x start_pc
          | Set_field (x, _, Non_float, y) ->
              discard y;
              if check_field_access depth x then Var.Hashtbl.replace updated x start_pc
          | _ -> Freevars.iter_instr_free_vars discard i)
        block.body;
      Freevars.iter_last_free_var discard block.branch;
      match block.branch with
      | Pushtrap ((pc', _), _, (pc'', _)) ->
          traverse (depth + 1) start_pc pc';
          traverse depth start_pc pc''
      | Poptrap (pc', _) -> traverse (depth - 1) start_pc pc'
      | _ -> Code.fold_children p.blocks pc (fun pc' () -> traverse depth start_pc pc') ())
  in
  traverse 0 p.start p.start;
  if debug ()
  then
    Print.program
      Format.err_formatter
      (fun _ i ->
        match i with
        | Instr (Let (x, _))
          when Var.Hashtbl.mem candidates x && Var.Hashtbl.mem updated x -> "REF"
        | _ -> "")
      p;
  Var.Hashtbl.filter_map_inplace
    (fun x _depth -> try Some (Var.Hashtbl.find updated x) with Not_found -> None)
    candidates;
  let functions =
    Var.Hashtbl.fold (fun _ pc s -> Addr.Set.add pc s) candidates Addr.Set.empty
  in
  let p = Addr.Set.fold (fun pc p -> rewrite_function p candidates pc) functions p in
  if times () then Format.eprintf "  reference unboxing: %a@." Timer.print t;
  if stats ()
  then Format.eprintf "Stats - reference unboxing: %d@." (Var.Hashtbl.length candidates);
  p
