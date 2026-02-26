open! Stdlib
open Code

module VarSet = Var.Set

type conversion_kind =
  | Unbox_i32
  | Unbox_i64
  | Unbox_f64
  | Box_i32
  | Box_i64
  | Box_f64
  | Untag_int
  | Tag_int

module Conv = struct
  type t = conversion_kind * Var.t

  let compare (k1, v1) (k2, v2) =
    let c = Poly.compare k1 k2 in
    if c <> 0 then c else Var.compare v1 v2
end

module ConvSet = Set.Make (Conv)
module ConvMap = Map.Make (Conv)

let prim_of_kind = function
  | Unbox_i32 -> Wasm_unbox_i32
  | Unbox_i64 -> Wasm_unbox_i64
  | Unbox_f64 -> Wasm_unbox_f64
  | Box_i32 -> Wasm_box_i32
  | Box_i64 -> Wasm_box_i64
  | Box_f64 -> Wasm_box_f64
  | Untag_int -> Wasm_untag_int
  | Tag_int -> Wasm_tag_int

let kind_of_prim = function
  | Wasm_unbox_i32 -> Some Unbox_i32
  | Wasm_unbox_i64 -> Some Unbox_i64
  | Wasm_unbox_f64 -> Some Unbox_f64
  | Wasm_box_i32 -> Some Box_i32
  | Wasm_box_i64 -> Some Box_i64
  | Wasm_box_f64 -> Some Box_f64
  | Wasm_untag_int -> Some Untag_int
  | Wasm_tag_int -> Some Tag_int
  | _ -> None

let type_of_kind = function
  | Unbox_i32 -> Typing.Number (Typing.Int32, Typing.Unboxed)
  | Unbox_i64 -> Typing.Number (Typing.Int64, Typing.Unboxed)
  | Unbox_f64 -> Typing.Number (Typing.Float, Typing.Unboxed)
  | Box_i32 | Box_i64 | Box_f64 -> Typing.Top
  | Untag_int -> Typing.Int Typing.Integer.Normalized
  | Tag_int -> Typing.Top

(* First pass: only lower number boxing/unboxing conversions. *)
let number_conversion_kind ~(from : Typing.typ) ~(into : Typing.typ) =
  match from, into with
  | Typing.Number (Typing.Int32, Typing.Unboxed), Typing.Number (Typing.Int32, Typing.Unboxed)
  | Typing.Number (Typing.Int64, Typing.Unboxed), Typing.Number (Typing.Int64, Typing.Unboxed)
  | Typing.Number (Typing.Float, Typing.Unboxed), Typing.Number (Typing.Float, Typing.Unboxed)
    -> None
  | Typing.Int _, _ | _, Typing.Int _ -> None
  | Typing.Number (_, Typing.Unboxed), Typing.Number (_, Typing.Unboxed) -> None
  | _, Typing.Number (Typing.Int32, Typing.Unboxed) -> Some Unbox_i32
  | _, Typing.Number (Typing.Int64, Typing.Unboxed) -> Some Unbox_i64
  | _, Typing.Number (Typing.Float, Typing.Unboxed) -> Some Unbox_f64
  | Typing.Number (Typing.Int32, Typing.Unboxed), _ -> Some Box_i32
  | Typing.Number (Typing.Int64, Typing.Unboxed), _ -> Some Box_i64
  | Typing.Number (Typing.Float, Typing.Unboxed), _ -> Some Box_f64
  | _ -> None

let lower_var_conversion ~types ~(from : Typing.typ) ~(into : Typing.typ) x =
  match number_conversion_kind ~from ~into with
  | None -> [], x
  | Some kind ->
      let tmp = Var.fresh () in
      Typing.set_var_type types tmp into;
      [ Let (tmp, Prim (prim_of_kind kind, [ Pv x ])) ], tmp

let lower_conversions (p : program) (types : Typing.t) (global_flow_info : Global_flow.info)
    =
  let lower_apply x ~f ~args ~exact =
    let closure =
      if exact
      then
        match Global_flow.get_unique_closure global_flow_info f with
        | Some (g, params) when List.compare_length_with args ~len:(List.length params) = 0 ->
            Some (g, params)
        | Some _ | None -> None
      else None
    in
    let target_types =
      match closure with
      | Some (_, params) -> List.map params ~f:(Typing.var_type types)
      | None -> List.map args ~f:(fun _ -> Typing.Top)
    in
    let lowered_args_rev = ref [] in
    let args' =
      List.map2
        ~f:(fun arg into ->
          let from = Typing.var_type types arg in
          let lowered, arg' = lower_var_conversion ~types ~from ~into arg in
          lowered_args_rev := List.rev_append lowered !lowered_args_rev;
          arg')
        args
        target_types
    in
    let lowered_args = List.rev !lowered_args_rev in
    match closure with
    | Some (g, _) -> (
        let from = Typing.return_type types g in
        let into = Typing.var_type types x in
        match number_conversion_kind ~from ~into with
        | None -> lowered_args @ [ Let (x, Apply { f; args = args'; exact }) ]
        | Some kind ->
            let tmp = Var.fresh () in
            Typing.set_var_type types tmp from;
            lowered_args
            @ [ Let (tmp, Apply { f; args = args'; exact })
              ; Let (x, Prim (prim_of_kind kind, [ Pv tmp ]))
              ])
    | None -> lowered_args @ [ Let (x, Apply { f; args = args'; exact }) ]
  in
  let lower_prim x p args =
    let target_types_opt =
      match p with
      | Extern nm -> fst (Typing.prim_sig nm)
      | _ -> None
    in
    let args', lowered_args =
      match target_types_opt with
      | Some target_types when List.compare_length_with args ~len:(List.length target_types) = 0 ->
          let lowered_args_rev = ref [] in
          let args' =
            List.map2
              ~f:(fun arg into ->
                match arg with
                | Pv v ->
                    let from = Typing.var_type types v in
                    let lowered, v' = lower_var_conversion ~types ~from ~into v in
                    lowered_args_rev := List.rev_append lowered !lowered_args_rev;
                    Pv v'
                | Pc _ -> arg)
              args
              target_types
          in
          args', List.rev !lowered_args_rev
      | _ -> args, []
    in
    lowered_args @ [ Let (x, Prim (p, args')) ]
  in
  let lower_instr = function
    | Assign (x, y) ->
        let from = Typing.var_type types y in
        let into = Typing.var_type types x in
        let lowered, y' = lower_var_conversion ~types ~from ~into y in
        lowered @ [ Assign (x, y') ]
    | Let (x, Apply { f; args; exact }) -> lower_apply x ~f ~args ~exact
    | Let (x, Prim (p, args)) -> lower_prim x p args
    | i -> [ i ]
  in
  let blocks =
    Addr.Map.map
      (fun block -> { block with body = List.concat_map ~f:lower_instr block.body })
      p.blocks
  in
  { p with blocks }

let get_all_conversions program types =
  let all_convs = ref ConvSet.empty in
  let conv_types = ref ConvMap.empty in
  Addr.Map.iter
    (fun _ block ->
      List.iter
        ~f:(function
          | Let (x, Prim (p, [ Pv v ])) -> (
              match kind_of_prim p with
              | Some kind ->
                  let conv = kind, v in
                  all_convs := ConvSet.add conv !all_convs;
                  let typ = Typing.var_type types x in
                  let typ =
                    match ConvMap.find_opt conv !conv_types with
                    | Some current -> Typing.join current typ
                    | None -> typ
                  in
                  conv_types := ConvMap.add conv typ !conv_types
              | None -> ())
          | _ -> ())
        block.body)
    program.blocks;
  !all_convs, !conv_types

type block_props =
  { transp : ConvSet.t
  ; comp : ConvSet.t
  ; antloc : ConvSet.t
  }

let remove_conversions_of_var convs v =
  ConvSet.filter (fun (_, arg) -> not (Var.equal arg v)) convs

let compute_local_props all_convs def_blocks pc block =
  let killed_vars = ref VarSet.empty in
  Var.Tbl.iter (fun v def_pc -> if def_pc = pc then killed_vars := VarSet.add v !killed_vars) def_blocks;
  let transp = ConvSet.filter (fun (_, v) -> not (VarSet.mem v !killed_vars)) all_convs in
  let comp = ref ConvSet.empty in
  let antloc = ref ConvSet.empty in
  let current_killed = ref VarSet.empty in
  let kill_var v =
    current_killed := VarSet.add v !current_killed;
    comp := remove_conversions_of_var !comp v
  in
  List.iter
    ~f:(function
      | Let (v, Prim (p, [ Pv arg ])) -> (
          match kind_of_prim p with
          | Some kind ->
              let conv = kind, arg in
              if not (VarSet.mem arg !current_killed) then antloc := ConvSet.add conv !antloc;
              comp := ConvSet.add conv !comp;
              kill_var v
          | None -> kill_var v)
      | Let (v, _) | Assign (v, _) -> kill_var v
      | Set_field _ | Offset_ref _ | Array_set _ | Event _ -> ())
    block.body;
  { transp; comp = !comp; antloc = !antloc }

module CFG = struct
  let successors blocks pc =
    let b = Addr.Map.find pc blocks in
    match b.branch with
    | Return _ | Raise _ | Stop -> []
    | Branch (pc', _) | Pushtrap ((pc', _), _, _) | Poptrap (pc', _) -> [ pc' ]
    | Cond (_, (pc1, _), (pc2, _)) -> [ pc1; pc2 ]
    | Switch (_, targets) -> Array.to_list (ArrayLabels.map ~f:(fun (pc, _) -> pc) targets)

  let predecessors blocks =
    let preds = ref Addr.Map.empty in
    Addr.Map.iter
      (fun pc _ ->
        let succs = successors blocks pc in
        List.iter
          ~f:(fun succ ->
            let p = Addr.Map.find_opt succ !preds |> Option.value ~default:[] in
            preds := Addr.Map.add succ (pc :: p) !preds)
          succs)
      blocks;
    !preds
end

let rec apply_subst subst x =
  match Var.Map.find_opt x subst with
  | Some y when not (Var.equal x y) -> apply_subst subst y
  | Some _ | None -> x

let f (p : program) (types : Typing.t) ~(global_flow_info : Global_flow.info) ~fun_info =
  (* Global unboxing decision for direct calls. *)
  fold_closures
    p
    (fun name_opt _ _ _ () ->
      match name_opt with
      | Some g ->
          if Typing.can_unbox_parameters fun_info g
          then (
            let s = Var.Map.find g global_flow_info.info_return_vals in
            let t =
              Var.Set.fold (fun x acc -> Typing.join (Typing.var_type types x) acc) s Typing.Bot
            in
            match t with
            | Typing.Number (_, Typing.Unboxed)
            | Typing.Int Typing.Integer.Normalized ->
                Typing.set_return_type types g t
            | Typing.Top
            | Typing.Int Typing.Integer.Ref
            | Typing.Int Typing.Integer.Unnormalized
            | Typing.Number (_, Typing.Boxed)
            | Typing.Tuple _
            | Typing.Bigarray _
            | Typing.Bot -> ())
      | None -> ())
    ();
  let p = lower_conversions p types global_flow_info in
  let all_convs, conv_types = get_all_conversions p types in
  if ConvSet.is_empty all_convs
  then p, types
  else (
    let def_blocks = Var.Tbl.make () (-1) in
    let visited = BitSet.create' p.free_pc in
    let rec traverse_defs pc =
      if not (BitSet.mem visited pc) then (
        BitSet.set visited pc;
        let block = Addr.Map.find pc p.blocks in
        Freevars.iter_block_bound_vars (fun x -> Var.Tbl.set def_blocks x pc) block;
        List.iter
          ~f:(function
            | Let (_, Closure (params, (pc', _), _)) ->
                List.iter params ~f:(fun x -> Var.Tbl.set def_blocks x pc);
                traverse_defs pc'
            | _ -> ())
          block.body;
        Code.fold_children p.blocks pc (fun pc' () -> traverse_defs pc') ()
      )
    in
    traverse_defs p.start;
    let component_of = Addr.Hashtbl.create 16 in
    let rec mark_comp pc comp_id =
      if not (Addr.Hashtbl.mem component_of pc) then (
        Addr.Hashtbl.add component_of pc comp_id;
        Code.fold_children p.blocks pc (fun pc' () -> mark_comp pc' comp_id) ()
      )
    in
    Code.fold_closures p (fun _ _ (pc, _) _ () -> mark_comp pc pc) ();
    let get_comp pc = try Addr.Hashtbl.find component_of pc with Not_found -> -1 in
    let conv_comp = ConvSet.fold (fun ((_, arg) as conv) acc ->
        ConvMap.add conv (get_comp (Var.Tbl.get def_blocks arg)) acc
      ) all_convs ConvMap.empty in
    let comp_all_convs pc =
      let my_comp = get_comp pc in
      ConvSet.filter (fun conv -> let c = ConvMap.find conv conv_comp in c = my_comp || c = -1 || c = get_comp p.start) all_convs
    in
    let props = Addr.Map.mapi (fun pc block -> compute_local_props all_convs def_blocks pc block) p.blocks in
    let preds = CFG.predecessors p.blocks in
    (* 1. Anticipatability *)
    let antin = ref (Addr.Map.mapi (fun pc _ -> comp_all_convs pc) p.blocks) in
    let worklist = ref (Addr.Map.fold (fun pc _ acc -> pc :: acc) p.blocks []) in
    while not (List.is_empty !worklist) do
      let pc = List.hd !worklist in
      worklist := List.tl !worklist;
      let b_props = Addr.Map.find pc props in
      let succs = CFG.successors p.blocks pc in
      let new_antout =
        if List.is_empty succs
        then ConvSet.empty
        else
          List.fold_left
            ~f:(fun acc succ ->
              let succ_antin = Addr.Map.find succ !antin in
              let valid_succ_antin = ConvSet.filter (fun (_, arg) -> Var.Tbl.get def_blocks arg <> succ) succ_antin in
              ConvSet.inter acc valid_succ_antin)
            ~init:(comp_all_convs pc)
            succs
      in
      let new_antin = ConvSet.union b_props.antloc (ConvSet.inter b_props.transp new_antout) in
      if not (ConvSet.equal (Addr.Map.find pc !antin) new_antin)
      then (
        antin := Addr.Map.add pc new_antin !antin;
        let ps = Addr.Map.find_opt pc preds |> Option.value ~default:[] in
        List.iter
          ~f:(fun p' ->
            if not (List.mem ~eq:Int.equal p' !worklist) then worklist := p' :: !worklist)
          ps)
    done;
    (* 2. Availability *)
    let avout = ref (Addr.Map.map (fun _ -> all_convs) p.blocks) in
    avout := Addr.Map.add p.start ConvSet.empty !avout;
    worklist := Addr.Map.fold (fun pc _ acc -> pc :: acc) p.blocks [];
    while not (List.is_empty !worklist) do
      let pc = List.hd !worklist in
      worklist := List.tl !worklist;
      let b_props = Addr.Map.find pc props in
      let ps = Addr.Map.find_opt pc preds |> Option.value ~default:[] in
      let new_avin =
        if pc = p.start || List.is_empty ps
        then ConvSet.empty
        else
          List.fold_left
            ~f:(fun acc p' -> ConvSet.inter acc (Addr.Map.find p' !avout))
            ~init:all_convs
            ps
      in
      let new_avout = ConvSet.union b_props.comp (ConvSet.inter b_props.transp new_avin) in
      if not (ConvSet.equal (Addr.Map.find pc !avout) new_avout)
      then (
        avout := Addr.Map.add pc new_avout !avout;
        let succs = CFG.successors p.blocks pc in
        List.iter
          ~f:(fun s ->
            if not (List.mem ~eq:Int.equal s !worklist) then worklist := s :: !worklist)
          succs)
    done;
    let avin =
      Addr.Map.mapi
        (fun pc _ ->
          let ps = Addr.Map.find_opt pc preds |> Option.value ~default:[] in
          if pc = p.start || List.is_empty ps
          then ConvSet.empty
          else
            List.fold_left
              ~f:(fun acc p' -> ConvSet.inter acc (Addr.Map.find p' !avout))
              ~init:all_convs
              ps)
        p.blocks
    in
    let earliest =
      Addr.Map.mapi
        (fun pc _ ->
          ConvSet.diff (Addr.Map.find pc !antin) (Addr.Map.find pc avin))
        p.blocks
    in
    (* 3. Delayability *)
    let delayin = ref earliest in
    let delayout = ref (Addr.Map.map (fun _ -> all_convs) p.blocks) in
    worklist := Addr.Map.fold (fun pc _ acc -> pc :: acc) p.blocks [];
    while not (List.is_empty !worklist) do
      let pc = List.hd !worklist in
      worklist := List.tl !worklist;
      let b_props = Addr.Map.find pc props in
      let ps = Addr.Map.find_opt pc preds |> Option.value ~default:[] in
      let new_delayin =
        if pc = p.start || List.is_empty ps
        then Addr.Map.find pc earliest
        else
          ConvSet.union
            (Addr.Map.find pc earliest)
            (List.fold_left
               ~f:(fun acc p' -> ConvSet.inter acc (Addr.Map.find p' !delayout))
               ~init:all_convs
               ps)
      in
      delayin := Addr.Map.add pc new_delayin !delayin;
      let new_delayout =
        ConvSet.diff new_delayin b_props.comp
      in
      if not (ConvSet.equal (Addr.Map.find pc !delayout) new_delayout)
      then (
        delayout := Addr.Map.add pc new_delayout !delayout;
        let succs = CFG.successors p.blocks pc in
        List.iter
          ~f:(fun s ->
            if not (List.mem ~eq:Int.equal s !worklist) then worklist := s :: !worklist)
          succs)
    done;
    (* 4. Latest *)
    let latest =
      Addr.Map.mapi
        (fun pc _block ->
          let succs = CFG.successors p.blocks pc in
          let delayin_pc = Addr.Map.find pc !delayin in
          let b_props = Addr.Map.find pc props in
          let delayin_succs_intersect =
            if List.is_empty succs
            then ConvSet.empty
            else
              List.fold_left
                ~f:(fun acc s -> ConvSet.inter acc (Addr.Map.find s !delayin))
                ~init:all_convs
                succs
          in
          ConvSet.inter delayin_pc (ConvSet.union b_props.comp (ConvSet.diff all_convs delayin_succs_intersect)))
        p.blocks
    in
    (* 5. Isolated *)
    let isolatedout = ref (Addr.Map.map (fun _ -> all_convs) p.blocks) in
    let isolatedin = ref (Addr.Map.map (fun _ -> ConvSet.empty) p.blocks) in
    worklist := Addr.Map.fold (fun pc _ acc -> pc :: acc) p.blocks [];
    while not (List.is_empty !worklist) do
      let pc = List.hd !worklist in
      worklist := List.tl !worklist;
      let b_props = Addr.Map.find pc props in
      let succs = CFG.successors p.blocks pc in
      let new_isolatedout =
        if List.is_empty succs
        then ConvSet.empty
        else
          List.fold_left
            ~f:(fun acc s ->
              let s_isolatedin = Addr.Map.find s !isolatedin in
              let valid_s_isolatedin = ConvSet.filter (fun (_, arg) -> Var.Tbl.get def_blocks arg <> s) s_isolatedin in
              ConvSet.inter acc valid_s_isolatedin)
            ~init:all_convs
            succs
      in
      isolatedout := Addr.Map.add pc new_isolatedout !isolatedout;
      let new_isolatedin =
        ConvSet.union (Addr.Map.find pc latest) (ConvSet.diff new_isolatedout b_props.comp)
      in
      if not (ConvSet.equal (Addr.Map.find pc !isolatedin) new_isolatedin)
      then (
        isolatedin := Addr.Map.add pc new_isolatedin !isolatedin;
        let ps = Addr.Map.find_opt pc preds |> Option.value ~default:[] in
        List.iter
          ~f:(fun p' ->
            if not (List.mem ~eq:Int.equal p' !worklist) then worklist := p' :: !worklist)
          ps)
    done;
    (* Track which conversions are inserted per component. *)
    let inserted_by_comp = Int.Hashtbl.create 16 in
    Addr.Map.iter
      (fun pc _ ->
        let to_insert =
          ConvSet.diff (Addr.Map.find pc latest) (Addr.Map.find pc !isolatedout)
        in
        if not (ConvSet.is_empty to_insert)
        then (
          let comp = get_comp pc in
          let existing =
            (try Int.Hashtbl.find inserted_by_comp comp with Not_found -> ConvSet.empty)
          in
          Int.Hashtbl.replace inserted_by_comp comp (ConvSet.union existing to_insert)))
      p.blocks;
    let comp_inserted pc =
      try Int.Hashtbl.find inserted_by_comp (get_comp pc) with Not_found -> ConvSet.empty
    in
    (* Use per-component tmp variables to avoid cross-function sharing. *)
    let comp_tmp_map = ref Addr.Map.empty in
    let get_comp_tmp comp ((kind, _) as conv) =
      let comp_convs =
        Addr.Map.find_opt comp !comp_tmp_map |> Option.value ~default:ConvMap.empty
      in
      match ConvMap.find_opt conv comp_convs with
      | Some tmp -> tmp
      | None ->
          let tmp = Var.fresh () in
          let typ =
            ConvMap.find_opt conv conv_types |> Option.value ~default:(type_of_kind kind)
          in
          Typing.set_var_type types tmp typ;
          comp_tmp_map := Addr.Map.add comp (ConvMap.add conv tmp comp_convs) !comp_tmp_map;
          tmp
    in
    let global_subst = ref Var.Map.empty in
    let blocks =
      Addr.Map.mapi
        (fun pc block ->
          let comp = get_comp pc in
          let to_insert =
            ConvSet.diff (Addr.Map.find pc latest) (Addr.Map.find pc !isolatedout)
          in
          let inserted_rev = ref [] in
          ConvSet.iter
            (fun ((kind, arg) as conv) ->
              let tmp = get_comp_tmp comp conv in
              inserted_rev := Let (tmp, Prim (prim_of_kind kind, [ Pv arg ])) :: !inserted_rev)
            to_insert;
          let my_inserted = comp_inserted pc in
          let body_rev = ref [] in
          List.iter
            ~f:(fun i ->
              match i with
              | Let (v, Prim (p, [ Pv arg ])) -> (
                  match kind_of_prim p with
                  | Some kind -> (
                      let conv = kind, arg in
                      if ConvSet.mem conv my_inserted
                      then (
                        let tmp = get_comp_tmp comp conv in
                        global_subst := Var.Map.add v tmp !global_subst)
                      else body_rev := i :: !body_rev)
                  | None -> body_rev := i :: !body_rev)
              | Let (_, _) | Assign (_, _) | Set_field _ | Offset_ref _ | Array_set _ | Event _ ->
                  body_rev := i :: !body_rev)
            block.body;
          { block with body = List.rev !inserted_rev @ List.rev !body_rev })
        p.blocks
    in
    let global_subst_var x = apply_subst !global_subst x in
    let blocks =
      Addr.Map.mapi
        (fun _pc block ->
          let body = List.map ~f:(Subst.Excluding_Binders.instr global_subst_var) block.body in
          let branch = Subst.Excluding_Binders.last global_subst_var block.branch in
          { block with body; branch })
        blocks
    in
    { p with blocks }, types)
