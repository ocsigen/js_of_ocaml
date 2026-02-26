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
  | Unbox_i32 -> Number (Int32, Unboxed)
  | Unbox_i64 -> Number (Int64, Unboxed)
  | Unbox_f64 -> Number (Float, Unboxed)
  | Box_i32 | Box_i64 | Box_f64 -> Top
  | Untag_int -> Int Normalized
  | Tag_int -> Top

(* First pass: only lower number boxing/unboxing conversions. *)
let number_conversion_kind ~(from : Typing.typ) ~(into : Typing.typ) =
  match from, into with
  | Number (Int32, Unboxed), Number (Int32, Unboxed)
  | Number (Int64, Unboxed), Number (Int64, Unboxed)
  | Number (Float, Unboxed), Number (Float, Unboxed) -> None
  | Int _, _ | _, Int _ -> None
  | Number (_, Unboxed), Number (_, Unboxed) -> None
  | _, Number (Int32, Unboxed) -> Some Unbox_i32
  | _, Number (Int64, Unboxed) -> Some Unbox_i64
  | _, Number (Float, Unboxed) -> Some Unbox_f64
  | Number (Int32, Unboxed), _ -> Some Box_i32
  | Number (Int64, Unboxed), _ -> Some Box_i64
  | Number (Float, Unboxed), _ -> Some Box_f64
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
  let lower_apply x ({ f; args; exact } as apply) =
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
        | None -> lowered_args @ [ Let (x, Apply { apply with args = args' }) ]
        | Some kind ->
            let tmp = Var.fresh () in
            Typing.set_var_type types tmp from;
            lowered_args
            @ [ Let (tmp, Apply { apply with args = args' })
              ; Let (x, Prim (prim_of_kind kind, [ Pv tmp ]))
              ])
    | None -> lowered_args @ [ Let (x, Apply { apply with args = args' }) ]
  in
  let lower_instr = function
    | Assign (x, y) ->
        let from = Typing.var_type types y in
        let into = Typing.var_type types x in
        let lowered, y' = lower_var_conversion ~types ~from ~into y in
        lowered @ [ Assign (x, y') ]
    | Let (x, Apply apply) -> lower_apply x apply
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

let compute_local_props all_convs block =
  let killed_vars = ref VarSet.empty in
  List.iter
    ~f:(function
      | Let (v, _) | Assign (v, _) -> killed_vars := VarSet.add v !killed_vars
      | Set_field _ | Offset_ref _ | Array_set _ | Event _ -> ())
    block.body;
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

let remove_killed_mappings map v =
  ConvMap.filter
    (fun (_, arg) mapped -> not (Var.equal arg v) && not (Var.equal mapped v))
    map

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
            | Number (_, Unboxed) | Int Normalized -> Typing.set_return_type types g t
            | Top | Int Ref | Int Unnormalized | Number (_, Boxed) | Tuple _ | Bigarray _ | Bot
              -> ())
      | None -> ())
    ();
  let p = lower_conversions p types global_flow_info in
  let all_convs, conv_types = get_all_conversions p types in
  if ConvSet.is_empty all_convs
  then p, types
  else (
    let props = Addr.Map.map (compute_local_props all_convs) p.blocks in
    let preds = CFG.predecessors p.blocks in
    (* 1. Anticipatability *)
    let antin = ref (Addr.Map.map (fun _ -> all_convs) p.blocks) in
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
            ~f:(fun acc succ -> ConvSet.inter acc (Addr.Map.find succ !antin))
            ~init:all_convs
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
    let blocks =
      Addr.Map.mapi
        (fun pc block ->
          let to_insert = ConvSet.diff (Addr.Map.find pc !antin) (Addr.Map.find pc avin) in
          let inserted_rev = ref [] in
          let conv_to_var = ref ConvMap.empty in
          ConvSet.iter
            (fun ((kind, arg) as conv) ->
              let tmp = Var.fresh () in
              let typ =
                ConvMap.find_opt conv conv_types |> Option.value ~default:(type_of_kind kind)
              in
              Typing.set_var_type types tmp typ;
              conv_to_var := ConvMap.add conv tmp !conv_to_var;
              inserted_rev := Let (tmp, Prim (prim_of_kind kind, [ Pv arg ])) :: !inserted_rev)
            to_insert;
          let subst = ref Var.Map.empty in
          let subst_var x = apply_subst !subst x in
          let body_rev = ref [] in
          List.iter
            ~f:(fun i ->
              let i = Subst.Excluding_Binders.instr subst_var i in
              match i with
              | Let (v, Prim (p, [ Pv arg ])) -> (
                  conv_to_var := remove_killed_mappings !conv_to_var v;
                  subst := Var.Map.remove v !subst;
                  match kind_of_prim p with
                  | Some kind -> (
                      let conv = kind, arg in
                      match ConvMap.find_opt conv !conv_to_var with
                      | Some tmp -> subst := Var.Map.add v tmp !subst
                      | None ->
                          body_rev := i :: !body_rev;
                          conv_to_var := ConvMap.add conv v !conv_to_var)
                  | None -> body_rev := i :: !body_rev)
              | Let (v, _) ->
                  conv_to_var := remove_killed_mappings !conv_to_var v;
                  subst := Var.Map.remove v !subst;
                  body_rev := i :: !body_rev
              | Assign (v, _) ->
                  conv_to_var := remove_killed_mappings !conv_to_var v;
                  subst := Var.Map.remove v !subst;
                  body_rev := i :: !body_rev
              | Set_field _ | Offset_ref _ | Array_set _ | Event _ ->
                  body_rev := i :: !body_rev)
            block.body;
          let branch = Subst.Excluding_Binders.last subst_var block.branch in
          { block with body = List.rev !inserted_rev @ List.rev !body_rev; branch })
        p.blocks
    in
    { p with blocks }, types)
