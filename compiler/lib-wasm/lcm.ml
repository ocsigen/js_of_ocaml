open! Stdlib
open Code
module VarSet = Var.Set

let debug = Debug.find "lcm"

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
  | Tag_int -> Typing.Int Typing.Integer.Ref

(* First pass: only lower number boxing/unboxing conversions. *)
let number_conversion_kind ~(from : Typing.typ) ~(into : Typing.typ) =
  match from, into with
  | ( Typing.Number (Typing.Int32, Typing.Unboxed)
    , Typing.Number (Typing.Int32, Typing.Unboxed) )
  | ( Typing.Number (Typing.Int64, Typing.Unboxed)
    , Typing.Number (Typing.Int64, Typing.Unboxed) )
  | ( Typing.Number (Typing.Float, Typing.Unboxed)
    , Typing.Number (Typing.Float, Typing.Unboxed) )
  | Int (Normalized | Unnormalized), Int (Normalized | Unnormalized) -> None
  | _, Typing.Int (Typing.Integer.Normalized | Typing.Integer.Unnormalized) ->
      Some Untag_int
  | ( Typing.Int (Typing.Integer.Normalized | Typing.Integer.Unnormalized)
    , Typing.Int Typing.Integer.Ref ) -> Some Tag_int
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

let lower_conversions
    (p : program)
    (types : Typing.t)
    (global_flow_info : Global_flow.info) =
  let lower_apply x ~f ~args ~exact =
    let closure =
      if exact
      then
        match Global_flow.get_unique_closure global_flow_info f with
        | Some (g, params)
          when List.compare_length_with args ~len:(List.length params) = 0 ->
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
      let top = Typing.Top in
      let int_n = Typing.Int Typing.Integer.Normalized in
      match p with
      | Extern nm -> fst (Typing.prim_sig nm)
      | Array_get -> Some [ top; int_n ]
      | Lt | Le | Ult -> Some [ int_n; int_n ]
      | _ -> None
    in
    let args', lowered_args =
      match target_types_opt with
      | Some target_types
        when List.compare_length_with args ~len:(List.length target_types) = 0 ->
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
    | Set_field (x, n, Non_float, y) ->
        let lowered, y' =
          lower_var_conversion ~types ~from:(Typing.var_type types y) ~into:Typing.Top y
        in
        lowered @ [ Set_field (x, n, Non_float, y') ]
    | Array_set (x, y, z) ->
        let int_n = Typing.Int Typing.Integer.Normalized in
        let lowered1, y' =
          lower_var_conversion ~types ~from:(Typing.var_type types y) ~into:int_n y
        in
        let lowered2, z' =
          lower_var_conversion ~types ~from:(Typing.var_type types z) ~into:Typing.Top z
        in
        lowered1 @ lowered2 @ [ Array_set (x, y', z') ]
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

let get_all_conversions blocks types =
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
    blocks;
  !all_convs, !conv_types

type block_props =
  { transp : ConvSet.t
  ; comp : ConvSet.t
  ; antloc : ConvSet.t
  }

let remove_conversions_of_var convs v =
  ConvSet.filter (fun (_, arg) -> not (Var.equal arg v)) convs

let remove_killed_mappings map v =
  ConvMap.filter
    (fun (_, arg) mapped -> (not (Var.equal arg v)) && not (Var.equal mapped v))
    map

let compute_local_props all_convs block =
  let killed_vars = ref VarSet.empty in
  (* Block parameters are definitions. They kill conversions involving them. *)
  List.iter ~f:(fun v -> killed_vars := VarSet.add v !killed_vars) block.params;
  List.iter
    ~f:(function
      | Let (v, _) | Assign (v, _) -> killed_vars := VarSet.add v !killed_vars
      | Set_field _ | Offset_ref _ | Array_set _ | Event _ -> ())
    block.body;
  let transp = ConvSet.filter (fun (_, v) -> not (VarSet.mem v !killed_vars)) all_convs in
  let comp = ref ConvSet.empty in
  let antloc = ref ConvSet.empty in
  let current_killed = ref VarSet.empty in
  List.iter ~f:(fun v -> current_killed := VarSet.add v !current_killed) block.params;
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
              if not (VarSet.mem arg !current_killed)
              then antloc := ConvSet.add conv !antloc;
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
    | Branch (pc', _) | Poptrap (pc', _) -> [ pc' ]
    | Pushtrap ((pc', _), _, (pc_h, _)) -> [ pc'; pc_h ]
    | Cond (_, (pc1, _), (pc2, _)) -> [ pc1; pc2 ]
    | Switch (_, targets) ->
        Array.to_list (ArrayLabels.map ~f:(fun (pc, _) -> pc) targets)

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

let apply_subst subst x =
  let rec loop visited subst x =
    match Var.Map.find_opt x subst with
    | Some y ->
        if List.exists ~f:(fun v -> Var.equal v y) visited then failwith "subst cycle";
        loop (x :: visited) subst y
    | None -> x
  in
  loop [] subst x

let reachable_blocks all_blocks entry =
  let visited = ref Addr.Set.empty in
  let rec visit pc =
    if not (Addr.Set.mem pc !visited)
    then (
      visited := Addr.Set.add pc !visited;
      List.iter ~f:visit (CFG.successors all_blocks pc))
  in
  visit entry;
  !visited

(* Run the LCM data flow analysis and rewrite on a single function's blocks.
   The analysis must be per-function to avoid cross-function variable references
   in the inserted conversion instructions. *)
let process_function types conv_types entry fun_blocks =
  let all_convs, local_conv_types = get_all_conversions fun_blocks types in
  (* Merge local conv_types into the global conv_types *)
  ConvMap.iter
    (fun conv typ -> conv_types := ConvMap.add conv typ !conv_types)
    local_conv_types;
  if ConvSet.is_empty all_convs
  then fun_blocks
  else
    let props = Addr.Map.map (compute_local_props all_convs) fun_blocks in
    let preds = CFG.predecessors fun_blocks in
    (* 1. Anticipatability *)
    let antin = ref (Addr.Map.map (fun _ -> all_convs) fun_blocks) in
    let worklist = ref (Addr.Map.fold (fun pc _ acc -> pc :: acc) fun_blocks []) in
    while not (List.is_empty !worklist) do
      let pc = List.hd !worklist in
      worklist := List.tl !worklist;
      let b_props = Addr.Map.find pc props in
      let succs = CFG.successors fun_blocks pc in
      let new_antout =
        if List.is_empty succs
        then ConvSet.empty
        else
          List.fold_left
            ~f:(fun acc succ ->
              let succ_antin = Addr.Map.find succ !antin in
              let succ_block = Addr.Map.find succ fun_blocks in
              let valid_succ_antin =
                ConvSet.filter
                  (fun (_, arg) -> not (List.mem ~eq:Var.equal arg succ_block.params))
                  succ_antin
              in
              ConvSet.inter acc valid_succ_antin)
            ~init:all_convs
            succs
      in
      let new_antin =
        ConvSet.union b_props.antloc (ConvSet.inter b_props.transp new_antout)
      in
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
    let avout = ref (Addr.Map.map (fun _ -> all_convs) fun_blocks) in
    avout := Addr.Map.add entry ConvSet.empty !avout;
    worklist := Addr.Map.fold (fun pc _ acc -> pc :: acc) fun_blocks [];
    while not (List.is_empty !worklist) do
      let pc = List.hd !worklist in
      worklist := List.tl !worklist;
      let b_props = Addr.Map.find pc props in
      let ps = Addr.Map.find_opt pc preds |> Option.value ~default:[] in
      let new_avin =
        if pc = entry || List.is_empty ps
        then ConvSet.empty
        else
          List.fold_left
            ~f:(fun acc p' -> ConvSet.inter acc (Addr.Map.find p' !avout))
            ~init:all_convs
            ps
      in
      let new_avout =
        ConvSet.union b_props.comp (ConvSet.inter b_props.transp new_avin)
      in
      if not (ConvSet.equal (Addr.Map.find pc !avout) new_avout)
      then (
        avout := Addr.Map.add pc new_avout !avout;
        let succs = CFG.successors fun_blocks pc in
        List.iter
          ~f:(fun s ->
            if not (List.mem ~eq:Int.equal s !worklist) then worklist := s :: !worklist)
          succs)
    done;
    let avin =
      Addr.Map.mapi
        (fun pc _ ->
          let ps = Addr.Map.find_opt pc preds |> Option.value ~default:[] in
          if pc = entry || List.is_empty ps
          then ConvSet.empty
          else
            List.fold_left
              ~f:(fun acc p' -> ConvSet.inter acc (Addr.Map.find p' !avout))
              ~init:all_convs
              ps)
        fun_blocks
    in
    let earliest =
      Addr.Map.mapi
        (fun pc _ -> ConvSet.diff (Addr.Map.find pc !antin) (Addr.Map.find pc avin))
        fun_blocks
    in
    (* 3. Delayability *)
    let delayin = ref earliest in
    let delayout = ref (Addr.Map.map (fun _ -> all_convs) fun_blocks) in
    worklist := Addr.Map.fold (fun pc _ acc -> pc :: acc) fun_blocks [];
    while not (List.is_empty !worklist) do
      let pc = List.hd !worklist in
      worklist := List.tl !worklist;
      let b_props = Addr.Map.find pc props in
      let ps = Addr.Map.find_opt pc preds |> Option.value ~default:[] in
      let new_delayin =
        if pc = entry || List.is_empty ps
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
      let new_delayout = ConvSet.diff new_delayin b_props.comp in
      if not (ConvSet.equal (Addr.Map.find pc !delayout) new_delayout)
      then (
        delayout := Addr.Map.add pc new_delayout !delayout;
        let succs = CFG.successors fun_blocks pc in
        List.iter
          ~f:(fun s ->
            if not (List.mem ~eq:Int.equal s !worklist) then worklist := s :: !worklist)
          succs)
    done;
    (* 4. Latest *)
    let latest =
      Addr.Map.mapi
        (fun pc _block ->
          let succs = CFG.successors fun_blocks pc in
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
          ConvSet.inter
            delayin_pc
            (ConvSet.union b_props.comp (ConvSet.diff all_convs delayin_succs_intersect)))
        fun_blocks
    in
    (* 5. Isolated *)
    let isolatedout = ref (Addr.Map.map (fun _ -> all_convs) fun_blocks) in
    let isolatedin = ref (Addr.Map.map (fun _ -> ConvSet.empty) fun_blocks) in
    worklist := Addr.Map.fold (fun pc _ acc -> pc :: acc) fun_blocks [];
    while not (List.is_empty !worklist) do
      let pc = List.hd !worklist in
      worklist := List.tl !worklist;
      let b_props = Addr.Map.find pc props in
      let succs = CFG.successors fun_blocks pc in
      let new_isolatedout =
        if List.is_empty succs
        then ConvSet.empty
        else
          List.fold_left
            ~f:(fun acc s ->
              let s_isolatedin = Addr.Map.find s !isolatedin in
              let succ_block = Addr.Map.find s fun_blocks in
              let valid_s_isolatedin =
                ConvSet.filter
                  (fun (_, arg) -> not (List.mem ~eq:Var.equal arg succ_block.params))
                  s_isolatedin
              in
              ConvSet.inter acc valid_s_isolatedin)
            ~init:all_convs
            succs
      in
      isolatedout := Addr.Map.add pc new_isolatedout !isolatedout;
      let new_isolatedin =
        ConvSet.union
          (Addr.Map.find pc latest)
          (ConvSet.diff new_isolatedout b_props.comp)
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

    let rpo = Structure.blocks_in_reverse_post_order (Structure.build_graph fun_blocks entry) in
    let conv_out_map = ref Addr.Map.empty in
    let result = ref fun_blocks in
    List.iter
      ~f:(fun pc ->
        let block = Addr.Map.find pc fun_blocks in
        let preds_pc = Addr.Map.find_opt pc preds |> Option.value ~default:[] in
        let processed_preds =
          List.fold_left
            ~f:(fun acc p ->
              match Addr.Map.find_opt p !conv_out_map with
              | Some map -> map :: acc
              | None -> acc)
            ~init:[]
            preds_pc
        in
        let conv_in =
          match processed_preds with
          | [] -> ConvMap.empty
          | hd :: tl ->
              List.fold_left
                ~f:(fun acc pred_conv_out ->
                  ConvMap.filter
                    (fun k v ->
                      match ConvMap.find_opt k pred_conv_out with
                      | Some v' when Var.equal v v' -> true
                      | _ -> false)
                    acc)
                ~init:hd
                tl
        in
        let pc_avin = Addr.Map.find pc avin in
        let safe_conv_in = ConvMap.filter (fun k _ -> ConvSet.mem k pc_avin) conv_in in
        let to_insert =
          ConvSet.diff (Addr.Map.find pc latest) (Addr.Map.find pc !isolatedout)
        in
        let inserted_rev = ref [] in
        let conv_to_var = ref safe_conv_in in
        ConvSet.iter
          (fun ((kind, arg) as conv) ->
            let tmp = Var.fresh () in
            let typ =
              ConvMap.find_opt conv !conv_types
              |> Option.value ~default:(type_of_kind kind)
            in
            Typing.set_var_type types tmp typ;
            conv_to_var := ConvMap.add conv tmp !conv_to_var;
            inserted_rev :=
              Let (tmp, Prim (prim_of_kind kind, [ Pv arg ])) :: !inserted_rev)
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
        let rewrite_cont (pc', args) =
          let target_block = Addr.Map.find pc' fun_blocks in
          let target_types = List.map ~f:(Typing.var_type types) target_block.params in
          let args' =
            List.map2
              ~f:(fun arg into ->
                let from = Typing.var_type types arg in
                match number_conversion_kind ~from ~into with
                | Some kind -> (
                    let conv = kind, arg in
                    match ConvMap.find_opt conv !conv_to_var with
                    | Some tmp -> tmp
                    | None -> arg)
                | None -> arg)
              args
              target_types
          in
          pc', args'
        in
        let rewrite_branch branch =
          let int_n = Typing.Int Typing.Integer.Normalized in
          match branch with
          | Return _ | Raise _ | Stop -> branch
          | Branch cont -> Branch (rewrite_cont cont)
          | Cond (v, cont1, cont2) ->
              let v' =
                match number_conversion_kind ~from:(Typing.var_type types v) ~into:int_n with
                | Some kind -> (
                    match ConvMap.find_opt (kind, v) !conv_to_var with
                    | Some tmp -> tmp
                    | None -> v)
                | None -> v
              in
              Cond (v', rewrite_cont cont1, rewrite_cont cont2)
          | Switch (v, conts) ->
              let v' =
                match number_conversion_kind ~from:(Typing.var_type types v) ~into:int_n with
                | Some kind -> (
                    match ConvMap.find_opt (kind, v) !conv_to_var with
                    | Some tmp -> tmp
                    | None -> v)
                | None -> v
              in
              Switch (v', Array.map ~f:rewrite_cont conts)
          | Pushtrap (cont1, v, cont2) ->
              Pushtrap (rewrite_cont cont1, v, rewrite_cont cont2)
          | Poptrap cont ->
              Poptrap (rewrite_cont cont)
        in
        let branch = rewrite_branch branch in
        let new_block =
          { block with body = List.rev !inserted_rev @ List.rev !body_rev; branch }
        in
        conv_out_map := Addr.Map.add pc !conv_to_var !conv_out_map;
        result := Addr.Map.add pc new_block !result)
      rpo;
    !result

let f (p : program) (types : Typing.t) ~(global_flow_info : Global_flow.info) ~fun_info =
  (* Global unboxing decision for direct calls. *)
  fold_closures
    p
    (fun name_opt _ _ _ () ->
      match name_opt with
      | Some g -> (
          if Typing.can_unbox_parameters fun_info g
          then
            let s = Var.Map.find g global_flow_info.info_return_vals in
            let t =
              Var.Set.fold
                (fun x acc -> Typing.join (Typing.var_type types x) acc)
                s
                Typing.Bot
            in
            match t with
            | Typing.Number (_, Typing.Unboxed) | Typing.Int Typing.Integer.Normalized ->
                Typing.set_return_type types g t
            | Typing.Top
            | Typing.Int Typing.Integer.Ref
            | Typing.Int Typing.Integer.Unnormalized
            | Typing.Number (_, Typing.Boxed)
            | Typing.Tuple _ | Typing.Bigarray _ | Typing.Bot -> ())
      | None -> ())
    ();
  let p = lower_conversions p types global_flow_info in
  if debug ()
  then (
    prerr_endline "BEFORE";
    Print.program Format.err_formatter (fun _ _ -> "") p);
  (* Collect function entry points *)
  let fun_entries = ref [ p.start ] in
  Addr.Map.iter
    (fun _ block ->
      List.iter
        ~f:(function
          | Let (_, Closure (_, (pc, _), _)) -> fun_entries := pc :: !fun_entries
          | _ -> ())
        block.body)
    p.blocks;
  (* Process each function independently to avoid cross-function variable leakage
     in the data flow analysis. *)
  let conv_types = ref ConvMap.empty in
  let blocks = ref p.blocks in
  List.iter
    ~f:(fun entry ->
      let fun_block_pcs = reachable_blocks !blocks entry in
      let fun_blocks =
        Addr.Map.filter (fun pc _ -> Addr.Set.mem pc fun_block_pcs) !blocks
      in
      let result = process_function types conv_types entry fun_blocks in
      Addr.Map.iter (fun pc block -> blocks := Addr.Map.add pc block !blocks) result)
    !fun_entries;
  let p = { p with blocks = !blocks } in
  if debug ()
  then (
    prerr_endline "AFTER";
    Print.program Format.err_formatter (fun _ _ -> "") p);
  p, types
