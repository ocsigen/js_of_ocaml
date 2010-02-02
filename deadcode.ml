
let debug = false

open Util

open Code

type t =
  { blocks : block IntMap.t;
    live : int array;
    deps : instr list array;
    mutable live_block : IntSet.t }

(****)

let pure_prims = ["caml_int64_float_of_bits"; "caml_sys_get_argv"]

(****)

let req_expr e =
  match e with
    Const _  | Block _ | Field _ | Closure _ | Constant _ | Variable _ ->
      false
  | Apply _ | Direct_apply _ ->
      true
  | Prim (p, l) ->
      match p with
        C_call f -> not (List.mem f pure_prims)
      | _        -> false

let req_instr i =
  match i with
    Let (_, e) ->
      req_expr e
  | Assign _ | Set_field _ | Offset_ref _ | Array_set _ ->
      false

(****)

let rec mark_var st x =
  let x = Var.idx x in
  st.live.(x) <- st.live.(x) + 1;
  if st.live.(x) = 1 then
    List.iter (fun i -> if not (req_instr i) then mark_instr st i) st.deps.(x)

and mark_expr st e =
  match e with
    Const _ | Constant _ ->
      ()
  | Apply (f, l) | Direct_apply (f, l) ->
      mark_var st f; List.iter (fun x -> mark_var st x) l
  | Block (_, a) ->
      Array.iter (fun x -> mark_var st x) a
  | Field (x, _) ->
      mark_var st x
  | Closure (_, pc) ->
      mark_req st pc
  | Prim (_, l) ->
      List.iter (fun x -> mark_var st x) l
  | Variable x ->
      mark_var st x

and mark_instr st i =
  match i with
    Let (_, e) ->
      mark_expr st e
  | Assign (_, x)
  | Set_field (_, _, x) ->
      mark_var st x
  | Array_set (_, x, y) ->
      mark_var st x; mark_var st y
  | Offset_ref _ ->
      assert false

and mark_cont st (pc, param) =
  mark_req st pc (*; opt_iter (mark_var st) param*)

and mark_req st pc =
  if not (IntSet.mem pc st.live_block) then begin
    st.live_block <- IntSet.add pc st.live_block;
    let (_, instr, last) = IntMap.find pc st.blocks in
    List.iter
      (fun i ->
         match i with
           Let (_, e) ->
             if req_expr e then mark_expr st e
         | Assign (x, _) | Set_field (x, _, _) | Offset_ref (x, _)
         | Array_set (x, _, _) ->
             mark_var st x)
      instr;
    match last with
      Return x | Raise x ->
        mark_var st x
    | Stop ->
        ()
    | Branch cont ->
        mark_cont st cont
    | Cond (_, x, cont1, cont2) ->
        mark_var st x; mark_cont st cont1; mark_cont st cont2
    | Switch (x, a1, a2) ->
        mark_var st x;
        Array.iter (fun cont -> mark_cont st cont) a1;
        Array.iter (fun cont -> mark_cont st cont) a2
    | Pushtrap (cont, pc, _) ->
        mark_cont st cont; mark_req st pc
    | Poptrap cont ->
        mark_cont st cont

  end

(****)

let ref_count st i =
  match i with
    Let (x, _) -> st.live.(Var.idx x)
  | _          -> 0

let fully_live_instr st i =
  match i with
    Let (x, _) | Assign (x, _)
  | Set_field (x, _, _) | Offset_ref (x, _) | Array_set (x, _, _) ->
      st.live.(Var.idx x) > 0

let live_instr st i = req_instr i || fully_live_instr st i

let filter_cont blocks st (pc, arg) =
  let (param, _, _) =
    try
      IntMap.find pc blocks
    with Not_found ->
      assert (pc = -1); (None, [], Stop)
  in
  match param with
    Some x when st.live.(Var.idx x) > 0 ->
      (pc, arg)
  | _ ->
      (pc, None)

let filter_live_last blocks st l =
  match l with
    Return _ | Raise _ | Stop ->
      l
  | Branch cont ->
      Branch (filter_cont blocks st cont)
  | Cond (c, x, cont1, cont2) ->
      Cond (c, x, filter_cont blocks st cont1, filter_cont blocks st cont2)
  | Switch (x, a1, a2) ->
      Switch (x,
              Array.map (fun cont -> filter_cont blocks st cont) a1,
              Array.map (fun cont -> filter_cont blocks st cont) a2)
  | Pushtrap (cont1, pc, cont2) ->
      Pushtrap (filter_cont blocks st cont1, pc, filter_cont blocks st cont2)
  | Poptrap cont ->
      Poptrap (filter_cont blocks st cont)

let annot st pc xi =
  if not (IntSet.mem pc st.live_block) then "x" else
  match xi with
    Last _ ->
      " "
  | Instr i ->
      if fully_live_instr st i then begin
        let c = ref_count st i in
        if c = 0 then " " else Format.sprintf "%d" c
      end else
        if req_instr i then "*" else "x"

let add_dep deps x i =
  let idx = Var.idx x in
  deps.(idx) <- i :: deps.(idx)

let add_cont_dep blocks deps (pc, arg) =
  match arg with
    None ->
      ()
  | Some x ->
      let e = Variable x in
      match IntMap.find pc blocks with
        (Some y, _, _) -> add_dep deps y (Let (y, e))
      | _              -> ()  (* We can have a value in the accumulator
                                 which is not used afterwards... *)

let f (pc, blocks, free_pc) =
  let nv = Var.count () in
  let deps = Array.make nv [] in
  let live = Array.make nv 0 in
  IntMap.iter
    (fun _ (_, instr, last) ->
       List.iter
         (fun i ->
            match i with
              Let (x, _) | Assign (x, _)
            | Set_field (x, _, _) | Array_set (x, _, _) ->
                add_dep deps x i
            | Offset_ref _  ->
                ())
         instr;
       match last with
         Return _ | Raise _ | Stop ->
           ()
       | Branch cont ->
           add_cont_dep blocks deps cont
       | Cond (_, _, cont1, cont2) ->
           add_cont_dep blocks deps cont1;
           add_cont_dep blocks deps cont2
       | Switch (_, a1, a2) ->
           Array.iter (fun cont -> add_cont_dep blocks deps cont) a1;
           Array.iter (fun cont -> add_cont_dep blocks deps cont) a2
       | Pushtrap (cont, _, _) ->
           add_cont_dep blocks deps cont
       | Poptrap cont ->
           add_cont_dep blocks deps cont)
    blocks;
  let st =
    { live = live; deps = deps; blocks = blocks; live_block = IntSet.empty }
  in
  mark_req st pc;

  if debug then
    print_program (fun pc xi -> annot st pc xi) (pc, blocks, free_pc);

  let all_blocks = blocks in
  let blocks =
    IntMap.fold
      (fun pc (param, instr, last) blocks ->
         if not (IntSet.mem pc st.live_block) then blocks else
         IntMap.add pc
           (opt_filter (fun x -> st.live.(Var.idx x) > 0) param,
            List.filter (fun i -> live_instr st i) instr,
            filter_live_last all_blocks st last)
           blocks)
      blocks IntMap.empty
  in
  (pc, blocks, free_pc), st.live
