(*
We add spilling points at the end of each block and before each
possible GC: function calls and allocations. Local variables are
spilled at most once, at the first following spilling points.

We first compute which local variables contain valid values at the
beginning of each block: either there has been no GC since their
definition or they have been accessed since the last GC point (so they
must have been reloaded).
Then, we compute which variables neeeds to be spilled at some point
(we access the local variable while it does not contain any valid
value).
From this, we can compute what need to be spilled at each spilling
point, and the stack contents at any point in the program.

When allocating, we currently always spill everything. We should
probably spill everything only when a GC takes place. To keep the code
short, we should always spill variables that are still live after the
allocation, but variables that are no longer live after the allocation
only need to be spilled when a GC takes place.

We should find a way to reuse local variables while they are spilled,
to minimize the number of local variables used.
*)

let debug = Debug.find "spilling"

open! Stdlib
open Code

module Domain = struct
  type t =
    | Bot
    | Set of
        { input : Var.Set.t
        ; output : Var.Set.t
        }

  let bot = Bot

  let equal v v' =
    match v, v' with
    | Bot, Bot -> true
    | Bot, Set _ | Set _, Bot -> false
    | Set { input; _ }, Set { input = input'; _ } -> Var.Set.equal input input'
end

let make_table l =
  let h = Hashtbl.create 16 in
  List.iter ~f:(fun s -> Hashtbl.add h s ()) l;
  h

(*ZZZ See lambda/translprim.ml + stdlib *)
let no_alloc_tbl =
  make_table
    [ "caml_array_unsafe_set"
    ; "caml_string_unsafe_get"
    ; "caml_string_unsafe_set"
    ; "caml_bytes_unsafe_get"
    ; "caml_bytes_unsafe_set"
    ; "%int_add"
    ; "%int_sub"
    ; "%int_mul"
    ; "%int_neg"
    ; "%int_or"
    ; "%int_and"
    ; "%int_xor"
    ; "%int_lsl"
    ; "%int_lsr"
    ; "%int_asr"
    ]

let no_pointer_tbl =
  make_table
    [ "caml_string_unsafe_get"
    ; "caml_string_unsafe_set"
    ; "caml_bytes_unsafe_get"
    ; "caml_bytes_unsafe_set"
    ; "%int_add"
    ; "%int_sub"
    ; "%int_mul"
    ; "%int_neg"
    ; "%int_or"
    ; "%int_and"
    ; "%int_xor"
    ; "%int_lsl"
    ; "%int_lsr"
    ; "%int_asr"
    ]

let no_alloc p =
  match p with
  | Vectlength | Array_get | Not | IsInt | Eq | Neq | Lt | Le | Ult -> true
  | Extern nm -> Hashtbl.mem no_alloc_tbl nm (* ZZZ Refine *)

let no_pointer p =
  match p with
  | Vectlength | Not | IsInt | Eq | Neq | Lt | Le | Ult -> true
  | Extern nm -> Hashtbl.mem no_pointer_tbl nm (* ZZZ Refine *)
  | Array_get -> false

(*ZZZ from wa_generate *)
let get_free_variables ~context info =
  List.filter
    ~f:(fun x -> not (Hashtbl.mem context.Wa_code_generation.constants x))
    info.Wa_closure_conversion.free_variables

let function_free_variables ~context ~closures x =
  let info = Code.Var.Map.find x closures in
  let f, _ = List.hd info.Wa_closure_conversion.functions in
  if Code.Var.equal x f then get_free_variables ~context info else []

let get_set h x = try Hashtbl.find h x with Not_found -> Addr.Set.empty

let get_list h x = try Hashtbl.find h x with Not_found -> []

let cont_deps (deps, rev_deps) pc ?exn (pc', _) =
  Hashtbl.replace deps pc (Addr.Set.add pc' (get_set deps pc));
  Hashtbl.replace rev_deps pc' ((pc, exn) :: get_list rev_deps pc')

let block_deps bound_vars deps block pc =
  match fst block.branch with
  | Return _ | Raise _ | Stop -> ()
  | Branch cont | Poptrap cont -> cont_deps deps pc cont
  | Cond (_, cont1, cont2) ->
      cont_deps deps pc cont1;
      cont_deps deps pc cont2
  | Switch (_, a1, a2) ->
      Array.iter a1 ~f:(fun cont -> cont_deps deps pc cont);
      Array.iter a2 ~f:(fun cont -> cont_deps deps pc cont)
  | Pushtrap (cont, exn, cont_h, _) ->
      cont_deps deps pc cont;
      bound_vars := Var.Set.add exn !bound_vars;
      cont_deps deps pc ~exn cont_h

let function_deps blocks ~context ~closures pc params =
  let bound_vars = ref params in
  let non_spillable_vars = ref Var.Set.empty in
  let domain = ref Addr.Set.empty in
  let deps = Hashtbl.create 16, Hashtbl.create 16 in
  let mark_non_spillable x = non_spillable_vars := Var.Set.add x !non_spillable_vars in
  Code.traverse
    { fold = fold_children }
    (fun pc () ->
      domain := Addr.Set.add pc !domain;
      let block = Addr.Map.find pc blocks in
      List.iter
        ~f:(fun (i, _) ->
          match i with
          | Let (x, e) -> (
              match e with
              | Constant _ -> mark_non_spillable x
              | Prim (p, _) when no_pointer p -> mark_non_spillable x
              | Closure _
                when List.is_empty (function_free_variables ~context ~closures x) ->
                  mark_non_spillable x
              | Prim _ | Closure _ | Apply _ | Block _ | Field _ -> ())
          | Assign _ | Set_field _ | Offset_ref _ | Array_set _ -> ())
        block.body;
      bound_vars :=
        List.fold_left
          ~f:(fun vars (i, _) ->
            match i with
            | Let (x, _) -> Var.Set.add x vars
            | Assign _ | Set_field _ | Offset_ref _ | Array_set _ -> vars)
          ~init:!bound_vars
          block.body;
      bound_vars := Var.Set.union !bound_vars (Var.Set.of_list block.params);
      block_deps bound_vars deps block pc)
    pc
    blocks
    ();
  !domain, deps, !bound_vars, Var.Set.diff !bound_vars !non_spillable_vars

let inter s s' =
  match s, s' with
  | None, None -> None
  | _, None -> s
  | None, _ -> s'
  | Some s, Some s' -> Some (Var.Set.inter s s')

let propagate_through_expr ~context ~closures s x e =
  match e with
  | Apply _ | Block _ -> Var.Set.empty
  | Prim (p, _) -> if no_alloc p then s else Var.Set.empty
  | Closure _ ->
      if List.is_empty (function_free_variables ~context ~closures x)
      then s
      else Var.Set.empty
  | Constant _ | Field _ -> s

let propagate_through_instr ~context ~closures s (i, _) =
  match i with
  | Let (x, e) -> Var.Set.add x (propagate_through_expr ~context ~closures s x e)
  | Assign _ | Set_field _ | Offset_ref _ | Array_set _ -> s

let propagate blocks ~context ~closures rev_deps pc0 params st pc =
  let input =
    pc
    |> get_list rev_deps
    |> List.map ~f:(fun (pc', exn_opt) ->
           match Addr.Map.find pc' st with
           | Domain.Bot -> None
           | Set { output; _ } ->
               Some
                 (match exn_opt with
                 | None -> output
                 | Some x -> Var.Set.add x output))
    |> List.fold_left ~f:inter ~init:None
  in
  let input = if pc = pc0 then inter input (Some params) else input in
  match input with
  | None -> Domain.Bot
  | Some input ->
      let b = Addr.Map.find pc blocks in
      let input = Var.Set.union input (Var.Set.of_list b.params) in
      let output =
        List.fold_left
          ~f:(fun s i -> propagate_through_instr ~context ~closures s i)
          ~init:input
          b.body
      in
      Set { input; output }

module G = Dgraph.Make (Int) (Addr.Set) (Addr.Map)
module Solver = G.Solver (Domain)

type spill_ctx =
  { env : Var.t
  ; bound_vars : Var.Set.t
  ; spillable_vars : Var.Set.t
  ; context : Wa_code_generation.context
  }

let check_spilled ~ctx loaded x spilled =
  if Hashtbl.mem ctx.context.Wa_code_generation.constants x
  then spilled
  else
    let x = if Var.Set.mem x ctx.bound_vars then x else ctx.env in
    if Var.Set.mem x loaded || not (Var.Set.mem x ctx.spillable_vars)
    then spilled
    else Var.Set.add x spilled

let spilled_variables
    ~blocks
    ~context
    ~closures
    ~domain
    ~env
    ~bound_vars
    ~spillable_vars
    st =
  let spilled = Var.Set.empty in
  let ctx = { env; bound_vars; spillable_vars; context } in
  Addr.Set.fold
    (fun pc spilled ->
      let loaded =
        match Addr.Map.find pc st with
        | Domain.Bot -> assert false
        | Domain.Set { input; _ } -> input
      in
      let block = Addr.Map.find pc blocks in
      let loaded, spilled =
        List.fold_left
          ~f:(fun (loaded, spilled) i ->
            let loaded' = propagate_through_instr ~context ~closures loaded i in
            let reloaded =
              match fst i with
              | Let (x, e) -> (
                  match e with
                  | Apply { f; args; _ } ->
                      List.fold_left
                        ~f:(fun reloaded x -> check_spilled ~ctx loaded x reloaded)
                        (f :: args)
                        ~init:Var.Set.empty
                  | Block (_, l, _) ->
                      Array.fold_left
                        ~f:(fun reloaded x -> check_spilled ~ctx loaded' x reloaded)
                        l
                        ~init:Var.Set.empty
                  | Prim (_, args) ->
                      List.fold_left
                        ~f:(fun reloaded x ->
                          match x with
                          | Pv x -> check_spilled ~ctx loaded x reloaded
                          | Pc _ -> reloaded)
                        args
                        ~init:Var.Set.empty
                  | Closure _ ->
                      let fv = function_free_variables ~context ~closures x in
                      List.fold_left
                        ~f:(fun reloaded x -> check_spilled ~ctx loaded' x reloaded)
                        fv
                        ~init:Var.Set.empty
                  | Constant _ -> Var.Set.empty
                  | Field (x, _) -> check_spilled ~ctx loaded x Var.Set.empty)
              | Assign (_, x) | Offset_ref (x, _) ->
                  check_spilled ~ctx loaded x Var.Set.empty
              | Set_field (x, _, y) ->
                  Var.Set.empty
                  |> check_spilled ~ctx loaded x
                  |> check_spilled ~ctx loaded y
              | Array_set (x, y, z) ->
                  Var.Set.empty
                  |> check_spilled ~ctx loaded x
                  |> check_spilled ~ctx loaded y
                  |> check_spilled ~ctx loaded z
            in
            Var.Set.union loaded' reloaded, Var.Set.union spilled reloaded)
          ~init:(loaded, spilled)
          block.body
      in
      let handle_cont (_, args) spilled =
        List.fold_left
          ~f:(fun spilled x -> check_spilled ~ctx loaded x spilled)
          args
          ~init:spilled
      in
      match fst block.branch with
      | Return x | Raise (x, _) -> check_spilled ~ctx loaded x spilled
      | Stop -> spilled
      | Branch cont | Poptrap cont -> handle_cont cont spilled
      | Cond (_, cont1, cont2) -> spilled |> handle_cont cont1 |> handle_cont cont2
      | Switch (_, a1, a2) ->
          let spilled = Array.fold_right a1 ~f:handle_cont ~init:spilled in
          Array.fold_right a2 ~f:handle_cont ~init:spilled
      | Pushtrap (cont, _, cont_h, _) -> spilled |> handle_cont cont |> handle_cont cont_h)
    domain
    spilled

let traverse ~f pc blocks input =
  let rec traverse_rec f pc visited blocks inp =
    if not (Addr.Set.mem pc visited)
    then
      let visited = Addr.Set.add pc visited in
      let out = f pc inp in
      Code.fold_children
        blocks
        pc
        (fun pc visited -> traverse_rec f pc visited blocks out)
        visited
    else visited
  in
  ignore (traverse_rec f pc Addr.Set.empty blocks input)

let filter_stack live stack =
  List.fold_right
    ~f:(fun v rem ->
      match v, rem with
      | Some x, _ when Var.Set.mem x live -> v :: rem
      | _, [] -> []
      | _ -> None :: rem)
    stack
    ~init:[]

let rec spill i x stack =
  match stack with
  | None :: rem -> i, Some x :: rem
  | [] -> i, [ Some x ]
  | v :: rem ->
      let i, rem = spill (i + 1) x rem in
      i, v :: rem

let spill_vars live vars stack =
  let stack = filter_stack live stack in
  let stack, spills =
    Var.Set.fold
      (fun x (stack, spills) ->
        let i, stack = spill 0 x stack in
        stack, (x, i) :: spills)
      vars
      (stack, [])
  in
  let last = List.length stack - 1 in
  stack, List.map ~f:(fun (x, i) -> x, last - i) spills

let print_stack s =
  if List.is_empty s
  then ""
  else
    Format.asprintf
      "{%a}"
      (fun f l ->
        Format.pp_print_list
          ~pp_sep:(fun f () -> Format.fprintf f " ")
          (fun f v ->
            match v with
            | None -> Format.fprintf f "*"
            | Some x -> Var.print f x)
          f
          l)
      s

type stack = Var.t option list

type spilling_info =
  { depth_change : int
  ; spills : (Var.t * int) list
  ; stack : stack
  }

let print_spilling { depth_change; spills; stack; _ } =
  let print_actions f l =
    Format.pp_print_list
      ~pp_sep:(fun f () -> Format.fprintf f " ")
      (fun f (x, i) -> Format.fprintf f "%d:%a" i Var.print x)
      f
      l
  in
  if false
  then print_stack stack
  else Format.asprintf "%d %s {%a}" depth_change (print_stack stack) print_actions spills

type block_info =
  { initial_stack : stack (* Stack at beginning of block *)
  ; loaded_variables : Var.Set.t (* Values in local variables at beginning of block *)
  ; spilling : spilling_info (* Spilling at end of block *)
  }

type info =
  { max_depth : int
  ; subcalls : bool
  ; env : Var.t
  ; bound_vars : Var.Set.t
  ; initial_spilling : spilling_info
  ; block : block_info Addr.Map.t
  ; instr : spilling_info Var.Map.t
  }

let update_stack ~max_depth live_vars vars stack =
  let stack', spills = spill_vars live_vars vars stack in
  max_depth := max !max_depth (List.length stack);
  { depth_change = List.length stack' - List.length stack; stack = stack'; spills }

let spilling blocks st env bound_vars spilled_vars live_info pc params =
  let stack = [] in
  let max_depth = ref 0 in
  let subcalls = ref false in
  let vars = Var.Set.inter params spilled_vars in
  let stack, spills = spill_vars Var.Set.empty vars stack in
  let initial_spilling = { depth_change = List.length stack; stack; spills } in
  let instr_info = ref Var.Map.empty in
  let block_info = ref Addr.Map.empty in
  traverse pc blocks stack ~f:(fun pc stack ->
      let block = Addr.Map.find pc blocks in
      let block_live_vars = Addr.Map.find pc live_info.Wa_liveness.block in
      let initial_stack, _ =
        spill_vars block_live_vars.initially_live Var.Set.empty stack
      in
      let vars = Var.Set.inter (Var.Set.of_list block.params) spilled_vars in
      let stack, vars =
        List.fold_left
          ~f:(fun (stack, vars) (i, _) ->
            let stack, vars =
              match i with
              | Let (x, e) -> (
                  match e with
                  | Apply _ | Block _ | Closure _ ->
                      let live_vars = Var.Map.find x live_info.instr in
                      let ({ stack; _ } as sp) =
                        update_stack ~max_depth live_vars vars stack
                      in
                      instr_info := Var.Map.add x sp !instr_info;
                      (match e with
                      | Apply _ when not (List.is_empty stack) -> subcalls := true
                      | _ -> ());
                      stack, Var.Set.empty
                  | Prim (p, _) when not (no_alloc p) ->
                      let live_vars = Var.Map.find x live_info.instr in
                      let ({ stack; _ } as sp) =
                        update_stack ~max_depth live_vars vars stack
                      in
                      instr_info := Var.Map.add x sp !instr_info;
                      stack, Var.Set.empty
                  | Prim _ | Constant _ | Field _ -> stack, vars)
              | Assign _ | Offset_ref _ | Set_field _ | Array_set _ -> stack, vars
            in
            let vars =
              match i with
              | Let (x, _) when Var.Set.mem x spilled_vars -> Var.Set.add x vars
              | _ -> vars
            in
            stack, vars)
          ~init:(initial_stack, vars)
          block.body
      in
      (* ZZZ Spilling at end of block *)
      let ({ stack; _ } as sp) =
        update_stack ~max_depth block_live_vars.live_before_branch vars stack
      in
      let loaded_variables =
        match Addr.Map.find pc st with
        | Domain.Bot -> assert false
        | Domain.Set { input; _ } -> input
      in
      block_info :=
        Addr.Map.add pc { initial_stack; loaded_variables; spilling = sp } !block_info;
      stack);
  { max_depth = !max_depth
  ; subcalls = !subcalls
  ; env
  ; bound_vars
  ; initial_spilling
  ; block = !block_info
  ; instr = !instr_info
  }

let generate_spilling_information { blocks; _ } ~context ~closures ~pc:pc0 ~env ~params =
  let params = Var.Set.add env (Var.Set.of_list params) in
  let domain, (deps, rev_deps), bound_vars, spillable_vars =
    function_deps blocks ~context ~closures pc0 params
  in
  let fold_children f pc acc = Addr.Set.fold f (get_set deps pc) acc in
  let g = { G.domain; fold_children } in
  let st =
    Solver.f g (fun st pc ->
        propagate blocks ~context ~closures rev_deps pc0 params st pc)
  in
  let spilled_vars =
    spilled_variables
      ~blocks
      ~context
      ~closures
      ~domain
      ~env
      ~bound_vars
      ~spillable_vars
      st
  in
  if debug ()
  then (
    Format.eprintf "PARAMS: (%a)" Var.print env;
    Var.Set.iter (fun x -> Format.eprintf " %a" Var.print x) params;
    Format.eprintf "@.";
    Format.eprintf "SPILLED:";
    Var.Set.iter (fun x -> Format.eprintf " %a" Var.print x) spilled_vars;
    Format.eprintf "@.");
  (*
  Addr.Set.iter
    (fun pc ->
      let s = Addr.Map.find pc st in
      (match s with
      | Domain.Bot -> ()
      | Domain.Set { input; output } ->
          Format.eprintf "INPUT:";
          Var.Set.iter (fun x -> Format.eprintf " %a" Var.print x) input;
          Format.eprintf "@.";
          Format.eprintf "OUTPUT:";
          Var.Set.iter (fun x -> Format.eprintf " %a" Var.print x) output;
          Format.eprintf "@.");
      let block = Addr.Map.find pc blocks in
      Code.Print.block (fun _ _ -> "") pc block)
    domain;
  *)
  let live_info =
    Wa_liveness.f
      ~blocks
      ~context
      ~closures
      ~domain
      ~env
      ~bound_vars
      ~spilled_vars
      ~pc:pc0
  in
  let info = spilling blocks st env bound_vars spilled_vars live_info pc0 params in
  if debug ()
  then (
    Format.eprintf "== %d == depth %d calls %b@." pc0 info.max_depth info.subcalls;
    Format.eprintf "%s@." (print_spilling info.initial_spilling);
    Addr.Set.iter
      (fun pc ->
        let block = Addr.Map.find pc blocks in
        let _print_vars s =
          if Var.Set.is_empty s
          then ""
          else
            Format.asprintf
              "{%a}"
              (fun f l ->
                Format.pp_print_list
                  ~pp_sep:(fun f () -> Format.fprintf f " ")
                  Var.print
                  f
                  l)
              (Var.Set.elements s)
        in
        Code.Print.block
          (fun _pc loc ->
            match loc with
            | Instr (Let (x, _), _) -> (
                match Var.Map.find_opt x info.instr with
                | Some s -> print_spilling s
                | None -> "")
            | Instr _ -> ""
            | Last _ ->
                let s = Addr.Map.find pc info.block in
                print_spilling s.spilling)
          pc
          block)
      domain);
  info

type context =
  { loaded_variables : Var.Set.t
  ; loaded_sp : Code.Var.t option
  ; stack : stack
  ; info : info
  ; context : Wa_code_generation.context
  }

type ctx = context ref

open Wa_code_generation
module W = Wa_ast

let rec find_in_stack x stack =
  match stack with
  | [] -> raise Not_found
  | Some y :: rem when Var.equal x y -> List.length rem
  | _ :: rem -> find_in_stack x rem

let load_sp ctx =
  match !ctx.loaded_sp with
  | Some sp -> return sp
  | None ->
      let sp = Var.fresh_n "sp" in
      ctx := { !ctx with loaded_sp = Some sp };
      let* () = store sp (return (W.GlobalGet (S "sp"))) in
      return sp

let perform_reloads ctx l =
  let vars = ref Var.Map.empty in
  let add_var x =
    if not (Hashtbl.mem !ctx.context.Wa_code_generation.constants x)
    then
      let x = if Var.Set.mem x !ctx.info.bound_vars then x else !ctx.info.env in
      if not (Var.Set.mem x !ctx.loaded_variables)
      then
        try
          let i = find_in_stack x !ctx.stack in
          vars := Var.Map.add x i !vars
        with Not_found -> ()
  in
  (match l with
  | `Instr i -> Freevars.iter_instr_free_vars add_var i
  | `Branch l -> Freevars.iter_last_free_var add_var l
  | `Vars s -> Var.Set.iter add_var s);
  if Var.Map.is_empty !vars
  then return ()
  else
    let* sp = load_sp ctx in
    let* () =
      List.fold_left
        ~f:(fun before (x, i) ->
          let* () = before in
          let* sp = load sp in
          let offset = 4 * i in
          store x (return (W.Load (I32 (Int32.of_int offset), sp))))
        (List.sort ~cmp:(fun (_, i) (_, j) -> compare i j) (Var.Map.bindings !vars))
        ~init:(return ())
    in
    ctx :=
      { !ctx with
        loaded_variables =
          Var.Set.union
            !ctx.loaded_variables
            (Var.Map.fold (fun x _ s -> Var.Set.add x s) !vars Var.Set.empty)
      };
    return ()

let assign ctx x =
  match find_in_stack x !ctx.stack with
  | exception Not_found -> return ()
  | i ->
      let* sp = load_sp ctx in
      let* sp = load sp in
      let* x = load x in
      let offset = 4 * i in
      instr (W.Store (I32 (Int32.of_int offset), sp, x))

let perform_spilling ctx loc =
  match
    match loc with
    | `Function -> !ctx.info.initial_spilling
    | `Instr x -> Var.Map.find x !ctx.info.instr
    | `Block pc -> (Addr.Map.find pc !ctx.info.block).spilling
  with
  | exception Not_found -> return ()
  | spilling ->
      if spilling.depth_change = 0 && List.is_empty spilling.spills
      then return ()
      else
        let* sp = load_sp ctx in
        let* sp =
          if spilling.depth_change = 0
          then return sp
          else
            let sp' = Var.fresh_n "sp" in
            let delta = -4 * spilling.depth_change in
            let* sp = tee sp' Arith.(load sp + const (Int32.of_int delta)) in
            ctx := { !ctx with loaded_sp = Some sp' };
            let* () = instr (W.GlobalSet (S "sp", sp)) in
            return sp'
        in
        let* () =
          List.fold_left
            ~f:(fun before (x, i) ->
              let* () = before in
              let* sp = load sp in
              let* x = load x in
              let offset = 4 * i in
              instr (W.Store (I32 (Int32.of_int offset), sp, x)))
            spilling.spills
            ~init:(return ())
        in
        ctx := { !ctx with stack = spilling.stack };
        return ()

let adjust_stack ctx ~src ~dst =
  let src_stack =
    if src = -1 then !ctx.stack else (Addr.Map.find src !ctx.info.block).spilling.stack
  in
  let dst_info = Addr.Map.find dst !ctx.info.block in
  let delta = List.length dst_info.initial_stack - List.length src_stack in
  if delta = 0
  then return ()
  else
    let* sp = load_sp ctx in
    let delta = -4 * delta in
    let* sp = Arith.(load sp + const (Int32.of_int delta)) in
    instr (W.GlobalSet (S "sp", sp))

let stack_adjustment_needed ctx ~src ~dst =
  let src_stack =
    if src = -1 then !ctx.stack else (Addr.Map.find src !ctx.info.block).spilling.stack
  in
  let dst_info = Addr.Map.find dst !ctx.info.block in
  let delta = List.length dst_info.initial_stack - List.length src_stack in
  delta <> 0

let start_block ~context spilling_info pc =
  let info = Addr.Map.find pc spilling_info.block in
  ref
    { loaded_variables = info.loaded_variables
    ; loaded_sp = None
    ; stack = info.initial_stack
    ; info = spilling_info
    ; context
    }

let start_function ~context (spilling_info : info) =
  (*ZZZ Check stack depth *)
  ref
    { loaded_variables = Var.Set.empty
    ; loaded_sp = None
    ; stack = []
    ; info = spilling_info
    ; context
    }

let kill_variables ctx =
  ctx := { !ctx with loaded_variables = Var.Set.empty; loaded_sp = None }

let make_info () =
  { max_depth = 0
  ; subcalls = false
  ; env = Var.fresh ()
  ; bound_vars = Var.Set.empty
  ; initial_spilling = { depth_change = 0; spills = []; stack = [] }
  ; block = Addr.Map.empty
  ; instr = Var.Map.empty
  }

let add_spilling info ~location:x ~stack ~live_vars ~spilled_vars =
  let max_depth = ref info.max_depth in
  let spilling = update_stack ~max_depth live_vars spilled_vars stack in
  ( { info with
      max_depth = !max_depth
    ; instr = Var.Map.add x spilling info.instr
    ; bound_vars = Var.Set.union info.bound_vars spilled_vars
    }
  , spilling.stack )

(*
ZZZ TODO
- We could improve the code generated for stack adjustment after a switch
- We need to deal with exceptions...
- Check available stack depth at beginning of function (also for curry/apply)
- We could zero-out no longer used stack slots to avoid memory leaks
*)
