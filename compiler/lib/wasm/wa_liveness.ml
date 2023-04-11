(*
ZZZ If live in exception handler, live any place we may raise in the body
*)

open! Stdlib
open Code

module Domain = struct
  type t =
    { input : Var.Set.t
    ; output : Var.Set.t
    }

  let bot = { input = Var.Set.empty; output = Var.Set.empty }

  let equal v v' = Var.Set.equal v.input v'.input
end

(*ZZZ from wa_generate *)
let get_free_variables ~context info =
  List.filter
    ~f:(fun x -> not (Hashtbl.mem context.Wa_code_generation.constants x))
    info.Wa_closure_conversion.free_variables

let function_free_variables ~context ~closures x =
  let info = Var.Map.find x closures in
  let f, _ = List.hd info.Wa_closure_conversion.functions in
  if Var.equal x f then get_free_variables ~context info else []

let get_set h x = try Hashtbl.find h x with Not_found -> Addr.Set.empty

let cont_deps (deps, rev_deps) pc (pc', _) =
  Hashtbl.replace deps pc' (Addr.Set.add pc (get_set deps pc'));
  Hashtbl.replace rev_deps pc (Addr.Set.add pc' (get_set rev_deps pc))

let block_deps deps block pc =
  match fst block.branch with
  | Return _ | Raise _ | Stop -> ()
  | Branch cont | Poptrap cont -> cont_deps deps pc cont
  | Cond (_, cont1, cont2) ->
      cont_deps deps pc cont1;
      cont_deps deps pc cont2
  | Switch (_, a1, a2) ->
      Array.iter a1 ~f:(fun cont -> cont_deps deps pc cont);
      Array.iter a2 ~f:(fun cont -> cont_deps deps pc cont)
  | Pushtrap (cont, _, cont_h, _) ->
      cont_deps deps pc cont;
      cont_deps deps pc cont_h

let function_deps blocks pc =
  let deps = Hashtbl.create 16, Hashtbl.create 16 in
  Code.traverse
    { fold = fold_children }
    (fun pc () ->
      let block = Addr.Map.find pc blocks in
      block_deps deps block pc)
    pc
    blocks
    ();
  deps

type ctx =
  { env : Var.t
  ; bound_vars : Var.Set.t
  ; spilled_vars : Var.Set.t
  ; context : Wa_code_generation.context
  }

let add_var ~ctx s x =
  if Hashtbl.mem ctx.context.Wa_code_generation.constants x
  then s
  else
    let x = if Var.Set.mem x ctx.bound_vars then x else ctx.env in
    if Var.Set.mem x ctx.spilled_vars then Var.Set.add x s else s

let add_list ~ctx s l = List.fold_left ~f:(fun s x -> add_var ~ctx s x) ~init:s l

let add_prim_args ~ctx s l =
  List.fold_left
    ~f:(fun s x ->
      match x with
      | Pc _ -> s
      | Pv x -> add_var ~ctx s x)
    ~init:s
    l

let add_array ~ctx s a = Array.fold_left ~f:(fun s x -> add_var ~ctx s x) ~init:s a

let expr_used ~context ~closures ~ctx x e s =
  match e with
  | Apply { f; args; _ } -> add_list ~ctx s (f :: args)
  | Block (_, a, _) -> add_array ~ctx s a
  | Prim (_, l) -> add_prim_args ~ctx s l
  | Closure _ -> add_list ~ctx s (function_free_variables ~context ~closures x)
  | Constant _ -> s
  | Field (x, _) -> add_var ~ctx s x

let propagate_through_instr ~context ~closures ~ctx (i, _) s =
  match i with
  | Let (x, e) -> expr_used ~context ~closures ~ctx x e (Var.Set.remove x s)
  | Set_field (x, _, y) -> add_var ~ctx (add_var ~ctx s x) y
  | Assign (_, x) | Offset_ref (x, _) -> add_var ~ctx s x
  | Array_set (x, y, z) -> add_var ~ctx (add_var ~ctx (add_var ~ctx s x) y) z

let cont_used ~ctx (_, args) s = add_list ~ctx s args

let propagate_through_branch ~ctx (b, _) s =
  match b with
  | Return x | Raise (x, _) -> add_var ~ctx s x
  | Stop -> s
  | Branch cont | Poptrap cont -> cont_used ~ctx cont s
  | Cond (_, cont1, cont2) -> s |> cont_used ~ctx cont1 |> cont_used ~ctx cont2
  | Switch (_, a1, a2) ->
      let s = Array.fold_right a1 ~f:(fun cont s -> cont_used ~ctx cont s) ~init:s in
      Array.fold_right a2 ~f:(fun cont s -> cont_used ~ctx cont s) ~init:s
  | Pushtrap (cont, x, cont_h, _) ->
      s |> cont_used ~ctx cont |> cont_used ~ctx cont_h |> Var.Set.remove x

let propagate blocks ~context ~closures ~ctx rev_deps st pc =
  let input =
    pc
    |> get_set rev_deps
    |> Addr.Set.elements
    |> List.map ~f:(fun pc' -> (Addr.Map.find pc' st).Domain.output)
    |> List.fold_left ~f:Var.Set.union ~init:Var.Set.empty
  in
  let b = Addr.Map.find pc blocks in
  let s = propagate_through_branch ~ctx b.branch input in
  let output =
    List.fold_right
      ~f:(fun i s -> propagate_through_instr ~context ~closures ~ctx i s)
      ~init:s
      b.body
  in
  let output = Var.Set.diff output (Var.Set.of_list b.params) in
  { Domain.input; output }

module G = Dgraph.Make (Int) (Addr.Set) (Addr.Map)
module Solver = G.Solver (Domain)

type block_info =
  { initially_live : Var.Set.t (* Live at start of block *)
  ; live_before_branch : Var.Set.t
  }

type info =
  { instr : Var.Set.t Var.Map.t (* Live variables at spilling point *)
  ; block : block_info Addr.Map.t
  }

let compute_instr_info ~blocks ~context ~closures ~domain ~ctx st =
  Addr.Set.fold
    (fun pc live_info ->
      let live_vars = (Addr.Map.find pc st).Domain.input in
      let block = Addr.Map.find pc blocks in
      let live_vars = propagate_through_branch ~ctx block.Code.branch live_vars in
      let _, live_info =
        List.fold_right
          ~f:(fun i (live_vars, live_info) ->
            let live_vars' =
              propagate_through_instr ~context ~closures ~ctx i live_vars
            in
            let live_info =
              match fst i with
              | Let (x, e) -> (
                  match e with
                  | Apply _ | Prim _ ->
                      Var.Map.add x (Var.Set.remove x live_vars) live_info
                  | Block _ | Closure _ -> Var.Map.add x live_vars' live_info
                  | Constant _ | Field _ -> live_info)
              | Assign _ | Offset_ref _ | Set_field _ | Array_set _ -> live_info
            in
            live_vars', live_info)
          ~init:(live_vars, live_info)
          block.body
      in
      live_info)
    domain
    Var.Map.empty

let compute_block_info ~blocks ~ctx st =
  Addr.Map.mapi
    (fun pc { Domain.input; output } ->
      let block = Addr.Map.find pc blocks in
      let live_before_branch = propagate_through_branch ~ctx block.Code.branch input in
      { initially_live = output; live_before_branch })
    st

let f ~blocks ~context ~closures ~domain ~env ~bound_vars ~spilled_vars ~pc =
  let ctx = { env; bound_vars; spilled_vars; context } in
  let deps, rev_deps = function_deps blocks pc in
  let fold_children f pc acc = Addr.Set.fold f (get_set deps pc) acc in
  let g = { G.domain; fold_children } in
  let st =
    Solver.f g (fun st pc -> propagate blocks ~context ~closures ~ctx rev_deps st pc)
  in
  let instr = compute_instr_info ~blocks ~context ~closures ~domain ~ctx st in
  let block = compute_block_info ~blocks ~ctx st in
  (*
  Addr.Set.iter
    (fun pc ->
      let { Domain.input; output } = Addr.Map.find pc st in
      Format.eprintf "input:";
      Var.Set.iter (fun x -> Format.eprintf " %a" Var.print x) input;
      Format.eprintf "@.";
      Format.eprintf "output:";
      Var.Set.iter (fun x -> Format.eprintf " %a" Var.print x) output;
      Format.eprintf "@.";
      let b = Addr.Map.find pc blocks in
      let print_vars s =
        Format.asprintf
          "{%a}"
          (fun f l ->
            Format.pp_print_list ~pp_sep:(fun f () -> Format.fprintf f " ") Var.print f l)
          (Var.Set.elements s)
      in
      Code.Print.block
        (fun _pc loc ->
          match loc with
          | Instr (Let (x, _), _) -> (
              match Var.Map.find_opt x instr with
              | Some s -> print_vars s
              | None -> "")
          | Instr _ -> ""
          | Last _ ->
              let s = Addr.Map.find pc block in
              print_vars s.live_before_branch)
        pc
        b)
    domain;
  *)
  { block; instr }
