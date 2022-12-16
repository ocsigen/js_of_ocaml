(*
Closure in CPS if contains any calling point in CPS
                      => one-way dep
   or if called from CPS calling point => two-way dep
   or used in unknown context  ==> possibly mutable

Calling point in CPS if function unknown
                        ==> known_origins = false
    or function in CPS
                       ==> dep

When a function escape but is also used directly, maybe we should wrap
it so that we do not have to convert it to CPS:
- Three levels: CPS, undecided, direct
- For each undecided call site, duplicate the corresponding functions
*)
open Stdlib
open Code

let add_var s x = s := Var.Set.add x !s

(* x depends on y *)
let add_dep deps x y =
  if not (Var.Map.mem x !deps) then deps := Var.Map.add x Var.Set.empty !deps;
  deps :=
    Var.Map.add
      y
      (Var.Set.add x (try Var.Map.find y !deps with Not_found -> Var.Set.empty))
      !deps

let rec list_iter ~f = function
  | [] -> ()
  | [ a ] -> f true a
  | a :: l ->
      f false a;
      list_iter ~f l

let block_deps ~info ~vars ~tail_deps ~deps ~blocks ~fun_name pc =
  let block = Code.Addr.Map.find pc blocks in
  list_iter block.Code.body ~f:(fun is_last i ->
      match i with
      | Code.Let (x, Apply { f; _ }) ->
          let tail_call =
            is_last
            &&
            match block.Code.branch with
            | Return x' -> Var.equal x x'
            | _ -> false
          in
          add_var vars x;
          (match fun_name with
          | None -> ()
          | Some fun_name ->
              add_var vars fun_name;
              add_dep deps fun_name x);
          Var.Set.iter
            (fun g ->
              add_var vars g;
              (if tail_call
              then
                match fun_name with
                | None -> ()
                | Some fun_name -> add_dep tail_deps fun_name g);
              add_dep deps x g;
              add_dep deps g x)
            (Var.Tbl.get info.Flow.info_known_origins f)
      | Code.Let (x, Prim (Extern ("%perform" | "%reperform" | "%resume"), _)) -> (
          add_var vars x;
          match fun_name with
          | None -> ()
          | Some fun_name ->
              add_var vars fun_name;
              add_dep deps fun_name x)
      | Code.Let (x, (Closure _ | Prim (Extern "%closure", _))) -> add_var vars x
      | _ -> ())

module G' = Dgraph.Make (Var) (Var.Set) (Var.Map)

module Domain = struct
  type t = bool

  let equal = Bool.equal

  let bot = false
end

module Solver = G'.Solver (Domain)

let fold_children g f x acc = g.G'.fold_children (fun y acc -> f y acc) x acc

let cps_needed ~info ~in_loop ~rev_deps st x =
  let res =
    Var.Set.mem x in_loop
    ||
    let idx = Var.idx x in
    fold_children
      rev_deps
      (fun y acc -> acc || Var.Map.find y st)
      x
      (match info.Flow.info_defs.(idx) with
      | Flow.Expr (Apply { f; _ }) ->
          Var.Tbl.get info.Flow.info_maybe_unknown f
          || Var.Set.exists
               (fun g ->
                 match info.Flow.info_defs.(Var.idx g) with
                 | Expr (Closure _ | Prim (Extern "%closure", _)) -> false
                 | _ -> true)
               (Var.Tbl.get info.Flow.info_known_origins f)
      | Flow.Expr (Closure _) | Flow.Expr (Prim (Extern "%closure", _)) ->
          info.Flow.info_possibly_mutable.(idx)
      | Flow.Expr (Prim (Extern ("%perform" | "%reperform" | "%resume"), _)) -> true
      | _ -> false)
  in
  (*
  if res && not (Var.Tbl.get st x)
  then
    Format.eprintf
      "===> %a (%b / %b %s)@."
      Code.Var.print
      x
      (Var.Tbl.get info.Flow.info_maybe_unknown x)
      info.Flow.info_possibly_mutable.(Var.idx x)
      (let idx = Var.idx x in
       match info.Flow.info_defs.(idx) with
       | Flow.Expr (Apply { f = _f; _ }) ->
           "apply" (*Var.Tbl.get info.Flow.info_maybe_unknown f*)
       | Flow.Expr (Closure _) -> "closure" (*info.Flow.info_possibly_mutable.(idx)*)
       | _ -> "other");
*)
  res

module SCC = Strongly_connected_components.Make (struct
  type t = Var.t

  module Set = Var.Set
  module Map = Var.Map
end)

let annot st xi =
  match (xi : Code.Print.xinstr) with
  | Instr (Let (x, _)) when Var.Set.mem x st -> "*"
  | _ -> " "

let f (p, info) =
  let vars = ref Var.Set.empty in
  let deps = ref Var.Map.empty in
  let tail_deps = ref Var.Map.empty in
  Code.fold_closures
    p
    (fun fun_name _ (pc, _) _ ->
      Code.traverse
        { fold = Code.fold_children }
        (fun pc () ->
          block_deps ~info ~vars ~tail_deps ~deps ~blocks:p.blocks ~fun_name pc)
        pc
        p.blocks
        ())
    ();
  let scc = SCC.component_graph !tail_deps in
  let in_loop =
    Array.fold_left
      ~f:(fun s (c, _) ->
        match c with
        | SCC.No_loop _ -> s
        | Has_loop l ->
            (*
            Format.eprintf "LOOP ";
            List.iter ~f:(fun x -> Format.eprintf " v%d" (Var.idx x)) l;
            Format.eprintf "@.";
*)
            List.fold_left ~f:(fun s x -> Var.Set.add x s) l ~init:s)
      ~init:Var.Set.empty
      scc
  in
  let g =
    { G'.domain = !vars
    ; fold_children =
        (fun f x r ->
          Var.Set.fold f (try Var.Map.find x !deps with Not_found -> Var.Set.empty) r)
    }
  in
  let rev_deps = G'.invert g in
  let res = Solver.f g (cps_needed ~info ~in_loop ~rev_deps) in

  Var.Map.fold (fun x v s -> if v then Var.Set.add x s else s) res Var.Set.empty
