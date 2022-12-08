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

open Code

let add_var = Var.ISet.add

(* x depends on y *)
let add_dep deps x y =
  let idx = Var.idx y in
  deps.(idx) <- Var.Set.add x deps.(idx)

let block_deps info vars deps blocks fun_name pc =
  let block = Code.Addr.Map.find pc blocks in
  Stdlib.List.iter block.Code.body ~f:(fun i ->
      match i with
      | Code.Let (x, Apply { f; _ }) ->
          add_var vars x;
          (match fun_name with
          | None -> ()
          | Some fun_name ->
              add_var vars fun_name;
              add_dep deps fun_name x);
          Var.Set.iter
            (fun g ->
              add_var vars g;
              add_dep deps x g;
              add_dep deps g x)
            (Var.Tbl.get info.Flow.info_known_origins f)
      | Code.Let (x, Prim (Extern ("%perform" | "%resume"), _)) -> add_var vars x
      | _ -> ())

module G = Dgraph.Make_Imperative (Var) (Var.ISet) (Var.Tbl)

module Domain = struct
  type t = bool

  let equal (x : bool) y = x = y

  let bot = false
end

module Solver = G.Solver (Domain)

let fold_children g f x acc =
  let acc = ref acc in
  g.G.iter_children (fun y -> acc := f y !acc) x;
  !acc

let cps_needed info rev_deps st x =
  let res =
    let idx = Var.idx x in
    fold_children
      rev_deps
      (fun y acc -> acc || Var.Tbl.get st y)
      x
      (match info.Flow.info_defs.(idx) with
      | Flow.Expr (Apply { f; _ }) ->
          Var.Tbl.get info.Flow.info_maybe_unknown f
          || Var.Set.exists
               (fun g ->
                 match info.Flow.info_defs.(Var.idx g) with
                 | Expr _ -> false
                 | _ -> true)
               (Var.Tbl.get info.Flow.info_known_origins f)
      | Flow.Expr (Closure _) | Flow.Expr (Prim (Extern "%closure", _)) ->
          info.Flow.info_possibly_mutable.(idx)
      | Flow.Expr (Prim (Extern ("%perform" | "%resume"), _)) -> true
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

let annot st xi =
  match (xi : Code.Print.xinstr) with
  | Instr (Let (x, _)) when Var.Tbl.get st x -> "*"
  | _ -> " "

let f (p, info) =
  let nv = Var.count () in
  let vars = Var.ISet.empty () in
  let deps = Array.make nv Var.Set.empty in
  Code.fold_closures
    p
    (fun name _ (pc, _) _ ->
      Code.traverse
        { fold = Code.fold_children }
        (fun pc () -> block_deps info vars deps p.blocks name pc)
        pc
        p.blocks
        ())
    ();
  let g =
    { G.domain = vars; G.iter_children = (fun f x -> Var.Set.iter f deps.(Var.idx x)) }
  in
  let rev_deps = G.invert () g in
  Solver.f () g (cps_needed info rev_deps)
