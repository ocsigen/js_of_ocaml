
let debug = false

open Code

(****)

let add_var vars x = vars := VarSet.add x !vars

type def = Phi of VarSet.t | Expr of Code.expr | Param

let undefined = Phi VarSet.empty

let is_undefined d = match d with Phi s -> VarSet.is_empty s | _ -> false

let add_expr_def defs x e =
  let idx = Var.idx x in
  assert (is_undefined defs.(idx));
  defs.(idx) <- Expr e

let add_assign_def vars defs x y =
  add_var vars x;
  let idx = Var.idx x in
  match defs.(idx) with
    Expr _ | Param ->
      assert false
  | Phi s  ->
      defs.(idx) <- Phi (VarSet.add y s)

let add_param_def vars defs x =
  add_var vars x;
  let idx = Var.idx x in
  assert (is_undefined defs.(idx) || defs.(idx) = Param);
  defs.(idx) <- Param

(* x depends on y *)
let add_dep deps x y =
  let idx = Var.idx y in
  deps.(idx) <- VarSet.add x deps.(idx)

let rec arg_deps vars deps defs params args =
  match params, args with
    x :: params, y :: args ->
      add_dep deps x y;
      add_assign_def vars defs x y;
      arg_deps vars deps defs params args
  | _ ->
      ()

let cont_deps blocks vars deps defs (pc, args) =
  let block = AddrMap.find pc blocks in
  arg_deps vars deps defs block.params args

let expr_deps blocks vars deps defs x e =
  match e with
    Const _ | Constant _ | Apply _ | Direct_apply _ | Prim _ ->
      ()
  | Closure (l, cont) ->
      List.iter (fun x -> add_param_def vars defs x) l;
      cont_deps blocks vars deps defs cont
  | Block (_, a) ->
      Array.iter (fun y -> add_dep deps x y) a
  | Field (y, _) ->
      add_dep deps x y
  | Variable y ->
      assert false

let program_deps (_, blocks, _) =
  let nv = Var.count () in
  let vars = ref VarSet.empty in
  let deps = Array.make nv VarSet.empty in
  let defs = Array.make nv undefined in
  AddrMap.iter
    (fun pc block ->
       List.iter
         (fun i ->
            match i with
              Let (x, e) ->
                add_var vars x;
                add_expr_def defs x e;
                expr_deps blocks vars deps defs x e
            | Set_field _ | Array_set _ | Offset_ref _ ->
                ())
         block.body;
       Util.opt_iter
         (fun (x, cont) ->
            add_param_def vars defs x;
            cont_deps blocks vars deps defs cont)
         block.handler;
       match block.branch with
         Return _ | Raise _ | Stop ->
           ()
       | Branch cont ->
           cont_deps blocks vars deps defs cont
       | Cond (_, _, cont1, cont2) ->
           cont_deps blocks vars deps defs cont1;
           cont_deps blocks vars deps defs cont2
       | Switch (_, a1, a2) ->
           Array.iter (fun cont -> cont_deps blocks vars deps defs cont) a1;
           Array.iter (fun cont -> cont_deps blocks vars deps defs cont) a2
       | Pushtrap (cont, _, _, _) ->
           cont_deps blocks vars deps defs cont
       | Poptrap cont ->
           cont_deps blocks vars deps defs cont)
    blocks;
  (vars, deps, defs)

let propagate1 deps defs st x =
  match defs.(Var.idx x) with
    Param ->
      VarSet.empty
  | Phi s ->
      VarSet.fold (fun y s -> VarSet.union (VarMap.find y st) s) s VarSet.empty
  | Expr e ->
      match e with
        Const _ | Constant _  | Apply _ | Direct_apply _ | Prim _ ->
          VarSet.singleton x
      | Closure _ | Block _ ->
          VarSet.singleton x
      | Field (y, n) ->
          let s = VarMap.find y st in
          VarSet.fold
            (fun z s ->
               match defs.(Var.idx z) with
                 Phi _ | Param ->
                   assert false
               | Expr (Block (_, a)) ->
(*Format.eprintf "%d/%d@." n (Array.length a);*)
                   if n >= Array.length a then s else begin
                     let t = a.(n) in
(*Format.eprintf "%a --> %a@." Var.print t Var.print x;*)
                     add_dep deps x t;
                     VarSet.union (VarMap.find t st) s
                   end
               | Expr _ ->
                   s)
            s VarSet.empty
      | Variable _ ->
          assert false

module G = Dgraph.Make (Var) (VarSet) (VarMap)

module Domain1 = struct
  type t = VarSet.t
  let equal = VarSet.equal
  let bot = VarSet.empty
end

module Solver1 = G.Solver (Domain1)

let solver1 vars deps defs =
  let g =
    { G.domain = vars;
      G.fold_children = fun f x a -> VarSet.fold f (deps.(Var.idx x)) a }
  in
  Solver1.f g (propagate1 deps defs)

(****)

type approx = Known | AnyBlock | Any

let a_max u v =
  match u, v with
    Any, _ | _, Any -> Any
  | Known, Known    -> Known
  | _               -> AnyBlock

let rec block_approx defs def_approx approx x =
  let s = VarMap.find x def_approx in
  VarSet.iter
    (fun y ->
       match defs.(Var.idx y) with
         Expr (Block (_, l)) ->
           let idx = Var.idx y in
           if not approx.(idx) then begin
             approx.(idx) <- true;
             Array.iter (fun z -> block_approx defs def_approx approx z) l
           end
       | _ ->
           ())
    s

let expr_approx defs def_approx approx x e =
  match e with
    Const _ | Constant _ | Closure _ | Block _ | Field _ ->
      ()
  | Apply (_, l) | Direct_apply (_, l) | Prim (_, l) ->
      List.iter (fun x -> block_approx defs def_approx approx x) l
  | Variable y ->
      assert false

let program_approx defs def_approx (_, blocks, _) =
  let nv = Var.count () in
  let approx = Array.make nv false in
  AddrMap.iter
    (fun pc block ->
       List.iter
         (fun i ->
            match i with
              Let (x, e) ->
                expr_approx defs def_approx approx x e
            | Set_field (x, _, _) | Array_set (x, _, _) | Offset_ref (x, _) ->
                approx.(Var.idx x) <- true)
         block.body;
       match block.branch with
         Return x | Raise x ->
           block_approx defs def_approx approx x
       | Stop | Branch _ | Cond _ | Switch _ | Pushtrap _ | Poptrap _ ->
           ())
    blocks;
  approx

let propagate2 defs def_approx approx st x =
  match defs.(Var.idx x) with
    Param ->
      Any
  | Phi s ->
      VarSet.fold (fun y u -> a_max (VarMap.find y st) u) s Known
  | Expr e ->
      match e with
        Const _ | Constant _  | Apply _ | Direct_apply _ | Prim _ ->
          Any
      | Closure _ ->
          Known
      | Block _ ->
          if approx.(Var.idx x) then AnyBlock else Known
      | Field (y, n) ->
          if VarMap.find y st <> Known then Any else begin
            let s = VarMap.find y def_approx in
            VarSet.fold
              (fun z u ->
                 match defs.(Var.idx x) with
                   Phi _ | Param ->
                     assert false
                 | Expr (Block (_, a)) ->
                     if Array.length a >= n then Any else begin
                       let t = a.(n) in
                       a_max (VarMap.find t st) u
                   end
               | Expr _ ->
                   u)
              s Known
          end
      | Variable _ ->
          assert false

module Domain2 = struct
  type t = approx
  let equal (u : approx) v = u = v
  let bot = Known
end

module Solver2 = G.Solver (Domain2)

let solver2 vars deps defs def_approx approx =
  let g =
    { G.domain = vars;
      G.fold_children = fun f x a -> VarSet.fold f (deps.(Var.idx x)) a }
  in
  Solver2.f g (propagate2 defs def_approx approx)

(****)

let the_def_of defs def_approx known_approx x =
  let s = VarMap.find x def_approx in
  if VarSet.cardinal s = 1 && VarMap.find x known_approx <> Any then
    match defs.(Var.idx (VarSet.choose s)) with
      Expr e -> Some e
    | _      -> None
  else
    None

let specialize_call defs def_approx known_approx i =
  match i with
    Let (x, Apply (f, l)) ->
(*
(*Format.eprintf "==> %a@." Var.print f;*)
      begin match the_def_of defs def_approx known_approx f with
        Some (Block (_, a)) when Array.length a > 0 ->
(*Format.eprintf "block@.";*)
          let f = a.(0) in
          begin match the_def_of defs def_approx known_approx f with
            Some (Closure (l', _)) when List.length l = List.length l' ->
(*Format.eprintf "OK@.";*)
              Let (x, Direct_apply (f, l))
          | _ ->
              i
          end
      | _ ->
          i
      end
      *)
      begin match the_def_of defs def_approx known_approx f with
        Some (Closure (l', _)) when List.length l = List.length l' ->
(*Format.eprintf "OK@.";*)
          Let (x, Direct_apply (f, l))
      | _ ->
          i
      end
  | _ ->
      i

let specialize_calls defs def_approx known_approx (pc, blocks, free_pc) =
  let blocks =
    AddrMap.map
      (fun block ->
         { block with Code.body =
             List.map
               (fun i -> specialize_call defs def_approx known_approx i)
               block.body })
      blocks
  in
  (pc, blocks, free_pc)

(****)

let build_subst def_approx known_approx =
  let nv = Var.count () in
  let subst = Array.make nv None in
  VarMap.fold
    (fun x u () ->
       if u <> Any then begin
         let s = VarMap.find x def_approx in
         if VarSet.cardinal s = 1 then
           subst.(Var.idx x) <- Some (VarSet.choose s)
       end)
    known_approx ();
  subst

(****)

let f ((pc, blocks, free_pc) as p) =
  let (vars, deps, defs) = program_deps p in
  let def_approx = solver1 !vars deps defs in
  let approx = program_approx defs def_approx p in
  let known_approx = solver2 !vars deps defs def_approx approx in

  if debug then begin
    VarMap.iter
      (fun x s ->
         if not (VarSet.is_empty s) (*&& VarSet.choose s <> x*) then begin
    Format.eprintf "%a: {%a} / %s@."
    Var.print x Code.print_var_list (VarSet.elements s)
    (match VarMap.find x known_approx with
       Known -> "known"
     | AnyBlock -> "anyblock"
     | Any -> "any")
           end)
        def_approx;
  end;

  let p = specialize_calls defs def_approx known_approx p in
  let s = build_subst def_approx known_approx in
  let p = Subst.program (Subst.from_array s) p in
  p
