(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

let debug = Util.debug "flow"
let disable_optcall = Util.disabled "optcall"
let times = Util.debug "times"

open Code

(****)

let add_var = VarISet.add

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
    Const _ | Constant _ | Apply _ | Prim _ ->
      ()
  | Closure (l, cont) ->
      List.iter (fun x -> add_param_def vars defs x) l;
      cont_deps blocks vars deps defs cont
  | Block (_, a) ->
      Array.iter (fun y -> add_dep deps x y) a
  | Field (y, _) ->
      add_dep deps x y

let program_deps (_, blocks, _) =
  let nv = Var.count () in
  let vars = VarISet.empty () in
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
       | Branch cont | Poptrap cont ->
           cont_deps blocks vars deps defs cont
       | Cond (_, _, cont1, cont2) ->
           cont_deps blocks vars deps defs cont1;
           cont_deps blocks vars deps defs cont2
       | Switch (_, a1, a2) ->
           Array.iter (fun cont -> cont_deps blocks vars deps defs cont) a1;
           Array.iter (fun cont -> cont_deps blocks vars deps defs cont) a2
       | Pushtrap (cont, _, _, _) ->
           cont_deps blocks vars deps defs cont)
    blocks;
  (vars, deps, defs)

let var_set_lift f s =
  VarSet.fold (fun y s -> VarSet.union (f y) s) s VarSet.empty

let propagate1 deps defs st x =
  match defs.(Var.idx x) with
    Param ->
      VarSet.singleton x
  | Phi s ->
      var_set_lift (fun y -> VarTbl.get st y) s
  | Expr e ->
      match e with
        Const _ | Constant _  | Apply _ | Prim _
      | Closure _ | Block _ ->
          VarSet.singleton x
      | Field (y, n) ->
          var_set_lift
            (fun z ->
               match defs.(Var.idx z) with
                 Expr (Block (_, a)) when n < Array.length a ->
                   let t = a.(n) in
                   add_dep deps x t;
                   VarTbl.get st t
               | Phi _ | Param | Expr _ ->
                   VarSet.empty)
            (VarTbl.get st y)

module G = Dgraph.Make_Imperative (Var) (VarISet) (VarTbl)

module Domain1 = struct
  type t = VarSet.t
  let equal = VarSet.equal
  let bot = VarSet.empty
end

module Solver1 = G.Solver (Domain1)

let solver1 vars deps defs =
  let g =
    { G.domain = vars;
      G.iter_children = fun f x -> VarSet.iter f deps.(Var.idx x) }
  in
  Solver1.f () g (propagate1 deps defs)

(****)

type mutability_state =
  { defs : def array;
    known_origins : Code.VarSet.t Code.VarTbl.t;
    may_escape : bool array;
    possibly_mutable : bool array }

let rec block_escape st x =
  VarSet.iter
    (fun y ->
       let idx = Var.idx y in
       if not st.may_escape.(idx) then begin
         st.may_escape.(idx) <- true;
         st.possibly_mutable.(idx) <- true;
         match st.defs.(Var.idx y) with
           Expr (Block (_, l)) -> Array.iter (fun z -> block_escape st z) l
         | _                   -> ()
       end)
    (VarTbl.get st.known_origins x)

let expr_escape st x e =
  match e with
    Const _ | Constant _ | Closure _ | Block _ | Field _ ->
      ()
  | Apply (_, l, _) ->
      List.iter (fun x -> block_escape st x) l
  | Prim (_, l) ->
      List.iter
        (fun x ->
           match x with
             Pv x -> block_escape st x
           | Pc _ -> ())
        l

let program_escape defs known_origins (_, blocks, _) =
  let nv = Var.count () in
  let may_escape = Array.make nv false in
  let possibly_mutable = Array.make nv false in
  let st =
    { defs = defs;
      known_origins = known_origins;
      may_escape = may_escape;
      possibly_mutable = possibly_mutable }
  in
  AddrMap.iter
    (fun pc block ->
       List.iter
         (fun i ->
            match i with
              Let (x, e) ->
                expr_escape st x e
            | Set_field (x, _, y) | Array_set (x, _, y) ->
                VarSet.iter (fun y -> possibly_mutable.(Var.idx y) <- true)
                  (VarTbl.get known_origins x);
                block_escape st y
            | Offset_ref (x, _) ->
                VarSet.iter (fun y -> possibly_mutable.(Var.idx y) <- true)
                  (VarTbl.get known_origins x))
         block.body;
       match block.branch with
         Return x | Raise x ->
           block_escape st x
       | Stop | Branch _ | Cond _ | Switch _ | Pushtrap _ | Poptrap _ ->
           ())
    blocks;
  possibly_mutable

(****)

type approx = Known | Maybe_unknown

let a_max u v =
  match u, v with
    Known, Known    -> Known
  | _               -> Maybe_unknown

let approx_lift f s = VarSet.fold (fun y u -> a_max (f y) u) s Known

let propagate2 defs known_origins possibly_mutable st x =
  match defs.(Var.idx x) with
    Param ->
      false
  | Phi s ->
      VarSet.exists (fun y -> VarTbl.get st y) s
  | Expr e ->
      match e with
        Const _ | Constant _ | Closure _ | Apply _ | Prim _ | Block _ ->
          false
      | Field (y, n) ->
          VarTbl.get st y
            ||
          VarSet.exists
            (fun z ->
               match defs.(Var.idx z) with
                 Expr (Block (_, a)) ->
                   n >= Array.length a
                    ||
                   possibly_mutable.(Var.idx z)
                    ||
                   VarTbl.get st a.(n)
               | Phi _ | Param | Expr _ ->
                   true)
              (VarTbl.get known_origins y)

module Domain2 = struct
  type t = bool
  let equal (u : bool) v = u = v
  let bot = false
end

module Solver2 = G.Solver (Domain2)

let solver2 vars deps defs known_origins possibly_mutable =
  let g =
    { G.domain = vars;
      G.iter_children = fun f x -> VarSet.iter f deps.(Var.idx x) }
  in
  Solver2.f () g (propagate2 defs known_origins possibly_mutable)

(****)

let get_approx (defs, known_origins, maybe_unknown) f top join x =
  let s = VarTbl.get known_origins x in
  if VarTbl.get maybe_unknown x then top else
  match VarSet.cardinal s with
    0 -> top
  | 1 -> f (VarSet.choose s)
  | _ -> VarSet.fold (fun x u -> join (f x) u) s (f (VarSet.choose s))

let the_def_of ((defs, _, _) as info) x =
  get_approx info
    (fun x -> match defs.(Var.idx x) with Expr e -> Some e | _ -> None)
    None (fun u v -> None) x

let the_int ((defs, _, _) as info) x =
  get_approx info
    (fun x -> match defs.(Var.idx x) with Expr (Const i) -> Some i | _ -> None)
    None
    (fun u v -> match u, v with Some i, Some j when i = j -> u | _ -> None)
    x

let function_cardinality ((defs, _, _) as info) x =
  get_approx info
    (fun x ->
       match defs.(Var.idx x) with
         Expr (Closure (l, _)) -> Some (List.length l)
       | _                     -> None)
    None
    (fun u v -> match u, v with Some n, Some m when n = m -> u | _ -> None)
    x

let specialize_instr info i =
  match i with
    Let (x, Apply (f, l, _)) when not (disable_optcall ()) ->
      Let (x, Apply (f, l, function_cardinality info f))

(*FIX this should be moved to a different file (javascript specific) *)
  | Let (x, Prim (Extern "caml_js_var", [Pv y])) ->
      begin match the_def_of info y with
        Some (Constant (String _ as c)) ->
          Let (x, Prim (Extern "caml_js_var", [Pc c]))
      | _ ->
          i
      end
  | Let (x, Prim (Extern "caml_js_const", [Pv y])) ->
      begin match the_def_of info y with
        Some (Constant (String _ as c)) ->
          Let (x, Prim (Extern "caml_js_const", [Pc c]))
      | _ ->
          i
      end
  | Let (x, Prim (Extern "caml_js_call", [Pv f; Pv o; Pv a])) ->
      begin match the_def_of info a with
        Some (Block (_, a)) ->
          let a = Array.map (fun x -> Pv x) a in
          Let (x, Prim (Extern "caml_js_opt_call",
                        Pv f :: Pv o :: Array.to_list a))
      | _ ->
          i
      end
  | Let (x, Prim (Extern "caml_js_fun_call", [Pv f; Pv a])) ->
      begin match the_def_of info a with
        Some (Block (_, a)) ->
          let a = Array.map (fun x -> Pv x) a in
          Let (x, Prim (Extern "caml_js_opt_fun_call",
                        Pv f :: Array.to_list a))
      | _ ->
          i
      end
  | Let (x, Prim (Extern "caml_js_meth_call", [Pv o; Pv m; Pv a])) ->
      begin match the_def_of info m with
        Some (Constant (String _ as m)) ->
          begin match the_def_of info a with
            Some (Block (_, a)) ->
              let a = Array.map (fun x -> Pv x) a in
              Let (x, Prim (Extern "caml_js_opt_meth_call",
                            Pv o :: Pc m :: Array.to_list a))
          | _ ->
              i
          end
      | _ ->
          i
      end
  | Let (x, Prim (Extern "caml_js_new", [Pv c; Pv a])) ->
      begin match the_def_of info a with
        Some (Block (_, a)) ->
          let a = Array.map (fun x -> Pv x) a in
          Let (x, Prim (Extern "caml_js_opt_new",
                        Pv c :: Array.to_list a))
      | _ ->
          i
      end
  | Let (x, Prim (Extern "caml_js_get", [Pv o; Pv f])) ->
      begin match the_def_of info f with
        Some (Constant (String _ as c)) ->
          Let (x, Prim (Extern "caml_js_get", [Pv o; Pc c]))
      | _ ->
          i
      end
  | Let (x, Prim (Extern "caml_js_set", [Pv o; Pv f; Pv v])) ->
      begin match the_def_of info f with
        Some (Constant (String _ as c)) ->
          Let (x, Prim (Extern "caml_js_set", [Pv o; Pc c; Pv v]))
      | _ ->
          i
      end
  | Let (x, Prim (Extern "%int_mul", [Pv y; Pv z])) ->
      begin match the_int info y, the_int info z with
        Some j, _ | _, Some j when abs j < 0x200000 ->
          Let (x, Prim (Extern "%direct_int_mul", [Pv y; Pv z]))
      | _ ->
          i
      end
  | Let (x, Prim (Extern "%int_div", [Pv y; Pv z])) ->
      begin match the_int info z with
        Some j when j <> 0 ->
          Let (x, Prim (Extern "%direct_int_div", [Pv y; Pv z]))
      | _ ->
          i
      end
  | Let (x, Prim (Extern "%int_mod", [Pv y; Pv z])) ->
      begin match the_int info z with
        Some j when j <> 0 ->
          Let (x, Prim (Extern "%direct_int_mod", [Pv y; Pv z]))
      | _ ->
          i
      end

  | _ ->
      i

let specialize_instrs info (pc, blocks, free_pc) =
  let blocks =
    AddrMap.map
      (fun block ->
         { block with Code.body =
             List.map (fun i -> specialize_instr info i) block.body })
      blocks
  in
  (pc, blocks, free_pc)

(****)

(*XXX Maybe we could iterate? *)
let direct_approx defs known_origins maybe_unknown possibly_mutable x =
  match defs.(Var.idx x) with
    Expr (Field (y, n)) ->
      get_approx (defs, known_origins, maybe_unknown)
        (fun z ->
           if possibly_mutable.(Var.idx z) then None else
           match defs.(Var.idx z) with
             Expr (Block (_, a)) when n < Array.length a ->
               Some a.(n)
           | _ ->
               None)
        None
        (fun u v ->
           match u, v with
             Some n, Some m when Var.compare n m = 0 -> u
           | _ -> None)
        y
  | _ ->
      None

let build_subst defs vars known_origins maybe_unknown possibly_mutable =
  let nv = Var.count () in
  let subst = Array.make nv None in
  VarISet.iter
    (fun x ->
       let u = VarTbl.get maybe_unknown x in
       if not u then begin
         let s = VarTbl.get known_origins x in
         if VarSet.cardinal s = 1 then
           subst.(Var.idx x) <- Some (VarSet.choose s)
       end;
       if subst.(Var.idx x) = None then
         subst.(Var.idx x) <-
           direct_approx defs known_origins maybe_unknown possibly_mutable x)
    vars;
  subst

(****)

let f ((pc, blocks, free_pc) as p) =
  let t = Util.Timer.make () in
  let t1 = Util.Timer.make () in
  let (vars, deps, defs) = program_deps p in
  if times () then Format.eprintf "    flow analysis 1: %a@." Util.Timer.print t1;
  let t2 = Util.Timer.make () in
  let known_origins = solver1 vars deps defs in
  if times () then Format.eprintf "    flow analysis 2: %a@." Util.Timer.print t2;
  let t3 = Util.Timer.make () in
  let possibly_mutable = program_escape defs known_origins p in
  if times () then Format.eprintf "    flow analysis 3: %a@." Util.Timer.print t3;
  let t4 = Util.Timer.make () in
  let maybe_unknown = solver2 vars deps defs known_origins possibly_mutable in
  if times () then Format.eprintf "    flow analysis 4: %a@." Util.Timer.print t4;

  if debug () then begin
    VarISet.iter
      (fun x ->
         let s = VarTbl.get known_origins x in
         if not (VarSet.is_empty s) (*&& VarSet.choose s <> x*) then begin
           Format.eprintf "%a: {%a} / %s@."
             Var.print x Code.print_var_list (VarSet.elements s)
             (if VarTbl.get maybe_unknown x then "any" else "known")
         end)
      vars
  end;

  let t5 = Util.Timer.make () in
  let p = specialize_instrs (defs, known_origins, maybe_unknown) p in
  let s = build_subst defs vars known_origins maybe_unknown possibly_mutable in
  let p = Subst.program (Subst.from_array s) p in
  if times () then Format.eprintf "    flow analysis 5: %a@." Util.Timer.print t5;
  if times () then Format.eprintf "  flow analysis: %a@." Util.Timer.print t;
  p
