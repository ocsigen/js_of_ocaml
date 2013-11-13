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

let debug = Option.Debug.find "flow"
let times = Option.Debug.find "times"

open Code

(****)

let add_var = VarISet.add

type def = Phi of VarSet.t | Expr of Code.expr | Param of (Var.t * int) option


type 'a v =
  | Bottom
  | Val of 'a
  | Top

let string_of_approx f = function
  | Bottom -> "Bottom"
  | Top -> "Top"
  | Val i -> Printf.sprintf "Val %s" (f i)


type info = {
  info_defs:def array;
  info_params: VarSet.t array array;
  info_known_origins : Code.VarSet.t Code.VarTbl.t;
  info_maybe_unknown : bool Code.VarTbl.t;
  info_possibly_mutable : bool array
}


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
    Expr _ | Param _ ->
      assert false
  | Phi s  ->
      defs.(idx) <- Phi (VarSet.add y s)


let add_params_origin state f args =
  let fx = Var.idx f in
  let argsl = List.length args in
  let a =
    if Array.length state.(fx) < argsl
    then
      let a = Array.make argsl VarSet.empty in
      Array.blit state.(fx) 0 a 0 (Array.length state.(fx));
      state.(fx) <- a;
      a
    else state.(fx) in
  List.iteri (fun i ar -> a.(i) <- VarSet.add ar a.(i) ) args

let add_param_def vars defs x extra =
  add_var vars x;
  let idx = Var.idx x in
  match defs.(idx) with
    | x when is_undefined x -> defs.(idx) <- Param extra
    | Param x' when x' = extra -> ()
    | _ -> assert false

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

let expr_deps blocks vars deps defs params x e =
  match e with
    | Const _ | Constant _ | Prim _ ->
      ()
    | Apply (f,args,_) ->
      add_params_origin params f args
    | Closure (l, cont) ->
      List.iteri (fun i v -> add_param_def vars defs v (Some (x,i))) l;
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
  let params = Array.make nv [||] in
  AddrMap.iter
    (fun pc block ->
       List.iter
         (fun i ->
            match i with
              | Let (x, e) ->
                add_var vars x;
                add_expr_def defs x e;
                expr_deps blocks vars deps defs params x e
              | Set_field _ | Array_set _ | Offset_ref _ ->
                ())
         block.body;
       Util.opt_iter
         (fun (x, cont) ->
            add_param_def vars defs x None;
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
  (vars, deps, defs,params)

let var_set_lift f s =
  VarSet.fold (fun y s -> VarSet.union (f y) s) s VarSet.empty

let propagate1 deps defs st x =
  match defs.(Var.idx x) with
    Param _ ->
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
               | Phi _ | Param _ | Expr _ ->
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

let propagate2 ?(skip_param=false) defs known_origins possibly_mutable st x =
  match defs.(Var.idx x) with
    Param _ -> skip_param
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
               | Phi _ | Param _| Expr _ ->
                   true)
              (VarTbl.get known_origins y)

module Domain2 = struct
  type t = bool
  let equal (u : bool) v = u = v
  let bot = false
end

module Solver2 = G.Solver (Domain2)

let solver2 ?skip_param vars deps defs known_origins possibly_mutable =
  let g =
    { G.domain = vars;
      G.iter_children = fun f x -> VarSet.iter f deps.(Var.idx x) }
  in
  Solver2.f () g (propagate2 ?skip_param defs known_origins possibly_mutable)

let get_approx {info_defs; info_known_origins;info_maybe_unknown;info_possibly_mutable;info_params} f top bottom join x =
  let rec aux visited x =
    let visited = VarSet.add x visited in
    let s = VarTbl.get info_known_origins x in
    if VarTbl.get info_maybe_unknown x then top,visited else
      match VarSet.cardinal s with
        | 0 -> bottom,visited
        | 1 ->
          begin
            let x = VarSet.choose s in
            match info_defs.(Var.idx x) with
              | Param (Some (fct,i)) ->
                let idx_f = Var.idx fct in
                (* check if fct can escape *)
                if info_possibly_mutable.(idx_f)
                then top,visited
                else
                  let a = info_params.(idx_f) in
                  if Array.length a > i
                  then
                    let s = a.(i) in
                    let s = VarSet.elements s in
                    let rec loop visited res = function
                      | [] -> res,visited
                      | x::xs ->
                        if VarSet.mem x visited
                        then loop visited res xs
                        else
                          let visited = VarSet.add x visited in
                          let r,visited = aux visited x in
                          let res = join r res in
                          loop visited res xs
                    in loop visited bottom s
                  else top,visited
              | _ -> f x,visited
          end
        | _ -> VarSet.fold (fun x (u,visited) -> join (f x) u, visited) s (f (VarSet.choose s), visited)
  in
  fst(aux VarSet.empty x)


let the_def_of info x =
  match x with
    | Pc c -> Some (Constant c)
    | Pv x ->
      let v = get_approx info
        (fun x -> match info.info_defs.(Var.idx x) with
          | Expr e -> Val e
          | _ -> Top)
        Top Bottom (fun u v -> Top) x in
      match v with
        | Val v -> Some v
        | _ -> None

let the_int info x =
  match x with
    | Pv x -> begin
      let v = get_approx info
        (fun x -> match info.info_defs.(Var.idx x) with
          | Expr (Const i) -> Val i
          | Expr (Constant (Int i)) -> Val i
          | _ -> Top)
        Top Bottom
        (fun u v -> match u, v with
          | Val i, Val j when i = j -> u
          | Bottom, x | x, Bottom -> x
          | _ -> Top) x in
      match v with
        | Val v -> Some v
        | _ -> None
    end
    | Pc (Int i) -> Some i
    | _ -> None

(*XXX Maybe we could iterate? *)
let direct_approx info x =
  match info.info_defs.(Var.idx x) with
    Expr (Field (y, n)) ->
      get_approx info
        (fun z ->
           if info.info_possibly_mutable.(Var.idx z) then None else
           match info.info_defs.(Var.idx z) with
             Expr (Block (_, a)) when n < Array.length a ->
               Some a.(n)
           | _ ->
               None)
        None None
        (fun u v ->
           match u, v with
             Some n, Some m when Var.compare n m = 0 -> u
           | _ -> None)
        y
  | _ ->
      None

let build_subst info  vars =
  let nv = Var.count () in
  let subst = Array.make nv None in
  VarISet.iter
    (fun x ->
       let u = VarTbl.get info.info_maybe_unknown x in
       if not u then begin
         let s = VarTbl.get info.info_known_origins x in
         if VarSet.cardinal s = 1 then
           subst.(Var.idx x) <- Some (VarSet.choose s)
       end;
       if subst.(Var.idx x) = None then
         subst.(Var.idx x) <-
           direct_approx info x)
    vars;
  subst

(****)

let f ?skip_param ((pc, blocks, free_pc) as p) =
  let t = Util.Timer.make () in
  let t1 = Util.Timer.make () in
  let (vars, deps, defs,params) = program_deps p in
  if times () then Format.eprintf "    flow analysis 1: %a@." Util.Timer.print t1;
  let t2 = Util.Timer.make () in
  let known_origins = solver1 vars deps defs in
  if times () then Format.eprintf "    flow analysis 2: %a@." Util.Timer.print t2;
  let t3 = Util.Timer.make () in
  let possibly_mutable = program_escape defs known_origins p in
  if times () then Format.eprintf "    flow analysis 3: %a@." Util.Timer.print t3;
  let t4 = Util.Timer.make () in
  let maybe_unknown = solver2 ?skip_param vars deps defs known_origins possibly_mutable in
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
  let info = {
    info_defs = defs;
    info_params = params;
    info_known_origins = known_origins;
    info_maybe_unknown = maybe_unknown;
    info_possibly_mutable = possibly_mutable;
  } in
  let s = build_subst info vars in
  let p = Subst.program (Subst.from_array s) p in
  if times () then Format.eprintf "    flow analysis 5: %a@." Util.Timer.print t5;
  if times () then Format.eprintf "  flow analysis: %a@." Util.Timer.print t;
  p, info
