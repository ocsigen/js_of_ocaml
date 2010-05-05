
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
          VarSet.empty
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
(*
      begin match the_def_of defs def_approx known_approx f with
        Some (Closure (l', _)) when List.length l = List.length l' ->
Format.eprintf "OK@.";
          Let (x, Direct_apply (f, l))
      | _ ->
          i
      end
*)
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
(*
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
*)

  let p = specialize_calls defs def_approx known_approx p in
  let s = build_subst def_approx known_approx in
  let p = Subst.program (Subst.from_array s) p in
  p

(***********************************************************************)

(*
type 'a flat = Void | Known of 'a | Unknown

type v =
  | Blk of t list
  | Cst of int

and t = Var.t flat * v flat

let join_pair join1 join2 (v1, w1) (v2, w2) = (join1 v1 v2, join2 w1 w2)

let join_flat join v1 v2 =
  match v1, v2 with
    Void, _ -> v2
  | _, Void -> v1
  | Known v1, Known v2 -> join v1 v2
  | _ -> Unknown

let join_var v1 v2 : Var.t flat =
  join_flat
    (fun x1 x2 -> if Var.compare x1 x2 = 0 then Known x1 else Unknown) v1 v2

let rec join_val v1 v2 =
  join_flat
    (fun v1 v2 ->
       match v1, v2 with
         Blk l1, Blk l2 when List.length l1 = List.length l2 ->
           Known (Blk (List.map2 join l1 l2))
       | Cst n1, Cst n2 when n1 = n2 ->
           Known v1
       | _ ->
           Unknown)
    v1 v2

and join v1 v2 =
  join_pair join_var join_val v1 v2

let unknown = (Unknown, Unknown)
let known v = (Unknown, Known v)
let void = (Void, Void)

let get_field (_, v) n =
  match v with
    Known (Blk l) when n < List.length l ->
      List.nth l n
  | Void ->
      void
  | _ ->
      unknown

let get_const (_, v) =
  match v with
    Known (Cst i) -> Some i
  | _             -> None

let get_label (v, _) =
  match v with
    Known x -> Some x
  | _       -> None

type st =
  { approx : t array;
    deps : instr list array }

(****)

let rec approx_val_to_string a =
  match a with
    Void -> ""
  | Known x ->
      let b = Buffer.create 5 in
      Format.bprintf b "(%a)" Var.print x;
      Buffer.contents b
  | Unknown -> ""

let rec approx_var_to_string a =
  match a with
    Void -> "/"
  | Known (Blk l) -> "b[" ^ String.concat "" (List.map approx_to_string l) ^ "]"
  | Known (Cst n) -> Format.sprintf "c[%d]" n
  | Unknown -> "?"

and approx_to_string (a, b) = approx_val_to_string a ^ approx_var_to_string b

let annot st pc xi =
  match xi with
    Instr (Let (x, _)) -> approx_to_string st.approx.(Var.idx x)
  | _                  -> ""

(****)

let add_dep deps x i =
  let idx = Var.idx x in
  deps.(idx) <- i :: deps.(idx)

let add_expr_deps deps e i =
  match e with
    Const _ | Closure _ | Constant _ ->
      ()
  | Apply (f, l) | Direct_apply (f, l) ->
      add_dep deps f i; List.iter (fun x -> add_dep deps x i) l
  | Block (_, a) ->
      Array.iter (fun x -> add_dep deps x i) a
  | Field (x, _) ->
      add_dep deps x i
  | Prim (_, l) ->
      List.iter (fun x -> add_dep deps x i) l
  | Variable x ->
      add_dep deps x i

let rec add_arg_dep deps params args =
  match params, args with
    x :: params, y :: args ->
      add_dep deps x (Let (x, Variable y));
      add_arg_dep deps params args
  | _ ->
      ()

let add_cont_dep blocks deps (pc, args) =
  let block = AddrMap.find pc blocks in
  add_arg_dep deps block.params args

let name v x =
  match v with
    (Unknown, v') -> (Known x, v')
  | _             -> v

let rec update_var st x v =
  let v0 = st.approx.(Var.idx x) in
  let v' = join v v0 in
  if v' <> v0 then begin
    st.approx.(Var.idx x) <- v';
    List.iter (fun i -> propagate st i) st.deps.(Var.idx x)
  end

and propagate st i =
  match i with
    Let (x, e) ->
      begin match e with
        Const n ->
          update_var st x (known (Cst n))
      | Constant _ ->
          update_var st x unknown
      | Closure (l, _) ->
          update_var st x unknown;
          List.iter (fun x -> update_var st x unknown) l
      | Apply (f, l) | Direct_apply (f, l) ->
          update_var st x unknown
      | Block (t, l) ->
          update_var st x
            (known
               (Blk (Array.to_list
                       (Array.map (fun x -> name st.approx.(Var.idx x) x)
                          l))))
      | Field (y, n) ->
          update_var st x (get_field st.approx.(Var.idx y) n)
      | Prim _ ->
          update_var st x unknown
      | Variable x ->
          update_var st x (name st.approx.(Var.idx x) x)
      end
  | Set_field (x, _, _)
  | Array_set (x, _, _) | Offset_ref (x, _) ->
      update_var st x unknown

(****)

let specialize_call a i =
  match i with
    Let (x, Apply (f, l)) ->
      let v = a.(Var.idx f) in
      let n = get_const (get_field v 1) in
      let lab = get_label (get_field v 0) in
      begin match lab, n with
        Some f, Some n when List.length l = n ->
          Let (x, Direct_apply (f, l))
      | _ ->
          i
      end
  | _ ->
      i

let specialize_calls a (pc, blocks, free_pc) =
  let blocks =
    AddrMap.map
      (fun (param, instr, last) ->
         (param, List.map (fun i -> specialize_call a i) instr, last))
      blocks
  in
  (pc, blocks, free_pc)

(****)

(* FIX: use Subst module + specialize_calls above *)

let build_subst st =
  Array.map (fun (v, _) -> match v with Known x -> Some x | _ -> None)
    st.approx

let subst_var s x = match s.(Var.idx x) with Some y -> y | None -> x

let subst_expr s a e =
  match e with
    Const _ | Constant _ ->
      e
  | Apply (f, l) ->
      let v = a.(Var.idx f) in
      let n = get_const (get_field v 1) in
      let lab = get_label (get_field v 0) in
      begin match lab, n with
        Some f', Some n when List.length l = n ->
(*Format.eprintf "!!! %a@." Var.print f;*)
          Direct_apply (f', List.map (fun x -> subst_var s x) l)
      | _ ->
(*
opt_iter (fun n -> Format.eprintf "===>%a : %d / %d (%b)@." Var.print (subst_var s f) n (List.length l) (n = List.length l)) n;
*)
          Apply (subst_var s f, List.map (fun x -> subst_var s x) l)
      end
  | Direct_apply (f, l) ->
      Direct_apply (subst_var s f, List.map (fun x -> subst_var s x) l)
  | Block (n, a) ->
      Block (n, Array.map (fun x -> subst_var s x) a)
  | Field (x, n) ->
      Field (subst_var s x, n)
  | Closure (l, pc) ->
      Closure (l, pc)
  | Prim (p, l) ->
      Prim (p, List.map (fun x -> subst_var s x) l)
  | Variable x ->
      Variable (subst_var s x)

let subst_instr s a i =
  match i with
    Let (x, e) ->
      Let (x, subst_expr s a e)
  | Set_field (x, n, y) ->
      Set_field (subst_var s x, n, subst_var s y)
  | Offset_ref (x, n) ->
      Offset_ref (subst_var s x, n)
  | Array_set (x, y, z) ->
      Array_set (subst_var s x, subst_var s y, subst_var s z)

let subst_cont s (pc, arg) = (pc, List.map (fun x -> subst_var s x) arg)

let subst_last s l =
  match l with
    Stop ->
      l
  | Branch cont ->
      Branch (subst_cont s cont)
  | Pushtrap (cont1, x, cont2, pc) ->
      Pushtrap (subst_cont s cont1, x, subst_cont s cont2, pc)
  | Return x ->
      Return (subst_var s x)
  | Raise x ->
      Raise (subst_var s x)
  | Cond (c, x, cont1, cont2) ->
      Cond (c, subst_var s x, subst_cont s cont1, subst_cont s cont2)
  | Switch (x, a1, a2) ->
      Switch (subst_var s x,
              Array.map (fun cont -> subst_cont s cont) a1,
              Array.map (fun cont -> subst_cont s cont) a2)
  | Poptrap cont ->
      Poptrap (subst_cont s cont)

let subst s a (pc, blocks, free_pc) =
  let blocks =
    AddrMap.map
      (fun block ->
         { params = block.params;
           handler =
             Util.opt_map
               (fun (x, cont) -> (x, subst_cont s cont)) block.handler;
           body = List.map (fun i -> subst_instr s a i) block.body;
           branch = subst_last s block.branch })
      blocks
  in
  (pc, blocks, free_pc)

(****)

let f p =
  let (pc, blocks, free_pc) = f p in
  let nv = Var.count () in
  let deps = Array.make nv [] in
  let approx = Array.make nv void in
  AddrMap.iter
    (fun pc block ->
       List.iter
         (fun i ->
            match i with
              Let (_, e) ->
                add_expr_deps deps e i
            | Set_field _ | Array_set _ | Offset_ref _ ->
                ())
         block.body;
       match block.branch with
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
       | Pushtrap (cont, _, _, _) ->
           add_cont_dep blocks deps cont
       | Poptrap cont ->
           add_cont_dep blocks deps cont)
    blocks;
  let st = { approx = approx; deps = deps } in
  AddrMap.iter
    (fun _ block ->
       List.iter (fun x -> update_var st x unknown) block.params;
       List.iter (fun i -> propagate st i) block.body;
       match block.branch with
         Pushtrap (_, _, (pc, _), _) ->
           let block = AddrMap.find pc blocks in
           List.iter (fun x -> update_var st x unknown) block.params
       | _ ->
           ())
    blocks;

  let s = build_subst st in
  let (pc, blocks, free_pc) = subst s st.approx (pc, blocks, free_pc) in

  if debug then
    print_program (fun pc xi -> annot st pc xi) (pc, blocks, free_pc);

  ((pc, blocks, free_pc), st.approx)
    *)
