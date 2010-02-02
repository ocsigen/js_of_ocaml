(*
FIX: should take into account block mutation

FIX: use constraint-based algorithm for better performances
     (and dealing with cycles)?
     See Aiken
==> post-order traversal rather than propagation

Backward propagation for mutable tables ==> unification?
*)

let debug = false

open Util
open Code

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

let add_cont_dep blocks deps (pc, arg) =
  match arg with
    None ->
      ()
  | Some x ->
      let e = Variable x in
      match IntMap.find pc blocks with
        (Some y, _, _) -> add_dep deps x (Let (y, e))
      | _              -> ()  (* We can have a value in the accumulator
                                 which is not used afterwards... *)

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
  | Assign (x, _) | Set_field (x, _, _)
  | Array_set (x, _, _) | Offset_ref (x, _) ->
      update_var st x unknown

(****)

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
        Some f, Some n when List.length l = n ->
          Direct_apply (f, List.map (fun x -> subst_var s x) l)
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
  | Assign (x, y) ->
      Assign (subst_var s x, subst_var s y)
  | Set_field (x, n, y) ->
      Set_field (subst_var s x, n, subst_var s y)
  | Offset_ref (x, n) ->
      Offset_ref (subst_var s x, n)
  | Array_set (x, y, z) ->
      Array_set (subst_var s x, subst_var s y, subst_var s z)

let subst_cont s (pc, arg) = (pc, opt_map (fun x -> subst_var s x) arg)

let subst_last s l =
  match l with
    Stop ->
      l
  | Branch cont ->
      Branch (subst_cont s cont)
  | Pushtrap (cont1, pc, cont2) ->
      Pushtrap (subst_cont s cont1, pc, subst_cont s cont2)
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
    IntMap.map
      (fun (param, instr, last) ->
         (param, List.map (fun i -> subst_instr s a i) instr,
          subst_last s last))
      blocks
  in
  (pc, blocks, free_pc)

(****)

let f (pc, blocks, free_pc) =
  let nv = Var.count () in
  let deps = Array.make nv [] in
  let approx = Array.make nv void in
  IntMap.iter
    (fun pc (_, instr, last) ->
       List.iter
         (fun i ->
            match i with
              Let (_, e) ->
                add_expr_deps deps e i
            | Assign _ | Set_field _ | Array_set _ | Offset_ref _ ->
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
  let st = { approx = approx; deps = deps } in
  IntMap.iter
    (fun _ (param, instr, last) ->
       opt_iter (fun x -> update_var st x unknown) param;
       List.iter (fun i -> propagate st i) instr;
       match last with
         Pushtrap (_, pc, _) ->
           begin match IntMap.find pc blocks with
             (Some x, _, _) -> update_var st x unknown
           | _              -> ()
           end
       | _ ->
           ())
    blocks;

  let s = build_subst st in
  let (pc, blocks, free_pc) = subst s st.approx (pc, blocks, free_pc) in

  if debug then
    print_program (fun pc xi -> annot st pc xi) (pc, blocks, free_pc);

  ((pc, blocks, free_pc), st.approx)
