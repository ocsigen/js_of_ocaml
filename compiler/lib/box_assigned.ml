(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
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

open! Stdlib
open Code

(*
The bytecode parser emits [Assign (x, y)] instructions to keep the
value of a variable [x] used in an exception handler up to date when
the corresponding stack slot is modified in the body of the [try].
ocamlc only uses mutable variables when they are not captured by a
closure. But the CPS transformation turns exception handlers and
continuations into closures, which can then read or assign a variable
bound in an enclosing function. Closures capture variables by value
in Wasm, and in JavaScript when they are created in a loop or lifted
by [Lambda_lifting]. So these variables are stored in a mutable block
instead:

- right after [x] is bound, we insert [x_box = {x}];
- uses of [x] read [x_box.(0)];
- [Assign (x, y)] becomes [x_box.(0) <- y].

Within a block, the value of [x] is reused until the next call, which
may run a closure that assigns [x].
*)

let debug = Debug.find "box-assigned"

let iter_block_assigned_vars f block =
  List.iter block.body ~f:(fun i ->
      match i with
      | Assign (x, _) -> f x
      | Let _ | Set_field _ | Offset_ref _ | Array_set _ | Event _ -> ())

(* The variables read or assigned in a block *)
let iter_block_used_vars f block =
  Freevars.iter_block_free_vars f block;
  iter_block_assigned_vars f block

let assigned_variables p =
  let assigned = ref Var.Set.empty in
  Addr.Map.iter
    (fun _ block ->
      iter_block_assigned_vars (fun x -> assigned := Var.Set.add x !assigned) block)
    p.blocks;
  !assigned

(* The assigned variables referenced from another function than the one
   binding them *)
let referenced_from_another_function p assigned =
  let binder = Var.Tbl.make () (-1) in
  let refs = ref [] in
  let _ : int =
    fold_closures
      p
      (fun _ params (pc, args) _ id ->
        let bind x = if Var.Set.mem x assigned then Var.Tbl.set binder x id in
        let reference x = if Var.Set.mem x assigned then refs := (x, id) :: !refs in
        List.iter ~f:bind params;
        List.iter ~f:reference args;
        traverse
          { fold = fold_children }
          (fun pc () ->
            let block = Addr.Map.find pc p.blocks in
            Freevars.iter_block_bound_vars bind block;
            iter_block_used_vars reference block)
          pc
          p.blocks
          ();
        id + 1)
      0
  in
  List.fold_left !refs ~init:Var.Set.empty ~f:(fun acc (x, id) ->
      let id' = Var.Tbl.get binder x in
      assert (id' <> -1);
      if id' = id then acc else Var.Set.add x acc)

let box p vars =
  if debug ()
  then
    Var.Set.iter
      (fun x -> Format.eprintf "Boxing assigned variable %a@." Var.print x)
      vars;
  let boxes = Var.Set.fold (fun x m -> Var.Map.add x (Var.fork x) m) vars Var.Map.empty in
  let is_boxed x = Var.Map.mem x boxes in
  let is_boxed_cont (_, args) = List.exists ~f:is_boxed args in
  let mentions_boxed_variable block =
    let check x = if is_boxed x then raise Exit in
    try
      Freevars.iter_block_bound_vars check block;
      iter_block_used_vars check block;
      List.iter block.body ~f:(fun i ->
          match i with
          | Let (_, Closure (params, (_, args), _)) ->
              List.iter ~f:check params;
              List.iter ~f:check args
          | Let _ | Assign _ | Set_field _ | Offset_ref _ | Array_set _ | Event _ -> ());
      false
    with Exit -> true
  in
  let free_pc = ref p.free_pc in
  let new_blocks = ref [] in
  let rec rewrite_block ~entry block =
    (* The variables holding the current value of the boxed variables *)
    let values = ref Var.Map.empty in
    let body = ref [] in
    let emit i = body := i :: !body in
    let create_box x =
      match Var.Map.find_opt x boxes with
      | Some x_box ->
          emit (Let (x_box, Block (0, [| x |], NotArray, Maybe_mutable)));
          values := Var.Map.add x x !values
      | None -> ()
    in
    let value x =
      match Var.Map.find_opt x boxes with
      | None -> x
      | Some x_box -> (
          match Var.Map.find_opt x !values with
          | Some y -> y
          | None ->
              let y = Var.fork x in
              emit (Let (y, Field (x_box, 0, Non_float)));
              values := Var.Map.add x y !values;
              y)
    in
    let value_cont (pc, args) = pc, List.map ~f:value args in
    (* The boxes of closures are created after the whole sequence of
       closures, which may be mutually recursive *)
    let pending = ref [] in
    let flush () =
      List.iter ~f:create_box (List.rev !pending);
      pending := []
    in
    List.iter ~f:create_box block.params;
    List.iter ~f:create_box entry;
    List.iter block.body ~f:(fun i ->
        match i with
        | Let (f, Closure (params, cont, info)) ->
            let cont =
              (* The arguments of [cont] are evaluated when the closure
                 is called. The entry block may be a loop header, so we
                 create the boxes of the parameters in a new block. *)
              if List.exists ~f:is_boxed params || is_boxed_cont cont
              then entry_block params cont
              else cont
            in
            emit (Let (f, Closure (params, cont, info)));
            pending := f :: !pending
        | Assign (x, y) when is_boxed x ->
            flush ();
            let y = value y in
            emit (Set_field (Var.Map.find x boxes, 0, Non_float, y));
            values := Var.Map.add x y !values
        | Let (x, e) ->
            flush ();
            emit (Let (x, Subst.Excluding_Binders.expr value e));
            (match e with
            | Apply _ -> values := Var.Map.empty
            | Prim (Extern (name, _), _) when not (Primitive.is_pure name) ->
                values := Var.Map.empty
            | Block _ | Field _ | Closure _ | Constant _ | Prim _ | Special _ -> ());
            create_box x
        | Assign _ | Set_field _ | Offset_ref _ | Array_set _ | Event _ ->
            flush ();
            emit (Subst.Excluding_Binders.instr value i));
    flush ();
    let branch =
      match block.branch with
      | Pushtrap (cont1, x, cont2) ->
          (* The arguments of [cont2] are evaluated when an exception is
             caught *)
          let cont2 =
            if is_boxed x || is_boxed_cont cont2 then entry_block [ x ] cont2 else cont2
          in
          Pushtrap (value_cont cont1, x, cont2)
      | Return _ | Raise _ | Stop | Branch _ | Cond _ | Switch _ | Poptrap _ ->
          Subst.Excluding_Binders.last value block.branch
    in
    { block with body = List.rev !body; branch }
  (* A new block creating the boxes of [vars] before jumping to [cont].
     It starts with the location of the target block, which
     [Generate_closure] uses for function entries. *)
  and entry_block vars ((target, _) as cont) =
    let pc = !free_pc in
    incr free_pc;
    let block =
      rewrite_block ~entry:vars { params = []; body = []; branch = Branch cont }
    in
    let block =
      match (Addr.Map.find target p.blocks).body with
      | (Event _ as i) :: _ -> { block with body = i :: block.body }
      | _ -> block
    in
    new_blocks := (pc, block) :: !new_blocks;
    pc, []
  in
  let blocks =
    Addr.Map.map
      (fun block ->
        if mentions_boxed_variable block then rewrite_block ~entry:[] block else block)
      p.blocks
  in
  let blocks =
    List.fold_left !new_blocks ~init:blocks ~f:(fun blocks (pc, block) ->
        Addr.Map.add pc block blocks)
  in
  let p = { p with blocks; free_pc = !free_pc } in
  if debug () then Code.Print.program Format.err_formatter (fun _ _ -> "") p;
  Code.invariant p;
  p

let f p =
  let assigned = assigned_variables p in
  if Var.Set.is_empty assigned
  then p
  else
    let vars = referenced_from_another_function p assigned in
    if Var.Set.is_empty vars then p else box p vars
