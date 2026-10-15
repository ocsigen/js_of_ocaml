(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2026 Hugo Heuzard
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

(* Move pure instructions whose only use is an argument of a single
   branch of a conditional (or switch) into that branch, splitting the
   corresponding edge:

     block:  ...; h = i + 1; if (i === n) goto exit else goto header(h)

   becomes

     block:  ...; if (i === n) goto exit else goto edge
     edge:   h = i + 1; goto header(h)

   The value is then only computed when needed, and the code generator
   can inline it at its use site. For [for] loops, this removes the
   temporary used for the incremented loop counter. *)

open! Stdlib
open Code

let debug = Debug.find "branch-sinking"

let movable e =
  match e with
  | Constant _ | Block _ -> true
  | Prim ((Vectlength _ | Not | IsInt _ | Eq _ | Neq _ | Lt | Le | Ult), _) -> true
  | Prim (Extern (name, _), _) -> (
      match Primitive.kind name with
      | `Pure -> true
      | `Mutable | `Mutator -> false)
  | Prim (Array_get _, _) | Field _ | Apply _ | Closure _ | Special _ -> false

let sink_block ~variable_uses ~fresh_pc ~add_block block =
  let conts =
    match block.branch with
    | Cond (_, c1, c2) -> Some [| c1; c2 |]
    | Switch (_, a) -> Some (Array.copy a)
    | Return _ | Raise _ | Stop | Branch _ | Pushtrap _ | Poptrap _ -> None
  in
  match conts with
  | None -> block
  | Some conts ->
      let n = Array.length conts in
      let used_once x =
        let i = Var.idx x in
        i < Array.length variable_uses && variable_uses.(i) = 1
      in
      (* Instructions moved to each edge, and the variables they use *)
      let sunk = Array.make n [] in
      let needed = Array.make n Var.Set.empty in
      let edge_of x =
        let rec find i =
          if i = n
          then None
          else if List.mem ~eq:Var.equal x (snd conts.(i)) || Var.Set.mem x needed.(i)
          then Some i
          else find (i + 1)
        in
        find 0
      in
      (* The location of each instruction *)
      let body =
        let _, l =
          List.fold_left block.body ~init:(None, []) ~f:(fun (loc, acc) i ->
              match i with
              | Event loc' -> Some loc', (i, None) :: acc
              | _ -> loc, (i, loc) :: acc)
        in
        l
      in
      let _, kept =
        List.fold_left body ~init:(Var.Set.empty, []) ~f:(fun (assigned, kept) (i, loc) ->
            match i with
            | Let (x, e)
              when movable e
                   && used_once x
                   &&
                   let ok = ref true in
                   Freevars.iter_instr_free_vars
                     (fun y -> if Var.Set.mem y assigned then ok := false)
                     i;
                   !ok -> (
                match edge_of x with
                | Some k ->
                    let rem =
                      match sunk.(k), loc with
                      | Event loc' :: rem, Some loc when Poly.equal loc loc' -> rem
                      | l, _ -> l
                    in
                    sunk.(k) <-
                      (match loc with
                      | Some loc -> Event loc :: i :: rem
                      | None -> i :: rem);
                    Freevars.iter_instr_free_vars
                      (fun y -> needed.(k) <- Var.Set.add y needed.(k))
                      i;
                    assigned, kept
                | None -> assigned, i :: kept)
            | Assign (y, _) -> Var.Set.add y assigned, i :: kept
            | _ -> assigned, i :: kept)
      in
      if Array.for_all sunk ~f:List.is_empty
      then block
      else
        let conts =
          Array.mapi conts ~f:(fun k cont ->
              match sunk.(k) with
              | [] -> cont
              | body ->
                  let pc = fresh_pc () in
                  add_block pc { params = []; body; branch = Branch cont };
                  pc, [])
        in
        let branch =
          match block.branch with
          | Cond (x, _, _) -> Cond (x, conts.(0), conts.(1))
          | Switch (x, _) -> Switch (x, conts)
          | Return _ | Raise _ | Stop | Branch _ | Pushtrap _ | Poptrap _ -> assert false
        in
        (* Moving instructions out can leave consecutive events: only the
           last one is relevant *)
        let rec remove_redundant_events l =
          match l with
          | Event _ :: (Event _ :: _ as rem) -> remove_redundant_events rem
          | i :: rem -> i :: remove_redundant_events rem
          | [] -> []
        in
        { block with body = remove_redundant_events kept; branch }

let f ~variable_uses p =
  let t = Timer.make () in
  let free_pc = ref p.free_pc in
  let new_blocks = ref [] in
  let fresh_pc () =
    let pc = !free_pc in
    incr free_pc;
    pc
  in
  let add_block pc b = new_blocks := (pc, b) :: !new_blocks in
  let blocks =
    Addr.Map.map (fun b -> sink_block ~variable_uses ~fresh_pc ~add_block b) p.blocks
  in
  let count = List.length !new_blocks in
  let blocks =
    List.fold_left !new_blocks ~init:blocks ~f:(fun blocks (pc, b) ->
        Addr.Map.add pc b blocks)
  in
  if debug ()
  then Format.eprintf "  branch sinking: %d edges split, %a@." count Timer.print t;
  let p = { p with blocks; free_pc = !free_pc } in
  Code.invariant p;
  p
