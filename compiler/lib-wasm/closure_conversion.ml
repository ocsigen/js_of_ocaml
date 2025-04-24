(* Wasm_of_ocaml compiler
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

type closure =
  { functions : (Var.t * int) list
  ; free_variables : Var.t list
  }

module SCC = Strongly_connected_components.Make (Var)

let iter_closures ~f instrs =
  let rec iter_closures_rec f instr_acc clos_acc instrs =
    let push_closures clos_acc instr_acc =
      if Var.Map.is_empty clos_acc
      then instr_acc
      else
        let l = f clos_acc in
        List.rev_map
          ~f:(fun g ->
            let params, cont, cloc = Var.Map.find g clos_acc in
            Let (g, Closure (params, cont, cloc)))
          l
        @ instr_acc
    in
    match instrs with
    | [] -> List.rev (push_closures clos_acc instr_acc)
    | Let (g, Closure (params, cont, cloc)) :: rem ->
        iter_closures_rec f instr_acc (Var.Map.add g (params, cont, cloc) clos_acc) rem
    | i :: rem ->
        iter_closures_rec f (i :: push_closures clos_acc instr_acc) Var.Map.empty rem
  in
  iter_closures_rec f [] Var.Map.empty instrs

let collect_free_vars program var_depth depth pc closures =
  let vars = ref Var.Set.empty in
  let add_if_free_variable x =
    let idx = Var.idx x in
    let d = var_depth.(idx) in
    assert (d >= 0);
    if d < depth then vars := Var.Set.add x !vars
  in
  Code.preorder_traverse
    { fold = Code.fold_children }
    (fun pc () ->
      let block = Code.Addr.Map.find pc program.blocks in
      Freevars.iter_block_free_vars add_if_free_variable block;
      List.iter block.body ~f:(fun i ->
          match i with
          | Let (f, Closure _) -> (
              match Var.Map.find_opt f closures with
              | Some { functions = (g, _) :: _; free_variables; _ } when Var.equal f g ->
                  List.iter ~f:add_if_free_variable free_variables
              | Some _ | None -> ())
          | _ -> ()))
    pc
    program.blocks
    ();
  !vars

let mark_bound_variables var_depth block depth =
  Freevars.iter_block_bound_vars (fun x -> var_depth.(Var.idx x) <- depth) block;
  List.iter block.body ~f:(fun i ->
      match i with
      | Let (_, Closure (params, _, _)) ->
          List.iter params ~f:(fun x -> var_depth.(Var.idx x) <- depth + 1)
      | _ -> ())

let rec traverse var_depth closures program pc depth =
  Code.preorder_traverse
    { fold = Code.fold_children }
    (fun pc (program : Code.program) ->
      let block = Code.Addr.Map.find pc program.blocks in
      mark_bound_variables var_depth block depth;
      let program =
        List.fold_left
          ~f:(fun program i ->
            match i with
            | Let (_, Closure (_, (pc', _), _)) ->
                traverse var_depth closures program pc' (depth + 1)
            | _ -> program)
          ~init:program
          block.body
      in
      let body =
        iter_closures block.body ~f:(fun l ->
            let free_vars =
              Var.Map.fold
                (fun f (_, (pc', _), _) free_vars ->
                  Var.Map.add
                    f
                    (collect_free_vars program var_depth (depth + 1) pc' !closures)
                    free_vars)
                l
                Var.Map.empty
            in
            let domain = Var.Map.fold (fun f _ s -> Var.Set.add f s) l Var.Set.empty in
            let graph = Var.Map.map (fun s -> Var.Set.inter s domain) free_vars in
            let components = SCC.connected_components_sorted_from_roots_to_leaf graph in
            let l =
              Array.map
                ~f:(fun component ->
                  let fun_lst =
                    match component with
                    | SCC.No_loop x -> [ x ]
                    | SCC.Has_loop l -> l
                  in
                  let free_variables =
                    Var.Set.elements
                      (List.fold_left
                         ~f:(fun fv x -> Var.Set.remove x fv)
                         ~init:
                           (List.fold_left
                              ~f:(fun fv x -> Var.Set.union fv (Var.Map.find x free_vars))
                              ~init:Var.Set.empty
                              fun_lst)
                         fun_lst)
                  in
                  let functions =
                    let arities =
                      Var.Map.fold
                        (fun f (params, _, _) m -> Var.Map.add f (List.length params) m)
                        l
                        Var.Map.empty
                    in
                    List.map ~f:(fun f -> f, Var.Map.find f arities) fun_lst
                  in
                  List.iter
                    ~f:(fun (f, _) ->
                      closures := Var.Map.add f { functions; free_variables } !closures)
                    functions;
                  fun_lst)
                components
            in
            List.concat (List.rev (Array.to_list l)))
      in
      { program with blocks = Code.Addr.Map.add pc { block with body } program.blocks })
    pc
    program.blocks
    program

let f p =
  let t = Timer.make () in
  let nv = Var.count () in
  let var_depth = Array.make nv (-1) in
  let closures = ref Var.Map.empty in
  let p = traverse var_depth closures p p.start 0 in
  if Debug.find "times" () then Format.eprintf "  closure conversion: %a@." Timer.print t;
  p, !closures
