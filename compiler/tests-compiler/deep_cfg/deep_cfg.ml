(* Js_of_ocaml tests
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

(* Stack-safety regressions for [Code.traverse] and [Code.preorder_traverse],
   and for the passes that walk the control-flow graph without using them:
   [Deadcode.merge_blocks], [Pure_fun.f] and the two DFS passes of
   [Strongly_connected_components].
   A function can be a chain of hundreds of thousands of blocks (e.g. the
   initialization code of a whole program, with a block per call), so these
   passes must not use a stack frame per block.

   The dune file runs this test with a 256 KiB stack (OCAMLRUNPARAM=l=32k),
   so a pass that uses even a small stack frame per block overflows on a
   chain of [deep] blocks. *)

module Jsoo = Js_of_ocaml_compiler
open Jsoo.Code

let deep = 100_000

let block params body branch = { params; body; branch }

let program start blocks =
  let blocks, free_pc =
    List.fold_left
      (fun (m, free_pc) (pc, b) -> Addr.Map.add pc b m, max free_pc (pc + 1))
      (Addr.Map.empty, 0)
      blocks
  in
  { start; blocks; free_pc }

(* A chain of [n] trivial blocks [first .. first + n - 1], each branching to
   its successor, the last one to [last]. *)
let chain ~first ~n ~last =
  List.init n (fun i ->
      let pc = first + i in
      let next = if i = n - 1 then last else pc + 1 in
      pc, block [] [] (Branch (next, [])))

let test name f =
  Printf.printf "%s:\n%!" name;
  f ();
  Printf.printf "\n%!"

let () =
  (* A join edge to the last block every few blocks, so that the traversals
     also reach blocks that are already visited. *)
  test "traverse and preorder_traverse on a deep chain of blocks" (fun () ->
      let x = Var.fresh () in
      let p =
        program
          0
          ((deep, block [] [] (Return x))
          :: List.init deep (fun pc ->
              let next = pc + 1, [] in
              ( pc
              , block
                  []
                  []
                  (if pc mod 3 = 0 then Cond (x, next, (deep, [])) else Branch next) )))
      in
      let count traverse =
        traverse { fold = fold_children } (fun _ n -> n + 1) p.start p.blocks 0
      in
      Printf.printf "postorder visited %d blocks\n" (count traverse);
      Printf.printf "preorder visited %d blocks\n" (count preorder_traverse))

let () =
  test "merge_blocks merges a chain and substitutes block parameters" (fun () ->
      let a = Var.fresh () in
      let p1 = Var.fresh () in
      let p2 = Var.fresh () in
      let p =
        program
          0
          [ ( 0
            , block
                []
                [ Let (a, Block (0, [||], NotArray, Immutable, Block_desc.none)) ]
                (Branch (1, [ a ])) )
          ; 1, block [ p1 ] [] (Branch (2, [ p1 ]))
          ; 2, block [ p2 ] [] (Return p2)
          ]
      in
      let p = Jsoo.Deadcode.merge_blocks p in
      Printf.printf "blocks after merge: %d\n" (Addr.Map.cardinal p.blocks);
      match (Addr.Map.find p.start p.blocks).branch with
      | Return x ->
          Printf.printf
            "returns the variable bound in the first block: %b\n"
            (Var.equal x a)
      | _ -> Printf.printf "unexpected branch\n")

let () =
  test "merge_blocks on a deep chain of blocks" (fun () ->
      let x = Var.fresh () in
      let p =
        program 0 ((deep, block [] [] (Return x)) :: chain ~first:0 ~n:deep ~last:deep)
      in
      let p = Jsoo.Deadcode.merge_blocks p in
      Printf.printf "blocks after merge: %d\n" (Addr.Map.cardinal p.blocks))

let () =
  (* Both arms of each [Cond] go to the same block, so every block has two
     predecessors and nothing is merged: the traversal itself must be
     stack-safe. *)
  test "merge_blocks on a deep chain of unmergeable blocks" (fun () ->
      let x = Var.fresh () in
      let p =
        program
          0
          ((deep, block [] [] (Return x))
          :: List.init deep (fun pc ->
              let next = pc + 1, [] in
              pc, block [] [] (Cond (x, next, next))))
      in
      let p = Jsoo.Deadcode.merge_blocks p in
      Printf.printf "blocks after merge: %d\n" (Addr.Map.cardinal p.blocks))

let () =
  test "pure function analysis on a deep chain of blocks" (fun () ->
      let f = Var.fresh () in
      let x = Var.fresh () in
      let p =
        program
          0
          ((0, block [] [ Let (f, Closure ([], (1, []), (None, None))) ] (Return f))
          :: (deep + 1, block [] [] (Return x))
          :: chain ~first:1 ~n:deep ~last:(deep + 1))
      in
      let pure_funs = Jsoo.Pure_fun.f p in
      Printf.printf "closure is pure: %b\n" (Jsoo.Pure_fun.pure pure_funs f))

let () =
  test "pure function analysis: a closure that loops is not pure" (fun () ->
      let f = Var.fresh () in
      let p =
        program
          0
          [ 0, block [] [ Let (f, Closure ([], (1, []), (None, None))) ] (Return f)
          ; 1, block [] [] (Branch (1, []))
          ]
      in
      let pure_funs = Jsoo.Pure_fun.f p in
      Printf.printf "closure is pure: %b\n" (Jsoo.Pure_fun.pure pure_funs f))

module Scc = Jsoo.Strongly_connected_components.Make (Addr)

let print_components components =
  Array.iter
    (fun c ->
      match c with
      | Scc.No_loop id -> Printf.printf "no_loop %d\n" id
      | Scc.Has_loop l ->
          Printf.printf "has_loop [%s]\n" (String.concat " " (List.map string_of_int l)))
    components

let () =
  test "strongly connected components" (fun () ->
      let graph =
        Addr.Map.empty
        |> Addr.Map.add 0 (Addr.Set.singleton 1)
        |> Addr.Map.add 1 (Addr.Set.of_list [ 0; 2 ])
        |> Addr.Map.add 2 Addr.Set.empty
      in
      print_components (Scc.connected_components_sorted_from_roots_to_leaf graph))

let () =
  test "strongly connected components of a deep chain" (fun () ->
      let graph =
        let g = ref (Addr.Map.add deep Addr.Set.empty Addr.Map.empty) in
        for i = 0 to deep - 1 do
          g := Addr.Map.add i (Addr.Set.singleton (i + 1)) !g
        done;
        !g
      in
      let components = Scc.connected_components_sorted_from_roots_to_leaf graph in
      Printf.printf "components: %d\n" (Array.length components);
      match components.(0), components.(Array.length components - 1) with
      | Scc.No_loop first, Scc.No_loop last ->
          Printf.printf "first %d, last %d\n" first last
      | _ -> Printf.printf "unexpected loops\n")
