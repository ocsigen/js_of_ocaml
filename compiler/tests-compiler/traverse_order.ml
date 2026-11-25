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

(* Tests for [Code.traverse] and [Code.preorder_traverse], which are implemented
   with an explicit stack: the visit order must match the former recursive
   implementation. That a deep chain of blocks does not overflow the stack is
   tested in deep_cfg/deep_cfg.ml, which runs with a small stack. *)

open Js_of_ocaml_compiler.Code

(* No variable is created at the toplevel of the module: that would run at
   module initialization time and shift the variable numbering of every test
   running in the same process. *)

let c pc = pc, []

let mk l =
  List.fold_left
    (fun blocks (pc, branch) -> Addr.Map.add pc { params = []; body = []; branch } blocks)
    Addr.Map.empty
    l

(* Print the order in which the callback of each traversal is applied, for both
   folds. The callback result is threaded through the traversal, so this also
   checks that the accumulator is threaded in visit order. *)
let print_orders blocks start =
  List.iter
    (fun (name, fold) ->
      let visit traverse =
        let rev_order = traverse fold (fun pc acc -> pc :: acc) start blocks [] in
        String.concat " " (List.rev_map string_of_int rev_order)
      in
      Printf.printf
        "%s: postorder [%s], preorder [%s]\n"
        name
        (visit traverse)
        (visit preorder_traverse))
    [ "fold_children", { fold = fold_children }
    ; "fold_children_skip_try_body", { fold = fold_children_skip_try_body }
    ]

let%expect_test "diamond" =
  let x = Var.fresh () in
  let blocks =
    mk [ 0, Cond (x, c 1, c 2); 1, Branch (c 3); 2, Branch (c 3); 3, Return x ]
  in
  print_orders blocks 0;
  [%expect
    {|
    fold_children: postorder [3 1 2 0], preorder [0 1 3 2]
    fold_children_skip_try_body: postorder [3 1 2 0], preorder [0 1 3 2]
    |}]

let%expect_test "switch with duplicate targets" =
  let x = Var.fresh () in
  let blocks =
    mk
      [ 0, Switch (x, [| c 1; c 2; c 3; c 2 |])
      ; 1, Branch (c 4)
      ; 2, Branch (c 4)
      ; 3, Stop
      ; 4, Return x
      ]
  in
  print_orders blocks 0;
  [%expect
    {|
    fold_children: postorder [4 2 3 1 0], preorder [0 2 4 3 1]
    fold_children_skip_try_body: postorder [4 2 3 1 0], preorder [0 2 4 3 1]
    |}]

let%expect_test "loop" =
  let x = Var.fresh () in
  let blocks =
    mk [ 0, Branch (c 1); 1, Cond (x, c 2, c 3); 2, Branch (c 1); 3, Return x ]
  in
  print_orders blocks 0;
  [%expect
    {|
    fold_children: postorder [2 3 1 0], preorder [0 1 2 3]
    fold_children_skip_try_body: postorder [2 3 1 0], preorder [0 1 2 3]
    |}]

let%expect_test "self loop at the root" =
  let x = Var.fresh () in
  let blocks = mk [ 0, Cond (x, c 0, c 1); 1, Stop ] in
  print_orders blocks 0;
  [%expect
    {|
    fold_children: postorder [1 0], preorder [0 1]
    fold_children_skip_try_body: postorder [1 0], preorder [0 1]
    |}]

let%expect_test "exception handler" =
  let x = Var.fresh () in
  let blocks =
    mk
      [ 0, Pushtrap (c 1, x, c 4)
      ; 1, Cond (x, c 2, c 3)
      ; 2, Poptrap (c 5)
      ; 3, Raise (x, `Normal)
      ; 4, Branch (c 5)
      ; 5, Return x
      ]
  in
  print_orders blocks 0;
  [%expect
    {|
    fold_children: postorder [5 2 3 1 4 0], preorder [0 1 2 5 3 4]
    fold_children_skip_try_body: postorder [5 4 0], preorder [0 5 4]
    |}]

let%expect_test "nested exception handlers" =
  let x = Var.fresh () in
  let blocks =
    mk
      [ 0, Pushtrap (c 1, x, c 6)
      ; 1, Pushtrap (c 2, x, c 4)
      ; 2, Poptrap (c 3)
      ; 3, Poptrap (c 7)
      ; 4, Branch (c 5)
      ; 5, Poptrap (c 7)
      ; 6, Branch (c 7)
      ; 7, Return x
      ]
  in
  print_orders blocks 0;
  [%expect
    {|
    fold_children: postorder [7 3 2 5 4 1 6 0], preorder [0 1 2 3 7 4 5 6]
    fold_children_skip_try_body: postorder [7 6 0], preorder [0 7 6]
    |}]
