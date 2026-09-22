(* Js_of_ocaml tests
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

(* Differential testing of Int_set against Set.Make (Int) *)

module T = Js_of_ocaml_compiler.Int_set
module S = Set.Make (Int)

let fail fmt = Printf.ksprintf failwith fmt

let check msg t s =
  let et = T.elements t and es = S.elements s in
  if et <> es then fail "%s: elements differ" msg;
  if T.cardinal t <> S.cardinal s then fail "%s: cardinal" msg;
  if T.is_empty t <> S.is_empty s then fail "%s: is_empty" msg;
  let l = ref [] in
  T.iter (fun k -> l := k :: !l) t;
  if List.rev !l <> es then fail "%s: iter order" msg;
  if T.fold (fun k acc -> k :: acc) t [] <> S.fold (fun k acc -> k :: acc) s []
  then fail "%s: fold order" msg;
  if List.of_seq (T.to_seq t) <> es then fail "%s: to_seq" msg;
  (match S.min_elt_opt s, S.max_elt_opt s with
  | None, _ | _, None ->
      List.iter
        (fun (name, f) ->
          match f t with
          | (_ : int) -> fail "%s: %s on an empty set" msg name
          | exception Not_found -> ())
        [ "min_elt", T.min_elt; "max_elt", T.max_elt; "choose", T.choose ]
  | Some mn, Some mx ->
      if T.min_elt t <> mn || T.choose t <> mn then fail "%s: min_elt" msg;
      if T.max_elt t <> mx then fail "%s: max_elt" msg);
  S.iter (fun k -> if not (T.mem k t) then fail "%s: mem %d" msg k) s;
  if not (T.equal t (T.of_list (List.rev es))) then fail "%s: of_list / equal" msg;
  if T.compare t (T.of_list es) <> 0 then fail "%s: compare (equal sets)" msg;
  if
    T.compare_cardinal_with t (List.length es) <> 0
    || T.compare_cardinal_with t (List.length es + 1) >= 0
    || (es <> [] && T.compare_cardinal_with t (List.length es - 1) <= 0)
  then fail "%s: compare_cardinal_with" msg;
  if
    T.to_list_bounded (List.length es) t <> Some es
    || T.to_list_bounded (List.length es - 1) t <> None
  then fail "%s: to_list_bounded" msg

let check_ops msg (t1, s1) (t2, s2) =
  check (msg ^ " union") (T.union t1 t2) (S.union s1 s2);
  check (msg ^ " inter") (T.inter t1 t2) (S.inter s1 s2);
  check (msg ^ " diff") (T.diff t1 t2) (S.diff s1 s2);
  check (msg ^ " diff'") (T.diff t2 t1) (S.diff s2 s1);
  if T.subset t1 t2 <> S.subset s1 s2 then fail "%s: subset" msg;
  if T.disjoint t1 t2 <> S.disjoint s1 s2 then fail "%s: disjoint" msg;
  if T.equal t1 t2 <> S.equal s1 s2 then fail "%s: equal" msg;
  if Int.compare (T.compare t1 t2) 0 <> Int.compare (S.compare s1 s2) 0
  then fail "%s: compare" msg;
  (* Sharing *)
  if S.subset s2 s1 && T.union t1 t2 != t1 then fail "%s: union sharing" msg;
  if S.disjoint s1 s2 && T.diff t1 t2 != t1 then fail "%s: diff sharing" msg

let random_ops range steps =
  Random.init (range + steps);
  let t = ref T.empty and s = ref S.empty in
  let history = ref [] in
  for step = 1 to steps do
    let k = Random.int range in
    (match Random.int 10 with
    | 0 | 1 | 2 | 3 | 4 | 5 ->
        t := T.add k !t;
        s := S.add k !s
    | 6 | 7 ->
        t := T.remove k !t;
        s := S.remove k !s
    | _ ->
        if T.mem k !t <> S.mem k !s then fail "mem %d" k;
        if S.mem k !s && T.add k !t != !t then fail "add sharing";
        if (not (S.mem k !s)) && T.remove k !t != !t then fail "remove sharing");
    if step mod (steps / 20) = 0
    then (
      check "random" !t !s;
      history := (!t, !s) :: !history;
      let p k = k mod 3 <> 0 in
      check "filter" (T.filter p !t) (S.filter p !s);
      check "map" (T.map (fun k -> k / 2) !t) (S.map (fun k -> k / 2) !s);
      if T.exists p !t <> S.exists p !s || T.for_all p !t <> S.for_all p !s
      then fail "exists";
      (* Set operations against other versions, of various heights *)
      List.iter (fun other -> check_ops "ops" (!t, !s) other) !history;
      let small = T.of_list [ 1; 2; 3 ] and ssmall = S.of_list [ 1; 2; 3 ] in
      check_ops "small" (!t, !s) (small, ssmall);
      check_ops "small'" (small, ssmall) (!t, !s);
      check_ops "empty" (!t, !s) (T.empty, S.empty);
      let big = T.add 1_000_000 !t and sbig = S.add 1_000_000 !s in
      check_ops "taller" (!t, !s) (big, sbig);
      check_ops "taller'" (big, sbig) (!t, !s))
  done;
  List.iter (fun (t, s) -> check "history" t s) !history;
  T.cardinal !t

let%expect_test "random operations" =
  (* The sizes are not printed: the random number generator differs
     between OCaml versions *)
  List.iter
    (fun (range, steps) ->
      let (_ : int) = random_ops range steps in
      Printf.printf "range %d, %d steps: ok\n" range steps)
    [ 8, 2_000; 40, 5_000; 1_500, 20_000; 200_000, 40_000; 5_000_000, 20_000 ];
  [%expect
    {|
    range 8, 2000 steps: ok
    range 40, 5000 steps: ok
    range 1500, 20000 steps: ok
    range 200000, 40000 steps: ok
    range 5000000, 20000 steps: ok
    |}]

let%expect_test "sparse elements" =
  (* The largest element must remain positive on 32-bit platforms *)
  let n = min 41 (Sys.int_size - 2) in
  let keys =
    List.concat_map
      (fun i -> [ (1 lsl i) - 1; 1 lsl i; (1 lsl i) + 1 ])
      (List.init n Fun.id)
  in
  let t = T.of_list keys and s = S.of_list keys in
  check "sparse" t s;
  if T.mem (1 lsl n) t || T.mem max_int t || T.mem (-1) t then fail "absent";
  let t, s =
    List.fold_left
      (fun (t, s) k ->
        let t = T.remove k t and s = S.remove k s in
        check "removing" t s;
        t, s)
      (t, s)
      (List.rev keys)
  in
  if not (T.is_empty t && T.equal t T.empty && T.compare t T.empty = 0)
  then fail "not empty";
  let t = T.add 3 (T.add 0 t) in
  check "re-add" t (S.add 3 (S.add 0 s));
  if not (T.equal t (T.of_list [ 0; 3 ])) then fail "equal (re-add)";
  (match T.add (-1) T.empty with
  | _ -> fail "negative"
  | exception Invalid_argument _ -> ());
  (* compare is a total order compatible with equal *)
  let sets =
    List.map T.of_list [ []; [ 0 ]; [ 1 ]; [ 0; 1 ]; [ 5; 1000 ]; [ 1000 ]; [ 31; 32 ] ]
  in
  List.iter
    (fun a ->
      List.iter
        (fun b ->
          let c = T.compare a b in
          if c = 0 <> T.equal a b then fail "compare vs equal";
          if Int.compare c 0 <> -Int.compare (T.compare b a) 0
          then fail "compare antisymmetry";
          if Int.compare c 0 <> Int.compare (compare (T.elements a) (T.elements b)) 0
          then fail "compare order")
        sets)
    sets;
  print_endline "ok";
  [%expect {| ok |}]
