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

(* Differential testing of Int_trie against Map.Make (Int) *)

module T = Js_of_ocaml_compiler.Int_trie
module M = Map.Make (Int)

let fail fmt = Printf.ksprintf failwith fmt

let check msg t m =
  let bt = T.bindings t and bm = M.bindings m in
  if bt <> bm then fail "%s: bindings differ" msg;
  if T.cardinal t <> M.cardinal m then fail "%s: cardinal" msg;
  if T.is_empty t <> M.is_empty m then fail "%s: is_empty" msg;
  let l = ref [] in
  T.iter (fun k v -> l := (k, v) :: !l) t;
  if List.rev !l <> bm then fail "%s: iter order" msg;
  if
    T.fold (fun k v acc -> (k, v) :: acc) t []
    <> M.fold (fun k v acc -> (k, v) :: acc) m []
  then fail "%s: fold order" msg;
  if List.of_seq (T.to_seq t) <> List.of_seq (M.to_seq m) then fail "%s: to_seq" msg;
  if List.of_seq (T.to_rev_seq t) <> List.of_seq (M.to_rev_seq m)
  then fail "%s: to_rev_seq" msg;
  (match M.min_binding_opt m, M.max_binding_opt m with
  | None, _ | _, None ->
      List.iter
        (fun (name, f) ->
          match f t with
          | (_ : int * int) -> fail "%s: %s on an empty map" msg name
          | exception Not_found -> ())
        [ "min_binding", T.min_binding; "max_binding", T.max_binding; "choose", T.choose ]
  | Some mn, Some mx ->
      if T.min_binding t <> mn || T.choose t <> mn then fail "%s: min_binding" msg;
      if T.max_binding t <> mx then fail "%s: max_binding" msg);
  M.iter
    (fun k v ->
      if T.find k t <> v || T.find_opt k t <> Some v || not (T.mem k t)
      then fail "%s: lookup of %d" msg k)
    m;
  if not (T.equal ( = ) t (T.of_seq (M.to_seq m))) then fail "%s: of_seq / equal" msg;
  if not (T.equal ( = ) t (T.of_seq (List.to_seq (List.rev bm))))
  then fail "%s: of_seq in decreasing order" msg

let random_ops range steps =
  Random.init (range + steps);
  let t = ref T.empty and m = ref M.empty in
  let history = ref [] in
  for step = 1 to steps do
    let k = Random.int range in
    (match Random.int 10 with
    | 0 | 1 | 2 | 3 | 4 ->
        let v = Random.int 1000 in
        t := T.add k v !t;
        m := M.add k v !m
    | 5 | 6 ->
        t := T.remove k !t;
        m := M.remove k !m
    | 7 ->
        let f = function
          | None -> Some 7
          | Some x -> if x mod 2 = 0 then None else Some (x + 1)
        in
        t := T.update k f !t;
        m := M.update k f !m
    | _ ->
        if T.find_opt k !t <> M.find_opt k !m then fail "find_opt %d" k;
        if T.mem k !t <> M.mem k !m then fail "mem %d" k;
        if (try Some (T.find k !t) with Not_found -> None) <> M.find_opt k !m
        then fail "find %d" k);
    if step mod (steps / 20) = 0
    then (
      check "random" !t !m;
      history := (!t, !m) :: !history;
      (* Whole-map operations, checking the order of the calls *)
      let c1 = ref [] and c2 = ref [] in
      let t' =
        T.mapi
          (fun k v ->
            c1 := k :: !c1;
            v + k)
          !t
      in
      let m' =
        M.mapi
          (fun k v ->
            c2 := k :: !c2;
            v + k)
          !m
      in
      if !c1 <> !c2 then fail "mapi order";
      check "mapi" t' m';
      check "map" (T.map succ !t) (M.map succ !m);
      let c1 = ref [] and c2 = ref [] in
      let f c k v =
        c := k :: !c;
        if (k + v) mod 3 = 0 then None else Some (v * 2)
      in
      let t' = T.filter_map (f c1) !t and m' = M.filter_map (f c2) !m in
      if !c1 <> !c2 then fail "filter_map order";
      check "filter_map" t' m';
      let p k _ = k mod 5 <> 0 in
      check "filter" (T.filter p !t) (M.filter p !m);
      check "filter all" (T.filter (fun _ _ -> false) !t) M.empty;
      if not (T.equal ( = ) !t (T.map Fun.id !t)) then fail "equal (copy)";
      if (not (T.is_empty !t)) && T.equal ( = ) !t (T.map succ !t)
      then fail "equal (differs)")
  done;
  (* Persistence: older versions are unaffected by later updates *)
  List.iter (fun (t, m) -> check "history" t m) !history;
  (* Comparison of maps of different heights *)
  let tall = T.remove 1_000_000 (T.add 1_000_000 0 !t) in
  if not (T.equal ( = ) tall !t && T.equal ( = ) !t tall) then fail "equal (heights)";
  check "tall" tall !m;
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

let%expect_test "sparse keys and removals" =
  (* Keys spanning many levels, then removing them all. The largest
     key must remain positive on 32-bit platforms. *)
  let n = min 41 (Sys.int_size - 2) in
  let keys =
    List.concat_map
      (fun i -> [ (1 lsl i) - 1; 1 lsl i; (1 lsl i) + 1 ])
      (List.init n Fun.id)
  in
  let t = List.fold_left (fun t k -> T.add k k t) T.empty keys in
  let m = List.fold_left (fun m k -> M.add k k m) M.empty keys in
  check "sparse" t m;
  if T.find_opt (1 lsl n) t <> None || T.find_opt max_int t <> None || T.mem (-1) t
  then fail "absent keys";
  let t, m =
    List.fold_left
      (fun (t, m) k ->
        let t = T.remove k t and m = M.remove k m in
        check "removing" t m;
        t, m)
      (t, m)
      (List.rev keys)
  in
  if not (T.is_empty t && T.equal ( = ) t T.empty) then fail "not empty";
  (* Adding to an empty map which used to be tall *)
  let t = T.add 3 3 (T.add 0 0 t) in
  check "re-add" t (M.add 3 3 (M.add 0 0 m));
  if not (T.equal ( = ) t (T.add 0 0 (T.add 3 3 T.empty))) then fail "equal (re-add)";
  print_endline "ok";
  [%expect {| ok |}]

let%expect_test "physical equality and sharing" =
  let t = T.of_seq (List.to_seq (List.init 100 (fun i -> i * 3, i))) in
  if T.add 30 10 t != t then fail "add of the same value";
  if T.update 30 (fun v -> v) t != t then fail "update with the same value";
  if T.remove 31 t != t then fail "remove of an absent key";
  if T.cardinal (T.add 30 11 t) <> T.cardinal t then fail "cardinal on replace";
  (* Same bindings, different histories: structurally equal *)
  let a = List.fold_left (fun t k -> T.add k k t) T.empty (List.init 500 Fun.id) in
  let b =
    List.fold_left (fun t k -> T.add k k t) T.empty (List.rev (List.init 500 Fun.id))
  in
  let c =
    List.fold_left
      (fun t k -> T.remove k t)
      (T.of_seq (List.to_seq (List.init 600 (fun k -> k, k))))
      (List.init 100 (fun i -> 500 + i))
  in
  if not (T.equal ( = ) a b && T.equal ( = ) b c) then fail "canonical structure";
  print_endline "ok";
  [%expect {| ok |}]

let%expect_test "values" =
  (* Floats: the arrays of slots must not be flat float arrays *)
  let f =
    List.fold_left
      (fun t k -> T.add k (float_of_int k *. 0.5) t)
      T.empty
      (List.init 100 Fun.id)
  in
  List.iter
    (fun k -> if T.find k f <> float_of_int k *. 0.5 then fail "float")
    (List.init 100 Fun.id);
  if T.find 3 (T.map (fun x -> x +. 1.) f) <> 2.5 then fail "float map";
  (* Immediates and mutable values *)
  let z = T.add 5 0 (T.add 7 (-1) T.empty) in
  if T.find 5 z <> 0 || T.find 7 z <> -1 || T.cardinal z <> 2 then fail "immediates";
  if not (T.mem 1 (T.add 1 (ref 0) T.empty)) then fail "ref";
  (match T.add (-1) 0 T.empty with
  | _ -> fail "negative key"
  | exception Invalid_argument _ -> ());
  print_endline "ok";
  [%expect {| ok |}]
