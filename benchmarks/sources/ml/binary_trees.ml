(* The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 *
 * Contributed by Troestler Christophe
 * Modified by Fabrice Le Fessant
 *)

type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree

let rec make i d =
  (* if d = 0 then Empty *)
  if d = 0
  then Node (Empty, i, Empty)
  else
    let i2 = 2 * i and d = d - 1 in
    Node (make (i2 - 1) d, i, make i2 d)

let rec check = function
  | Empty -> 0
  | Node (l, i, r) -> i + check l - check r

let min_depth = 4

let max_depth =
  let n = try int_of_string Sys.argv.(1) with _ -> 10 in
  max (min_depth + 2) n

let stretch_depth = max_depth + 1

let () =
  (* Gc.set { (Gc.get()) with Gc.minor_heap_size = 1024 * 1024; max_overhead = -1; }; *)
  let _c = check (make 0 stretch_depth) in
  ( (*
  Printf.printf "stretch tree of depth %i\t check: %i\n" stretch_depth c
 *) )

let long_lived_tree = make 0 max_depth

let loop_depths d =
  for i = 0 to ((max_depth - d) / 2) + 1 - 1 do
    let d = d + (i * 2) in
    let niter = 1 lsl (max_depth - d + min_depth) in
    let c = ref 0 in
    for i = 1 to niter do
      c := !c + check (make i d) + check (make (-i) d)
    done;
    ( (*
      Printf.printf "%i\t trees of depth %i\t check: %i\n" (2 * niter) d !c;
 *) )
  done

let () =
  (*
  flush stdout;
*)
  loop_depths min_depth;
  ( (*
  Printf.printf "long lived tree of depth %i\t check: %i\n"
    max_depth (check long_lived_tree)
 *) )
