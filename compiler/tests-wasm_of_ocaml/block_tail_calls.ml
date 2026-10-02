(* Casting call results to a block type must not prevent tail calls.
   The results of [f] and [g] are used as blocks. *)

let n = 10_000_000

(* Only called directly *)
let rec f1 n = if n = 0 then n, n + 1 else g1 (n - 1)

and g1 n = if n = 0 then n, n + 2 else f1 (n - 1)

let () = Printf.printf "direct calls: %d\n" (snd (f1 n))

(* [g2] escapes, so its result is not cast *)
let rec f2 n = if n = 0 then n, n + 1 else g2 (n - 1)

and g2 n = if n = 0 then n, n + 2 else f2 (n - 1)

let r = ref g2

let () =
  (if Array.length Sys.argv > 5 then r := fun n -> n, n);
  Printf.printf "escaping function: %d %d\n" (snd (f2 n)) (snd (!r 3))
