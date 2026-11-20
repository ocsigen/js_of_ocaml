(* A raising function returning an integer uses the null-return
   convention: its return value is tagged. *)

exception Bad of int

let[@inline never] rec parse s i acc =
  if i >= String.length s
  then acc
  else
    match s.[i] with
    | '0' .. '9' as c -> parse s (i + 1) ((acc * 10) + Char.code c - 48)
    | _ -> raise (Bad i)

(* Tail call between raising functions returning integers *)
let[@inline never] parse_all s = parse s 0 0

let () =
  let n = ref 0 in
  for i = 0 to 9 do
    try n := !n + parse_all (if i land 1 = 0 then "123" else "12x") + 1
    with Bad k -> n := !n + (1000 * k)
  done;
  Printf.printf "%d\n" !n

(* Mutually recursive raising functions returning integers: the tail
   calls between them must be preserved. Each function has several
   call sites, so that they are not inlined into one another. Their
   return values are computed, hence untagged integers. *)
let[@inline never] rec down_a n acc =
  if n = 0
  then acc + 2
  else if n < 0
  then raise (Bad n)
  else if n land 1 = 0
  then down_b (n - 1) (acc + 1)
  else down_b (n - 1) (acc + 3)

and[@inline never] down_b n acc =
  if n = 0
  then acc + 1
  else if n land 2 = 0
  then down_a (n - 1) (acc + 2)
  else down_a (n - 1) (acc + 1)

(* A tail call through a closure *)
let next = ref (fun _ _ -> 0)

let[@inline never] through_closure n acc =
  if n = 0 then acc else if n < 0 then raise (Bad n) else !next (n - 1) (acc + 1)

let () = next := through_closure

let () =
  (try Printf.printf "deep: %d\n" (down_a 10_000_000 0)
   with Bad k -> Printf.printf "deep: raised %d\n" k);
  (try Printf.printf "%d %d\n" (down_a (-1) 0) (down_b 7 0)
   with Bad k -> Printf.printf "raised %d\n" k);
  (try Printf.printf "%d\n" (down_b 10_000_001 0)
   with Bad k -> Printf.printf "raised %d\n" k);
  (* Calls through a closure go through the wrapper *)
  let f = Sys.opaque_identity down_a in
  (try Printf.printf "closure: %d " (f 10 0) with Bad k -> Printf.printf "raised %d " k);
  (try Printf.printf "%d\n" (f (-2) 0) with Bad k -> Printf.printf "raised %d\n" k);
  try Printf.printf "through a closure: %d\n" (through_closure 100_000 0)
  with Bad k -> Printf.printf "through a closure: raised %d\n" k
