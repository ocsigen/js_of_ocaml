(* A loop whose header is the entry of the function: the counter [shift],
   received tagged, is untagged once in the entry block, and compared with 0
   untagged. The results must not change, including for large counters. *)

let cell = [| 0 |]

let o x =
  cell.(0) <- x;
  cell.(0)

let rec find (a : int array) shift k =
  let c = Array.unsafe_get a ((k lsr shift) land 31) in
  if c = 0 || shift = 0 then c else find a (shift - 5) k

let rec count shift n = if shift = 0 then n else count (shift - 1) (n + 1)

let () =
  let a = Array.init 32 (fun i -> i + 1) in
  assert (find a (o 25) (o (3 lsl 25)) = 1);
  assert (find a (o 10) (o (7 lsl 10)) = 1);
  assert (find a (o 0) (o 5) = 6);
  let r = ref 0 in
  for i = 0 to 100 do
    r := !r + find a (o (5 * (i mod 6))) (o (i * 7919))
  done;
  assert (!r = 1675);
  assert (count (o 1000) 0 = 1000)
