(* Normalizations of an integer are shared between its uses: bound checks,
   array accesses, divisions, and comparisons, which use a normalized copy
   when one is available. The sums below overflow when [n] is large, so an
   unnormalized value differs from its normalized copy. *)

let[@inline never] get a i n =
  let j = i + n in
  (* The bound check and the access share the normalization of [j] *)
  a.(j) + a.(j)

let[@inline never] div i n d =
  let j = i + n in
  j / d, j mod d, j < 0, j = -1

let[@inline never] sum a n =
  let s = ref 0 in
  for i = 0 to Array.length a - 1 do
    let j = i + n in
    if j >= 0 && j < Array.length a then s := !s + a.(j) + a.(j)
  done;
  !s

let () =
  let a = Array.init 10 (fun i -> i) in
  assert (get a 3 4 = 14);
  (match get a 3 (Sys.opaque_identity max_int) with
  | _ -> assert false
  | exception Invalid_argument _ -> ());
  assert (
    div 0 (Sys.opaque_identity max_int) 3 = (max_int / 3, max_int mod 3, false, false));
  assert (div 1 (Sys.opaque_identity max_int) 7 = (min_int / 7, min_int mod 7, true, false));
  assert (div 0 (Sys.opaque_identity (-1)) 2 = (0, -1, true, true));
  assert (sum a 5 = 2 * (5 + 6 + 7 + 8 + 9));
  assert (sum a (Sys.opaque_identity max_int) = 0);
  assert (sum a (Sys.opaque_identity min_int) = 0)
