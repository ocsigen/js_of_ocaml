(* The conversions of nativeints are explicit, as those of the other
   boxed numbers, so that LCM places them *)

(* A loop invariant read from memory: its unboxing is hoisted out of the
   loop *)
let[@inline never] scale (r : nativeint ref) a =
  let k = !r in
  let acc = ref 0n in
  for i = 0 to Array.length a - 1 do
    acc := Nativeint.add !acc (Nativeint.mul k a.(i))
  done;
  !acc

(* A number boxed only on a rare path *)
let last = ref 0n

let[@inline never] step b x =
  let z = Nativeint.add x 1n in
  if b then last := z;
  Nativeint.mul z z

(* A parameter which is also stored must stay boxed *)
let[@inline never] keep x =
  last := x;
  Nativeint.add x 1n

let[@inline never] pick b x y =
  let z = if b then Nativeint.add x 1n else Nativeint.sub y 1n in
  if Nativeint.compare z 0n > 0 then Some z else None

let () =
  let a = Array.init 1000 (fun i -> Nativeint.of_int i) in
  let r = ref 3n in
  let s = ref 0n in
  for _ = 1 to 10 do
    s := Nativeint.add !s (scale r a)
  done;
  assert (!s = 14985000n);
  let t = ref 0n in
  for i = 1 to 1000 do
    t := Nativeint.add !t (step (i = 500) (Nativeint.of_int i))
  done;
  assert (!last = 501n);
  assert (!t = 334835500n);
  assert (keep a.(7) = 8n);
  assert (!last = 7n);
  assert (pick (Sys.opaque_identity true) 4n 0n = Some 5n);
  assert (pick (Sys.opaque_identity false) 4n 0n = None)
