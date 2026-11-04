(* Random choices for the program generators. *)

type t = Random.State.t

let make seed = Random.State.make [| seed |]

let chance st p = Random.State.float st 1. < p

(* Uniform integer in [lo, hi] (inclusive) *)
let range st lo hi =
  if hi - lo < 0x3FFFFFFF
  then lo + Random.State.int st (hi - lo + 1)
  else lo + Int64.to_int (Random.State.int64 st (Int64.of_int (hi - lo + 1)))

(* Uniform integer in [lo, hi] (inclusive), for 64-bit bounds *)
let range64 st lo hi =
  let open Int64 in
  let span = sub hi lo in
  if compare span zero < 0 || equal span max_int
  then Random.State.bits64 st
  else add lo (Random.State.int64 st (succ span))

let pick st l = List.nth l (Random.State.int st (List.length l))

let pick_array st a = a.(Random.State.int st (Array.length a))

let float st lo hi = lo +. Random.State.float st (hi -. lo)
