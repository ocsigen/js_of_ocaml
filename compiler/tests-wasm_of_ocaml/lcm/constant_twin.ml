(* A number constant can reach, on a dead path, a use expecting a number of
   another kind: here, once the functions are inlined, the integer addition
   is applied to a float constant and to an int32 constant. *)

type _ t =
  | I : int t
  | F : float t
  | N : int32 t

let g (type u) (b : u t) (x : u) : u =
  match b with
  | I -> x + 1
  | F -> x +. 1.
  | N -> Int32.add x 1l

let h (type u) (b : u t) (x : u) : u =
  match b with
  | I -> x + 1
  | F -> x +. 1.
  | N -> Int32.add x 1l

let g' b = g b 5.

let h' b = h b 7l

let () =
  ignore (Sys.opaque_identity I);
  assert (g' F = 6.);
  assert (g' F = 6.);
  assert (h' N = 8l);
  assert (h' N = 8l)
