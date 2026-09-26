(* A parameter which is unboxed on every path from the function entry is
   passed unboxed, the callers unboxing it. This must not happen when a
   path does not unbox it: here, [x] is not an [int32] when [b] is false. *)

let[@inline never] f (x : Obj.t) b = if b then Int32.add (Obj.obj x : int32) 1l else 0l

(* Boxed parameters used only by arithmetic, called with values read from
   memory *)
let[@inline never] rot x n =
  Int32.logor (Int32.shift_left x n) (Int32.shift_right_logical x (32 - n))

let[@inline never] mix x = Int32.logxor (rot x 3) (Int32.logxor (rot x 11) (rot x 23))

let[@inline never] scale (x : float) y = (x *. y) +. x

let[@inline never] add64 x y = Int64.add (Int64.mul x 3L) y

(* A parameter which is also stored must stay boxed *)
let last = ref 0l

let[@inline never] keep x =
  last := x;
  Int32.add x 1l

let () =
  let s = Obj.repr (Sys.opaque_identity "str") in
  assert (f (Obj.repr 5l) true = 6l);
  assert (f s false = 0l);
  assert (f (Obj.repr (Sys.opaque_identity 7l)) (Sys.opaque_identity true) = 8l);
  let a = Array.init 8 (fun i -> Int32.of_int ((i * 0x1234567) + 1)) in
  let acc = ref 0l in
  for i = 0 to Array.length a - 1 do
    acc := Int32.add !acc (mix a.(i))
  done;
  assert (!acc = -407293850l);
  let r = ref 1.5 in
  for _ = 1 to 3 do
    r := scale !r 2.
  done;
  assert (!r = 40.5);
  let b = [| 1L; 2L; 3L |] in
  assert (add64 b.(0) (add64 b.(1) b.(2)) = 12L);
  assert (keep a.(1) = Int32.add a.(1) 1l);
  assert (!last = a.(1))
