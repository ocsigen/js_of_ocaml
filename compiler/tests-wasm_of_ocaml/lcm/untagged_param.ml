(* A parameter which is untagged on every path from the function entry is
   passed untagged, the callers untagging it. This must not happen when a
   path does not untag it: here, [x] is not an integer when [b] is false. *)

let[@inline never] f (x : Obj.t) b = if b then (Obj.obj x : int) + 1 else 0

let[@inline never] g (x : Obj.t) = (Obj.obj x : int) + (Obj.obj x : int)

(* Nor when a path loops without untagging it, leaving the loop by an
   exception raised by a primitive *)
type _ ty =
  | I : int ty
  | S : string ty

let[@inline never] h : type a. a ty -> a -> int array -> int =
 fun w x a ->
  match w with
  | I -> x + 1
  | S ->
      let i = ref 0 in
      while true do
        ignore (Sys.opaque_identity a.(!i));
        incr i
      done;
      0

let () =
  let s = Obj.repr (Sys.opaque_identity "str") in
  assert (f (Obj.repr 5) true = 6);
  assert (f s false = 0);
  assert (f (Obj.repr 7) (Sys.opaque_identity true) = 8);
  assert (g (Obj.repr 5) = 10);
  assert (g (Obj.repr (Sys.opaque_identity 6)) = 12);
  assert (h I 5 [||] = 6);
  assert (
    try
      ignore (h S (Sys.opaque_identity "str") [| 1; 2 |]);
      false
    with Invalid_argument _ -> true)
