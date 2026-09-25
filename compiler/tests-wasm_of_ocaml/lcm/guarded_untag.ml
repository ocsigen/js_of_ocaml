(* The untagging of a loop invariant of unknown type is hoisted out of the
   loop, and must then not fail when the value is not an integer: here, it
   is only used as an integer when [Obj.is_int] holds. *)

let f (x : Obj.t) n =
  let s = ref 0 in
  for i = 1 to n do
    if Obj.is_int x then s := !s + (Obj.obj x : int) + i else s := !s - i
  done;
  !s

let () =
  assert (f (Obj.repr 5) 10 = 105);
  assert (f (Obj.repr (Sys.opaque_identity "str")) 10 = -55)
