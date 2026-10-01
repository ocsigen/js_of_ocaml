(* An untagged copy of a value may be 0 when the value is not an integer
   (a speculative untagging, hoisted out of a loop): it must not be used to
   compare the value with 0. Here, [p] is not an integer when [y] is not. *)

let f (y : Obj.t) c n =
  let s = ref 0 in
  let j = ref 0 in
  while !j < n do
    if Obj.is_int y then s := !s + (Obj.obj y : int);
    let p = if c then y else Obj.repr 0 in
    let i = ref 0 in
    while !i < n do
      if p == Obj.repr 0 then s := !s + 1000;
      if Obj.is_int p then s := !s + (Obj.obj p : int);
      incr i
    done;
    incr j
  done;
  !s

let () =
  assert (f (Obj.repr 5) true 2 = 30);
  assert (f (Obj.repr 5) false 2 = 4010);
  assert (f (Obj.repr (Sys.opaque_identity "str")) true 2 = 0);
  assert (f (Obj.repr (Sys.opaque_identity "str")) false 2 = 4000)
