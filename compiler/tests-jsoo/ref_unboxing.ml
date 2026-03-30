let f () =
  let r =
    try
      assert (Random.int 3 < 10);
      ref 1
    with _ -> assert false
  in
  try
    if Random.int 3 < 0 then incr r else decr r;
    !r
  with _ -> assert false

(* Test: while loop with cross-assignment between refs.
   This is the pattern from #2209: i := !pi followed by
   reading !i (which should be the old !pi value). *)
let loop_cross_assign () =
  let i = ref 4 in
  let pi = ref 2 in
  while !i > 0 do
    i := !pi;
    pi := !i - 1
  done;
  !i
