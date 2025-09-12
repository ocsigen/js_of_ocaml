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
