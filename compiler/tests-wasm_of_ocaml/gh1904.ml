let empty = [||]

let get (x : float array) = x.(0)

let set (x : float array) e = x.(0) <- e

let catch_bound_error f x =
  try
    ignore (f x);
    assert false
  with Invalid_argument _ -> ()

let () =
  catch_bound_error get empty;
  catch_bound_error (fun () -> set empty 0.) ()
