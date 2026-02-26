let slow_path x =
  (* This forces x to be boxed as a generic value on a cold path. *)
  ignore (Obj.magic x : int)

let test n =
  let rec loop i acc =
    if i = 0
    then acc
    else (
      let acc' = Int64.add acc (Int64.of_int i) in
      if i = 1 then slow_path acc';
      loop (i - 1) acc')
  in
  loop n 0L

let () = assert (Int64.equal (test 100) 5050L)
