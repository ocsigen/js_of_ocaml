let test n =
  let rec loop i acc =
    if i = 0
    then acc
    else
      let acc' = Int64.add acc (Int64.of_int i) in
      loop (i - 1) acc'
  in
  loop n 0L

let () = assert (Int64.equal (test 100) 5050L)
