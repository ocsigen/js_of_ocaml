let slow_path x =
  (* This forces x to be boxed as a Value *)
  ignore (Obj.magic x : 'a)

let test n =
  let rec loop i acc =
    if i = 0
    then acc
    else
      let acc' = acc + i in
      if i = 1 then slow_path i;
      (* Conditional slow path *)
      loop (i - 1) acc'
  in
  loop n 0

let () =
  let r = test 100 in
  Printf.printf "Result: %d\n" r
