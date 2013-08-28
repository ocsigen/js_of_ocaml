open Common

let log_stop = log_start "Closure test suite"


let _ =
  let l_fun = ref [] in
  let l_var = ref [] in
  let l_base = [0;1;2;3;4;5;6;7;8;9;10] in
  for i=0 to 10
  do
    l_fun := (fun () -> i)::!l_fun;
    l_var := i::!l_var;
  done;

  let sum l = List.fold_left (+) 0 l in

  let sum_base = sum l_base in
  if (sum !l_var) <> sum_base
  then log_failure "l_var"
  else if (sum (List.map (fun f -> f()) !l_fun)) <> sum_base
  then log_failure "l_fun"
  else log_success ()

let () = log_stop ()
