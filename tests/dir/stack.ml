open Common

let log_stop = log_start "Stack size test suite"

let rec fact = function
  | 0. -> 1.
  | n -> fact ( n -. 1.0) *. n

let rec fibo = function
  | 0. | 1. -> 1.
  | n -> fibo ( n -. 1.0) +. fibo ( n -. 2.0)



let check f n =
  try
    let res = f (float_of_int n) in
    if res < 0.0 then exit 0
    else log_success ()
  with _ ->
    log_failure (Printf.sprintf "stack overflow with size %d" n);
    raise Not_found


let trywith f =
  for i = 0 to 1000
  do check f i
  done

let _ = trywith fact


let () = log_stop ()
