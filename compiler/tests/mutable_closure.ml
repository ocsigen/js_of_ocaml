
open Common
let log_stop = log_start "Closure test suite (2)"

let direct = ref []
let indirect = ref []

let () =
  for i = 0 to 3 do
    let rec f = function
      | 0 -> i
      | -1 -> g (-2) (* deadcode or infinite loop *)
      | n -> g (pred n)
    and g = function
      | 0 -> i
      | -1 -> f (-2)  (* deadcode or infinite loop *)
      | n -> f (pred n)
    in
    direct   := f i :: !direct;
    indirect := (fun () -> f i) :: !indirect
  done;
  let indirect = List.map (fun f -> f ()) !indirect in
  let direct = !direct in
  assert (indirect = direct)

let () =
  let delayed = ref (fun () -> ()) in
  for i = 1 to 2 do
    let rec f n = function
      | 0 -> assert (i = n)
      | j -> delayed :=
         let prev = !delayed in
         (fun () -> prev (); f (succ n + i - i) (pred j))
    in f 0 i
  done;
  !delayed ();;



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
