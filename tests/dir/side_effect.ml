open Common
let log_stop = log_start "Side effect test suite"
let i = ref 0

let side_effect yes label =
  if yes
  then begin
    Printf.printf "Side effect: %s\n%!" label;
    incr i
  end;
  0

let _ = side_effect false "this is only to avoid inlining"

let f = match side_effect true "Should only see this once" with
  | 0 | 1 | 2 -> Printf.printf "Please don't optimize this away\n%!"
  | _ -> Printf.printf "Or this\n%!"

let _ = if !i = 1
  then log_success ()
  else log_failure "side effect computed twice"

let _ = log_stop ()
