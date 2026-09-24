(* Exercise the pass splitting the toplevel function into smaller
   functions (compiled with a very small [toplevel_split_size]). *)

let r = ref 1

let a = Array.make 8 0

let bump n =
  r := ((!r * 31) + n) mod 1009;
  !r

(* Straight-line code with short-lived variables *)
let x1 = bump 1

let x2 = x1 + bump 2

let x3 = x2 * bump 3

let () = Printf.printf "%d %d %d\n" x1 x2 x3

(* A variable defined in both branches of a conditional *)
let y = if !r land 1 = 0 then bump 4 else bump 5

let z = if y > 500 then "big" else "small"

let () = Printf.printf "%d %s\n" y z

(* A variable defined in a single branch and read afterwards *)
let () =
  let v = ref 0 in
  if !r land 2 = 0 then v := bump 6;
  Printf.printf "%d\n" !v

(* Exception handlers in the toplevel *)
exception E of int

let w = try if !r land 4 = 0 then raise (E (bump 7)) else bump 8 with E n -> n + 1

let () = Printf.printf "%d\n" w

let () =
  try
    for i = 0 to 20 do
      a.(i mod 8) <- a.(i mod 8) + bump i;
      if i > 15 then raise Exit
    done
  with Exit -> Printf.printf "exit %d\n" !r

(* Bound checks and division: the branches to the runtime error
   handlers escape the outlined code *)
let () =
  try Printf.printf "%d\n" a.(!r mod 12)
  with Invalid_argument _ -> print_endline "bound"

let () =
  try Printf.printf "%d\n" (1000 / (!r mod 3))
  with Division_by_zero -> print_endline "div"

(* Loops with locals live across iterations *)
let s =
  let acc = ref 0 in
  let i = ref 0 in
  while !i < 10 do
    acc := !acc + bump !i;
    incr i
  done;
  !acc

let () = Printf.printf "%d\n" s

(* A switch *)
let () =
  match !r mod 5 with
  | 0 -> print_endline "zero"
  | 1 -> print_endline "one"
  | 2 -> print_endline "two"
  | _ -> print_endline "many"

(* Closures capturing toplevel variables *)
let f n = n + x1 + y + w + s

let () = Printf.printf "%d\n" (f 10)

(* A long-lived local variable *)
let long = bump 9

let () =
  for i = 0 to 30 do
    ignore (bump i)
  done

let () = Printf.printf "%d %d\n" long !r

let () =
  Array.iter (fun v -> Printf.printf "%d " v) a;
  print_newline ()
