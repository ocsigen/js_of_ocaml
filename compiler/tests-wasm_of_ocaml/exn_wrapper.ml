(* A raising function which also escapes: direct calls under an
   exception handler use the null-return convention, while calls
   through a closure go through a wrapper that throws the exception. *)

exception Stop of int

let[@inline never] rec find x l =
  match l with
  | [] -> raise (Stop x)
  | y :: r -> if y = x then y else find x r

(* [find] escapes *)
let forward = ref (fun _ _ -> 0)

let () = forward := find

let l = [ 1; 2; 3; 4; 5 ]

let[@inline never] count () =
  let n = ref 0 in
  for i = 0 to 9 do
    try n := !n + find i l with Stop k -> n := !n + (100 * k)
  done;
  !n

(* No exception handler in this function *)
let[@inline never] no_handler x = find x l + 1

let () =
  Printf.printf "direct: %d\n" (count ());
  Printf.printf "through a ref: %d " (!forward 3 l);
  (match !forward 7 l with
  | n -> Printf.printf "%d\n" n
  | exception Stop k -> Printf.printf "raised %d\n" k);
  let partial = find 9 in
  (match partial l with
  | n -> Printf.printf "partial application: %d\n" n
  | exception Stop k -> Printf.printf "partial application: raised %d\n" k);
  (match List.iter (fun x -> ignore (find x [ 1 ])) [ 1; 2 ] with
  | () -> print_endline "iter: no exception"
  | exception Stop k -> Printf.printf "iter: raised %d\n" k);
  Printf.printf "no handler: %d " (no_handler 2);
  match no_handler 8 with
  | n -> Printf.printf "%d\n" n
  | exception Stop k -> Printf.printf "raised %d\n" k
