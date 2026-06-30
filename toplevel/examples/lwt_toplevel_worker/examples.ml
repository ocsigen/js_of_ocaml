(** Overview *)

let x = 10 + 10
let y = x * 3
let c = String.make x 'a'
let sin1 = sin 1.
let rec fact n = if n = 0 then 1. else float n *. fact (n - 1)
let _ = Printf.printf "fact 20 = %f\n" (fact 20)
let _ = "abc" < "def"

(** Mutually recursive function *)

let rec even n =
  match n with
  | 0 -> true
  | x -> odd (x - 1)

and odd n =
  match n with
  | 0 -> false
  | x -> even (x - 1)

(** Mutually recursive module *)

module rec Odd : sig
  val odd : int -> bool
end = struct
  let odd x = if x = 0 then false else Even.even (pred x)
end

and Even : sig
  val even : int -> bool
end = struct
  let even x = if x = 0 then true else Odd.odd (pred x)
end

(** Data structures *)

let squares = List.map (fun n -> n * n) [ 1; 2; 3; 4; 5 ]
let sum = List.fold_left ( + ) 0 squares

module M = Map.Make (String)

let m = M.(empty |> add "one" 1 |> add "two" 2 |> add "three" 3)
let _ = Printf.printf "two = %d\n" (M.find "two" m)
let _ = M.bindings m

(** Exceptions *)

exception Too_big of int

let check n = if n > 100 then raise (Too_big n) else n

let _ =
  try check 200 with
  | Too_big n ->
      Printf.printf "too big: %d\n" n;
      0

(** Lazy values *)

let lazy_fib =
  let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2) in
  lazy (fib 30)

let _ = Printf.printf "not forced yet\n"
let _ = Lazy.force lazy_fib
