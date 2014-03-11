(** First *)
let x = 10+10
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
  | x -> odd (x-1)
and odd n =
  match n with
  | 0 -> false
  | x -> even (x-1)

(** Recusive module *)
module rec Odd : sig
  val odd : int -> bool
end = struct
  let odd x = if x = 0 then false else Even.even (pred x)
end and Even : sig
  val even : int -> bool
end = struct
  let even x = if x = 0 then true else Odd.odd (pred x)
end
