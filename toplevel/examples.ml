(** First *)
let x = 10+10
let y = x * 3
let c = String.make x 'a'
let sin1 = sin 1.
let rec fact n = if n = 0 then 1. else float n *. fact (n - 1)
let _ = Printf.printf "fact 20 = %f\n" (fact 20)
let _ = "abc" < "def"

(** Recusive module *)
module rec Odd : sig
  val odd : int -> bool
end = struct
  let odd x = if x = 0 then true else Even.even (pred x)
end and Even : sig
  val even : int -> bool
end = struct
  let even x = if x = 0 then false else Odd.odd (pred x)
end
