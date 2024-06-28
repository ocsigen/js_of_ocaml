type lst =
  | Cons of lst * int
  | Nil

let rec make n = if n = 0 then Nil else Cons (make (n - 1), n)

let () = assert (make 10 = make 10)
