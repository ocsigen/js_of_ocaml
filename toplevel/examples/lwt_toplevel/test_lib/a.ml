external test_stubs : unit -> int = "test_lib_jsoo_a"

let () = print_endline "This is A"

let test () =
  let i = test_stubs () in
  flush_all ();
  Printf.printf "returned %d\n" i

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

let f a b = a + b
