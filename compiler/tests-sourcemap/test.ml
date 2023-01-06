exception Int of int

let () = raise (Int (Testlib.B.f 2))
