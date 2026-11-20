exception Int of int

let () = raise (Int (Testlib.B.f (Array.length Sys.argv)))
