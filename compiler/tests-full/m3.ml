let f () = if Random.int 2 > 1 then M1.f else fun () () -> M2.f ()

let x = f () () ()
