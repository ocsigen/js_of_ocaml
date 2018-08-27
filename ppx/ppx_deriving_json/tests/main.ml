let _ = Deriving_Json.read
let () = Toplevel_expect_test.run (fun _ -> Ppx_deriving.mapper)
