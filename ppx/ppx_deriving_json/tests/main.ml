let _ = Deriving_Json.read
let () = Expect_test_404.run (fun _ -> Ppx_deriving.mapper)
