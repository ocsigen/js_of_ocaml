external test_stubs : unit -> int = "test_lib_jsoo_a"

let () = print_endline "This is A"

let test () =
  let i = test_stubs () in
  flush_all ();
  Printf.printf "returned %d\n" i
