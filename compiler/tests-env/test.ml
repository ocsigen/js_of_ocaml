let test x =
  match Sys.getenv x with
  | exception Not_found -> print_endline (x ^ " not found")
  | v -> print_endline (x ^ " = " ^ v)

let () =
  test "JSOO_A";
  test "JSOO_B";
  test "JSOO_C";
  test "JSOO_D"
