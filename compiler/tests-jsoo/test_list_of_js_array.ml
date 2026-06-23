let%expect_test "caml_list_of_js_array" =
  (* Round-trip: OCaml list -> JS array -> OCaml list.
     Tests that caml_list_of_js_array preserves element order. *)
  let original = [ 1; 2; 3; 4; 5 ] in
  let js_arr = Jsoo_runtime.For_compatibility_only.caml_list_to_js_array original in
  let result = Jsoo_runtime.For_compatibility_only.caml_list_of_js_array js_arr in
  List.iter (fun x -> Printf.printf "%d " x) result;
  print_newline ();
  Printf.printf "equal: %b\n" (original = result);
  [%expect {|
    1 2 3 4 5
    equal: true
    |}]
