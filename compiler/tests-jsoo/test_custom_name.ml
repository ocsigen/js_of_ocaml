let%expect_test _ =
  print_endline (Jsoo_runtime.Js.custom_identifier (Obj.repr 1L));
  [%expect {| _j |}];

  print_endline
    (Jsoo_runtime.Js.custom_identifier
       (Obj.repr (Bigarray.Array1.create Int16_unsigned C_layout 3)));
  [%expect {| _bigarr02 |}];

  (match Sys.backend_type, Jsoo_runtime.Js.custom_identifier (Obj.repr 1l) with
  | Other "js_of_ocaml", "" -> print_endline "_i"
  | Other "wasm_of_ocaml", x -> print_endline x
  | _ -> assert false);
  [%expect {| _i |}];

  (match Sys.backend_type, Jsoo_runtime.Js.custom_identifier (Obj.repr 1n) with
  | Other "js_of_ocaml", "" -> print_endline "_n"
  | Other "wasm_of_ocaml", x -> print_endline x
  | _ -> assert false);
  [%expect {| _n |}];

  print_endline (Jsoo_runtime.Js.custom_identifier (Obj.repr "test"));
  [%expect {| |}]
