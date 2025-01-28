(* TEST *)

let _ =
  (match Sys.backend_type with
  | Other _ ->
  begin match Obj.new_block 255 1 with
  | v -> ()
  | exception (Invalid_argument _) -> failwith "jsoo/wasm has been updated. please remove"
  end;

  begin match Obj.new_block 252 0 with
  | v -> ()
  | exception (Invalid_argument _) -> failwith "jsoo/wasm has been updated. please remove"
  end
  | Native | Bytecode ->
  begin match Obj.new_block 255 1 with
  | v -> failwith "Expected failure for custom block"
  | exception (Invalid_argument _) -> ()
  end;

  begin match Obj.new_block 252 0 with
  | v -> failwith "Expected failure for zero length string block"
  | exception (Invalid_argument _) -> ()
  end);

  print_endline "OK"
