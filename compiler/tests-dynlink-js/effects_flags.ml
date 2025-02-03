let () =
  let major = String.split_on_char '.' Sys.ocaml_version |> List.hd |> int_of_string in
  let effects_flags l =
    match l, major >= 5 with
    | [ "with-effects-double-translation" ], true -> [ "--effects"; "double-translation" ]
    | [ "with-effects" ], true -> [ "--enable"; "effects" ]
    | _, true -> [ "--disable"; "effects" ]
    | _, false -> [ "--disable"; "effects" ]
  in
  match Sys.argv |> Array.to_list |> List.tl with
  | "txt" :: rest -> List.iter print_endline (effects_flags rest)
  | "sexp" :: rest ->
      print_endline "(";
      List.iter print_endline (effects_flags rest);
      print_endline ")"
  | _ -> assert false
