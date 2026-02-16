let () =
  let major = String.split_on_char '.' Sys.ocaml_version |> List.hd |> int_of_string in
  let effects_flags l =
    match l, major >= 5 with
    | [ "with-effects" ], true -> [ "--effects"; "cps" ]
    | _ -> [ "--effects"; "disabled" ]
  in
  match Sys.argv |> Array.to_list |> List.tl with
  | rest -> List.iter print_endline (effects_flags rest)
