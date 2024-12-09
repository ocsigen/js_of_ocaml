let enable b n =
  let f = if b then "--enable" else "--disable" in
  [ f; n ]

let () =
  let major = String.split_on_char '.' Sys.ocaml_version |> List.hd |> int_of_string in
  let has_effect l =
    match l with
    | [ "with-effects" ] -> major >= 5
    | _ -> false
  in
  let aux l = enable (has_effect l) "effects" in
  match Sys.argv |> Array.to_list |> List.tl with
  | "txt" :: rest -> List.iter print_endline (aux rest)
  | "sexp" :: rest ->
      print_endline "(";
      List.iter print_endline (aux rest);
      print_endline ")"
  | _ -> assert false
