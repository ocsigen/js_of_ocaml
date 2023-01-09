let enable b n =
  let f = if b then "--enable" else "--disable" in
  [ f; n ]

let () =
  let major = String.split_on_char '.' Sys.ocaml_version |> List.hd |> int_of_string in
  let has_effect = major >= 5 in
  let l = enable has_effect "effects" in
  match Sys.argv |> Array.to_list |> List.tl with
  | "txt" :: [] -> List.iter print_endline l
  | "sexp" :: [] ->
      print_endline "(";
      List.iter print_endline l;
      print_endline ")"
  | _ -> assert false
