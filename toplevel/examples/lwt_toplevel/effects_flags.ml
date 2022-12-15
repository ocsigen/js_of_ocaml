let enable b n =
  let f = if b then "--enable" else "--disable" in
  Printf.printf "%s=%s\n" f n

let () =
  let major = String.split_on_char '.' Sys.ocaml_version |> List.hd |> int_of_string in
  let has_effect = major >= 5 in
  enable has_effect "effects"
