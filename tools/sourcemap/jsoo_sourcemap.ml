open Js_of_ocaml_compiler.Stdlib

let input_lines file =
  let rec loop acc ic =
    match input_line ic with
    | line -> loop (line :: acc) ic
    | exception End_of_file -> List.rev acc
  in
  let ic = open_in file in
  let lines = loop [] ic in
  close_in ic;
  lines

let () =
  let file = Sys.argv.(1) in
  let lines =
    input_lines file
    |> List.filter_map ~f:(String.drop_prefix ~prefix:"//# sourceMappingURL=")
  in
  let content =
    match lines with
    | [ line ] -> (
        match String.drop_prefix ~prefix:"data:application/json;base64," line with
        | None -> String.concat ~sep:"\n" (input_lines line)
        | Some base64 -> Js_of_ocaml_compiler.Base64.decode_exn base64)
    | _ -> failwith "unable to find sourcemap"
  in
  let sm = Js_of_ocaml_compiler.Source_map_io.of_string content in
  print_endline (Js_of_ocaml_compiler.Source_map_io.to_string sm)
