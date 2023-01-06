open Js_of_ocaml_compiler
open Stdlib

let normalize_path s =
  let s =
    String.map
      ~f:(function
        | '\\' -> '/' (* Normalize windows path for the tests *)
        | x -> x)
      s
  in
  Filename.basename s

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

let extract_sourcemap lines =
  let lines =
    List.filter_map lines ~f:(String.drop_prefix ~prefix:"//# sourceMappingURL=")
  in
  match lines with
  | [ line ] ->
      let content =
        match String.drop_prefix ~prefix:"data:application/json;base64," line with
        | None -> String.concat ~sep:"\n" (input_lines line)
        | Some base64 -> Base64.decode_exn base64
      in
      Some (Source_map_io.of_string content)
  | _ -> None

let print_mapping lines (sm : Source_map.t) =
  let lines = Array.of_list lines in
  let sources = Array.of_list sm.sources in
  let _names = Array.of_list sm.names in
  List.iter sm.mappings ~f:(fun (m : Source_map.map) ->
      let file = function
        | -1 -> "null"
        | n -> normalize_path sources.(n)
      in
      let mark col line =
        let len = String.length line in
        if col > len
        then line ^ String.init (col - len) ~f:(fun _ -> ' ') ^ "<>"
        else
          String.sub line ~pos:0 ~len:col
          ^ "<>"
          ^ String.sub line ~pos:col ~len:(len - col)
      in
      if match file m.ori_source with
         | "a.ml" | "b.ml" | "c.ml" | "d.ml" -> true
         | _ -> false
      then
        Printf.printf
          "%s:%d:%d -> %d:%s\n"
          (file m.ori_source)
          m.ori_line
          m.ori_col
          m.gen_col
          (mark m.gen_col lines.(m.gen_line - 1)))

let files = Sys.argv |> Array.to_list |> List.tl

let () =
  List.iter files ~f:(fun f ->
      let lines = input_lines f in
      match extract_sourcemap lines with
      | None -> Printf.printf "not sourcemap for %s\n" f
      | Some sm ->
          Printf.printf "sourcemap for %s\n" f;
          print_mapping lines sm)
