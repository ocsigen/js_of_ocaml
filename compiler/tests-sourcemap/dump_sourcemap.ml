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

let print_mapping lines ?(line_offset = 0) (sm : Source_map.Standard.t) =
  let lines = Array.of_list lines in
  let sources = Array.of_list sm.sources in
  let _names = Array.of_list sm.names in
  let mappings = Source_map.Mappings.decode_exn sm.mappings in
  List.iter mappings ~f:(fun (m : Source_map.map) ->
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
      match m with
      | Gen _ -> ()
      | Gen_Ori { gen_line; gen_col; ori_line; ori_col; ori_source }
      | Gen_Ori_Name { gen_line; gen_col; ori_line; ori_col; ori_source; ori_name = _ }
        -> (
          match file ori_source with
          | "a.ml" | "b.ml" | "c.ml" | "d.ml" ->
              let root =
                match sm.sourceroot with
                | None -> ""
                | Some root -> root ^ "#"
              in
              Printf.printf
                "%s%s:%d:%d -> %d:%s\n"
                root
                (file ori_source)
                ori_line
                ori_col
                gen_col
                (mark gen_col lines.(gen_line - 1 + line_offset))
          | _ -> ()))

let print_sourcemap lines = function
  | Source_map.Standard sm -> print_mapping lines sm
  | Index l ->
      List.iter
        l.Source_map.Index.sections
        ~f:(fun { Source_map.Index.offset = { gen_line; gen_column }; map } ->
          assert (gen_column = 0);
          print_mapping lines ~line_offset:gen_line map)

let files = Sys.argv |> Array.to_list |> List.tl

let () =
  List.iter files ~f:(fun f ->
      let lines = file_lines_bin f in
      match Source_map.find_in_js_file f with
      | None -> Printf.printf "not sourcemap for %s\n" f
      | Some sm ->
          Printf.printf "sourcemap for %s\n" f;
          print_sourcemap lines sm)
