let delim = Str.regexp_string "│"

let geometric_mean l =
  exp (List.fold_left ( +. ) 0. (List.map log l) /. float (List.length l))

let parse s =
  Scanf.sscanf (String.trim s) "%f%s"
  @@ fun f u ->
  match u with
  | "ns" -> f *. 1e-6
  | "us" -> f *. 1e-3
  | "ms" -> f
  | "s" -> f *. 1e3
  | _ -> assert false

let () =
  let measures = ref [] in
  (try
     while true do
       let l = read_line () in
       if String.starts_with ~prefix:"├" l
       then
         let l = read_line () |> Str.split delim |> List.tl |> List.map parse in
         measures := l @ !measures
     done
   with End_of_file -> ());
  Format.printf
    {|{ "name": "%s",
  "results":
    [ { "name": "Partial Render Table",
        "metrics":
          [ { "name": "Execution time (geometric mean)",
              "units": "ms",
               "value": %f } ] } ] }@.|}
    (String.capitalize_ascii Sys.argv.(1))
    (geometric_mean !measures)
