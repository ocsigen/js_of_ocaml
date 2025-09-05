let compiler = Sys.argv.(1)

let benchmark_name = Sys.argv.(2)

let output = Sys.argv.(3)

let time_re = Str.regexp "^  \\([^ ].*\\): \\([0-9.]+\\)$"

let () =
  let times = Hashtbl.create 16 in
  let last_line = ref "" in
  (try
     while true do
       let l = read_line () in
       (if Str.string_match time_re l 0
        then
          let nm = Str.matched_group 1 l in
          let t = float_of_string (Str.matched_group 2 l) in
          Hashtbl.replace times nm (t +. try Hashtbl.find times nm with Not_found -> 0.));
       last_line := l
     done
   with End_of_file -> ());
  let file_size =
    let file =
      match compiler with
      | "js_of_ocaml" -> output
      | "wasm_of_ocaml" ->
          let dir = Filename.chop_suffix output ".js" ^ ".assets" in
          let contents = Sys.readdir dir in
          let code =
            Array.find_opt (fun nm -> Filename.check_suffix nm ".wasm") contents
          in
          Filename.concat dir (Option.get code)
      | _ -> assert false
    in
    In_channel.(with_open_bin file length)
  in
  let l = Hashtbl.fold (fun nm v rem -> (nm, v) :: rem) times [] in
  let l = List.filter (fun (_, v) -> v > 0.2) l in
  let l = List.map (fun (nm, v) -> "Compilation phases/" ^ nm, "s", v) l in
  let l' =
    Scanf.sscanf !last_line "%f:%f %f" (fun m s mem ->
        [ "Compilation time", "s", (m *. 60.) +. s
        ; "Compilation memory usage", "KiB", mem
        ; "Code size", "KiB", float (Int64.to_int file_size / 1024)
        ])
  in
  Format.printf
    {|{ "name": "%s",
  "results":
    [ { "name": "%s",
        "metrics":@.|}
    (String.capitalize_ascii compiler)
    benchmark_name;
  Format.printf "          [ @[";
  List.iteri
    (fun i (nm, u, v) ->
      if i > 0 then Format.printf ",@ ";
      Format.printf {|{ "name": "%s", "units": "%s", "value": %f }|} nm u v)
    (l' @ l);
  Format.printf "@] ] } ] }@."
