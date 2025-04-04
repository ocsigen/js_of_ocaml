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
  let l = Hashtbl.fold (fun nm v rem -> (nm, v) :: rem) times [] in
  let l = List.filter (fun (_, v) -> v > 0.2) l in
  let l = List.map (fun (nm, v) -> "Compilation phases/" ^ nm, "s", v) l in
  let l' =
    Scanf.sscanf !last_line "%f:%f %f" (fun m s mem ->
        [ "Compilation time", "s", (m *. 60.) +. s
        ; "Compilation memory usage", "KiB", mem
        ])
  in
  Format.printf
    {|{ "name": "%s",
  "results":
    [ { "name": "%s",
        "metrics":@.|}
    (String.capitalize_ascii Sys.argv.(1))
    Sys.argv.(2);
  Format.printf "          [ @[";
  List.iteri
    (fun i (nm, u, v) ->
      if i > 0 then Format.printf ",@ ";
      Format.printf {|{ "name": "%s", "units": "%s", "value": %f }|} nm u v)
    (l' @ l);
  Format.printf "@] ] } ] }@."
