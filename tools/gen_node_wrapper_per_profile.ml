let profile = Sys.argv.(1)

let () =
  match profile with
  | "with-effects-double-translation" ->
      (* We somehow need a larger stack with --effects=double-translation. *)
      print_endline {|
let args = [ "--stack-size=3000" ]
|}
  | _ -> print_endline {|
let args = [ ]
|}
