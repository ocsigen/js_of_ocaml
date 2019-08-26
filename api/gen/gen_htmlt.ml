let f ~input_file ~output_file =
  try
    let open Soup.Infix in
    let input = Soup.read_file input_file |> Soup.parse in
    let htmlt = input $ ".content" in
    let out = Soup.pretty_print htmlt in
    match output_file with
    | "-" -> print_endline out
    | fn ->
        let oc = open_out fn in
        output_string oc out;
        close_out oc
  with e ->
    Format.eprintf "Cannot post process %s\n%!" input_file;
    Format.eprintf "%s\n%!" (Printexc.to_string e)

let () =
  let open Cmdliner in
  let info = Term.info "post_process_api" in
  let output_file =
    let doc = "Output file." in
    Arg.(required & opt (some string) None & info ["o"; "output"] ~docv:"OUTPUT" ~doc)
  in
  let input_file =
    let doc = "Html file to post process." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"html" ~doc)
  in
  let term =
    Cmdliner.Term.(
      pure (fun input_file output_file -> f ~input_file ~output_file)
      $ input_file
      $ output_file)
  in
  Cmdliner.Term.exit (Cmdliner.Term.eval (term, info))
