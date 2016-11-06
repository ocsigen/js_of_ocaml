
let f {LinkerArg.output_file; source_map; resolve_sourcemap_url; js_files; } =
  let output =
    match output_file with
    | None -> stdout
    | Some file -> open_out_bin file
  in
  Link_js.link ~output ~files:js_files ~source_map ~resolve_sourcemap_url;
  match output_file with
  | None -> ()
  | Some _ -> close_out output

let main =
  Cmdliner.Term.(pure f $ LinkerArg.options),
  LinkerArg.info

let _ =
  Util.Timer.init Sys.time;
  try Cmdliner.Term.eval ~catch:false ~argv:(Util.normalize_argv ~warn_:true Sys.argv) main with
  | (Match_failure _ | Assert_failure _ | Not_found) as exc ->
    let backtrace = Printexc.get_backtrace () in
    Format.eprintf
      "%s: You found a bug. \
       Please report it at https://github.com/ocsigen/js_of_ocaml/issues :@."
      Sys.argv.(0);
    Format.eprintf "Error: %s@." (Printexc.to_string exc);
    prerr_string backtrace;
    exit 1
  | Failure s ->
    Format.eprintf "%s: Error: %s@." Sys.argv.(0) s;
    exit 1
  | exc ->
    let backtrace = Printexc.get_backtrace () in
    Format.eprintf "%s: Error: %s@." Sys.argv.(0) (Printexc.to_string exc);
    prerr_string backtrace;
    exit 1
