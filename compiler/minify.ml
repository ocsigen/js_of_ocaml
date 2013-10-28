

let error k = Format.ksprintf (fun s ->
    Printf.eprintf "%s\n" s;
    exit 1
  ) k


let read_file f =
  let c = open_in f in
  let out = Buffer.create 1024 in
  let buf = String.create 1024 in
  try
    while true; do
      let i = input c buf 0 1023 in
      if i <> 0
      then Buffer.add_substring out buf 0 i
      else raise Not_found
    done;
    assert false;
  with Not_found -> Buffer.contents out

let _ =
  Util.Timer.init Unix.gettimeofday;
  let js_files = ref [] in
  let output_file = ref None in
  let options =
    [("-debug", Arg.String Option.Debug.set, "<name> debug module <name>");
     ("-disable",
      Arg.String Option.Optim.disable, "<name> disable optimization <name>");
     ("-pretty", Arg.Unit (fun () -> Option.Optim.enable "pretty"), " pretty print the output");
     ("-debuginfo", Arg.Unit (fun () -> Option.Optim.enable "debuginfo"), " output debug info");
     ("-noinline", Arg.Unit (fun () -> Option.Optim.disable "inline"), " disable inlining");
     ("-o", Arg.String (fun s -> output_file := Some s),
      "<file> set output file name to <file>")]
  in
  Arg.parse (Arg.align options)
    (fun s ->
       if Filename.check_suffix s ".js" then
         if Sys.file_exists s
         then js_files := s :: !js_files
         else error "file '%s' do not exist" s
       else
         error "Don't know what to do with '%s'" s
    )
    (Format.sprintf "Usage: %s [options]" Sys.argv.(0));
  let chop_extension s =
    try Filename.chop_extension s with Invalid_argument _ -> s in


  let pp,finalize = match !output_file with
    | Some "-" ->
      Pretty_print.to_out_channel stdout,(fun _ -> ())
    | Some file ->
      let oc = open_out file in
      Pretty_print.to_out_channel oc, (fun _ -> close_out oc)
    | None when List.length !js_files = 1 ->
      let file = chop_extension (List.hd !js_files) ^ ".min.js" in
      let oc = open_out file in
      Pretty_print.to_out_channel oc, (fun _ -> close_out oc)
    | None ->
      error "You must provide an output file" in

  Pretty_print.set_compact pp (not (Option.Optim.pretty ()));

  let p = List.flatten (List.map (fun file ->
    let lex = Parse_js.lexer_from_file file in
    Parse_js.parse lex) !js_files) in

  let p = (new Js_traverse.free true)#program p in
  let to_string = Js_var.program p in
  Js_output.program pp (fun _ -> None) to_string p;


  finalize()
