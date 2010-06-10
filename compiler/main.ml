
let debug = Util.debug "main"

let f js_files input_file output_file =
  List.iter Linker.add_file js_files;

  let p =
    match input_file with
      None ->
        Parse.f stdin
    | Some f ->
        let ch = open_in f in
        let p = Parse.f ch in
        close_in ch;
        p
  in
if debug () then Format.eprintf "Tail-call optimization...@.";
  let p = Tailcall.f p in

if debug () then Format.eprintf "Variable passing simplification...@.";
  let p = Phisimpl.f p in

if debug () then Code.print_program (fun _ _ -> "") p;

if debug () then Format.eprintf "Data flow...@.";
  let p = Flow.f p in
if debug () then Format.eprintf "Dead-code...@.";
  let (p, live_vars) = Deadcode.f p in

if debug () then Format.eprintf "Inlining...@.";
  let p = Inline.f p live_vars in
if debug () then Format.eprintf "Dead-code...@.";
  let (p, live_vars) = Deadcode.f p in


if debug () then Code.print_program (fun _ _ -> "") p;
if debug () then Format.eprintf "Data flow...@.";
  let p = Flow.f p in
if debug () then Format.eprintf "Dead-code...@.";
  let (p, live_vars) = Deadcode.f p in

if debug () then Format.eprintf "Inlining...@.";
  let p = Inline.f p live_vars in
if debug () then Format.eprintf "Dead-code...@.";
  let (p, live_vars) = Deadcode.f p in


if debug () then Format.eprintf "Variable passing simplification...@.";
  let p = Phisimpl.f p in

if debug () then Format.eprintf "Data flow...@.";
  let p = Flow.f p in
if debug () then Format.eprintf "Dead-code...@.";
  let (p, live_vars) = Deadcode.f p in

if debug () then Code.print_program (fun _ _ -> "") p;
  match output_file with
    None ->
      Format.printf "%a" (fun ch -> Generate.f ch p) live_vars
  | Some f ->
      let ch = open_out f in
      Format.fprintf (Format.formatter_of_out_channel ch)
        "%a" (fun ch -> Generate.f ch p) live_vars;
      close_out ch

let _ =
  Findlib.init ();
  let js_files = ref [] in
  let output_file = ref None in
  let input_file = ref None in
  let no_runtime = ref false in
  let options =
    [("-debug", Arg.String Util.set_debug, "<name> debug module <name>");
     ("-pretty", Arg.Unit Generate.set_pretty, " pretty print the output");
     ("-noinline", Arg.Unit Inline.disable_inlining, " disable inlining");
     ("-noruntime", Arg.Unit (fun () -> no_runtime := true),
      " do not include the standard runtime");
     ("-o", Arg.String (fun s -> output_file := Some s),
      "<file> set output file name to <file>")]
  in
  Arg.parse (Arg.align options)
    (fun s ->
       if Filename.check_suffix s ".js" then
         js_files := s :: !js_files
       else
         input_file := Some s)
    (Format.sprintf "Usage: %s [options]" Sys.argv.(0));
  let runtime =
    if !no_runtime then [] else
    try
      [Filename.concat (Findlib.package_directory "js") "runtime.js"]
    with Findlib.No_such_package _ ->
      Format.eprintf "Runtime file 'runtime.js' not found."; exit 1
  in
  let chop_extension s =
    try Filename.chop_extension s with Invalid_argument _ -> s in
  f (runtime @ List.rev !js_files) !input_file
    (match !output_file with
       Some _ -> !output_file
     | None   -> Util.opt_map (fun s -> chop_extension s ^ ".js") !input_file)
