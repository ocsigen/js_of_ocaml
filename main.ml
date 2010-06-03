
let debug = Util.debug "main"

let f file =

Linker.add_file "/home/vouillon/misc/ocaml2js/runtime/runtime.js";

  let p =
    match file with
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
  match file with
    None ->
      Format.printf "%a" (fun ch -> Generate.f ch p) live_vars
  | Some f ->
      let ch = open_out (f ^ ".js") in
      Format.fprintf (Format.formatter_of_out_channel ch)
        "%a" (fun ch -> Generate.f ch p) live_vars;
      close_out ch

let _ =
  let file = ref None in
  Arg.parse
    [("-debug", Arg.String Util.set_debug, "debug module xxx");
     ("-pretty", Arg.Unit Generate.set_pretty, "pretty print the output");
     ("-noinline", Arg.Unit Inline.disable_inlining, "disable inlining")]
    (fun s -> file := Some s)
    (Format.sprintf "Usage: %s [options]" Sys.argv.(0));
  f !file
