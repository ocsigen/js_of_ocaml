
let debug = true

let _ =
  let p = Parse.f stdin in

(*
  Code.print_program (fun _ _ -> "") p;
*)

if debug then Format.eprintf "Tail-call optimization...@.";
  let p = Tailcall.f p in

if debug then Format.eprintf "Variable passing simplification...@.";
  let p = Phisimpl.f p in

if debug then Format.eprintf "Data flow...@.";
  let p = Flow.f p in
if debug then Format.eprintf "Dead-code...@.";
  let (p, _) = Deadcode.f p in
if debug then Format.eprintf "Control flow simplifications...@.";
  let p = Control.simpl p in
if debug then Format.eprintf "Dead-code...@.";
  let (p, live_vars) = Deadcode.f p in

(*
Format.eprintf "Data flow...@.";
  let p = Flow.f p in
Format.eprintf "Dead-code...@.";
  let (p, _) = Deadcode.f p in
*)

if debug then Code.print_program (fun _ _ -> "") p;

  Generate.f p live_vars
