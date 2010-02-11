let _ =
  let p = Parse.f stdin in

(*
  print_program (fun _ _ -> "") p;
*)
Format.eprintf "Data flow...@.";
  let (p, approx) = Flow.f p in
Format.eprintf "Dead-code...@.";
  let (p, _) = Deadcode.f p in
Format.eprintf "Control flow simplifications...@.";
  let p = Control.simpl p in
Format.eprintf "Dead-code...@.";
  let (p, live_vars) = Deadcode.f p in


(*

  let p = Control.simpl p in

  let p = Flow.f p in
  let p = Deadcode.f p in

  let p = Control.simpl p in
*)
  Code.print_program (fun _ _ -> "") p;

Struct.f p;

Generate.f p live_vars
