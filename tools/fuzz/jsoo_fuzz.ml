(* Differential fuzzers for js_of_ocaml and wasm_of_ocaml. See README.md. *)

let usage =
  {|usage: jsoo_fuzz CAMPAIGN [OPTIONS]
       jsoo_fuzz corpus [OPTIONS] BYTECODE...
       jsoo_fuzz gen KIND SEED [DIR]
       jsoo_fuzz reduce [OPTIONS] FILE.ml

Campaigns:
  whole      generated OCaml programs, compared with ocamlrun
  numeric    generated numeric kernels, compared with ocamlrun
  separate   generated programs split into modules, compiled whole and separately
  prims      runtime primitives on random inputs, compared with ocamlrun
  interop    programs using the js_of_ocaml library, across configurations
  runtime    programs with user runtime files, compared with an OCaml model
  corpus     existing bytecode programs with random flags, compared with
             the default flags

gen prints a generated program (KIND: whole, numeric, prims, interop) or writes the
files of a runtime program in DIR (KIND: runtime).

Options:|}

let () =
  let args = Array.to_list Sys.argv in
  match args with
  | _ :: "gen" :: kind :: seed :: rest -> (
      let seed = int_of_string seed in
      match kind, rest with
      | "whole", [] -> print_string (Gen_ocaml.program (Gen_ocaml.parts ~seed ~size:4))
      | "numeric", [] -> print_string (Gen_numeric.program seed)
      | "prims", [] -> print_string (Gen_prims.program seed)
      | "interop", [] -> print_string (Gen_interop.program seed)
      | "runtime", [ dir ] ->
          let p = Gen_runtime.generate seed in
          Harness.mkdir_p dir;
          List.iter
            (fun (f, s) -> Harness.write_file (Filename.concat dir f) s)
            (p.runtime_files @ [ "prims.wat", p.wat; "t.ml", p.main; "ref.ml", p.oracle ]);
          Printf.printf
            "runtime files (load order): %s\nlines meaningful in Wasm: %s\n"
            (String.concat " " (List.map fst p.runtime_files))
            (String.concat " " (List.map string_of_int p.wasm_lines))
      | _ ->
          prerr_endline usage;
          exit 2)
  | _ :: campaign :: _ ->
      let jsoo = ref "js_of_ocaml" in
      let wasmoo = ref (Some "wasm_of_ocaml") in
      let out = ref "_fuzz" in
      let start = ref 1 in
      let count = ref 100 in
      let jobs = ref 3 in
      let anon = ref [] in
      let flags = ref "" in
      let pattern = ref "" in
      let ocamlc_flags = ref "" in
      let words s = List.filter (( <> ) "") (String.split_on_char ' ' s) in
      let js_flags = ref [] and wasm_flags = ref [] in
      let js_options = ref [] and wasm_options = ref [] in
      let absolute f =
        if Filename.is_relative f && Filename.basename f <> f
        then Filename.concat (Sys.getcwd ()) f
        else f
      in
      let spec =
        [ ( "--js-of-ocaml"
          , Arg.String (fun s -> jsoo := absolute s)
          , "PATH js_of_ocaml compiler" )
        ; ( "--wasm-of-ocaml"
          , Arg.String (fun s -> wasmoo := Some (absolute s))
          , "PATH wasm_of_ocaml compiler" )
        ; "--no-wasm", Arg.Unit (fun () -> wasmoo := None), " Only test js_of_ocaml"
        ; ( "--no-js"
          , Arg.Clear Harness.test_js
          , " Only test wasm_of_ocaml (whole, separate, prims and corpus campaigns)" )
        ; ( "--js-flags"
          , Arg.String (fun s -> js_flags := words s)
          , "FLAGS Flags added to every js_of_ocaml compilation" )
        ; ( "--wasm-flags"
          , Arg.String (fun s -> wasm_flags := words s)
          , "FLAGS Flags added to every wasm_of_ocaml compilation" )
        ; ( "--js-options"
          , Arg.String (fun s -> js_options := words s)
          , "NAMES Additional js_of_ocaml options disabled at random" )
        ; ( "--wasm-options"
          , Arg.String (fun s -> wasm_options := words s)
          , "NAMES Additional wasm_of_ocaml options disabled at random" )
        ; ( "-o"
          , Arg.String (fun s -> out := absolute s)
          , "DIR Output directory (default _fuzz)" )
        ; "--seed", Arg.Set_int start, "N First seed (default 1)"
        ; "-n", Arg.Set_int count, "N Number of seeds (default 100)"
        ; "-j", Arg.Set_int jobs, "N Number of parallel jobs (default 3)"
        ; "--flags", Arg.Set_string flags, "FLAGS (reduce) compiler flags"
        ; "--pattern", Arg.Set_string pattern, "TEXT (reduce) the failure to preserve"
        ; "--ocamlc-flags", Arg.Set_string ocamlc_flags, "FLAGS (reduce) ocamlc flags"
        ]
      in
      Arg.current := 1;
      Arg.parse spec (fun s -> anon := absolute s :: !anon) usage;
      let anon = List.rev !anon in
      (Harness.extra_flags :=
         function
         | Js -> !js_flags
         | Wasm -> !wasm_flags);
      (Harness.extra_options :=
         function
         | Js -> !js_options
         | Wasm -> !wasm_options);
      if campaign = "reduce"
      then (
        match anon with
        | [ file ] ->
            Campaigns.reduce
              ~file
              ~compiler:!jsoo
              ~flags:(words !flags)
              ~pattern:!pattern
              ~ocamlc_flags:(words !ocamlc_flags)
        | _ ->
            prerr_endline "reduce: expected one OCaml file";
            exit 2)
      else
        let interp =
          List.filter
            (fun t ->
              match t, !wasmoo with
              | Harness.Js, _ -> Harness.supports_interp !jsoo
              | Wasm, Some c -> Harness.supports_interp c
              | Wasm, None -> false)
            [ Harness.Js; Wasm ]
        in
        let config =
          { Harness.jsoo = !jsoo
          ; wasmoo = !wasmoo
          ; out = !out
          ; start = !start
          ; count = !count
          ; jobs = max 1 !jobs
          ; interp
          }
        in
        Printf.printf
          "%s: seeds %d to %d, reports in %s%s\n%!"
          campaign
          config.start
          (config.start + config.count - 1)
          config.out
          (if interp = [] then "" else " (with the IR interpreter)");
        let job =
          match campaign, anon with
          | "whole", [] -> Campaigns.whole config
          | "numeric", [] -> Campaigns.numeric config
          | "separate", [] -> Campaigns.separate config
          | "prims", [] -> Campaigns.prims config
          | "interop", [] -> Campaigns.interop config
          | "runtime", [] -> Campaigns.runtime config
          | "corpus", _ :: _ ->
              fun seed -> [ Campaigns.corpus config ~programs:anon seed ]
          | _ ->
              Arg.usage spec usage;
              exit 2
        in
        Harness.campaign config job
  | _ ->
      prerr_endline usage;
      exit 2
