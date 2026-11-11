(* The fuzzing campaigns. Each one generates a program per seed, runs it in
   several configurations and compares the outputs with an oracle. *)

open Harness

let sprintf = Printf.sprintf

let section title text = sprintf "--- %s\n%s\n" title (last 3000 text)

let ocamlc_flags st = if Rng.chance st 0.5 then [ "-g" ] else []

(* [--debug interp] checks that no pass changes the behavior of the program,
   when the compiler supports it *)
let interp_flags config target =
  if List.mem target config.interp
  then [ "--debug"; "interp"; "--debug"; "interp-output" ]
  else []

let compile_problem (r : result) =
  if contains r.stderr "changed the behavior"
  then "pass changed behavior"
  else if r.status = Timeout
  then "compile timeout"
  else "compiler error"

(* The interpreter's report on the input program: it ran to completion *)
let interp_ran stderr =
  List.exists
    (fun l ->
      String.starts_with ~prefix:"interp: input:" l
      && (contains l ": finished" || contains l ": exited"))
    (lines stderr)

let short_log stderr =
  String.concat "\n" (List.filter (fun l -> not (contains l ": ok (")) (lines stderr))

let native target effects = target = Wasm && effects = "native"

(****)

(* Whole-program compilation of a generated OCaml program, compared with
   ocamlrun (and with the IR interpreter, when available) *)
let compare_with_ocamlrun config seed st ~src ~uses_effects =
  let ocamlc_flags = ocamlc_flags st in
  with_temp_dir (fun dir ->
      write_file (Filename.concat dir "t.ml") src;
      let r =
        ocamlc ~cwd:dir ([ "-w"; "-a" ] @ ocamlc_flags @ [ "t.ml"; "-o"; "t.byte" ])
      in
      if not (ok r)
      then (
        print_problem ~seed ("generator error: " ^ last 500 r.stderr);
        [ "generator error" ])
      else
        let expected = run ~timeout:30. ~cwd:dir "ocamlrun" [ "t.byte" ] in
        if not (ok expected)
        then [ "ocamlrun failed" ]
        else
          List.map
            (fun target ->
              let effects = random_effects st ~handlers:uses_effects target in
              let flags = ("--effects=" ^ effects) :: random_flags st target in
              let c =
                run
                  ~cwd:dir
                  (compiler config target)
                  (interp_flags config target @ flags @ [ "t.byte"; "-o"; "a.js" ])
              in
              let problem, details =
                if not (ok c)
                then Some (compile_problem c), short_log c.stderr
                else
                  let interp_problem =
                    List.mem target config.interp
                    && interp_ran c.stderr
                    && c.stdout <> expected.stdout
                  in
                  let n = node ~native_effects:(native target effects) ~cwd:dir "a.js" in
                  let problem =
                    if interp_problem
                    then Some "interpreter differs from ocamlrun"
                    else if n.status = Timeout
                    then Some "node timeout"
                    else if n.stdout <> expected.stdout
                    then Some "node differs from ocamlrun"
                    else None
                  in
                  ( problem
                  , short_log c.stderr
                    ^ section "expected" expected.stdout
                    ^ section "interpreter" c.stdout
                    ^ section "node" n.stdout
                    ^ section "node stderr" n.stderr )
              in
              match problem with
              | None -> target_name target ^ " ok"
              | Some problem ->
                  let name = target_name target in
                  save_report
                    config
                    ~seed
                    ~files:[ "t.ml", src ]
                    ~report:
                      (sprintf
                         "%s (%s)\nocamlc %s\nflags: %s\n\n%s"
                         problem
                         name
                         (String.concat " " ocamlc_flags)
                         (String.concat " " flags)
                         details);
                  print_problem
                    ~seed
                    (sprintf "%s: %s %s" problem name (String.concat " " flags));
                  name ^ " " ^ problem)
            (targets config))

let whole config seed =
  let st = Rng.make seed in
  let p = Gen_ocaml.parts ~seed ~size:(Rng.pick st [ 3; 4; 5 ]) in
  compare_with_ocamlrun
    config
    seed
    st
    ~src:(Gen_ocaml.program p)
    ~uses_effects:p.uses_effects

(* Numeric kernels, for the placement of conversions in wasm_of_ocaml *)
let numeric config seed =
  let st = Rng.make seed in
  compare_with_ocamlrun config seed st ~src:(Gen_numeric.program seed) ~uses_effects:false

(****)

(* Generated programs split into several modules, compiled whole and
   separately *)
let separate config seed =
  let st = Rng.make seed in
  let p = Gen_ocaml.parts ~seed ~size:(Rng.pick st [ 3; 4; 5 ]) in
  let nmods = min (Rng.range st 1 3) (max 1 (List.length p.items)) in
  let groups =
    (* Split the toplevel items into [nmods] consecutive groups *)
    let n = List.length p.items in
    let cuts =
      List.sort_uniq compare (List.init (nmods - 1) (fun _ -> Rng.range st 0 n))
    in
    let rec split i items cuts acc cur =
      match items, cuts with
      | _, c :: cuts when c = i -> split i items cuts (List.rev cur :: acc) []
      | x :: items, _ -> split (i + 1) items cuts acc (x :: cur)
      | [], _ -> List.rev (List.rev cur :: acc)
    in
    split 0 p.items cuts [] []
  in
  (* Not [M<n>]: the generator names its modules this way *)
  let modules =
    ("prelude", p.prelude)
    :: List.mapi (fun i grp -> sprintf "part%d" i, String.concat "\n\n" grp) groups
    @ [ "main", String.concat "\n\n" p.tests ]
  in
  let sources =
    List.mapi
      (fun i (name, body) ->
        let opens =
          String.concat
            ""
            (List.map
               (fun (n, _) -> sprintf "open %s\n" (String.capitalize_ascii n))
               (List.filteri (fun j _ -> j < i) modules))
        in
        name, opens ^ body ^ "\n")
      modules
  in
  let ocamlc_flags = ocamlc_flags st in
  with_temp_dir (fun dir ->
      List.iter
        (fun (name, src) -> write_file (Filename.concat dir (name ^ ".ml")) src)
        sources;
      let r =
        ocamlc
          ~cwd:dir
          ([ "-w"; "-a" ]
          @ ocamlc_flags
          @ [ "-c" ]
          @ List.map (fun (n, _) -> n ^ ".ml") sources)
      in
      let r =
        if ok r
        then
          ocamlc
            ~cwd:dir
            (ocamlc_flags
            @ List.map (fun (n, _) -> n ^ ".cmo") sources
            @ [ "-o"; "t.byte" ])
        else r
      in
      if not (ok r)
      then (
        print_problem ~seed ("generator error: " ^ last 500 r.stderr);
        [ "generator error" ])
      else
        let expected = run ~timeout:30. ~cwd:dir "ocamlrun" [ "t.byte" ] in
        if not (ok expected)
        then [ "ocamlrun failed" ]
        else
          List.concat_map
            (fun target ->
              let effects =
                match target with
                | Js -> random_effects st ~handlers:p.uses_effects Js
                | Wasm ->
                    (* The standard library is not compiled with [disabled] *)
                    Rng.pick st [ "jspi"; "cps"; "double-translation"; "native" ]
              in
              let flags = ("--effects=" ^ effects) :: random_flags st target in
              let compiler = compiler config target in
              let check kind out =
                let n = node ~native_effects:(native target effects) ~cwd:dir out in
                if n.stdout = expected.stdout
                then sprintf "%s %s ok" (target_name target) kind
                else (
                  save_report
                    config
                    ~seed
                    ~files:(List.map (fun (n, s) -> n ^ ".ml", s) sources)
                    ~report:
                      (sprintf
                         "%s %s: output differs\nocamlc %s\nflags: %s\n\n%s%s%s"
                         (target_name target)
                         kind
                         (String.concat " " ocamlc_flags)
                         (String.concat " " flags)
                         (section "expected" expected.stdout)
                         (section "node" n.stdout)
                         (section "node stderr" n.stderr));
                  print_problem
                    ~seed
                    (sprintf
                       "%s %s mismatch %s"
                       (target_name target)
                       kind
                       (String.concat " " flags));
                  sprintf "%s %s mismatch" (target_name target) kind)
              in
              let failure kind (r : result) =
                save_report
                  config
                  ~seed
                  ~files:(List.map (fun (n, s) -> n ^ ".ml", s) sources)
                  ~report:
                    (sprintf
                       "%s %s: compilation failed\nflags: %s\n\n%s"
                       (target_name target)
                       kind
                       (String.concat " " flags)
                       (last 3000 r.stderr));
                print_problem
                  ~seed
                  (sprintf "%s %s compile error" (target_name target) kind);
                sprintf "%s %s compile error" (target_name target) kind
              in
              let whole =
                let r = run ~cwd:dir compiler (flags @ [ "t.byte"; "-o"; "w.js" ]) in
                if ok r then check "whole" "w.js" else failure "whole" r
              in
              let runtime, stdlib, std_exit = separate_cache config target effects in
              let ext =
                match target with
                | Js -> ".js"
                | Wasm -> ".wasmo"
              in
              let rec compile_units = function
                | [] -> None
                | (name, _) :: rem ->
                    let args = flags @ [ name ^ ".cmo"; "-o"; name ^ ext ] in
                    let args =
                      match target with
                      | Js -> args
                      | Wasm -> "compile" :: args
                    in
                    let r = run ~cwd:dir compiler args in
                    if ok r then compile_units rem else Some r
              in
              let separate =
                match compile_units sources with
                | Some r -> failure "separate" r
                | None ->
                    let r =
                      run
                        ~cwd:dir
                        compiler
                        ("link"
                         :: runtime
                         :: stdlib
                         :: List.map (fun (n, _) -> n ^ ext) sources
                        @ [ std_exit; "-o"; "s.js" ])
                    in
                    if ok r then check "separate" "s.js" else failure "link" r
              in
              [ whole; separate ])
            (targets config))

(****)

(* Runtime primitives on random inputs, compared with ocamlrun line by
   line *)
let prims config seed =
  let st = Rng.make (seed + 1_000_000) in
  let src = Gen_prims.program seed in
  let marked =
    List.filter_map
      (fun l ->
        if contains l Gen_prims.jsdiff_marker
        then Scanf.sscanf_opt l "let () = t %d " Fun.id
        else None)
      (lines src)
  in
  with_temp_dir (fun dir ->
      write_file (Filename.concat dir "t.ml") src;
      let r = ocamlc ~cwd:dir [ "-w"; "-a"; "t.ml"; "-o"; "t.byte" ] in
      if not (ok r)
      then (
        print_problem ~seed ("generator error: " ^ last 500 r.stderr);
        [ "generator error" ])
      else
        let expected = run ~timeout:30. ~cwd:dir "ocamlrun" [ "t.byte" ] in
        if not (ok expected)
        then [ "ocamlrun failed" ]
        else
          let exp = numbered_lines expected.stdout in
          List.map
            (fun target ->
              let flags =
                random_flags
                  ~exclude:[ "excwrap"; "callgen"; "es6"; "stable_var" ]
                  st
                  target
              in
              let c =
                run ~cwd:dir (compiler config target) (flags @ [ "t.byte"; "-o"; "a.js" ])
              in
              let name = target_name target in
              if not (ok c)
              then (
                save_report
                  config
                  ~seed
                  ~files:[ "t.ml", src ]
                  ~report:
                    (sprintf
                       "%s compilation failed\nflags: %s\n%s"
                       name
                       (String.concat " " flags)
                       (last 3000 c.stderr));
                print_problem ~seed (name ^ " compile error");
                name ^ " compile error")
              else
                let n = node ~cwd:dir "a.js" in
                let got = numbered_lines n.stdout in
                let bad =
                  List.filter
                    (fun (k, v) ->
                      List.assoc_opt k got <> Some v
                      && not (target = Js && List.mem k marked))
                    exp
                in
                if bad = []
                then name ^ " ok"
                else
                  let src_line k =
                    Option.value
                      ~default:"?"
                      (List.find_opt
                         (fun l ->
                           String.starts_with ~prefix:(sprintf "let () = t %d " k) l)
                         (lines src))
                  in
                  save_report
                    config
                    ~seed
                    ~files:[ "t.ml", src ]
                    ~report:
                      (sprintf
                         "%s: %d lines differ\nflags: %s\n\n%s%s"
                         name
                         (List.length bad)
                         (String.concat " " flags)
                         (String.concat
                            ""
                            (List.map
                               (fun (k, v) ->
                                 sprintf
                                   "%s\n  ocamlrun: %s\n  node:     %s\n\n"
                                   (src_line k)
                                   v
                                   (Option.value
                                      ~default:"<missing>"
                                      (List.assoc_opt k got)))
                               bad))
                         (if ok n then "" else section "node stderr" n.stderr));
                  print_problem
                    ~seed
                    (sprintf "%s: %d lines differ" name (List.length bad));
                  name ^ " mismatch")
            (targets config))

(****)

let compare_configs config ~seed ~files ~configs ~expected ~lines_for =
  (* [configs]: (name, target, effects, build steps, output) *)
  List.map
    (fun (name, target, effects, steps, out, dir) ->
      let rec build = function
        | [] -> None
        | (prog, args) :: rem ->
            let r = run ~cwd:dir prog args in
            if ok r then build rem else Some (prog, args, r)
      in
      match build steps with
      | Some (prog, args, r) ->
          let msg =
            sprintf
              "%s: %s %s failed (%s)\n%s"
              name
              prog
              (String.concat " " args)
              (status_to_string r.status)
              (last 3000 r.stderr)
          in
          save_report config ~seed ~files ~report:msg;
          print_problem ~seed (name ^ " compile error");
          target_name target ^ " compile error"
      | None ->
          let n = node ~native_effects:(native target effects) ~cwd:dir out in
          let got = numbered_lines n.stdout in
          let exp = expected name target in
          let keys = lines_for target (List.map fst exp) in
          let bad =
            List.filter_map
              (fun k ->
                let e = List.assoc_opt k exp and g = List.assoc_opt k got in
                if e <> g
                then
                  Some
                    (sprintf
                       "line %d: expected %s, got %s"
                       k
                       (Option.value ~default:"<missing>" e)
                       (Option.value ~default:"<missing>" g))
                else
                  match g with
                  | Some v when contains v "DIFF" -> Some (sprintf "line %d: %s" k v)
                  | _ -> None)
              keys
          in
          if bad = []
          then target_name target ^ " ok"
          else (
            save_report
              config
              ~seed
              ~files
              ~report:
                (sprintf
                   "%s: %s\n%s\n%s"
                   name
                   (String.concat " " (List.concat_map snd steps))
                   (String.concat "\n" bad)
                   (if ok n then "" else section "node stderr" n.stderr));
            print_problem ~seed (sprintf "%s: %d lines differ" name (List.length bad));
            target_name target ^ " mismatch"))
    configs

(* Programs using the js_of_ocaml library. The reference is the JavaScript
   output with optimizations disabled. *)
let interop config seed =
  let st = Rng.make (seed + 2_000_000) in
  let src = Gen_interop.program seed in
  let ocamlc_flags = ocamlc_flags st in
  with_temp_dir (fun dir ->
      write_file (Filename.concat dir "t.ml") src;
      let r =
        ocamlc
          ~cwd:dir
          ([ "-w"; "-a"; "-package"; "js_of_ocaml,js_of_ocaml-ppx"; "-linkpkg" ]
          @ ocamlc_flags
          @ [ "t.ml"; "-o"; "t.byte" ])
      in
      if not (ok r)
      then (
        print_problem ~seed ("generator error: " ^ last 500 r.stderr);
        [ "generator error" ])
      else
        let reference_flags =
          [ "--opt"
          ; "1"
          ; "--disable"
          ; "inline"
          ; "--disable"
          ; "staticeval"
          ; "--disable"
          ; "deadcode"
          ; "--disable"
          ; "globaldeadcode"
          ; "--disable"
          ; "share"
          ; "--disable"
          ; "compact"
          ]
        in
        let config_for i target =
          let effects =
            match target with
            | Js -> Rng.pick st [ "disabled"; "disabled"; "cps"; "double-translation" ]
            | Wasm -> Rng.pick st (effect_modes Wasm)
          in
          (* [--disable excwrap] stops the conversion of JavaScript
             exceptions to [Js_error.Exn] *)
          let flags =
            ("--effects=" ^ effects) :: random_flags ~exclude:[ "excwrap" ] st target
          in
          ( sprintf "%s%d" (target_name target) i
          , target
          , effects
          , [ ( compiler config target
              , flags @ [ "t.byte"; "-o"; sprintf "%s%d.js" (target_name target) i ] )
            ]
          , sprintf "%s%d.js" (target_name target) i
          , dir )
        in
        let reference =
          run ~cwd:dir config.jsoo (reference_flags @ [ "t.byte"; "-o"; "ref.js" ])
        in
        if not (ok reference)
        then (
          save_report
            config
            ~seed
            ~files:[ "t.ml", src ]
            ~report:("reference compilation failed\n" ^ last 3000 reference.stderr);
          print_problem ~seed "reference compile error";
          [ "js compile error" ])
        else
          let ref_out = node ~cwd:dir "ref.js" in
          let exp = numbered_lines ref_out.stdout in
          let ref_diffs = List.filter (fun (_, v) -> contains v "DIFF") exp in
          let configs =
            config_for 0 Js
            :: config_for 1 Js
            ::
            (if Option.is_some config.wasmoo
             then [ config_for 2 Wasm; config_for 3 Wasm ]
             else [])
          in
          let outcomes =
            compare_configs
              config
              ~seed
              ~files:[ "t.ml", src ]
              ~configs
              ~expected:(fun _ _ -> exp)
              ~lines_for:(fun _ keys -> keys)
          in
          if ref_diffs <> [] || not (ok ref_out)
          then (
            save_report
              config
              ~seed
              ~files:[ "t.ml", src ]
              ~report:
                (sprintf
                   "reference: %s\n%s"
                   (String.concat
                      "\n"
                      (List.map (fun (k, v) -> sprintf "line %d: %s" k v) ref_diffs))
                   (if ok ref_out then "" else section "node stderr" ref_out.stderr));
            print_problem ~seed "reference mismatch";
            "js mismatch" :: outcomes)
          else "js ok" :: outcomes)

(****)

(* Programs with user-supplied runtime files, compared with their OCaml
   model *)
let runtime config seed =
  let st = Rng.make (seed + 3_000_000) in
  let p = Gen_runtime.generate seed in
  let files = List.map fst p.runtime_files in
  let ocamlc_flags = ocamlc_flags st in
  with_temp_dir (fun dir ->
      List.iter (fun (f, s) -> write_file (Filename.concat dir f) s) p.runtime_files;
      write_file (Filename.concat dir "prims.wat") p.wat;
      write_file (Filename.concat dir "t.ml") p.main;
      write_file (Filename.concat dir "ref.ml") p.oracle;
      let report_files =
        p.runtime_files @ [ "prims.wat", p.wat; "t.ml", p.main; "ref.ml", p.oracle ]
      in
      let r =
        ocamlc
          ~cwd:dir
          ([ "-w"; "-a"; "-no-check-prims" ] @ ocamlc_flags @ [ "t.ml"; "-o"; "t.byte" ])
      in
      let r =
        if ok r
        then
          ocamlc
            ~cwd:dir
            ([ "-w"; "-a"; "-no-check-prims"; "-c" ] @ ocamlc_flags @ [ "t.ml" ])
        else r
      in
      let r =
        if ok r then ocamlc ~cwd:dir [ "-w"; "-a"; "ref.ml"; "-o"; "ref.byte" ] else r
      in
      if not (ok r)
      then (
        print_problem ~seed ("generator error: " ^ last 500 r.stderr);
        [ "generator error" ])
      else
        let settings = Hashtbl.create 8 in
        let config_for i target separate =
          let effects =
            match target with
            | Js -> Rng.pick st [ "disabled"; "disabled"; "cps"; "double-translation" ]
            | Wasm ->
                Rng.pick
                  st
                  ([ "jspi"; "jspi"; "cps"; "double-translation"; "native" ]
                  @ if separate then [] else [ "disabled" ])
          in
          (* wasm_of_ocaml has no --target-env *)
          let env =
            match target with
            | Js -> Rng.pick st [ "isomorphic"; "isomorphic"; "nodejs"; "browser" ]
            | Wasm -> "isomorphic"
          in
          let pretty = Rng.chance st 0.2 and debuginfo = Rng.chance st 0.2 in
          let flags =
            ("--effects=" ^ effects)
            ::
            (match target with
            | Js -> [ "--target-env=" ^ env ]
            | Wasm -> [])
            @ (if pretty then [ "--enable"; "pretty" ] else [])
            @ if debuginfo then [ "--enable"; "debuginfo" ] else []
          in
          let opt =
            random_flags
              ~exclude:
                [ "excwrap"; "pretty"; "debuginfo"; "callgen"; "es6"; "stable_var" ]
              ~p:0.12
              st
              target
            @ if (not separate) && Rng.chance st 0.2 then [ "--linkall" ] else []
          in
          let name =
            sprintf "%s%s%d" (target_name target) (if separate then "-sep" else "") i
          in
          Hashtbl.replace
            settings
            name
            (Gen_runtime.oracle_args
               ~effects
               ~wasm:(target = Wasm)
               ~env
               ~pretty
               ~debuginfo);
          let c = compiler config target in
          let out = name ^ ".js" in
          let wasm_files =
            match target with
            | Js -> []
            | Wasm -> [ "prims.wat" ]
          in
          let steps =
            if not separate
            then [ c, flags @ opt @ files @ wasm_files @ [ "t.byte"; "-o"; out ] ]
            else
              let rt, stdlib, std_exit = separate_cache config target effects in
              ignore rt;
              match target with
              | Js ->
                  [ c, ("build-runtime" :: flags) @ files @ [ "-o"; name ^ "-rt.js" ]
                  ; c, ("compile" :: flags) @ opt @ [ "t.cmo"; "-o"; name ^ "-t.js" ]
                  ; ( c
                    , [ "link"
                      ; name ^ "-rt.js"
                      ; stdlib
                      ; name ^ "-t.js"
                      ; std_exit
                      ; "-o"
                      ; out
                      ] )
                  ]
              | Wasm ->
                  [ ( c
                    , ("build-runtime" :: flags)
                      @ files
                      @ wasm_files
                      @ [ "-o"; name ^ "-rt.wasma" ] )
                  ; c, ("compile" :: flags) @ opt @ [ "t.cmo"; "-o"; name ^ "-t.wasmo" ]
                  ; ( c
                    , [ "link"
                      ; name ^ "-rt.wasma"
                      ; stdlib
                      ; name ^ "-t.wasmo"
                      ; std_exit
                      ; "-o"
                      ; out
                      ] )
                  ]
          in
          name, target, effects, steps, out, dir
        in
        let configs =
          [ config_for 0 Js false; config_for 1 Js false; config_for 2 Js true ]
          @
          if Option.is_some config.wasmoo
          then
            [ config_for 3 Wasm false; config_for 4 Wasm false; config_for 5 Wasm true ]
          else []
        in
        let expected name _ =
          let r =
            run ~timeout:30. ~cwd:dir "ocamlrun" ("ref.byte" :: Hashtbl.find settings name)
          in
          numbered_lines r.stdout
        in
        compare_configs
          config
          ~seed
          ~files:report_files
          ~configs
          ~expected
          ~lines_for:(fun target keys ->
            match target with
            | Js -> keys
            | Wasm -> List.filter (fun k -> List.mem k p.wasm_lines) keys))

(****)

(* Existing bytecode programs (for instance the tests-ocaml corpus) compiled
   with random flags. The reference is the output with the default flags
   for the same target and effect mode. *)
let corpus config ~programs seed =
  let st = Rng.make (seed + 4_000_000) in
  let bc = Rng.pick st programs in
  let target = Rng.pick st (targets config) in
  let effects = Rng.pick st (effect_modes target) in
  let flags = ("--effects=" ^ effects) :: random_flags ~p:0.2 st target in
  let compile_and_run dir flags =
    let c =
      run
        ~timeout:900.
        ~cwd:dir
        (compiler config target)
        (interp_flags config target @ flags @ [ bc; "-o"; "a.js" ])
    in
    if ok c
    then c, Some (node ~native_effects:(native target effects) ~cwd:dir "a.js")
    else c, None
  in
  let ref_dir = Filename.concat config.out ".corpus-ref" in
  mkdir_p ref_dir;
  let key =
    Digest.to_hex (Digest.string (sprintf "%s %s %s" bc (target_name target) effects))
  in
  let ref_file = Filename.concat ref_dir key in
  (* The reference output, computed twice to detect nondeterministic
     programs *)
  let reference =
    if Sys.file_exists ref_file
    then
      let s = read_file ref_file in
      if s = "" then None else Some (String.sub s 1 (String.length s - 1))
    else
      let attempt () =
        with_temp_dir (fun dir ->
            match compile_and_run dir [ "--effects=" ^ effects ] with
            | c, Some n when ok c && n.status <> Timeout -> Some n.stdout
            | _ -> None)
      in
      let r =
        match attempt () with
        | Some a when attempt () = Some a -> Some a
        | _ -> None
      in
      let tmp = ref_file ^ ".tmp" ^ string_of_int (Unix.getpid ()) in
      write_file
        tmp
        (match r with
        | Some s -> "+" ^ s
        | None -> "");
      Sys.rename tmp ref_file;
      r
  in
  with_temp_dir (fun dir ->
      let c, n = compile_and_run dir flags in
      let name = target_name target in
      let problem =
        match n with
        | None -> Some (compile_problem c)
        | Some n -> (
            match reference with
            | Some r when n.status <> Timeout && n.stdout <> r ->
                Some "node output differs"
            | _ -> None)
      in
      match problem with
      | None -> if reference = None then name ^ " ok (no reference)" else name ^ " ok"
      | Some problem ->
          save_report
            config
            ~seed
            ~files:[]
            ~report:
              (sprintf
                 "%s (%s)\n%s\nflags: %s\n\n%s%s"
                 problem
                 name
                 bc
                 (String.concat " " flags)
                 (short_log c.stderr)
                 (match n with
                 | Some n ->
                     section "reference" (Option.value ~default:"" reference)
                     ^ section "node" n.stdout
                     ^ section "node stderr" n.stderr
                 | None -> ""));
          print_problem
            ~seed
            (sprintf "%s: %s %s %s" problem name bc (String.concat " " flags));
          name ^ " " ^ problem)

(****)

(* Remove the toplevel items of a generated program (separated by blank
   lines) while the failure persists: node's output on the compiled program
   contains [pattern] *)
let reduce ~file ~compiler ~flags ~pattern ~ocamlc_flags =
  let fails items =
    with_temp_dir (fun dir ->
        write_file (Filename.concat dir "t.ml") (String.concat "\n\n" items ^ "\n");
        ok (ocamlc ~cwd:dir ([ "-w"; "-a" ] @ ocamlc_flags @ [ "t.ml"; "-o"; "t.byte" ]))
        && ok (run ~cwd:dir compiler (flags @ [ "t.byte"; "-o"; "a.js" ]))
        &&
        let n =
          node ~native_effects:(List.mem "--effects=native" flags) ~cwd:dir "a.js"
        in
        n.status <> Timeout && contains (n.stdout ^ n.stderr) pattern)
  in
  let split s =
    let rec loop acc cur = function
      | [] ->
          List.rev (if cur = [] then acc else String.concat "\n" (List.rev cur) :: acc)
      | "" :: rem ->
          loop (if cur = [] then acc else String.concat "\n" (List.rev cur) :: acc) [] rem
      | l :: rem -> loop acc (l :: cur) rem
    in
    loop [] [] (lines s)
  in
  let items = split (read_file file) in
  if not (fails items) then failwith "the program does not fail";
  let rec pass items =
    let changed = ref false in
    let items = ref items in
    for i = List.length !items - 1 downto 0 do
      let candidate = List.filteri (fun j _ -> j <> i) !items in
      if fails candidate
      then (
        items := candidate;
        changed := true)
    done;
    if !changed then pass !items else !items
  in
  print_string (String.concat "\n\n" (pass items) ^ "\n")
