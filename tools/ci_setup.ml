module Config = Ci_setup_config
module StringSet = Set.Make (String)

(****)

let jane_root, wasmoo_root =
  match Sys.argv with
  | [| _; jane_root; wasmoo_root |] -> jane_root, wasmoo_root
  | _ -> "janestreet", "wasm_of_ocaml"

let repo = Filename.concat jane_root "opam-repository/packages"

(* Which backend this run targets, matching the generated [dune-workspace]
   toggle. Used to emit backend-specific test expectations and tags. *)
let target_is_wasm =
  match Sys.getenv_opt "WASM_OF_OCAML" with
  | Some ("true" | "1") -> true
  | _ -> false

let roots = Config.roots

let additional_others = StringSet.of_list Config.additional_others

let omitted_others = StringSet.of_list Config.omitted_others

let omitted_js = StringSet.of_list Config.omitted_js

let do_pin = StringSet.of_list Config.do_pin

let forked_packages = StringSet.of_list Config.forked_packages

let fixed_packages =
  StringSet.of_list [ "bonsai_web"; "bonsai_web_components"; "virtual_dom" ]

let dune_workspace =
  {|(lang dune 3.17)
(env
 (_
  (env-vars (TESTING_FRAMEWORK inline-test))
  (wasm_of_ocaml (enabled_if %{env:WASM_OF_OCAML=false}))
  ;; Jane Street's runtime.js stubs assume string and bytes share a
  ;; representation (they build with use-js-string=false), so run the js
  ;; tests the same way.
  (js_of_ocaml
   (enabled_if %{env:JS_OF_OCAML=false})
   (flags (:standard --disable use-js-string)))
|}
  ^ Config.dune_workspace_extra_env
  ^ {|  (flags :standard -alert -all -warn-error -7-8-27-30-32-34-37-49-52-55 -w -7-27-30-32-34-37-49-52-55-56-58-67-69)))
|}

let node_wrapper =
  [ ( "node_wrapper/dune"
    , {|(executable
 (public_name node)
 (name node_wrapper)
       (libraries unix))|} )
  ; "node_wrapper/node_wrapper_per_engine.ml", {|let engine = "node"|}
  ; "node_wrapper/dune-project", "(lang dune 3.17)"
  ; "node_wrapper/node_wrapper.opam", ""
  ]

let patches = Config.patches ~target_is_wasm

let removes =
  [ "core/core/test/test_sys.ml"
  ; "core/core/test/test_sys.mli"
  ; "core/core/test/test_timezone.ml"
  ; "core/core/test/test_timezone.mli"
  ]
(****)

let read_opam_file filename =
  OpamPp.parse
    OpamPp.Op.(OpamFormat.I.file -| OpamPp.map_snd OpamFile.OPAM.pp_raw_fields)
    ~pos:{ filename; start = 0, 0; stop = 0, 0 }
    (OpamParser.FullPos.file (Filename.concat (Filename.concat repo filename) "opam"))

let dependencies (_, { OpamFile.OPAM.depends; _ }) =
  let open OpamFormula in
  depends
  |> map (fun (nm, _) -> Atom (nm, None))
  |> of_atom_formula
  |> atoms
  |> List.map fst
  |> List.map OpamPackage.Name.to_string

let latest_version pkg =
  let dir = Filename.concat repo pkg in
  let prefix = pkg ^ "." in
  Sys.readdir dir
  |> Array.to_list
  |> List.filter (String.starts_with ~prefix)
  |> List.sort (fun a b ->
      let va =
        String.sub a (String.length prefix) (String.length a - String.length prefix)
      in
      let vb =
        String.sub b (String.length prefix) (String.length b - String.length prefix)
      in
      OpamVersionCompare.compare vb va)
  |> List.hd

let packages =
  repo
  |> Sys.readdir
  |> Array.to_list
  |> List.map (fun s ->
      if String.contains s '.'
      then String.sub s 0 (String.index s '.'), read_opam_file s
      else s, read_opam_file (Filename.concat s (latest_version s)))
  |> List.filter Config.keep_package

let rec traverse visited p =
  if StringSet.mem p visited
  then visited
  else
    let visited = StringSet.add p visited in
    match List.assoc p packages with
    | exception Not_found -> visited
    | opam ->
        let l = dependencies opam in
        List.fold_left traverse visited l

let is_forked p = StringSet.mem p forked_packages

let is_fixed p = StringSet.mem p fixed_packages

let exec_async cmd =
  let p = Unix.open_process_out cmd in
  fun () -> ignore (Unix.close_process_out p)

let ( let* ) (f : unit -> 'a) (g : 'a -> unit -> 'b) : unit -> 'b = fun () -> g (f ()) ()

let sync_exec f l =
  let l = List.map f l in
  List.iter (fun f -> f ()) l

let pin nm =
  exec_async
    (Printf.sprintf
       "opam pin add -n %s https://github.com/ocaml-wasm/%s.git#%s"
       nm
       nm
       Config.pin_branch)

let pin_packages () = sync_exec pin (StringSet.elements do_pin)

let install_others others =
  let others =
    StringSet.elements
      (StringSet.union (StringSet.diff others omitted_others) additional_others)
  in
  ignore (Sys.command ("opam install -y " ^ String.concat " " others))

let clone ?branch ?(depth = 1) nm src =
  exec_async
    (Printf.sprintf
       "git clone -q --depth %d %s%s %s/lib/%s"
       depth
       (match branch with
       | None -> ""
       | Some b -> Printf.sprintf "-b %s " b)
       src
       jane_root
       nm)

let clone' ?branch ?commit nm src =
  match commit with
  | None -> clone ?branch nm src
  | Some commit ->
      let* () = clone ?branch ~depth:100 nm src in
      exec_async
        (Printf.sprintf "cd %s/lib/%s && git checkout -b wasm %s" jane_root nm commit)

let () =
  let write f contents =
    Out_channel.(with_open_bin f @@ fun ch -> output_string ch contents)
  in
  let copy f f' =
    let contents = In_channel.(with_open_bin f @@ input_all) in
    Out_channel.(with_open_bin f' @@ fun ch -> output_string ch contents)
  in
  write (Filename.concat jane_root "dune-workspace") dune_workspace;
  Unix.mkdir (Filename.concat jane_root "node_wrapper") 0o755;
  List.iter
    (fun (f, contents) -> write (Filename.concat jane_root f) contents)
    node_wrapper;
  copy
    (Filename.concat wasmoo_root "tools/node_wrapper.ml")
    (Filename.concat jane_root "node_wrapper/node_wrapper.ml")

let () =
  let js, others =
    List.fold_left traverse StringSet.empty roots
    |> StringSet.partition (fun p -> List.mem_assoc p packages)
  in
  pin_packages ();
  install_others others;
  sync_exec (fun () -> clone "ocaml-uri" "https://github.com/mirage/ocaml-uri") [ () ];
  sync_exec (fun () -> exec_async "opam install uri --deps-only") [ () ];
  sync_exec
    (fun nm ->
      let branch =
        if is_fixed nm
        then Some Config.fixed_branch
        else if is_forked nm
        then Some Config.forked_branch
        else Config.default_branch
      in
      let commit =
        if is_fixed nm || is_forked nm
        then None
        else
          let commit =
            let _, opam = List.assoc nm packages in
            let url = OpamUrl.to_string (Option.get (OpamFile.OPAM.get_url opam)) in
            let tar_file = Filename.basename url in
            String.sub tar_file 0 (String.index tar_file '.')
          in
          Format.eprintf "Cloning %s#%s@." nm commit;
          Some commit
      in
      clone'
        ?branch
        ?commit
        nm
        (Printf.sprintf
           "https://github.com/%s/%s"
           (if is_fixed nm || is_forked nm then "ocaml-wasm" else "janestreet")
           nm))
    (StringSet.elements (StringSet.diff js omitted_js))

let () =
  List.iter
    (fun (dir, patch) ->
      let p = if Sys.win32 then "patch --binary" else "patch" in
      let ch =
        Unix.open_process_out
          (Printf.sprintf "cd %s/lib/%s && %s -p 1 --" jane_root dir p)
      in
      let patch =
        if Sys.win32
        then String.concat "\r\n" (String.split_on_char '\n' patch)
        else patch
      in
      output_string ch patch;
      match Unix.close_process_out ch with
      | WEXITED 0 -> ()
      | e ->
          let name, i =
            match e with
            | WEXITED n -> "exit", n
            | WSIGNALED n -> "signal", n
            | WSTOPPED n -> "stop", n
          in
          failwith (Printf.sprintf "%s %d while patching %s" name i dir))
    patches;
  List.iter (fun p -> Sys.remove (Printf.sprintf "%s/lib/%s" jane_root p)) removes

(****)

(* The forked packages and the [dune] patches above bake in an inline-test
   tag set that was tuned for wasm: it drops [no-js]/[no-wasm]/[64-bits-only]
   but never [wasm-only] or [js-only]. A single [(inline_tests (flags ...))]
   stanza is shared by the [js] and [wasm] runners, so we cannot vary the
   flags per mode. Each job however enables a single backend (see the
   generated [dune-workspace], keyed on [WASM_OF_OCAML]/[JS_OF_OCAML]), so we
   finish by dropping the tag reserved for the *other* backend: [wasm-only]
   when targeting js, [js-only] when targeting wasm. *)

let extra_drop_tag = if target_is_wasm then "js-only" else "wasm-only"

let rec dune_files dir =
  Sys.readdir dir
  |> Array.to_list
  |> List.concat_map (fun name ->
      let path = Filename.concat dir name in
      if try Sys.is_directory path with Sys_error _ -> false
      then dune_files path
      else if String.equal name "dune"
      then [ path ]
      else [])

let () =
  let anchor = Re.compile (Re.str "(inline_tests (flags ") in
  let by = Printf.sprintf "(inline_tests (flags -drop-tag %s " extra_drop_tag in
  let already = Re.compile (Re.str ("-drop-tag " ^ extra_drop_tag)) in
  List.iter
    (fun f ->
      let contents = In_channel.(with_open_bin f @@ input_all) in
      if Re.execp anchor contents && not (Re.execp already contents)
      then
        Out_channel.(
          with_open_bin f
          @@ fun ch -> output_string ch (Re.replace_string anchor ~by contents)))
    (dune_files (Filename.concat jane_root "lib"))
