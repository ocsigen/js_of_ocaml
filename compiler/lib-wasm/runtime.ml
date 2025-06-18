open Stdlib

let build ~allowed_imports ~link_options ~opt_options ~variables ~inputs ~output_file =
  Fs.with_intermediate_file (Filename.temp_file "runtime-merged" ".wasm")
  @@ fun merge_file ->
  (Wat_preprocess.with_preprocessed_files ~variables ~inputs
  @@ fun inputs ->
  Binaryen.link
    ~options:link_options
    ~opt_output_sourcemap:None
    ~inputs
    ~output_file:merge_file
    ());
  Binaryen.optimize
    ~profile:Profile.O1
    ~options:opt_options
    ~opt_input_sourcemap:None
    ~input_file:merge_file
    ~opt_output_sourcemap:None
    ~output_file
    ();
  let imports = Link.Wasm_binary.read_imports ~file:output_file in
  Option.iter allowed_imports ~f:(fun allowed_imports ->
      let missing_imports =
        List.filter
          ~f:(fun { Link.Wasm_binary.module_; _ } ->
            not (List.mem ~eq:String.equal module_ allowed_imports))
          imports
      in
      if not (List.is_empty missing_imports)
      then (
        Format.eprintf "The runtime contains unknown imports:@.";
        List.iter
          ~f:(fun { Link.Wasm_binary.module_; name } ->
            Format.eprintf "  %s %s@." module_ name)
          missing_imports;
        exit 2))
