open Stdlib

let build ~link_options ~opt_options ~variables ~inputs ~output_file =
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
    ~profile:None
    ~options:opt_options
    ~opt_input_sourcemap:None
    ~input_file:merge_file
    ~opt_output_sourcemap:None
    ~output_file
    ()
