val link :
     runtime_files:string list
  -> input_files:string list
  -> opt_output_sourcemap:string option
  -> output_file:string
  -> unit

val dead_code_elimination :
     dependencies:string
  -> opt_input_sourcemap:string option
  -> input_file:string
  -> opt_output_sourcemap:string option
  -> output_file:string
  -> Stdlib.StringSet.t

val optimize :
     profile:Driver.profile option
  -> opt_input_sourcemap:string option
  -> input_file:string
  -> opt_output_sourcemap:string option
  -> opt_sourcemap_url:string option
  -> output_file:string
  -> unit
