val build :
     allowed_imports:string list option
  -> link_options:string list
  -> opt_options:string list
  -> variables:(string * Wat_preprocess.value) list
  -> inputs:Wat_preprocess.input list
  -> output_file:string
  -> unit
