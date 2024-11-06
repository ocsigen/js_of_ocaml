type input =
  { module_name : string
  ; file : string
  ; code : string option
  ; opt_source_map : [ `File of string | `Data of string ] option
  }

val f :
  input list -> output_file:string -> opt_output_sourcemap_file:string option -> unit
