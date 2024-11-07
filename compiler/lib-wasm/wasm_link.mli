type input =
  { module_name : string
  ; file : string
  ; code : string option
  ; opt_source_map : Source_map.Standard.t option
  }

val f : input list -> output_file:string -> Source_map.t
