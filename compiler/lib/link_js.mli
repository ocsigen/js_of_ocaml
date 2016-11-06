
val link : output:out_channel -> files:string list ->
  resolve_sourcemap_url:bool ->
  source_map: (string option * Source_map.t) option -> unit
