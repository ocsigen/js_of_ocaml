type output

val open_out : string -> output

val add_entry : output -> name:string -> contents:string -> unit

val add_file : output -> name:string -> file:string -> unit

val close_out : output -> unit

type input

val open_in : string -> input

val with_open_in : string -> (input -> 'a) -> 'a

val has_entry : input -> name:string -> bool

val read_entry : input -> name:string -> string

val get_entry :
  input -> name:string -> in_channel * int (* pos *) * int (* len *) * int32 (* crc *)

val extract_file : input -> name:string -> file:string -> unit

val copy_file : input -> output -> src_name:string -> dst_name:string -> unit

val close_in : input -> unit
