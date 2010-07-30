


val urldecode : string -> string

val urlencode : string -> string

type protocol = Http | Https | File | Exotic of string

val protocol_of_string : string -> protocol
val string_of_protocol : ?comma:bool -> protocol -> string

val default_http_port : int
val default_https_port : int

val port_of_string : protocol -> string -> int option
val string_of_port : int -> string
val string_of_port_option : protocol -> int option -> string option

val path_of_path_string : string -> string list

val encode_arguments : (string * string) list -> string
val decode_arguments : string -> (string * string) list

type url = {
  protocol : protocol;
  host : string;
  port : int option;
  path : string list;
  path_string : string;
  arguments : (string * string) list;
  fragment : string;
}

val url_of_string : string -> url option
val string_of_url : ?encode:bool -> url -> string

module Current :
  sig
    val protocol : protocol
    val host : string
    val port : int option
    val path_string : string
    val path : string list
    val arguments : (string * string) list
    val fragment : unit -> string
    val set_fragment : string -> unit
    val get : unit -> url
    val set : url -> unit
    val as_string : string
  end
