type t =
  | Atom of string
  | List of t list

val to_string : t -> string

val from_string : string -> t

module Util : sig
  val single : (t -> 'a) -> t list -> 'a

  val mandatory : (t list -> 'a) -> t list option -> 'a

  val string : t -> string

  val bool : t -> bool

  val assoc : t -> (string * t list) list

  val member : string -> t -> t list option
end
